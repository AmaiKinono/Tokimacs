;;; facets.el --- My zettlekasten -*- lexical-binding: t -*-

;; Copyright (C) 2022 Hao Wang
;; License: GPL v3, or (at your option) any later version

;;; Commentary:

;; Facets.el is a zettlekasten plugin that's heavily inspired by denote.el
;; (https://protesilaos.com/emacs/denote), but with less functionality, less
;; code, and suits my need better in some minor aspects.  Here's a brief user
;; guide.
;;
;; * Overview
;;
;; - Your cards (facets) are stored in `facets-directory', or any subdirectory
;;   under it.
;; - Each facet has a filename like <id>--<title>__<tag1_tag2_...>.<extension>.
;; - ID is in the format of yyyyMMddTHHmmss, e.g., 19990101T123030.
;; - Tags contain only alphanumeric chars, non-ascii chars and hyphens.
;; - Titles can be named rather freely, but should not use chars in
;;   `facets/illegal-filename-chars-regexp'.
;; - A facet can be any kind of file.  If it uses a supported markup language
;;   (org and markdown for now), the ID, title and tags can also be written to
;;   the frontmatter in that file.  This is for: 1. Using chars in the title
;;   that can't appear in a filename; 2. Recover the filename in case it's
;;   corrupted; 3. Exporting, which we may consider in the future.
;; - A text file card can contain links to other cards.  Links are in the
;;   simple format "facet:<id>".
;;
;; * Configuration
;;
;; Add `facets-auto-enable-facets-mode' to `find-file-hook'.  You may also want
;; to customize `facets-mode-map'.
;;
;; * Create a facet
;;
;; - Call `facets-new-facet', a new facet will be created with the frontmatter
;;   inserted.  You could now fill in the "title" and "tags" attributes in the
;;   frontmatter.
;; - For a non-text-file facet, move it to `facets-directory', and call
;;   `facets-rename-file'.
;;
;; * Edit a facet
;;
;; - Use `facets-edit-tags' to edit the tags in the frontmatter.  This has the
;;   advantage that you can pick from all tags used by existing facets.
;; - Use `facets-insert-link' to insert a link to another facet.  Another way
;;   is to open a facet, call `facets-copy-id-as-link', then paste in the
;;   current file.
;; - Use `facets-save-file' to save the current facet.  In `facets-mode',
;;   `save-buffer' is remapped to it.  This has the advantage that the file
;;   name will be synced with the frontmatter (by `facets-sync-file-name'), and
;;   that when creating a new file, you don't have to manually edit the file
;;   name.
;; - If you find it's time to put a facet and related resources (like images)
;;   into their own dedicated directory, call `facets-move-file-into-subdir'.
;;
;; * Read a facet
;;
;; - Use `facets-find-facet' to open a facet.  If you are using a completing
;;   style like `substring' or `orderless', You can filter the tags by "_tag".
;; - Click or press RET on a link to follow it, which calls
;;   `facets-follow-link'.
;; - Then call `facets-jump-back' to jump back.
;; - Call `facets-list-references' to find all links pointing to the current
;;   facet.
;;
;; * Others
;;
;; - Some user options and commands are not mentioned in this guide.  You may
;;   found them useful.
;; - The name "facets" comes from my mantra "Every card is a facet of a grand
;;   design".

;;; Code:

;; To see the outline of this file, run `outline-minor-mode', then
;; `outline-hide-body'.  Another way is to run `occur' with the query:
;; ^;;;;* \|^(

;;;; Libraries

(require 'cl-lib)
(require 'ring)
(require 'rx)

;;;; User options

;;;###autoload
(put 'facets-directory 'safe-local-variable #'stringp)
(defvar-local facets-directory "~/facets/"
  "The directory for facets.")

(defvar-local facets-default-mode #'org-mode
  "The default mode for new facets.")

(defvar facets-grep-program-debug nil
  "Nil to silent grep errors.")

(defvar facets-excluded-dirs-regexp
  (rx (or (seq string-start ".git" string-end)
          (seq string-start "ltximg" string-end)))
  "Regexp to match excluded directories.")

;; TODO: refactor this to a list in the precedence order.
(defvar facets-grep-program-kind 'rg
  "The grep program.
Can be `gnugrep' or `rg'.  Unless `facets-grep-program' is
specified, when this is set to `rg' and the rg executable is not
found, GNU grep is used as fallback.")

(defvar facets-grep-program nil
  "The path of the grep program.
Or it can be the program name if it's in PATH.")

(defvar facets-link-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'facets-follow-link)
    (define-key map (kbd "<mouse-1>") 'facets-follow-link)
    map)
  "Keymap that's enabled on links.")

(defvar facets-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map [remap save-buffer] 'facets-save-file)
    map)
  "Keymap used in `facets-mode'.")

(defvar facets-new-facet-hook '(facets-mode facets-insert-frontmatter)
  "Hook run after `facets-new-facet'.")

(defvar facets-markdown-extensions '("md")
  "Markdown file extensions.")

(defvar facets-markdown-major-modes '(markdown-mode)
  "Major modes for markdown.")

(defvar facets-org-extensions '("org")
  "Org file extensions.")

(defvar facets-org-major-modes '(org-mode)
  "Major modes for org.")

;;;; Utils

;;;;; Process

(defun facets/get-grep-process-output (program &optional args)
  "Return the output of PROGRAM with args ARGS.
If the process exits abnormally and:

- `facets-grep-program-debug' is non-nil, signal an error using
the output.
- it's nil, then return nil."
  (let* (status
         (output
          (with-output-to-string
            (with-current-buffer standard-output
              (setq status
                    (apply #'process-file program
                           nil standard-output nil args))))))
    (if (eq status 0)
        output
      (when facets-grep-program-debug
        (error "%s exits %s:\n%s"program status output)))))

(defun facets/run-process (program &optional args)
  "Run PROGRAM with ARGS and output to the current buffer."
  (apply #'process-file program
         nil (current-buffer) nil args))

;;;;; Grep

(defun facets/grep-program-kind ()
  "Return the grep program kind to use."
  (if facets-grep-program
      facets-grep-program-kind
    (let ((prog (pcase facets-grep-program-kind
                  ('gnugrep (when (executable-find "grep") 'gnugrep))
                  ('rg (if (executable-find "rg")
                           'rg
                         (when (executable-find "grep")) 'gnugrep)))))
      (or prog (user-error "Grep program not available")))))

(defun facets/grep-program ()
  "Return the grep program to use."
  (or facets-grep-program
      (pcase (facets/grep-program-kind)
        ('gnugrep "grep")
        ('rg "rg"))))

(defvar facets/gnugrep-common-args
  '("--recursive" "--extended-regexp" "--color=never" "--exclude-dir=.git")
  "Common args used for GNU grep commands.")

(defvar facets/rg-common-args
  '("--color=never")
  "Common args used for rg commands.")

(defun facets/gnugrep-pattern-arg (pattern)
  "Build regexp pattern args using PATTERN for GNU grep."
  (list (concat "--regexp=" pattern)))

(defun facets/rg-pattern-arg (pattern)
  "Build regexp pattern args using PATTERN for rg."
  (list (concat "--regexp=" pattern)))

(defvar facets/gnugrep-show-file-args
  '("--files-with-matches")
  "Args for GNU grep to show only file names.")

(defvar facets/rg-show-file-args
  '("--files-with-matches")
  "Args for rg to show only file names.")

;; Do we need --null here?
(defvar facets/gnugrep-show-location-args
  '("--line-number" "--with-filename")
  "Args for GNU grep to output in a format suitable for `grep-mode'.")

(defvar facets/rg-show-location-args
  '("--line-number" "--no-heading")
  "Args for rg to output in a format suitable for `grep-mode'.")

(defun facets/gnugrep-regexp-quote (str)
  "Return a regexp that matches STR in GNU grep pattern.
It's assumed that POSIX extended regular expressions (ERE) is
begin used.  It can be enabled by the -E flag in GNU grep."
  (replace-regexp-in-string
   (rx (or "(" ")" "[" "]" "{" "}" "." "*" "+" "^" "$" "|" "?" "\\"))
   "\\\\\\&" str))

(defun facets/rg-regexp-quote (str)
  "Return a regexp that matches STR in rg pattern.
It's assumed that the default regex engine of rg (Rust's regex
engine) is being used."
  (replace-regexp-in-string
   ;; Taken from `is_meta_character' in regex/regex-syntax/src/lib.rs.
   (rx (or "\\" "." "+" "*" "?" "(" ")" "|" "[" "]" "{" "}"
           "^" "$" "#" "&" "-" "~"))
   "\\\\\\&" str))

(defun facets/grep-cmd (pattern grep-kind output-kind files)
  "Build grep command.
Search PATTERN in FILES.  GREP-KIND can be `gnugrep' or `rg'.
The returned value is a list of args.

OUTPUT-KIND can be `file' or `location'.  It specifies whether to
output only the file names or the whole locations.

The command should be run at `facets-directory'."
  (let* (common-args output-args regexp-args file-args)
    (pcase grep-kind
      ('rg
       (setq common-args facets/rg-common-args)
       (setq output-args (pcase output-kind
                           ('file facets/rg-show-file-args)
                           ('location facets/rg-show-location-args)
                           (_ (error "Invalid OUTPUT_KIND"))))
       (setq regexp-args (facets/rg-pattern-arg pattern)))
      ('gnugrep
       (setq common-args facets/gnugrep-common-args)
       (setq output-args (pcase output-kind
                           ('file facets/gnugrep-show-file-args)
                           ('location facets/gnugrep-show-location-args)
                           (_ (error "Invalid OUTPUT_KIND"))))
       (setq regexp-args (facets/gnugrep-pattern-arg pattern)))
      (_ (error "Invalid grep program kind")))
    (setq file-args (mapcar (lambda (f)
                              (file-relative-name f facets-directory))
                            files))
    `(,@common-args ,@output-args ,@regexp-args "--" ,@file-args)))

;;;;; File

(defun facets/current-file-extension ()
  "Return the extension of current file.
Return nil if current buffer is not a file buffer."
  (when-let ((file (buffer-file-name)))
      (file-name-extension file)))

(defun facets/file-modification-time (file)
  "Return the latest modification time of FILE."
  (file-attribute-modification-time (file-attributes file)))

;;;; Internals

;;;;; File naming

(defconst facets/illegal-filename-chars-regexp
  (rx (or "/" "<" ">" ":" "\"" "\\" "|" "?" "*" "\n" control))
  "Illegal chars in filenames.
These makes sure that facets works on both Unix/Windows systems.")

(defconst facets/filename-regexp
  (rx string-start
      (group (+? not-newline))
      (seq "--" (group (* not-newline)))
      (seq "__" (group (opt (* (not ".")))))
      (opt "." (group (* not-newline)))
      string-end)
  "Regexp to match facet filenames.  See `facets/parse-filename'.")

(defun facets/parse-filename (filename)
  "Parse FILENAME.
If FILENAME is a legal facet file, return (ID TITLE TAGS
EXTENSION)."
  (setq filename (file-name-nondirectory filename))
  (when (string-match facets/filename-regexp
                      filename)
    (let ((id (match-string 1 filename))
          (title (match-string 2 filename))
          (tags (match-string 3 filename))
          (ext (match-string 4 filename)))
      (if (equal tags "")
          (setq tags nil)
        (setq tags (split-string tags "_")))
      (list (pcase id ("" nil) (val val))
            (pcase title ("" nil) (val val))
            tags
            (pcase ext ("" nil) (val val))))))

(defun facets/filename-sans-id (filename)
  "Strip the directory and ID part of FILENAME.
If FILENAME is not in the facet format, only strip the directory
part."
  (setq filename (file-name-nondirectory filename))
  (if (string-match facets/filename-regexp filename)
      (substring filename (match-beginning 2))
    filename))

(defun facets/make-filename (id title tags &optional ext)
  "Encode ID, TITLE, TAGS and EXT in a filename."
  (unless id (error "ID is empty"))
  (when (string-match-p "--" id)
    (error "\"--\" found in ID: %s" id))
  (when (string-match-p facets/illegal-filename-chars-regexp id)
    (error "Illegal filename char found in ID: %s" id))
  (unless (cl-every #'facets/tag-legal-p tags)
    (error "Tag contains illegal char: %s"
           (cl-find-if (lambda (tag) (not (facets/tag-legal-p tag))) tags)))
  (if (null title) (setq title "")
    (setq title (facets/encode-title title)))
  (concat id "--" title "__" (string-join tags "_")
          (if ext (concat "." ext) "")))

;;;;; ID

(defconst facets/format-time-string "%Y%m%dT%H%M%S"
  "Timestamp format.")

(defconst facets/legal-id-regexp (rx string-start
                                     (= 8 num) "T" (= 6 num)
                                     string-end)
  "Regexp to match a legal ID string.")

;; Original format: "%F/%T"
(defun facets/generate-id (time)
  "Generate an ID from TIME.
If the ID already exists (in `facets-directory'), plus 1 sec on
the time until a unique ID is generated."
  (let ((all-ids (mapcar (lambda (f) (car (facets/parse-filename f)))
                         (facets/all-facets)))
        result)
    (while (progn
             (setq result (format-time-string facets/format-time-string time))
             (member result all-ids))
      (setq time (time-add time 1)))
    result))

(defun facets/get-id-files (id)
  "Get files that have id ID."
  (let ((result nil))
    (dolist (f (facets/all-facets))
      (when (equal id (car (facets/parse-filename f)))
        (push f result)))
    result))

(defun facets/read-timestamp ()
  "Prompt the user for a timestamp.
Returns a string in the format of a facet ID."
  (cl-do ((result ""))
      ((string-match facets/legal-id-regexp result) result)
    (setq result
          (read-string
           "Timestamp (yyyyMMddTHHmmss, e.g., 19990101T123030): "))))

(defun facets/read-id (&optional filename)
  "Prompt the user for an ID.
If FILENAME is non-nil, add the option to use the modification
time of FILENAME as ID.  The actual timestamp may be shifted by
several seconds to avoid clash with existing facets."
  (let ((current-time (current-time))
        (mod-time (when filename (facets/file-modification-time filename)))
        choice)
    (if mod-time
        (setq choice
              (pcase (read-char-choice
                      (format
                       "Generate ID by ...
[1] Current time: %s
[2] File modification time: %s
[3] User input
==> Please type a number (1-3) to choose: "
                       (format-time-string facets/format-time-string current-time)
                       (format-time-string facets/format-time-string mod-time))
                      '(?1 ?2 ?3))
                (?1 'current-time)
                (?2 'mod-time)
                (?3 'user-input)))
      (setq choice
            (pcase (read-char-choice
                    (format
                     "Generate ID by ...
[1] Current time: %s
[2] User input
==> Please type a number (1-2) to choose: "
                     (format-time-string facets/format-time-string current-time))
                    '(?1 ?2))
              (?1 'current-time)
              (?2 'user-input))))
    (pcase choice
      ('current-time (facets/generate-id current-time))
      ('mod-time (facets/generate-id mod-time))
      ('user-input (facets/generate-id
                    (time-convert (encode-time (parse-time-string
                                                (facets/read-timestamp)))
                                  'list))))))

;;;;; Title

(defconst facets/title-punct-encoding-table
  '(("<" . " lt ")
    (">" . " gt ")
    ("_" . "-")
    ("/" . "-")
    (":" . "-")
    ("\"" . "")
    ("?" . "")
    ("*" . "")
    ("\n" . ""))
  "Table for encoding certain chars in a title in a filename.")

(defun facets/encode-title (title)
  "Encode TITLE to be put in a filename."
  (setq title (replace-regexp-in-string
               (rx-to-string (nconc '(or) (mapcar #'car facets/title-punct-encoding-table)))
               (lambda (punct) (cdr (assoc punct facets/title-punct-encoding-table)))
               title))
  (setq title (replace-regexp-in-string
               facets/illegal-filename-chars-regexp
               "" title)))

(defun facets/read-title (&optional file)
  "Prompt the user for a title.
If FILE is non-nil, use the title part in it, or the filename
itself as the initial input."
  (read-string "Title: "
               (when file
                 (if-let ((s (facets/parse-filename file))) (nth 1 s)
                   (file-name-sans-extension
                    (file-name-nondirectory file))))))

;;;;; Tag

;; NOTE: I don't know if we need to loosen the format of tags (switch to
;; `facets/illegal-tag-chars-regexp'). These are chars we should excluded:

;; . to prevent part of the tags to be recognised as file extension.
;; _ is used as tag delimiter in the filename.
;; : is used as tag delimiter in org-mode.
;; , is likely a member in `crm-separator'.
(defconst facets/legal-tag-regexp (rx string-start
                                      (* (or alnum nonascii "-"))
                                      string-end)
  "Regexp to match a legal tag name.")

(defun facets/tag-legal-p (tag)
  "Non-nil if TAG is legal."
  (string-match facets/legal-tag-regexp tag))

(defun facets/tags-of-file (file)
  "Return the tags encoded in the name of FILE."
  (nth 2 (facets/parse-filename (file-name-nondirectory file))))

(defun facets/all-tags ()
  "Return all tags used by existing facets."
  (let (result)
    (dolist (f (facets/all-facets))
      (dolist (tag (facets/tags-of-file f))
        (unless (member tag result)
          (push tag result))))
    (sort result #'string<)))

(defun facets/read-tags (&optional initial-tags)
  "Prompt the user for tags.
If the list INITIAL-TAGS is non-nil, use it as the initial
input."
  (cl-remove-duplicates (completing-read-multiple
                         "Tags: " (facets/all-tags)
                         nil nil (string-join initial-tags ", "))
                        :test #'equal))

;;;;; Link

(defconst facets/link-regexp
  (list (rx (group "facet:")
            (group (= 8 num) "T" (= 6 num)))
        1 2)
  "Regexp to match links.
This is a list.  The car is a regexp and it must contain a group
that matches \"facet:\", and a group that matches the id inside
the link, and the remaining elemets are numbers of these groups.")

(defun facets/id-to-link (id)
  "Convert ID to link."
  (concat "facet:" id ))

(defun facets/link-id-bounds-at-point ()
  "Get the bounds of id in the link at point.
The bounds is returned as a cons pair."
  (save-excursion
    (let ((pt (point))
          (regexp (car facets/link-regexp))
          (group (nth 2 facets/link-regexp)))
      (goto-char (line-beginning-position))
      (when (and (re-search-forward regexp (line-end-position) t)
                 (<= (match-beginning 0) pt (match-end 0)))
        (cons (match-beginning group) (match-end group))))))

(defun facets/link-id-at-point ()
  "Get the id in the link at point."
  (when-let ((bounds (facets/link-id-bounds-at-point)))
    (buffer-substring-no-properties (car bounds) (cdr bounds))))

;;;;; Facets Mode

(defun facets/link-help-echo-func (_ _ pos)
  "Show the relative path to facet of link at POS."
  (when-let* ((id (save-excursion
                    (goto-char pos)
                    (facets/link-id-at-point)))
              (facets (facets/get-id-files id)))
    (file-relative-name (car facets) facets-directory)))

;; Fontification functions are borrowed from clue.el.

(defun facets/unfontify (beg end)
  "Remove fontification of links between BEG and END."
  (dolist (ov (overlays-in beg end))
    (when (overlay-get ov 'facets-link-p)
      (delete-overlay ov))))

(defun facets/fontify (beg end)
  "Fontify the links between BEG and END."
  (save-excursion
    (goto-char beg)
    (let ((beg-group (nth 1 facets/link-regexp))
          (end-group (nth 2 facets/link-regexp)))
      (while (re-search-forward (car facets/link-regexp) end t)
        (let* ((b (match-beginning beg-group))
               (e (match-end end-group))
               ov)
          (setq ov (make-overlay b e))
          (overlay-put ov 'facets-link-p t)
          (overlay-put ov 'face 'button)
          (overlay-put ov 'mouse-face 'link)
          (overlay-put ov 'evaporate t)
          (overlay-put ov 'help-echo #'facets/link-help-echo-func)
          ;; Fixing the behavior of pressing RET just before the link.
          (overlay-put (make-overlay (1+ b) e) 'keymap facets-link-keymap))))))

(defun facets/refontify (beg end)
  "Refontify the links between BEG and END.
This is for use with jit-lock fontification, so the region to
refontify includes the two logical lines including BEG and END,
to prevent miss caused by line truncation inside the links."
  (let ((beg (save-excursion (goto-char beg)
                             (line-beginning-position)))
        (end (save-excursion (goto-char end)
                             (line-end-position))))
    (facets/unfontify beg end)
    (facets/fontify beg end)))

;;;;; Markup language spec

(defvar facets/extension-spec-table
  (make-hash-table :test #'equal)
  "File extension -> file format spec.")

(defvar facets/major-mode-spec-table
  (make-hash-table :test #'equal)
  "Major mode -> file format spec.")

(defvar facets/file-format-specs nil
  "List of file format specs.")

(defun facets/register-file-format (spec)
  "Register file format spec SPEC."
  (let ((exts (facets/file-format-spec-extensions spec)))
    (dolist (ext exts)
      (puthash ext spec facets/extension-spec-table)))
  (let ((modes (facets/file-format-spec-major-modes spec)))
    (dolist (mode modes)
      (puthash mode spec facets/major-mode-spec-table)))
  (unless (memq (facets/file-format-spec-name spec)
                (mapcar #'facets/file-format-spec-name
                        facets/file-format-specs))
    (push spec facets/file-format-specs)))

(cl-defstruct (facets/file-format-spec
               (:constructor nil)
               (:constructor
                facets-make-file-format-spec))
  (name
   nil
   :documentation
   "The name of the file format."
   :type "string")
  (extensions
   nil
   :documentation
   "The extensions of the file format."
   :type "list of strings")
  (major-modes
   nil
   :documentation
   "The major modes of the file format."
   :type "list of symbols")
  (generate-attr-line-func
   nil
   :documentation
   "A function that generates an attribute line in the frontmatter.
It takes 2 arguments: ATTR and VALUE, and returns the attribute line.
ATTR and VALUE can be:

- `title', title string;
- `id', ID string;
- `tags', list of tags."
   :type "function: kind, value -> string")
  (write-attr-line-func
   nil
   :documentation
   "A function that writes an attribute line to current buffer.
If there's no frontmatter in current buffer, this should create
one.  If there's already an attribute line of the same attribute,
this should overwrite it."
   :type "function: string -> ()")
  (get-attr-func
   nil
   :documentation
   "A function that returns the attribute from the frontmatter.
It takes 1 argument: ATTR, and returns the attribute value. ATTR
and the returned value can be:

- `title' -> string of the title;
- `id' -> ID string;
- `tags' -> list of tags.

This function shouldn't assume the major mode is activated, as it
may be called in a general buffer with the file content inserted.
If the value doesn't exist, this should return nil rather than an
empty string."))

(defun facets/current-file-format-spec ()
  "Return the file format spec for current file.
If the current file extension or major mode is not supported,
return nil."
  (or (when-let ((ext (facets/current-file-extension)))
        (gethash ext facets/extension-spec-table))
      (when-let ((mode major-mode))
        (gethash mode facets/major-mode-spec-table))))

(defun facets/error-if-format-not-supported ()
  "Signal an error if current markup language is not supported."
  (unless (facets/current-file-format-spec)
    (user-error "Current markup language not supported")))

(defun facets/generate-attr-line (attr val)
  "Generate an attribute line for current buffer.
ATTR can be `title', `id' or `tags'.  VAL is its value, see
`facets/file-format-spec-generate-attr-line-func'."
  (when-let ((spec (facets/current-file-format-spec)))
    (funcall (facets/file-format-spec-generate-attr-line-func spec)
             attr val)))

(defun facets/write-attr-line (line)
  "Write LINE as an attribute line in current buffer."
  (when-let ((spec (facets/current-file-format-spec)))
    (funcall (facets/file-format-spec-write-attr-line-func spec)
             line)))

(defun facets/write-attr (attr val)
  "Write attribute line of ATTR with value VAL in current buffer."
  (facets/write-attr-line (facets/generate-attr-line attr val)))

(defun facets/get-attr (attr)
  "Get attribute ATTR in current buffer."
  (when-let ((spec (facets/current-file-format-spec)))
    (funcall (facets/file-format-spec-get-attr-func spec) attr)))

(defun facets/write-attr-in-file (file attr val)
  "Write attirbute ATTR with value VAL in FILE."
  (with-temp-file file
    (insert-file-contents file)
    (let ((buffer-file-name file))
      (facets/write-attr attr val))))

(defun facets/get-attr-of-file (file attr)
  "Get ATTR of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((buffer-file-name file))
      (facets/get-attr attr))))

;;;;; Find facets

(defun facets/all-facets (&optional dir)
  "Return the paths of all facets.
If DIR is non-nil, return the paths of all facets in the directory"
  (setq dir (or dir facets-directory))
  (directory-files-recursively
   dir facets/filename-regexp nil
   (lambda (subdir)
     (let ((parts (split-string (file-relative-name subdir dir)
                                (rx (or "/" "\\")))))
       (not (cl-find-if
             (lambda (p) (string-match-p facets-excluded-dirs-regexp p))
             parts))))))

;;;;; Find references

(defun facets/grep-find-references-cmd (id)
  "Build grep command for finding referenced to ID.
The returned value is a list, its car is the grep program, cdr is
a list of args.

The command should be run at `facets-directory'."
  (let* ((grep-kind (facets/grep-program-kind))
         (grep-prog (facets/grep-program))
         (extensions nil)
         (str (facets/id-to-link id))
         (pattern (pcase grep-kind
                    ('gnugrep (facets/gnugrep-regexp-quote str))
                    ('rg (facets/rg-regexp-quote str)))))
    (dolist (spec facets/file-format-specs)
      (setq extensions (append extensions
                               (facets/file-format-spec-extensions spec))))
    `(,grep-prog ,@(facets/grep-cmd pattern grep-kind 'location (facets/all-facets)))))

;;;; Markup Language Extensions

;;;;; Markdown

(defun facets/escape-yaml-string (str)
  "Escape STR to make it legal in a double-quoted yaml string."
  (dolist (pair '(("\\\\" . "\\\\\\")
                  ("\n" . "\\\\n")
                  ("\t" . "\\\\t")
                  ("\"" . "\\\\\"")))
    (setq str (replace-regexp-in-string (car pair) (cdr pair) str)))
  str)

(defun facets/unescape-yaml-string (str)
  "Unescape STR in a double-quotted yaml string.
This cannot parse a general yaml string.  It's only used to parse
string generated by `facets/escape-yaml-string'."
  (dolist (pair '(("\\\\\\\\" . "\\\\")
                  ("\\\\n" . "\n")
                  ("\\\\t" . "\t")
                  ("\\\\\"" . "\"")))
    (setq str (replace-regexp-in-string (car pair) (cdr pair) str)))
  str)

(defun facets/markdown-maybe-write-yaml-fences ()
  "Write yaml frontmatter fences if it doesn't exist."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min)))
    (unless (looking-at (rx (* "\n") "---" (* " ") eol))
      (insert "---\n---\n\n"))))

(defun facets/markdown-get-attr-lines-bounds ()
  "Get the bounds of attribute lines in a markdown buffer.
The returned value is (BEG . END).  BEG is the beginning of the
first attribute line, END is the beginning of the ending fences
of the frontmatter.

If the frontmatter is not presented, return nil."
  (let (beg end)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min)))
      (when (search-forward-regexp (rx bol "---" (* " ") "\n") nil 'noerror)
        (setq beg (point))
        (when (search-forward-regexp (rx bol "---") nil 'noerror)
          (setq end (match-beginning 0))
          (cons beg end))))))

(defun facets/markdown-generate-attr-line (attr value)
  "Generate attribute line of ATTR with value VALUE."
  (pcase attr
    ('id (format "facet-id: \"%s\"" (facets/escape-yaml-string value)))
    ('title (format "title: \"%s\"" (facets/escape-yaml-string value)))
    ('tags (format "keywords: [%s]" (string-join value ", ")))))

(defun facets/markdown-write-attr-line (line)
  "Write attribute line LINE in current buffer."
  (facets/markdown-maybe-write-yaml-fences)
  (let* ((attr (if (string-match
                    (rx string-start (group (+? not-newline)) ":") line)
                   (match-string 1 line)
                 (error "Invalid LINE: %s" line)))
         (bounds (facets/markdown-get-attr-lines-bounds))
         (beg (car bounds))
         (end (cdr bounds)))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char beg)
        (if (search-forward-regexp (rx bol (literal attr) ":") end 'noerror)
            (progn (goto-char (line-beginning-position))
                   (delete-region (point) (line-end-position))
                   (insert line))
          (goto-char beg)
          (pcase attr
            ("facet-id" nil)
            ("title"
             (if (search-forward-regexp (rx bol "facet-id:") beg 'noerror)
                 (forward-line 1)
               (goto-char beg)))
            ("tags"
             (if (or (search-forward-regexp (rx bol "title") beg 'noerror)
                     (search-forward-regexp (rx bol "facet-id:") beg 'noerror))
                 (forward-line 1)
               (goto-char beg)))
            (_ (goto-char end)))
          (insert line "\n"))))))

(defun facets/markdown-get-attr (attr)
  "Get the value of attribute ATTR in current buffer."
  (when-let ((bounds (facets/markdown-get-attr-lines-bounds))
             (attr (pcase attr
                     ('title "title")
                     ('id "facet-id")
                     ('tags "keywords"))))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (car bounds))
        (when (search-forward-regexp (rx bol (literal attr) ":")
                                     (cdr bounds) 'noerror)
          (let (result)
            (pcase attr
              ((or "title" "facet-id")
               ;; If the value is not wrapped by double quotes, we aren't
               ;; reading the value following YAML spec, but it should be fine
               ;; as `facets/markdown-write-attr-line' always wraps title and
               ;; ID in double quotes..
               (setq result (string-trim
                             (buffer-substring-no-properties
                              (point) (line-end-position))))
               (when (string-match (rx string-start
                                       "\"" (group (* not-newline)) "\""
                                       string-end)
                                   result)
                 (setq result (facets/unescape-yaml-string
                               (match-string 1 result))))
               (when (equal result "") (setq result nil)))
              ("keywords"
               (setq result (string-trim
                             (buffer-substring-no-properties
                              (point) (line-end-position))))
               (when (string-match (rx string-start
                                       "[" (group (* not-newline)) "]"
                                       string-end)
                                   result)
                 (if (equal (match-string 1 result) "")
                     (setq result nil)
                   (setq result (split-string (match-string 1 result) ", "))))))
            result))))))

(defvar facets/markdown-spec
  (facets-make-file-format-spec
   :name 'markdown
   :extensions facets-markdown-extensions
   :major-modes facets-markdown-major-modes
   :generate-attr-line-func #'facets/markdown-generate-attr-line
   :write-attr-line-func #'facets/markdown-write-attr-line
   :get-attr-func #'facets/markdown-get-attr))

(facets/register-file-format facets/markdown-spec)

;;;;; Org

(defun facets/org-generate-attr-line (attr value)
  "Generate attribute line of ATTR with value VALUE."
  (pcase attr
    ('id (format "#+FACET-ID: %s" value))
    ('title (format "#+TITLE: %s" value))
    ('tags (format "#+FILETAGS: :%s:" (string-join value ":")))))

;; Info node org > Exporting > Export Settings: "In-buffer settings may appear
;; anywhere in the file".
(defun facets/org-write-attr-line (line)
  "Write attribute line LINE in current file."
  (let* ((attr (if (string-match
                    (rx string-start "#+" (group (+? not-newline)) ":") line)
                   (match-string 1 line)
                 (error "Invalid LINE: %s" line))))
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (if (search-forward-regexp
             (rx bol "#+" (literal attr) ":") nil 'noerror)
            (progn (goto-char (line-beginning-position))
                   (delete-region (point) (line-end-position))
                   (insert line))
          (goto-char (point-min))
          (pcase attr
            ("FACET-ID" nil)
            ("TITLE"
             (if (search-forward-regexp (rx bol "#+FACET-ID:") nil 'noerror)
                 (forward-line 1)
               (goto-char (point-min))))
            ("FILETAGS"
             (if (or (search-forward-regexp (rx bol "#+TITLE:") nil 'noerror)
                     (search-forward-regexp
                      (rx bol "#+FACET-ID:") nil 'noerror))
                 (forward-line 1)
               (goto-char (point-min))))
            (_
             (if (search-forward-regexp
                  (rx (* "\n") (+ (seq bol "#+" (* not-newline) eol))))
                 (forward-line 1)
               (goto-char (point-min)))))
          (insert line "\n")
          (unless (or (and (eolp) (not (eobp)))
                      (looking-at "#+"))
            (insert "\n")))))))

(defun facets/org-get-attr (attr)
  "Get value of attribute ATTR in current buffer."
  (let ((attr (pcase attr
                ('title "TITLE")
                ('id "FACET-ID")
                ('tags "FILETAGS")))
        result)
    (save-excursion
      (save-restriction
        (widen)
        (goto-char (point-min))
        (when (search-forward-regexp (rx bol "#+" (literal attr) ":")
                                     nil 'noerror)
          (pcase attr
            ((or "TITLE" "FACET-ID")
             (setq result
                   (string-trim
                    (buffer-substring-no-properties
                     (point) (line-end-position))))
             (when (equal result "") (setq result nil)))
            ("FILETAGS"
             (setq result
                   (string-trim
                    (buffer-substring-no-properties
                     (point) (line-end-position))))
             (if (member result '("" "::"))
                 (setq result nil)
               (setq result
                     (split-string
                      (string-trim (buffer-substring-no-properties
                                    (point) (line-end-position)))
                      ":" 'omit-nulls)))))
          result)))))

(defvar facets/org-spec
  (facets-make-file-format-spec
   :name 'org
   :extensions facets-org-extensions
   :major-modes facets-org-major-modes
   :generate-attr-line-func #'facets/org-generate-attr-line
   :write-attr-line-func #'facets/org-write-attr-line
   :get-attr-func #'facets/org-get-attr))

(facets/register-file-format facets/org-spec)

;;;; Commands/UI

;;;;; Facets Mode

(defun facets/trim-buffer-name ()
  "Trim the ID in current buffer name."
  (when-let* ((file-name (buffer-file-name)))
    (rename-buffer (facets/filename-sans-id file-name))))

(defun facets/eldoc-call (callback)
  "Eldoc backend for `facets-mode'.
It calls CALLBACK in an idle timer."
  (if-let (id (facets/link-id-at-point))
      (run-with-idle-timer
       0 nil
       (lambda (id)
         (when-let ((facets (facets/get-id-files id)))
           (funcall callback
                    (file-relative-name (car facets) facets-directory)
                    :thing id
                    :face 'font-lock-comment-face)))
       id)
    nil))

(defvar-local facets/eldoc-mode-enabled-orig nil
  "Whether `eldoc-mode' is enabled before `facets-mode' is enabled.")

;;;###autoload
(define-minor-mode facets-mode
  "Minor mode for highlighting facet links."
  :lighter " Facets"
  :keymap facets-mode-map
  (cond
   (facets-mode
    (jit-lock-register #'facets/refontify)
    (add-hook 'eldoc-documentation-functions
              #'facets/eldoc-call nil t)
    (add-hook 'before-save-hook
              #'facets-sync-file-name nil t)
    (add-hook 'after-change-major-mode-hook
              #'facets-mode nil t)
    (setq facets/eldoc-mode-enabled-orig eldoc-mode)
    (facets/trim-buffer-name)
    (eldoc-mode))
   (t
    (jit-lock-unregister #'facets/refontify)
    (remove-hook 'eldoc-documentation-functions
                 #'facets/eldoc-call t)
    (remove-hook 'before-save-hook
                 #'facets-sync-file-name t)
    (remove-hook 'after-change-major-mode-hook
                 #'facets-mode t)
    (rename-buffer (file-name-nondirectory (buffer-file-name)))
    (unless facets/eldoc-mode-enabled-orig
      (eldoc-mode -1))
    (save-restriction
      (widen)
      (facets/unfontify (point-min) (point-max))))))

;;;###autoload
(defun facets-auto-enable-facets-mode ()
  "Enable `facets-mode' for files under `facets-directory'.
File formats not supported by facets are not affected.

Add this to `find-file-hook' to enable `facets-mode' for facets
automatically."
  (when (and (file-in-directory-p (buffer-file-name) facets-directory)
             (facets/parse-filename (buffer-file-name)))
    (facets-mode)))

;;;;; File management

;;;###autoload
(defun facets-find-facet ()
  "Open a facet.
This prompts you for a directory, then you can pick a facet in
it, including all its subdirectories."
  (interactive)
  (let* ((dir (read-directory-name "Dir: " facets-directory))
         (cands (mapcar (lambda (f)
                          (cons (file-name-nondirectory f)
                                f))
                        (facets/all-facets dir))))
    (find-file (cdr (assoc (completing-read "Facet: " cands nil t)
                           cands #'equal)))))

;;;###autoload
(defun facets-move-file-into-subdir ()
  "Move a file into subdir.
This is used for markup files that refer to other resources (like
images) so it's better to put it into a dedicated directory,
together with the resources."
  (interactive)
  (let* ((buf (current-buffer))
         (this-file (buffer-file-name))
         (dir (if this-file (file-name-directory this-file) facets-directory))
         (file (read-file-name
                "File: " dir nil t
                (when this-file (file-relative-name this-file dir))))
         (subdir (read-directory-name
                  "Subdir: "
                  (file-name-directory file) nil nil
                  (nth 1 (facets/parse-filename file))))
         (newname (expand-file-name (file-name-nondirectory file) subdir)))
    (make-directory subdir t)
    (rename-file file newname)
    (when this-file
      (with-current-buffer buf (set-visited-file-name newname nil t)))))

;;;###autoload
(defun facets-rename-file ()
  "Rename file in the facet format."
  (interactive)
  (let* ((this-file (buffer-file-name))
         (dir (if this-file (file-name-directory this-file) facets-directory))
         (file (read-file-name
                "File: " dir nil t
                (when this-file (file-relative-name this-file dir))))
         (filebuf (find-buffer-visiting file))
         newname)
    (when (or (not (member (file-name-extension file)
                           (hash-table-keys facets/extension-spec-table)))
              (y-or-n-p "This file is a supported markup file.  It's \
suggested to use `facets-sync-file-name' instead.  Really continue? "))
      (pcase-let ((`(,id ,title ,tags ,ext) (facets/parse-filename file)))
        (setq ext (or ext (file-name-extension file)))
        (setq title (facets/read-title file))
        (setq tags (facets/read-tags tags))
        (when (or (null id)
                  (y-or-n-p "This file already contains a ID.  Modify the ID? "))
          (setq id (facets/read-id file)))
        (setq newname (facets/make-filename id title tags ext))
        (setq newname (expand-file-name newname (file-name-directory file)))
        (rename-file file newname)
        (when filebuf
          (with-current-buffer filebuf
            (set-visited-file-name newname nil t)))))))

;;;###autoload
(defun facets-sync-file-name ()
  "Sync the current file name with the frontmatter."
  (interactive)
  (pcase-let* ((buf (current-buffer))
               (file (buffer-file-name))
               (`(,id ,title ,tags ,ext) (when file (facets/parse-filename file)))
               (attr-id (facets/get-attr 'id))
               (attr-title (facets/get-attr 'title))
               (attr-tags (facets/get-attr 'tags))
               (newname))
    (setq title (or attr-title title (facets/read-title file)))
    (setq tags (or attr-tags tags))
    (when (or (null id)
              (and attr-id
                   (not (equal id attr-id))
                   (y-or-n-p "ID in frontmatter is not the ID in the filename.  Sync ID? ")))
      (setq id (or attr-id id (facets/read-id file))))
    (setq ext (or ext (file-name-extension file)))
    (setq newname (facets/make-filename id title tags ext))
    (setq newname (expand-file-name newname (file-name-directory file)))
    (unless (equal file newname)
      (rename-file file newname)
      (with-current-buffer buf
        (set-visited-file-name newname nil t)))))

;;;###autoload
(defun facets-save-file ()
  "Save current file/buffer as a facet."
  (interactive)
  (if (buffer-file-name)
      (facets-sync-file-name)
    (let* ((buf (current-buffer))
           (id (facets/get-attr 'id))
           (title (facets/get-attr 'title))
           (tags (facets/get-attr 'tags))
           dir filename ext)
      (setq title (or title (facets/read-title)))
      (setq id (or id (facets/read-id)))
      (setq ext (or (when-let (spec (facets/current-file-format-spec))
                      (car (facets/file-format-spec-extensions spec)))
                    (read-string "File extension: ")))
      (when (equal ext "") (setq ext nil))
      (setq dir (read-directory-name "Save in directory: " facets-directory))
      (setq filename (facets/make-filename id title tags ext))
      (setq filename (expand-file-name filename dir))
      (with-current-buffer buf
        ;; `set-visited-file-name' will discard buffer-local
        ;; `write-file-functions'.  Restore it.
        (let ((write-file-functions-orig write-file-functions))
          (set-visited-file-name filename)
          (setq write-file-functions write-file-functions-orig)))))
  (save-buffer)
  (when facets-mode
    (facets/trim-buffer-name)))

;;;;; Create facets

;;;###autoload
(defun facets-new-facet ()
  "Create a new facet."
  (interactive)
  (let ((buf (generate-new-buffer "untitled facet")))
    (switch-to-buffer buf)
    (when facets-default-mode (funcall facets-default-mode))
    (setq buffer-offer-save t)
    (run-hooks 'facets-new-facet-hook)))

;;;###autoload
(defun facets-insert-frontmatter ()
  "Insert frontmatter in current buffer."
  (interactive)
  (facets/error-if-format-not-supported)
  (facets/write-attr
   'id (facets/generate-id
        (if (buffer-file-name)
            (facets/file-modification-time (buffer-file-name))
          (current-time))))
  (facets/write-attr 'title "")
  (facets/write-attr 'tags ""))

;;;;; Edit

;;;###autoload
(defun facets-edit-id ()
  "Edit ID in the frontmatter in current buffer."
  (interactive)
  (facets/error-if-format-not-supported)
  (facets/write-attr
   'id
   (facets/read-id (buffer-file-name))))

;;;###autoload
(defun facets-edit-tags ()
  "Edit tags in the frontmatter in current buffer."
  (interactive)
  (facets/error-if-format-not-supported)
  (facets/write-attr
   'tags
   (facets/read-tags (facets/get-attr 'tags))))

;;;###autoload
(defun facets-insert-link ()
  "Pick a facet and insert a link pointing to it."
  (interactive)
  (let* ((facet (completing-read
                 "Facet: "
                 (mapcar (lambda (f) (file-relative-name f facets-directory))
                         (facets/all-facets))))
         (id (car (facets/parse-filename facet))))
    (when id (insert (format "facet:%s" id)))))

;;;###autoload
(defun facets-copy-id-as-link ()
  "Copy the id of the current file as a link."
  (interactive)
  (if-let ((id (or (when (buffer-file-name)
                     (car (facets/parse-filename (buffer-file-name))))
                   (facets/get-attr 'id))))
      (kill-new (concat "facet:" id))
    (user-error "Facet ID not found for current buffer")))

;;;;; Reading

(defvar facets/marker-ring (make-ring 50)
  "The marker ring used by `facets-follow-link'.")

;;;###autoload
(defun facets-follow-link ()
  "Find the source of link at point."
  (interactive)
  (let* ((id (or (facets/link-id-at-point)
                 (user-error "No link at point")))
         (files (or (facets/get-id-files id)
                    (user-error "Facet not found")))
         (marker (point-marker)))
    (ring-insert facets/marker-ring marker)
    (if (eq (length files) 1)
        (find-file (car files))
      (let ((cands (mapcar (lambda (f) (file-relative-name f facets-directory))
                           files)))
        (find-file (expand-file-name (completing-read
                                      "Pick a source: " cands nil t)
                                     facets-directory))))))

(defun facets-jump-back ()
  "Go back to the position before last `facets-follow-link'."
  (interactive)
  (let ((ring facets/marker-ring))
    (when (ring-empty-p ring)
      (user-error "No more previous history"))
    (let ((marker (ring-remove ring 0)))
      (switch-to-buffer
       (or (marker-buffer marker)
           (user-error "The previous buffer has been deleted")))
      (goto-char (marker-position marker))
      (set-marker marker nil))))

;; TODO: Pick a file so we can list references to non-text-file facets.
;;;###autoload
(defun facets-list-references ()
  "List links to the id of current file."
  (interactive)
  (unless (buffer-file-name)
    (user-error "Current buffer is not visiting a file"))
  (let* ((id (or (car (facets/parse-filename (buffer-file-name)))
                 (user-error "ID not found for current file")))
         (cmd (facets/grep-find-references-cmd id))
         (buf (generate-new-buffer "*Facets Link Locations*"))
         success-flag)
    (with-current-buffer buf
      (setq default-directory facets-directory)
      (facets/run-process (car cmd) (cdr cmd))
      (unless (eq (point-min) (point-max))
        (setq success-flag t)
        (grep-mode)))
    (if success-flag
        (pop-to-buffer
         buf
         '(display-buffer-use-some-window . ((inhibit-same-window . t)
                                             (inhibit-switch-frame . t))))
      (kill-buffer buf)
      (user-error "No references found for this facet"))))

;;;;; Misc

(defun facets/update-old-id-format ()
  "Update old ID and link format.
Previously, facets.el uses a different ID and link format without
specific file name scheme.  Run this in an old facet file to
update it."
  (interactive)
  (widen)
  (goto-char (point-min))
  (while (re-search-forward
          (rx (opt "[[")
              (group (opt "facet:"))
              (group (= 4 digit)) "-" (group (= 2 digit)) "-" (group (= 2 digit)) "/"
              (group (= 2 digit)) ":" (group (= 2 digit)) ":" (group (= 2 digit))
              (opt "]]"))
          nil t)
    (replace-match "\\1\\2\\3\\4T\\5\\6\\7")))

(provide 'facets)

;;; facets.el ends here
