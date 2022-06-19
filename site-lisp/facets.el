;;; facets.el --- My zettlekasten -*- lexical-binding: t -*-

;; Copyright (C) 2022 Hao Wang
;; License: GPL v3, or (at your option) any later version

;;; Commentary:

;; Every card is a facet of a grand design.

;;; Code:

;; To see the outline of this file, run `outline-minor-mode', then
;; `outline-hide-body'.  Another way is to run `occur' with the query:
;; ^;;;;* \|^(

;;;; Libraries

(require 'cl-lib)
(require 'ring)
(require 'rx)

;;;; User options

(defvar facets-directory "~/facets/"
  "The directory for facets.")

(defvar facets-id-function #'facets/timestamp
  "A function that returns a new ID string.
Timestamp is a good choice because it's nearly unique.")

(defvar facets-link-regexp
  (cons (rx "[[facet:"
            (group (* (not (any "[" "]"))))
            "]]")
        1)
  "Regexp to match links.
This is a cons pair.  The car is a regexp and it must contain a
group that matches the id inside the link, and the cdr is the
number of the group.")

(defvar facets-id-to-link-function #'facets/default-id-to-link
  "A function that convers ID to a link.")

(defvar facets-grep-program-debug nil
  "Nil to silent grep errors.")

;; TODO: refactor this to a list in the precedence order.

(defvar facets-grep-program-kind 'rg
  "The grep program.
Can be `gnugrep' or `rg'.  Unless `facets-grep-program' is
specified, when this is set to `rg' and the rg executable is not
found, GNU grep is used as fallback.")

(defvar facets-grep-program nil
  "The path of the grep program.
Or it can be the program name if it's in PATH.")

(defvar facets-markdown-extensions '("md")
  "Markdown file extensions.")

(defvar facets-org-extensions '("org")
  "Org file extensions.")

(defvar facets-link-keymap
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") 'facets-find-source)
    (define-key map (kbd "<mouse-1>") 'facets-find-source)
    map)
  "Keymap that's enabled on links.")

(defvar facets-mode-map (make-sparse-keymap)
  "Keymap used in `facets-mode'.")

;;;; Internals

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

;;;;; Editing

(defmacro facets/save-excursion-and-goto-start (&rest args)
  "Run ARGS with `save-excursion' and point at the start of widened buffer."
  `(save-excursion
     (save-restriction
       (widen)
       (goto-char 1)
       ,@args)))

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

(defun facets/glob-of-extensions (extensions)
  "Return a glob pattern that matches file with EXTENSIONS."
  (if (eq (length extensions) 1)
      (concat "*." (car extensions))
    (concat "*.{" (string-join extensions ",") "}")))

(defvar facets/gnugrep-common-args
  '("--recursive" "--extended-regexp" "--color=never" "--exclude-dir=.git")
  "Common args used for GNU grep commands.")

(defvar facets/rg-common-args
  '("--color=never")
  "Common args used for rg commands.")

(defun facets/gnugrep-glob-arg (extensions)
  "Build glob args that matches file with EXTENSIONS for GNU grep."
  (list (concat "--include="
                (facets/glob-of-extensions extensions))))

(defun facets/rg-glob-arg (extensions)
  "Build glob args that matches file with EXTENSIONS for rg."
  (list (concat "--glob="
                (facets/glob-of-extensions extensions))))

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

;;;;; File

(defun facets/current-file-extension ()
  "Return the extension of current file.
Signal an error if current buffer is not a file buffer."
  (if-let ((file (buffer-file-name)))
      (file-name-extension file)
    (user-error "Current buffer is not visiting a file")))

;;;;; ID

(defun facets/timestamp ()
  "Return a timestamp."
  (format-time-string "%F/%T" (current-time)))

(defun facets/generate-id ()
  "Return a new facet ID."
  (funcall facets-id-function))

;;;;; Link

(defun facets/default-id-to-link (id)
  "Default function to convert ID to link."
  (concat "[[facet:" id "]]"))

(defun facets/id-to-link (id)
  "Convert ID to link."
  (funcall facets-id-to-link-function id))

(defun facets/link-id-bounds-at-point ()
  "Get the bounds of id in the link at point.
The bounds is returned as a cons pair."
  (save-excursion
    (let ((pt (point))
          (regexp (car facets-link-regexp))
          (group (cdr facets-link-regexp)))
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
              (facets (facets-get-facets-by-id id)))
    (file-relative-name (car facets) facets-directory)))

;; Fontification functions are borrowed from clue.

(defun facets/unfontify (beg end)
  "Remove fontification of links between BEG and END."
  (dolist (ov (overlays-in beg end))
    (when (overlay-get ov 'facets-link-p)
      (delete-overlay ov))))

(defun facets/fontify (beg end)
  "Fontify the links between BEG and END."
  (save-excursion
    (goto-char beg)
    (while (re-search-forward (car facets-link-regexp) end t)
      (let* ((b (match-beginning 0))
             (e (match-end 0))
             ov)
        (setq ov (make-overlay b e))
        (overlay-put ov 'facets-link-p t)
        (overlay-put ov 'face 'button)
        (overlay-put ov 'mouse-face 'link)
        (overlay-put ov 'evaporate t)
        (overlay-put ov 'help-echo #'facets/link-help-echo-func)
        ;; Fixing the behavior of pressing RET just before the link.
        (overlay-put (make-overlay (1+ b) e) 'keymap facets-link-keymap)))))

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

;;;; Internals APIs

;;;;; Markup Language Extensions

(defvar facets/extension-spec-table
  (make-hash-table :test #'equal)
  "File extension -> file format spec.")

(defvar facets/file-format-specs nil
  "List of file format specs.")

(defun facets-register-file-format (spec)
  "Register file format spec SPEC."
  (let ((exts (facets/file-format-spec-extensions spec)))
    (dolist (ext exts)
      (puthash ext spec facets/extension-spec-table)))
  (unless (memq (facets/file-format-spec-name spec)
                (mapcar #'facets/file-format-spec-name
                        facets/file-format-specs))
    (push spec facets/file-format-specs)))

;; TODO: add major-modes so it works in a not-already-saved buffer.
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
  (generate-id-line-func
   nil
   :documentation
   "A function that takes the ID and returns an ID line.
Every facet in this file format should have an ID line that
contains its ID.  The returned value should not contain the
newline."
   :type "function: string -> string")
  (file-id-bounds-func
   nil
   :documentation
   "A function that returns the bounds of ID of current file.
The ID is contained in the ID line.  When there's none, it should
return None.

The function is called with the buffer widen, the point at the
beginning of the buffer, and `save-excursion'."
   :type "function: () -> a cons pair")
  (insert-id-line-func
   nil
   :documentation
   "A function that inserts the ID line in a facet.
When it's called, the current buffer is a facet that doesn't have
an ID line.  It should take the ID line (without the final
newline) and inserts it to current ibuffer.

The function is called with the buffer widen, the point at the
beginning of the buffer, and `save-excursion'."
   :type "function: str -> any")
  (gnugrep-regexp-func
   nil
   :documentation
   "A function that converts an ID to a GNU grep regexp that matches ID lines.
The --extended-regexp option is enabled.")
  (rg-regexp-func
   nil
   :documentation
   "A function that converts an ID to an rg regexp that matches ID lines.
The default regexp engine (Rust's regex enging) is used."))

(defun facets/current-file-format-spec ()
  "Return the file format spec for current file.
Signal an error if current buffer is not visiting a file, or the
extension is not supported."
  (if-let* ((ext (facets/current-file-extension))
            (spec (gethash ext facets/extension-spec-table)))
      spec
    (error "File extension \"%s\" is not supported" ext)))

;;;;; Grep Helpers

(defun facets-gnugrep-regexp-quote (str)
  "Return a regexp that matches STR in GNU grep pattern.
It's assumed that POSIX extended regular expressions (ERE) is
begin used.  It can be enabled by the -E flag in GNU grep."
  (replace-regexp-in-string
   (rx (or "(" ")" "[" "]" "{" "}" "." "*" "+" "^" "$" "|" "?" "\\"))
   "\\\\\\&" str))

(defun facets-rg-regexp-quote (str)
  "Return a regexp that matches STR in rg pattern.
It's assumed that the default regex engine of rg (Rust's regex
engine) is being used."
  (replace-regexp-in-string
   ;; Taken from `is_meta_character' in regex/regex-syntax/src/lib.rs.
   (rx (or "\\" "." "+" "*" "?" "(" ")" "|" "[" "]" "{" "}"
           "^" "$" "#" "&" "-" "~"))
   "\\\\\\&" str))

(defun facets-grep-cmd (pattern grep-kind extensions output-kind)
  "Build grep command.
The matched files has extension in EXTENSIONS and the content
matches PATTERN.  GREP-KIND can be `gnugrep' or `rg'.  The
returned value is a list of args.

OUTPUT-KIND can be `file' or `location'.  It specifies whether to
output only the file names or the whole locations.

The command should be run at `facets-directory'."
  (let* (common-args glob-args output-args regexp-args)
    (pcase grep-kind
      ('rg
       (setq common-args facets/rg-common-args)
       (setq glob-args (facets/rg-glob-arg extensions))
       (setq output-args (pcase output-kind
                           ('file facets/rg-show-file-args)
                           ('location facets/rg-show-location-args)
                           (_ (error "Invalid OUTPUT_KIND"))))
       (setq regexp-args (facets/rg-pattern-arg pattern)))
      ('gnugrep
       (setq common-args facets/gnugrep-common-args)
       (setq glob-args (facets/gnugrep-glob-arg extensions))
       (setq output-args (pcase output-kind
                           ('file facets/gnugrep-show-file-args)
                           ('location facets/gnugrep-show-location-args)
                           (_ (error "Invalid OUTPUT_KIND"))))
       (setq regexp-args (facets/gnugrep-pattern-arg pattern)))
      (_ (error "Invalid grep program kind")))
    `(,@common-args ,@glob-args ,@output-args ,@regexp-args)))

(defun facets-grep-find-source-cmd (id)
  "Build grep command for finding sources of ID.
The returned value is a list, its car is the grep program, and
each remaining element is a list of args.  The caller need to run
grep multiple times using these args to find sources for all
supported file formats.

The command should be run at `facets-directory'."
  (let* ((grep-kind (facets/grep-program-kind))
         (grep-prog (facets/grep-program))
         (args nil))
    (dolist (spec facets/file-format-specs)
      (let ((pattern
             (pcase grep-kind
               ('gnugrep (funcall
                          (facets/file-format-spec-gnugrep-regexp-func spec)
                          id))
               ('rg (funcall
                     (facets/file-format-spec-rg-regexp-func spec)
                     id))))
            (extensions (facets/file-format-spec-extensions spec)))
        (push (facets-grep-cmd pattern grep-kind extensions 'file) args)))
    `(,grep-prog ,@args)))

(defun facets-grep-find-references-cmd (id)
  "Build grep command for finding sources of ID.
The returned value is a list, its car is the grep program, cdr is
a list of args.

The command should be run at `facets-directory'."
  (let* ((grep-kind (facets/grep-program-kind))
         (grep-prog (facets/grep-program))
         (extensions nil)
         (str (facets/id-to-link id))
         (pattern (pcase grep-kind
                    ('gnugrep (facets-gnugrep-regexp-quote str))
                    ('rg (facets-rg-regexp-quote str)))))
    (dolist (spec facets/file-format-specs)
      (setq extensions (append extensions
                               (facets/file-format-spec-extensions spec))))
    `(,grep-prog ,@(facets-grep-cmd pattern grep-kind extensions 'location))))

;;;;; Find Facets

(defun facets-get-facets-by-id (id)
  "Return the facets with id ID."
  (let* ((cmd (facets-grep-find-source-cmd id))
         (default-directory facets-directory)
         (files nil))
    (dolist (args (cdr cmd))
      (when-let ((output (facets/get-grep-process-output (car cmd) args)))
        (setq files (nconc files
                           (split-string output "\n" 'omit-nulls)))))
    (cl-remove-duplicates files :test #'equal)))

;;;;; ID related

(defun facets-generate-id-line (id)
  "Generate an ID line for current file."
  (let* ((spec (facets/current-file-format-spec)))
    (funcall (facets/file-format-spec-generate-id-line-func spec) id)))

(defun facets-current-file-id-bounds ()
  "Return the bound of ID of curren file."
  (when-let* ((spec (facets/current-file-format-spec))
              (id-bounds
               (facets/save-excursion-and-goto-start
                (funcall (facets/file-format-spec-file-id-bounds-func
                          spec)))))
    id-bounds))

(defun facets-goto-id-line ()
  "Goto the ID line in current file."
  (when-let* ((point-min (point-min))
              (id-start (car (facets-current-file-id-bounds))))
    (if (< id-start point-min)
        (progn (widen) (goto-char id-start))
      (goto-char id-start))))

(defun facets-insert-id-line (id-line)
  "Insert ID-LINE in current file."
  (let* ((spec (facets/current-file-format-spec)))
    (facets/save-excursion-and-goto-start
     (funcall (facets/file-format-spec-insert-id-line-func spec) id-line))))

(defun facets-current-file-id ()
  "Get the ID of current file."
  (when-let ((bounds (facets-current-file-id-bounds)))
    (buffer-substring-no-properties (car bounds) (cdr bounds))))

(defun facets-file-id (file)
  "Get the ID of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (let ((buffer-file-name file))
      (facets-current-file-id))))

;;;; Markup Language Extensions

;;;;; Markdown

(defun facets/markdown-generate-id-line (id)
  "Convert ID to ID line in markdown files."
  (concat "facet-id: " id))

(defun facets/markdown-file-id-bounds ()
  "Find the bounds of the ID in the ID line in markdown files."
  (when (re-search-forward (rx bol "facet-id:" (* space)
                               (group (* not-newline))
                               (* space) eol)
                           nil t)
    (cons (match-beginning 1)
          (match-end 1))))

(defun facets/markdown-insert-id-line (id-line)
  "Insert ID-LINE to a markdown buffer."
  (if (looking-at (rx (* "\n") "---" eol))
      (progn (goto-char (match-end 0))
             (if (eq (char-after) ?\n)
                 (progn (forward-char)
                        (if (save-excursion (re-search-forward
                                             (rx bol "---" eol) nil t))
                            (insert id-line "\n")
                          (insert id-line "\n" "---" "\n")))
               (insert "\n" id-line "\n" "---" "\n")))
    (insert "---" "\n" id-line "\n" "---" "\n")))

(defun facets/markdown-id-line-gnugrep-pattern (id)
  "Search pattern of ID line in markdown using GNU grep."
  (concat "^facet-id:[[:space:]]*"
          (facets-gnugrep-regexp-quote id)
          "[[:space:]]*$"))

(defun facets/markdown-id-line-rg-pattern (id)
  "Search pattern of ID line in markdown using rg."
  (concat "^facet-id:\\s*"
          (facets-rg-regexp-quote id)
          "\\s*$"))

(defvar facets/markdown-spec
  (facets-make-file-format-spec
   :name 'markdown
   :extensions facets-markdown-extensions
   :generate-id-line-func #'facets/markdown-generate-id-line
   :file-id-bounds-func #'facets/markdown-file-id-bounds
   :insert-id-line-func #'facets/markdown-insert-id-line
   :gnugrep-regexp-func #'facets/markdown-id-line-gnugrep-pattern
   :rg-regexp-func #'facets/markdown-id-line-rg-pattern))

(facets-register-file-format facets/markdown-spec)

;;;;; Org

;; I still don't know if this is the best way to include metadata in an org
;; file.

(defun facets/org-generate-id-line (id)
  "Convert ID to ID line in org files."
  (concat "#+FACET-ID: " id))

(defun facets/org-file-id-bounds ()
  "Find the bounds of the ID in the ID line in org files."
  (when (re-search-forward (rx bol "#+FACET-ID:" (* space)
                               (group (* not-newline))
                               (* space) eol)
                           nil t)
    (cons (match-beginning 1)
          (match-end 1))))

(defun facets/org-insert-id-line (id-line)
  "Insert ID-LINE to a org buffer."
  (insert id-line "\n")
  (unless (eq (char-after) ?\n)
    (insert "\n")))

(defun facets/org-id-line-gnugrep-pattern (id)
  "Search pattern of ID line in org using GNU grep."
  (concat "^#\\+FACET-ID:[[:space:]]*"
          (facets-gnugrep-regexp-quote id)
          "[[:space:]]*$"))

(defun facets/org-id-line-rg-pattern (id)
  "Search pattern of ID line in org using rg."
  (concat "^#\\+FACET-ID:\\s*"
          (facets-rg-regexp-quote id)
          "\\s*$"))

(defvar facets/org-spec
  (facets-make-file-format-spec
   :name 'org
   :extensions facets-org-extensions
   :generate-id-line-func #'facets/org-generate-id-line
   :file-id-bounds-func #'facets/org-file-id-bounds
   :insert-id-line-func #'facets/org-insert-id-line
   :gnugrep-regexp-func #'facets/org-id-line-gnugrep-pattern
   :rg-regexp-func #'facets/org-id-line-rg-pattern))

(facets-register-file-format facets/org-spec)

;;;; Commands/UI

;;;;; Facets Mode

(defun facets/eldoc-call (callback)
  "Eldoc backend for `facets-mode'.
It calls CALLBACK in an idle timer."
  (if-let (id (facets/link-id-at-point))
      (run-with-idle-timer
       0 nil
       (lambda (id)
         (when-let ((facets (facets-get-facets-by-id id)))
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
    (setq facets/eldoc-mode-enabled-orig eldoc-mode)
    (eldoc-mode))
   (t
    (jit-lock-unregister #'facets/refontify)
    (remove-hook 'eldoc-documentation-functions
                 #'facets/eldoc-call t)
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
  (when (file-in-directory-p (buffer-file-name) facets-directory)
    (facets-mode 1)))

;;;;; Edit

;;;###autoload
(defun facets-find-file ()
  "Run `find-file' in `facets-directory'.
This can be used to open a facet or create a new facet."
  (interactive)
  (let ((default-directory facets-directory))
    (call-interactively #'find-file)))

;;;###autoload
(defun facets-insert-or-update-id ()
  "Generate a facet ID and put it in the current file.
If there's already one, update it."
  (interactive)
  (let ((id-line (facets-generate-id-line (facets/generate-id))))
    (facets/save-excursion-and-goto-start
     (if (facets-goto-id-line)
         (progn (beginning-of-line)
                (delete-region (point) (line-end-position))
                (insert id-line))
       (facets-insert-id-line id-line)))))

;;;###autoload
(defun facets-insert-link ()
  "Pick a facet and insert a link pointing to it."
  (interactive)
  (if-let* ((directory (if (file-in-directory-p default-directory
                                                facets-directory)
                           default-directory facets-directory))
            (facet (read-file-name "Pick a facet: " directory nil t))
            (id (facets-file-id facet)))
      (insert (facets/id-to-link id))
    (user-error "Selected file doesn't contain an ID line")))

;;;###autoload
(defun facets-copy-id ()
  "Copy the id of the link if there's one at point, or of the current file.
Return t if success, otherwise return nil."
  (interactive)
  (if-let ((bounds (facets-current-file-id-bounds)))
      (progn (kill-new (concat "facet-id:"
                               (buffer-substring-no-properties
                                (car bounds) (cdr bounds))))
             (message "ID of current facet copied"))
    (user-error "Facet ID not found for current buffer")))

;;;###autoload
(defun facets-copy-id-or-region ()
  "Copy the id of the link if there's one at point, or of the current file.
When there's an active region, copy it instead.

It may be convenient to bind this to the keybinding of
`kill-ring-save' in `facets-mode-map'."
  (interactive)
  (if (use-region-p)
      (call-interactively #'kill-ring-save)
    (facets-copy-id)))

;;;;; Finding Source

(defvar facets/marker-ring (make-ring 50)
  "The marker ring used by `facets-find-sources'.")

;;;###autoload
(defun facets-find-source ()
  "Find the source of link at point."
  (interactive)
  (let ((default-directory facets-directory)
        (id (or (facets/link-id-at-point)
                (user-error "No link at point")))
        (marker (point-marker))
        src-list)
    (setq src-list (or (facets-get-facets-by-id id)
                       (user-error "Source not found")))
    (ring-insert facets/marker-ring marker)
    (if (eq (length src-list) 1)
        (find-file (car src-list))
      (find-file (completing-read
                  "Pick a source: " src-list nil t)))))

(defun facets-jump-back ()
  "Go back to the position before last `facets-find-source'."
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

;;;;;; Finding Links

;;;###autoload
(defun facets-list-links ()
  "List links to the id of current file."
  (interactive)
  (let* ((buf (generate-new-buffer "*Facets Link Locations*"))
         id cmd success-flag)
    (or (setq id (facets-current-file-id))
        (user-error "ID not found for current file"))
    (setq cmd (facets-grep-find-references-cmd id))
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
      (user-error "No links found for this file"))))

(provide 'facets)

;;; facets.el ends here
