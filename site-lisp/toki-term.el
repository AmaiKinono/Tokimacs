;;; toki-term.el --- Terminal emulator -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Hao Wang
;; License: GPL v3, or (at your option) any later version

;;; Commentary:

;; A better terminal emulator based on term.el

;;; Code:

;; To see the outline of this file, run `outline-minor-mode', then
;; `outline-hide-body'.  Another way is to run `occur' with the query:
;; ^;;;;* \|^(

;;;; Libraries

(require 'term)
(require 'project)

;;;; User options

(defvar toki-term-shell-program nil
  "Shell program for `toki-term'.
We need this because we may use a lightweight shell like dash for
inferior shell, but want bash/zsh for interactive shell.")

(defvar toki-term-project-root-function
  (lambda ()
    (when-let ((project (project-current nil)))
      (expand-file-name (cdr project))))
  "A function that returns project root for current buffer.")

(defvar toki-term-escape-keys
  '("M-x")
  "Escape keys for `toki-term'.
Notice that if you enable minor modes in the term, keys defined
by them override `term-raw-map', so they also behave like they
are escaped.

After you set this, call `toki-term-setup-escape-keys'.")

(defvar toki-term-bracketed-paste-programs
  '("julia")
  "Use bracketed paste for these programs running in term.
This influences `toki-term-yank' and
`toki-term-send-region-to-visible-term'.

The main effect of bracketed paste is changing the behavior of
newline char in REPLs. Some REPLs evaluates the user input even
if the expression is incomplete. By turning on bracketed paste,
the terminal makes the REPL know the difference of a user
inputted newline and a pasted/sended newline.

The rule of thumb is: if you notice wrong/strange behavior when
sending multiple expressions to a REPL, see if adding the program
name in this list fixes that.")

;;;; term.el tweaks

(defun toki-term-remove-escape-char ()
  "Undo change by `term-set-escape-char'."
  (when term-escape-char
    (define-key term-raw-map term-escape-char 'term-send-raw)
    (setq term-escape-char nil)))

(defun toki-term-setup-escape-keys ()
  "Set keys in `toki-term-escape-keys' as escape chars in term."
  (dolist (key toki-term-escape-keys)
    (define-key term-raw-map (kbd key) nil)))

(with-eval-after-load 'term
  ;; In term.el, `C-c' is explicitely escaped, in the top level scope, using
  ;; `term-set-escape-char'.  It's a useless API because it undoes its previous
  ;; call, so you can't escape multiple chars (`ansi-term' works around it
  ;; using a hack).  We undo it here because we have `toki-term-escape-keys'
  ;; where you can specify more than 1 escaped keys.
  (toki-term-remove-escape-char)
  (toki-term-setup-escape-keys))

;;;; Internals

(defconst toki-term/open-bracket "\033[200~")
(defconst toki-term/close-bracket "\033[201~")

(defun toki-term/get-term-buffer (&optional dir)
  "Get a term buffer for DIR.
That's the first buffer in `buffer-list' that's in `term-mode'
and its `default-directory' is DIR.  When DIR is nil, current
project root or `default-directory' is used."
  (let ((dir (file-truename (or dir (funcall toki-term-project-root-function)
                                default-directory))))
    (cl-dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (derived-mode-p 'term-mode)
                   (equal (file-truename default-directory) dir))
          (cl-return buf))))))

(defun toki-term/get-term-buffers (&optional dir)
  "Get term buffers for DIR.
That's all buffers in `buffer-list' that's in `term-mode' and its
`default-directory' is DIR.  When DIR is nil, current project
root or `default-directory' is used."
  (let ((dir (file-truename (or dir (funcall toki-term-project-root-function)
                                default-directory)))
        result)
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (and (derived-mode-p 'term-mode)
                   (equal (file-truename default-directory) dir))
          (push buf result))))
    (nreverse result)))

(defun toki-term/buf-desc (buf)
  "Return description of terminal buffer BUF.
The description contains the buffer name, and the program running
in it, or \"idle\" if the terminal is not running any program."
  (concat (buffer-name buf)
          " "
          (or (toki-term/term-buf-child-proc-name buf)
              "(idle)")))

(defun toki-term/get-visible-term-bufs ()
  "Get visible term buffers."
  (seq-filter (lambda (buf)
                (with-current-buffer buf
                  (derived-mode-p 'term-mode)))
              (mapcar #'window-buffer (window-list))))

(defun toki-term/create-term (&optional dir)
  "Create a term buffer in DIR.
When DIR is nil, use current project root or `default-directory'.
The created buffer is returned."
  (let ((buf (generate-new-buffer "*term*"))
        (dir (or dir (funcall toki-term-project-root-function) default-directory))
        (prog (toki-term/get-shell-prog)))
    (with-current-buffer buf
      (let ((default-directory dir))
        (term-mode)
        (term-exec buf (buffer-name) prog nil nil)
        (term-char-mode)))
    buf))

(defun toki-term/term-buf-child-proc-name (buf)
  "Get the child process name of term buffer BUF.
If the terminal isn't running any program, return nil."
  (when-let ((pid (process-running-child-p (get-buffer-process buf))))
    (alist-get 'args (process-attributes pid))))

(defun toki-term/get-shell-prog ()
  "Get shell program name."
  (or toki-term-shell-program explicit-shell-file-name
      (getenv "SHELL") shell-file-name
      (read-file-name "Shell executable: " "/" nil t)))

;;;; Create/switch to term commands

;;;###autoload
(defun toki-pick-term (&optional dir)
  "Pick a term buffer with DIR being the pwd.
This uses `completing-read' and a \"*New*\" option creates a new
terminal in DIR."
  (interactive)
  (let* ((dir (or dir (funcall toki-term-project-root-function) default-directory))
         (bufs (toki-term/get-term-buffers dir))
         (cands (append (list '("*New*" . nil))
                        (mapcar
                         (lambda (buf)
                           (cons (toki-term/buf-desc buf)
                                 buf))
                         bufs)))
         (cand (completing-read "Term: " cands nil t))
         (buf (alist-get cand cands nil nil #'equal)))
    (if buf (pop-to-buffer buf)
      (pop-to-buffer (toki-term/create-term dir)))
    ;; If the user calls `ansi-term' before, undo its call to
    ;; `term-set-escape-char'.
    (when term-escape-char
      (toki-term-remove-escape-char)
      (toki-term-setup-escape-keys))))

;;;###autoload
(defun toki-term (&optional dir pick-in-term)
  "Start a terminal emulator on DIR.
When DIR is nil, set it to the project root, or current directory
if project can't be detected.

When called interactively in a term buffer, or PICK-IN-TERM is
non-nil and the current buffer is a term buffer, run
`toki-pick-term'.

When there's already a terminal with DIR being the working
directory, switch to that terminal.  If you do want a new
terminal, call `toki-term' again and select \"*New*\".

This is like `term' but with several tweaks to make you happier,
see the package description of `toki-term'."
  (interactive "i\np")
  (let* ((dir (or dir (funcall toki-term-project-root-function) default-directory)))
    (if (and pick-in-term (derived-mode-p 'term-mode))
        (toki-pick-term dir)
      (let ((buf (or (toki-term/get-term-buffer dir)
                     (toki-term/create-term dir))))
        (pop-to-buffer buf)))
    (when term-escape-char
      (toki-term-remove-escape-char)
      (toki-term-setup-escape-keys))))

;;;###autoload
(defun toki-term-in-dir ()
  "Run `toki-term' with DIR being manually selected."
  (interactive)
  (let ((dir (read-directory-name "Dir: ")))
    (toki-term dir)))

;;;;; Browse mode

(defvar toki-term-browse-mode-map
  (make-sparse-keymap)
  "Keymap for `toki-term-browse-mode'.")

(defun toki-term-browse-mode ()
  "Turn the terminal buffer into a read-only normal buffer."
  (interactive)
  ;; Workaround: Without this code, there's a bug: Press `C-p' in char mode to
  ;; browse history, then `C-n' to go back, then `toki-term-browse-mode', then
  ;; `C-n', you'll find a newline is produced.  Call `term-char-mode', that
  ;; newline is sent to the shell.  This is not a problem with
  ;; `toki-term-browse-mode', since `term-line-mode' also has it.
  (let ((inhibit-read-only t))
    (save-excursion
      (goto-char (point-max))
      (while (eq (char-before) ?\n)
        (delete-char -1))))
  ;; Idea: We could put a `read-only' property to the region before
  ;; `process-mark', so current input could be edited, but I think there's
  ;; little benefit.
  (setq buffer-read-only t)
  (remove-hook 'pre-command-hook #'term-set-goto-process-mark t)
  (remove-hook 'post-command-hook #'term-goto-process-mark-maybe t)
  (use-local-map toki-term-browse-mode-map))

;;;;; Editing commands

(defun toki-term-yank ()
  "Paste recent kill into terminal, in char mode."
  (interactive)
  (when-let ((text (current-kill 0))
             ;; Remove newlines at the beginning/end.))
             (text (string-trim text "\n+" "\n+")))
    (when (or (not (string-match-p "\n" text))
              (y-or-n-p "You are pasting a multiline string.  Continue? "))
      (let ((bracketed-p (member (toki-term/term-buf-child-proc-name (current-buffer))
                                 toki-term-bracketed-paste-programs)))
        (when bracketed-p (term-send-raw-string toki-term/open-bracket))
        (term-send-raw-string text)
        (when bracketed-p (term-send-raw-string toki-term/close-bracket))))))

;;;###autoload
(defun toki-term-send-region-to-visible-term ()
  "Send active region to a visible term buffer."
  (interactive)
  (unless (use-region-p)
    (user-error "No active region"))
  (let ((bufs (toki-term/get-visible-term-bufs))
        buf)
    (pcase (length bufs)
      (0 (user-error "No visible term buffer"))
      (1 (setq buf (car bufs)))
      (_ (setq buf (completing-read
                    "Term: "
                    (mapcar (lambda (buf)
                              (cons (toki-term/buf-desc buf) buf))
                            bufs)
                    nil t))))
    (let ((bracketed-p (member (toki-term/term-buf-child-proc-name buf)
                               toki-term-bracketed-paste-programs)))
      (when bracketed-p (process-send-string buf toki-term/open-bracket))
      (process-send-region buf (region-beginning) (region-end))
      (when bracketed-p (process-send-string buf toki-term/close-bracket)))
    (pop-to-buffer buf)))

(provide 'toki-term)

;;; toki-term.el ends here
