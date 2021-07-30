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

;;;; Commands

;;;###autoload
(defun toki-term (&optional dir)
  "Start a terminal emulator on DIR.
When DIR is nil, set it to the project root, or current directory
if project can't be detected.

This is like `term' but with several tweaks to make you happier."
  (interactive)
  (let* ((prog (or toki-term-shell-program explicit-shell-file-name
                   (getenv "SHELL") shell-file-name
                   (read-file-name "Shell executable: " "/" nil t)))
         (dir (or dir (funcall toki-term-project-root-function) default-directory))
         (default-dir (file-truename default-directory))
         buflist target)
    (setq dir (file-truename dir))
    (unless (derived-mode-p 'term-mode)
      (dolist (buf (buffer-list))
        (with-current-buffer buf
          (when (and (derived-mode-p 'term-mode)
                     (equal default-dir dir)
                     ;; Make sure the term isn't running any program.
                     (not (process-running-child-p (get-buffer-process buf))))
            (push buf buflist)))))
    (cond
     ((eq (length buflist) 1)
      (when (y-or-n-p (format "%s is visiting %s.  Use it instead? "
                              (buffer-name (car buflist)) dir))
        (setq target (car buflist))))
     ((> (length buflist) 1)
      (when (y-or-n-p (format "Some terminals is visiting %s.  Pick one of them? "
                              dir))
        (setq target
              (read-buffer "Term buffer: " nil t
                           (lambda (buf)
                             (memq (if (consp buf) (cdr buf) (get-buffer buf))
                                   buflist)))))))
    (unless target
      (setq target (generate-new-buffer "*term*"))
      (with-current-buffer target
        (let ((default-directory dir))
          (term-mode)
          (term-exec target (buffer-name) prog nil nil)
          (term-char-mode)))
      ;; If the user calls `ansi-term' before, undo its call to
      ;; `term-set-escape-char'.
      (when term-escape-char
        (toki-term-remove-escape-char)
        (toki-term-setup-escape-keys)))
    (pop-to-buffer target)))

;;;###autoload
(defun toki-term-in-dir ()
  "Start a terminal emulator in the directory you selected.
It uses `toki-term' internally."
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
      (term-send-raw-string text))))

(provide 'toki-term)

;;; toki-term.el ends here
