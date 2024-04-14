;;; toki-file.el --- File management -*- lexical-binding: t -*-

;; Copyright (C) 2021 Hao Wang
;; License: GPL v3, or (at your option) any later version

;;; Commentary:

;;; Code:

;; To see the outline of this file, run `outline-minor-mode', then
;; `outline-hide-body'.  Another way is to run `occur' with the query:
;; ^;;;;* \|^(

;;;; Libraries

(require 'consult)
(require 'project)

;;;; User options

(defvar toki-project-root-files
  '("src/" "node_modules/" "Makefile" "setup.py"
    "configure.ac" "configure.in")
  "A list of files that marks the project root.")

(defvar toki-fd-cmd
  "fd -HIL -E .wine -E .git --color=never --full-path ARG OPTS"
  "Find command used when \"fd\" program is avaliable.
See `consult-find-command'.")

(defvar toki-find-cmd
  "find -L . -type d ( -name .wine -o -name .git ) -prune -o \
( -type f -ipath *ARG* ) -print OPTS"
  "Find command used when \"fd\" program is not avaliable.
See `consult-find-command'.")

;;;; APIs

;;;###autoload
(defun toki-project-root ()
  "Return project root of current buffer."
  (if-let ((project (project-current nil)))
      (expand-file-name
       (if (fboundp #'project-root)
           (project-root project)
         (car (project-roots project))))
    (locate-dominating-file
     (or (buffer-file-name) default-directory)
     (lambda (dir)
       (cl-some (lambda (f) (file-exists-p (expand-file-name f dir)))
                toki-project-root-files)))))

;;;; Commands

;;;###autoload
(defun toki-find-file (&optional dir)
  (interactive)
  (let* ((consult-find-command (if (executable-find "fd")
                                   toki-fd-cmd
                                 toki-find-cmd))
         (dir (or dir (read-directory-name "Find file in: "
                                           (toki-project-root)))))
    (consult-find dir nil)))

;;;###autoload
(defun toki-find-file-in-tokimacs ()
  (interactive)
  (let* ((default-directory user-emacs-directory)
         (pred (lambda (dir)
                 (not (string-match (rx (or line-start "/")
                                        (or ".git" "straight"
                                            "auto-save-list" "var"
                                            ".build" "eln-cache"))
                                    dir))))
         (files (directory-files-recursively user-emacs-directory ""
                                             nil pred))
         (files (mapcar (lambda (f) (file-relative-name f)) files))
         (collection (lambda (str pred action)
                       (if (eq action 'metadata)
                           '(metadata (display-sort-function . identity)
                                      (cycle-sort-function . identity))
                         (complete-with-action action files str pred)))))
    (find-file (completing-read "Find file in Tokimacs: " collection))))

;; Ref: http://ergoemacs.org/emacs/emacs_new_empty_buffer.html
;;;###autoload
(defun toki-new-file ()
  "Create an untitled file."
  (interactive)
  (let ((buf (generate-new-buffer "untitled")))
    (switch-to-buffer buf)
    (funcall initial-major-mode)
    (setq buffer-offer-save t)
    buf))

(provide 'toki-file)

;;; toki-file.el ends here
