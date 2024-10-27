;; -*- lexical-binding: t; -*-

;; File/buffer/window management

;;; File

;;;; Defaults

;; Make UTF-8 the default coding system.
(set-language-environment "UTF-8")
;; On windows, clipboard may use UTF-16.  Let the system decide what to use
;; here.
(unless toki-win-p
  (setq selection-coding-system 'utf-8))

;; Don't ask me whether to follow a symlink when visiting it
(setq vc-follow-symlinks t)
(global-auto-revert-mode 1)

;;;; Packages

(use-package toki-file
  :defer t
  :straight nil)

(use-package epa-file
  :straight nil
  :trigger find-file-noselect
  :config
  (toki/setq
   epg-pinentry-mode 'loopback
   ;; This makes it easier to work with symmetric encryption. Sometimes its
   ;; behavior may confuse you, and I'll explain:
   ;;
   ;; 1. The docstring says this doesn't work for GnuPG 2.0. In my experiment
   ;; it works for GnuPG 2.2, and I guess you could use any version >= 2.1.
   ;;
   ;; 2. Most of the time, it will only ask for password once when you open a
   ;; file, and use that password for saving.  Perfect.
   ;;
   ;; 3. Sometimes it won't ask for password when you open a file.  This is
   ;; because your opened it before, and gpg-agent caches it for you.  Since
   ;; there's no way Emacs could know this password, it will ask for password
   ;; when first saving.  If you don't like this, configure your gpg-agent to
   ;; not caching passwords.  I haven't found a way to disable it in Emacs.
   epa-file-cache-passphrase-for-symmetric-encryption t)
  ;; Don't show "epa already enabled" when enabling.
  (let ((inhibit-message t))
    (epa-file-enable)))

(use-package dired
  :straight nil
  :defer t
  :config
  (toki/setq
   dired-kill-when-opening-new-dired-buffer t)

  (defun toki-dired-term ()
    "Open a terminal in current directory in a dired buffer."
    (interactive)
    (toki-term default-directory))

  (defun toki-dired-execute-in-term ()
    "Run executable at point in a terminal."
    (interactive)
    (let ((f (dired-get-file-for-visit)))
      (toki-term-send-text-to-visible-term
       ;; When there's double quote in the file name, this may cause shell
       ;; injection.
       (concat "\"./" (file-name-nondirectory f) "\"\n")
       'idle-only 'new-term default-directory)))

  (defun toki-dired-find-file ()
    "Like `dired-find-file', but open binaries externally and run executables."
    (interactive)
    (let ((f (dired-get-file-for-visit)))
      (cond
       ((file-directory-p f) (dired-find-file))
       ((not (toki-file-binary-p f)) (dired-find-file))
       (t (cond
           ;; Visit compressed files directly.
           ((cl-dolist (s dired-compress-file-suffixes)
              (when (string-match (car s) f)
                (cl-return t)))
            (dired-find-file))
           ;; NOTE: `file-executable-p' can return t on non-executable files in
           ;; NTFS drives.  Haven't find a good way to fix that.
           ((file-executable-p f) (toki-dired-execute-in-term))
           (t (browse-url-of-dired-file)))))))

  (defun toki-dired-open-with ()
    "Open file at point with program."
    (interactive)
    (let ((f (dired-get-file-for-visit))
          (prog (completing-read "Program: " (toki-get-executables))))
      (start-process prog nil prog f)))

  (defun toki-dired-find-dir-new-buffer ()
    "Open dir at point in new buffer."
    (interactive)
    (let ((dired-kill-when-opening-new-dired-buffer nil))
      (dired-find-file)))

  (setq dired-mode-map (make-sparse-keymap))
  (general-def
    :keymaps 'dired-mode-map
    "h" '(dired-up-directory :wk "Prev")
    "<left>" '(dired-up-directory :wk "Prev")
    "j" '(dired-next-line :wk "Next")
    "<down>" '(dired-next-line :wk "Next")
    "k" '(dired-previous-line :wk "Prev")
    "<up>" '(dired-previous-line :wk "Prev")
    "l" '(toki-dired-find-file :wk "Open")
    "<right>" '(toki-dired-find-file :wk "Open")
    "r" '(revert-buffer :wk "Revert")
    "RET" '(dired-find-file :wk "Open")
    [remap isearch-forward-regexp] '(dired-isearch-filenames-regexp
                                     :wk "Search Regexp")
    [remap isearch-forward] '(dired-isearch-forward :wk "Search Literally")

    "c" '(:wk "Compress")
    "cc" '(dired-do-compress-to :wk "Compress Marked Files")
    "cu" '(dired-do-compress :wk "Uncompress")

    "e" '(:wk "Edit")
    "ee" '(wdired-change-to-wdired-mode :wk "Edit")
    "ek" '(dired-do-kill-lines :wk "Flush Lines")
    "ec" '(dired-copy-filename-as-kill :wk "Copy File Name")

    "f" '(:wk "File")
    "fo" '(dired-find-file :wk "Open File in Emacs")
    "fO" '(dired-find-file-other-window :wk "Open in Other Window")
    "fe" '(browse-url-of-dired-file :wk "Open Externally")
    "fr" '(toki-dired-execute-in-term :wk "Run")
    "fw" '(toki-dired-open-with :wk "Open With")
    "fc" '(dired-do-copy :wk "Copy")
    "fR" '(dired-do-rename :wk "Rename")
    "fD" '(dired-do-delete :wk "Delete")

    "d" '(:wk "Dir")
    "do" '(toki-dired-find-dir-new-buffer :wk "Open Dir in New Buffer")
    "di" '(dired-maybe-insert-subdir :wk "Add This Dir")
    "df" '(dired-hide-subdir :wk "<> Fold This Dir")
    "dk" '(dired-kill-subdir :wk "Remove This Dir")

    "m" '(:wk "Mark")
    "mm" '(dired-mark :wk "Mark")
    "mu" '(dired-unmark :wk "Unmark")
    "mU" '(dired-unmark-all-marks :wk "Unmark All")
    "mr" '(dired-mark-files-containing-regexp
           :wk "Mark Files Containing Regexp")
    "mR" '(dired-mark-files-regexp :wk "Mark Files Matching Regexp")
    "mi" '(dired-toggle-marks :wk "Invert Mark")

    "n" '(:wk "New")
    "nd" '(dired-create-directory :wk "New Directory")
    "nf" '(dired-create-empty-file :wk "New File")

    "t" '(:wk "Toggle/Term")
    "th" '(dired-omit-mode :wk "<> Hidden File")
    "td" '(dired-hide-details-mode :wk "<> Details")
    "tt" '(toki-dired-term :wk "Term in Current Dir")))

;;; File

;; Quickly input favorite location in file-related commands in vertico.
(with-eval-after-load 'vertico
  (defvar-keymap toki/vertico-file-category-map
    :parent vertico-map)
  (toki-local-def
    :keymaps 'toki/vertico-file-category-map
    "f" '("Favorite Location" . toki-replace-minibuf-input-by-fav-location))
  (add-to-list 'vertico-multiform-categories
               `(file (vertico-map . ,toki/vertico-file-category-map))))

;;; window

(setq mouse-autoselect-window t)

(use-package toki-window
  :straight nil
  :defer t)

;;; Buffer

;; ref: http://ergoemacs.org/emacs/emacs_new_empty_buffer.html
(defun toki-new-buffer ()
  "Create a new empty buffer."
  (interactive)
  (let ((buf (generate-new-buffer "*scratch*")))
    (switch-to-buffer buf)
    (funcall initial-major-mode)
    buf))

(use-package toki-tabs
  :straight nil
  :config
  (toki/setq toki-tabs-project-root-function #'toki-project-root
             toki-tabs-visible-buffer-limit nil)
  (toki-tabs-mode)
  ;; Needed for tabs in modeline.
  ;; (add-to-list 'toki-tabs-update-hook #'toki-modeline/refresh-active-modeline)
  (setq tab-bar-format '(toki-tabs-tab-bar-format))
  (face-spec-set 'tab-bar
                 '((t :inherit default
                      :height 0.9
                      :background unspecified)))
  ;; `tab-bar-mode' actually takes a lot of init time, and it's super weird
  ;; that put it in a 0.0s idle timer is actually faster.
  (run-with-idle-timer 0.0 nil #'tab-bar-mode))

(with-eval-after-load 'consult
  (define-advice consult--buffer-pair (:around (_ buffer) show-path)
    "Also show path for file buffers so the user can filter them by path."
    (let ((dir (or (toki-project-root) default-directory)))
      (if-let ((path (buffer-file-name buffer)))
          (progn (when (file-in-directory-p path dir)
                   (setq path (file-relative-name path dir)))
                 (cons path buffer))
        (cons (buffer-name buffer) buffer)))))

;;; Keybinds

(general-def
  "C-s" 'save-buffer)

(toki-file-def
  "d" '(dired-jump :wk "Dired Here")
  "n" '(toki-new-file :wk "New File")
  "s" '(save-buffer :wk "Save File")
  "S" '(write-file :wk "Save As")
  "o" '(find-file :wk "Open File")
  "f" '(toki-find-file :wk "Find File")
  "t" '(toki-find-file-in-tokimacs :wk "Find in Tokimacs")
  "l" '(find-library :wk "Open Elisp Library"))

(toki-window-def
  "o" '(toki-window-other-window :wk "Other Window")
  "v" '(split-window-right :wk "VSplit")
  "h" '(split-window-below :wk "HSplit")
  "k" '(delete-window :wk "Kill This")
  "K" '(toki-window-kill-other-window :wk "Kill Another")
  "m" '(delete-other-windows :wk "Maximize This")
  "M" '(toki-window-select-minibuffer :wk "Select Minibuffer Window"))

(general-def
  :states 'normal
  "gt" 'toki-tabs-next
  "gT" 'toki-tabs-previous)

(toki-buffer-def
  "n" '(toki-new-buffer :wk "New Buffer")
  "k" '(kill-current-buffer :wk "Kill This")
  "K" '(toki-window-kill-other-buffer :wk "Kill Another")
  "g" '(toki-tabs-switch-to-buffer-in-other-group :wk "Switch Buffer by Group")
  "G" '(toki-tabs-kill-buffers-in-other-group :wk "Kill Buffer Group")
  "D" '(toki-tabs-kill-buffers-in-directory :wk "Kill Buffer in Dir")
  "b" '(consult-buffer :wk "Switch Buffer")
  "p" '(previous-buffer :wk "Prev Buffer")
  "P" '(toki-window-prev-buffer-in-another-window :wk "Prev Buf in Another Window")
  "s" '(toki-window-send-buffer-to-window :wk "Send to Window")
  "c" '(toki-window-copy-buffer-to-window :wk "Copy to Window"))

(toki-tab-def
  "b" `(,(toki-make-combo toki-tabs-previous) :wk "Prev Tab")
  "f" `(,(toki-make-combo toki-tabs-next) :wk "Next Tab"))
