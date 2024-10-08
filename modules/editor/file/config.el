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

;;; Window

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
  :after toki-modeline
  :config
  (toki-tabs-mode)
  (add-to-list 'toki-tabs-update-hook #'toki-modeline/refresh-active-modeline)
  (setq toki-tabs-project-root-function #'toki-project-root))

(with-eval-after-load 'consult
  (define-advice consult--buffer-pair (:around (_ buffer) show-path)
    "Also show path for file buffers so the user can filter them by path."
    (let ((dir (or (toki-project-root) default-directory))
          name)
      (if-let ((path (buffer-file-name buffer)))
          (progn (when (file-in-directory-p path dir)
                   (setq path (file-relative-name path dir)))
                 (cons path buffer))
        (cons (buffer-name buffer) buffer)))))

;;; Keybinds

(general-def
  "C-s" 'save-buffer)

(toki-file-def
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
  "b" '(consult-buffer :wk "Switch Buffer")
  "p" '(previous-buffer :wk "Prev Buffer")
  "P" '(toki-window-prev-buffer-in-another-window :wk "Prev Buf in Another Window")
  "s" '(toki-window-send-buffer-to-window :wk "Send to Window")
  "c" '(toki-window-copy-buffer-to-window :wk "Copy to Window"))

(toki-tab-def
  "b" `(,(toki-make-combo toki-tabs-previous) :wk "Prev Tab")
  "f" `(,(toki-make-combo toki-tabs-next) :wk "Next Tab")
  "k" '(toki-tabs-kill-invisible-buffers-in-group :wk "Keep Visible in Group")
  "K" '(toki-tabs-kill-buffers-in-group :wk "Kill Buffer Group"))
