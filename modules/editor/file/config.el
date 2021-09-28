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

(use-package winum
  :trigger after-init-hook
  :config
  (toki/setq winum-auto-setup-mode-line nil)
  (winum-mode))

(defun toki/get-another-window ()
  "Return another window.
When there are 2 windows, this will return the other one.  With
more than 2, it will ask the user for the number of the window."
  (let ((n (length (window-list)))
        (char))
    (cond
     ((eq 1 n) nil)
     ((eq 2 n) (next-window))
     (t
      (setq char (read-key "Window number:"))
      (when (and (>= char ?0) (<= char ?9)
                 (not (eq (- char 48) (winum-get-number (selected-window)))))
        (winum-get-window-by-number (- char 48)))))))

;;;; Commands

(defun toki-select-window ()
  "Select window by its number.
Bind this to a keystroke that ends with a number, like \"M-1\" or
\"C-x 1\", it will recognize this number and select the
corresponding window."
  (interactive)
  (let* ((event last-input-event)
         (key (make-vector 1 event))
         (key-desc (key-description key)))
    (winum-select-window-by-number
     (string-to-number (car (nreverse (split-string key-desc "-")))))))

(defun toki-kill-another-window ()
  "Kill another window.
When there are 2 windows, this will kill the other one.  With
more than 2, it will ask for the number of the window."
  (interactive)
  (delete-window (toki/get-another-window)))

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

;;; Buffer <-> Window

(defun toki-send-buf-to-window ()
  "Send buffer to another window."
  (interactive)
  (when-let ((b (current-buffer))
             (w (toki/get-another-window)))
    (with-selected-window w
      (switch-to-buffer b))
    (previous-buffer)))

(defun toki-copy-buf-to-window ()
  "Copy buffer to another window."
  (interactive)
  (when-let ((b (current-buffer))
             (w (toki/get-another-window)))
    (with-selected-window w
      (switch-to-buffer b))))

(defun toki-kill-another-buffer ()
  "Kill buffer in another window."
  (interactive)
  (when-let ((w (toki/get-another-window)))
    (with-selected-window w
      (kill-current-buffer))))

(defun toki-prev-buffer-in-another-window ()
  "Go to previous buffer in another window."
  (when-let ((w (toki/get-another-window)))
    (with-selected-window w
      (previous-buffer))))

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

(toki-leader-def
  "0" 'toki-select-window
  "1" 'toki-select-window
  "2" 'toki-select-window
  "3" 'toki-select-window
  "4" 'toki-select-window
  "5" 'toki-select-window
  "6" 'toki-select-window
  "7" 'toki-select-window
  "8" 'toki-select-window
  "9" 'toki-select-window)

;; Show all the above commands by "0..9".
(with-eval-after-load 'which-key
  ;; create a fake key to represent all ten keys
  (push '(("0" . "toki-select-window") . ("0..9" . "Select Window")) which-key-replacement-alist)
  ;; hide other keys
  (push '(("[1-9]" . "toki-select-window") . t) which-key-replacement-alist))

(toki-window-def
  "o" '(other-window :wk "Other Window")
  "v" '(split-window-right :wk "VSplit")
  "h" '(split-window-below :wk "HSplit")
  "k" '(delete-window :wk "Kill This")
  "K" '(toki-kill-another-window :wk "Kill Another")
  "m" '(delete-other-windows :wk "Maximize This"))

(general-def
  :states 'normal
  "gt" 'toki-tabs-next
  "gT" 'toki-tabs-previous)

(toki-buffer-def
  "n" '(toki-new-buffer :wk "New Buffer")
  "k" '(kill-current-buffer :wk "Kill This")
  "K" '(toki-kill-another-buffer :wk "Kill Another")
  "b" '(consult-buffer :wk "Switch Buffer")
  "p" '(previous-buffer :wk "Prev Buffer")
  "s" '(toki-send-buf-to-window :wk "Send to Window")
  "c" '(toki-copy-buf-to-window :wk "Copy to Window"))

(toki-tab-def
  "b" `(,(toki-make-combo toki-tabs-previous) :wk "Prev Tab")
  "f" `(,(toki-make-combo toki-tabs-next) :wk "Next Tab")
  "k" '(toki-tabs-kill-invisible-buffers-in-group :wk "Keep Visible in Group")
  "K" '(toki-tabs-kill-buffers-in-group :wk "Kill Buffer Group"))
