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
  (epa-file-enable))

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

(use-package awesome-tab
  :straight (:host github :repo "manateelazycat/awesome-tab")
  :hook (after-init . awesome-tab-mode)
  :config
  (toki/setq
   awesome-tab-dark-unselected-blend 0.6
   awesome-tab-light-unselected-blend 0.9
   ;; awesome-tab-height 140
   awesome-tab-ace-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
   awesome-tab-terminal-dark-select-background-color "#DDDDDD"
   awesome-tab-terminal-dark-select-foreground-color "#333333"
   awesome-tab-terminal-dark-unselect-background-color "#333333"
   awesome-tab-terminal-dark-unselect-foreground-color "#999999")
  (when (not toki-gui-p)
    (toki/setq awesome-tab-display-icon nil))

  ;; get rid of header line in which-key buffer.
  ;; awesome-tab actually have `awesometab-hide-tabs-hooks' for this, but it has
  ;; to be set before awesome-tab is loaded. So we just do it ourselves.
  (defun toki/remove-which-key-buffer-header-line ()
    (setq-local header-line-format nil))
  (add-hook 'which-key-init-buffer-hook
            #'toki/remove-which-key-buffer-header-line)

  (define-advice awesome-tab-project-name (:around (fn) fix)
    "Use our own project root function."
    (if-let ((project (toki-project-root)))
        (format "Project: %s" project)
      awesome-tab-common-group-name))

  (defun toki-buffer-group ()
    "An `awesome-tab-buffer-groups-function' that group buffers by projects."
    (awesome-tab-get-group-name (current-buffer)))
  (toki/setq awesome-tab-buffer-groups-function #'toki-buffer-group)

  ;; show window numbers on the header line.
  (define-advice awesome-tab-line (:around (fn) fix)
    "show window number"
    (let* ((tabline (funcall fn))
           (win-num (winum-get-number)))
      (if win-num
          (push
           (propertize
            (format " %s " win-num) 'face 'toki-window-number) tabline)
        tabline)))

  (defface toki-window-number
    '((t))
    "Face for active window number in tabline.")

  (defun toki-refresh-win-num ()
    (set-face-attribute 'toki-window-number nil
                        ;;:height awesome-tab-face-height
                        :background (face-background 'default)
                        :foreground (face-foreground 'font-lock-function-name-face)))

  (defun toki-refresh-tabs ()
    "Refresh the display of tabs. This runs with`toki-after-load-theme-hook'
in order to have the right face colors."
    (interactive)
    (toki-refresh-win-num)
    (awesome-tab-refresh-display))

  (defun toki-awesome-tab-buffer-groups-function ()
    "Group rules for awesome-tab."
    (cond
     ((or (string-equal "*Help*" (buffer-name))
          (string-equal "*ielm*" (buffer-name)))
      (list "Elisp"))
     (t
      (awesome-tab-buffer-groups))))

  (toki/setq awesome-tab-buffer-groups-function #'toki-awesome-tab-buffer-groups-function)

  (add-hook 'toki-after-load-theme-hook #'toki-refresh-tabs)
  (add-hook 'after-change-major-mode-hook #'toki-refresh-tabs)

  (toki-refresh-win-num))

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
  "gt" 'awesome-tab-forward
  "gT" 'awesome-tab-backward)

(toki-buffer-def
  "n" '(toki-new-buffer :wk "New Buffer")
  "k" '(kill-current-buffer :wk "Kill This")
  "K" '(toki-kill-another-buffer :wk "Kill Another")
  "b" '(consult-buffer :wk "Switch Buffer")
  "s" '(toki-send-buf-to-window :wk "Send to Window")
  "c" '(toki-copy-buf-to-window :wk "Copy to Window"))

(toki-tab-def
  "b" '(awesome-tab-backward-tab :wk "Backward Tab")
  "f" '(awesome-tab-forward-tab :wk "Forward Tab")
  "n" '(awesome-tab-forward-group :wk "Forward Group")
  "p" '(awesome-tab-backward-group :wk "Backward Group")
  "a" '(awesome-tab-select-beg-tab :wk "Select Beginning Tab")
  "A" '(awesome-tab-move-current-tab-to-beg :wk "Move Tab to Beginning")
  "j" '(awesome-tab-ace-jump :wk "Jump to Tab"))
