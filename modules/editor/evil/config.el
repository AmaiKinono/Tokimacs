;; -*- lexical-binding: t; -*-

;; Vim Emulator

;;; Packages

(use-package evil
  :init
  (setq
   evil-want-C-u-scroll t
   evil-want-keybinding nil
   evil-overriding-maps nil
   evil-intercept-maps nil)
  :hook (after-init . evil-mode)
  :config
  (toki/setq
   evil-disable-insert-state-bindings t
   evil-default-state 'normal
   evil-default-cursor #'toki/evil-default-cursor
   evil-insert-state-cursor '(bar . 3)
   evil-emacs-state-cursor `((bar . 3) ,(face-foreground 'warning))
   evil-normal-state-cursor '(box)
   evil-visual-state-cursor '(hbar)
   evil-motion-state-cursor '(hollow)
   evil-replace-state-cursor '(hbar . 6))
  (evil-set-initial-state 'fundamental-mode 'normal)
  (evil-set-initial-state 'text-mode 'normal)
  (evil-set-initial-state 'prog-mode 'normal)
  (evil-set-initial-state 'conf-mode 'normal)
  (evil-make-overriding-map general-override-mode-map)
  (general-def
    :states 'motion
    "C-d" 'toki-smooth-scroll-window-half-page-up
    "C-u" 'toki-smooth-scroll-window-half-page-down))

(defun toki/evil-default-cursor ()
  "Set cursor color for non-emacs states."
  (if (eq (frame-parameter nil 'background-mode) 'dark)
      (set-cursor-color "#f1f1f1")
    (set-cursor-color "#252525")))

(use-package evil-terminal-cursor-changer
  :if (not toki-gui-p)
  :trigger after-init-hook
  :config
  (evil-terminal-cursor-changer-activate))

;;; Commands

(defun toki-insert-state ()
  "Switch to insert state.  If in visual state, keep the region."
  (interactive)
  (if (not (evil-visual-state-p))
      (evil-insert-state)
    (evil-insert-state)
    (activate-mark)))

(defun toki-emacs-state ()
  "Switch to emacs state.  If in visual state, keep the region."
  (interactive)
  (if (not (evil-visual-state-p))
      (evil-emacs-state)
    (evil-emacs-state)
    (activate-mark)))

(defun toki-visual-state ()
  "Switch to visual state.  If region is activate, keep it."
  (interactive)
  (if (not (region-active-p))
      (evil-visual-state)
    (let ((beg (region-beginning))
          (end (region-end)))
      (cond ((eq (point) beg)
             (evil-visual-state)
             (set-mark (1- end))
             (activate-mark))
            (t
             (evil-visual-state)
             (goto-char (1- end))
             (activate-mark))))))

;;; Keybinds

(defvar toki-evil-leader-key "SPC"
  "Leader key used in normal and visual state.")

(defvar toki-evil-2nd-leader-key ","
  "2nd leader key used in normal and visual state.")

(general-def
  :states '(normal visual)
  :keymaps 'override
  :start-maps t
  ;; 1. `general-key' can't be used here, or all overrided descriptions will not
  ;;    take effect.  See https://github.com/justbur/emacs-which-key/issues/177.
  ;;    So evil leader key doesn't work well with `describe-key' for now
  ;;    (`general-key' can handle this).
  ;;
  ;;    An alternative way is just getting rid of simulating keys.  I can create
  ;;    a macro to create both definers for the `override' keymap (like
  ;;    `toki/leader-def') and normal/visual states (like
  ;;    `toki/evil-leader-def'), then it creates another macro/function (like
  ;;    `toki-leader-def') that calls the above two definers.  That shouldn't be
  ;;    too hard, but there's already some plan
  ;;    (https://github.com/noctuid/general.el/issues/126) to implement this in
  ;;    general.el.  So let's just wait for a while.
  ;;
  ;; 2. I have to wrap it in an `eval' form or it won't work.  See
  ;;    https://github.com/noctuid/general.el/issues/150 and
  ;;    https://github.com/noctuid/general.el/issues/149#issuecomment-484933619.
  toki-evil-leader-key (eval `(general-simulate-key ,toki-leader-key))
  toki-evil-2nd-leader-key (eval `(general-simulate-key ,toki-2nd-leader-key)))

(general-def 'normal
  "``" 'evil-visual-block)

(toki-edit-def
  "v" '(toki-visual-state :wk "Visual State")
  "e" '(toki-emacs-state :wk "Emacs State")
  "i" '(toki-insert-state :wk "Insert State"))
