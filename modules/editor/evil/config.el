;; -*- lexical-binding: t; -*-

;; Vim Emulator

;;; User options

(defvar toki-minimal-evil-keybinds
  '("h" "j" "k" "l" ":" "G" "gg")
  "Evil keybindings to enable in motion state in special modes.
They are also enabled in some appropriate minor modes like
`undo-propose-mode'.")

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
   ;; We want all modes, except a few (REPL, terminal, etc.), to start in
   ;; normal state.
   evil-emacs-state-modes nil
   evil-insert-state-modes '(term-mode comint-mode)
   evil-motion-state-modes nil
   evil-default-cursor #'toki/evil-default-cursor
   evil-insert-state-cursor '(bar . 3)
   evil-emacs-state-cursor `((bar . 3) ,(face-foreground 'warning))
   evil-normal-state-cursor '(box)
   evil-visual-state-cursor '(hbar)
   evil-motion-state-cursor '(hollow)
   evil-replace-state-cursor '(hbar . 6))
  (evil-make-overriding-map general-override-mode-map))

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

(with-eval-after-load 'evil
  (defun toki/add-special-mode-evil-keybinds-to-map (map)
    "Enable `toki-minimal-evil-keybinds' for keymap MAP."
    (let (keybinds)
      (dolist (k toki-minimal-evil-keybinds)
        (push k keybinds)
        (push (lookup-key evil-motion-state-map k) keybinds))
      (apply #'evil-define-key* 'motion map (nreverse keybinds))))

  (defun toki/add-minimal-evil-keybinds-to-local-map ()
    "Apply `toki-minimal-evil-keybinds' to buffer-local map.
Before this, the buffer-local map is made to override than motion
state keybinds.  Keys in `toki-minimal-evil-keybinds' are added
back afterwards."
    (let ((map (current-local-map)))
      (toki/add-special-mode-evil-keybinds-to-map map)
      ;; Ref: https://github.com/emacs-evil/evil/issues/511
      (evil-make-overriding-map map 'motion)
      (evil-normalize-keymaps)))

  (add-hook 'special-mode-hook #'toki/add-minimal-evil-keybinds-to-local-map)

  (with-eval-after-load 'undo-propose
    (toki/add-special-mode-evil-keybinds-to-map undo-propose-mode-map)
    (evil-make-overriding-map undo-propose-mode-map 'motion)
    (add-hook 'undo-propose-mode-hook #'evil-normalize-keymaps)))

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
