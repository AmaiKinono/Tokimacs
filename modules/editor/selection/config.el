;; -*- lexical-binding: t; -*-

;; Interactively filter & select from a list

;; Copyright (C) 2021 Hao Wang
;; License: GPL v3, or (at your option) any later version

(setq enable-recursive-minibuffers t)

(use-package vertico
  :trigger pre-command-hook
  :config
  (toki/setq
   vertico-count 8)
  (general-def
    :keymaps 'vertico-map
    "C-c" 'vertico-save
    "C-r" 'vertico-scroll-up
    "C-t" 'vertico-scroll-down)
  (face-spec-set 'vertico-current
                 '((((background light))
                    :background "#d8d8d8" :extend t)
                   (t
                    :background "#454545" :extend t))
                 'face-defface-spec)
  (vertico-mode))

(use-package orderless
  :trigger pre-command-hook
  :config
  (setq
   completion-styles '(orderless)
   completion-category-defaults nil
   completion-category-overrides '((file (styles partial-completion)))))

(use-package consult
  :defer t
  :init
  (setq
   completion-in-region-function #'consult-completion-in-region)
  :config
  (setq
   consult-project-root-function #'toki-project-root
   consult-async-min-input 2))

(use-package marginalia
  :trigger pre-command-hook
  :config
  (marginalia-mode))

(use-package embark
  :trigger pre-command-hook
  :config
  (defun embark-which-key-indicator ()
    "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
    (lambda (&optional keymap targets prefix)
      (if (null keymap)
          (kill-buffer which-key--buffer)
        (which-key--show-keymap
         (if (eq (caar targets) 'embark-become)
             "Become"
           (format "Act on %s '%s'%s"
                   (caar targets)
                   (embark--truncate-target (cdar targets))
                   (if (cdr targets) "…" "")))
         (if prefix (lookup-key keymap prefix) keymap)
         nil nil t))))
  (toki/setq embark-indicator #'embark-which-key-indicator))

(use-package embark-consult
  :after (embark consult))

;;; Keybinds

(toki-leader-def
  "SPC" '(execute-extended-command :wk "M-x"))

(general-def
  "C-." 'embark-act)
