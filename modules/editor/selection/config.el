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
    "C-c" 'vertico-save)
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
  (marginalia-mode)
  (toki-local-def
   :keymaps 'minibuffer-local-map
   "c" `(,(toki/make-combo marginalia-cycle) :wk "Cycle Annotation")))

(toki-leader-def
  "SPC" 'execute-extended-command)
