;; -*- lexical-binding: t; -*-

;; Terminal emulator

(use-package toki-term
  :straight nil
  :defer t
  :config
  (setq
   toki-term-project-root-function #'toki-project-root
   toki-term-escape-keys `(,toki-leader-key
                           ,toki-2nd-leader-key
                           "M-x"))
  (toki-term-setup-escape-keys)

  (toki-local-def
    :keymaps 'term-raw-map
    "b" '(toki-term-browse-mode :wk "Browse Mode"))
  (general-def
    :keymaps 'term-raw-map
    "C-S-v" 'toki-term-yank)
  (toki-local-def
    :keymaps 'toki-term-browse-mode-map
    "c" '(term-char-mode :wk "Char Mode")))

(toki-app-def
  "t" '(toki-term :wk "Terminal"))
