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
    "b" '("Browse Mode" . toki-term-browse-mode))
  (general-def
    :keymaps 'term-raw-map
    "C-S-v" 'toki-term-yank)
  (toki-local-def
    :keymaps 'toki-term-browse-mode-map
    "c" '("Char Mode" . term-char-mode)))

(toki-app-def
  "t" '(toki-term :wk "Terminal")
  "T" '(toki-term-in-dir :wk "Terminal (Pick Dir)")
  "s" '(toki-term-send-region-to-visible-term :wk "Send Region to Term"))
