;; -*- lexical-binding: t; -*-

(use-package facets
  :straight nil
  :defer t
  :init
  (add-hook 'find-file-hook #'facets-auto-enable-facets-mode)
  :config
  (toki-local-def
    :keymaps 'facets-mode-map
    "i" '("Insert ID" . facets-insert-or-update-id)
    "l" '("Insert Link" . facets-insert-link)
    "L" '("List Links" . facets-list-links)
    "c" '("Copy ID" . facets-copy-id)
    "b" '("Jump Back" . facets-jump-back)))

(toki-app-def
  "f" '(facets-find-file :wk "Facets"))
