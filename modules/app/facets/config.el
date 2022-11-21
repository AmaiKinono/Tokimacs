;; -*- lexical-binding: t; -*-

(use-package facets
  :straight nil
  :defer t
  :init
  (add-hook 'find-file-hook #'facets-auto-enable-facets-mode)
  :config
  (toki-local-def
    :keymaps 'facets-mode-map
    "l" '("Insert Link" . facets-insert-link)
    "L" '("List References" . facets-list-references)
    "c" '("Copy ID as Link" . facets-copy-id-as-link)
    "b" '("Jump Back" . facets-jump-back)))

(toki-app-def
  "f" '(facets-new-facet :wk "New Facet"))
