;; -*- lexical-binding: t; -*-

(use-package facets
  :straight nil
  :defer t
  :init
  (add-hook 'find-file-hook #'facets-auto-enable-facets-mode)
  :config
  (toki-local-def
    :keymap 'facets-mode-map
    "i" '(facets-insert-or-update-id :wk "Insert ID")
    "l" '(facets-insert-link :wk "Insert Link")
    "L" '(facets-list-links :wk "List Links")
    "c" '(facets-copy-id :wk "Copy ID")
    "b" '(facets-jump-back :wk "Jump Back")))

(toki-app-def
  "f" '(facets-find-file :wk "Facets"))
