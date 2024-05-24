;; -*- lexical-binding: t; -*-

(use-package facets
  :straight nil
  :defer t
  :init
  (add-hook 'find-file-hook #'facets-auto-enable-facets-mode))

(toki-app-def
  "f" '(facets-new-facet :wk "New Facet")
  "F" '(facets-find-facet :wk "Find Facet"))
