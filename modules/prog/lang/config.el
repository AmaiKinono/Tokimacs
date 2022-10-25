;; -*- lexical-binding: t; -*-

(require 'rx)

;; Language supports

;;; Misc

;; Use text mode for file that doesn't have an extension.
(add-to-list 'auto-mode-alist '("/[^./]*\\'" . text-mode))
;; Use conf-mode for dotfiles.
(add-to-list 'auto-mode-alist '("/\\.[^/]*\\'" . conf-mode))

;;; Elisp

(defun toki/sentence-end-double-space-for-elisp ()
  (set (make-local-variable 'sentence-end-double-space) t))

(add-hook 'emacs-lisp-mode-hook
          #'toki/sentence-end-double-space-for-elisp)

(use-package elispfl
  :straight (:host github :repo "cireu/elispfl")
  :hook ((emacs-lisp-mode . elispfl-mode)
         (ielm-mode . elispfl-ielm-mode)))

;;; Markdown

(use-package markdown-mode
  :mode (("/README\\(?:\\.md\\)?\\'" . gfm-mode)
         ("\\.m[k]d\\'" . markdown-mode))
  :config
  (toki/setq
   markdown-asymmetric-header t
   markdown-fontify-code-blocks-natively t)
  (toki/setq-default
   markdown-enable-math t))

;;; Org mode

(defun toki/org-on-latex-preview-p ()
  "See if point is on a latex fragment preview."
  (let ((ols (overlays-at (point))))
    (cl-dolist (ol ols)
      (when (eq (overlay-get ol 'org-overlay-type) 'org-latex-overlay)
        (cl-return t)))))

(defun toki/org-at-latex-fragment-p ()
  "See if point is at a latex fragment"
  (let ((datum (org-element-context)))
    (memq (org-element-type datum) '(latex-environment latex-fragment))))

(defun toki/org-buffer-has-latex-preview-p ()
  "See if the buffer contains LaTeX fragment preview."
  (let ((ols (overlays-in (point-min) (point-max))))
    (cl-dolist (ol ols)
      (when (eq (overlay-get ol 'org-overlay-type) 'org-latex-overlay)
        (cl-return t)))))

(defun toki-toggle-latex-preview ()
  "Toggle latex preview of fragment or buffer."
  (interactive)
  (cond
   ((or (toki/org-on-latex-preview-p) (toki/org-at-latex-fragment-p))
    (org-latex-preview))
   ((toki/org-buffer-has-latex-preview-p)
    (org-clear-latex-preview (point-min) (point-max)))
   (t
    (message "Generating preview...")
    (org--latex-preview-region (point-min) (point-max))
    (message "Generating preview... Done."))))

(defun toki-clear-latex-preview-cache ()
  "Clear LaTeX fragments preview cache."
  (interactive)
  (when (and (file-exists-p org-preview-latex-image-directory)
             (file-directory-p org-preview-latex-image-directory))
    (delete-directory org-preview-latex-image-directory 'recursive)))

(use-package org
  :straight nil
  :defer t
  :config
  ;; General Config
  (toki/setq org-startup-truncated nil
             org-startup-with-inline-images t
             org-link-descriptive nil
             org-edit-src-content-indentation 0
             org-image-actual-width
             (list (/ (min (display-pixel-height) (display-pixel-width)) 2.5)))
  ;; Inline format for CJK
  (setcar (nthcdr 0 org-emphasis-regexp-components) " \t('\"{[:nonascii:]")
  (setcar (nthcdr 1 org-emphasis-regexp-components) "- \t.,:!?;'\")}\\[[:nonascii:]")
  (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
  (org-element-update-syntax)
  ;; Otherwise when using "_" to mark CJK parts, Org thinks it's two subscripts.
  (setq org-use-sub-superscripts "{}")
  ;; LaTeX Preview
  (plist-put org-format-latex-options :scale 1.6)
  (when (executable-find "dvisvgm")
    (setq org-latex-create-formula-image-program 'dvisvgm))
  ;; Keybinds
  (toki-local-def
    :keymaps 'org-mode-map
    "t" '(org-insert-structure-template :wk "Insert Template")
    "p" `(,(toki-make-combo toki-toggle-latex-preview) :wk "<> LaTeX Preview")
    "P" '(toki-clear-latex-preview-cache :wk "Clear LaTeX Preview Cache")))

;;; Web

(use-package web-mode
  :mode (("\\.html?\\'" . web-mode)
         ("\\.css\\'" . web-mode)
         ("\\.scss\\'" . web-mode)
         ("\\.xml\\'" . web-mode))
  :config
  (toki/setq
   web-mode-markup-indent-offset 2
   web-mode-enable-auto-expanding t
   web-mode-enable-auto-quoting nil
   web-mode-css-indent-offset 2))

(toki/setq js-indent-level 2)

;;; Yaml

(use-package yaml-mode
  :defer t)

;;; Julia

(use-package julia-mode
  :defer t)
