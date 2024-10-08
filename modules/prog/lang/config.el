;; -*- lexical-binding: t; -*-

(require 'rx)

;; Language supports

;;; Misc

;; Use text mode for file that doesn't have an extension.
(add-to-list 'auto-mode-alist '("/[^./]*\\'" . text-mode))
;; Use conf-mode for dotfiles.
(add-to-list 'auto-mode-alist '("/\\.[^/]*\\'" . conf-mode))

;;; Tree Sitter

;; Run `treesit-auto-install-all' to install all grammars.
(use-package treesit-auto
  :trigger find-file-noselect
  :config
  (treesit-auto-add-to-auto-mode-alist)
  (global-treesit-auto-mode))

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
             (list (round (/ (min (display-pixel-height) (display-pixel-width))
                             2.5))))
  ;; HTML export style
  (toki/setq org-html-head
             "<style>
  img { max-width: 100%; }
</style>")
  ;; Inline emphasizing for CJK.  Commented as the `ox' configuration solves
  ;; the problem in another way.
  ;; (setcar (nthcdr 0 org-emphasis-regexp-components) " \t('\"{[:nonascii:]")
  ;; (setcar (nthcdr 1 org-emphasis-regexp-components) "- \t.,:!?;'\")}\\[[:nonascii:]")
  ;; (org-set-emph-re 'org-emphasis-regexp-components org-emphasis-regexp-components)
  ;; (org-element-update-syntax)

  ;; LaTeX Preview
  (when (executable-find "dvisvgm")
    (setq org-latex-create-formula-image-program 'dvisvgm))
  ;; Commands
  (defun toki-insert-zero-width-space ()
    "Insert an zero width space."
    (interactive)
    (insert "\u200b"))
  ;; Keybinds
  (general-unbind
    :keymaps 'org-mode-map
    ;; "C-'" is bind to `isearch-backward-regexp' by us.
    "C-'")
  (toki-local-def
    :keymaps 'org-mode-map
    "b" '("Edit Block" . org-edit-special)
    "t" '("Insert Template" . org-insert-structure-template)
    "p" `("<> LaTeX Preview" . ,(toki-make-combo toki-toggle-latex-preview))
    "P" '("Clear LaTeX Preview Cache" . toki-clear-latex-preview-cache)
    "SPC" '("Insert Zero Width Space" . toki-insert-zero-width-space)))

(use-package ox
  :straight nil
  :after org
  :config
  ;; Zero width spaces (ZWS) are recommended to resolve conflict between plain
  ;; text and markups.  So, in the final export, we remove single ZWSs.  If you
  ;; want to include n ZWSs, you should type n+1 ZWSs.

  ;; This is mainly used for CJK emphasizing, which was solved by setting
  ;; `org-emphasis-regexp-components' before.  Unfortunately the exporters
  ;; doesn't respect that variable, which may due to that fontification and
  ;; parser are inconsistent in org-mode
  ;; (https://list.orgmode.org/87ee7c9quk.fsf@localhost/).
  (defun toki/org-export-unescape-zero-width-space (text _backend _info)
    "Unescape zero width spaces in org export."
    (unless (org-export-derived-backend-p 'org)
      (replace-regexp-in-string (rx "\u200b" (group (* "\u200b")))
                                "\\1" text)))
  (add-to-list 'org-export-filter-final-output-functions
               #'toki/org-export-unescape-zero-width-space t))

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

;;; Rust

(use-package rust-mode
  :defer t)
