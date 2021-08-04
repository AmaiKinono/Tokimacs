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

;;; Keybinds

(toki-elisp-def
  "e" '(eval-last-sexp :wk "Eval Last Sexp")
  "p" '(eval-print-last-sexp :wk "Eval and Print")
  "E" '(eval-expression :wk "Eval Sexp")
  "m" '(pp-macroexpand-last-sexp :wk "Macroexpand Last Sexp"))
