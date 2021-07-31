;; -*- lexical-binding: t; -*-

;; Auto-completion, finding definition, version control, linting, etc.

;; TODO (maybe): debuffer, snippet, repl, help system

;;; Checker

(use-package flycheck
  :defer t
  :config
  (toki/setq
   flycheck-emacs-lisp-load-path 'inherit
   flycheck-check-syntax-automatically '(save mode-enabled)))

;;; Auto completion

(use-package company
  :hook (pre-command-hook . global-company-mode)
  :config
  (toki/setq
   company-backends '(company-dabbrev)
   company-minimum-prefix-length 2
   company-dabbrev-char-regexp "[[:alnum:]-_]"
   company-tooltip-align-annotations nil
   company-tooltip-limit 10
   company-idle-delay 0.2)
  (general-def company-active-map
    "C-p" 'company-select-previous
    "C-n" 'company-select-next)
  (defun toki/set-prog-company-backend ()
    (set (make-local-variable 'company-backends)
         '(company-dabbrev-code)))
  (add-hook 'prog-mode-hook #'toki/set-prog-company-backend))

;;; Ctags

(use-package citre
  :straight (:host github :repo "universal-ctags/citre")
  :defer t
  :init
  (require 'citre-config)
  :config
  (toki/setq
   citre-project-root-function #'toki-project-root))

;;; Code reading

(use-package clue
  :straight (:host github :repo "AmaiKinono/clue")
  :init
  (add-hook 'find-file-hook #'clue-auto-enable-clue-mode))

;;; Version Control

;; TODO: tool for resolving conflict (smerge?)

(use-package diff-hl
  :trigger find-file-noselect
  :config
  (toki/setq
   diff-hl-draw-borders nil)
  (when toki-gui-p
    (face-spec-set 'diff-hl-insert
                   '((((background light))
                      :background "#a3c776")
                     (t
                      :background "#4b690f")))
    (face-spec-set 'diff-hl-delete
                   '((((background light))
                      :background "#fc7465")
                     (t
                      :background "#e13320")))
    (face-spec-set 'diff-hl-change
                   '((((background light))
                      :background "#4cc4c2")
                     (t
                      :background "#007172"))))
  (global-diff-hl-mode)
  (diff-hl-flydiff-mode)
  (when (not toki-gui-p)
    (diff-hl-margin-mode)))

;;; Keybinds

(toki-code-def
  "c" '(completion-at-point :wk "Auto Complete")
  "f" '(flycheck-mode :wk "<> Flycheck Mode")
  "F" '(flycheck-list-errors :wk "List Flycheck Errors")
  "p" '(citre-ace-peek :wk "Peek Definition")
  "u" '(citre-update-this-tags-file :wk "Update Tags File")
  "C" '(clue-copy :wk "Copy Location")
  "P" '(clue-paste :wk "Paste Location"))

(toki-vc-def
  "a" '(diff-hl-amend-mode :wk "<> Amend Mode")
  "d" '(vc-diff :wk "Diff")
  "r" '(diff-hl-revert-hunk :wk "Revert Hunk")
  "p" '(diff-hl-previous-hunk :wk "Prev Hunk")
  "n" '(diff-hl-next-hunk :wk "Next Hunk")
  "l" '(vc-print-root-log :wk "Print Log")
  "L" '(vc-print-log :wk "Print File Log")
  "R" '(diff-hl-set-reference-rev :wk "Set Reference Rev")
  "C-r" '(diff-hl-reset-reference-rev :wk "Reset Reference Rev"))
