;; -*- lexical-binding: t; -*-

;; Auto-completion, finding definition, version control, linting, etc.

(require 'cl-lib)
(require 'rx)

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
  :hook (pre-command . global-company-mode)
  :config
  (toki/setq
   company-backends '(company-dabbrev-code company-dabbrev)
   company-minimum-prefix-length 2
   company-dabbrev-code-everywhere t
   company-tooltip-align-annotations nil
   company-tooltip-limit 5)
  (define-advice company-dabbrev--filter (:around (fn &rest args) more-filter)
    "Further filter canndidates provided by `company-dabbrev'/`company-dabbrev-code' backend.
Currently this removes long candidate that contains non-latin
chars.  This should be useful for CJK users."
    (let ((cands (apply fn args))
          (exclude (lambda (cand)
                     (or (and (string-match (rx (not (category latin))) cand)
                              (> (length cand) 12))))))
      (cl-remove-if exclude cands)))
  ;; NOTE: "RET" and "TAB" are for terminal Emacs (equivalent to "C-m" and
  ;; "C-i"), while "<return>" and "<tab>" are for GUI Emacs.
  (general-unbind
    :keymaps 'company-active-map
    "C-s" "RET" "<return>")
  (general-def
    :keymaps 'company-active-map
    "C-p" 'company-select-previous
    "C-n" 'company-select-next
    "TAB" 'company-complete
    "<tab>" 'company-complete))

;;; Ctags

(use-package citre
  :defer t
  :init
  (require 'citre-config)
  :config
  (toki/setq
   citre-project-root-function #'toki-project-root))

(use-package citre-global
  :straight nil
  :trigger 'citre-mode-hook
  :init
  (general-def
    :keymaps 'citre-peek-keymap
    "M-l r" 'citre-peek-through-references))

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
  "r" '(citre-ace-peek-references :wk "Peek Reference")
  "u" '(citre-update-this-tags-file :wk "Update Tags File")
  "U" '(citre-global-update-database :wk "Update Global DB")
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
