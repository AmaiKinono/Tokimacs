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
  (define-advice company-pseudo-tooltip-show (:around (fn &rest args) dont-hide-text)
    "Don't let company tooltip mask my text.
You'll notice when the tooltip pops up, the text below it are
pushed down.  This is my desired behavior, because:

- The tooltip pops up while I'm typing.  I don't explicitely
  control it.
- I may want to read the code below current line at anytime while
  typing."
    (cl-letf* ((make-overlay-orig
                (symbol-function 'make-overlay))
               ((symbol-function 'make-overlay)
                (lambda (beg end &rest args)
                  (apply make-overlay-orig beg beg args))))
      (apply fn args)))
  (define-advice company-buffer-lines (:around (fn &rest _) dont-hide-text)
    "See the `dont-hide-text' advice around `company-pseudo-tooltip-show'."
    nil)
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
