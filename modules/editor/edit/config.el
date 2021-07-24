;; -*- lexical-binding: t; -*-

;; Slick text editing experience.

;; Copyright (C) 2021 Hao Wang
;; License: GPL v3, or (at your option) any later version

;;; Defaults

(setq-default
 fill-column 79
 indent-tabs-mode nil
 tab-width 4
 truncate-partial-width-windows nil
 sentence-end-double-space nil
 require-final-newline t)

;;; Undo & Redo

;; Undo Fu is a wrapper for Emacs built-in undo system, offering undo and redo
;; commands that we would expect in a normal editor or word-processor.
(use-package undo-fu
  :defer t
  :init
  (general-def
    "C-z" 'undo-fu-only-undo
    "C-u" 'undo-fu-only-redo)
  :config
  (toki/setq
   undo-fu-ignore-keyboard-quit t))

;; undo-propose gives us a temporary buffer where we can use the built-in undo
;; command to go back to any previous state.  Then we can apply the changes as
;; only one edit event, making it easier to undo/redo later.
(use-package undo-propose
  :defer t
  :config
  (general-def 'undo-propose-mode-map
    "C-z" 'undo)
  (toki-local-def 'undo-propose-mode-map
    "c" '(undo-propose-squash-commit :wk "Squash and Commit")
    "C" '(undo-propose-commit :wk "Commit")
    "d" '(undo-propose-diff :wk "Diff")
    "q" '(undo-propose-cancel :wk "Quit")))

;;; Search & Replace

;; CTRLF is a single-buffer text search tool.  Currently it's alpha-quality,
;; and can't do replace.
(use-package ctrlf
  :straight (:host github :repo "raxod502/ctrlf")
  :defer t
  ;; CTRLF doesn't do autoload now, so we do it manually.
  :commands (ctrlf-forward ctrlf-backward
             ctrlf-forward-fuzzy ctrlf-backward-fuzzy
             ctrlf-forward-regexp ctrlf-backward-regexp
             ctrlf-forward-fuzzy-regexp ctrlf-backward-fuzzy-regexp
             ctrlf-mode)
  :init
  (toki-search-def
    "s" '(ctrlf-forward-fuzzy-regexp :wk "Search")
    "S" '(ctrlf-forward-fuzzy :wk "Literal Search"))
  (general-def
    "C-/" 'ctrlf-forward-fuzzy-regexp)
  :config
  (face-spec-set 'ctrlf-highlight-active
                 '((((background light))
                    :background "#ff74a4" :foreground "#eeeeee" :bold t)
                   (t
                    :background "#ffb0bb" :foreground "#111111" :bold t))
                 'face-defface-spec)
  (face-spec-set 'ctrlf-highlight-passive
                 '((((background light))
                    :background "#87ceeb" :foreground "#222222")
                   (t
                    :background "#4367a5" :foreground "#dddddd"))
                 'face-defface-spec)
  (face-spec-set 'ctrlf-highlight-line
                 '((((background light))
                    :background "#e4e4e4")
                   (t
                    :background "#444444")))
  (toki/setq
   ctrlf-auto-recenter t
   ctrlf-minibuffer-bindings
   '(([remap abort-recursive-edit]     . ctrlf-cancel)
     ([remap minibuffer-keyboard-quit] . ctrlf-cancel)
     ([remap beginning-of-buffer]      . ctrlf-first-match)
     ([remap end-of-buffer]            . ctrlf-last-match)
     ("C-n"       . ctrlf-next-match)
     ("TAB"       . ctrlf-next-match)
     ("C-p"       . ctrlf-previous-match)
     ("S-TAB"     . ctrlf-previous-match)
     ("<backtab>" . ctrlf-previous-match))))

(defun toki-replace-string-fold ()
  "Replace string in buffer or region, with case folded.
This means matches should ignore case if your input is all
lowercase, and replacing preserves the case of text to be
replaced.  This is suitable for writing.

Notice that `replace-lax-whitespace' and `replace-char-fold' can
also affect the result, which is not handled by this command.
See the docstring of `replace-string' for details."
  (interactive)
  (let ((case-fold-search t)
        (case-replace t))
    (if (use-region-p)
        (call-interactively #'replace-string)
      (save-excursion
        (goto-char (point-min))
        (call-interactively #'replace-string)))))

(defun toki-replace-string-strictly ()
  "Replace string in buffer or region, case strictly.
This means matching is case sensitive, and replacing doesn't
preserve the case of text to be replaced.  This is suitable for
programming.

Notice that `replace-lax-whitespace' and `replace-char-fold' can
also affect the result, which is not handled by this command.
See the docstring of `replace-string' for details."
  (interactive)
  (let ((case-fold-search nil)
        (case-replace nil))
    (if (use-region-p)
        (call-interactively #'replace-string)
      (save-excursion
        (goto-char (point-min))
        (call-interactively #'replace-string)))))

(toki-search-def
  "r" '(toki-replace-string-fold :wk "Replace String")
  "R" '(toki-replace-string-strictly :wk "Replace String (Strictly)"))

;; TODO: grep integration

;;; Structural editing

;; toki-editing offers fine and structural editing commands
(use-package toki-editing
  :straight nil
  :trigger after-init-hook
  :config
  (defun toki-forward-punct-or-delim ()
    (interactive)
    (if prog-mode
        (toki-forward-list-delimiter)
      (toki-forward-punct)))
  (defun toki-backward-punct-or-delim ()
    (interactive)
    (if prog-mode
        (toki-backward-list-delimiter)
      (toki-backward-punct)))
  (defun toki-forward-subsentence-or-punct ()
    (interactive)
    (if prog-mode
        (toki-forward-punct)
      (toki-forward-subsentence)))
  (defun toki-backward-subsentence-or-punct ()
    (interactive)
    (if prog-mode
        (toki-backward-punct)
      (toki-backward-subsentence)))
  (toki-global-sedit-mode))

(use-package elec-pair
  :straight nil
  :trigger pre-command-hook
  :config
  (electric-pair-mode))

;;; Misc

(use-package editorconfig
  :trigger find-file-noselect
  :config
  (editorconfig-mode)
  (add-hook 'hack-local-variables-hook
            #'editorconfig-apply))

;; Whitespace lets us visualize blanks, mainly bad blanks like trailing
;; whitespace, see the configuration below.
(use-package whitespace
  :straight nil
  :trigger find-file-noselect
  :config
  (global-whitespace-mode)
  ;; Don't use different background for tabs.
  (face-spec-set 'whitespace-tab
                 '((t :background unspecified)))

  ;; Only use background and underline for long lines, so we can still have
  ;; syntax highlight.

  ;; For some reason use face-defface-spec as spec-type doesn't work.  My guess
  ;; is it's due to the variables with the same name as the faces in
  ;; whitespace.el.  Anyway, we have to manually set some attribute to
  ;; unspecified here.
  (face-spec-set 'whitespace-line
                 '((((background light))
                    :background "#d8d8d8" :foreground unspecified
                    :underline t :weight unspecified)
                   (t
                    :background "#404040" :foreground unspecified
                    :underline t :weight unspecified)))

  ;; Use softer visual cue for trailing whitespaces.
  (face-spec-set 'trailing-whitespace
                 '((((background light))
                    :background "#e5d5d5")
                   (t
                    :background "#483838")))

  ;; Use softer visual cue for space before tabs.
  (face-spec-set 'whitespace-space-before-tab
                 '((((background light))
                    :background "#d8d8d8" :foreground "#de4da1")
                   (t
                    :background "#404040" :foreground "#ee6aa7")))

  (toki/setq
   whitespace-line-column nil
   whitespace-style
   '(face             ; visualize things below:
     empty            ; empty lines at beginning/end of buffer
     lines-tail       ; lines go beyond `fill-column'
     space-before-tab ; spaces before tab
     trailing         ; trailing blanks
     tabs             ; tabs (show by face)
     tab-mark         ; tabs (show by symbol)
     )))

;; whitespace-cleanup-mode automatically clean up bad blanks (like trailing
;; whitespaces, empty lines at the end of file) when we save, but only when the
;; file is initially clean.  So we don't introduce a lot of changes when
;; working on projects that come with whitespace problems.
(use-package whitespace-cleanup-mode
  ;; Whether a buffer is "initially clean" is decided when
  ;; `whitespace-cleanup-mode' is enabled, so we want to load the package
  ;; before any editing.
  :trigger pre-command-hook
  :config
  (toki/setq whitespace-cleanup-mode-preserve-point t)
  (global-whitespace-cleanup-mode))

;; Delete selection when we type or paste.
(use-package delsel
  :straight nil
  :trigger pre-command-hook
  :config
  (delete-selection-mode))

(defun toki-set-tab-width-to-8 ()
  "Set the tab width of current buffer to 8.
Many codes indent using 8-char-wide tabs, but don't specify it
using file local variables.  This command is for working with
these codes."
  (interactive)
  (setq tab-width 8))

;;; Keybinds

;;;; Hack of remapping C-c/C-x

(general-def
  :keymaps 'override
  "C-x" (general-predicate-dispatch nil
          (region-active-p) 'toki-kill-region
          t (general-key "C-x"
              :setup (general-override-mode -1)
              :teardown (general-override-mode)))
  "C-c" (general-predicate-dispatch nil
          (region-active-p) 'kill-ring-save
          t (general-key "C-c"
              :setup (general-override-mode -1)
              :teardown (general-override-mode))))

(toki-leader-def
  "C-x" 'Control-X-prefix
  "C-c" (general-key "C-c"
          :setup (general-override-mode -1)
          :teardown (general-override-mode)))

;; C-c won't show by which-key, since it doesn't display menu items.  See
;; https://github.com/justbur/emacs-which-key/issues/177.  This is a workaround
;; that puts both C-c/C-x descriptions under C-x.
(with-eval-after-load 'which-key
  (push (cons
         (cons (concat "\\`" toki-leader-key " C-x\\'") nil)
         '("C-x/C-c" . "C-x/C-c"))
        which-key-replacement-alist))

(general-def
  ;; Word
  "M-f" 'toki-forward-word
  "M-b" 'toki-backward-word
  ;; Line
  "C-a" 'toki-beginning-of-line
  "C-e" 'toki-end-of-line
  ;; Sentence
  "M-\"" 'toki-forward-subsentence-or-punct
  "M-'" 'toki-backward-subsentence-or-punct
  "M-." 'toki-forward-punct-or-delim
  "M-," 'toki-backward-punct-or-delim
  ;; Paragraph
  "M-n" 'forward-paragraph
  "M-p" 'backward-paragraph
  ;; Edit
  "M-h" 'toki-shrink-whitespace
  "C-v" 'yank
  "M-v" 'consult-yank-from-kill-ring
  ;; Scroll
  "C-r" 'toki-smooth-scroll-half-page-up
  "C-t" 'toki-smooth-scroll-half-page-down
  "C-l" 'toki-recenter
  ;; Mark
  "M-m" 'set-mark-command)

(toki-edit-def
 "u" '(undo-propose :wk "Browse Undo History")
 "n" '(narrow-to-region :wk "Narrow")
 "w" '(widen :wk "Widen")
 "8" '(toki-set-tab-width-to-8 :wk "8-char Tab for Buffer")
 "T" '(toki-tabify :wk "Tabify")
 "t" '(toki-untabify :wk "Untabify"))
