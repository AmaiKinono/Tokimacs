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

;;;; Search

;; Isearch is the Emacs built-in primary search & replace interface.  It's
;; default UI has serious usability problem:

;; - You can only use backspace to modify the search query.
;; - While searching, if you type any non-isearch command, it quits the search
;;   session.
;; - There's an `isearch-edit-string' command, which allows editing search
;;   query in the minibuffer.  Unfortunately, while doing this, the matches in
;;   the buffer don't update as you type.

;; Isearch-mb lets you edit your search query in the minibuffer, by default,
;; with the matches update as you type.  This is the right design.

(use-package isearch-mb
  :trigger pre-command-hook
  :config
  (toki/setq-default
   isearch-lazy-count t)
  (general-def
    :keymaps 'isearch-mb-minibuffer-map
    "C-n" 'isearch-repeat-forward
    "C-p" 'isearch-repeat-backward
    "C-." 'toki-search-insert-.*)
  (toki-local-def
    :keymaps 'isearch-mb-minibuffer-map
    "r" '(isearch-query-replace :wk "Replace")
    "C-j" '(newline :wk "[Newline]")
    "." '(toki-search-insert-.* :wk "[.*]")
    "a" '(toki-search-insert-anychar :wk "[Anychar]")
    "g" '(toki-search-insert-group :wk "[Group]")
    "w" '(toki-search-wrap-word-boundary :wk "(Word Bounds)")
    "s" '(toki-search-wrap-symbol-boundary :wk "(Symbol Bounds)")
    "C" '(isearch-toggle-case-fold :wk "<> Case Fold")
    "R" '(isearch-toggle-regexp :wk "<> Regexp Search"))
  (define-advice isearch-mb--update-prompt (:around (fn &rest _) show-case-fold-info)
    "Show case fold info in the prompt."
    (when isearch-mb--prompt-overlay
      (let ((count (isearch-lazy-count-format))
            (len (or (overlay-get isearch-mb--prompt-overlay 'isearch-mb--len) 0)))
        (overlay-put isearch-mb--prompt-overlay
                     'isearch-mb--len (max len (length count)))
        (overlay-put isearch-mb--prompt-overlay
                     'before-string
                     (concat count ;; Count is padded so that it only grows.
                             (make-string (max 0 (- len (length count))) ?\ )
                             ;; PATCH
                             (if isearch-case-fold-search "[Case Fold] " "")
                             (capitalize
                              (isearch--describe-regexp-mode
                               isearch-regexp-function)))))))

  (isearch-mb-mode))

(with-eval-after-load 'isearch-mb
  (defun toki-search-insert-.* ()
    "Insert \".*\"."
    (interactive)
    (insert ".*"))

  (defun toki-search-insert-anychar ()
    "Insert regexp for anychar including newline."
    (interactive)
    (insert "[^z-a]"))

  (defun toki-insert-group ()
    "Insert a pair of regexp group delimiter, or wrap them around active region."
    (interactive)
    (when (use-region-p)
      (save-excursion
        (goto-char (region-beginning))
        (insert "\\(")
        (goto-char (region-end))
        (insert "\\)"))
      (insert "\\(\\)")))

  (defun toki-search-wrap-word-boundary ()
    "Wrap active region or the search query inside a pair of word boundary."
    (interactive)
    (save-excursion
      (goto-char (if (use-region-p) (region-beginning) (minibuffer-prompt-end)))
      (insert "\\<")
      (goto-char (if (use-region-p) (region-end) (point-max)))
      (insert "\\>")))

  (defun toki-search-wrap-symbol-boundary ()
    "Wrap active region or the search query inside a pair of symbol boundary."
    (interactive)
    (save-excursion
      (goto-char (if (use-region-p) (region-beginning) (minibuffer-prompt-end)))
      (insert "\\_<")
      (goto-char (if (use-region-p) (region-end) (point-max)))
      (insert "\\_>"))))

;;;; Replace

;; Emacs has complex "smart" case treatment for replacing (see FIXEDCASE arg in
;; `replace-match').  I call it "soft case".  Here are some hacks so when using
;; edit the replacement string, the user could see if soft/hard case is being
;; used.

(defvar toki-replace-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map minibuffer-local-map)
    map)
  "Keymap used for `query-replace-read-to'.")

(toki-local-def
  :keymaps 'toki-replace-map
  "m" '(toki-replace-insert-whole-match :wk "[Whole Match]")
  "c" '(toki-replace-insert-counter :wk "[Counter]")
  "i" '(toki-replace-insert-user-input :wk "[User Input]")
  "l" '(toki-replace-insert-lisp-expression :wk "[Lisp]")
  "C" '(toki-replace-toggle-soft-case :wk "<> Soft Case"))

(defvar toki/replace-ol nil)

(defvar toki/case-replace-orig nil)

(defun toki/replace-minibuffer-setup-hook ()
  (setq toki/case-replace-orig case-replace)
  (when toki/replace-ol
    (delete-overlay toki/replace-ol))
  (setq toki/replace-ol (make-overlay (point-min) (point-min) (current-buffer)))
  (toki/replace-minibuffer-update-ol)
  (add-hook 'post-command-hook #'toki/replace-minibuffer-update-ol nil 'local)
  (add-hook 'minibuffer-exit-hook #'toki/replace-minibuffer-exit-hook))

(defun toki/replace-minibuffer-update-ol ()
  (overlay-put toki/replace-ol 'before-string
               (if (and case-replace case-fold-search)
                   "[Soft Case] " "[Hard Case] ")))

(defun toki/replace-minibuffer-exit-hook ()
  (remove-hook 'post-command-hook #'toki/replace-minibuffer-update-ol 'local)
  (remove-hook 'minibuffer-exit-hook #'toki/replace-minibuffer-exit-hook 'local)
  (setq case-replace toki/case-replace-orig)
  (when toki/replace-ol
    (delete-overlay toki/replace-ol)
    (setq toki/replace-ol nil)))

(define-advice query-replace-read-to (:around (fn from prompt regexp-flag)
                                              prompt-and-keymap)
  (minibuffer-with-setup-hook
      #'toki/replace-minibuffer-setup-hook
    (let ((minibuffer-local-map toki-replace-map))
      (funcall fn from prompt regexp-flag))))

(defun toki-replace-toggle-soft-case ()
  "Toggle soft case for replace."
  (interactive)
  (setq case-replace (not case-replace)))

(defun toki-replace-insert-whole-match ()
  "Insert a snippet that means the whole match."
  (interactive)
  (insert "\\&"))

(defun toki-replace-insert-counter ()
  "Insert a snippet that works as a counter from 0."
  (interactive)
  (insert "\\#"))

(defun toki-replace-insert-user-input ()
  "Insert a snippet that requires user input for every replacement."
  (interactive)
  (insert "\\?"))

(defun toki-replace-insert-lisp-expression ()
  "Insert a lisp expression."
  (interactive)
  (insert "\\,()")
  (backward-char))

(setq query-replace-help
      "<Replace>
[y/SPC] replace                         [!] replace all remaining
[.] replace only one                    [,] replace and not move to next
<Exit>
[q/RET] exit
<Move>
[n/DEL] next          [^] prev          [C-l] recenter
<Undo>
[u] undo previous                       [U] undo all
<Edit>
[C-r] recursive edit                    [\\[exit-recursive-edit]]: exit recursive edit
[C-w]: delete match and recursive edit  [E]: edit the replacement string.
<Multi-file replace>
[Y]: replace all in remaining buffers   [N]: skip to the next buffer")

;;;; Misc replace commands

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
  ;; Undo/Redo
  "C-z" 'undo-fu-only-undo
  "C-S-z" 'undo-fu-only-redo
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
  "M-m" 'set-mark-command
  ;; Search
  "C-/" 'isearch-forward-regexp)

(toki-search-def
  "s" '(isearch-forward-regexp :wk "Search Regexp")
  "S" '(isearch-forward :wk "Search Literally")
  "r" '(toki-replace-string-fold :wk "Replace String")
  "R" '(toki-replace-string-strictly :wk "Replace String (Strictly)"))

(toki-edit-def
 "u" '(undo-propose :wk "Browse Undo History")
 "n" '(narrow-to-region :wk "Narrow")
 "w" '(widen :wk "Widen")
 "8" '(toki-set-tab-width-to-8 :wk "8-char Tab for Buffer")
 "T" '(toki-tabify :wk "Tabify")
 "t" '(toki-untabify :wk "Untabify"))
