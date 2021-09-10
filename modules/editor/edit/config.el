;; -*- lexical-binding: t; -*-

;; Slick text editing experience.

;; Copyright (C) 2021 Hao Wang
;; License: GPL v3, or (at your option) any later version

(require 'rx)

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

;; AFAIK, undo-fu wraps emacs built-in `undo-only' and `undo-redo' (in Emacs
;; 28) commands.  So we shouldn't need it after we have Emacs 28.
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
    "g" '(toki-search-insert-group :wk "(Group)")
    "w" '(toki-search-insert-word-boundary :wk "(Word Bounds)")
    "s" '(toki-search-insert-symbol-boundary :wk "(Symbol Bounds)")
    "C" '(isearch-toggle-case-fold :wk "<> Case Fold")
    "R" '(isearch-toggle-regexp :wk "<> Regexp Search"))
  (define-advice isearch-mb--update-prompt (:around (fn &rest args) show-case-fold-info)
    "Show case fold info in the prompt."
    (cl-letf* ((isearch--describe-regexp-mode-orig
                (symbol-function 'isearch--describe-regexp-mode))
               ((symbol-function 'isearch--describe-regexp-mode)
                (lambda (regexp-function &optional space-before)
                  (concat (if isearch-case-fold-search "[Case Fold] " "")
                          (funcall isearch--describe-regexp-mode-orig
                                   regexp-function space-before)))))
      (funcall fn args)))
  (isearch-mb-mode))

;; Ref: https://stackoverflow.com/questions/285660/automatically-wrapping-i-search/36707038#36707038

(define-advice isearch-search (:around (fn) auto-wrap)
  "Auto-wrap for isearch commands."
  (funcall fn)
  (unless isearch-success
    ;; `isearch-repeat' calls `isearch-search'.  We need to use the unadvised
    ;; version, or when the wrapped search also fails, it will call
    ;; `isearch-repeat' in turn and reach max callstack depth.
    (cl-letf (((symbol-function 'isearch-search)
               (ad-get-orig-definition #'isearch-search)))
      (isearch-repeat (if isearch-forward 'forward 'backward)))))

;; The current count is 0 when we begin searching using isearch-mb.  This is
;; fixed in Emacs 28, the following advice is a dirty backport.
(when (version< emacs-version "28")
  (define-advice isearch-lazy-highlight-buffer-update (:around (fn &rest args) fix)
  "Fix the problem of current count being 0 when starting the search."
  (cl-letf* ((my-opoint nil)
             (window-group-start-orig (symbol-function 'window-group-start))
             ((symbol-function 'window-group-start)
              (lambda (&optional window)
                (setf my-opoint (point))
                (funcall window-group-start-orig window)))
             (gethash-orig (symbol-function 'gethash))
             ((symbol-function 'gethash)
              (lambda (key &rest args)
                (if (integerp key)
                    (apply gethash-orig my-opoint args)
                  (apply gethash-orig key args)))))
    (apply fn args))))

(with-eval-after-load 'isearch-mb
  (defun toki-search-insert-.* ()
    "Insert \".*\"."
    (interactive)
    (insert ".*"))

  (defun toki-search-insert-anychar ()
    "Insert regexp for anychar including newline."
    (interactive)
    (insert "[^z-a]"))

  (defun toki-search-insert-group ()
    "Insert a pair of regexp group delimiter, or wrap them around active region."
    (interactive)
    (if (use-region-p)
        (let ((beg (region-beginning))
              (end (region-end)))
          (save-excursion
            (goto-char end)
            (insert "\\)")
            (goto-char beg)
            (insert "\\(")))
      (insert "\\(\\)")
      (forward-char -2)))

  (defun toki-search-insert-word-boundary ()
    "Insert a pair of word boundary, or wrap them around active region."
    (interactive)
    (if (use-region-p)
        (let ((beg (region-beginning))
              (end (region-end)))
          (save-excursion
            (goto-char end)
            (insert "\\>")
            (goto-char beg)
            (insert "\\<")))
      (insert "\\<\\>")
      (forward-char -2)))

  (defun toki-search-insert-symbol-boundary ()
    "Insert a pair of symbol boundary, or wrap them around active region."
    (interactive)
    (if (use-region-p)
        (let ((beg (region-beginning))
              (end (region-end)))
          (save-excursion
            (goto-char end)
            (insert "\\_>")
            (goto-char beg)
            (insert "\\_<")))
      (insert "\\_<\\_>")
      (forward-char -3))))

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
  "Show prompt for soft/hard case, and use our `toki-replace-map'.
See the docstring about FIXEDCASE in `replace-match' to know about
soft/hard case.  Non-nil FIXEDCASE equals \"hard case\"."
  (minibuffer-with-setup-hook
      #'toki/replace-minibuffer-setup-hook
    (let ((minibuffer-local-map toki-replace-map))
      (funcall fn from prompt regexp-flag))))

(define-advice perform-replace (:around (fn &rest args) dont-exit-on-anykey)
  "Don't exit replace for anykey that's not in `query-replace-map'."
  (cl-letf* ((lookup-key-orig
              (symbol-function 'lookup-key))
             ((symbol-function 'lookup-key)
              (lambda (map key &optional accept-default)
                (or (apply lookup-key-orig map key accept-default)
                    (when (eq map query-replace-map) 'help)))))
    (apply fn args)))

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

;; TODO: grep integration

;; TODO: more ideas on structural editing; evil-matchit, embrace

(use-package puni
  :defer t
  :init
  ;; The default `puni-mode-map' respects "Emacs conventions".  We don't, so
  ;; it's better to simply rewrite it.
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "DEL") 'puni-backward-delete-char)
    (define-key map (kbd "C-d") 'puni-forward-delete-char)
    (define-key map (kbd "M-d") 'puni-forward-kill-word)
    (define-key map (kbd "M-DEL") 'puni-backward-kill-word)
    (define-key map (kbd "C-k") 'puni-kill-line)
    (define-key map (kbd "C-u") 'puni-backward-kill-line)
    (define-key map (kbd "C-h") 'puni-force-delete)
    (define-key map (kbd "C-M-f") 'puni-forward-sexp)
    (define-key map (kbd "C-M-b") 'puni-backward-sexp)
    (define-key map (kbd "C-M-a") 'puni-beginning-of-sexp)
    (define-key map (kbd "C-M-e") 'puni-end-of-sexp)
    (setq puni-mode-map map))
  (puni-global-mode)
  ;; We want keys like C-k, M-DEL to be handled by the shell program.
  (add-hook 'term-mode-hook #'puni-disable-puni-mode)
  :config
  (toki/setq puni--debug t))

;; We don't map soft delete commands defined in toki-editing for now, to test
;; puni in daily use.
(use-package toki-editing
  :straight nil
  :defer t)

(use-package elec-pair
  :straight nil
  :trigger pre-command-hook
  :config
  (electric-pair-mode))

;; paren is a package for showing matching parens.
(use-package paren
  :straight nil
  :trigger pre-command-hook
  :init
  ;; NOTE: I just can't find a good way to redefine a face.  I know
  ;; `face-spec-set', I know there are face-related standard properties of
  ;; symbols, I know `theme-face' is kind of a hidden spec of a face...  But I
  ;; can't come up with a way to redefine a face.  We must manually set
  ;; something to `unspecified'.
  (face-spec-set 'show-paren-match
                 '((t :foreground unspecified :background unspecified
                      :inherit error)))
  :config
  (define-advice show-paren-function (:around (fn) fix)
    "Highlight enclosing parens when point is inside them."
    (if (looking-at-p (rx (* white) (syntax open-parenthesis)))
        (funcall fn)
      (save-excursion
        (ignore-errors (backward-up-list))
        (funcall fn))))
  (toki/setq
   show-paren-when-point-in-periphery t
   show-paren-when-point-inside-paren t)
  (show-paren-mode))

;;; Misc packages

(use-package avy
  :defer t
  :config
  (toki/setq
   avy-all-windows nil))

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
  :trigger after-init-hook
  :config
  (global-whitespace-mode)
  ;; Don't use different background for tabs.
  (face-spec-set 'whitespace-tab
                 '((t :background unspecified)))

  ;; Only use underline for long lines, so we can still have
  ;; syntax highlight.
  (face-spec-set 'whitespace-line
                 '((t :background unspecified :foreground unspecified
                      :underline t :weight unspecified)))

  ;; Use softer visual cue for trailing whitespaces.
  (face-spec-set 'whitespace-trailing
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

  (face-spec-set 'whitespace-empty
                 '((((background light))
                    :background "#e7dfdf")
                   (t
                    :background "#353535")))

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

;;; Misc commands

(defun toki-set-tab-width-to-8 ()
  "Set the tab width of current buffer to 8.
Many codes indent using 8-char-wide tabs, but don't specify it
using file local variables.  This command is for working with
these codes."
  (interactive)
  (setq tab-width 8))

(defun toki-quit-emacs ()
  "Confirm and quit Emacs."
  (interactive)
  (when (y-or-n-p "Really quit Emacs?")
    (save-buffers-kill-terminal)))

(defun toki-reload-init-file ()
  "Reload init file."
  (interactive)
  (load-file user-init-file))

;;; Keybinds

;;;; Hack of remapping C-c/C-x

(general-def
  :keymaps 'override
  "C-x" (general-predicate-dispatch nil
          (region-active-p) 'puni-kill-active-region
          t (general-key "C-x"
              :setup (general-override-mode -1)
              :teardown (general-override-mode)))
  "C-c" (general-predicate-dispatch nil
          (region-active-p) 'kill-ring-save
          t (general-key "C-c"
              :setup (general-override-mode -1)
              :teardown (general-override-mode))))

(toki-leader-def
  "C-x" '(Control-X-prefix :wk "C-x")
  "C-c" (general-key "C-c"
          :setup (general-override-mode -1)
          :teardown (general-override-mode)))

;; C-c won't show by which-key, since it doesn't display menu items.  See
;; https://github.com/justbur/emacs-which-key/issues/177.  This is a workaround
;; that puts both C-c/C-x descriptions under C-x.
(with-eval-after-load 'which-key
  (push '(("C-c" . nil) . (nil . "C-c"))
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
  "M-." 'puni-syntactic-forward-punct
  "M-," 'puni-syntactic-backward-punct
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
  "C-/" 'isearch-forward-regexp
  ;; Avy
  "C-'" 'avy-goto-line
  "C-\"" 'avy-goto-char-2)

(toki-leader-def
  "i" '(universal-argument :wk "Universal Arg")
  "C-h" '(help-command :wk "C-h"))

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
  "t" '(toki-untabify :wk "Untabify")
  "i" '(insert-char :wk "Insert Char")
  "v" '(consult-yank-from-kill-ring :wk "Clipboard")
  "c" '(capitalize-dwim :wk "Capitalize")
  "U" '(upcase-dwim :wk "Upcase")
  "d" '(downcase-dwim :wk "Downcase")
  "C" '(:ignore t :wk "Coding System")
  "Cd" '(describe-current-coding-system :wk "Describe Current")
  "Cr" '(revert-buffer-with-coding-system :wk "Reopen With")
  "Cc" '(set-buffer-file-coding-system :wk "Convert To")
  "q" '(toki-quit-emacs :wk "Quit Emacs")
  "r" '(toki-reload-init-file :wk "Reload Config"))

(toki-help-def
  "t" '(which-key-show-top-level :wk "Top Level Keybinds")
  "k" '(describe-key :wk "Key")
  "p" '(describe-package :wk "Package")
  "c" '(describe-char :wk "Char at Point")
  "f" '(describe-function :wk "Function")
  "F" '(describe-face :wk "Face")
  "v" '(describe-variable :wk "Variable"))

(toki-navigate-def
  "i" '(imenu :wk "Imenu")
  "l" '(goto-line :wk "Goto Line")
  "B" '(minibuffer-beginning-of-buffer :wk "Beginning of Buffer")
  "E" '(end-of-buffer :wk "End of Buffer")
  "'" '(avy-goto-line :wk "Avy: Line")
  "\"" '(avy-goto-char-2 :wk "Avy: 2 Chars"))

(toki-mark-def
  "b" '(mark-whole-buffer :wk "Buffer")
  "h" '(diff-hl-mark-hunk :wk "Hunk"))
