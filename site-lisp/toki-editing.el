;;; toki-editing.el --- Fine, structural editing -*- lexical-binding: t -*-

;; Copyright (C) 2020 Hao Wang
;; License: GPL v3, or (at your option) any later version

;;; Commentary:

;; This package offers:

;; - Finer editing commands (especially word related ones)
;; - Structural editing based on Emacs built-in mechanisms.  These are various
;;   deletion commands.  For auto-balancing parenthesis, use the `elec-pair'
;;   package.

;;; Code:

;; To see the outline of this file, run `outline-minor-mode', then
;; `outline-hide-body'.  Another way is to run `occur' with the query:
;; ^;;;;* \|^(

;;;; Libraries

(toki-declare-ext-pkg puni)

(require 'cl-lib)
(require 'puni)

;;;; User options

(defvar toki-smooth-scroll-step-length 2
  "How many lines to skip in a step when smooth scrolling.
See also `toki-smooth-scroll-interval'.")

(defvar toki-smooth-scroll-interval 0.016
  "Interval between steps when smooth scrolling.
See also `toki-smooth-scroll-step-length'.")

;;;;; Errors

(defun toki/error-if-before-point (bound)
  "Error if BOUND is non-nil and is before point."
  (when (and bound (< bound (point)))
    (error "BOUND is before point")))

(defun toki/error-if-after-point (bound)
  "Error if BOUND is non-nil and is after point."
  (when (and bound (> bound (point)))
    (error "BOUND is after point")))

;;;;; Movements: syntax

(defun toki/forward-block ()
  "Go forward a block.
Return the point if success.

A block is a continuous region with the same syntax, which
contains no more than 1 word.  See the implementation for
details."
  (unless (eobp)
    ;; A word may actually end at a position where the syntax on both sides are
    ;; "word", e.g., when subword-mode is enabled.
    (let ((word-end (save-excursion (when (forward-word) (point)))))
      (puni--forward-same-syntax word-end))))

(defun toki/backward-block ()
  "Backward version of `toki/forward-block'."
  (unless (bobp)
    (let ((word-beg (save-excursion (when (forward-word -1) (point)))))
      (puni--backward-same-syntax word-beg))))

;;;;; Movements: scroll

(defun toki/scroll-down-or-prev-line (&optional arg)
  "Scroll down ARG lines, or go to previous line if it can't be done.
Scroll 1 line if ARG is nil."
  (let ((arg (or arg 1)))
    (condition-case nil
        (scroll-down arg)
      (beginning-of-buffer
       (forward-line (- arg))))))

;;;;; Errors

(defun toki/bob-error ()
  "Signal an error if point is at the beginning of buffer."
  (when (bobp)
    (signal 'beginning-of-buffer nil)))

(defun toki/eob-error ()
  "Signal an error if point is and the end of buffer."
  (when (eobp)
    (signal 'end-of-buffer nil)))

;;;; Commands

;;;;; Word

;;;###autoload
(defun toki-forward-word ()
  "A finer version of `forward-word'.
If there's *only one* non-word char between point and next word,
move after it.  Then jump forward by a block.  A block is a
continuous region with the same syntax, like a word, a bunch of
whitespaces/punctuations, etc.

This doesn't fly over most punctuations, while `forward-word'
does."
  (interactive)
  (toki/eob-error)
  (when (eq (puni--syntax-char-after (1+ (point))) ?w)
    (forward-char))
  (toki/forward-block))

;;;###autoload
(defun toki-forward-delete-word ()
  "Delete word forward while keeping expressions balanced."
  (interactive)
  (if (use-region-p)
      (puni-delete-active-region)
    (puni-soft-delete-by-move #'toki-forward-word nil nil nil 'jump-and-reverse-delete)))

;;;###autoload
(defun toki-backward-word ()
  "Backward version of `toki-forward-word'."
  (interactive)
  (toki/bob-error)
  (when (eq (puni--syntax-char-after (- (point) 2)) ?w)
    (backward-char))
  (toki/backward-block))

;;;###autoload
(defun toki-backward-delete-word ()
  "Delete word backward while keeping expressions balanced."
  (interactive)
  (if (use-region-p)
      (puni-delete-active-region)
    (puni-soft-delete-by-move #'toki-backward-word nil nil nil 'jump-and-reverse-delete)))

;;;;;; Line

;;;###autoload
(defun toki-beginning-of-line ()
  "A smarter version of `beginning-of-line'.
Jump to the beginning of current line, or if the point is already
there, move to the first non-whitespace character in current
line."
  (interactive)
  (if (and (bolp) (not (puni--line-empty-p)))
      (skip-chars-forward "[:blank:]")
    (beginning-of-line)))

;;;###autoload
(defun toki-end-of-line ()
  "A smarter version of `end-of-line'.
Jump to the end of current line, or if the point is already
there, move to the last non-whitespace character in current
line."
  (interactive)
  (if (and (eolp) (not (puni--line-empty-p)))
      (skip-chars-backward "[:blank:]")
    (end-of-line)))

;;;;;; Sentence

;;;###autoload
(defun toki-forward-subsentence ()
  "Jump forward by a subsentence.
It jumps to the next period, comma, exclamation mark, question
mark, colon, semicolon, or ellipsis."
  (interactive)
  (re-search-forward "\\.\\|,\\|\\!\\|\\?\\|:\\|;\\|…\
\\|。\\|，\\|、\\|！\\|？\\|：\\|；\\|⋯"))

;;;###autoload
(defun toki-backward-subsentence ()
  "Jump backward by a subsentence.
It jumps to the previous period, comma, exclamation mark, question
mark, colon, semicolon, or ellipsis."
  (interactive)
  (re-search-backward "\\.\\|,\\|\\!\\|\\?\\|:\\|;\\|…\
\\|。\\|，\\|、\\|！\\|？\\|：\\|；\\|⋯"))

;;;;;; Scroll

;;;###autoload
(defun toki-smooth-scroll-half-page-down ()
  "Smoothly scroll down by half a page."
  (interactive)
  (when (eq (line-number-at-pos) 1)
    (signal 'beginning-of-buffer nil))
  (let* ((lines (max (round (/ (window-body-height) 2.3)) 10))
         (times (max 1 (/ lines toki-smooth-scroll-step-length)))
         (interval (* (- times 0.5) toki-smooth-scroll-interval))
         (timer
          (run-with-timer
           nil toki-smooth-scroll-interval
           (lambda ()
             (toki/scroll-down-or-prev-line toki-smooth-scroll-step-length)))))
    (run-with-timer interval nil (lambda () (cancel-timer timer)))))

;;;###autoload
(defun toki-smooth-scroll-half-page-up ()
  "Smoothly scroll down by half a page."
  (interactive)
  (let* ((lines (max (round (/ (window-body-height) 2.3)) 10))
         (times (max 1 (/ lines toki-smooth-scroll-step-length)))
         (interval (* (- times 0.5) toki-smooth-scroll-interval))
         (timer
          (run-with-timer
           nil toki-smooth-scroll-interval
           (lambda ()
             (scroll-up toki-smooth-scroll-step-length)))))
    (run-with-timer interval nil (lambda () (cancel-timer timer)))))

;;;; Misc commands

;;;###autoload
(defun toki-shrink-whitespace ()
  "Intelligently shrink whitespaces around point.

When in the middle of a line, delete whitespaces around point, or
add a space if there are no whitespaces.

When in an empty line, leave only one blank line, or delete it if
it's the only one.

When at the beginning/end of a line, first delete whitespaces
around it, then delete empty lines before/after it.  Finally join
with the line before/after it."
  (interactive)
  (let* ((beg
          (save-excursion (while (member (char-before) '(?\s ?\t))
                            (backward-char))
                          (point)))
         ;; el = empty lines
         (beg-with-el
          (save-excursion (while (member (char-before) '(?\n ?\s ?\t))
                            (backward-char))
                          (point)))
         (end
          (save-excursion (while (member (char-after) '(?\s ?\t))
                            (forward-char))
                          (point)))
         (end-with-el
          (save-excursion (while (member (char-after) '(?\n ?\s ?\t))
                            (forward-char))
                          (point))))
    (cond
     ((puni--line-empty-p)
      (delete-blank-lines))
     ((eq beg end)
      (cond
       ((eq beg-with-el end-with-el)
        (insert-char ?\s))
       (t
        (cond
         ((eq (point) beg-with-el)
          (save-excursion
            (forward-char)
            (if (puni--line-empty-p)
                (delete-blank-lines)
              (delete-char -1))))
         (t
          (save-excursion
            (backward-char)
            (if (puni--line-empty-p)
                (delete-blank-lines)
              (delete-char 1))))))))
     (t
      (delete-region beg end)))))

;;;###autoload
(defun toki-tabify ()
  "Tabify region, or the whole buffer when no active region."
  (interactive)
  (if (use-region-p)
      (tabify (region-beginning) (region-end))
    (tabify (point-min) (point-max))))

;;;###autoload
(defun toki-untabify ()
  "Untabify region, or the whole buffer when no active region."
  (interactive)
  (if (use-region-p)
      (untabify (region-beginning) (region-end))
    (untabify (point-min) (point-max))))

(provide 'toki-editing)

;;; toki-editing.el ends here
