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

(require 'cl-lib)

;;;; User options

(defvar toki-smooth-scroll-step-length 2
  "How many lines to skip in a step when smooth scrolling.
See also `toki-smooth-scroll-interval'.")

(defvar toki-smooth-scroll-interval 0.016
  "Interval between steps when smooth scrolling.
See also `toki-smooth-scroll-step-length'.")

;;;; Internals

;;;;; Probes

(defun toki/line-empty-p ()
  "Return t if current line is empty."
  (when (string-match "^[[:blank:]]*$"
                      (buffer-substring (line-beginning-position)
                                        (line-end-position)))
    t))

(defun toki/in-string-p ()
  "Return t if point is in a string.
This returns nil if point is before the opening quote, or after
the end quote."
  (eq (syntax-ppss-context (syntax-ppss)) 'string))

(defun toki/in-comment-p ()
  "Return t if point is in a comment.
This returns nil if point is before/in the opening delimiter, or
after/in the end delimiter."
  (eq (syntax-ppss-context (syntax-ppss)) 'comment))

(defun toki/in-interval-p (num interval)
  "Return t if NUM is inside INTERVAL.
INTERVAL is a cons pair of numbers, with the cdr larger than the
car.  NUM is not inside INTERVAL if it's the beginning or end of
INTERVAL."
  (< (car interval) num (cdr interval)))

;;;;; Syntax

;; Ref: https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Class-Table.html
;; and https://debbugs.gnu.org/cgi/bugreport.cgi?bug=37452

(defun toki/syntax-class-to-char (syntax-class)
  "Return the designator char of SYNTAX-CLASS."
  (aref " .w_()'\"$\\/<>@!|" syntax-class))

(defun toki/syntax-char-after (&optional point)
  "Return the syntax code after POINT, described by a char.
When POINT is nil, return the syntax code after the current
point."
  (let ((point (or point (point))))
    (toki/syntax-class-to-char (syntax-class (syntax-after point)))))

;;;;; Movements: syntax

(defun toki/forward-same-syntax (&optional arg)
  "Move point past all characters with the same syntax class.
If ARG is positive, do it ARG times.  If ARG is negative, move
backwards ARG times.  Return nil if it fails in the middle,
otherwise return t.

This is more robust than `forward-same-syntax' because it takes
`syntax-table' text properties into account.  See the docstring
of `char-syntax'."
  (let ((arg (or arg 1))
        (fail-flag nil))
    (while (< arg 0)
      (if (bobp)
          (setq arg 0
                fail-flag t)
        (skip-syntax-backward
         (char-to-string (toki/syntax-char-after (1- (point))))))
      (setq arg (1+ arg)))
    (while (> arg 0)
      (if (eobp)
          (setq arg 0
                fail-flag t)
        (skip-syntax-forward
         (char-to-string (toki/syntax-char-after (point)))))
      (setq arg (1- arg)))
    (unless fail-flag t)))

(defun toki/forward-block ()
  "Go forward a block.
Return the point if success.

A block is a continuous region with the same syntax, which
contains no more than 1 word.  See the implementation for
details."
  (unless (eobp)
    (let ((syntax-pos (save-excursion
                        (toki/forward-same-syntax)
                        (point)))
          ;; A word may actually end at a position where the syntax on both
          ;; sides are "word", e.g., when subword-mode is enabled.  In this
          ;; situation we go forward by a word.
          (word-pos (save-excursion
                      (forward-word)
                      (point))))
      (goto-char (min syntax-pos word-pos)))))

(defun toki/backward-block ()
  "Go backward a block.
Return the point if success.

A block is a continuous region with the same syntax, which
contains no more than 1 word.  See the implementation for
details."
  (unless (bobp)
    (let ((syntax-pos (save-excursion
                        (toki/forward-same-syntax -1)
                        (point)))
          ;; A word may actually start at a position where the syntax on both
          ;; sides are "word", e.g., when subword-mode is enabled.  In this
          ;; situation we go back by a word.
          (word-pos (save-excursion
                      (forward-word -1)
                      (point))))
      (goto-char (max syntax-pos word-pos)))))

;;;;; Movements: scroll

(defun toki/scroll-down-or-prev-line (&optional arg)
  "Scroll down ARG lines, or go to previous line if it can't be done.
Scroll 1 line if ARG is nil."
  (let ((arg (or arg 1)))
    (condition-case nil
        (scroll-down arg)
      (beginning-of-buffer
       (forward-line (- arg))))))

;;;;; Movements: blank

(defun toki/forward-blanks (&optional bound)
  "Jump forward whitespaces and newlines.
Return t if success.  When BOUND is non-nil, don't go further
than BOUND."
  (when (looking-at (rx (+ (or space "\n"))))
    (goto-char (match-end 0))
    (when (and bound (> (point) bound))
      (goto-char bound))
    t))

(defun toki/backward-blanks (&optional bound)
  "Jump backward whitespaces and newlines.
Return t if success.  When BOUND is non-nil, don't go further
than BOUND."
  (when (looking-back (rx (+ (or space "\n"))) bound 'greedy)
    (goto-char (match-beginning 0))))

;;;;; Movements: comment

(defun toki/forward-comment-block ()
  "Jump forward a whole comment block.
Return the point if success."
  (let ((beg (point))
        end)
    (save-excursion
      (when (forward-comment 1)
        (toki/backward-blanks beg)
        (setq end (point))))
    (when end (goto-char end))))

(defun toki/backward-comment-block ()
  "Jump backward a whole comment block.
Return the point if success."
  (let ((end (point))
        beg)
    (save-excursion
      ;; Sometimes it looks like the point is at the end of comment, but
      ;; actually not, e.g., there are spaces after point, or a newline follows
      ;; the point, which is the end delimiter of comment itself.  In these
      ;; situations, we jump over blanks to put the point after the comment
      ;; block.
      (toki/forward-blanks)
      (when (forward-comment -1)
        (toki/forward-blanks end)
        (setq beg (point))))
    (when beg (goto-char beg))))

;;;;; Movements: sexp

(defun toki/internal-forward-sexp ()
  "Move forward a sexp, return t if success, otherwise return nil.
This is a wrapper of `forward-sexp'."
  (condition-case nil
      (progn (forward-sexp) t)
    (scan-error nil)))

(defun toki/internal-backward-sexp ()
  "Move backward a sexp, return t if success, otherwise return nil.
This is a wrapper of (forward-sexp -1)."
  (condition-case nil
      (progn (forward-sexp -1) t)
    (scan-error nil)))

(defun toki/forward-sexp-in-construct (probe construct)
  "Move forward a sexp in certain construct.
PROBE is a function that should return non-nil when the point is
in the construct, and nil when it's not.

Return the point after move.  When we can't move forward, return
nil.

When the point is not in the construct in the first place, throw
and error \"Not in a CONSTRUCT\"."
  (when (not (funcall probe))
    (error (format "Not in a %s" construct)))
  (let ((end (and (save-excursion (toki/internal-forward-sexp))
                  (point)))
        pos)
    (when end
      (save-excursion
        (while (and (toki/forward-same-syntax)
                    (funcall probe)
                    (setq pos (point))
                    (< (point) end))))
      (when (and pos (>= pos end))
        (goto-char pos)))))

(defun toki/backward-sexp-in-construct (probe construct)
  "Move backward a sexp in certain construct.
PROBE is a function that should return non-nil when the point is
in the construct, and nil when it's not.

Return the point after move.  When we can't move backward, return
nil.

When the point is not in the construct in the first place, throw
and error \"Not in a CONSTRUCT\"."
  (when (not (funcall probe))
    (error (format "Not in a %s" construct)))
  (let ((beg (and (save-excursion (toki/internal-backward-sexp))
                  (point)))
        pos)
    (when beg
      (save-excursion
        (while (and (toki/forward-same-syntax -1)
                    (funcall probe)
                    (setq pos (point))
                    (> (point) beg))))
      (when (and pos (<= pos beg))
        (goto-char pos)))))

(defun toki/forward-sexp-in-string ()
  "Move forward a sexp in when point is in string.
The default `(forward-sexp)' thinks the end quote of a string and
a beginning quote of the next string wraps a sexp.  This fixed
that.

Return the point after move.  When we can't move forward (i.e.,
hit the ending quote), return nil."
  (toki/forward-sexp-in-construct #'toki/in-string-p "string"))

(defun toki/backward-sexp-in-string ()
  "Move backward a sexp in when point is in string.
The default `(backward-sexp)' thinks the end quote of the
previous string and a beginning quote of this string wraps a
sexp.  This fixed that.

Return the point after move.  When we can't move backward (i.e.,
hit the beginning quote), return nil."
  (toki/backward-sexp-in-construct #'toki/in-string-p "string"))

(defun toki/forward-sexp-in-comment ()
  "Move forward a sexp in when point is in a comment.
The default `(forward-sexp)' goes over the end quote of the
comment block.  This fixed that.

Return the point after move.  When we can't move forward (i.e.,
hit the ending quote), return nil.

Notice that a point inside the (multichar) quote is not
considered as in the comment."
  (toki/forward-sexp-in-construct #'toki/in-comment-p "comment"))

(defun toki/backward-sexp-in-comment ()
  "Move backward a sexp in when point is in a comment.
The default `(backward-sexp)' goes over the beginning quote of
the comment block.  This fixes that.

Return the point after move.  When we can't move backward (i.e.,
hit the beginning quote), return nil.

Notice that a point inside the (multichar) quote is not
considered as in the comment."
  (toki/backward-sexp-in-construct #'toki/in-comment-p "comment"))

(defun toki/forward-sexp ()
  "Jump forward a sexp, including a whole comment block.
Return t if success."
  (toki/forward-blanks)
  (and (or (toki/forward-comment-block)
           (cond
            ;; `toki/in-comment-p' doesn't consert a point inside the
            ;; (multichar) comment quote as in the comment, but this is fine as
            ;; when a user is deleting things with the point at there, they
            ;; probably want to break the balanced comment quotes.
            ((toki/in-comment-p) (toki/forward-sexp-in-comment))
            ((toki/in-string-p) (toki/forward-sexp-in-string))
            (t (toki/internal-forward-sexp))))
       t))

(defun toki/backward-sexp ()
  "Jump backward a sexp or balanced comment block.
Return t if success."
  (toki/backward-blanks)
  (and (or (toki/backward-comment-block)
           (cond
            ;; `toki/in-comment-p' doesn't consert a point inside the
            ;; (multichar) comment quote as in the comment, but this is fine
            ;; as when a user is deleting things with the point at there,
            ;; they probably want to break the balanced comment quotes.
            ((toki/in-comment-p) (toki/backward-sexp-in-comment))
            ((toki/in-string-p) (toki/backward-sexp-in-string))
            (t (toki/internal-backward-sexp))))
       t))

;;;;; Movements: symbol + blank

(defun toki/forward-symbol-and-blank (&optional bound)
  "Go forward symbols and blanks.
Return t if success.  If BOUND is non-nil, don't go further than
BOUND."
  (when (looking-at (rx (+ (or (syntax symbol) (syntax word)
                               space "\n"))))
    (goto-char (match-end 0))
    (when (and bound (> (point) bound))
      (goto-char bound))
    t))

;;;;; Balance test

(defun toki/region-balance-p (beg end)
  "Return non-nil when the region from BEG to END is balanced.
This is tolerant to unbalanced word delimeters like \"if..end
if\", \"begin..end\", \"def\", as it's common to delete part of
such a block and then rewrite it."
  (save-excursion
    (goto-char beg)
    (cl-block nil
      (when (eq beg end) (cl-return t))
      (while (< (point) end)
        (if (or (toki/forward-symbol-and-blank end)
                (toki/forward-sexp))
            (when (or (eq (point) end)
                      (and (< (point) end)
                           (progn (toki/forward-blanks end)
                                  (>= (point) end))))
              (cl-return t))
          (cl-return nil))))))

;;;;; Deletion

(defun toki/maybe-delete-active-region ()
  "When there's an active region, delete it.
If deleting it will cause unbalanced state, don't delete it, and
throw an error."
  (when (use-region-p)
    (let ((beg (region-beginning))
          (end (region-end)))
      (if (toki/region-balance-p beg end)
          (progn (delete-region beg end) t)
        (toki/unbalance-error "Can't delete region")))))

(defun toki/kill-region (beg end &optional msg)
  "Delete the region between BEG and END.
When deleting it causes unbalanced state, don't delete it.  In
this situation, if MSG is non-nil, throw an error using MSG."
  (if (toki/region-balance-p beg end)
      (progn (kill-region beg end) t)
    (when msg (toki/unbalance-error msg))))

(defun toki/delete-region (beg end &optional msg)
  "Delete the region between BEG and END.
When deleting it causes unbalanced state, don't delete it.  In
this situation, if MSG is non-nil, throw an error using MSG."
  (if (toki/region-balance-p beg end)
      (progn (delete-region beg end) t)
    (when msg (toki/unbalance-error msg))))

(defun toki/delete-by-move (func &optional msg delete-sexp kill)
  "Delete between point and the position after calling FUNC.
If deleting will cause unbalanced state, then:

- If MSG is non-nil, throw an error using MSG.
- If MSG is nil, DELETE-SEXP is non-nil, delete (at least one)
  sexps until the position after calling FUNC.
- If both MSG and DELETE-SEXP is nil, do nothing and return nil.

If KILL is non-nil, save the deleted text to kill ring."
  (or (toki/maybe-delete-active-region)
      (let ((pt (point))
            (target (save-excursion (funcall func) (point)))
            (soft-delete (if kill #'toki/kill-region #'toki/delete-region))
            (hard-delete (if kill #'kill-region #' delete-region)))
        (unless (eq pt target)
          (or (funcall soft-delete (min pt target) (max pt target) msg)
              (if delete-sexp
                  (funcall
                   hard-delete
                   pt
                   (if (< target pt)
                       (save-excursion (while (and (> (point) target)
                                                   (toki/backward-sexp)))
                                       (point))
                     (save-excursion (while (and (< (point) target)
                                                 (toki/forward-sexp)))
                                     (point))))
                (funcall func)))))))

;;;;; Errors

(defun toki/bob-error ()
  "Signal an error if point is at the beginning of buffer."
  (when (bobp)
    (signal 'beginning-of-buffer nil)))

(defun toki/eob-error ()
  "Signal an error if point is and the end of buffer."
  (when (eobp)
    (signal 'end-of-buffer nil)))

(defun toki/unbalance-error (msg)
  "Signal a error using MSG.
This should be used when certain operation causes unbalanced
state."
  (user-error "%s: will cause unbalanced state" msg))

;;;; Commands

;;;;; Char

;;;###autoload
(defun toki-backward-delete-char ()
  "Delete char backward while keeping expressions balanced."
  (interactive)
  (toki/delete-by-move #'backward-char))

;;;###autoload
(defun toki-forward-delete-char ()
  "Delete char forward while keeping expressions balanced."
  (interactive)
  (toki/delete-by-move #'forward-char))

;;;;; Word

;;;###autoload
(defun toki-forward-word ()
  "A finer version of `forward-word'.
If there's *only one* space, tab, '-' or '_' between point and
next word, move after it.  Then jump forward by a block.  A block
is a continuous region with the same syntax, like a word, a bunch
of whitespaces/punctuations, etc.

This doesn't fly over most punctuations, while `forward-word'
does."
  (interactive)
  (toki/eob-error)
  (unless (> (point) (- (point-max) 2))
    (when (and (member (char-after) '(?\s ?\t ?- ?_))
               (eq (toki/syntax-char-after (1+ (point))) ?w))
      (forward-char)))
  (toki/forward-block))

;;;###autoload
(defun toki-forward-delete-word ()
  "Delete word forward while keeping expressions balanced."
  (interactive)
  (toki/delete-by-move #'toki-forward-word nil 'delete-sexp))

;;;###autoload
(defun toki-backward-word ()
  "A finer version of `backward-word'.
If there's *only one* space, tab, '-' or '_' between point and
previous word, move before it.  Then jump back by a block.  A
block is a continuous region with the same syntax, like a word, a
bunch of whitespaces/punctuations, etc.

This doesn't fly over most punctuations, while `backward-word'
does."
  (interactive)
  (toki/bob-error)
  (unless (< (point) (+ (point-min) 2))
    (when (and (member (char-before) '(?\s ?\t ?- ?_))
               (eq (toki/syntax-char-after (- (point) 2)) ?w))
      (backward-char)))
  (toki/backward-block))

;;;###autoload
(defun toki-backward-delete-word ()
  "Delete word backward while keeping expressions balanced."
  (interactive)
  (toki/delete-by-move #'toki-backward-word nil 'delete-sexp))

;;;;;; Line

;;;###autoload
(defun toki-kill-line ()
  "Kill a line forward while keeping expressions balanced."
  (interactive)
  (toki/delete-by-move (lambda ()
                         (if (eolp) (forward-line) (end-of-line)))
                       nil 'delete-sexp 'kill))

;;;###autoload
(defun toki-backward-kill-line ()
  "Kill a line backward while keeping expressions balanced."
  (interactive)
  (toki/delete-by-move (lambda ()
                         (if (bolp) (forward-line -1) (beginning-of-line)))
                       nil 'delete-sexp 'kill))

;;;###autoload
(defun toki-beginning-of-line ()
  "A smarter version of `beginning-of-line'.
Jump to the beginning of current line, or if the point is already
there, move to the first non-whitespace character in current
line."
  (interactive)
  (if (and (bolp) (not (toki/line-empty-p)))
      (skip-chars-forward "[:blank:]")
    (beginning-of-line)))

;;;###autoload
(defun toki-end-of-line ()
  "A smarter version of `end-of-line'.
Jump to the end of current line, or if the point is already
there, move to the last non-whitespace character in current
line."
  (interactive)
  (if (and (eolp) (not (toki/line-empty-p)))
      (skip-chars-backward "[:blank:]")
    (end-of-line)))

;;;;;; Sentence

;;;###autoload
(defun toki-forward-punct ()
  "Jump to next punctuation that's not inside a symbol."
  (interactive)
  ;; For some reason \"：\" is not considered as a punctuation.
  (while (and (re-search-forward "[[:punct:]]\\|：")
              (toki/in-interval-p (point)
                                  (bounds-of-thing-at-point 'symbol)))))

;;;###autoload
(defun toki-backward-punct ()
  "Jump to previous punctuation that's not inside a symbol."
  (interactive)
  (while (and (re-search-backward "[[:punct:]]\\|：")
              (toki/in-interval-p (point)
                                  (bounds-of-thing-at-point 'symbol)))))

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

;;;;;; Sexp

;;;###autoload
(defun toki-forward-list-delimiter ()
  "Go to the next list delimiter."
  (interactive)
  (or (condition-case nil
          (progn (down-list) t)
        (scan-error nil))
      (condition-case nil
          (progn (up-list) t)
        (scan-error nil))))

;;;###autoload
(defun toki-backward-list-delimiter ()
  "Go to the previous list delimiter."
  (interactive)
  (or (condition-case nil
          (progn (down-list -1) t)
        (scan-error nil))
      (condition-case nil
          (progn (up-list -1) t)
        (scan-error nil))))

;;;;;; Scroll

;;;###autoload
(defun toki-smooth-scroll-half-page-down ()
  "Smoothly scroll down by half a page."
  (interactive)
  (when (eq (line-number-at-pos) 1)
    (signal 'beginning-of-buffer nil))
  (let* ((lines (max (round (/ (window-body-height) 2.3)) 10))
         (times (max 1 (/ lines toki-smooth-scroll-step-length)))
         (timer
          (run-with-timer
           nil toki-smooth-scroll-interval
           (lambda ()
             (toki/scroll-down-or-prev-line toki-smooth-scroll-step-length)))))
    (run-with-timer (* (- times 0.5) toki-smooth-scroll-interval) nil
                    (lambda () (cancel-timer timer)))))

;;;###autoload
(defun toki-smooth-scroll-half-page-up ()
  "Smoothly scroll down by half a page."
  (interactive)
  (let* ((lines (max (round (/ (window-body-height) 2.3)) 10))
         (times (max 1 (/ lines toki-smooth-scroll-step-length)))
         (timer
          (run-with-timer
           nil toki-smooth-scroll-interval
           (lambda ()
             (scroll-up toki-smooth-scroll-step-length)))))
    (run-with-timer (* (- times 0.5) toki-smooth-scroll-interval) nil
                    (lambda () (cancel-timer timer)))))

;;;; Misc commands

;;;###autoload
(defun toki-force-delete ()
  "Force delete backward char, or the active region.
Can be used to fight with undesired behavior of structural
editing."
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (when (not (bobp))
      (delete-region (1- (point)) (point)))))

;;;###autoload
(defun toki-kill-region ()
  "Kill active region.
If this will cause unbalanced state, ask the user to confirm."
  (interactive)
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (when (or (toki/region-balance-p beg end)
                  (y-or-n-p "Kill the region will cause unbalanced state.  \
Continue? "))
          (setq this-command 'kill-region)
          (kill-region beg end)))
    (user-error "No active region")))

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
     ((toki/line-empty-p)
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
            (if (toki/line-empty-p)
                (delete-blank-lines)
              (delete-char -1))))
         (t
          (save-excursion
            (backward-char)
            (if (toki/line-empty-p)
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

(defvar toki-sedit-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "DEL") 'toki-backward-delete-char)
    (define-key map (kbd "C-d") 'toki-forward-delete-char)
    (define-key map (kbd "M-d") 'toki-forward-delete-word)
    (define-key map (kbd "M-DEL") 'toki-backward-delete-word)
    (define-key map (kbd "C-k") 'toki-kill-line)
    (define-key map (kbd "C-u") 'toki-backward-kill-line)
    (define-key map (kbd "C-h") 'toki-force-delete)
    map)
  "Keymap used for `toki-structural-editing-mode'.")

;;;###autoload
(define-minor-mode toki-sedit-mode
  "Curated keybinds for structural deleting commands."
  :keymap toki-sedit-mode-map)

;;;###autoload
(define-globalized-minor-mode toki-global-sedit-mode
  toki-sedit-mode
  (lambda () (toki-sedit-mode 1)))

(provide 'toki-editing)

;;; toki-editing.el ends here
