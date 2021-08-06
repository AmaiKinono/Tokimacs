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

;;;;; Errors

(defun toki/error-if-before-point (bound)
  "Error if BOUND is non-nil and is before point."
  (when (and bound (< bound (point)))
    (error "BOUND is before point")))

(defun toki/error-if-after-point (bound)
  "Error if BOUND is non-nil and is after point."
  (when (and bound (> bound (point)))
    (error "BOUND is after point")))

;;;;; Syntax

;; Ref: https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Class-Table.html
;; and https://debbugs.gnu.org/cgi/bugreport.cgi?bug=37452

(defun toki/syntax-class-to-char (syntax-class)
  "Return the designator char of SYNTAX-CLASS."
  (aref " .w_()'\"$\\/<>@!|" syntax-class))

(defun toki/syntax-char-after (&optional point)
  "Return the syntax code after POINT, described by a char.
When POINT is nil, return the syntax code after the current
point.  When POINT doesn't exist, or there's no char after POINT,
return nil.

For the meaning of the returned char, see `modify-syntax-entry'."
  (let ((point (or point (point))))
    (unless (or (< point (point-min))
                (>= point (point-max)))
      (toki/syntax-class-to-char (syntax-class (syntax-after point))))))

;;;;; Movements: syntax

(defun toki/forward-syntax (syntax &optional bound)
  "Go forward across chars in specified syntax classes.
SYNTAX is a string of syntax code chars.  When BOUND is non-nil,
stop before BOUND.

This is the same as `skip-syntax-forward', except that:

- It signals an error is BOUND is before point at the first place.
- If it fails, return 0.
- If sucess, return the point after move."
  (toki/error-if-before-point bound)
  (pcase (skip-syntax-forward syntax bound)
    (0 nil)
    (_ (point))))

(defun toki/backward-syntax (syntax &optional bound)
  "Backward version of `toki/forward-syntax'."
  (toki/error-if-after-point bound)
  (pcase (skip-syntax-backward syntax bound)
    (0 nil)
    (_ (point))))

(defun toki/forward-same-syntax (&optional bound)
  "Move point past all characters with the same syntax class.
This is more robust than `forward-same-syntax' because it takes
`syntax-table' text properties into account.  See the docstring
of `char-syntax'."
  (toki/error-if-before-point bound)
  (unless (eobp)
    (skip-syntax-forward
     (char-to-string (toki/syntax-char-after (point))))
    (if (and bound (> (point) bound))
        (goto-char bound)
      (point))))

(defun toki/backward-same-syntax (&optional bound)
  "Backward version of `toki/forward-same-syntax'."
  (toki/error-if-after-point bound)
  (unless (bobp)
    (skip-syntax-backward
     (char-to-string (toki/syntax-char-after (1- (point)))))
    (if (and bound (< (point) bound))
        (goto-char bound)
      (point))))

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
      (toki/forward-same-syntax word-end))))

(defun toki/backward-block ()
  "Backward version of `toki/forward-block'."
  (unless (bobp)
    (let ((word-beg (save-excursion (when (forward-word -1) (point)))))
      (toki/backward-same-syntax word-beg))))

;;;;; Movements: atom

(defun toki/forward-atom (&optional bound)
  "Move forward an atom if there's one in front.
An atom is a symbol, plus the escape chars, char quotes, and
expression prefixes in/around it.

If BOUND is non-nil, stop before BOUND."
  ;; It may be a good idea to treat a series of punctuations as an atom (think
  ;; of operators).  Unfortunately there are major modes where "<>" should be
  ;; delimiters, but are given the punctuation syntax.
  (toki/error-if-before-point bound)
  (unless (eobp)
    (let ((from (point)))
      (toki/forward-syntax "'" bound)
      (toki/forward-syntax "_w\\/" bound)
      (let ((to (point)))
        (unless (eq from to)
          to)))))

(defun toki/backward-atom (&optional bound)
  "Backward version of `toki/forward-atom'."
  (toki/error-if-after-point bound)
  (unless (bobp)
    (let ((from (point)))
      (toki/backward-syntax "_w\\/" bound)
      (toki/backward-syntax "'" bound)
      (let ((to (point)))
        (unless (eq from to)
          to)))))

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

(defun toki/forward-consecutive-single-line-comments ()
  "Jump forward a series of single-line comments.
Return the point if success."
  (let ((beg (point))
        end)
    (save-excursion
      (while (and (forward-comment 1)
                  (progn (toki/backward-blanks) (eolp))))
      (unless (eq beg (point))
        (toki/backward-blanks beg)
        (setq end (point))))
    (when end (goto-char end))))

(defun toki/backward-consecutive-single-line-comments ()
  "Jump backward a series of single-line comments.
Return the point if success."
  (let ((end (point))
        beg)
    (save-excursion
      (while (and (progn (toki/forward-blanks)
                         (forward-comment -1))
                  (progn (toki/backward-blanks) (eolp))))
      (unless (eq end (point))
        (toki/forward-blanks end)
        (setq beg (point))))
    (when beg (goto-char beg))))

;;;;; Movements: sexp

(defun toki/primitive-forward-sexp (&optional n)
  "A wrapper around `forward-sexp'.
Move forward N sexp, return the point if success, otherwise
return nil.

This wrapper is here since `forward-sexp' can fail differently in
different major modes, e.g., the built-in one for Lisp will throw
a `scan-error', the one from `nxml-mode' throws a plain error,
while the one from `web-mode' just does nothing and returns nil."
  (condition-case _
      (let ((beg (point))
            (end (progn (forward-sexp n) (point))))
        (unless (eq beg end) end))
    (error nil)))

(defun toki/strict-primitive-forward-sexp ()
  "Move forward a sexp, return the point if success, otherwise return nil.
If there's a balanced sexp in front, but jumping over it will
move us to a different depth in the whole syntax tree, this won't
go over it (unlike the built-in `forward-sexp').

This assumes there's no whitespaces after the point, and ensures
it.

Notice this doesn't work well in strings, as the built-in
`forward-sexp' thinks the closing quote of this string, and the
opening quote of the next one, forms a string.  It also doesn't
work well for balanced comment blocks.  So we'll build on top of
this function until we have `toki/strict-forward-sexp', which
works on these situations."
  (when (eq (toki/syntax-char-after) ?\s)
    (error "Whitespaces after point"))
  (when-let* ((beg (point))
              ;; This incidentally checks if the point is at the end of buffer.
              (end (save-excursion (toki/primitive-forward-sexp)))
              (beg-of-maybe-another-sexp
               (save-excursion (goto-char end)
                               (toki/primitive-forward-sexp -1))))
    (let (pt)
      (cond
       ;; Logically this shouldn't happen.  The only situation where I've seen
       ;; this is (in `text-mode'):
       ;;
       ;;     foo|.
       ;;     ## bar baz
       ;;
       ;; Call `forward-sexp', it becomes:
       ;;
       ;;     foo.
       ;;     ## bar| baz
       ;;
       ;; Call `backward-sexp', it becomes:
       ;;
       ;;     foo.
       ;;     ## |bar baz
       ;;
       ;; So this branch deals with this situation.  It's because the default
       ;; `forward-sexp' handles punctuations poorly.
       ((> beg-of-maybe-another-sexp beg)
        (setq end (save-excursion
                    (goto-char beg)
                    (toki/forward-syntax "."))))
       ;; This can happen in html (in `xml-mode'):
       ;;
       ;;     <p>text|</p>        call (forward-sexp)
       ;;     <p>text</p>|        call (backward-sexp)
       ;;     |<p>text</p>
       ;;
       ;; It also happens when the original state is:
       ;;
       ;;     <p>text</|p>
       ;;
       ;; In this situation, we shouldn't jump as it changes the current depth.
       ;; But it may also be the case where the point is inside an atom:
       ;;
       ;;     some|-symbol        call (forward-sexp)
       ;;     some-symbol|        call (backward-sexp)
       ;;     |some-symbol
       ;;
       ;; So we need to check if this is the case.
       ((< beg-of-maybe-another-sexp beg)
        (cond
         ;; This means the point is in the middle of an atom, and the atom
         ;; itself is not a delimiter.  We can delete half of an atom safely.
         ((save-excursion
            (goto-char beg-of-maybe-another-sexp)
            (eq (toki/forward-atom end) end))
          nil)
         ;; This means the point is in the middle of an atom, and the initial
         ;; `forward-sexp' takes us to a position after the end of the atom.
         ;; If this happens, we could assume the atom itself is not the
         ;; delimiter, and the part after it is.
         ((save-excursion
            (goto-char beg)
            (and (setq pt (toki/forward-atom))
                 (< pt end)))
          (setq end pt))
         (t (setq end nil))))))
    (when end (goto-char end))))

(defun toki/strict-primitive-backward-sexp ()
  "Move backward a sexp, return the point if success, otherwise return nil.
It's the backward version of
`toki/strict-primitive-forward-sexp'."
  (when (eq (toki/syntax-char-after (1- (point))) ?\s)
    (error "Whitespaces before point"))
  (when-let* ((end (point))
              (beg (save-excursion (toki/primitive-forward-sexp -1)))
              (end-of-maybe-bigger-sexp
               (save-excursion (goto-char beg)
                               (toki/primitive-forward-sexp))))
    (let (pt)
      (cond
       ((< end-of-maybe-bigger-sexp end)
        (setq beg (save-excursion
                    (goto-char end)
                    (toki/backward-syntax "."))))
       ((> end-of-maybe-bigger-sexp end)
        (cond
         ((save-excursion
            (goto-char beg)
            (eq (toki/forward-atom end-of-maybe-bigger-sexp)
                end-of-maybe-bigger-sexp))
          nil)
         ((save-excursion
            (goto-char end)
            (and (setq pt (toki/backward-atom))
                 (> pt beg)))
          (setq beg pt))
         (t (setq beg nil))))))
    (when beg (goto-char beg))))

(defun toki/strict-primitive-forward-sexp-in-construct (probe construct)
  "Move strict forward a sexp in certain construct.
PROBE is a function that should return non-nil when the point is
in the construct, and nil when it's not.

Return the point after move.  When we can't move forward, return
nil.

When the point is not in the construct in the first place, throw
and error \"Not in a CONSTRUCT\"."
  (when (not (funcall probe))
    (error (format "Not in a %s" construct)))
  (let ((end (save-excursion (toki/strict-primitive-forward-sexp)))
        pos)
    (when end
      (save-excursion
        (while (and (toki/forward-same-syntax)
                    (funcall probe)
                    (setq pos (point))
                    (< (point) end))))
      (when (and pos (>= pos end))
        (goto-char pos)))))

(defun toki/strict-primitive-backward-sexp-in-construct (probe construct)
  "Move strict backward a sexp in certain construct.
PROBE is a function that should return non-nil when the point is
in the construct, and nil when it's not.

Return the point after move.  When we can't move backward, return
nil.

When the point is not in the construct in the first place, throw
and error \"Not in a CONSTRUCT\"."
  (when (not (funcall probe))
    (error (format "Not in a %s" construct)))
  (let ((beg (save-excursion (toki/strict-primitive-backward-sexp)))
        pos)
    (when beg
      (save-excursion
        (while (and (toki/backward-same-syntax)
                    (funcall probe)
                    (setq pos (point))
                    (> (point) beg))))
      (when (and pos (<= pos beg))
        (goto-char pos)))))

(defun toki/strict-forward-sexp-in-string ()
  "Move strict forward a sexp in when point is in string.
The default `(forward-sexp)' thinks the end quote of a string and
a beginning quote of the next string wraps a sexp.  This fixed
that.

Return the point after move.  When we can't move forward (i.e.,
hit the ending quote), return nil."
  (toki/strict-primitive-forward-sexp-in-construct #'toki/in-string-p "string"))

(defun toki/strict-backward-sexp-in-string ()
  "Move strict backward a sexp in when point is in string.
The default `(backward-sexp)' thinks the end quote of the
previous string and a beginning quote of this string wraps a
sexp.  This fixed that.

Return the point after move.  When we can't move backward (i.e.,
hit the beginning quote), return nil."
  (toki/strict-primitive-backward-sexp-in-construct #'toki/in-string-p "string"))

(defun toki/strict-forward-sexp-in-comment ()
  "Move strict forward a sexp in when point is in a comment.
The default `(forward-sexp)' goes over the end quote of the
comment block.  This fixed that.

Return the point after move.  When we can't move forward (i.e.,
hit the ending quote), return nil.

Notice that a point inside the (multichar) quote is not
considered as in the comment."
  (toki/strict-primitive-forward-sexp-in-construct #'toki/in-comment-p "comment"))

(defun toki/strict-backward-sexp-in-comment ()
  "Move strict backward a sexp in when point is in a comment.
The default `(backward-sexp)' goes over the beginning quote of
the comment block.  This fixes that.

Return the point after move.  When we can't move backward (i.e.,
hit the beginning quote), return nil.

Notice that a point inside the (multichar) quote is not
considered as in the comment."
  (toki/strict-primitive-backward-sexp-in-construct #'toki/in-comment-p "comment"))

(defun toki/strict-forward-sexp ()
  "Move strict forward a sexp, including a whole comment block.
Return the point if success, otherwise return nil."
  (let ((from (point)))
    (toki/forward-blanks)
    (or (toki/forward-comment-block)
        (cond
         ;; `toki/in-comment-p' doesn't consert a point inside the
         ;; (multichar) comment quote as in the comment, but this is fine as
         ;; when a user is deleting things with the point at there, they
         ;; probably want to break the balanced comment quotes.
         ((toki/in-comment-p) (toki/strict-forward-sexp-in-comment))
         ((toki/in-string-p) (toki/strict-forward-sexp-in-string))
         (t (toki/strict-primitive-forward-sexp))))
    (let ((to (point)))
      (unless (eq from to) to))))

(defun toki/strict-backward-sexp ()
  "Jump backward a sexp or balanced comment block.
Return the point if success, otherwise return nil"
  (let ((from (point)))
    (toki/backward-blanks)
    (or (toki/backward-comment-block)
        (cond
         ;; `toki/in-comment-p' doesn't consert a point inside the
         ;; (multichar) comment quote as in the comment, but this is fine
         ;; as when a user is deleting things with the point at there,
         ;; they probably want to break the balanced comment quotes.
         ((toki/in-comment-p) (toki/strict-backward-sexp-in-comment))
         ((toki/in-string-p) (toki/strict-backward-sexp-in-string))
         (t (toki/strict-primitive-backward-sexp))))
    (let ((to (point)))
      (unless (eq from to) to))))

;;;;; Movements: symbol + blank

(defun toki/forward-symbol (&optional bound)
  "Go forward a symbol in front.
Return the point if success.  If BOUND is non-nil, don't go
further than BOUND."
  (unless (eobp)
    (let ((from (point)))
      (while (and (or (null bound)
                      (< (point) bound))
                  (memq (toki/syntax-char-after) '(?w ?_))
                  (toki/forward-same-syntax)))
      (let ((pt (point)))
        (if (and bound (> pt bound))
            (goto-char bound)
          (unless (eq pt from) pt))))))

(defun toki/backward-symbol (&optional bound)
  "Go backward a symbol behind the point.
Return the point if success.  If BOUND is non-nil, don't go
further than BOUND."
  (unless (bobp)
    (let ((from (point)))
      (while (and (or (null bound)
                      (> (point) bound))
                  (memq (toki/syntax-char-after (1- (point))) '(?w ?_))
                  (toki/backward-same-syntax)))
      (let ((pt (point)))
        (if (and bound (< pt bound))
            (goto-char bound)
          (unless (eq pt from) pt))))))

;;;;; Balance test

(defun toki/region-balance-p (pt1 pt2 &optional strict)
  "Return t when the region from PT1 to PT2 is balanced.
When STRICT is nil, this is tolerant to unbalanced word
delimeters like \"if..end if\", \"begin..end\", \"def\", as it's
common to delete part of such a block and then rewrite it.  When
STRICT is non-nil, scan the expressions strictly and don't treat
word delimiters differently."
  (cl-block nil
    (when (eq pt1 pt2)
      (cl-return t))
    (save-excursion
      (let ((beg (min pt1 pt2))
            (end (max pt1 pt2)))
        (goto-char beg)
        (while (< (point) end)
          (if (or (unless strict (toki/forward-symbol end))
                  (toki/forward-blanks end)
                  (toki/strict-forward-sexp))
              (when (eq (point) end)
                (cl-return t))
            (cl-return nil)))
        ;; Now we have (> (point) end).  This means END is in a sexp.  If END
        ;; is in an atom, we can still delete the part of this atom before END,
        ;; resulting a balanced state, so we check if this is true.  Notice the
        ;; (= (point) end) situation causes return in the above code, so we
        ;; don't need to handle it.
        (let ((goal (point)))
          (goto-char end)
          (if (and (toki/strict-forward-sexp)
                   (eq (point) goal))
              (cl-return t)
            (cl-return nil)))))))

;;;;; Deletion

(defun toki/delete-region (pt1 pt2 &optional kill)
  "Delete the region between PT1 and PT2.
When KILL is non-nil, also save it to kill ring.

Return t if success."
  ;; `kill-region' and `delete-region' signal errors when they fail.
  (if kill
      (kill-region pt1 pt2)
    (delete-region pt1 pt2))
  t)

(defun toki/soft-delete-region (pt1 pt2 &optional strict kill msg)
  "Soft delete the region between PT1 and PT2.
Return t if success.  When deleting it causes unbalanced state,
don't delete it.  In this situation, if MSG is non-nil, throw an
error using MSG.

When KILL is non-nil, also save the deleted part to the kill
ring.

This is tolerant to deleting word delimiters, unless STRICT is
non-nil, see the explanation in `toki/region-balance-p'."
  (if (toki/region-balance-p pt1 pt2 strict)
      (progn (if kill (kill-region pt1 pt2)
               (delete-region pt1 pt2))
             t)
    (when msg (toki/unbalance-error msg))))

(defun toki/soft-delete
    (from to &optional strict-sexp style kill fail-action)
    "Soft delete from point FROM to TO.
If STRICT-SEXP is nil, word delimiters like \"if..end if\",
\"begin..end\" and \"def\" are considered as balanced
expressions, and can be delete safely, as it's common to delete
part of such a block and then rewrite it.  If STRICT-SEXP is
non-nil, only consider syntactically balanced expressions as
balanced.

STYLE can be:

- `precise' or nil: Delete the region from FROM to TO if it's
  balanced.
- `within': Delete sexps until no more to delete, or we've
  reached a position at or before TO, and going over one more
  sexp will cause point to go beyond TO.
- `beyond': Delete at least one sexps (can be empty lines) until
  no more to delete, or we've reached a position beyond TO

A series of consecutive whitespaces is considered a sexp.

When KILL is non-nil, save the deleted part to the kill ring.

When something is deleted, this returns non-nil; when nothing is
deleted, we say the deletion fails.  FAIL-ACTION specifies the
action after failure.  It can be:

- nil: If MSG is nil, do nothing and return nil, otherwise signal
  an user error using MSG.
- `delete-one': Delete 1 sexp, if there is one.
- `jump': Jump to point TO, and return nil.
- `jump-and-reverse-delete': Jump to point TO, and try soft
  delete from TO to FROM, with the same arguments, but STYLE
  being `within'."
  (setq style (or style 'precise))
  (unless (eq from to)
    (let* ((forward (< from to))
           (move-atom (if forward #'toki/forward-atom #'toki/backward-atom))
           (move-blanks (if forward #'toki/forward-blanks #'toki/backward-blanks))
           (move-sexp (if forward #'toki/strict-forward-sexp #'toki/strict-backward-sexp))
           (move (lambda ()
                   (or (unless strict-sexp
                         (funcall move-atom
                                  (when (eq style 'precise) to)))
                       (funcall move-blanks to)
                       (funcall move-sexp))))
           (within-p (if forward (lambda () (< (point) to)) (lambda () (> (point) to))))
           (beyond-goal (lambda ()
                          (let ((goal (save-excursion
                                        (goto-char from)
                                        (while (and (funcall within-p)
                                                    (funcall move)))
                                        (point))))
                            (when (and goal (not (eq from goal))) goal))))
           (within-goal (lambda ()
                          (let* (prev-goal
                                 (goal
                                  (save-excursion
                                    (goto-char from)
                                    (while (and (funcall within-p)
                                                (setq prev-goal (point))
                                                (funcall move)))
                                    prev-goal)))
                            (when (and goal (not (eq from goal)))
                              goal))))
           (fail-act (lambda ()
                       (pcase fail-action
                         ('nil nil)
                         ('delete-one (save-excursion
                                        (goto-char from)
                                        (funcall move)
                                        (let ((pt (point)))
                                          (unless (eq from pt)
                                            (toki/delete-region from pt kill)))))
                         ('jump (goto-char to) nil)
                         ('jump-and-reverse-delete
                          (goto-char to)
                          (toki/soft-delete
                           to from strict-sexp 'within kill nil))
                         (_ (error "Invalid FAIL-ACTION"))))))
      (or (pcase style
            ('beyond (when-let ((goal (funcall beyond-goal)))
                       (toki/delete-region from goal kill)))
            ('within (when-let ((goal (funcall within-goal)))
                       (toki/delete-region from goal kill)))
            ('precise (toki/soft-delete-region from to strict-sexp kill)))
          (funcall fail-act)))))

;;;;; Core API

(defun toki/soft-delete-by-move
    (func &optional strict-sexp style kill fail-action)
  "Soft delete between point and the position after calling FUNC.
This calls `toki/soft-delete' internally, see its docstring for
details."
  (let ((pt (point))
        (goal (save-excursion (funcall func) (point))))
    (toki/soft-delete pt goal strict-sexp style kill fail-action)))

(defun toki/delete-by-move (func &optional kill)
  "(Hard) Delete between point and the position after calling FUNC.
If KILL is non-nil, save the deleted part in the kill ring."
  (let ((pt (point))
        (goal (save-excursion (funcall func) (point))))
    (toki/delete-region pt goal kill)))

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
  (if (use-region-p)
      (toki-delete-active-region)
    (toki/soft-delete-by-move #'backward-char nil nil nil 'jump-and-reverse-delete)))

;;;###autoload
(defun toki-forward-delete-char ()
  "Delete char forward while keeping expressions balanced."
  (interactive)
  (if (use-region-p)
      (toki-delete-active-region)
    (toki/soft-delete-by-move #'forward-char nil nil nil 'jump-and-reverse-delete)))

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
  (if (use-region-p)
      (toki-delete-active-region)
    (toki/soft-delete-by-move #'toki-forward-word nil nil nil 'jump-and-reverse-delete)))

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
  (if (use-region-p)
      (toki-delete-active-region)
    (toki/soft-delete-by-move #'toki-backward-word nil nil nil 'jump-and-reverse-delete)))

;;;;;; Line

;;;###autoload
(defun toki-kill-line ()
  "Kill a line forward while keeping expressions balanced."
  (interactive)
  (if (use-region-p)
      (toki-kill-active-region)
    (and
     (toki/soft-delete-by-move (lambda ()
                                 (if (eolp) (forward-line) (end-of-line)))
                               'strict-sexp 'beyond 'kill)
     (when (not (toki/line-empty-p))
       ;; Sometimes `indent-according-to-mode' causes the point to move, like
       ;; in `markdown-mode'.
       (save-excursion (indent-according-to-mode))))))

;;;###autoload
(defun toki-backward-kill-line ()
  "Kill a line backward while keeping expressions balanced."
  (interactive)
  (if (use-region-p)
      (toki-kill-active-region)
    (and
     (toki/soft-delete-by-move (lambda ()
                                 (if (bolp) (forward-line -1) (beginning-of-line)))
                               'strict-sexp 'beyond 'kill)
     (when (not (toki/line-empty-p))
       (save-excursion (indent-according-to-mode))))))

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

;;;###autoload
(defun toki-forward-sexp ()
  "Go forward a sexp.
This is the same as `toki/strict-forward-sexp', except that it
jumps forward consecutive single-line comments."
  (interactive)
  (let ((from (point)))
    (toki/forward-blanks)
    (or (toki/forward-consecutive-single-line-comments)
        (toki/strict-forward-sexp))
    (let ((to (point)))
      (unless (eq from to) to))))

;;;###autoload
(defun toki-backward-sexp ()
  "Go backward a sexp.
This is the same as `toki/strict-backward-sexp', except that it
jumps backward consecutive single-line comments."
  (interactive)
  (let ((from (point)))
    (toki/backward-blanks)
    (or (toki/backward-consecutive-single-line-comments)
        (toki/strict-backward-sexp))
    (let ((to (point)))
      (unless (eq from to) to))))

;;;###autoload
(defun toki-beginning-of-sexp ()
  "Go to the beginning of current sexp."
  (interactive)
  (push-mark)
  (while (toki/strict-backward-sexp)))

;;;###autoload
(defun toki-end-of-sexp ()
  "Go to the beginning of current sexp."
  (interactive)
  (push-mark)
  (while (toki/strict-forward-sexp)))

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
(defun toki-delete-active-region ()
  "Delete active region.
When this will cause unbalanced state, ask the user to confirm."
  (interactive)
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (when (or (toki/region-balance-p beg end)
                  (y-or-n-p "Delete the region will cause unbalanced state.  \
Continue? "))
          (toki/delete-region beg end)))
    (user-error "No active region")))

;;;###autoload
(defun toki-kill-active-region ()
  "Kill active region.
When this will cause unbalanced state, ask the user to confirm."
  (interactive)
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (when (or (toki/region-balance-p beg end)
                  (y-or-n-p "Delete the region will cause unbalanced state.  \
Continue? "))
          (setq this-command 'kill-region)
          (toki/delete-region beg end 'kill)))
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
    (define-key map (kbd "C-M-f") 'toki-forward-sexp)
    (define-key map (kbd "C-M-b") 'toki-backward-sexp)
    (define-key map (kbd "C-M-a") 'toki-beginning-of-sexp)
    (define-key map (kbd "C-M-e") 'toki-end-of-sexp)
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
