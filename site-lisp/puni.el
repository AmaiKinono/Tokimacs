;;; puni.el --- Parentheses Universalistic -*- lexical-binding: t -*-

;; Copyright (C) 2021 Hao Wang
;; License: GPL v3, or (at your option) any later version

;;; Commentary:

;;; Code:

;; To see the outline of this file, run `outline-minor-mode', then
;; `outline-hide-body'.  Another way is to run `occur' with the query:
;; ^;;;;* \|^(

;;;; Libraries

(require 'cl-lib)
(require 'rx)

;;;; Internals

;;;;; Probes

(defun puni--line-empty-p ()
  "Return t if current line is empty or contains only spaces."
  (save-excursion
    (beginning-of-line)
    (looking-at (rx line-start (* space) line-end))))

(defun puni--in-string-p ()
  "Return t if point is in a string.
Notice this returns nil if point is before the opening quote, or
after the end quote."
  (eq (syntax-ppss-context (syntax-ppss)) 'string))

(defun puni--in-comment-p ()
  "Return t if point is in a comment.
Notice this returns nil if point is before/in the opening
delimiter, or after/in the end delimiter."
  (eq (syntax-ppss-context (syntax-ppss)) 'comment))

;;;;; Errors

(defun puni--error-if-before-point (bound)
  "Error if BOUND is non-nil and is before point."
  (when (and bound (< bound (point)))
    (error "BOUND is before point")))

(defun puni--error-if-after-point (bound)
  "Error if BOUND is non-nil and is after point."
  (when (and bound (> bound (point)))
    (error "BOUND is after point")))

;;;;; Syntax

;; Ref: https://www.gnu.org/software/emacs/manual/html_node/elisp/Syntax-Class-Table.html
;; and https://debbugs.gnu.org/cgi/bugreport.cgi?bug=37452

(defun puni--syntax-class-to-char (syntax-class)
  "Return the designator char of SYNTAX-CLASS."
  (aref " .w_()'\"$\\/<>@!|" syntax-class))

(defun puni--syntax-char-after (&optional point)
  "Return the syntax code after POINT, described by a char.
When POINT is nil, return the syntax code after the current
point.  When POINT doesn't exist, or there's no char after POINT,
return nil.

For the meaning of the returned char, see `modify-syntax-entry'."
  (let ((point (or point (point))))
    (unless (or (< point (point-min))
                (>= point (point-max)))
      (puni--syntax-class-to-char (syntax-class (syntax-after point))))))

;;;;; Basic move: blank

;; NOTE: ALl "basic moves" (take those move forward as example), except those
;; explicitely deals with blanks, assumes they starts in a position where no
;; spaces are after point, and will go to a position where no spaces are before
;; point.

(defun puni--forward-blanks (&optional bound)
  "Jump forward whitespaces and newlines.
Return t if success.  When BOUND is non-nil, don't go further
than BOUND."
  (puni--error-if-before-point bound)
  (when (looking-at (rx (+ (or space "\n"))))
    (goto-char (match-end 0))
    (when (and bound (> (point) bound))
      (goto-char bound))
    t))

(defun puni--backward-blanks (&optional bound)
  "Backward version of `puni--forward-blanks'."
  (puni--error-if-after-point bound)
  (unless (bobp)
    (let ((from (point)))
      (while (and (or (null bound) (> (point) bound))
                  (or (puni--backward-syntax " " bound)
                      (when (bolp) (forward-char -1) t))))
      (let ((to (point)))
        (unless (eq from to) to)))))

;;;;; Basic move: syntax

(defun puni--forward-syntax (syntax &optional bound)
  "Go forward across chars in specified syntax classes.
SYNTAX is a string of syntax code chars.  When BOUND is non-nil,
stop before BOUND.

This is the same as `skip-syntax-forward', except that:

- It signals an error is BOUND is before point at the first place.
- If it fails, return 0.
- If sucess, return the point after move."
  (puni--error-if-before-point bound)
  (pcase (skip-syntax-forward syntax bound)
    (0 nil)
    (_ (point))))

(defun puni--backward-syntax (syntax &optional bound)
  "Backward version of `puni--forward-syntax'."
  (puni--error-if-after-point bound)
  (pcase (skip-syntax-backward syntax bound)
    (0 nil)
    (_ (point))))

(defun puni--forward-same-syntax (&optional bound)
  "Move point past all characters with the same syntax class.
It returns the point after move.  If BOUND is non-nil, stop
before BOUND.

This is more robust than `forward-same-syntax' because it takes
`syntax-table' text properties into account.  See the docstring
of `char-syntax'."
  (puni--error-if-before-point bound)
  (unless (eobp)
    (skip-syntax-forward
     (char-to-string (puni--syntax-char-after (point))))
    (if (and bound (> (point) bound))
        (goto-char bound)
      (point))))

(defun puni--backward-same-syntax (&optional bound)
  "Backward version of `puni--forward-same-syntax'."
  (puni--error-if-after-point bound)
  (unless (bobp)
    (skip-syntax-backward
     (char-to-string (puni--syntax-char-after (1- (point)))))
    (if (and bound (< (point) bound))
        (goto-char bound)
      (point))))

;;;;; Basic move: atom

(defun puni--forward-atom (&optional bound)
  "Move forward an atom if there's one in front.
An atom is a symbol, allowing escaped non-symbol chars in it, and
an expression prefixe before it.

If BOUND is non-nil, stop before BOUND."
  ;; It may be a good idea to treat a series of punctuations as an atom (think
  ;; of operators).  Unfortunately there are major modes where "<>" should be
  ;; delimiters, but are given the punctuation syntax.
  (puni--error-if-before-point bound)
  (let ((from (point)))
    (puni--forward-syntax "'" bound)
    (while (or (puni--forward-syntax "_w" bound)
               (when (eq (puni--syntax-char-after) ?\\)
                 (puni--forward-syntax "\\" bound)
                 (unless (eq (point) bound) (forward-char))
                 t)))
    (let ((to (point)))
      (unless (eq from to)
        to))))

(defun puni--backward-atom (&optional bound)
  "Backward version of `puni--forward-atom'."
  (puni--error-if-after-point bound)
  (let ((from (point)))
    ;; In case we start after an escape char.
    (puni--backward-syntax "\\" bound)
    (while (or (puni--backward-syntax "_w" bound)
               (when (eq (puni--syntax-char-after (- (point) 2)) ?\\)
                 (unless (eq (point) bound) (forward-char -1))
                 (puni--backward-syntax "\\" bound)
                 t)))
    (puni--backward-syntax "'" bound)
    (let ((to (point)))
      (unless (eq from to)
        to))))

(defun puni--in-atom-p (beg end)
  "Return t if the region between BEG and END is in one atom.
See `puni--forward-atom' to know what's an atom."
  (save-excursion
    (goto-char beg)
    (eq (puni--forward-atom end) end)))

;;;;; Basic move: string

(defun puni--forward-string ()
  "Move forward a string.
Return the point if success, otherwise return nil."
  (let ((from (point)))
    (save-excursion
      (puni--forward-syntax "\\")
      (puni--forward-syntax "\"")
      (when (and (not (eq from (point)))
                 (puni--in-string-p))
        ;; The default `forward-sexp' could jump over a string.
        ;; `forward-sexp-function' from the major-mode sometimes doesn't, when
        ;; they jump to the end of a further delimiter.
        (let ((forward-sexp-function nil))
          (goto-char from)
          (forward-sexp))
        (let ((to (point)))
          (when (not (eq from to))
            (goto-char to)))))))

(defun puni--backward-string ()
  "Backward version of `puni--forward-string'."
  (let ((from (point)))
    (save-excursion
      (puni--backward-syntax "\"")
      (puni--backward-syntax "\\")
      (when (and (not (eq from (point)))
                 (puni--in-string-p))
        (let ((forward-sexp-function nil))
          (goto-char from)
          (forward-sexp -1))
        (let ((to (point)))
          (when (not (eq from to))
            (goto-char to)))))))

;;;;; Basic move: comment

(defun puni--forward-comment-block ()
  "Jump forward a whole comment block.
Return the point if success."
  (let (to)
    (save-excursion
      (when (or (when (eq (puni--syntax-char-after) ?<)
                  ;; When we are before a single line comment at the end of the
                  ;; file, and there's no trailing newline, `forward-comment'
                  ;; does its work but returns nil.
                  (forward-comment 1) t)
                (forward-comment 1))
        (puni--backward-blanks)
        (setq to (point))))
    (when to (goto-char to))))

(defun puni--backward-comment-block ()
  "Jump backward a whole comment block.
Return the point if success."
  (let ((from (point))
        to)
    (save-excursion
      ;; Sometimes it looks like the point is at the end of comment, but
      ;; actually not, e.g., there are spaces after point, or a newline follows
      ;; the point, which is the end delimiter of comment itself.  In these
      ;; situations, we jump over blanks to put the point after the comment
      ;; block.
      (puni--forward-blanks)
      (when (forward-comment -1)
        (puni--forward-blanks from)
        (setq to (point))))
    (when to (goto-char to))))

;;;;; Basic move: single line comment

;; This section has nothing to do with core functionality, but only the
;; interactive `puni-forward/backward-sexp' commands.

(defun puni--begin-of-single-line-comment-p ()
  "Return t if point is at the opening delimiter of a single line comment."
  (save-excursion
    (and (eq (puni--syntax-char-after) ?<)
         (progn (forward-comment 1)
                (or (eobp) (eq (char-before) ?\n))))))

(defun puni--end-of-single-line-comment-p ()
  "Return t if point is after the end delimiter of a single line comment.
The end delimiter is a newline.

This doesn't work for single line comment at the end of file
without a trailing newline."
  (and (eq (puni--syntax-char-after (1- (point))) ?>)
       (eq (char-before) ?\n)))

(defun puni--forward-consecutive-single-line-comments ()
  "Jump forward a series of single-line comments.
Return the point if success."
  (let ((from (point))
        to)
    (save-excursion
      (while (and (puni--begin-of-single-line-comment-p)
                  (progn (forward-line)
                         (beginning-of-line)
                         (puni--forward-syntax " ")
                         t)))
      (unless (eq from (point))
        (puni--backward-blanks from)
        (setq to (point))))
    (when to (goto-char to))))

;; TODO: I found this not easy to use...
(defun puni--backward-consecutive-single-line-comments ()
  "Jump backward a series of single-line comments.
Return the point if success."
  (let ((from (point))
        to)
    (save-excursion
      (while (and (progn (puni--forward-syntax " ")
                         (when (eq (char-after) ?\n)
                           (forward-char))
                         (puni--end-of-single-line-comment-p))
                  (when (forward-comment -1)
                    (puni--backward-syntax " ")
                    (and (bolp)
                         (unless (bobp)
                           (forward-char -1)
                           (not (puni--line-empty-p)))))))
      (unless (>= (point) from)
        (puni--forward-blanks from)
        (setq to (point))))
    (when to (goto-char to))))

;;;;; Basic move: sexp

;; In this section, we build necessary components for the final
;; `puni--strict-forward/backward-sexp' functions.

(defun puni--primitive-forward-sexp (&optional n)
  "A wrapper around `forward-sexp'.
Move forward N sexp, return the point if success, otherwise
return nil.

This wrapper is here since `forward-sexp' can fail differently in
different major modes, e.g., the built-in one for Lisp will throw
a `scan-error', the one from `nxml-mode' throws a plain error,
while the one from `web-mode' just does nothing and returns nil."
  (condition-case _
      (let ((from (point))
            (to (progn (forward-sexp n) (point))))
        (unless (eq from to) to))
    (error nil)))

;; NOTE: The real challenge is what to do when the delimiter is not the
;; symbol/char at point, but something like "bound of symbol" or "newline"?
(defun puni--pair-or-in-delim-p (beg end)
  "Return t if BEG and END is a pair of delimiter, or in the same delimiter.
This uses syntax table and some heuristic, and is not completely
reliable.  It's also not generic, as it's designed only for a
branch in `puni--inside-delim-p'.  So it assumes one of BEG and
END is the bound of delimiter."
  (when (eq beg end) (error "BEG is the same as END"))
  (or (eq beg (1- end))
      (eq (save-excursion (goto-char beg)
                          (puni--forward-same-syntax end))
          end)
      (let ((beg-syntax (puni--syntax-char-after beg))
            (end-syntax (puni--syntax-char-after (1- end)))
            (beg-char (char-after beg))
            (end-char (char-before end)))
        ;; If beg & end are a pair of delimiters, we think beg is paired with
        ;; end.
        (or (and (eq beg-syntax ?\() (eq end-syntax ?\)))
            (and (eq beg-syntax ?<) (eq end-syntax ?>))
            ;; If we've reached here, we need to consider the situations
            ;; where BOUND is a delimiter (as assumed), but doesn't have
            ;; delimiter syntax.
            (and (eq beg-syntax ?.) (eq end-syntax ?.)
                 (eq beg-char ?<) (eq end-char ?>))
            (and (eq beg-syntax end-syntax)
                 (eq beg-char end-char))))))

(defun puni--inside-delim-p (pt beg end direction)
  "See if PT is inside the delimiters at BEG or END.
The delimiters could be multi-char (like html tags).  By
\"inside\" we also mean \"just after the opening delimiter\", or
\"just before the closing delimiter\".

if DIRECTION is `forward', check if the char after PT is inside
any of the delimiters, or if DIRECTION is `backward', check the
char before PT instead."
  (unless (< beg pt end) (error "PT is not between BEG and END"))
  ;; Assume a string can't be a delimiter. We also assume an atom can't be a
  ;; delimiter.  This is not true, but many major modes thinks "a = b" is a
  ;; sexp, where it's actually safe to delete a or b.  In this situation, it's
  ;; the "bound of symbol" being the delimiter, not the symbol itself.
  (unless (or (save-excursion (goto-char beg) (or (puni--forward-string)
                                                  (puni--forward-atom)))
              (save-excursion (goto-char end) (or (puni--backward-string)
                                                  (puni--backward-atom))))
    (pcase direction
      ('forward (or (puni--pair-or-in-delim-p beg (1+ pt))
                    (puni--pair-or-in-delim-p pt end)))
      ('backward (or (puni--pair-or-in-delim-p beg pt)
                     (puni--pair-or-in-delim-p (1- pt) end)))
      (_ (error "Invalid DIRECTION")))))

;; NOTE: Try delete `(something)'
;; NOTE: \"thing\"

(defun puni--strict-primitive-forward-sexp ()
  "Move forward a sexp, return the point if success, otherwise return nil.
If there's a balanced sexp in front, but jumping over it will
move us to a different depth in the whole syntax tree, this won't
go over it.

Notice this doesn't work well in strings, as the built-in
`forward-sexp' thinks the closing quote of this string, and the
opening quote of the next one, forms a string.  It also doesn't
work well for balanced comment blocks.  So we'll build on top of
this function until we have `puni-strict-forward-sexp', which
works on these situations."
  (let (beg end beg-of-maybe-another-sexp end-of-maybe-another-sexp)
    (save-excursion
      (setq beg (point))
      (setq end (puni--primitive-forward-sexp))
      (setq beg-of-maybe-another-sexp (puni--primitive-forward-sexp -1))
      (setq end-of-maybe-another-sexp (puni--primitive-forward-sexp)))
    ;; Make sure we can actually go forward a sexp.  This also incidentally
    ;; checks if the point is at the end of buffer.  Notice that
    ;; beg/end-of-maybe-another-sexp shouldn't be nil if end is non-nil, unless
    ;; we are using a `forward-sexp-function' that really doesn't work.
    (when (and beg end beg-of-maybe-another-sexp end-of-maybe-another-sexp)
      (cond
       ;; Logically, this shouldn't happen.  In reality, the only situation
       ;; I've found is the default sexp don't work with punctuations well.
       ;; Try this:
       ;;
       ;;     foo|. bar        (forward and backward sexp)
       ;;
       ;; Or this:
       ;;
       ;;     foo bar|.        (forward -> backward -> forward sexp)
       ;;
       ;; The punctuations are skipped over while finding the next sexp.
       ;; When this happens, we think the next syntax block that's skipped
       ;; over is a balanced expression.
       ;;
       ;; NOTE: another situation:
       ;;
       ;;     <p|></p>
       ((> beg-of-maybe-another-sexp beg)
        (setq end (save-excursion
                    (goto-char beg)
                    (or (puni--forward-atom)
                        (puni--forward-string)
                        (puni--forward-same-syntax)))))
       ((not (eq end end-of-maybe-another-sexp))
        (when (puni--inside-delim-p beg beg-of-maybe-another-sexp end 'forward)
          (setq end nil)))
       ;; If we didn't fall into the last branch, that means BEG is inside
       ;; another bigger sexp, and `forward-sexp' knows the bounds of that
       ;; bigger sexp.
       ((< beg-of-maybe-another-sexp beg)
        (if (puni--inside-delim-p beg beg-of-maybe-another-sexp end 'forward)
            (setq end nil)
          (setq end (save-excursion (or (puni--forward-atom)
                                        (puni--forward-string)
                                        (puni--forward-same-syntax))))))
       ;; The implicit branch here is (= beg-of-maybe-another-sexp beg).  We
       ;; don't need to do anything as this means BEG and END forms a
       ;; balanced expression.
       )
      (when end (goto-char end)))))

;; TODO: fix this according to the forward version.
(defun puni--strict-primitive-backward-sexp ()
  "Backward version of `puni--strict-primitive-forward-sexp'."
  (let (beg end beg-of-maybe-another-sexp end-of-maybe-another-sexp)
    (save-excursion
      (setq end (point))
      (setq beg (puni--primitive-forward-sexp -1))
      (setq end-of-maybe-another-sexp (puni--primitive-forward-sexp))
      (setq beg-of-maybe-another-sexp (puni--primitive-forward-sexp -1)))
    (when (and beg end beg-of-maybe-another-sexp end-of-maybe-another-sexp)
      (cond
       ((< end-of-maybe-another-sexp end)
        (setq beg (save-excursion
                    (goto-char end)
                    (or (puni--backward-atom)
                        (puni--backward-string)
                        (puni--backward-same-syntax)))))
       ((not (eq beg beg-of-maybe-another-sexp))
        (when (puni--inside-delim-p end beg end-of-maybe-another-sexp 'backward)
          (setq beg nil)))
       ((> end-of-maybe-another-sexp end)
        (if (puni--inside-delim-p end beg end-of-maybe-another-sexp 'backward)
            (setq beg nil)
          (setq beg (save-excursion (or (puni--backward-atom)
                                        (puni--backward-string)
                                        (puni--backward-same-syntax)))))))
      (when beg (goto-char beg)))))

(defun puni--strict-primitive-forward-sexp-in-thing (probe thing)
  "Move strict forward a sexp in certain thing.
PROBE is a function that should return non-nil when the point is
in that thing, and nil when it's not.

Return the point after move.  When we can't move forward, return
nil.

When the point is not in the construct in the first place, throw
and error \"Not in a THING\"."
  (when (not (funcall probe))
    (error (format "Not in a %s" thing)))
  (let ((to (save-excursion (puni--strict-primitive-forward-sexp)))
        pos)
    (when to
      (save-excursion
        (while (and (puni--forward-same-syntax to)
                    (funcall probe)
                    (setq pos (point))
                    (< (point) to))))
      (when (and pos (>= pos to))
        (goto-char pos)))))

(defun puni--strict-primitive-backward-sexp-in-thing (probe thing)
  "Backward version of `puni--strict-primitive-forward-sexp-in-thing'."
  (when (not (funcall probe))
    (error (format "Not in a %s" thing)))
  (let ((to (save-excursion (puni--strict-primitive-backward-sexp)))
        pos)
    (when to
      (save-excursion
        (while (and (puni--backward-same-syntax to)
                    (funcall probe)
                    (setq pos (point))
                    (> (point) to))))
      (when (and pos (<= pos to))
        (goto-char pos)))))

(defun puni-strict-forward-sexp-in-string ()
  "Move strict forward a sexp in when point is in string.
The default `(forward-sexp)' thinks the end quote of a string and
a beginning quote of the next string wraps a sexp.  This fixed
that.

Return the point after move.  When we can't move forward (i.e.,
hit the ending quote), return nil."
  (puni--strict-primitive-forward-sexp-in-thing #'puni--in-string-p "string"))

(defun puni-strict-backward-sexp-in-string ()
  "Backward version of `puni-strict-forward-sexp-in-string'."
  (puni--strict-primitive-backward-sexp-in-thing #'puni--in-string-p "string"))

(defun puni-strict-forward-sexp-in-comment ()
  "Move strict forward a sexp in when point is in a comment.
The default `(forward-sexp)' goes to the end of the sexp after
the end quote of the comment block.  This fixed that.

Return the point after move.  When we can't move forward (i.e.,
hit the ending quote), return nil.

Notice that a point inside the (multichar) quote is not
considered as in the comment."
  (puni--strict-primitive-forward-sexp-in-thing #'puni--in-comment-p "comment"))

(defun puni-strict-backward-sexp-in-comment ()
  "Backward version of `puni-strict-forward-sexp-in-comment'."
  (puni--strict-primitive-backward-sexp-in-thing #'puni--in-comment-p
                                                 "comment"))

;;;; APIs

;;;;; API: Strict forward/backward sexp functions

(defun puni-strict-forward-sexp ()
  "Move strict forward a sexp, including a whole comment block.
Return the point if success, otherwise return nil."
  (let ((from (point)))
    (puni--forward-blanks)
    (or (puni--forward-comment-block)
        (cond
         ;; `puni--in-comment-p' doesn't consert a point inside the
         ;; (multichar) comment quote as in the comment, but this is fine as
         ;; when a user is deleting things with the point at there, they
         ;; probably want to break the balanced comment quotes.
         ((puni--in-comment-p) (puni-strict-forward-sexp-in-comment))
         ((puni--in-string-p) (puni-strict-forward-sexp-in-string))
         (t (puni--strict-primitive-forward-sexp))))
    (let ((to (point)))
      (unless (eq from to) to))))

(defun puni-strict-backward-sexp ()
  "Backward version of `puni-strict-forward-sexp'."
  (let ((from (point)))
    (puni--backward-blanks)
    (or (puni--backward-comment-block)
        (cond
         ((puni--in-comment-p) (puni-strict-backward-sexp-in-comment))
         ((puni--in-string-p) (puni-strict-backward-sexp-in-string))
         (t (puni--strict-primitive-backward-sexp))))
    (let ((to (point)))
      (unless (eq from to) to))))

;;;;; API: Balance test

(defun puni-region-balance-p (pt1 pt2 &optional strict)
  "Return t when the region from PT1 to PT2 is balanced.
When STRICT is nil, this is tolerant to unbalanced symbol
delimeters like \"if..end if\", \"begin..end\", \"def\", as it's
common to delete part of such a block and then rewrite it.  When
STRICT is non-nil, scan the expressions strictly and don't treat
symbol delimiters differently."
  (cl-block nil
    (when (eq pt1 pt2)
      (cl-return t))
    (save-excursion
      (let ((beg (min pt1 pt2))
            (end (max pt1 pt2)))
        (goto-char beg)
        (while (< (point) end)
          (if (or (unless strict (puni--forward-atom end))
                  (puni--forward-blanks end)
                  (puni--forward-string)
                  (puni-strict-forward-sexp))
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
          (if (and (puni-strict-forward-sexp)
                   (eq (point) goal))
              (cl-return t)
            (cl-return nil)))))))

;;;;; API: Deletion

(defun puni-delete-region (pt1 pt2 &optional kill)
  "Delete the region between PT1 and PT2.
When KILL is non-nil, also save it to kill ring.

Return t if success."
  ;; `kill-region' and `delete-region' signal errors when they fail.
  (if kill
      (kill-region pt1 pt2)
    (delete-region pt1 pt2))
  t)

(defun puni-delete-region-keep-balanced (pt1 pt2 &optional strict kill)
  "Delete the region between PT1 and PT2 if it's balanced.
Return t if success.  When deleting it causes unbalanced state,
don't delete it.

When KILL is non-nil, also save the deleted part to the kill
ring.

This is tolerant to deleting symbol delimiters, unless STRICT is
non-nil, see the explanation in `puni-region-balance-p'."
  (if (puni-region-balance-p pt1 pt2 strict)
      (progn (if kill (kill-region pt1 pt2)
               (delete-region pt1 pt2))
             t)))

(defun puni-soft-delete
    (from to &optional strict-sexp style kill fail-action)
    "Soft delete from point FROM to TO.
If STRICT-SEXP is nil, symbol delimiters like \"if..end if\",
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

- nil: Do nothing and return nil.
- `delete-one': Delete 1 sexp, if there is one.
- `jump': Jump to point TO, and return nil.
- `jump-and-reverse-delete': Jump to point TO, and try soft
  delete from TO to FROM, with the same arguments, but STYLE
  being `within'."
  (setq style (or style 'precise))
  (unless (eq from to)
    (let* ((forward (< from to))
           (move-atom (if forward #'puni--forward-atom #'puni--backward-atom))
           (move-blanks (if forward
                            #'puni--forward-blanks #'puni--backward-blanks))
           (move-sexp (if forward
                          #'puni-strict-forward-sexp #'puni-strict-backward-sexp))
           (move (lambda ()
                   (or (unless strict-sexp
                         (funcall move-atom
                                  (when (eq style 'precise) to)))
                       (funcall move-blanks to)
                       (funcall move-sexp))))
           (within-p (if forward
                         (lambda () (< (point) to))
                       (lambda () (> (point) to))))
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
                                            (puni-delete-region
                                             from pt kill)))))
                         ('jump (goto-char to) nil)
                         ('jump-and-reverse-delete
                          (goto-char to)
                          (puni-soft-delete
                           to from strict-sexp 'within kill nil))
                         (_ (error "Invalid FAIL-ACTION"))))))
      (or (pcase style
            ('beyond (when-let ((goal (funcall beyond-goal)))
                       (puni-delete-region from goal kill)))
            ('within (when-let ((goal (funcall within-goal)))
                       (puni-delete-region from goal kill)))
            ('precise (puni-delete-region-keep-balanced
                       from to strict-sexp kill)))
          (funcall fail-act)))))

(defun puni-soft-delete-by-move
    (func &optional strict-sexp style kill fail-action)
  "Soft delete between point and the position after calling FUNC.
This calls `puni-soft-delete' internally, see its docstring for
the meaning of STRICT-SEXP, STYLE, KILL and FAIL-ACTION."
  (let ((pt (point))
        (goal (save-excursion (funcall func) (point))))
    (puni-soft-delete pt goal strict-sexp style kill fail-action)))

;;;; Commands

;;;;; Kill/delete active region

;;;###autoload
(defun puni-delete-active-region ()
  "Delete active region.
When this will cause unbalanced state, ask the user to confirm."
  (interactive)
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (when (or (puni-region-balance-p beg end)
                  (y-or-n-p "Delete the region will cause unbalanced state.  \
Continue? "))
          (puni-delete-region beg end)))
    (user-error "No active region")))

;;;###autoload
(defun puni-kill-active-region ()
  "Kill active region.
When this will cause unbalanced state, ask the user to confirm."
  (interactive)
  (if (use-region-p)
      (let ((beg (region-beginning))
            (end (region-end)))
        (when (or (puni-region-balance-p beg end)
                  (y-or-n-p "Delete the region will cause unbalanced state.  \
Continue? "))
          (setq this-command 'kill-region)
          (puni-delete-region beg end 'kill)))
    (user-error "No active region")))

;;;;; Char

;;;###autoload
(defun puni-backward-delete-char ()
  "Delete char backward while keeping expressions balanced."
  (interactive)
  (if (use-region-p)
      (puni-delete-active-region)
    (puni-soft-delete-by-move #'backward-char nil nil nil
                              'jump-and-reverse-delete)))

;;;###autoload
(defun puni-forward-delete-char ()
  "Delete char forward while keeping expressions balanced."
  (interactive)
  (if (use-region-p)
      (puni-delete-active-region)
    (puni-soft-delete-by-move #'forward-char nil nil nil
                              'jump-and-reverse-delete)))

;;;;; Word

;;;###autoload
(defun puni-forward-kill-word ()
  "Kill word forward while keeping expressions balanced."
  (interactive)
  (if (use-region-p)
      (puni-kill-active-region)
    (puni-soft-delete-by-move #'forward-word nil nil 'kill
                              'jump-and-reverse-delete)))

;;;###autoload
(defun puni-backward-kill-word ()
  "Kill word backward while keeping expressions balanced."
  (interactive)
  (if (use-region-p)
      (puni-kill-active-region)
    (puni-soft-delete-by-move #'backward-word nil nil 'kill
                              'jump-and-reverse-delete)))

;;;;;; Line

;;;###autoload
(defun puni-kill-line ()
  "Kill a line forward while keeping expressions balanced."
  (interactive)
  (if (use-region-p)
      (puni-kill-active-region)
    (and
     (puni-soft-delete-by-move (lambda ()
                                 (if (eolp) (forward-line) (end-of-line)))
                               'strict-sexp 'beyond 'kill)
     (when (not (puni--line-empty-p))
       ;; Sometimes `indent-according-to-mode' causes the point to move, like
       ;; in `markdown-mode'.
       (save-excursion (indent-according-to-mode))))))

;;;###autoload
(defun puni-backward-kill-line ()
  "Kill a line backward while keeping expressions balanced."
  (interactive)
  (if (use-region-p)
      (puni-kill-active-region)
    (and
     (puni-soft-delete-by-move (lambda ()
                                 (if (bolp) (forward-line -1) (beginning-of-line)))
                               'strict-sexp 'beyond 'kill)
     (when (not (puni--line-empty-p))
       (save-excursion (indent-according-to-mode))))))

;;;;;; Sexp

;;;###autoload
(defun puni-forward-sexp ()
  "Go forward a sexp.
This is the same as `puni-strict-forward-sexp', except that it
jumps forward consecutive single-line comments."
  (interactive)
  (let ((from (point)))
    (puni--forward-blanks)
    (or (puni--forward-consecutive-single-line-comments)
        (puni-strict-forward-sexp))
    (let ((to (point)))
      (unless (eq from to) to))))

;;;###autoload
(defun puni-backward-sexp ()
  "Go backward a sexp.
This is the same as `puni-strict-backward-sexp', except that it
jumps backward consecutive single-line comments."
  (interactive)
  (let ((from (point)))
    (puni--backward-blanks)
    (or (puni--backward-consecutive-single-line-comments)
        (puni-strict-backward-sexp))
    (let ((to (point)))
      (unless (eq from to) to))))

;;;###autoload
(defun puni-beginning-of-sexp ()
  "Go to the beginning of current sexp.
If it goes to the beginning of the buffer, set a mark at where we
begin so we can pop back to it."
  (interactive)
  (unless (bobp)
    (let ((from (point)))
      (while (puni-strict-backward-sexp))
      (when (bobp)
        (push-mark from)))))

;;;###autoload
(defun puni-end-of-sexp ()
  "Go to the end of current sexp.
If it goes to the end of the buffer, set a mark at where we begin
so we can pop back to it."
  (interactive)
  (unless (eobp)
    (let ((from (point)))
      (while (puni-strict-forward-sexp))
      (when (eobp)
        (push-mark from)))))

;;;;;; Force delete

;;;###autoload
(defun puni-force-delete ()
  "Force delete backward char, or the active region.
Can be used to fight with undesired behavior of structural
editing."
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (when (not (bobp))
      (delete-region (1- (point)) (point)))))

(defvar puni-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "DEL") 'puni-backward-delete-char)
    (define-key map (kbd "C-d") 'puni-forward-delete-char)
    (define-key map (kbd "M-d") 'puni-forward-kill-word)
    (define-key map (kbd "M-DEL") 'puni-backward-kill-word)
    (define-key map (kbd "C-k") 'puni-kill-line)
    (define-key map (kbd "C-S-k") 'puni-backward-kill-line)
    (define-key map (kbd "C-DEL") 'puni-force-delete)
    (define-key map (kbd "C-<backspace>") 'puni-force-delete)
    (define-key map (kbd "C-M-f") 'puni-forward-sexp)
    (define-key map (kbd "C-M-b") 'puni-backward-sexp)
    (define-key map (kbd "C-M-a") 'puni-beginning-of-sexp)
    (define-key map (kbd "C-M-e") 'puni-end-of-sexp)
    map)
  "Keymap used for `puni-structural-editing-mode'.")

;;;###autoload
(define-minor-mode puni-mode
  "Curated keybinds for structural deleting commands."
  :keymap puni-mode-map)

;;;###autoload
(define-globalized-minor-mode puni-global-mode
  puni-mode
  (lambda () (puni-mode 1)))

(provide 'puni)

;;; puni.el ends here
