;;; toki-combo.el --- Combo command -*- lexical-binding: t -*-

;; Copyright (C) 2021 Hao Wang
;; License: GPL v3, or (at your option) any later version

;;; Commentary:

;; See the API `toki-make-combo-command'.

;;; Code:

;; To see the outline of this file, run `outline-minor-mode', then
;; `outline-hide-body'.  Another way is to run `occur' with the query:
;; ^;;;;* \|^(

(defun toki/strip-modifier (key)
  "Strip the modifier of KEY, and return the result.
KEY is an integer representing the key event, and so does the
return value.

Some keys are considered to have a modifier by
Emacs, like \"RET\" is the same as \"C-m\". Such keys are not
modified by this function."
  (if (member (key-description (make-vector 1 key))
              '("RET"))
      key
    (event-basic-type key)))

(defvar toki/combo-key-timer nil)

(defmacro toki-make-combo (command)
  "Return the combo version of COMMAND.
A combo command lets you repeat it by typing the last key you
used to call it, without modifier.  For example, repeat the
command bound to \"C-c C-o\" by a single \"o\".  Typing any other
key, or wait for 3 secs will both end the combo state.

To use this, bind your key to \"(toki-make-combo command-name)\"."
  `(defun ,(gensym (intern (concat (symbol-name command) "-combo-"))) ()
     ,(format "A combo version of `%s', see its docstring for details."
              command)
     (interactive)
     (let* ((minibuffer-message-timeout 3)
            (map (make-sparse-keymap))
            (key-event (make-vector 1
                                    (toki/strip-modifier
                                     (car (last (listify-key-sequence
                                                 (this-command-keys)))))))
            (combo-cmd (lambda ()
                         (interactive)
                         (when (timerp toki/combo-key-timer) (cancel-timer toki/combo-key-timer))
                         (setq toki/combo-key-timer (run-with-timer minibuffer-message-timeout
                                                                    nil (set-transient-map map t)))
                         (call-interactively #',command)
                         (message "Press %s to run %s again"
                                  (key-description key-event) ',command))))
       (define-key map key-event combo-cmd)
       (call-interactively combo-cmd))))

(provide 'toki-combo)

;;; toki-combo.el ends here
