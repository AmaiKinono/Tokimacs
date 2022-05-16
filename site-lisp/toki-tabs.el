;;; toki-tabs.el --- Tabs for frequently used buffers -*- lexical-binding: t -*-

;; Copyright (C) 2021 Hao Wang
;; License: GPL v3, or (at your option) any later version

;;; Commentary:

;;; Code:

;; To see the outline of this file, run `outline-minor-mode', then
;; `outline-hide-body'.  Another way is to run `occur' with the query:
;; ^;;;;* \|^(

;;;; Libraries

(require 'cl-lib)

;;;; User options

(defvar toki-tabs-count-freq-idle-time 1
  "Add used frequency after being idle for this much secs.")

(defvar toki-tabs-visible-buffer-limit 4
  "Max number of visible buffers in a group.")

(defvar toki-tabs-ellipsis "…"
  "Ellipsis used when truncating long buffer names.")

(defvar toki-tabs-buffer-name-max-length 16
  "Max length of buffer name shown in a tab.
Nil means don't truncate filenames.")

(defvar toki-tabs-project-root-function
  (lambda ()
    (when-let ((project (project-current nil))) (cdr project)))
  "A function that returns project root for current buffer.")

(defvar toki-tabs-separator "|"
  "A string used as separator between tabs.")

(defvar toki-tabs-update-hook nil
  "Hook to run when tabs need to be redisplayed.
You should customize this hook to run code that's needed to
update the UI.  `toki-tabs-string' can be used in the code.")

(defface toki-tabs-current-tab-face
  '((((background light))
     :background "#d5c9c0" :foreground "#282828"
     :bold t :inherit mode-line-active)
    (t
     :background "#504945" :foreground "#fbf1c7"
     :bold t :inherit mode-line-active))
  "Face for current tab.")

(defface toki-tabs-inactive-tab-face
  '((((background light))
     :foreground "#665c54" :inherit 'mode-line-active)
    (t
     :foreground "#bdae93" :bold nil :inherit 'mode-line-active))
  "Face for inactive tabs.")

(defface toki-tabs-separator-face
  '((((background light))
     :foreground "#bdae93" :bold t :inherit 'mode-line-active)
    (t
     :foreground "#665c54" :bold t :inherit 'mode-line-active))
  "Face for separator.")

(defface toki-tabs-rest-face
  '((t :italic t :bold nil :inherit 'mode-line-active))
  "Face for current tab.")

;;;; Internals

;;;;; Buffer group

(defvar-local toki-tabs/buffer-group nil)

(defun toki-tabs/buffer-group (&optional buffer)
  "Return the group name of BUFFER.
When BUFFER is nil, use current buffer."
  (let* ((buffer (or buffer (current-buffer)))
         (name (buffer-name buffer))
         group)
    (cond
     ;; These should be hidden.
     ((eq (aref name 0) ?\s) nil)
     ((cl-some (lambda (common-name-prefix)
                 (string-prefix-p common-name-prefix name))
               '("*Backtrace" "*scratch" "*Faces" "*Messages"
                 "*Customize"))
      "*Common*")
     ((setq group
            (or (buffer-local-value 'toki-tabs/buffer-group buffer)
                (with-current-buffer buffer
                  (setq toki-tabs/buffer-group
                        (when-let ((project
                                    (funcall toki-tabs-project-root-function)))
                          (expand-file-name project))))))
      group)
     ((eq (aref name 0) ?*) "*Common*")
     (t "*Others*"))))

;;;;; Frequency

(defvar toki-tabs/timer nil
  "Timer used for count buffer used frequency.")

(defvar-local toki-tabs/buffer-freq 0
  "Used frequency of current buffer.")

(defun toki-tabs/buffer-freq (buf)
  "Return the used frequency of buffer BUF."
  (or (buffer-local-value 'toki-tabs/buffer-freq buf)
      0))

(defun toki-tabs/increase-buffer-freq ()
  "Increase the used frequency of current buffer by 1."
  (cl-incf toki-tabs/buffer-freq))

(defun toki-tabs/buffer-freq-higher-p (buf1 buf2)
  "Return t if the used frequency of BUF1 is higher than BUF2."
  (> (toki-tabs/buffer-freq buf1)
     (toki-tabs/buffer-freq buf2)))

(defun toki-tabs/insert-buf (buf bufs)
  "Insert BUF into sorted BUFS.
BUFS is sorted in the decreasing order of used frequency.  The
insertion keeps this order.

This is non-destructive and the list after insertion is returned."
  (let ((freqs (mapcar #'toki-tabs/buffer-freq bufs))
        (freq (toki-tabs/buffer-freq buf))
        idx)
    (cl-dotimes (n (length freqs))
      (when (> freq (nth n freqs))
        (setq idx n)
        (cl-return nil)))
    (if (null idx)
        (append bufs (list buf))
      (append (cl-subseq bufs 0 idx)
              (list buf)
              (cl-subseq bufs idx)))))

(defun toki-tabs-start-count-freq ()
  "Start counting buffer used frequency."
  (setq toki-tabs/timer
        (run-with-idle-timer toki-tabs-count-freq-idle-time
                             t #'toki-tabs/increase-buffer-freq)))

(defun toki-tabs-stop-count-freq ()
  "Stop counting buffer used frequency."
  (cancel-timer toki-tabs/timer)
  (setq toki-tabs/timer nil))

;;;;; Sorted buffer list

(defvar toki-tabs/sorted-buffer-list nil
  "Buffer list sorted by used frequency.
This contains all non-hidden buffers returned by `buffer-list'.
It's updated by `toki-tabs/update-buffer-list'.")

(defvar toki-tabs/last-active-buffer nil
  "Last active buffer.
Minibuffer doesn't count.  This is updated by
`toki-tabs/update-buffer-list'.")

(defvar toki-tabs/buffer-list-changed nil
  "Whether buffer list has changed since last `toki-tabs/update-buffer-list'.")

(defun toki-tabs/buffer-list-changed ()
  "Notify toki-tabs that buffer list is updated."
  (setq toki-tabs/buffer-list-changed t))

(defvar toki-tabs/inhibit-resort-commands
  '(toki-tabs-previous toki-tabs-next)
  "Commands that shouldn't trigger resort.")

(defun toki-tabs/update-buffer-list ()
  "Update internal data when appropriate."
  (unless (window-minibuffer-p)
    (let ((current-buffer (current-buffer))
          non-hidden-bufs)
      ;; We don't update when the buffer list is not changed, or the current
      ;; buffer is not changed & no non-hidden buffers is killed/added.
      (unless (or (not toki-tabs/buffer-list-changed)
                  (and (eq current-buffer toki-tabs/last-active-buffer)
                       (cl-every #'buffer-live-p
                                 toki-tabs/sorted-buffer-list)
                       (progn (setq non-hidden-bufs
                                    (cl-remove-if-not #'toki-tabs/buffer-group
                                                      (buffer-list)))
                              (eq (length non-hidden-bufs)
                                  (length toki-tabs/sorted-buffer-list)))))
        (unless (member this-command toki-tabs/inhibit-resort-commands)
          (setq non-hidden-bufs (or non-hidden-bufs
                                    (cl-remove-if-not #'toki-tabs/buffer-group
                                                      (buffer-list))))
          (setq toki-tabs/sorted-buffer-list
                (sort non-hidden-bufs #'toki-tabs/buffer-freq-higher-p)))
        (run-hooks 'toki-tabs-update-hook)
        (setq toki-tabs/buffer-list-changed nil)
        (setq toki-tabs/last-active-buffer (current-buffer))))))

;;;;; UI

(defvar toki-tabs/update-buffer-list-timer nil
  "Idle timer to update buffer.")

(defun toki-tabs/truncated-buffer-name (buf)
  "Get the truncated name of BUF.
The buffer name may be truncated according to
`toki-tabs-buffer-name-max-length'."
  (if toki-tabs-buffer-name-max-length
      (let ((buf-name (buffer-name buf))
            name ext shrink-length)
        (setq shrink-length (- (length buf-name)
                               toki-tabs-buffer-name-max-length))
        (if (> shrink-length 0)
            (progn
              (if (and (string-match (rx "." (* (not ".")) line-end) buf-name)
                       (not (eq 0 (match-beginning 0))))
                  (setq name (substring buf-name 0 (match-beginning 0))
                        ext (substring buf-name (1+ (match-beginning 0))))
                (setq name buf-name
                      ext ""))
              (setq name
                    (substring name 0
                               (max (- (length name)
                                       ;; We've saved a char by not including
                                       ;; the dot before the extension, so we
                                       ;; minus 1 from `shrink-length'.
                                       (1- shrink-length)
                                       (length toki-tabs-ellipsis))
                                    0)))
              (concat name toki-tabs-ellipsis ext))
          buf-name))
    (buffer-name buf)))

(defun toki-tabs-visible-tabs-and-remain-num ()
  "Return the visible tabs and number of remaining tabs in a cons cell.
When the current buffer is a hidden buffer, return nil."
  (let* ((buf (current-buffer))
         (group (toki-tabs/buffer-group buf))
         (counter 0)
         tabs)
      (when group
        (dolist (b toki-tabs/sorted-buffer-list)
          (when (equal (toki-tabs/buffer-group b) group)
            (if (< (length tabs) toki-tabs-visible-buffer-limit)
                (push b tabs)
              (cl-incf counter))))
        (cons (nreverse tabs)
              counter))))

(defun toki-tabs-visible-tabs ()
  "Return the visible tabs."
  (let* ((buf (current-buffer))
         (group (toki-tabs/buffer-group buf))
         tabs)
      (when group
        (cl-dolist (b toki-tabs/sorted-buffer-list)
          (when (equal (toki-tabs/buffer-group b) group)
            (push b tabs)
            (when (= (length tabs) toki-tabs-visible-buffer-limit)
              (cl-return))))
        (nreverse tabs))))

(defun toki-tabs-string ()
  "Return a string that shows the tabs for current buffer.
Possible ways of using this string is to show it in the mode line
or header line, see `mode-line-format' and `header-line-format'
for instructions.  See \"site-lisp/toki-modeline.el\" for
example.

The string may look like

    tab1 | tab2 | 2+..tab5

\"tab1\" and \"tab2\" are the actual tabs, \"2+\" means there are
2 more buffers in the current group that's not shown.  When the
current buffer is one of these invisible buffers (\"tab5\" in
this case), it is shown after the \"1+\" part.

Notice that though \"tab5\" is shown to indicate the current
buffer, technically it's not in the tabs, and is still considered
\"invisible\" by `toki-tabs-kill-invisible-buffers-in-group'.

Current and non-active buffers are distinguished by faces."
  (let* ((current-buf (current-buffer))
         (tabs-and-remain (toki-tabs-visible-tabs-and-remain-num))
         (tabs (car tabs-and-remain))
         (tab-visible-p (memq current-buf tabs))
         (num (cdr tabs-and-remain))
         (rest (unless (or (eq num 0) (null num))
                 (propertize (concat "+" (number-to-string num) "..")
                             'face 'toki-tabs-rest-face)))
         (get-string (lambda (buf)
                       (if (eq buf current-buf)
                           (propertize (toki-tabs/truncated-buffer-name buf)
                                       'face 'toki-tabs-current-tab-face)
                         (propertize (toki-tabs/truncated-buffer-name buf)
                                     'face 'toki-tabs-inactive-tab-face))))
         (separator (propertize toki-tabs-separator
                                'face 'toki-tabs-separator-face)))
    (when (and rest (not tab-visible-p))
      (setq rest (concat rest
                         (propertize (toki-tabs/truncated-buffer-name
                                      current-buf)
                                     'face 'toki-tabs-current-tab-face))))
    (if tabs
        (string-join (nconc (mapcar get-string tabs) (when rest (list rest)))
                     separator)
      (propertize (toki-tabs/truncated-buffer-name current-buf)
                  'face 'toki-tabs-current-tab-face))))

;;;; Commands

(defun toki-tabs-previous ()
  "Switch to the previous tab.
When the current buffer is the first tab, or not in the tabs,
switch to the last tab."
  (interactive)
  (let* ((buf (current-buffer))
         (tabs (toki-tabs-visible-tabs))
         (idx (cl-position buf tabs :test #'eq)))
    (cond
     ((or (null idx) (eq idx 0))
      (switch-to-buffer (car (last tabs))))
     (t
      (switch-to-buffer (nth (1- idx) tabs))))))

(defun toki-tabs-next ()
  "Switch to the next tab.
When the current buffer is the last tab, or not in the tabs,
switch to the first tab."
  (interactive)
  (let* ((buf (current-buffer))
         (tabs (toki-tabs-visible-tabs))
         (idx (cl-position buf tabs :test #'eq)))
    (cond
     ((or (null idx) (eq idx (1- (length tabs))))
      (switch-to-buffer (car tabs)))
     (t
      (switch-to-buffer (nth (1+ idx) tabs))))))

(defun toki-tabs-kill-invisible-buffers-in-group ()
  "Kill all buffers that's not in the tabs in current group.
Notice when the current buffer is not in the tabs, though it may
still be shown after the \"n+\" part in the tabs, it will be
killed."
  (interactive)
  (when-let ((group (toki-tabs/buffer-group (current-buffer)))
             (tabs (toki-tabs-visible-tabs)))
    (dolist (b (buffer-list))
      (when (and (equal (toki-tabs/buffer-group b) group)
                 (not (memq b tabs)))
        (kill-buffer b)))))

(defun toki-tabs-kill-buffers-in-group ()
  "Kill all buffers in current group."
  (interactive)
  (when-let ((group (toki-tabs/buffer-group (current-buffer))))
    (dolist (b (buffer-list))
      (when (equal (toki-tabs/buffer-group b) group)
        (kill-buffer b)))))

(defun toki-tabs-switch-to-buffer-in-group ()
  "Switch to a buffer in current group."
  (interactive)
  (when-let ((group (toki-tabs/buffer-group (current-buffer))))
    (let (bufs collection)
      (dolist (b (buffer-list))
        (when (equal (toki-tabs/buffer-group b) group)
          (push b bufs)))
      (setq bufs (mapcar #'buffer-name (nreverse bufs)))
      (setq collection
            (lambda (str pred action)
              (if (eq action 'metadata)
                  '(metadata
                    (category . buffer)
                    (cycle-sort-function . identity)
                    (display-sort-function . identity))
                (complete-with-action action bufs str pred))))
      (switch-to-buffer
       (completing-read "Switch to: " collection nil t)))))

;;;###autoload
(define-minor-mode toki-tabs-mode
  "Minor mode for maintaining data for showing tabs.
This mode doesn't offer an UI for showing the tabs.  See
`toki-tabs-update-hook' and `toki-tabs-string' to know how to
plug-in an UI for tabs."
  :global t
  (if toki-tabs-mode
      (progn
        (toki-tabs-start-count-freq)
        (toki-tabs/buffer-list-changed)
        (toki-tabs/update-buffer-list)
        (add-hook 'buffer-list-update-hook
                  #'toki-tabs/buffer-list-changed)
        (add-hook 'post-command-hook
                  #'toki-tabs/update-buffer-list)
        ;; We do this in case buffer list is changed asynchronously.
        (setq toki-tabs/update-buffer-list-timer
              (run-with-idle-timer 0.1 'repeat #'toki-tabs/update-buffer-list)))
    (toki-tabs-stop-count-freq)
    (remove-hook 'buffer-list-update-hook
                 #'toki-tabs/buffer-list-changed)
    (remove-hook 'post-command-hook
                 #'toki-tabs/update-buffer-list)
    (cancel-timer toki-tabs/update-buffer-list-timer)
    (setq toki-tabs/update-buffer-list-timer nil)))

(provide 'toki-tabs)

;;; toki-tabs.el ends here
