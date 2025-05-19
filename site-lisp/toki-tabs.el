;;; toki-tabs.el --- Tabs for frequently used buffers -*- lexical-binding: t -*-

;; Copyright (C) 2021 Hao Wang
;; License: GPL v3, or (at your option) any later version

;;; Commentary:

;; Groups buffers and show those in current group in descending order of used
;; frequency.
;;
;; The tabs look like:
;;
;;   tab1 | tab2 | 2+.. | tab5
;;
;; "tab1" and "tab2" are the actual tabs, "2+" means there are 2 more
;; buffers in the current group that's not shown.  When the current buffer is
;; one of these invisible buffers ("tab5" in this case), it is shown after
;; the "2+" part.
;;
;; Current and non-active buffers are distinguished by faces.
;;
;; This package doesn't provide a UI.  You can use `toki-tabs-tab-bar-format'
;; to display it in the tab-bar, which is recommended, or you can utilize the
;; `toki-tabs-string' variable or function in your own UI.
;;
;; TODO: `switch-buffer' command powered by sorted buffer list?

;;; Code:

;; To see the outline of this file, run `outline-minor-mode', then
;; `outline-hide-body'.  Another way is to run `occur' with the query:
;; ^;;;;* \|^(

;;;; Libraries

(require 'cl-lib)
(require 'tab-bar)
(require 'project)

;;;; User options

(defvar toki-tabs-count-freq-idle-time 1
  "Add used frequency after being idle for this much secs.")

(defvar toki-tabs-visible-buffer-limit 6
  "Max number of visible buffers in a group.
Nil means no limit.")

(defvar toki-tabs-ellipsis "…"
  "Ellipsis used when truncating long buffer names.")

(defvar toki-tabs-buffer-name-max-length 16
  "Max length of buffer name shown in a tab.
Nil means don't truncate filenames.")

(defvar toki-tabs-project-root-function
  (lambda ()
    (when-let ((project (project-current nil)))
      (if (fboundp #'project-root)
          (project-root project)
        (car (project-roots project)))))
  "A function that returns project root for current buffer.")

(defvar toki-tabs-separator "|"
  "A string used as separator between tabs.")

(defvar toki-tabs-update-hook nil
  "Hook to run when tabs need to be redisplayed.
You should customize this hook to run code that's needed to
update the UI.")

(defface toki-tabs-current-tab-face
  '((((background light))
     :background "#d5c9c0" :foreground "#282828")
    (t
     :background "#504945" :foreground "#fbf1c7"))
  "Face for current tab.")

(defface toki-tabs-inactive-tab-face
  '((((background light))
     :foreground "#665c54")
    (t
     :foreground "#bdae93"))
  "Face for inactive tabs.")

(defface toki-tabs-separator-face
  '((((background light))
     :foreground "#bdae93")
    (t
     :foreground "#665c54"))
  "Face for separator.")

(defface toki-tabs-rest-face
  '((t :italic t :bold nil))
  "Face for current tab.")

;;;; Internals

;;;;; Wrappers

(defun toki-tabs/current-buffer ()
  "Return current buffer, or the buffer before entering minibuffer."
  (window-buffer (minibuffer-selected-window)))

;;;;; Buffer group

(defvar-local toki-tabs/buffer-group nil)

(defun toki-tabs/buffer-group (&optional buffer)
  "Return the group name of BUFFER.
When BUFFER is nil, use current buffer."
  (let* ((buffer (or buffer (toki-tabs/current-buffer)))
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
  "Update internal data when appropriate.
Returns non-nil if the buffer list is updated."
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

(defvar toki-tabs/update-buffer-list-timer-repeat nil
  "Idle timer to update buffer.
This repeats in the background.")

(defvar toki-tabs/update-buffer-list-timer-once nil
  "Idle timer to update buffer.
This is runned once when entering the idle state.")

(cl-defstruct (toki-tabs/tab
               (:constructor nil)
               (:constructor toki-tabs/make-tab))
  (buffer
   nil
   :documentation
   "The buffer of the tab."
   :type "buffer")
  (name
   nil
   :documentation
   "The displayed name of the buffer, propertized with face."
   :type "string")
  (current
   nil
   :documentation
   "Whether it's the current buffer."
   :type "boolean"))

(defun toki-tabs/separator-str ()
  "Return a separator string with proper face."
  (propertize toki-tabs-separator 'face 'toki-tabs-separator-face))

(defun toki-tabs/buffer-tab (buf)
  "Create `toki-tabs/tab' for BUF."
  (let ((current (eq buf (window-buffer (minibuffer-selected-window))))
        (name (toki-tabs/buffer-tab-name buf)))
    (toki-tabs/make-tab
     :buffer buf
     :name (propertize name 'face
                       (if current
                           'toki-tabs-current-tab-face
                         'toki-tabs-inactive-tab-face))
     :current current)))

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

(defun toki-tabs/buffer-tab-name (buf)
  "Return tab name of buffer BUF."
  (let ((name (toki-tabs/truncated-buffer-name buf)))
    (with-current-buffer buf
      (cond
       ((derived-mode-p 'dired-mode) (format "@%s" name))
       (t name)))))

(defun toki-tabs/visible-tabs-and-remain-num ()
  "Return the visible tabs and number of remaining tabs in a cons cell.
When the current buffer is a hidden buffer, return nil."
  (let* ((buf (toki-tabs/current-buffer))
         (group (toki-tabs/buffer-group buf))
         (counter 0)
         tabs)
    (when group
      (dolist (b toki-tabs/sorted-buffer-list)
        (when (equal (toki-tabs/buffer-group b) group)
          (if (or (null toki-tabs-visible-buffer-limit)
                  (< (length tabs) toki-tabs-visible-buffer-limit))
              (push b tabs)
            (cl-incf counter))))
      (cons (mapcar #'toki-tabs/buffer-tab (nreverse tabs))
            counter))))

(defun toki-tabs/visible-bufs ()
  "Return the buffers of visible tabs."
  (let* ((buf (toki-tabs/current-buffer))
         (group (toki-tabs/buffer-group buf))
         tabs)
    (when group
      (cl-dolist (b toki-tabs/sorted-buffer-list)
        (when (equal (toki-tabs/buffer-group b) group)
          (push b tabs)
          (when (and toki-tabs-visible-buffer-limit
                     (= (length tabs) toki-tabs-visible-buffer-limit))
            (cl-return))))
      (nreverse tabs))))

(defun toki-tabs/update-buffer-list-and-refresh ()
  "Update buffer list and redisplay the mode line."
  (and (toki-tabs/update-buffer-list)
       (force-mode-line-update)))

;;;;; Tab-bar integration helpers

(defun toki-tabs/tab-bar-menu-item (tab i)
  "Create menu item for use in tab-bar of TAB with index I."
  (list
   ;; I don't think this is a standard part of a menu-item, but tab-bar
   ;; requires it.
   (if (toki-tabs/tab-current tab) 'current-tab (intern (format "tab-%i" i)))
   'menu-item
   (toki-tabs/tab-name tab)
   (lambda () (interactive) (switch-to-buffer (toki-tabs/tab-buffer tab)))))

(defun toki-tabs/str-to-menu-item (str)
  "Create a placeholder menu item that can be used in tab-bar and shows STR."
  (list
   'placeholder
   'menu-item
   str
   #'ignore
   :enable nil))

(defun toki-tabs/tab-bar-invisible-tabs-menu-item (num)
  "Create a menu item that represents NUM invisible tabs."
  (when (and num (> num 0))
    (toki-tabs/str-to-menu-item
     (propertize (concat "+" (number-to-string num) "..")
                 'face 'toki-tabs-rest-face))))

;;;; APIs

(defvar toki-tabs-string nil
  "A string that shows the tabs for current buffer.
This is update by the function `toki-tabs-string', so if you use
this in your UI, make sure to call `toki-tabs-string' in
`toki-tabs-update-hook'.")

(defun toki-tabs-string ()
  "Return a string that shows the tabs for current buffer.
This also updates the variable `toki-tabs-string'.

Possible ways of using this string is to show it in the mode line
or header line, see `mode-line-format' and `header-line-format'.
Have an element like

  (:eval (toki-tabs-string))

will do the work.  If you have some cache mechanism over the mode
line that causes the tabs to not update instantly, customize
`toki-tabs-update-hook' to update the cache."
  (let* ((current-buf (window-buffer (minibuffer-selected-window)))
         (tabs-and-remain (toki-tabs/visible-tabs-and-remain-num))
         (tabs (car tabs-and-remain))
         (num (cdr tabs-and-remain))
         (buf-visible-p (cl-find current-buf tabs :key #'toki-tabs/tab-buffer))
         (rest (unless (or (eq num 0) (null num))
                 (propertize (concat "+" (number-to-string num) "..")
                             'face 'toki-tabs-rest-face))))
    ;; The current buffer may be unfrequently used (e.g. when switched to using
    ;; `switch-to-buffer') and therefore not in the visible tabs.
    (when (and rest (not buf-visible-p))
      (setq rest (concat rest
                         (propertize (toki-tabs/buffer-tab-name
                                      current-buf)
                                     'face 'toki-tabs-current-tab-face))))
    (setq toki-tabs-string
          (if tabs
              (string-join (nconc (mapcar #'toki-tabs/tab-name tabs)
                                  (when rest (list rest)))
                           (toki-tabs/separator-str))
            (propertize (toki-tabs/buffer-tab-name current-buf)
                        'face 'toki-tabs-current-tab-face)))))

(defun toki-tabs-tab-bar-format ()
  "A function that can be used in `tab-bar-format'."
  (let* ((current-buf (window-buffer (minibuffer-selected-window)))
         (tabs-and-remain (toki-tabs/visible-tabs-and-remain-num))
         (tabs (car tabs-and-remain))
         (num (cdr tabs-and-remain))
         (buf-visible-p (cl-find current-buf tabs :key #'toki-tabs/tab-buffer))
         (result (cl-loop for i from 1
                          for tab in tabs
                          collect (toki-tabs/tab-bar-menu-item tab i)))
         (sep (toki-tabs/str-to-menu-item (toki-tabs/separator-str))))
    (when-let ((invisible (toki-tabs/tab-bar-invisible-tabs-menu-item num)))
      (setq result (nconc result (list invisible))))
    (unless buf-visible-p
      (setq result (nconc result (list (toki-tabs/tab-bar-menu-item
                                        (toki-tabs/buffer-tab current-buf)
                                        (1+ (length tabs)))))))
    (cons (car result)
          (mapcan (lambda (x) (list sep x)) (cdr result)))))

(defun toki-tabs-groups ()
  "Return a list of all buffer groups.
The current group will be the first one."
  (let ((current-group (toki-tabs/buffer-group))
        groups)
    (cl-loop for b in toki-tabs/sorted-buffer-list
             for g = (toki-tabs/buffer-group b)
             do (unless (member g groups) (push g groups)))
    (cl-delete current-group groups)
    (setq groups (nreverse groups))
    (push current-group groups)
    groups))

(defun toki-tabs-read-group ()
  "Read tab group name."
  (let* ((groups (toki-tabs-groups))
         (coll (lambda (str pred action)
                 (if (eq action 'metadata)
                     '(metadata (display-sort-function . identity)
                                (cycle-sort-function . identity))
                   (complete-with-action action groups str pred)))))
    (completing-read "Tab group: " coll)))

;;;; Commands

(defun toki-tabs-previous ()
  "Switch to the previous tab.
When the current buffer is the first tab, or not in the tabs,
switch to the last tab."
  (interactive)
  (let* ((buf (toki-tabs/current-buffer))
         (bufs (toki-tabs/visible-bufs))
         (idx (cl-position buf bufs :test #'eq)))
    (cond
     ((or (null idx) (eq idx 0))
      (switch-to-buffer (car (last bufs))))
     (t
      (switch-to-buffer (nth (1- idx) bufs))))))

(defun toki-tabs-next ()
  "Switch to the next tab.
When the current buffer is the last tab, or not in the tabs,
switch to the first tab."
  (interactive)
  (let* ((buf (toki-tabs/current-buffer))
         (bufs (toki-tabs/visible-bufs))
         (idx (cl-position buf bufs :test #'eq)))
    (cond
     ((or (null idx) (eq idx (1- (length bufs))))
      (switch-to-buffer (car bufs)))
     (t
      (switch-to-buffer (nth (1+ idx) bufs))))))

(defun toki-tabs-kill-invisible-buffers-in-group ()
  "Kill all buffers that's not in the shown tabs in current group."
  (interactive)
  (when-let ((current-buf (toki-tabs/current-buffer))
             (group (toki-tabs/buffer-group current-buf))
             (bufs (toki-tabs/visible-bufs)))
    (when (y-or-n-p (format "Really kill invisible buffers in group %s?"
                            group))
      (dolist (b (buffer-list))
        (when (and (equal (toki-tabs/buffer-group b) group)
                   (not (or (eq b current-buf) (memq b bufs))))
          (kill-buffer b))))))

(defun toki-tabs-kill-buffers-in-other-group (&optional current)
  "Pick a group and kill buffers in that group.
When CURRENT is non-nil, use current group."
  (interactive)
  (when-let ((group (if current (toki-tabs/buffer-group (toki-tabs/current-buffer))
                      (toki-tabs-read-group))))
    (when (y-or-n-p (format "Really kill invisible buffers in group %s?"
                            group))
      (dolist (b (buffer-list))
        (when (equal (toki-tabs/buffer-group b) group)
          (kill-buffer b))))))

(defun toki-tabs-kill-buffers-in-group ()
  "Kill all buffers in current group."
  (interactive)
  (toki-tabs-kill-buffers-in-other-group 'current))

;;;###autoload
(defun toki-tabs-kill-buffers-in-directory ()
  "Pick a directory and kill all buffers in it."
  (interactive)
  (let ((dir (read-directory-name "Dir: " nil nil 'mustmatch)))
    (when (y-or-n-p (format "Really kill all buffers in dir %s?" dir))
      (dolist (buf (buffer-list))
        (when (file-in-directory-p
               (or (buffer-file-name buf)
                   (with-current-buffer buf default-directory))
               dir)
          (kill-buffer buf))))))

(defun toki-tabs-switch-to-buffer-in-other-group (&optional current)
  "Pick a group and switch to a buffer in that group.
When CURRENT is non-nil, use current group."
  (interactive)
  (when-let ((group (if current (toki-tabs/buffer-group (toki-tabs/current-buffer))
                      (toki-tabs-read-group))))
    (let (bufs collection)
      (dolist (b toki-tabs/sorted-buffer-list)
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

(defun toki-tabs-switch-to-buffer-in-group ()
  "Switch to a buffer in current group."
  (interactive)
  (toki-tabs-switch-to-buffer-in-other-group 'current))

;;;###autoload
(define-minor-mode toki-tabs-mode
  "Minor mode for maintaining data for showing tabs.
This mode doesn't offer an UI for showing the tabs.  See
`toki-tabs-tab-bar-format' and function `toki-tabs-string' to
know how to plug-in an UI for tabs."
  :global t
  (if toki-tabs-mode
      (progn
        (toki-tabs-start-count-freq)
        (toki-tabs/buffer-list-changed)
        (toki-tabs/update-buffer-list)
        (add-hook 'buffer-list-update-hook
                  #'toki-tabs/buffer-list-changed)
        (setq toki-tabs/update-buffer-list-timer-once
              (run-with-idle-timer 0.05 nil #'toki-tabs/update-buffer-list-and-refresh))
        ;; We do this in case buffer list is changed asynchronously.
        (setq toki-tabs/update-buffer-list-timer-repeat
              (run-with-idle-timer 1 'repeat #'toki-tabs/update-buffer-list-and-refresh)))
    (toki-tabs-stop-count-freq)
    (remove-hook 'buffer-list-update-hook
                 #'toki-tabs/buffer-list-changed)
    (cancel-timer toki-tabs/update-buffer-list-timer-once)
    (cancel-timer toki-tabs/update-buffer-list-timer-repeat)
    (setq toki-tabs/update-buffer-list-timer-once nil
          toki-tabs/update-buffer-list-timer-repeat nil)))

(provide 'toki-tabs)

;;; toki-tabs.el ends here
