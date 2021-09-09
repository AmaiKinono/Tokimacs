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

(defvar toki-tabs-project-root-function
  (lambda ()
    (when-let ((project (project-current nil)))
      (expand-file-name (cdr project))))
  "A function that returns project root for current buffer.")

(defvar toki-tabs-separator "|"
  "A string used as separator between tabs.")

(defvar toki-tabs-update-hook nil
  "Hook to run when tabs need to be redisplayed.")

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

(defface toki-tabs-remainder-face
  '((t :inherit 'default))
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
     ((setq group (or (buffer-local-value 'toki-tabs/buffer-group buffer)
                      (with-current-buffer buffer
                        (setq toki-tabs/buffer-group
                              (funcall toki-tabs-project-root-function)))))
      group)
     ((eq (aref name 0) ?*) "*Common*")
     (t "*Others*"))))

;;;;; Frequency

(defvar toki-tabs/timer nil
  "Timer used for count buffer used frequency.")

(defvar-local toki-tabs/buffer-freq 0
  "Used frequency of current buffer.")

(defun toki-tabs/buffer-freq (buf)
  (or (buffer-local-value 'toki-tabs/buffer-freq buf)
      0))

(defun toki-tabs/increase-buffer-freq ()
  (cl-incf toki-tabs/buffer-freq))

(defun toki-tabs/buffer-freq-higher-p (buf1 buf2)
  "Return t if the used frequency of BUF1 is higher than BUF2."
  (> (toki-tabs/buffer-freq buf1)
     (toki-tabs/buffer-freq buf2)))

(defun toki-tabs/insert-buf (buf bufs)
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
  (setq toki-tabs/timer
        (run-with-idle-timer toki-tabs-count-freq-idle-time
                             t #'toki-tabs/increase-buffer-freq)))

(defun toki-tabs-stop-count-freq ()
  (cancel-timer toki-tabs/timer)
  (setq toki-tabs/timer nil))

;;;;; Sorted buffer list

(defvar toki-tabs/sorted-buffer-list nil)

(defvar toki-tabs/last-active-buffer nil)

(defvar toki-tabs/inhibit-resort nil)

(defvar toki-tabs/exit-from-minibuffer nil)

(defun toki-tabs/update-buffer-list ()
  (let ((minibufferp (minibufferp))
        (current-buffer (current-buffer)))
    (if (or toki-tabs/inhibit-resort
            toki-tabs/exit-from-minibuffer
            (window-minibuffer-p)
            ;; We don't resort when the current buffer is not changed, and non
            ;; of the non-hidden buffers is killed.
            (and (eq current-buffer toki-tabs/last-active-buffer)
                 (not (cl-some #'buffer-live-p toki-tabs/sorted-buffer-list))))
        (unless (or minibufferp
                    (eq current-buffer toki-tabs/last-active-buffer))
          (run-hooks 'toki-tabs-update-hook))
      (let ((bufs (buffer-list)))
        (setq bufs (cl-remove-if-not #'toki-tabs/buffer-group bufs))
        (setq bufs (sort bufs #'toki-tabs/buffer-freq-higher-p))
        (setq toki-tabs/sorted-buffer-list bufs)
        (run-hooks 'toki-tabs-update-hook)))
    (unless minibufferp
      (setq toki-tabs/last-active-buffer (current-buffer)))))

;;;;; UI

(defun toki-tabs-visible-tabs-and-remain-num ()
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
  (let* ((current-buf (current-buffer))
         (tabs-and-remain (toki-tabs-visible-tabs-and-remain-num))
         (tabs (car tabs-and-remain))
         (num (cdr tabs-and-remain))
         (num (if (eq num 0)
                  nil
                (list (propertize (concat "+" (number-to-string num))
                                  'face 'toki-tabs-remainder-face))))
         (get-string (lambda (buf)
                       (if (eq buf current-buf)
                           (propertize (buffer-name buf)
                                       'face 'toki-tabs-current-tab-face)
                         (propertize (buffer-name buf)
                                     'face 'toki-tabs-inactive-tab-face))))
         (separator (propertize toki-tabs-separator
                                'face 'toki-tabs-separator-face)))
    (if tabs
        (string-join (nconc (mapcar get-string tabs) num) separator)
      "")))

;;;; Commands

(defun toki-tabs-previous ()
  (interactive)
  (let* ((buf (current-buffer))
         (tabs (toki-tabs-visible-tabs))
         (idx (cl-position buf tabs :test #'eq))
         (toki-tabs/inhibit-resort t))
    (cond
     ((or (null idx) (eq idx 0))
      (switch-to-buffer (car (last tabs))))
     (t
      (switch-to-buffer (nth (1- idx) tabs))))))

(defun toki-tabs-next ()
  (interactive)
  (let* ((buf (current-buffer))
         (tabs (toki-tabs-visible-tabs))
         (idx (cl-position buf tabs :test #'eq))
         (toki-tabs/inhibit-resort t))
    (cond
     ((or (null idx) (eq idx (1- (length tabs))))
      (switch-to-buffer (car tabs)))
     (t
      (switch-to-buffer (nth (1+ idx) tabs))))))

(defun toki-tabs-kill-invisible-buffers-in-group ()
  (interactive)
  (when-let ((group (toki-tabs/buffer-group (current-buffer)))
             (tabs (toki-tabs-visible-tabs)))
    (dolist (b (buffer-list))
      (when (and (equal (toki-tabs/buffer-group b) group)
                 (not (memq b tabs)))
        (kill-buffer b)))))

(defun toki-tabs-kill-buffers-in-group ()
  (interactive)
  (when-let ((group (toki-tabs/buffer-group (current-buffer))))
    (dolist (b (buffer-list))
      (when (equal (toki-tabs/buffer-group b) group)
        (kill-buffer b)))))

(defun toki-tabs-switch-to-buffer-in-group ()
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
  "Minor mode for tabs."
  :global t
  (if toki-tabs-mode
      (progn
        (toki-tabs-start-count-freq)
        (toki-tabs/update-buffer-list)
        (add-hook 'buffer-list-update-hook
                  #'toki-tabs/update-buffer-list))
    (toki-tabs-stop-count-freq)
    (remove-hook 'buffer-list-update-hook
                 #'toki-tabs/update-buffer-list)))

(provide 'toki-tabs)

;;; toki-tabs.el ends here
