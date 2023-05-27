;;; toki-window.el --- Window management helpers -*- lexical-binding: t -*-

;; Copyright (C) 2022 Hao Wang
;; License: GPL v3, or (at your option) any later version

;;; Commentary:

;;; Code:

;; To see the outline of this file, run `outline-minor-mode', then
;; `outline-hide-body'.  Another way is to run `occur' with the query:
;; ^;;;;* \|^(

;;;; Libraries

;;;; User options

(defvar toki-window-keys '(?j ?f ?k ?d ?l ?s))

(defface toki-window-key-face
  '((t :inherit query-replace :bold t :height 1.6))
  "Face used for window key display.")

;;;; Internals

(defvar toki-window/overlays nil)

(defun toki-window/paint-window (&optional window)
  (unless (>= (length toki-window/overlays) (length toki-window-keys))
    (with-selected-window (or window (selected-window))
      (let* ((pos (window-start))
             (width (window-hscroll))
             (ol (make-overlay pos pos))
             (key (nth (length toki-window/overlays) toki-window-keys))
             (str (concat (make-string width ?\s)
                          (propertize (string key) 'face 'toki-window-key-face)
                          "\n")))
        (push ol toki-window/overlays)
        (overlay-put ol 'window (selected-window))
        (overlay-put ol 'before-string str)))))

(defun toki-window/paint-windows (windows)
  (dolist (w (cl-subseq windows 0
                        (min (length toki-window-keys)
                             (length windows))))
    (toki-window/paint-window w)))

(defun toki-window/erase ()
  (let (ol)
    (while (setq ol (pop toki-window/overlays))
      (delete-overlay ol))))

;;;; API

(defun toki-window-select-other ()
  (unwind-protect
      (let ((windows (window-list nil 'never)))
        (setq windows (cl-delete-if
                       (lambda (w) (window-parameter w 'no-other-window))
                       windows))
        (pop windows)
        (cond
         ((null windows) nil)
         ((eq (length windows) 1) (car windows))
         (t (toki-window/paint-windows windows)
            (when-let* ((key (read-key "Window:"))
                        (idx (cl-position key toki-window-keys :test #'eq)))
              (nth idx windows)))))
    (toki-window/erase)))

(defun toki-window-select ()
  (unwind-protect
      (let ((windows (window-list nil 'never)))
        (cond
         ((eq (length windows) 1) (car windows))
         ((eq (length windows) 2) (cadr windows))
         (t (toki-window/paint-windows windows)
            (when-let* ((key (read-key "Window:"))
                        (idx (cl-position key toki-window-keys :test #'eq)))
              (nth idx windows)))))
    (toki-window/erase)))

;;;; Commands

;;;###autoload
(defun toki-window-other-window ()
  (interactive)
  (when-let ((w (toki-window-select-other)))
    (select-window w)))

;;;###autoload
(defun toki-window-kill-other-window ()
  (interactive)
  (when-let ((w (toki-window-select-other)))
    (delete-window w)))

;;;###autoload
(defun toki-window-send-buffer-to-window ()
  (interactive)
  (when-let ((b (current-buffer))
             (w (toki-window-select-other)))
    (with-selected-window w
      (switch-to-buffer b))
    (previous-buffer)))

;;;###autoload
(defun toki-window-copy-buffer-to-window ()
  (interactive)
  (when-let ((b (current-buffer))
             (w (toki-window-select-other)))
    (with-selected-window w
      (switch-to-buffer b))))

;;;###autoload
(defun toki-window-kill-other-buffer ()
  "Kill buffer in another window."
  (interactive)
  (when-let ((w (toki-window-select-other)))
    (with-selected-window w
      (kill-current-buffer))))

;;;###autoload
(defun toki-window-prev-buffer-in-another-window ()
  "Go to previous buffer in another window."
  (interactive)
  (when-let ((w (toki-window-select-other)))
    (with-selected-window w
      (previous-buffer))))

;;;###autoload
(defun toki-window-select-minibuffer ()
  (interactive)
  (when-let ((w (active-minibuffer-window)))
    (select-window w)))

(provide 'toki-window)

;;; toki-window.el ends here
