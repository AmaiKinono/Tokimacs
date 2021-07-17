;;; toki-modeline.el --- A simple modeline -*- lexical-binding: t -*-

;; Copyright (C) 2021 Hao Wang
;; License: GPL v3, or (at your option) any later version

;;; Commentary:

;; toki-modeline offers a simple modeline to use with mini-modeline.

;;; Code:

;; To see the outline of this file, run `outline-minor-mode', then
;; `outline-hide-body'.  Another way is to run `occur' with the query:
;; ^;;;;* \|^(

;;;; Libraries

(require 'battery)
(require 'subr-x)

;;;; User options

(defvar toki-modeline-path-full-level 2
  "Levels of dirs to shown in full name.")

(defvar toki-modeline-path-truncate-level 2
  "Levels of dirs to shown in truncated name.")

(defvar toki-modeline-path-truncated-length 1
  "How many letters to leave after truncation.")

(defface toki-modeline-time-face
  '((((background light))
     :foreground "#7c7c7c")
    (t
     :foreground "#939393"))
  "Face for time info.")

(defface toki-modeline-battery-normal-face
  '((((background light))
     :foreground "#2c8361")
    (t
     :foreground "#5e8b57"))
  "Face for battery info when power is normal.")

(defface toki-modeline-battery-low-face
  '((t :inherit 'warning))
  "Face for battery info when power is low.")

(defface toki-modeline-battery-critical-face
  '((t :inherit 'error))
  "Face for battery info when power is critical.")

(defface toki-modeline-location-face
  '((((background light))
     :foreground "#767676" :bold t)
    (t
     :foreground "#999999" :bold t))
  "Face for location.")

(defface toki-modeline-path-face
  '((((background light))
     :foreground "#ac00ac" :italic t)
    (t
     :foreground "#cd96cd" :italic t))
  "Face for file path.")

(defface toki-modeline-vc-face
  '((((background light))
     :foreground "#de0279" :bold t)
    (t
     :foreground "#ee6aa7" :bold t))
  "Face for VCS info.")

(defface toki-modeline-mode-face
  '((((background light))
     :foreground "#4a7600")
    (t
     :foreground "#96d21e"))
  "Face for major mode info.")

(defface toki-modeline-evil-face
  '((((background light))
     :foreground "#00aca9" :bold t)
    (t
     :foreground "#00ced1" :bold t))
  "Face for evil state.")

;;;; Helpers

(defun toki-modeline-pad (str)
  "When STR is non-empty, add padding to its right."
  (if (or (null str) (string-empty-p str))
      ""
    (concat str " ")))

(defun toki-modeline-shrink-dir (name)
  "Shrink NAME.
This is used in the path info."
  (let ((dot-num (if (string-match "^\\.+" name)
                     (length (match-string 0 name))
                   0)))
    (substring name 0 (min (length name)
                           (+ dot-num
                              toki-modeline-path-truncated-length)))))

;;;; Info functions

(defun toki-modeline-path ()
  "Return the path info."
  (if buffer-file-name
      (let* ((path (split-string
                    (abbreviate-file-name buffer-file-name)
                    "/" 'omit-nulls))
             ;; We don't count the filename in path-len
             (path-len (1- (length path)))
             (modp (if (buffer-modified-p) "*" ""))
             (shown-path nil)
             (full-level toki-modeline-path-full-level)
             (trunc-level toki-modeline-path-truncate-level))
        (when (> path-len toki-modeline-path-full-level)
          (push (string-join
                 (mapcar #'toki-modeline-shrink-dir
                         (cl-subseq path
                                    (max 0 (- path-len (+ full-level
                                                          trunc-level)))
                                    (- path-len full-level)))
                 "/")
                shown-path))
        (push (string-join
               (cl-subseq path
                          (max 0 (- path-len full-level)))
               "/")
              shown-path)
        (concat modp
                (if (<= path-len (+ full-level trunc-level))
                    "/"
                  ".../")
                (string-join (nreverse shown-path) "/")))
    (buffer-name)))

(defun toki-modeline-vc ()
  "Return the VCS info."
  (if (and vc-mode buffer-file-name)
      (progn
        (let* ((backend (vc-backend buffer-file-name))
               (state (vc-state buffer-file-name backend)))
          (concat
           (pcase state
             ((or 'up-to-date 'edited) "@")
             ((or 'removed 'conflict 'unregistered) "!")
             ((or 'needs-update 'needs-merge) "^")
             ('added "+")
             ((pred stringp) (concat state ":"))
             (_ "?"))
           (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2)))))
    ""))

(defun toki-modeline-evil ()
  "Return the evil state."
  (if (bound-and-true-p evil-mode)
      (let ((tag (evil-state-property evil-state :tag t)))
        (when (functionp tag)
          (setq tag (funcall tag)))
        (string-trim tag))
    ""))

(defun toki-modeline-battery ()
  "Return battery state."
  (let* ((status (funcall battery-status-function))
         (ac (battery-format "%L" status))
         (percentage (battery-format "%p%%%%" status))
         (remaining (battery-format "%t" status))
         (indicator (battery-format "%b" status))
         (face (pcase indicator
                 ("-" 'toki-modeline-battery-low-face)
                 ("!" 'toki-modeline-battery-critical-face)
                 (_ 'toki-modeline-battery-normal-face))))
    (propertize
     (if (string-prefix-p "AC" ac)
         (format "%s%s/%s" indicator percentage ac)
       (format "%s%s/%s" indicator percentage remaining))
     'face face)))

(defun toki-modeline-time ()
  "Return time info."
  (let* ((week (pcase (format-time-string "%u")
                 ("1" "Mon")
                 ("2" "Tue")
                 ("3" "Wed")
                 ("4" "Thu")
                 ("5" "Fri")
                 ("6" "Sat")
                 ("7" "Sun"))))
    (concat
     (format-time-string "[%m-%d ")
     week
     (format-time-string " %H:%M]"))))

;;;; Modeline formats

(defvar toki-modeline-main-format
  (list
   '(:eval (toki-modeline-pad
            (propertize "(%l:%c %p)" 'face 'toki-modeline-location-face)))
   '(:eval (toki-modeline-pad
            (propertize (toki-modeline-path) 'face 'toki-modeline-path-face)))
   '(:eval (toki-modeline-pad
            (propertize (toki-modeline-vc) 'face 'toki-modeline-vc-face)))
   '(:eval (if (bound-and-true-p evil-mode)
               (toki-modeline-pad
                (propertize mode-name 'face 'toki-modeline-mode-face))
             (propertize mode-name 'face 'toki-modeline-mode-face)))
   '(:eval (propertize (toki-modeline-evil) 'face 'toki-modeline-evil-face)))
  "A minimal modeline format that shows essential info.
Set `mini-modeline-r-format' to this to use it.")

(defvar toki-modeline-additional-format
  (list
   '(:eval (toki-modeline-pad
            (toki-modeline-battery)))
   '(:eval (propertize (toki-modeline-time) 'face 'toki-modeline-time-face)))
  "A modeline format showing battery and time info.
It's meant to be shown at the left side.  Set
`mini-modeline-l-format' to this to use it.")

(provide 'toki-modeline)

;;; toki-modeline.el ends here
