;;; toki-modeline.el --- A simple modeline -*- lexical-binding: t -*-

;; Copyright (C) 2021 Hao Wang
;; License: GPL v3, or (at your option) any later version

;;; Commentary:

;; toki-modeline offers a simple & elegant modeline.

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

(defvar toki-modeline-update-interval 0.2
  "Update interval in secs.
Set to nil to update on every redisplay.")

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

(defface toki-modeline-winum-face
  '((t :inherit 'font-lock-function-name-face))
  "Face for winum info.")

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

(defun toki-modeline/right-align-space (string)
  "Return a propertized space that aligns STRING to the right."
  (setq string (replace-regexp-in-string "%%" "%" string))
  (setq string (propertize string 'face 'mode-line))
  (let ((len (string-width string)))
    (if (display-graphic-p)
        (let ((height (face-attribute 'mode-line :height))
              ratio)
          (cond
           ((floatp height)
            (setq ratio height))
           ((integerp height)
            (setq ratio (/ (float ratio)
                           (face-attribute 'default :height))))
           ;; HEIGHT can also be a function, for now I don't deal with it.
           )
          (when ratio (setq len (ceiling (* len ratio)))))
      (setq len (1+ len)))
    (propertize " " 'display `((space :align-to (- right ,len))))))

;;;; Info functions

(defun toki-modeline-location ()
  "Return the location info."
  (let ((percent (format-mode-line "%p")))
    (setq percent
          (pcase percent
            ("All" "All%")
            ("Top" "0%")
            ("Bottom" "100%")
            (val val)))
    (concat (format-mode-line "%l:%c") " " percent "%%%")))

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
  (let* ((file buffer-file-name)
         (dir (unless file default-directory))
         ;; NOTE: `vc-backend' doesn't accept dir, while
         ;; `vc-responsible-backend' does. This also works for
         ;; non-registered files.
         (backend (ignore-errors
                    (vc-responsible-backend (or file dir))))
         state)
    (if backend
        (progn
          (when file (setq state (vc-state file backend)))
          ;; Initialize `vc-mode' variable for directory.
          (when dir (vc-mode-line dir backend))
          (concat
           (pcase state
             ((or 'up-to-date 'edited 'nil) "@")
             ((or 'removed 'conflict 'unregistered) "!")
             ((or 'needs-update 'needs-merge) "^")
             ('added "+")
             ((pred stringp) (concat state ":"))
             (_ "?"))
           (substring vc-mode (+ (if (eq backend 'Hg) 2 3) 2))))
      "")))

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

;;;; Internals

(defvar toki-modeline/selected-window nil)

;; NOTE: This doesn't deal with miniframes.
(defun toki-modeline/update-selected-window (frame)
  (unless (minibufferp)
    (setq toki-modeline/selected-window (frame-selected-window frame))))

(defvar-local toki-modeline/cache (make-hash-table :test #'eq))

(defvar toki-modeline/last-update nil)

;;;; Modeline formats

(defvar toki-modeline-main-format
  (list
   '(:eval (toki-modeline-pad
            (propertize (toki-modeline-location) 'face 'toki-modeline-location-face)))
   '(:eval (toki-modeline-pad
            (propertize (toki-modeline-path) 'face 'toki-modeline-path-face)))
   '(:eval (toki-modeline-pad
            (propertize (toki-modeline-vc) 'face 'toki-modeline-vc-face)))
   '(:eval (toki-modeline-pad
            (propertize mode-name 'face 'toki-modeline-mode-face)))
   '(:eval (propertize (toki-modeline-evil) 'face 'toki-modeline-evil-face)))
  "Modeline format for active window.")

(defvar toki-modeline-left-format
  (when (bound-and-true-p winum-mode)
    (list '(:eval (propertize (concat " #" (format "%s" (winum-get-number)))
                              'face 'toki-modeline-winum-face))))
  "Modeline format for displaying in the left.")

;; NOTE: Currently unused.
(defvar toki-modeline-additional-format
  (list
   '(:eval (toki-modeline-pad
            (toki-modeline-battery)))
   '(:eval (propertize (toki-modeline-time) 'face 'toki-modeline-time-face)))
  "Modeline format showing battery and time info.")

(defun toki-modeline-compute (&optional force)
  (let ((format)
        (active-window-p (eq (selected-window) toki-modeline/selected-window)))
    (setq format
          (if (or force
                  (null toki-modeline-update-interval)
                  (null (gethash (selected-window) toki-modeline/cache))
                  (> (float-time (time-since toki-modeline/last-update))
                     toki-modeline-update-interval))
              (let* ((l (format-mode-line toki-modeline-left-format))
                     (r (string-trim (format-mode-line toki-modeline-main-format)))
                     (pad (toki-modeline/right-align-space r))
                     (format (list l pad r)))
                (puthash (selected-window) format toki-modeline/cache)
                (setq toki-modeline/last-update (current-time))
                format)
            (gethash (selected-window) toki-modeline/cache)))
    (if active-window-p
        format
      (mapcar (lambda (s) (propertize s 'face nil)) format))))

(defun toki-modeline-setup-hooks ()
  (add-hook 'window-selection-change-functions
            #'toki-modeline/update-selected-window)
  (toki-modeline/update-selected-window (selected-frame)))

(provide 'toki-modeline)

;;; toki-modeline.el ends here
