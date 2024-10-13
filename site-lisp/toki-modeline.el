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

(defvar toki-modeline-update-after-idle-time 0.3
  "The time to wait after idle to update modeline.")

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

(defface toki-modeline-view-mode-face
  '((((background light))
     :foreground "#00aca9" :bold t)
    (t
     :foreground "#00ced1" :bold t))
  "Face for view mode indicator.")

;;;; Helpers

(defun toki-modeline-pad (str)
  "When STR is non-empty, add padding to its right."
  (if (or (null str) (string-empty-p str))
      ""
    (concat str " ")))

;; This is modified from `string-replace' in subr.el
(defun toki-modeline/string-replace-keep-properties (from to str)
  "Replace FROM with TO in STR, keep text properties of replaced text."
  (declare (pure t) (side-effect-free t))
  (when (equal from "")
    (signal 'wrong-length-argument '(0)))
  (let ((start 0)
        (result nil)
        pos props)
    (while (setq pos (string-search from str start))
      (unless (= start pos)
        (push (substring str start pos) result))
      (setq props (text-properties-at pos str))
      (set-text-properties 0 (length to) props to)
      (push to result)
      (setq start (+ pos (length from))))
    (if (null result)
        str
      (unless (= start (length str))
        (push (substring str start) result))
      (apply #'concat (nreverse result)))))

(defun toki-modeline/escape (str)
  "Make STR show literally when used as a mode-line construct."
  (toki-modeline/string-replace-keep-properties "%" "%%" str))

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
    (concat (format-mode-line "%l:%c") " " percent)))

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

(defun toki-modeline--vc-file ()
  "Return VCS info for current file buffer.
Return nil if the file isn't version controlled."
  (when-let* ((file buffer-file-name)
              (backend (ignore-errors
                         (vc-responsible-backend file)))
              ;; `vc-mode' can actually be nil when `backend' is non-nil, e.g.,
              ;; in git commit message buffer.
              (branch (when vc-mode
                        (substring vc-mode (+ (if (eq backend 'Hg) 2 3)
                                              2)))))
    (concat
     (pcase (vc-state file backend)
       ((or 'up-to-date 'edited 'nil) "@")
       ((or 'removed 'conflict 'unregistered) "!")
       ((or 'needs-update 'needs-merge) "^")
       ('added "+")
       ((and (pred stringp) state) (concat state ":"))
       (_ "?"))
     branch)))

(defun toki-modeline--vc-dir ()
  "Return VCS info for current directory buffer.
Return nil if the dir isn't version controlled."
  (when-let* ((dir default-directory)
              (backend (ignore-errors
                         (vc-responsible-backend dir)))
              (infostr (vc-call-backend backend 'dir-extra-headers
                                        dir)))
    (when (string-match (rx line-start "Branch"
                            (* space) ":" (* space)
                            (group (+ not-newline))
                            line-end)
                        infostr)
      (concat "@" (substring-no-properties (match-string 1 infostr))))))

(defun toki-modeline-vc ()
  "Return the VCS info."
  (or (and buffer-file-name (toki-modeline--vc-file))
      (toki-modeline--vc-dir)))

(defun toki-modeline-evil ()
  "Return the evil state."
  (when (bound-and-true-p evil-mode)
    (let ((tag (evil-state-property evil-state :tag t)))
      (when (functionp tag)
        (setq tag (funcall tag)))
      (string-trim tag))))

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

(defun toki-modeline-winum ()
  "Return window number info provided by `winum-mode'."
  (when (bound-and-true-p winum-mode)
    (propertize (concat "#" (format "%s" (winum-get-number)))
                'face 'toki-modeline-winum-face)))

(defun toki-modeline-tabs ()
  "Return tabs."
  (when (bound-and-true-p toki-tabs-mode)
    (toki-tabs-string)))

(defun toki-modeline-view-mode ()
  "Indicator of `view-mode'."
  (when view-mode
    (propertize "<V>" 'face 'toki-modeline-view-mode-face)))

(defun toki-modeline-mode-name ()
  "Return mode name."
  (format-mode-line mode-name))

;;;; Internals

(defvar toki-modeline/selected-window nil)

;; NOTE: This doesn't deal with miniframes.
(defun toki-modeline/update-selected-window (frame)
  "Record the last selected window in FRAME."
  (unless (minibufferp)
    (setq toki-modeline/selected-window (frame-selected-window frame))
    (toki-modeline/refresh-modeline)))

(defvar toki-modeline/cache (make-hash-table :test #'eq))

(defvar toki-modeline/trigger-timer nil
  "The timer for triggering first update after idle.")

(defvar toki-modeline/update-timer nil
  "The timer for updating while idle.")

;;;; Modeline formats

(defvar toki-modeline-right-format
  '((toki-modeline-location . toki-modeline-location-face)
    (toki-modeline-vc . toki-modeline-vc-face)
    (toki-modeline-mode-name . toki-modeline-mode-face)
    (toki-modeline-evil . toki-modeline-evil-face)
    toki-modeline-view-mode)
  "Modeline format (right part) for active window.")

(defvar toki-modeline-inactive-right-format
  '(toki-modeline-location
    toki-modeline-mode-name)
  "Modeline format (right part) for inactive window.")

(defvar toki-modeline-left-format
  '(" "
    (toki-modeline-winum . toki-modeline-winum-face)
    (toki-modeline-path . toki-modeline-path-face))
  "Modeline format (left part) for active window.")

(defvar toki-modeline-inactive-left-format
  '(" "
    toki-modeline-winum
    buffer-name)
  "Modeline format (left part) for inactive window.")

(defun toki-modeline/format-item (item)
  "Format a component ITEM in toki-modeline's mode line format."
  (cond
   ((symbolp item) (toki-modeline-pad (or (funcall item) "")))
   ((stringp item) item)
   ((consp item) (toki-modeline-pad (propertize (or (funcall (car item)) "")
                                                'face (cdr item))))
   (t (error "Unsupported type of item"))))

(defun toki-modeline/format (items)
  "Format ITEMS, a list of components."
  (string-join (mapcar #'toki-modeline/format-item items)))

(defun toki-modeline-compute (&optional force)
  "Compute the modeline content for current window.
The result is cached.  If FORCE is non-nil, recompute and rewrite
the cache."
  (let ((active-window-p (eq (selected-window) toki-modeline/selected-window))
        fmt)
    (setq fmt
          (if (or force
                  (null (gethash (selected-window) toki-modeline/cache)))
              (let* ((l (toki-modeline/format
                         (if active-window-p
                             toki-modeline-left-format
                           toki-modeline-inactive-left-format)))
                     (r (string-trim (toki-modeline/format
                                      (if active-window-p
                                          toki-modeline-right-format
                                        toki-modeline-inactive-right-format))))
                     (pad (toki-modeline/right-align-space r))
                     (fmt (list (toki-modeline/escape l)
                                pad
                                (toki-modeline/escape r))))
                (puthash (selected-window) fmt toki-modeline/cache)
                fmt)
            (gethash (selected-window) toki-modeline/cache)))
    (if active-window-p
        fmt
      (mapcar (lambda (s) (propertize s 'face nil)) fmt))))

(defun toki-modeline/refresh-modeline ()
  "Recompute and refresh modeline."
  (dolist (w (window-list))
    (with-selected-window w
      (toki-modeline-compute 'force)))
  (force-mode-line-update 'all))

(defun toki-modeline/refresh-active-modeline ()
  "Recompute and refresh active modeline."
  (toki-modeline-compute 'force)
  (force-mode-line-update))

(defun toki-modeline/cleanup-cache ()
  "Cleanup unused windows in toki-modeline cache."
  (dolist (w (hash-table-keys toki-modeline/cache))
    (unless (window-live-p w)
      (remhash w toki-modeline/cache))))

(defun toki-modeline-setup ()
  "Setup modeline."
  (add-hook 'window-selection-change-functions
            #'toki-modeline/update-selected-window)
  (add-hook 'window-configuration-change-hook
            #'toki-modeline/cleanup-cache)
  (toki-modeline/update-selected-window (selected-frame))
  (setq toki-modeline/trigger-timer
        (run-with-idle-timer toki-modeline-update-after-idle-time
                             t #'toki-modeline/refresh-modeline)))

(provide 'toki-modeline)

;;; toki-modeline.el ends here
