;;; toki-blink.el --- A command to blink current line -*- lexical-binding: t -*-

;; Copyright (C) 2020 Hao Wang
;; License: GPL v3, or (at your option) any later version

;;; Commentary:

;; toki-blink offers you a `toki-blink' command.  Use it to blink current line.
;; You can use this in your own functions whose effect needs attention, like
;; `recenter'.

;;; Code:

;; To see the outline of this file, run `outline-minor-mode', then
;; `outline-hide-body'.  Another way is to run `occur' with the query:
;; ^;;;;* \|^(

(require 'cl-lib)

;;;; User options

(defvar toki-blink-color "#f08080"
  "The color of the blinking.")

(defvar toki-blink-fade-time 0.15
  "How long it takes for the blinking to fade out.")

(defvar toki-blink-fade-time-step 0.02
  "Time step used in the fading of blinking.")

;;;; Internals

(defvar toki/blink-colors nil
  "A list of colors used in the fading of blink.")

(defvar toki/blink-ov nil
  "Overlay used for blinking.")

(defvar toki/blink-timer nil
  "Timer used for fading the blinking.")

(defvar toki/blink-mode-line-colors nil
  "Tab line version of `toki/blink-colors'.")

(defvar toki/blink-mode-line-timer nil
  "Timer used for fading the blinking on tab line.")

(defun toki/blink-ov-range ()
  "Return the range to be used by the overlay in `toki-blink'."
  (save-excursion
    (let ((beg nil)
          (end nil))
      (setq beg (point))
      (end-of-visual-line)
      (setq end (point))
      (cons beg end))))

(defun toki/blink-blend (c1 c2 alpha)
  "Blend two colors C1 and C2 with ALPHA.
C1 and C2 are hexidecimal strings.  ALPHA is a number between 0.0
and 1.0 which is the influence of C1 on the result."
  (apply (lambda (r g b)
           (format "#%02x%02x%02x"
                   (ash r -8)
                   (ash g -8)
                   (ash b -8)))
         (cl-mapcar
          (lambda (x y)
            (round (+ (* x alpha) (* y (- 1 alpha)))))
          (color-values c1) (color-values c2))))

(defun toki/blink-colors (&optional bg times)
  "Return a list of colors to use in the fading of blink.
BG is the background color to fade into.  If it's nil, the
background of default face is used.  TIMES is the number of
blinking.  If it's nil, blink only once."
  (let* ((bg-mode (frame-parameter nil 'background-mode))
         (bg-unspecified-p (string= (face-background 'default)
                                    "unspecified-bg"))
         (bg (cond
              (bg bg)
              ((and bg-unspecified-p (eq bg-mode 'dark)) "#333333")
              ((and bg-unspecified-p (eq bg-mode 'light)) "#dddddd")
              (t (face-background 'default))))
         (colors nil)
         (times (or times 1))
         (fade-steps (ceiling
                      (/ toki-blink-fade-time toki-blink-fade-time-step))))
    (dotimes (_ times)
      (dotimes (n fade-steps)
        (push (toki/blink-blend toki-blink-color
                                bg (/ (float (1+ n)) fade-steps))
              colors)))
    colors))

(defun toki/blink-fade ()
  "Fade the blinking."
  (let ((color (pop toki/blink-colors)))
    (if color
        (progn
          (overlay-put toki/blink-ov 'face (list :background color))
          (overlay-put toki/blink-ov 'after-string
                       (propertize " "
                                   'face (list :background color)
                                   'display
                                   `(space :align-to right-fringe))))
      (delete-overlay toki/blink-ov)
      (setq toki/blink-ov nil)
      (cancel-timer toki/blink-timer)
      (setq toki/blink-timer nil))))

(defvar toki/mode-line-bg-orig (face-attribute 'mode-line :background))

(defun toki/blink-mode-line-fade ()
  "Fade the blinking of tab line."
  (let ((color (pop toki/blink-mode-line-colors)))
    (if color
        (set-face-attribute 'mode-line nil :background color)
      (set-face-attribute 'mode-line nil :background toki/mode-line-bg-orig)
      (cancel-timer toki/blink-mode-line-timer))))

;;;; Main entry point

;;;###autoload
(defun toki-blink ()
  "Blink after cursor.
Customize `toki-blink-color', `toki-blink-fade-time',
`toki-blink-fade-time-step' to change the behavior."
  (interactive)
  (when (timerp toki/blink-timer)
    (setq toki/blink-colors nil)
    (delete-overlay toki/blink-ov)
    (setq toki/blink-ov nil)
    (cancel-timer toki/blink-timer)
    (setq toki/blink-timer nil))
  (let ((range (toki/blink-ov-range)))
    (setq toki/blink-colors (toki/blink-colors))
    (setq toki/blink-ov (make-overlay (car range) (cdr range)))
    (setq toki/blink-timer
          (run-with-timer nil toki-blink-fade-time-step #'toki/blink-fade))))

;;;###autoload
(defun toki-blink-mode-line ()
  "Blink tab line."
  (interactive)
  (if (timerp toki/blink-mode-line-timer)
      (progn
        (setq toki/blink-mode-line-colors nil)
        (cancel-timer toki/blink-mode-line-timer)
        (setq toki/blink-mode-line-timer nil))
    (setq toki/mode-line-bg-orig (face-attribute 'mode-line :background)))
  (setq toki/blink-mode-line-colors (toki/blink-colors))
  (setq toki/blink-mode-line-timer
        (run-with-timer nil toki-blink-fade-time-step
                        #'toki/blink-mode-line-fade)))

;;;###autoload
(defun toki-recenter ()
  "Recenter and blink current line.
When point is in minibuffer, operate on the window selected
before entering minibuffer."
       (interactive)
       (if (minibufferp)
           (with-selected-window (minibuffer-selected-window)
             (recenter)
             (toki-blink))
         (recenter)
         (toki-blink)))

(provide 'toki-blink)

;;; toki-blink.el ends here
