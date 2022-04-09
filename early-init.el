;; -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Hao Wang
;; License: GPL v3, or (at your option) any later version

;;; Startup optimization

(defvar toki/gc-cons-threshold gc-cons-threshold
  "Cache `gc-cons-threshold' during startup.")

(defvar toki/file-name-handler-alist file-name-handler-alist
  "Cache `file-name-handler-alist' during startup.")

(setq gc-cons-threshold most-positive-fixnum
      file-name-handler-alist nil)

(defun toki/finish-startup ()
  "Restore default value of some variables after startup."
  (setq file-name-handler-alist toki/file-name-handler-alist)
  ;; The user may change `gc-cons-threshold' in custom-post.el.
  (unless (eq gc-cons-threshold most-positive-fixnum)
    (setq gc-cons-threshold toki/gc-cons-threshold))
  ;; GC when idle.
  (run-with-idle-timer 5 t #'garbage-collect)
  ;; GC when Emacs frame is out of focus.
  (add-function :after after-focus-change-function
                (defun gc-after-focus-change ()
                  (unless (frame-focus-state)
                    (garbage-collect)))))

(add-hook 'after-init-hook #'toki/finish-startup)

;;; Misc

;; straight.el should stop package initialize, but for some reason it doens't
;; work everytime.
(setq package-enable-at-startup nil)

;; Hide tool bar, menu bar and scroll bars.  (Ref:
;; https://github.com/raxod502/radian/issues/180)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars . nil) default-frame-alist)

;; Note: I really want to experiment with setting font in early-init.el.  Now
;; Emacs zoom/shrinks when setting the font, creating a visible flick, but
;; setting it in early-init.el eliminates it.  The problem is
;; `font-family-list' returns nil during early-init phase.
