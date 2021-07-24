;; -*- lexical-binding: t; -*-

;; File management

;;; Coding system

;; Make UTF-8 the default coding system.
(set-language-environment "UTF-8")
;; On windows, clipboard may use UTF-16.  Let the system decide what to use
;; here.
(unless toki-win-p
  (setq selection-coding-system 'utf-8))

;;; Misc

;; Don't ask me whether to follow a symlink when visiting it
(setq vc-follow-symlinks t)
(global-auto-revert-mode 1)

;;; Packages

(use-package toki-file
  :defer t
  :straight nil)

;;; Keybinds

(toki-file-def
  "s" '(save-buffer :wk "Save File")
  "S" '(write-file :wk "Save As")
  "o" '(find-file :wk "Open File")
  "f" '(toki-find-file :wk "Find File")
  "t" '(toki-find-file-in-tokimacs :wk "Find in Tokimacs")
  "l" '(find-library :wk "Open Elisp Library"))
