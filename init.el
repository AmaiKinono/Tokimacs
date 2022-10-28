;; -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Hao Wang
;; License: GPL v3, or (at your option) any later version

;;; Load configurations

;; When the user start Emacs using a different $HOME, make sure the symlinks
;; created by Emacs points to the right file.
(setq user-emacs-directory (expand-file-name user-emacs-directory))

(defconst toki-modules-dir (concat user-emacs-directory "modules/")
  "Directory of modules.")

(defconst toki-site-lisp-dir (concat user-emacs-directory "site-lisp/")
  "Directory of site-lisps.")

(load (concat user-emacs-directory "core") 'noerror 'nomessage)
(load (concat toki-modules-dir "private/custom-pre") 'noerror 'nomessage)

(defvar toki-modules
  '("editor/selection" ;; Interactively filter & select from a list
    "editor/ui"        ;; Tidy & beautiful UI
    "editor/file"      ;; File management
    "editor/edit"      ;; Fine & structural editing
    ;;"editor/evil"    ;; Vim emulator
    "editor/view-mode" ;; View mode that resembles vi normal mode (kind of)
    "prog/ide"         ;; IDE
    "prog/lang"        ;; Language supports
    "app/term"         ;; Terminal emulator
    "app/facets"       ;; My zettlekasten
    ))

(dolist (module toki-modules)
  (load (concat toki-modules-dir module "/config") 'noerror 'nomessage))

(load (concat toki-modules-dir "private/custom-post") 'noerror 'nomessage)
