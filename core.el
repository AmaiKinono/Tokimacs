;; -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Hao Wang
;; License: GPL v3, or (at your option) any later version

;;; Libraries

(require 'cl-lib)

;;; Basic helpers

;; Comes from https://github.com/noctuid/general.el
(defmacro toki/setq (&rest settings)
  "A simplified `customize-set-variable' with the syntax of `setq'.
It calls the setter of the variable, if it's defined.  Once the
package which defines the variable is loaded, using `setq' will
not call the setter, so the desired effect may not happen.  This
is where `toki/setq' should be used.

See the docstring of `general-setq' for details."
  `(progn
     ,@(cl-loop for (var val) on settings by 'cddr
                collect `(funcall (or (get ',var 'custom-set) #'set)
                                  ',var ,val))))

(defmacro toki/setq-default (&rest settings)
  "A `setq-default' version of the `toki-setq' macro.
See its docstring for details."
  `(progn
     ,@(cl-loop for (var val) on settings by 'cddr
                collect `(funcall (or (get ',var 'custom-set) #'set-default)
                                  ',var ,val))))

;; Comes from https://github.com/hlissner/doom-emacs and
;; https://framagit.org/citreu/emacs.d/
(defmacro toki/add-trigger (hook-or-function &rest forms)
  "Attaches a self-removing function to HOOK-OR-FUNCTION.
FORMS are evaluated once, when that function/hook is first
invoked, then never again.

HOOK-OR-FUNCTION can be a quoted hook or a sharp-quoted
function (which will be advised).  FORMS can start with `:after',
which means when HOOK-OR-FUNCTION is a function, run the rest of
FORMS after the finishing the function."
  (declare (indent 1))
  (let ((append (if (eq (car forms) :after) (pop forms)))
        (fn (gensym "toki|after-call")))
    `(progn
       (fset ',fn
             (lambda (&rest _)
               ,@forms
               (cond ((functionp ,hook-or-function)
                      (advice-remove ,hook-or-function #',fn))
                     ((symbolp ,hook-or-function)
                      (remove-hook ,hook-or-function #',fn)))
               (unintern ',fn nil)))
       (cond ((functionp ,hook-or-function)
              (advice-add ,hook-or-function ,(if append :after :before) #',fn))
             ((symbolp ,hook-or-function)
              (put ',fn 'permanent-local-hook t)
              (add-hook ,hook-or-function #',fn ,append))))))

;;; Probes

(defconst toki-mac-p (eq system-type 'darwin)
  "Non-nil on macOS.")

(defconst toki-linux-p (eq system-type 'gnu/linux)
  "Non-nil on GNU/Linux.")

(defconst toki-win-p (memq system-type '(cygwin windows-nt ms-dos))
  "Non-nil on Windows.")

(defconst toki-gui-p (display-graphic-p)
  "Non-nil when using GUI Emacs.")

(defconst toki-x-p (eq window-system 'x)
  "Non-nil when using GUI Emacs with X display.
If you are using GUI Emacs on GNU/Linux, this is likely to be
true.")

(defconst toki-cocoa-p (eq window-system 'ns)
  "Non-nil when using GUI Emacs with Cocoa display.
If you are using GUI Emacs on macOS, this is likely to be true.")

;;; Hooks

(defvar toki-after-load-theme-hook nil
  "Hook run after `load-theme'.")

(define-advice load-theme (:after (&rest _) run-hook)
  "Run `toki-after-load-theme-hook' after load theme."
  (run-hooks 'toki-after-load-theme-hook))

;;; Defaults

;;;; Misc

(setq
 ;; Don't let Emacs put customize code into my init.el.
 custom-file (concat toki-modules-dir "custom-set-variables.el")
 ;; Don't let me confirm when using disabled commands.
 disabled-command-function nil
 ;; Don't tell me "you can use this key" when using commands by M-x.
 suggest-key-bindings nil)

(load custom-file 'noerror 'nomessage)

(with-eval-after-load 'gnutls
  (toki/setq
   ;; Don't allow insecure TLS connections.
   gnutls-verify-error t
   ;; Set the required security level for TLS to a reasonable modern value.
   gnutls-min-prime-bits 3072))

;;;; Auto save

(setq auto-save-default nil)

;;;; Auto backup

;; We want backup files for files not under version control, which is
;; controlled by `vc-make-backup-files', and this is exactly the default
;; behavior.

;; We also want a global directory for all backup files, the package
;; `no-littering' helps us to do this, by setting `backup-directory-alist'.

(setq
 backup-by-copying-when-linked t
 version-control t
 delete-old-versions t
 kept-new-versions 10
 kept-old-versions 2)

;;;; OS specific

;; Little optimization
(unless toki-cocoa-p (setq command-line-ns-option-alist nil))
(unless toki-x-p (setq command-line-x-option-alist nil))

(cond
 (toki-mac-p
  (setq
   mac-command-modifier 'super
   mac-option-modifier 'meta
   mac-redisplay-dont-reset-vscroll t
   mac-mouse-wheel-smooth-scroll nil
   ns-use-native-fullscreen nil
   ns-pop-up-frames nil))
 (toki-linux-p
  (setq
   x-gtk-use-system-tooltips nil
   x-underline-at-descent-line t))
 (toki-win-p
  ;; Fix the slow IO problem on Windows.
  (setq w32-get-true-file-attributes nil)))

;;; Package manager

;;;; Straight.el

;; Straight.el is a package manager that download packages by cloning their
;; repo, and has many nice features.

(setq
 straight-use-package-by-default t
 straight-vc-git-default-clone-depth 1
 straight-check-for-modifications '(check-on-save find-when-checking))

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
                         user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(defun toki-update-package (package)
  "Normalize and pull package from upstream."
  (interactive (list (straight--select-package
                      "Update package" nil 'installed)))
  (straight-normalize-package package)
  (straight-pull-package package 'from-upstream))

(defun toki-update-all ()
  "Normalize and pull all packages from upstream."
  (interactive)
  (straight-normalize-all)
  (straight-pull-all 'from-upstream))

;;;; Site-lisp

;; The site-lisp directory is where we put our own packages.  We byte-compile
;; and generate an autoload file for them. We only do this when a package is
;; newer than its byte-compiled version.

;; This is needed, or `generated-autoload-file' will be not defined as a
;; variable at byte-compile time.  See the comments in
;; `straight--build-autoloads'.
(eval-and-compile
  (or (require 'loaddefs-gen nil 'noerror)
      (require 'autoload))
  (require 'bytecomp))

(defvar toki-site-lisp-build-dir
  (progn
    (make-directory (concat toki-site-lisp-dir ".build/") t)
    (concat toki-site-lisp-dir ".build/"))
  "Directory of byte-compiling and autoloads of site-lisps.")

(defvar toki-site-lisps-to-build nil)

(defun toki-build-site-lisp ()
  (when toki-site-lisps-to-build
    (let ((generated-autoload-file
           (concat toki-site-lisp-build-dir "autoloads.el"))
          (delete-by-moving-to-trash nil)
          ;; Prevent `update-directory-autoloads' from running hooks when
          ;; visiting the autoload file.
          (find-file-hook nil)
          (write-file-functions nil)
          ;; Prevent `update-directory-autoloads' from creating backup files.
          (backup-inhibited t)
          (version-control 'never))
      (cl-letf (((symbol-function #'byte-compile-log-file) #'ignore)
                ((symbol-function #'byte-compile-nogroup-warn) #'ignore))
        ;; Byte compile.
        (dolist (f toki-site-lisps-to-build)
          (byte-compile-file (concat toki-site-lisp-build-dir f)))
        ;; Generate autoload file.  I don't know why but
        ;; `update-directory-autoloads' seems to not work once autoloads.el is
        ;; generated. We need to delete & regenerate it.
        (when (file-exists-p generated-autoload-file)
          (delete-file generated-autoload-file))
        (with-current-buffer (find-file-noselect generated-autoload-file)
          (insert ";; -*- lexical-binding: t -*-\n")
          (save-buffer)
          (kill-buffer))
        (update-directory-autoloads toki-site-lisp-build-dir)
        ;; Byte compile the autoload file.
        (byte-compile-file generated-autoload-file)
        ;; Native compile.
        (when (native-comp-available-p)
          (native-compile-async (list toki-site-lisp-build-dir)))))))

(defun toki-build-site-lisp-and-load-autoload ()
  (toki-build-site-lisp)
  (load (concat toki-site-lisp-build-dir "autoloads") 'noerror 'nomessage))

;; We build after initialization so that external packages are already in
;; `load-path', and site-lisps depending on them could be built.
(add-hook 'after-init-hook #'toki-build-site-lisp-and-load-autoload)

;; TODO: recompile when Emacs version changes
(let* (;; Dir & files
       (site-lisp-files (directory-files toki-site-lisp-dir nil ".el$"))
       (site-lisp-symlinks
        (cl-remove "autoloads.el"
                   (directory-files toki-site-lisp-build-dir nil ".el$")
                   :test #'equal))
       (byte-compiled-files
        (cl-remove "autoloads.elc"
                   (directory-files toki-site-lisp-build-dir nil ".elc$")
                   :test #'equal))
       (delete-by-moving-to-trash nil))
  (cl-letf (((symbol-function #'byte-compile-log-file) #'ignore)
            ((symbol-function #'byte-compile-nogroup-warn) #'ignore))
    (add-to-list 'load-path toki-site-lisp-build-dir)
    (let ((unneeded-symlinks
           (cl-set-difference site-lisp-symlinks site-lisp-files
                              :test #'equal)))
      (dolist (f unneeded-symlinks)
        (delete-file (concat toki-site-lisp-build-dir f))))
    (let ((unneeded-byte-files
           (cl-set-difference byte-compiled-files
                              (mapcar (lambda (s) (concat s "c"))
                                      site-lisp-files)
                              :test #'equal)))
      (dolist (f unneeded-byte-files)
        (delete-file (concat toki-site-lisp-build-dir f))))
    (dolist (f site-lisp-files)
      ;; Make symlinks of site-lisp files in build-dir.  This is needed for
      ;; `byte-compile-file' and `update-directory-autoloads'.
      (unless (member f site-lisp-symlinks)
        (make-symbolic-link (concat toki-site-lisp-dir f)
                            (concat toki-site-lisp-build-dir f)))
      ;; Record files to compile.
      (let ((byte-file (concat toki-site-lisp-build-dir f "c"))
            (site-lisp (concat toki-site-lisp-dir f)))
        (when (or (not (file-exists-p byte-file))
                  (file-newer-than-file-p site-lisp byte-file))
          ;; Delete the byte file to prevent it being load before a fresh
          ;; build.
          (delete-file byte-file)
          (push f toki-site-lisps-to-build))))
    (add-to-list 'custom-theme-load-path toki-site-lisp-build-dir)))

;;; Basic packages

(straight-use-package 'use-package)

(use-package use-package
  :config
  ;; Add :trigger keyword to use-package, which means to load the package after
  ;; a function or hook.

  ;; Comes from https://framagit.org/citreu/emacs.d
  (add-to-list 'use-package-deferring-keywords :trigger)
  (setq use-package-keywords
        (use-package-list-insert :trigger use-package-keywords :after))

  (defun use-package-handler/:trigger (name _keyword hooks rest state)
    (if (plist-get state :demand)
        (use-package-process-keywords name rest state)
      (let (l)
        (dolist (hook hooks)
          (push `(toki/add-trigger ',hook (require ',name)) l))
        `(,@(nreverse l) ,@(use-package-process-keywords name rest state)))))

  (defalias 'use-package-normalize/:trigger 'use-package-normalize-symlist))

(use-package general)

(use-package toki-combo
  :straight nil)

(use-package no-littering
  :trigger after-init-hook
  :config
  (no-littering-theme-backups))

;; Integrate system clipboard with Emacs in terminal.
(use-package xclip
  :unless toki-gui-p
  :hook (tty-setup . xclip-mode))

;; Get environment variables from the user's shell in macOS.
;; Not needed for the MacPorts version.
(use-package exec-path-from-shell
  :if (and toki-mac-p toki-gui-p)
  :config
  (setq
   exec-path-from-shell-check-startup-files nil
   exec-path-from-shell-variables '("PATH" "MANPATH" "PYTHONPATH" "GOPATH")
   exec-path-from-shell-arguments '("-l"))
  (exec-path-from-shell-initialize))

;;; Keybinds

(defvar toki-leader-key "M-SPC"
  "Leader key.")

(defvar toki-2nd-leader-key "M-j"
  "Second Leader Key.")

(defvar toki-local-leader-key "M-l"
  "A key to simulate `<leader> l'.")

(defvar toki-app-key "a"
  "For app commands.")

(defvar toki-buffer-key "b"
  "For buffer commands.")

(defvar toki-code-key "c"
  "For coding related commands.")

(defvar toki-edit-key "e"
  "For misc editing/Emacs related commands.")

(defvar toki-file-key "f"
  "For file commands.")

(defvar toki-help-key "h"
  "For help commands.")

(defvar toki-local-key "l"
  "For commands in temporary status or mode.")

(defvar toki-mark-key "m"
  "For marking related commands.")

(defvar toki-navigate-key "n"
  "For navigating related commands.")

(defvar toki-search-key "r"
  "For search/replace commands.")

(defvar toki-sexp-key "s"
  "For sexp/pair related commands.")

(defvar toki-tab-key "t"
  "For tab related commands.")

(defvar toki-ui-key "u"
  "For UI related commands.")

(defvar toki-vc-key "v"
  "For version control commands.")

(defvar toki-window-key "w"
  "For window related commands.")

(general-create-definer toki-leader-def
  :prefix toki-leader-key
  :keymaps 'override)

(general-create-definer toki-2nd-leader-def
  :prefix toki-2nd-leader-key
  :keymaps 'override)

(general-create-definer toki-app-def
  :prefix (concat toki-leader-key " " toki-app-key)
  :keymaps 'override)

(general-create-definer toki-buffer-def
  :prefix (concat toki-leader-key " " toki-buffer-key)
  :keymaps 'override)

(general-create-definer toki-code-def
  :prefix (concat toki-leader-key " " toki-code-key)
  :keymaps 'override)

(general-create-definer toki-edit-def
  :prefix (concat toki-leader-key " " toki-edit-key)
  :keymaps 'override)

(general-create-definer toki-file-def
  :prefix (concat toki-leader-key " " toki-file-key)
  :keymaps 'override)

(general-create-definer toki-help-def
  :prefix (concat toki-leader-key " " toki-help-key)
  :keymaps 'override)

(general-create-definer toki-local-def
  :prefix (concat toki-leader-key " " toki-local-key)
  :keymaps 'override)

(general-create-definer toki-mark-def
  :prefix (concat toki-leader-key " " toki-mark-key)
  :keymaps 'override)

(general-create-definer toki-navigate-def
  :prefix (concat toki-leader-key " " toki-navigate-key)
  :keymaps 'override)

(general-create-definer toki-search-def
  :prefix (concat toki-leader-key " " toki-search-key)
  :keymaps 'override)

(general-create-definer toki-sexp-def
  :prefix (concat toki-leader-key " " toki-sexp-key)
  :keymaps 'override)

(general-create-definer toki-tab-def
  :prefix (concat toki-leader-key " " toki-tab-key)
  :keymaps 'override)

(general-create-definer toki-ui-def
  :prefix (concat toki-leader-key " " toki-ui-key)
  :keymaps 'override)

(general-create-definer toki-vc-def
  :prefix (concat toki-leader-key " " toki-vc-key)
  :keymaps 'override)

(general-create-definer toki-window-def
  :prefix (concat toki-leader-key " " toki-window-key)
  :keymaps 'override)

(toki-leader-def
  :start-maps t
  toki-app-key '(:wk "App/Tool")
  toki-buffer-key '(:wk "Buffer")
  toki-code-key '(:wk "Code")
  toki-edit-key '(:wk "Edit/Emacs")
  toki-file-key '(:wk "File")
  toki-help-key '(:wk "Help")
  toki-local-key '(:wk "Local")
  toki-mark-key '(:wk "Mark")
  toki-navigate-key '(:wk "Navigate")
  toki-search-key '(:wk "Search/Replace")
  toki-sexp-key '(:wk "Sexp")
  toki-tab-key '(:wk "Tab")
  toki-ui-key '(:wk "UI")
  toki-vc-key '(:wk "Version Control")
  toki-window-key '(:wk "Window"))

(general-def
  :keymaps 'override
  toki-local-leader-key (general-key (concat toki-leader-key " " toki-local-key)))

(toki-edit-def
  "u" '(toki-update-package :wk "Update Package")
  "U" '(toki-update-all :wk "Update All Packages"))

(use-package which-key
  :hook (after-init . which-key-mode)
  :config
  (toki/setq
   which-key-separator ": "
   which-key-max-display-columns 6
   which-key-idle-delay 0.8
   which-key-idle-secondary-delay 0
   which-key-sort-order 'which-key-key-order-alpha))
