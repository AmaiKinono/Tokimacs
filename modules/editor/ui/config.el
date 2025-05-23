;; -*- lexical-binding: t; -*-

;; Tidy and beautiful UI.

;; Copyright (C) 2021 Hao Wang
;; License: GPL v3, or (at your option) any later version

;;; User options

;;;; Themes

(defvar toki-themes '(toki-base16-gruvbox base16-standardized-light)
  "Themes used when calling `toki-switch-theme'.
The first element is used as the default theme.")

;;;; Transparency

(defvar toki-frame-alpha 0.9
  "Transparency of frame background.")

;;; Defaults

(toki/setq-default
 inhibit-startup-screen t
 ;; Don't show a buffer list when load 3+ files at a time.
 inhibit-startup-buffer-menu t
 ;; Don't load default.el.
 inhibit-default-init t
 initial-major-mode 'fundamental-mode
 initial-scratch-message nil
 ;; Don't let the cursor go into the minibuffer prompt.
 minibuffer-prompt-properties
 '(read-only t cursor-intangible t face minibuffer-prompt)
 use-file-dialog nil
 use-dialog-box nil
 ;; Don't compact font caches during GC.  This can reduce the lagging caused by
 ;; large fonts.
 inhibit-compacting-font-caches t
 ;; When spinning the mouse wheel, scroll 2 lines, or 1 lines if holding shift.
 mouse-wheel-scroll-amount '(2 ((shift) . 1))
 ;; Don't change the scrolling amount by the mouse wheel speed.
 mouse-wheel-progressive-speed nil
 cursor-type '(bar . 3)
 word-wrap t
 word-wrap-by-category t
 use-short-answers t
 ;; This actually works also for occur/grep buffers.
 next-error-message-highlight t
 redisplay-skip-fontification-on-input t)

(fset 'display-startup-echo-area-message 'ignore)

(blink-cursor-mode -1)

(set-frame-parameter nil 'alpha-background
                     toki-frame-alpha)

;; Match the title bar color with current theme in macOS.
(use-package ns-auto-titlebar
  :if toki-cocoa-p
  :hook (after-init . ns-auto-titlebar-mode))

;;; Theme

;; The `fixed-pitch' face is used for places like code blocks in markup
;; languages.  It uses "Monospace" family by default, which is reasonable, but
;; may actually fall back to an unwanted font.  Since most people use a
;; monospaced font as the default font, we let it use the default font.
(set-face-attribute 'fixed-pitch nil :family 'unspecified)

(defun toki/color-bright-p (color)
  "Return t if COLOR is bright."
  (when-let* ((rgb (color-name-to-rgb color))
              ;; Ref: https://www.w3.org/TR/AERT/#color-contrast
              (br (+ (* 0.299 (nth 0 rgb))
                     (* 0.587 (nth 1 rgb))
                     (* 0.114 (nth 2 rgb)))))
    (> br 0.5)))

(use-package base16-theme
  :init
  (toki/setq
   base16-distinct-fringe-background nil
   base16-theme-256-color-source 'colors)
  :defer t
  :config
  ;; TODO: change wgrep-face.  It's awful...
  (define-advice base16-theme-define (:around (fn theme-name theme-colors) fix)
    (base16-theme-set-faces
     theme-name theme-colors
     ;; Cursor should be light (on dark themes, vice versa).
     '((cursor :background base06)
       ;; Error should be in the same color as keywords.
       (error :foreground base0E)
       (mode-line-active :background base01 :box base03
                         :inherit mode-line)
       (mode-line-inactive :background base00 :foreground base03
                           :underline base02 :inherit mode-line)
       (toki-tabs-current-tab-face :background base02 :foreground base06)
       (toki-tabs-inactive-tab-face :foreground base04)
       (toki-tabs-separator-face :foreground base02)
       (toki-tabs-rest-face :italic t)
       ;; Don't mess with colors in terminals
       (term-color-black :inherit ansi-color-black)
       (term-color-white :inherit ansi-color-white)
       (term-color-red :inherit ansi-color-red)
       (term-color-yellow :inherit ansi-color-yellow)
       (term-color-green :inherit ansi-color-green)
       (term-color-cyan :inherit ansi-color-cyan)
       (term-color-blue :inherit ansi-color-blue)
       (term-color-magenta :inherit ansi-color-magenta)))
    ;; Source: https://www.markusweimar.de/en/color-schemes/. It's good to use
    ;; high contrast theme for terminals as some programs prints in close
    ;; foreground and background colors, for example, cyan text on green
    ;; background.
    (if (toki/color-bright-p (plist-get theme-colors :base00))
        (base16-theme-set-faces
         theme-name theme-colors
         '((ansi-color-black :foreground "#000000" :background "#000000")
           (ansi-color-white :foreground "#aaaaaa" :background "#aaaaaa")
           (ansi-color-red :foreground "#bd000d" :background "#bd000d")
           (ansi-color-yellow :foreground "#ffbb00" :background "#ffbb00")
           (ansi-color-green :foreground "#006607" :background "#006607")
           (ansi-color-cyan :foreground "#005a61" :background "#005a61")
           (ansi-color-blue :foreground "#004ce6" :background "#004ce6")
           (ansi-color-magenta :foreground "#ad007f" :background "#ad007f")
           (ansi-color-bright-black :foreground "#000000" :background "#000000")
           (ansi-color-bright-white :foreground "#ffffff" :background "#ffffff")
           (ansi-color-bright-red :foreground "#bd000d" :background "#bd000d")
           (ansi-color-bright-yellow :foreground "#ffbb00" :background "#ffbb00")
           (ansi-color-bright-green :foreground "#006607" :background "#006607")
           (ansi-color-bright-cyan :foreground "#005a61" :background "#005a61")
           (ansi-color-bright-blue :foreground "#004ce6" :background "#004ce6")
           (ansi-color-bright-magenta :foreground "#ad007f" :background "#ad007f")))
      (base16-theme-set-faces
       theme-name theme-colors
       '((ansi-color-black :foreground "#000000" :background "#000000")
         (ansi-color-white :foreground "#888888" :background "#888888")
         (ansi-color-red :foreground "#ff7c4d" :background "#ff7c4d")
         (ansi-color-yellow :foreground "#ffcc00" :background "#ffcc00")
         (ansi-color-green :foreground "#22ff00" :background "#22ff00")
         (ansi-color-cyan :foreground "#00ffff" :background "#00ffff")
         (ansi-color-blue :foreground "#1a66ff" :background "#1a66ff")
         (ansi-color-magenta :foreground "#ff61df" :background "#ff61df")
         (ansi-color-bright-black :foreground "#000000" :background "#000000")
         (ansi-color-bright-white :foreground "#ffffff" :background "#ffffff")
         (ansi-color-bright-red :foreground "#ff7c4d" :background "#ff7c4d")
         (ansi-color-bright-yellow :foreground "#ffcc00" :background "#ffcc00")
         (ansi-color-bright-green foreground "#22ff00" :background "#22ff00")
         (ansi-color-bright-cyan :foreground "#00ffff" :background "#00ffff")
         (ansi-color-bright-blue :foreground "#1a66ff" :background "#1a66ff")
         (ansi-color-bright-magenta :foreground "#ff61df" :background "#ff61df"))))
    (funcall fn theme-name theme-colors)))

(use-package toki-base16-gruvbox-theme
  :straight nil
  :defer t)

(defvar toki/theme-index 0
  "Index of current used theme in `toki-themes'.")

(defun toki/italic-comment ()
  "Use italic face for comments.
This is run in `toki-after-load-theme-hook' to use italic comment
for all themes."
  (set-face-attribute 'font-lock-comment-face nil :slant 'italic))

(defun toki/flat-fringe ()
  "Remove the background of fringe so it looks prettier."
  (set-face-attribute 'fringe nil :background 'unspecified))

(add-hook 'toki-after-load-theme-hook #'toki/italic-comment)
(add-hook 'toki-after-load-theme-hook #'toki/flat-fringe)

(defun toki-switch-theme ()
  "Cycle through themes in `toki-themes'."
  (interactive)
  (setq toki/theme-index (mod (1+ toki/theme-index) (length toki-themes)))
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme (nth toki/theme-index toki-themes) 'no-confirm))

(defun toki-load-theme ()
  "A no-confirm version of `load-theme' for interactive use.
See the docstring of `load-theme' for details."
  (interactive)
  (let ((theme (completing-read "Theme: "
                                (mapcar #'symbol-name
                                        (custom-available-themes)))))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme (intern theme) 'no-confirm)))

(load-theme (car toki-themes) 'no-confirm)

;;; Font

(use-package toki-font
  :straight nil
  :config
  (when toki-gui-p (toki-set-font)))

;;; Modeline

(use-package toki-modeline
  :straight nil
  :init
  (setq-default mode-line-format '(:eval (toki-modeline-compute)))
  (face-spec-set 'mode-line '((t :height 0.9)))
  :config
  (toki-modeline-setup))

;;; Misc

;; For Emacs in terminal, make the vertical border between windows looks
;; better.
(when (not toki-gui-p)
  (set-face-attribute 'vertical-border nil :background 'unspecified))

(setq ring-bell-function #'toki-blink-mode-line
      eldoc-echo-area-use-multiline-p nil)

;; toki-blink provides a simple command `toki-blink' to blink current line.
(use-package toki-blink
  :straight nil
  :defer t)

;;; Keybinds

(toki-ui-def
  "f" '(toggle-frame-fullscreen :wk "<> Fullscreen")
  "m" '(toggle-frame-maximized :wk "<> Maximize")
  "t" `(,(toki-make-combo toki-switch-theme) :wk "Switch Theme")
  "T" '(toki-load-theme :wk "Load Theme")
  "=" '(global-text-scale-adjust :wk "Increase Font Size")
  "-" '(global-text-scale-adjust :wk "Decrease Font Size")
  "0" '(global-text-scale-adjust :wk "Restore Font Size"))
