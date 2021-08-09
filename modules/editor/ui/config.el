;; -*- lexical-binding: t; -*-

;; Tidy and beautiful UI.

;; Copyright (C) 2021 Hao Wang
;; License: GPL v3, or (at your option) any later version

;;; User options

;;;; Themes

(defvar toki-themes '(toki-base16-gruvbox base16-atelier-plateau-light)
  "Themes used when calling `toki-switch-theme'.
The first element is used as the default theme.")

;;;; Fonts

(defvar toki-default-font-list
  (cond
   (toki-linux-p
    '("DejaVu Sans Mono"
      "Inconsolata"
      "Hack"
      "Fira Code"
      "Droid Sans Mono"
      "Luxi Mono"))
   (toki-mac-p
    '("SF Mono"
      "Menlo"
      "Monaco"))
   (toki-win-p
    '("Consolas"
      "Courier New"
      "Lucida Console")))
  "Default fonts.
This is a list of the font family names.  The first one avaliable
will be used as the default font.

This doesn't affect Emacs in terminal.")

(defvar toki-default-font-size 20
  "Default font size.
This doesn't affect Emacs in terminal.")

(defvar toki-fontset-font-list
  (cond
   (toki-linux-p
    '((unicode  nil ("Droid Sans" "Symbola" "Unifont"))
      (cjk-misc ("Sarasa Term SC" "WenQuanYi Micro Hei"
                 "IPAGothic" "NanumGothicCoding"
                 "NanumBarunGothic" "Droid Sans") nil)
      (bopomofo ("Sarasa Term SC" "WenQuanYi Micro Hei"
                 "Droid Sans") nil)
      (kana     ("Sarasa Term J" "IPAGothic" "Droid Sans") nil)
      (hangul   ("Sarasa Term K" "NanumGothicCoding"
                 "NanumBarunGothic" "Droid Sans") nil)
      (han      ("Sarasa Term SC" "WenQuanYi Micro Hei"
                 "Droid Sans" "IPAGothic") nil)))
   (toki-win-p
    '((unicode  nil ("Segoe UI Historic" "Arial Unicode MS"
                     "Lucida Sans Unicode"))
      (cjk-misc ("Microsoft YaHei UI" "Meiryo" "Malgun Gothic") nil)
      (bopomofo ("Microsoft YaHei UI") nil)
      (kana     ("Meiryo") nil)
      (hangul   ("Malgun Gothic") nil)
      (han      ("Microsoft YaHei UI" "Meiryo" "Malgun Gothic") nil)))
   (toki-mac-p
    '((cjk-misc ("PingFang SC" "Heiti SC" "Hiragino Sans"
                 "Apple SD Gothic Neo" "Apple Gothic") nil)
      (bopomofo ("PingFang SC" "Heiti SC") nil)
      (kana     ("Hiragino Sans") nil)
      (hangul   ("Apple SD Gothic Neo" "Apple Gothic") nil)
      (han      ("PingFang SC" "Heiti SC") nil))))
  "A list for override and fallback fonts configuration.
If you only deal with latin characters in Emacs, you can just
ignore this.  For users of other languages, please read on.

You can think that when searching font for a character, Emacs
goes through override fonts first, then your default font, then
fallback fonts.  The real situation is a bit more complicated, as
explained below.

Each element should consists of a fontset, a list of override
fonts, and a list of fallback fonts.  So the value of this
variable should look like this:

  ((fontset1 (\"override1\" \"override2\")
             (\"fallback1\" \"fallback2\"))
   (fontset2 (\"override1\" \"override2\")
             (\"fallback1\" \"fallback2\")))

For each fontset, the first override font avaliable, or
`toki-default-font' if none of them is avaliable, will be used as
default font for that fontset. Then, avaliable fallback fonts
will be appended. Notice that:

1. Fontsets that come later in the list will override the
previous ones.  So you want to make sure that each fontset
doesn't contain any fontsets before it.

2. The override font is set based on whether it's available from
the system, not whether it can display the fontset correctly.  So
you need to set the right override fonts for each fontset.

3. Overriding the latin fontset won't work.  As far as I'm
concerned, Emacs will always use `toki-default-font' to display
latin characters.

See the definition of this variable to get an idea of how to
customize it.  Since looking up glyphs in fallback fonts is slow,
it's recommended to configure `toki-default-font' and the
override fonts in this variable to suit the languages you are
using.

This doesn't affect Emacs in terminal.")

(defvar toki-font-rescale-alist nil
  "An alist to specify rescale factor for font families.
Each element is a cons pair like (font-family . rescale-factor).
See the docstring of `face-font-rescale-alist' for details.

The typical use is to rescale CJK fonts so that 2 latin
characters is the same width as 1 CJK character.  This makes the
same column always appear at the same horizontal screen
position, so some aligning problems is avoided.

This doesn't affect Emacs in terminal.")

;;;; Transparency

(defvar toki-active-frame-alpha 0.92
  "Transparency of active frame.")

(defvar toki-inactive-frame-alpha 0.85
  "Transparency of inactive frame.")

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
 cursor-type '(bar . 3))

(fset 'display-startup-echo-area-message 'ignore)

(blink-cursor-mode -1)
;; Don't ask us to input "yes" or "no".  Use "y" or "n" instead.
(defalias 'yes-or-no-p 'y-or-n-p)

(set-frame-parameter nil 'alpha
                     (list toki-active-frame-alpha toki-inactive-frame-alpha))

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

(use-package base16-theme
  :init
  (toki/setq
   base16-distinct-fringe-background nil)
  :defer t
  :config
  (define-advice base16-theme-define (:around (fn theme-name theme-colors) fix)
    (base16-set-faces
     theme-name theme-colors
     ;; Cursor should be light (on dark themes, vice versa).
     '((cursor :background base06)
       ;; Error should be in the same color as keywords.
       (error :foreground base0E)))
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

(add-hook 'toki-after-load-theme-hook #'toki/italic-comment)

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

(defun toki/initialize-theme ()
  "Load theme during startup.
This also add `toki/really-set-default-font' to
`toki-after-load-theme-hook'.  See its docstring for details."
  (load-theme (car toki-themes) 'no-confirm))

(add-hook 'after-init-hook #'toki/initialize-theme)

;;; Font

(defvar toki/default-font nil
  "The default font used.
This is the first available font in `toki-default-font-list', and
is automatically set by Tokimacs.")

;; TODO: seems the return value of font-family-list is a list of unibyte
;; strings.  I need to confirm if this is also the situation on Windows and
;; macOS.
(defvar toki/font-family-list
  (mapcar #'string-as-multibyte (font-family-list))
  "The available fonts.")

(defun toki/set-default-font ()
  "Set default font.
This finds the first avaliable font in `toki-default-font-list'
and use it.  Also, `toki/default-font' is set for later use.

The problem of this function is the font will change after
loading a theme manually.  We use `toki/really-set-default-font'
to solve this, see its docstring for details."
  (cl-dolist (font toki-default-font-list)
    (when (member font toki/font-family-list)
      (setq toki/default-font font)
      (add-to-list 'default-frame-alist
                   (cons 'font
                         (concat font ":size="
                                 (number-to-string toki-default-font-size))))
      (cl-return))))

(defun toki/really-set-default-font ()
  "Set default font after 0.1 seconds.

This is much slower than `toki/set-default-font' so we don't use
it during startup.  But `toki/set-default-font' doesn't work with
theme switching (see its docstring).  So during startup, we use
`toki/set-default-font' first, then use this to reallly set the
default font after 0.1 secs."
  (run-with-timer
   0.1 nil
   (lambda ()
     (set-face-attribute
      'default nil
      :font (frame-parameter nil 'font)))))

(defun toki/set-font-rescale-factor (font)
  "Set rescale factor for font family FONT.
This uses the values specified in `toki-font-rescale-alist'.  If
FONT is not in it, use 1.0 as the rescale factor."
  (let ((factor (alist-get font toki-font-rescale-alist
                           1.0 'noremove #'equal)))
    (setf (alist-get font face-font-rescale-alist
                     'nodefault 'noremove #'equal)
          factor)))

(defun toki/set-fontset-font ()
  "Set override and fallback fonts for fontsets.
See the docstring of `toki-fontset-font-list' for details."
  (when toki/default-font
    (dolist (pair toki-fontset-font-list)
      (let ((fontset (nth 0 pair))
            (override-font nil)
            (rescale-factor nil))
        (cl-dolist (font (nth 1 pair))
          (when (member font toki/font-family-list)
            (setq override-font font)
            (cl-return)))
        (toki/set-font-rescale-factor (or override-font toki/default-font))
        (set-fontset-font t fontset (font-spec :family override-font))
        (dolist (font (nth 2 pair))
          (when (member font toki/font-family-list)
            (set-fontset-font t fontset (font-spec :family font)
                              nil 'append)
            (toki/set-font-rescale-factor font)))))))

(defun toki/set-font ()
  "Set fonts."
  (toki/set-default-font)
  (toki/set-fontset-font)
  (toki/really-set-default-font))

(when toki-gui-p
  (add-hook 'after-init-hook #'toki/set-font))

(defun toki-adjust-font-size (inc)
  "Increment font size in current frame by INC."
  (let* ((frame-height (frame-height nil))
         (frame-width (frame-width nil))
         (font-size-orig (font-get (face-attribute 'default :font) :size))
         (font-size-new (+ inc font-size-orig))
         (ratio (/ (float font-size-orig) font-size-new)))
    (set-face-attribute 'default nil :font (font-spec :size font-size-new))
    (set-frame-parameter nil 'width (round (* frame-width ratio)))
    (set-frame-parameter nil 'height (round (* frame-height ratio)))))

(defun toki-increase-font-size ()
  "Increase font size in current frame."
  (interactive)
  (toki-adjust-font-size 2))

(defun toki-decrease-font-size ()
  "Decrease font size in current frame."
  (interactive)
  (toki-adjust-font-size -2))

;; All the icons is an icon fonts utility.  Some of our packages use it.
(use-package all-the-icons
  :if toki-gui-p
  :defer t)

;;; Modeline

(use-package toki-modeline
  :trigger after-init-hook
  :straight nil
  :config
  (toki-modeline-setup))

(defun toki/modeline-refresh-face ()
  (let ((bg (face-attribute 'default :background)))
    (set-face-attribute 'mode-line nil
                        :background bg :height 0.9)
    (set-face-attribute 'mode-line-inactive nil
                        :background bg :height 0.9
                        :bold t :inherit 'shadow)))

(add-hook 'toki-after-load-theme-hook #'toki/modeline-refresh-face)

;;; Misc

;; For Emacs in terminal, make the vertical border between windows looks
;; better.
(when (not toki-gui-p)
  (set-face-attribute 'vertical-border nil :background 'unspecified))

;; Ref: https://debbugs.gnu.org/cgi/bugreport.cgi?bug=44448
;; (setq ring-bell-function #'toki-blink-window-num)

;; toki-blink provides a simple command `toki-blink' to blink current line.
(use-package toki-blink
  :straight nil
  :defer t)

;;; Keybinds

(toki-ui-def
  "f" '(toggle-frame-fullscreen :wk "<> Fullscreen")
  "m" '(toggle-frame-maximized :wk "<> Maximize")
  "t" `(,(toki/make-combo toki-switch-theme) :wk "Switch Theme")
  "T" '(toki-load-theme :wk "Load Theme")
  "=" `(,(toki/make-combo toki-increase-font-size) :wk "Increase Font Size")
  "-" `(,(toki/make-combo toki-decrease-font-size) :wk "Decrease Font Size"))
