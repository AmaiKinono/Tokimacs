;; -*- lexical-binding: t; -*-

;; Tidy and beautiful UI.

;; Copyright (C) 2021 Hao Wang
;; License: GPL v3, or (at your option) any later version

;;; User options

;;;; Themes

(defvar toki-themes '(base16-default-dark)
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

(use-package base16-theme
  :defer t)

(use-package gruvbox-theme
  :defer t)

(use-package color-theme-sanityinc-tomorrow
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
  :straight nil)

(use-package mini-modeline
  :after toki-modeline
  :config
  (toki/setq
   ;; We don't need this.  We will use a thin modeline to distinguish between
   ;; active and inactive windows.
   mini-modeline-enhance-visual nil
   ;; Let mini modeline show immediately after startup.
   mini-modeline-update-interval 0.0
   mini-modeline-r-format toki-modeline-main-format)
  ;; Prevent frequent update of mini modeline after its first display.
  (run-with-idle-timer 0.1 'norepeat
                       (lambda ()
                         (toki/setq mini-modeline-update-interval 0.5)))
  (if toki-gui-p
      (toki/setq mini-modeline-right-padding 1)
    (toki/setq mini-modeline-right-padding 2))
  (mini-modeline-mode))

(with-eval-after-load 'mini-modeline
  (define-advice mini-modeline-display (:around (fn &optional arg) update-when-needed)
    "See https://github.com/kiennq/emacs-mini-modeline/pull/50.
The reason for this patch (from Eli Zaretskii):

What this (package) does is modify a non-current buffer at high
frequency, so the main redisplay optimization of updating only
the selected window will be disabled. And when that happens,
Emacs examines all the windows on all the frames to see which
ones need to be updated, something that takes more cycles.

This also makes me consider if we should use vanilla per-buffer
modeline.  The price mentioned above is simply unavoidable with
mini-modeline or similar plugins."
    (let ((old-cache mini-modeline--cache))
      (cl-letf* ((erase-buffer-orig (symbol-function 'erase-buffer))
                 ((symbol-function 'erase-buffer)
                  (if (eq (current-buffer) mini-modeline--minibuffer)
                      (lambda ()
                        (if (equal mini-modeline--cache old-cache)
                            (throw 'noupdate nil)
                          (funcall erase-buffer-orig)))
                    (lambda () (funcall erase-buffer-orig)))))
        (catch 'noupdate
          (funcall fn arg)))))
  ;; This solves https://github.com/kiennq/emacs-mini-modeline/issues/45, but
  ;; regress https://github.com/kiennq/emacs-mini-modeline/issues/23.  The
  ;; former is a more severe problem.  This makes me wants to use vanilla
  ;; modeline more.
  (define-advice mini-modeline--enable (:around (fn) dont-use-timer)
    (cl-letf* ((run-with-timer-orig (symbol-function 'run-with-timer))
               ((symbol-function 'run-with-timer)
                (lambda (secs repeat function &rest args)
                  (if (eq function #'mini-modeline-display)
                      (add-hook 'pre-redisplay-functions #'mini-modeline-display)
                    (funcall run-with-timer-orig secs repeat function args)))))
      (funcall fn)))
  (define-advice mini-modeline--disable (:around (fn) dont-use-timer)
    (funcall fn)
    (remove-hook 'pre-redisplay-functions #'mini-modeline-display)))

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

(defun toki-toggle-fullscreen ()
  "Toggle fullscreen state of selected frame.
When fullscreen, show time and battery information in the echo area."
  (interactive)
  (toggle-frame-fullscreen)
  (if (memq (frame-parameter nil 'fullscreen) '(fullscreen fullboth))
      (setq mini-modeline-l-format toki-modeline-additional-format)
    (setq mini-modeline-l-format '(:eval (mini-modeline-msg)))))

;;; Keybinds

(toki-ui-def
  "f" '(toki-toggle-fullscreen :wk "= Fullscreen")
  "m" '(toggle-frame-maximized :wk "= Maximize")
  "t" `(,(toki/make-combo toki-switch-theme) :wk "Switch Theme")
  "T" '(toki-load-theme :wk "Load Theme")
  "=" `(,(toki/make-combo toki-increase-font-size) :wk "Increase Font Size")
  "-" `(,(toki/make-combo toki-decrease-font-size) :wk "Decrease Font Size"))
