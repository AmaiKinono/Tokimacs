;;; toki-font.el --- Set fonts properly -*- lexical-binding: t -*-

;; Copyright (C) 2022 Hao Wang
;; License: GPL v3, or (at your option) any later version

;;; Commentary:

;;; Code:

;; To see the outline of this file, run `outline-minor-mode', then
;; `outline-hide-body'.  Another way is to run `occur' with the query:
;; ^;;;;* \|^(

;;;; User Options

(defvar toki-default-font-list
  (pcase system-type
    ('gnu/linux
     '("DejaVu Sans Mono"
       "Inconsolata"
       "Hack"
       "Fira Code"
       "Droid Sans Mono"
       "Luxi Mono"))
    ('darwin
     '("SF Mono"
       "Menlo"
       "Monaco"))
    ((or 'cygwin 'windows-nt 'ms-dos)
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
  (pcase system-type
    ('gnu/linux
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
    ('darwin
     '((cjk-misc ("PingFang SC" "Heiti SC" "Hiragino Sans"
                  "Apple SD Gothic Neo" "Apple Gothic") nil)
       (bopomofo ("PingFang SC" "Heiti SC") nil)
       (kana     ("Hiragino Sans") nil)
       (hangul   ("Apple SD Gothic Neo" "Apple Gothic") nil)
       (han      ("PingFang SC" "Heiti SC") nil)))
    ((or 'cygwin 'windows-nt 'ms-dos)
     '((unicode  nil ("Segoe UI Historic" "Arial Unicode MS"
                      "Lucida Sans Unicode"))
       (cjk-misc ("Microsoft YaHei UI" "Meiryo" "Malgun Gothic") nil)
       (bopomofo ("Microsoft YaHei UI") nil)
       (kana     ("Meiryo") nil)
       (hangul   ("Malgun Gothic") nil)
       (han      ("Microsoft YaHei UI" "Meiryo" "Malgun Gothic") nil))))
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

;;;; Internals

(defvar toki/default-font nil
  "The default font used.
This is the first available font in `toki-default-font-list', and
is automatically set by Tokimacs.")

;; TODO: seems the return value of font-family-list is a list of unibyte
;; strings.  I need to confirm if this is also the situation on Windows and
;; macOS.

;; TODO: `string-as-multibyte' obsoleted.
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
  "Set default font.

This is much slower than `toki/set-default-font' so we don't use
it during startup.  But `toki/set-default-font' doesn't work with
theme switching (see its docstring).  So during startup, we use
`toki/set-default-font' first, then use this to reallly set the
default font after 0.1 secs."
  (set-face-attribute
   'default nil
   :font (frame-parameter nil 'font)))

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
            (override-font nil))
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

;;;; API

(defun toki-set-font ()
  "Set fonts.
Run this when init to set the fonts."
  (toki/set-default-font)
  (run-with-idle-timer 0.1 nil #'toki/set-fontset-font)
  (run-with-idle-timer 0.1 nil #'toki/really-set-default-font))

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

(provide 'toki-font)

;;; toki-font.el ends here
