;;; toki-base16-gruvbox-theme.el --- A base16 mockup of doom-gruvbox theme -*- lexical-binding: t; -*-

;; Copyright (C) 2021 Hao Wang
;; License: GPL v3, or (at your option) any later version

;;; Commentary:

;;; Code:

;; To see the outline of this file, run `outline-minor-mode', then
;; `outline-hide-body'.  Another way is to run `occur' with the query:
;; ^;;;;* \|^(

;;;; Libraries

(toki-declare-ext-pkg base16-theme)
(require 'base16-theme)

;;;; Theme

(defvar toki-base16-gruvbox-colors
    '(:base00 "#1d2021"
      :base01 "#282828"
      :base02 "#504945"
      :base03 "#928374"
      :base04 "#ddcfb3"
      :base05 "#ebdbb2"
      :base06 "#fbf1c7"
      :base07 "#fcf5d7"
      :base08 "#8ec07c"
      :base09 "#d3869b"
      :base0A "#fabd2f"
      :base0B "#b8bb26"
      :base0C "#b8bb26"
      :base0D "#8ec07c"
      :base0E "#fb4934"
      ;; Don't know which color to use for now.  It's brown (or dark orange),
      ;; maybe gonna change this after I've seen where it is used.
      :base0F "#d65d0e")
    "Colors for Toki Base16 Gruvbox.")

(deftheme toki-base16-gruvbox)
(base16-theme-define 'toki-base16-gruvbox toki-base16-gruvbox-colors)
(provide-theme 'toki-base16-gruvbox)

(provide 'toki-base16-gruvbox-theme)

;;; toki-base16-gruvbox-theme.el ends here
