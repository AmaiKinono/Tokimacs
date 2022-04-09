;;; toki-common.el --- Common library for site-lisps -*- lexical-binding: t -*-

;; Copyright (C) 2022 Hao Wang
;; License: GPL v3, or (at your option) any later version

;;; Commentary:

;;; Code:

;; To see the outline of this file, run `outline-minor-mode', then
;; `outline-hide-body'.  Another way is to run `occur' with the query:
;; ^;;;;* \|^(

(defmacro toki-declare-ext-pkg (feature)
  "Require a feature from external packages.
This is for site-lisps that requires external packages."
  `(eval-when-compile
     (add-to-list 'load-path ,(concat user-emacs-directory "straight/build/"
                                      (symbol-name feature)))))

(provide 'toki-common)

;;; toki-editing.el ends here
