;; -*- lexical-binding: t; -*-

;; View mode that resembles vi normal mode (kind of).

;;; Commands

(defun toki-enable-view-mode ()
  "Enable view mode."
  (interactive)
  ;; We check it because `view-mode' mark the buffer as read-only, and a second
  ;; call to `view-mode' remembers that and restores it when disabled, then we
  ;; end up with a read-only buffer.
  (unless view-mode
    (view-mode)
    (message "View mode enabled.")))

(defun toki-disable-view-mode ()
  "Disable view-mode."
  (interactive)
  (view-mode -1)
  (message "View mode disabled."))

(defun toki-disable-view-mode-after-char ()
  "Move forward 1 char and disable view mode."
  (interactive)
  (forward-char)
  (view-mode -1))

(defun toki-push-mark ()
  "Set the mark but do not activate it."
  (interactive)
  (let ((mark (mark t)))
    (unless (= mark (point))
      (push-mark))))

(defun toki-scroll-up-half-page ()
  "Scroll up half page."
  (interactive)
  (let* ((lines (max (round (/ (window-body-height) 2)) 10)))
    (scroll-up lines)))

(defun toki-scroll-down-half-page ()
  "Scroll down half page."
  (interactive)
  (let* ((lines (max (round (/ (window-body-height) 2)) 10)))
    (scroll-down lines)))

;;; UI

(defun toki/view-mode-change-cursor-type ()
  "Change cursor type for view mode."
  (if view-mode
      (progn
        (setq old-cursor-type cursor-type)
        (setq cursor-type 'box))
    (setq cursor-type old-cursor-type)))

(add-hook 'view-mode-hook 'toki/view-mode-change-cursor-type)

;;; Keybinds

;; It's hard to bind ESC in terminal.  Evil does this but it's a rather complex
;; hack.  In terminal we use M-r (think of "read mode") to enable view-mode.
(general-def
  "<escape>" 'toki-enable-view-mode
  "M-r" 'toki-enable-view-mode)

(defvar toki-view-mode-leader-key "SPC"
  "Leader key used in view mode.")

(defvar toki-view-mode-2nd-leader-key ","
  "2nd leader key used in view mode.")

(with-eval-after-load 'view
  (setcdr view-mode-map nil)
  (general-def
    :keymaps 'view-mode-map
    ;; Leader key
    toki-view-mode-leader-key (general-key toki-leader-key)
    toki-view-mode-2nd-leader-key (general-key toki-2nd-leader-key)
    ;; Movement
    "h" 'backward-char
    "l" 'forward-char
    "j" 'next-line
    "k" 'previous-line
    "f" 'toki-forward-word
    "b" 'toki-backward-word
    "$" 'toki-end-of-line
    "^" 'toki-beginning-of-line
    "C-f" 'scroll-up-command
    "C-b" 'scroll-down-command
    "C-d" 'toki-scroll-up-half-page
    "C-u" 'toki-scroll-down-half-page
    "G" 'end-of-buffer
    "gg" 'beginning-of-buffer
    "m" 'toki-push-mark
    "'" 'pop-to-mark-command
    "/" 'isearch-forward-regexp
    "?" 'isearch-backward-regexp
    "gl" 'goto-line
    ;; Disable view mode
    "a" 'toki-disable-view-mode-after-char
    "i" 'toki-disable-view-mode))
