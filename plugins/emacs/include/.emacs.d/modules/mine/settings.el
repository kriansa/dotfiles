;;; settings.el --- Emacs settings
;;;
;;; Commentary:
;;; This file holds all Emacs settings customizations
;;;
;;; Code:

(provide 'mine/settings)

; Initial mode
(setq-default major-mode 'text-mode)

; No tabs!
(setq-default indent-tabs-mode nil)

; Disable auto-backup files
(setq make-backup-files nil)

; Enables auto-insert of matching pairs such as parenthesis or brackets
(electric-pair-mode 1)

; Supress dired warnings when using Mac/BSD ls
(when (eq system-type 'darwin)
  (require 'ls-lisp)
  (setq ls-lisp-use-insert-directory-program nil))

; Smooth scrolling like Vim
(setq scroll-step 1)
(setq scroll-margin 0)
(setq scroll-conservatively 9999)

;;; settings.el ends here
