;;; autosave.el --- Configure emacs to autosave files
;;;
;;; Commentary:
;;; This plugin autosaves every file upon edit.
;;;
;;; Code:

(provide 'mine/autosave)

(use-package super-save
  :ensure t
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t)

  ;; Disable the native auto-save-mode
  (setq auto-save-default nil))

;;; autosave.el ends here
