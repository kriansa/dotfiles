;;; init.el --- Emacs init file
;;;
;;; Commentary:
;;; This file is loaded whenever Emacs start.
;;;
;;; Code:

;; Load Emacs Package system and add "modules" to the load-path
(package-initialize)
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Load my own package configurations
(require 'mine/package-setup)

;; Load my functions lib
(require 'mine/functions)

;; Load my features setup
(require 'mine/evil)
(require 'mine/git)
(require 'mine/autosave)
(require 'mine/projectify)
(require 'mine/major-modes)
(require 'mine/editing-features)

;; Load customizations
(require 'mine/settings)
(require 'mine/themes)
(require 'mine/ui)
(require 'mine/shortcuts)

;; Keep emacs Custom-settings in a separate file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file)

;;; init.el ends here
