;;; init.el --- Emacs init file
;;;
;;; Commentary:
;;; This file is loaded whenever Emacs start.
;;;
;;; Code:

;; Load Emacs Package system and add "modules" to the load-path
(add-to-list 'load-path (expand-file-name "modules" user-emacs-directory))

;; Allow 100MB of memory (instead of 0.76MB) before calling garbage collection. This means GC runs
;; less often, which speeds up some operations.
(setq gc-cons-threshold 100000000)

;; Keep emacs Custom-settings in a separate file
(setq custom-file (expand-file-name ".tmp/custom.el" user-emacs-directory))

;; Load my own package configurations
(require 'mine/package-setup)

;; Load my functions lib
(require 'mine/functions)

;; Load my features setup
(require 'mine/evil)
(require 'mine/git)
(require 'mine/projectify)
(require 'mine/major-modes)
(require 'mine/editing-features)
(require 'mine/org)

;; Load customizations
(require 'mine/themes)
(require 'mine/ui)
(require 'mine/shortcuts)

;;; init.el ends here
