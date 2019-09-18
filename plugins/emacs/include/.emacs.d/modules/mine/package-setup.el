;;; package-setup.el --- Emacs package configuration
;;;
;;; Commentary:
;;; Configure the setup of use-package
;;;
;;; Code:
(provide 'mine/package-setup)

; Repositories
(setq package-archives
  '(("melpa"       . "https://melpa.org/packages/")
    ("gnu"         . "https://elpa.gnu.org/packages/")
    ("org"         . "https://orgmode.org/elpa/")))

;; Prevent emacs to load all packages at startup
(setq package-enable-at-startup nil)

;; Ensure we have use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

;; Auto-updater
(use-package auto-package-update
   :ensure t
   :config
   (setq auto-package-update-prompt-before-update t
         auto-package-update-delete-old-versions t
         auto-package-update-interval 4)
   (auto-package-update-maybe))

;; Workaround for `error: Package undo-tree-0.6.3 is unavailable`
;; See: https://github.com/bbatsov/prelude/issues/1225
 (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")

;;; package-setup.el ends here
