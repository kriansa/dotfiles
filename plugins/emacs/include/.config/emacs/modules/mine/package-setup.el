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
    ("gnu"         . "https://elpa.gnu.org/packages/")))

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
  :init
  (setq
    auto-package-update-prompt-before-update t
    auto-package-update-delete-old-versions t
    auto-package-update-interval 4
    auto-package-update-last-update-day-filename ".tmp/last-package-update-day")

  (auto-package-update-maybe))

;;; package-setup.el ends here
