;;; package-setup.el --- Emacs package configuration
;;;
;;; Commentary:
;;; Configure the setup of use-package
;;;
;;; Code:
(provide 'mine/package-setup)

; Repositories
(setq package-archives
  '(("melpa" . "https://melpa.org/packages/")
     ("nongnu" . "https://elpa.nongnu.org/nongnu/")
     ("gnu" . "https://elpa.gnu.org/packages/")))

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
    auto-package-update-interval 1
    auto-package-update-last-update-day-filename ".tmp/last-package-update-day")
  :config
  (defun mine/auto-update-when-open-frame (frame)
    "Automatically invoke auto-update when frame is open"
    (interactive)
    (with-selected-frame frame
      (auto-package-update-maybe)))

  (add-hook 'after-make-frame-functions 'mine/auto-update-when-open-frame)
  (when (not (daemonp)) (mine/auto-update-when-open-frame (selected-frame))))

;;; package-setup.el ends here
