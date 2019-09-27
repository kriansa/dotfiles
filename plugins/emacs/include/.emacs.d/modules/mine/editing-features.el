;;; editing-features.el --- Several editor features
;;;
;;; Commentary:
;;; This file contains new features that I found to make my life easier.
;;;
;;; Code:

(provide 'mine/editing-features)

;; expand-region allows us to keep expanding the current selection by sexps
(use-package expand-region :ensure t)

;; Flycheck
(use-package flycheck
  :ensure t
  :config
  (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t))
  (setq flycheck-check-syntax-automatically '(idle-change mode-enabled save))
  (setq flycheck-idle-change-delay 4)
  (add-hook 'after-init-hook #'global-flycheck-mode))

(use-package lsp-mode
  :ensure t
  :hook (prog-mode . lsp)
  :commands lsp
  :config
  ;; Use projectile to guess the root path
  (setq lsp-auto-guess-root t)
  ;; Disable snippets
  (setq lsp-enable-snippet nil))

(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-lsp
  :after lsp-mode company-mode
  :commands company-lsp
  :config
  (push 'company-lsp company-backends))
;;; editing-features.el ends here
