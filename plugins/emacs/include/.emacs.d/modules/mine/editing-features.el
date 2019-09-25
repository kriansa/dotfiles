;;; editing-features.el --- Several editor features
;;;
;;; Commentary:
;;; This file contains new features that I found to make my life easier.
;;;
;;; Code:

(provide 'mine/editing-features)

;; expand-region allows us to keep expanding the current selection by sexps
(use-package expand-region :ensure t)

(use-package lsp-mode
  :ensure t
  :hook (prog-mode . lsp)
  :commands lsp)

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
