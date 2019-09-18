;;; major-modes.el --- Major modes settings
;;;
;;; Commentary:
;;; Customizes all new major modes (usually, those providing a new
;;; filetype support)
;;;
;;; Code:

(provide 'mine/major-modes)

(use-package markdown-mode
  :ensure t
  :config
  ;; Adds asterisks as a pair to markdown-mode
  (defvar markdown-electric-pairs '((?* . ?*)) "Electric pairs for markdown-mode.")
  (defun mine/markdown-add-electric-pairs ()
    (setq-local electric-pair-pairs (append electric-pair-pairs markdown-electric-pairs))
    (setq-local electric-pair-text-pairs electric-pair-pairs))

  ;; Loads it only on markdown-mode
  (add-hook 'markdown-mode-hook 'mine/markdown-add-electric-pairs))

(use-package terraform-mode :ensure t)
(use-package hcl-mode :ensure t)
(use-package yaml-mode :ensure t)
(use-package dockerfile-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("Dockerfile.*\\'" . dockerfile-mode)))

(use-package enh-ruby-mode 
  :ensure t
  :config
  (add-to-list 'auto-mode-alist
    '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode)))

(use-package web-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode)))

;;; major-modes.el ends here
