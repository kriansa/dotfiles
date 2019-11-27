;;; major-modes.el --- Major modes settings
;;;
;;; Commentary:
;;; Customizes all new major modes (usually, those providing a new filetype support)
;;;
;;; Code:

(provide 'mine/major-modes)

; Initial mode
(setq-default major-mode 'text-mode)

;; Ensure we consider `_` as part of a word
(defun mine/set-word-boundaries ()
  "Define what is part of a word."
  (interactive)
  (modify-syntax-entry ?_ "w"))

(use-package beancount :mode ("\\.beancount\\'" . beancount-mode))
(use-package elixir-mode :ensure t)
(use-package gitattributes-mode :ensure t)
(use-package gitconfig-mode :ensure t)
(use-package gitignore-mode :ensure t)
(use-package markdown-mode :ensure t)
(use-package terraform-mode :ensure t)
(use-package hcl-mode :ensure t)
(use-package yaml-mode :ensure t)
(use-package typescript-mode :ensure t)
(use-package dockerfile-mode
  :ensure t
  :mode ("Dockerfile.*\\'" . dockerfile-mode))

(use-package enh-ruby-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist
    '("\\(?:\\.rb\\|ru\\|rake\\|thor\\|jbuilder\\|gemspec\\|podspec\\|/\\(?:Gem\\|Rake\\|Cap\\|Thor\\|Vagrant\\|Guard\\|Pod\\)file\\)\\'" . enh-ruby-mode))

  (add-hook 'enh-ruby-mode-hook
    (defun mine/set-ruby-word-boundaries ()
      "Define what is part of a word for Ruby."
      (interactive)
      (let ((char-list '( "_" "@" "!" "?" )))
        (dolist (char char-list)
          (modify-syntax-entry (string-to-char char) "w"))))))

(use-package rspec-mode
  :ensure t
  :config
  ;; Autosave buffer when running rspec
  (setq rspec-autosave-buffer t))

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
  (add-to-list 'auto-mode-alist '("\\.vue\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-hook 'web-mode-hook 'mine/set-word-boundaries))

(use-package go-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode)))

(use-package sh-script
  :ensure t
  :config
  ;; Use sh-mode for .env files
  (add-to-list 'auto-mode-alist '("\\.env\\'" . sh-mode))

  ;; When saving a file that starts with `#!', make it executable.
  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p))

;;; major-modes.el ends here
