;;; major-modes.el --- Major modes settings
;;;
;;; Commentary:
;;; Customizes all new major modes (usually, those providing a new filetype support)
;;;
;;; Code:

(provide 'mine/major-modes)

; Initial mode
(setq-default major-mode 'text-mode)

;; By default, use _ as part of the words on all prog modes
(use-package prog-mode
  :config
  (add-hook 'prog-mode-hook
    (defun mine/set-word-boundaries-default ()
      (modify-syntax-entry ?_ "w"))))

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
(use-package groovy-mode :ensure t)
(use-package lua-mode :ensure t :mode "\\.lua\\'" :interpreter "lua")
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
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode)))

(use-package js2-mode
  :ensure t
  :hook (js-mode . js2-minor-mode)
  :config
  ;; Disable js2-mode linting because we're using Flycheck already
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil))

(use-package json-mode
  :ensure t)

(use-package go-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode)))

(use-package sh-script
  :ensure t
  :config
  ;; Use sh-mode for .env files
  (add-to-list 'auto-mode-alist '("\\.env\\'" . sh-mode))

  ;; Add highlight to bash constructs
  (use-package smartparens
    :config
    (sp-local-pair 'sh-mode "if" "fi" :actions '(navigate))
    (sp-local-pair 'sh-mode "for" "done" :actions '(navigate))
    (sp-local-pair 'sh-mode "while" "done" :actions '(navigate)))

  ;; When saving a file that starts with `#!', make it executable.
  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p))

;;; major-modes.el ends here
