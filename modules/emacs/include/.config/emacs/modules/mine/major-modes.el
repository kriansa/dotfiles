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
(use-package elixir-mode :straight t)
(use-package git-modes
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.dockerignore\\'" . gitignore-mode)))
(use-package markdown-mode :straight t)
(use-package terraform-mode :straight t)
(use-package hcl-mode :straight t)
(use-package yaml-mode :straight t)
(use-package typescript-mode :straight t)
(use-package groovy-mode :straight t)
(use-package lua-mode :straight t :mode "\\.lua\\'" :interpreter "lua")
(use-package dockerfile-mode
  :straight t
  :mode ("Dockerfile.*\\'" . dockerfile-mode))

(use-package enh-ruby-mode
  :straight t
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

;; Configure flycheck for enh-ruby-mode
(use-package flycheck
  :defer t
  :after enh-ruby-mode
  :config
  (add-hook 'enh-ruby-mode-hook
    (defun mine/set-ruby-checker ()
      "Set the proper ruby checker based on the which config file the project has"
      (setq-local flycheck-checker
        (if (flycheck-locate-config-file '(".rubocop.yml") 'ruby-rubocop)
          'ruby-rubocop
          'ruby-standard)))))

;; Configure format-all for enh-ruby-mode
(use-package format-all
  :defer t
  :after enh-ruby-mode flycheck
  :config
  (add-hook 'enh-ruby-mode-hook
    (defun mine/set-ruby-formatter ()
      "Set the proper ruby formatter based on the which config file the project has"
      (setq-local format-all-formatters
        (if (flycheck-locate-config-file '(".rubocop.yml") 'ruby-rubocop)
          '(("Ruby" rubocop))
          '(("Ruby" standardrb)))))))

(use-package rspec-mode
  :straight t
  :config
  ;; Autosave buffer when running rspec
  (setq rspec-autosave-buffer t))

(use-package web-mode
  :straight t
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
  :straight t
  :hook (js-mode . js2-minor-mode)
  :config
  ;; Disable js2-mode linting because we're using Flycheck already
  (setq js2-mode-show-parse-errors nil)
  (setq js2-mode-show-strict-warnings nil))

(use-package json-mode :straight t)

(use-package go-mode
  :straight t
  :config
  (add-to-list 'auto-mode-alist '("\\.go\\'" . go-mode)))

(use-package sh-script
  :straight t
  :config
  ;; Use sh-mode for .env files
  (add-to-list 'auto-mode-alist '("\\.env\\'" . sh-mode))

  ;; When saving a file that starts with `#!', make it executable.
  (add-hook 'after-save-hook 'executable-make-buffer-file-executable-if-script-p))

;; Add highlight to bash constructs
(use-package smartparens
  :defer t
  :after sh-script
  :config
  (sp-local-pair 'sh-mode "if" "fi" :actions '(navigate))
  (sp-local-pair 'sh-mode "for" "done" :actions '(navigate))
  (sp-local-pair 'sh-mode "while" "done" :actions '(navigate)))


;;; major-modes.el ends here
