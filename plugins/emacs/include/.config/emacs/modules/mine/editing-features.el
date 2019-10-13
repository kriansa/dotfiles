;;; editing-features.el --- Several editor features
;;;
;;; Commentary:
;;; This file contains new features that I found to make my life easier.
;;;
;;; Code:

(provide 'mine/editing-features)

;; Automatically save files when switching buffers
(use-package super-save
  :ensure t
  :config
  ;; Add commands to auto-save hook
  (add-to-list 'super-save-triggers 'evil-switch-to-windows-last-buffer)

  (setq super-save-auto-save-when-idle t)

  ;; Disable native auto-backup files
  (setq make-backup-files nil)

  ;; Disable native auto-save-mode
  (setq auto-save-default nil)

  (super-save-mode +1))

;; Saves a list of recent opened files
(use-package recentf
  :ensure t
  :config
  (setq recentf-save-file (expand-file-name ".tmp/recentf" user-emacs-directory))
  (setq recentf-max-saved-items 500)
  (recentf-mode))

;; expand-region allows us to keep expanding the current selection by sexps
(use-package expand-region :ensure t)

(use-package smartparens
  :ensure t
  :hook ((prog-mode text-mode) . mine/smartparens-mode)
  :init
  (require 'smartparens-config)

  (defun mine/smartparens-mode ()
    "Init smartparens mode."
    (interactive)
    (smartparens-mode)
    (show-smartparens-mode)))

;; Whitespaces settings
;; ====================
;; This enables Emacs to display hidden characters such as spaces and tabs with
;; visible characters in the code

(use-package whitespace
  :hook ((prog-mode text-mode) . whitespace-mode)
  :init
  ;; Which characters we want to translate
  ;; add spaces space-mark to the list to see spaces
  (setq whitespace-style '(face tabs newline tab-mark newline-mark))

  ;; Set character replacements
  (setq whitespace-display-mappings
    ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
    '((space-mark 32 [183] [46]) ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
      (newline-mark 10 [172 10]) ; LF is replaced by a "¬"
      (tab-mark 9 [9656 32 32] [92 32 32]))) ; tab is replaced by a "▸  "
)

;; Right margin bar
(use-package display-fill-column-indicator
  :unless (version< emacs-version "27")
  :hook (prog-mode . display-fill-column-indicator-mode))

;; Flycheck
(use-package flycheck
  :ensure t
  :config
  ;; Workaround for bug that Flycheck sometimes takes too much time looking for an ESLint
  ;; installation
  (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t))

  ;; Use eslint_d
  (setq flycheck-javascript-eslint-executable "eslint_d")

  ;; Add eslint for web-mode
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'css-stylelint 'web-mode)
  (flycheck-add-next-checker 'javascript-eslint 'css-stylelint)

  (add-hook 'web-mode-hook
    (defun mine/set-web-mode-flycheck-checkers ()
      "Set eslint as the manually selected checker for Web-mode."
      (interactive)
      (setq flycheck-checker 'javascript-eslint)))

  ;; Makes flycheck faster
  (setq flycheck-check-syntax-automatically '(idle-change mode-enabled save))
  (setq flycheck-idle-change-delay 4)
  (global-flycheck-mode))

;; Snippets
(use-package yasnippet
  ;; Hooks to prog-mode are already setup by lsp-mode
  :ensure t)

(use-package yasnippet-snippets
  :ensure t
  :config
  (yas-reload-all))

;; Autocompletion framework
(use-package company
  :ensure t
  :config
  (global-company-mode))

;; LSP (Language Server Protocol) support
(use-package lsp-mode
  :ensure t
  :hook (prog-mode . lsp)
  :commands lsp
  :config
  ;; Use a separate path for the session file
  (setq lsp-session-file (expand-file-name ".tmp/lsp-session-v1" user-emacs-directory))
  ;; Use projectile to guess the root path
  (setq lsp-auto-guess-root t)
  ;; Disable Flymake
  (setq lsp-prefer-flymake :none)
  ;; Enable snippets
  (setq lsp-enable-snippet t)
  ;; Times out after 5s
  (setq lsp-response-timeout 5)
  ;; Don't make changes I don't explicitely told you to
  (setq lsp-before-save-edits nil)

  ;; Disable symbol highlighting
  (setq lsp-enable-symbol-highlighting nil)
  ;; Disable showing docs on hover
  (setq lsp-eldoc-enable-hover nil)
  ;; Disable showing symbol all signatures for a given method. Instead, shows only the current one
  (setq lsp-signature-render-all nil))

(use-package company-lsp
  :ensure t
  :commands company-lsp)

;;; editing-features.el ends here
