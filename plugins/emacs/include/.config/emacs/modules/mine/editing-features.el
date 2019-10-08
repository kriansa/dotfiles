;;; editing-features.el --- Several editor features
;;;
;;; Commentary:
;;; This file contains new features that I found to make my life easier.
;;;
;;; Code:

(provide 'mine/editing-features)

(defun mine/disable-tabs ()
  "Disable tabs."
  (interactive)
  (global-unset-key (kbd "TAB"))
  (setq-default indent-tabs-mode nil))

;; Automatically save files when switching buffers
(use-package super-save
  :ensure t
  :config
  (super-save-mode +1)
  (setq super-save-auto-save-when-idle t)

  ; Disable auto-backup files
  (setq make-backup-files nil)

  ;; Disable the native auto-save-mode
  (setq auto-save-default nil))

;; expand-region allows us to keep expanding the current selection by sexps
(use-package expand-region :ensure t)

(use-package smartparens
  :ensure t
  :config
  (require 'smartparens-config)

  (defun mine/smartparens-mode ()
    "Init smartparens mode."
    (interactive)
    (smartparens-mode)
    (show-smartparens-mode))

  (add-hook 'prog-mode-hook 'mine/smartparens-mode)
  (add-hook 'text-mode-hook 'mine/smartparens-mode))

;; Whitespaces settings
;; ====================
;; This enables Emacs to display hidden characters such as spaces and tabs with
;; visible characters in the code

(use-package whitespace
  :ensure t
  :config
  ;; Which characters we want to translate
  ;; add spaces space-mark to the list to see spaces
  (setq whitespace-style '(face tabs newline tab-mark newline-mark))

  ;; Set character replacements
  (setq whitespace-display-mappings
    ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
    '((space-mark 32 [183] [46]) ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
      (newline-mark 10 [172 10]) ; LF is replaced by a "¬"
      (tab-mark 9 [9656 32 32] [92 32 32]))) ; tab is replaced by a "▸  "

  (add-hook 'prog-mode-hook 'whitespace-mode)
  (add-hook 'text-mode-hook 'whitespace-mode))

;; Right margin bar
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

;; Flycheck
(use-package flycheck
  :ensure t
  :config
  ;; Workaround for bug that Flycheck sometimes takes too much time looking for an ESLint
  ;; installation
  (advice-add 'flycheck-eslint-config-exists-p :override (lambda() t))

  ;; Add eslint for web-mode
  (flycheck-add-mode 'css-stylelint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-next-checker 'javascript-eslint 'css-stylelint)

  ;; Makes flycheck faster
  (setq flycheck-check-syntax-automatically '(idle-change mode-enabled save))
  (setq flycheck-idle-change-delay 4)
  (global-flycheck-mode))

;; LSP (Language Server Protocol) support
(use-package lsp-mode
  :ensure t
  :hook (prog-mode . lsp)
  :commands lsp
  :config
  ;; Use projectile to guess the root path
  (setq lsp-auto-guess-root t)
  ;; Disable snippets
  (setq lsp-enable-snippet nil))

;; Autocompletion framework
(use-package company
  :ensure t
  :config
  (global-company-mode))

(use-package company-lsp
  :after lsp-mode company-mode
  :commands company-lsp
  :config
  (push 'company-lsp company-backends))
;;; editing-features.el ends here
