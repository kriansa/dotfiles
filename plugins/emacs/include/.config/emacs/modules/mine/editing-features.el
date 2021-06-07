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
  (dolist (item '(evil-switch-to-windows-last-buffer
                   counsel-projectile-switch-to-buffer
                   magit
                   magit-status
                   treemacs))
    (add-to-list 'super-save-triggers item))

  ;; Disable autosave on idle
  (setq super-save-auto-save-when-idle nil)

  ;; Disable native auto-backup files
  (setq make-backup-files nil)

  ;; Disable native auto-save-mode
  (setq auto-save-default nil)

  (super-save-mode +1))

;; expand-region allows us to keep expanding the current selection by sexps
(use-package expand-region :ensure t)

;; Enable usage of dead keys on non-US keyboards
;; See: https://www.emacswiki.org/emacs/DeadKeys
(use-package iso-transl)

;; Automatically detect the indentation style
(use-package dtrt-indent
  :ensure t
  :hook (prog-mode . dtrt-indent-mode)
  :config
  (setq dtrt-indent-max-lines 100)
  (setq dtrt-indent-verbosity 0))

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
  ;; Set character replacements
  (setq whitespace-display-mappings
    ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
    '((space-mark 32 [183] [46]) ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
      (newline-mark 10 [172 10]) ; LF is replaced by a "¬"
      (tab-mark 9 [9656 32 32] [92 32 32]))) ; tab is replaced by a "▸  "

  ;; Which characters we want to translate
  ;; add spaces space-mark to the list to see spaces
  (setq whitespace-style '(face tabs newline tab-mark newline-mark)))

;; Use vdiff (a package that is like vimdiff)
(use-package vdiff
  :ensure t
  :config
  ;; Whether to lock scrolling by default when starting vdiff
  (setq vdiff-lock-scrolling t))

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
  :hook (prog-mode . yas-minor-mode)
  :ensure t)

(use-package yasnippet-snippets
  :ensure t
  :after yasnippet
  :config
  (yas-reload-all))

;; Autocompletion framework
(use-package company
  :ensure t
  :config
  (defun mine/company-backend-with-yas (backends)
    "Add :with company-yasnippet to company BACKENDS.
Taken from https://github.com/syl20bnr/spacemacs/pull/179."
    (if (and (listp backends) (memq 'company-yasnippet backends))
      backends
      (append (if (consp backends)
                backends
                (list backends))
        '(:with company-yasnippet))))

  ;; add yasnippet to all backends
  (setq company-backends (mapcar #'mine/company-backend-with-yas company-backends))

  ;; Allow faster completions
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.0)

  (global-company-mode))

;; LSP (Language Server Protocol) support
(use-package lsp-mode
  :ensure t
  :after flycheck
  :hook ((enh-ruby-mode . lsp)
          (web-mode . lsp)
          (sh-mode . lsp)
          (js-mode . lsp))
  :commands lsp
  :config
  ;; Performance optimizations
  (setq read-process-output-max (* 1024 1024)) ;; 1mb
  (setq lsp-completion-provider :capf)
  (setq lsp-idle-delay 0.500)

  ;; Use a separate path for the session file
  (setq lsp-session-file (expand-file-name ".tmp/lsp-session-v1" user-emacs-directory))
  ;; Use projectile to guess the root path
  (setq lsp-auto-guess-root t)
  ;; Enable snippets
  (setq lsp-enable-snippet t)
  ;; Times out after 5s
  (setq lsp-response-timeout 5)
  ;; Don't make changes I didn't explicitely told you to
  (setq lsp-before-save-edits nil)
  ;; Disable the headerline breadcrumb
  (setq lsp-headerline-breadcrumb-enable nil)

  ;; Disable integration with flycheck
  ;; Currently, using lsp as a backend checker for flycheck, will prevent other checkers from
  ;; running, in which case they are a better option than the LSP one.
  ;; See: https://github.com/flycheck/flycheck/issues/1762
  (setq lsp-diagnostics-provider :none)

  ;; Disable symbol highlighting
  (setq lsp-enable-symbol-highlighting nil)
  ;; Disable showing docs on hover
  (setq lsp-eldoc-enable-hover nil))

;;; editing-features.el ends here
