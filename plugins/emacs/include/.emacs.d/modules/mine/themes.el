;;; themes.el --- Themes customization
;;;
;;; Commentary:
;;; This should store all themes and its customizations.
;;;
;;; Code:

(provide 'mine/themes)

(use-package doom-themes
  :defer t
  :config
  ;; Set theme for treemacs
  (setq doom-themes-treemacs-theme "doom-colors")
  (setq doom-themes-treemacs-enable-variable-pitch nil)
  (doom-themes-treemacs-config)

  (custom-set-faces
    ; Fix unchanged face so that it doesn't conflict with show-paren-mode
    ; See: https://github.com/syohex/emacs-git-gutter/issues/150
    `(git-gutter:unchanged ((t (:background nil :inherit 'default))))

    ;; Fix all other faces so they don't get a background when we select a full
    ;; line (shift-V)
    `(git-gutter:modified ((t (:background nil :inherit 'default))))
    `(git-gutter:added ((t (:background nil :inherit 'default))))
    `(git-gutter:deleted ((t (:background nil :inherit 'default)))))

  ;; Corrects (and improves) org-mode's native fontification.
  (doom-themes-org-config))

(use-package zenburn-theme
  :defer t
  :config
  ;;; Custom theme faces
  (zenburn-with-color-variables
    (custom-theme-set-faces
      'zenburn

      ;; Fringe
      `(fringe ((t (:foreground ,zenburn-fg :background ,zenburn-bg))))

      ;; Whitespace
      `(whitespace-space ((t (:foreground nil :background nil :inherit 'whitespace-newline))))
      `(whitespace-empty ((t (:foreground nil :background nil :inherit 'whitespace-newline))))
      `(whitespace-tab ((t (:foreground nil :background nil :inherit 'whitespace-newline))))
      `(whitespace-empty ((t (:foreground nil :background nil :inherit 'whitespace-newline))))
      ; `(whitespace-newline ((t (:foreground ,atom-one-dark-gray :background ,atom-one-dark-bg))))

      ;; Git-gutter
      `(git-gutter:unchanged ((t (:foreground nil :background nil :inherit 'default))))
      `(git-gutter:modified ((t (:foreground ,zenburn-orange :background nil :inherit 'default))))
      `(git-gutter:added ((t (:foreground ,zenburn-green+1 :background nil :inherit 'default))))
      `(git-gutter:deleted ((t (:foreground ,zenburn-red-1 :background nil :inherit 'default)))))))

(use-package atom-one-dark-theme
  :defer t
  :config
  (atom-one-dark-with-color-variables
    (custom-theme-set-faces
      'atom-one-dark

      ;; whitespace-mode
      `(whitespace-space ((t (:foreground ,atom-one-dark-gray :background ,atom-one-dark-bg))))
      `(whitespace-empty ((t (:foreground ,atom-one-dark-gray :background ,atom-one-dark-bg))))
      `(whitespace-tab ((t (:foreground ,atom-one-dark-gray :background ,atom-one-dark-bg))))
      `(whitespace-empty ((t (:foreground ,atom-one-dark-gray :background ,atom-one-dark-bg))))
      `(whitespace-newline ((t (:foreground ,atom-one-dark-gray :background ,atom-one-dark-bg))))

      ;; show-paren-mode
      `(show-paren-match ((t (:foreground ,atom-one-dark-red-1 :background ,atom-one-dark-bg
                               :underline t))))

      ;; git-gutter
      `(git-gutter:modified ((t (:foreground ,atom-one-dark-orange-1 :background nil :inherit 'default))))
      `(git-gutter:added ((t (:foreground ,atom-one-dark-green :background nil :inherit 'default))))
      `(git-gutter:deleted ((t (:foreground ,atom-one-dark-red-1 :background nil :inherit 'default)))))))

;;; themes.el ends here
