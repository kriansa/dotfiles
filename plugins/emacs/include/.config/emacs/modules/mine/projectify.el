;;; projectify.el --- Project setup
;;;
;;; Commentary:
;;; This file should keep project configuration, i.e. how should Emacs
;;; deal with projects.
;;;
;;; Code:

(provide 'mine/projectify)

(use-package treemacs
  :ensure t
  :config
  (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
        treemacs-deferred-git-apply-delay      0.5
        treemacs-display-in-side-window        t
        treemacs-eldoc-display                 t
        treemacs-file-event-delay              5000
        treemacs-file-follow-delay             0.2
        treemacs-follow-after-init             t
        treemacs-git-command-pipe              ""
        treemacs-goto-tag-strategy             'refetch-index
        treemacs-indentation                   2
        treemacs-indentation-string            " "
        treemacs-is-never-other-window         nil
        treemacs-max-git-entries               5000
        treemacs-missing-project-action        'ask
        treemacs-no-png-images                 nil
        treemacs-no-delete-other-windows       nil
        treemacs-project-follow-cleanup        nil
        treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
        treemacs-position                      'left
        treemacs-recenter-distance             0.1
        treemacs-recenter-after-file-follow    nil
        treemacs-recenter-after-tag-follow     nil
        treemacs-recenter-after-project-jump   'always
        treemacs-recenter-after-project-expand 'on-distance
        treemacs-show-cursor                   nil
        treemacs-show-hidden-files             t
        treemacs-silent-filewatch              nil
        treemacs-silent-refresh                nil
        treemacs-sorting                       'alphabetic-desc
        treemacs-space-between-root-nodes      t
        treemacs-tag-follow-cleanup            t
        treemacs-tag-follow-delay              1.5
        treemacs-width                         35)

  ;; Enable follow mode (updates the tree according to the selected file)
  (treemacs-follow-mode t)

  ;; Filewatch mode will watch for file modifications then update the tree
  (treemacs-filewatch-mode t)

  ;; Fringe indicator is a indicator on the currently selected file
  (treemacs-fringe-indicator-mode t)

  ;; Use deferred git mode whenever possible, otherwise fallback to simple mode
  (pcase (cons (not (null (executable-find "git")))
               (not (null treemacs-python-executable)))
    (`(t . t)
     (treemacs-git-mode 'deferred))
    (`(t . _)
     (treemacs-git-mode 'simple))))

(use-package treemacs-evil :ensure t :after treemacs evil)
(use-package treemacs-projectile :ensure t :after treemacs projectile)
(use-package treemacs-magit :ensure t :after treemacs magit)

;; Ivy-mode
(use-package swiper
  :ensure t
  :after counsel ivy
  :config
  ;; Ensure compatibility with magit
  (if (fboundp 'magit-status)
    (setq magit-completing-read-function 'ivy-completing-read))

  ;; Make ag works with hidden files
  (setq-default counsel-ag-base-command "ag --nocolor --nogroup --hidden --ignore .git %s")

  (ivy-mode 1)
  (counsel-mode 1))

;; Projectile
(use-package projectile
  :ensure t
  :config
  ;; Settings
  (setq projectile-completion-system 'ivy)

  ;; Sorting only works if we use 'hybrid as the indexing method. On large projects, it may affect
  ;; negatively the performance, so beware. If needed, one can set per-project (or folder) variables
  ;; using Directory Variables.
  ;; See: https://github.com/bbatsov/projectile/blob/master/doc/projects.md#storing-project-settings
  (setq projectile-sort-order 'recently-active)
  (setq projectile-indexing-method 'hybrid)

  (add-to-list 'projectile-globally-ignored-files ".DS_Store")

  ;; Enable it globally
  (projectile-mode))

;; Ivy-mode & Projectile integration
(use-package counsel-projectile
  :ensure t
  :after projectile counsel
  :config
  (counsel-projectile-mode))

;; Editorconfig (requires install of core editorconfig package)
(use-package editorconfig
  :ensure t
  :config
  (editorconfig-mode 1))

;;; projectify.el ends here
