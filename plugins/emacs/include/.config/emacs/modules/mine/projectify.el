;;; projectify.el --- Project setup
;;;
;;; Commentary:
;;; This file should keep project configuration, i.e. how should Emacs
;;; deal with projects.
;;;
;;; Code:

(provide 'mine/projectify)

;;; This block is the mini-module that makes the thin integration between Treemacs and Projectile.
;;; Whenever there's a project change on Projectile, its tree will show up on Treemacs.

(defun mine/treemacs-set-single-project-to-default-workspace (project-name project-path)
  "Set a single project named PROJECT-NAME in the PROJECT-PATH to the default workspace."
  (treemacs-block
    (treemacs--invalidate-buffer-project-cache)
    (-let
      [file-content (concat "* Default\n" "** " project-name "\n" " - path :: " project-path "\n")]
      (f-write file-content 'utf-8 treemacs-persist-file)
      (treemacs--restore)
      (treemacs--find-workspace)
      (treemacs--consolidate-projects)
      (run-hooks 'treemacs-workspace-edit-hook))))

(defvar last-selected-project nil
  "This is the last project that was previously selected.")

(defun mine/hook-buffer-switched (&rest _)
  "This is called when Emacs switch between any buffers."
  (let ((currently-selected-project (projectile-project-root)))
    (when (and buffer-file-name (not (eq last-selected-project currently-selected-project)))
      (setq last-selected-project currently-selected-project)
      (mine/hook-project-switched currently-selected-project))))

(defun mine/hook-project-switched (project-path)
  "This is called when we detect a project switch.  The new selected project is PROJECT-PATH."
  (let ((project-name (projectile-project-name project-path)))
    (mine/treemacs-set-single-project-to-default-workspace project-name project-path)))

(use-package projectile
  :defer t
  :config
  ;; Add hook for when the buffer is switched
  (advice-add 'select-window :after 'mine/hook-buffer-switched)
  (add-hook 'window-buffer-change-functions 'mine/hook-buffer-switched))

;;; projectile + treemacs integration ends here.

;; Use desktop-mode to store sessions
(use-package desktop
  :config
  (setq desktop-path (list (expand-file-name ".tmp" user-emacs-directory)))
  (desktop-save-mode 1))

;; Saves a list of recent opened files
(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name ".tmp/recentf" user-emacs-directory))
  (add-to-list 'recentf-exclude (regexp-quote (concat (file-truename (expand-file-name user-emacs-directory)) ".tmp")))
  (add-to-list 'recentf-exclude (regexp-quote (concat (file-truename (expand-file-name user-emacs-directory)) "elpa")))
  (setq recentf-max-saved-items 500)
  (recentf-mode))


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
        treemacs-persist-file                  (expand-file-name ".tmp/treemacs-persist" user-emacs-directory)
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

  ;; Hide asterisk buffers by default
  (setq ivy-ignore-buffers (append ivy-ignore-buffers `("^\*" "^magit[-a-zA-Z0-9]*:")))

  (ivy-mode 1)
  (counsel-mode 1))

;; Projectile
(use-package projectile
  :ensure t
  :config
  ;; Settings
  (setq projectile-completion-system 'ivy)
  (setq projectile-known-projects-file
    (expand-file-name ".tmp/projectile-bookmarks.eld" user-emacs-directory))

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
