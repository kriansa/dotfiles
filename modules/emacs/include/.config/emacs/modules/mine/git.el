;;; init.el --- Git settings
;;;
;;; Commentary:
;;; This file sets up Git and its different plugins
;;;
;;; Code:

(provide 'mine/git)

;; Magit
(use-package magit
  :ensure t
  :config
  ;; Tune diffs
  (setq magit-diff-paint-whitespace-lines 'all)
  (setq magit-diff-paint-whitespace t)
  (setq magit-diff-refine-ignore-whitespace t)
  (setq magit-diff-refine-hunk 'all)
  (setq magit-diff-highlight-trailing t)

  ;; Set transient files to .tmp
  (setq transient-levels-file (expand-file-name ".tmp/transient-levels.el" user-emacs-directory))
  (setq transient-values-file (expand-file-name ".tmp/transient-values.el" user-emacs-directory))
  (setq transient-history-file (expand-file-name ".tmp/transient-history.el" user-emacs-directory)))

;; Display gutter for VCS modified files
(use-package diff-hl
  :defines diff-hl-margin-symbols-alist
  :ensure t
  :config
  (add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

  (setq diff-hl-margin-symbols-alist
    '((insert . "+") (delete . "‾") (change . "~")
       (unknown . "?") (ignored . " ")))

  (diff-hl-margin-mode)
  (diff-hl-flydiff-mode)
  (global-diff-hl-mode))

;;; git.el ends here
