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
  ;; See below to understand evil-magit shortcuts
  ;; https://github.com/emacs-evil/evil-magit
  (use-package evil-magit :ensure t))

;; Git-gutter
(use-package git-gutter
  :ensure t
  :config

  ; Change gutter characters
  (setq
    git-gutter:modified-sign "~"
    git-gutter:added-sign "+"
    git-gutter:deleted-sign "_"
    ; This character is a exotic space (en-space) and its purpose is that
    ; it shows an invisible character instead of a visible one, or even a common
    ; space. If we would use a normal space character, then our "Whitespace"
    ; configuration would display a "point" instead. Emacs weirdly shows
    ; whitespace substituitions on the line-num bar.
    git-gutter:unchanged-sign "\u2002")

  ; Fix unchanged face so that it doesn't conflict with show-paren-mode
  ; See: https://github.com/syohex/emacs-git-gutter/issues/150
  (set-face-attribute 'git-gutter:unchanged nil :background nil :inherit 'default)

  ; And finally enable it globally
  (global-git-gutter-mode t))

;; Disable Emacs native Git client
(setq vc-handled-backends nil)

;;; git.el ends here
