;;; evil.el --- Evil and evil plugins setup
;;;
;;; Commentary:
;;; This file has every Evil plugin setup
;;;
;;; Code:

(provide 'mine/evil)

;; Tips of Evil for Vim users => https://github.com/noctuid/evil-guide
;; https://github.com/magnars/.emacs.d/blob/master/init.el
;; https://github.com/ninrod/dotfiles/tree/master/emacs

(use-package evil
  :straight t
  :init
  ; evil settings
  (setq evil-want-C-u-scroll t)
  (setq evil-want-Y-yank-to-eol t)
  (fset 'evil-visual-update-x-selection 'ignore)

  ; This is required by evil-collection
  (setq evil-want-keybinding nil)

  ;; Use emacs 28 native undo-redo plugin
  (setq evil-undo-system 'undo-redo)

  :config
  ;; Smooth scrolling like Vim
  (setq scroll-step 1)
  (setq scroll-margin 0)
  (setq scroll-conservatively 9999)

  ;; Disable showing state name at the echo area
  (setq evil-echo-state nil)

  ;; evil-emacs-state is annoying, the following function and hook automatically
  ;; switch back to evil-normal-state whenever the evil-emacs-state is entered.
  ;; It allows a more consistent navigation experience among all mode maps.
  ;; To enter special commands of custom mode maps, just enter the insert mode :-)
  ;; See: https://gist.github.com/syl20bnr/4447166
  (defun evil-emacs-state-2-evil-normal-state ()
    (evil-normal-state)
    (remove-hook 'post-command-hook 'evil-emacs-state-2-evil-normal-state))
  (add-hook 'evil-emacs-state-entry-hook
    (lambda ()
      (add-hook 'post-command-hook 'evil-emacs-state-2-evil-normal-state)))

  ;; Turn on evil-mode globally
  (evil-mode t))

(use-package evil
  :after projectile
  :defer t
  :config
  (evil-add-command-properties #'counsel-projectile-find-file :jump t)
  (evil-add-command-properties #'counsel-projectile-switch-project :jump t)
  (evil-add-command-properties #'counsel-projectile-switch-to-buffer :jump t)
  (evil-add-command-properties #'ivy-switch-buffer :jump t))

;; Packages to emulate closely Vim's cool features

(use-package anzu :straight t)
(use-package evil-anzu :straight t)
(use-package evil-args :straight t)

;; Required packages
(use-package goto-chg :straight t)
(use-package evil-numbers :straight t)

;; Evil collection is collection of evil mappings for the entire Emacs
(use-package evil-collection
  :straight t
  :config
  (evil-collection-init))

(use-package evil-matchit
  :straight t
  :config
  (global-evil-matchit-mode t))

(use-package evil-nerd-commenter :straight t)

(use-package evil-snipe
  :straight t
  :config
  (evil-snipe-mode t)
  (evil-snipe-override-mode t)
  ;; Avoid issues with magit-mode buffers
  (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode))

(use-package evil-surround
  :straight t
  :config
  (global-evil-surround-mode t))

(use-package evil-goggles
  :straight t
  :config
  (setq evil-goggles-duration 0.100
    evil-goggles-enable-delete nil
    evil-goggles-enable-indent nil
    evil-goggles-enable-yank t
    evil-goggles-enable-join nil
    evil-goggles-enable-fill-and-move nil
    evil-goggles-enable-paste nil
    evil-goggles-enable-shift nil
    evil-goggles-enable-surround nil
    evil-goggles-enable-commentary nil
    evil-goggles-enable-nerd-commenter nil
    evil-goggles-enable-replace-with-register nil
    evil-goggles-enable-set-marker nil)

  (evil-goggles-mode))

;;; evil.el ends here
