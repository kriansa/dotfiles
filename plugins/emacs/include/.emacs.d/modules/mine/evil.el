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
  :ensure t
  :init
  ; evil settings
  (setq evil-want-C-u-scroll t)
  (setq evil-want-Y-yank-to-eol t)
  (fset 'evil-visual-update-x-selection 'ignore)

  ; This is required by evil-collection
  (setq evil-want-keybinding nil)

  :config
  (use-package anzu :ensure t)
  (use-package evil-anzu :ensure t)
  (use-package evil-args :ensure t)

  ;; Required packages
  (use-package undo-tree :ensure t)
  (use-package goto-chg :ensure t)
  (use-package evil-numbers :ensure t)

  ;; Evil collection is collection of evil mappings for the entire Emacs
  (use-package evil-collection
    :ensure t
    :config
    (evil-collection-init))

  (use-package evil-matchit
    :ensure t
    :config
    (setq evilmi-ruby-extract-keyword-howtos
      '(("^[ \t]*[^ \t=]+[ \t]*=[ \t]*\\([a-z]+\\)\\( .*\\|(.*\\| *\\)$" 1)
         ("^[ \t]*\\([a-z]+\\)\\( .*\\|(.*\\| *\\)$" 1)
         ("^.* \\(do\\) |[a-z0-9A-Z_, *]+| *$" 1)
         ("^.* \\(do\\) *$" 1)
         ("^.* \\(begin\\) *$" 1)
         ("^.* \\(end\\)\\..*$" 1)))

    (global-evil-matchit-mode t))

  (use-package evil-commentary
    :ensure t
    :config
    (evil-commentary-mode))

  (use-package evil-snipe
    :ensure t
    :config
    (evil-snipe-mode t)
    (evil-snipe-override-mode t)
    ;; Avoid issues with magit-mode buffers
    (add-hook 'magit-mode-hook 'turn-off-evil-snipe-override-mode))

  (use-package evil-surround
    :ensure t
    :config
    (global-evil-surround-mode t))

  (use-package evil-goggles
    :ensure t
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

  ;; Turn on evil-mode globally
  (evil-mode t))

;;; evil.el ends here
