;;; shortcuts.el --- Custom shortcuts file
;;;
;;; Commentary:
;;; This file has every shortcut I use on Emacs.
;;;
;;; Code:

(provide 'mine/shortcuts)

(use-package evil
  :config
  ;; Disable shortcuts that aren't helpful
  (global-unset-key (kbd "M-c"))
  (global-unset-key (kbd "s-h"))
  (global-unset-key (kbd "s-j"))
  (global-unset-key (kbd "s-k"))
  (global-unset-key (kbd "s-l"))

  ;; These shortcut removals is to mimic closely Vim's behavior
  (global-unset-key (kbd "C-h"))
  (global-unset-key (kbd "C-l"))
  (global-unset-key (kbd "C-j"))
  (global-unset-key (kbd "C-k"))
  (global-unset-key (kbd "C-/"))

  ;; Window navigation
  (define-key evil-normal-state-map (kbd "C-h") 'windmove-left)
  (define-key evil-normal-state-map (kbd "C-j") 'windmove-down)
  (define-key evil-normal-state-map (kbd "C-k") 'windmove-up)
  (define-key evil-normal-state-map (kbd "C-l") 'windmove-right)
  ;; From within Treemacs
  (define-key evil-treemacs-state-map (kbd "C-h") 'windmove-left)
  (define-key evil-treemacs-state-map (kbd "C-l") 'windmove-right)

  ;; Window management
  (define-key evil-normal-state-map (kbd "SPC w") 'kill-this-buffer)
  (define-key evil-normal-state-map (kbd "SPC q") 'delete-window)
  (define-key evil-normal-state-map (kbd "SPC t") 'mine/toggle-maximize-buffer)

  ;; Increase/decrease font size
  (define-key global-map (kbd "s-=") 'text-scale-increase)
  (define-key global-map (kbd "s-+") 'text-scale-increase)
  (define-key global-map (kbd "s--") 'text-scale-decrease)
  (define-key global-map (kbd "s-_") 'text-scale-decrease)
  (define-key global-map (kbd "s-0") 'mine/reset-text-size)
  (define-key global-map (kbd "s-)") 'mine/reset-text-size)

  ;; Easy add line above/below
  (define-key evil-normal-state-map (kbd "SPC O") 'mine/create-line-above)
  (define-key evil-normal-state-map (kbd "SPC o") 'mine/create-line-below)
  (define-key evil-normal-state-map (kbd "SPC o") 'mine/create-line-below)

  ;; Split line
  (define-key evil-normal-state-map (kbd "K") 'mine/split-line)

  ;; Select pasted content
  (define-key evil-normal-state-map (kbd "SPC v") 'mine/select-pasted)

  ;; Remap g-d to make it act just like C-[
  (define-key evil-normal-state-map (kbd "gd") 'evil-jump-to-tag))

(use-package enh-ruby-mode
  :config
  (add-hook 'enh-ruby-mode
    (defun mine/fix-enh-ruby-keybindings ()
      "Set the right keybindings for enh-ruby-mode."
      (define-key enh-ruby-mode-map (kbd "C-j") nil))))

(use-package evil-nerd-commenter
  :ensure t
  :config
  (define-key evil-normal-state-map (kbd "gcc") 'evilnc-comment-or-uncomment-lines)
  (define-key evil-visual-state-map (kbd "gc") 'evilnc-comment-or-uncomment-lines))

(use-package evil-numbers
  :config
  (define-key evil-normal-state-map (kbd "C-c a") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-c x") 'evil-numbers/dec-at-pt))

(use-package treemacs
  :config
  (define-key evil-normal-state-map ",," 'treemacs))

;; evil-args
(use-package evil-args
  :config
  (define-key evil-inner-text-objects-map "," 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "," 'evil-outer-arg))

;; Git-gutter
(use-package git-gutter
  :config
  ; Jump to next/previous hunk
  (define-key evil-normal-state-map "[c" 'git-gutter:previous-hunk)
  (define-key evil-normal-state-map "]c" 'git-gutter:next-hunk)

  ; Stage/unstage current hunk
  (define-key evil-normal-state-map (kbd "SPC h s") 'git-gutter:stage-hunk)
  (define-key evil-normal-state-map (kbd "SPC h u") 'git-gutter:revert-hunk))

;; Magit
(use-package magit
  :config
  (define-key evil-normal-state-map (kbd "SPC g s") 'magit-status))

;; Ivy, counsel & swiper
(use-package ivy
  :config
  ;; Define vim keys to navigate on ivy mini buffers
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "C-d") 'ivy-scroll-up-command)
  (define-key ivy-minibuffer-map (kbd "C-u") 'ivy-scroll-down-command)
  (define-key ivy-minibuffer-map (kbd "C-w") 'backward-kill-word)

  ;; On buffer switch buffers, use C-d to kill buffers
  (define-key ivy-switch-buffer-map (kbd "C-k") 'ivy-previous-line)
  (define-key ivy-switch-buffer-map (kbd "C-d") 'ivy-switch-buffer-kill)

  ;; Global keys
  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (define-key evil-normal-state-map (kbd "SPC SPC") 'mine/switch-buffer)
  (define-key evil-normal-state-map (kbd "SPC a") 'counsel-ag)
  (define-key evil-normal-state-map (kbd "SPC s") 'mine/search-word-under-cursor))

;; Ivy (counsel-projectile)
(use-package counsel-projectile
  :config
  ;; Switch from files in different projects
  (define-key evil-normal-state-map (kbd "C-S-p") 'counsel-projectile-switch-project)
  ;; Switch files in this project
  (define-key evil-normal-state-map (kbd "C-p") 'counsel-projectile-find-file))

(use-package expand-region
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

;;; shortcuts.el ends here
