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
  (global-unset-key (kbd "M-h"))
  (global-unset-key (kbd "M-j"))
  (global-unset-key (kbd "M-k"))
  (global-unset-key (kbd "M-l"))
  (global-unset-key (kbd "s-x"))
  (global-unset-key (kbd "M-d"))
  (global-unset-key (kbd "M-v"))

  ;; Undo-tree
  (define-key undo-tree-map (kbd "C-_") nil)
  (define-key undo-tree-map (kbd "C-/") nil)
  (define-key undo-tree-map (kbd "C-?") nil)
  (global-unset-key (kbd "s-z"))

  ;; C-? to describe a keybinding
  (global-set-key (kbd "C-?") 'describe-key)

  ;; These shortcut removals is to mimic closely Vim's behavior
  (global-unset-key (kbd "C-h"))
  (global-unset-key (kbd "C-l"))
  (global-unset-key (kbd "C-j"))
  (global-unset-key (kbd "C-k"))
  (global-unset-key (kbd "C-/"))

  ;; Paste using C-S-v on Linux
  (when (eq system-type 'gnu/linux)
    (define-key evil-insert-state-map (kbd "C-S-v") 'yank)
    (define-key minibuffer-local-map (kbd "C-S-v") 'yank)
    (use-package ivy
      :config
      (define-key ivy-minibuffer-map (kbd "C-S-v") 'yank)))

  ;; Window navigation
  (define-key evil-normal-state-map (kbd "C-h") 'windmove-left)
  (define-key evil-normal-state-map (kbd "C-j") 'windmove-down)
  (define-key evil-normal-state-map (kbd "C-k") 'windmove-up)
  (define-key evil-normal-state-map (kbd "C-l") 'windmove-right)

  (use-package treemacs
    :config
    ;; From within Treemacs
    (define-key evil-treemacs-state-map (kbd "C-h") 'windmove-left)
    (define-key evil-treemacs-state-map (kbd "C-l") 'windmove-right))

  ;; Window management
  (define-key evil-normal-state-map (kbd "SPC w") 'mine/close-and-save-buffer)
  (define-key evil-normal-state-map (kbd "SPC q") 'delete-window)
  (define-key evil-normal-state-map (kbd "SPC t") 'mine/toggle-maximize-buffer)
  (define-key evil-normal-state-map (kbd "SPC =") 'balance-windows)

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
  (define-key evil-normal-state-map (kbd "SPC v") 'exchange-point-and-mark)

  ;; Copy current file path & line
  (define-key evil-normal-state-map (kbd "SPC fl") 'mine/get-current-file-line)

  ;; In ruby, g{ will switch the block between do/end and {/}
  (define-key evil-normal-state-map (kbd "g{") 'enh-ruby-toggle-block)

  ;; Remap g-d to make it act just like C-[
  (define-key evil-normal-state-map (kbd "gd") 'evil-jump-to-tag))

(use-package windsize
  :config
  (global-set-key (kbd "C-S-<left>") 'windsize-left)
  (global-set-key (kbd "C-S-<right>") 'windsize-right)
  (global-set-key (kbd "C-S-<up>") 'windsize-up)
  (global-set-key (kbd "C-S-<down>") 'windsize-down))

(use-package emmet-mode
  :config
  (define-key evil-insert-state-map (kbd "C-x ,") 'emmet-expand-line))

(use-package markdown-mode
  :config
  ;; Disable default keybindings for cycling header visibility
  (define-key markdown-mode-map (kbd "S-<tab>") nil)
  (define-key markdown-mode-map (kbd "S-<iso-lefttab>") nil)
  (define-key markdown-mode-map (kbd "<backtab>") nil))

(use-package enh-ruby-mode
  :config
  (define-key enh-ruby-mode-map (kbd "C-j") nil))

(use-package rspec-mode
  :config
  (define-key evil-normal-state-map (kbd "SPC rl") 'rspec-verify-single)
  (define-key evil-normal-state-map (kbd "SPC rf") 'rspec-verify))

(use-package evil-nerd-commenter
  :config
  (define-key evil-normal-state-map (kbd "gcc") 'evilnc-comment-or-uncomment-lines)
  (define-key evil-visual-state-map (kbd "gc") 'evilnc-comment-or-uncomment-lines))

(use-package evil-numbers
  :config
  (define-key evil-normal-state-map (kbd "C-c a") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-c x") 'evil-numbers/dec-at-pt))

(use-package treemacs
  :config
  (define-key evil-normal-state-map ",," 'treemacs)
  (define-key evil-treemacs-state-map ",," 'treemacs-quit)
  (define-key evil-treemacs-state-map (kbd "A") 'mine/toggle-maximize-treemacs))

;; evil-args
(use-package evil-args
  :config
  (define-key evil-inner-text-objects-map "," 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "," 'evil-outer-arg))

(use-package diff-hl
  :config
  ; Jump to next/previous hunk
  (define-key evil-normal-state-map "[c" 'diff-hl-previous-hunk)
  (define-key evil-normal-state-map "]c" 'diff-hl-next-hunk)

  ; Unstage current hunk
  (define-key evil-normal-state-map (kbd "SPC h u") 'diff-hl-revert-hunk))

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
  (define-key ivy-minibuffer-map (kbd "C-x") 'ivy-switch-buffer-kill)

  ;; On buffer switch buffers, don't use C-k to kill buffers
  (define-key ivy-switch-buffer-map (kbd "C-k") 'ivy-previous-line)

  ;; Global keys
  (global-set-key (kbd "C-s") 'swiper)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (define-key evil-normal-state-map (kbd "SPC SPC") 'counsel-projectile-switch-to-buffer)
  (define-key evil-normal-state-map (kbd "C-SPC C-SPC") 'mine/switch-all-buffers)
  (define-key evil-normal-state-map (kbd "SPC a") 'counsel-ag)
  (define-key evil-visual-state-map (kbd "SPC s") 'mine/search-selected-text)
  (define-key evil-normal-state-map (kbd "SPC s") 'mine/search-word-under-cursor))

(use-package counsel-projectile
  :config
  ;; Switch between implementation and test ([p]roject [t]oggle))
  (define-key evil-normal-state-map (kbd "SPC pt")
    'projectile-toggle-between-implementation-and-test)

  ;; Switch from files in different projects
  (define-key evil-normal-state-map (kbd "C-S-p") 'counsel-projectile-switch-project)
  ;; Switch files in this project
  (define-key evil-normal-state-map (kbd "C-p") 'counsel-projectile-find-file))

(use-package expand-region
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

;;; shortcuts.el ends here
