;;; shortcuts.el --- Custom shortcuts file
;;;
;;; Commentary:
;;; This file has every shortcut I use on Emacs.
;;;
;;; Code:

(provide 'mine/shortcuts)

(use-package evil
  :defer t
  :config
  ;; Disable shortcuts that aren't helpful
  (global-unset-key (kbd "M-c"))
  (global-unset-key (kbd "M-z"))
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

  ;; C-? to describe a keybinding
  (global-set-key (kbd "C-?") 'describe-key)

  ;; C-x 2 and C-x 3 opens new windows and switch to them immediately
  (global-set-key "\C-x2" (lambda () (interactive)(split-window-vertically) (other-window 1)))
  (global-set-key "\C-x3" (lambda () (interactive)(split-window-horizontally) (other-window 1)))

  ;; These shortcut removals is to mimic closely Vim's behavior
  (global-unset-key (kbd "C-h"))
  (global-unset-key (kbd "C-l"))
  (global-unset-key (kbd "C-j"))
  (global-unset-key (kbd "C-k"))
  (global-unset-key (kbd "C-/"))

  ;; Window navigation
  (define-key evil-normal-state-map (kbd "C-h") 'windmove-left)
  (define-key evil-normal-state-map (kbd "M-h") 'windmove-left)
  (define-key evil-normal-state-map (kbd "C-j") 'windmove-down)
  (define-key evil-normal-state-map (kbd "M-j") 'windmove-down)
  (define-key evil-normal-state-map (kbd "C-k") 'windmove-up)
  (define-key evil-normal-state-map (kbd "M-k") 'windmove-up)
  (define-key evil-normal-state-map (kbd "C-l") 'windmove-right)
  (define-key evil-normal-state-map (kbd "M-l") 'windmove-right)

  ;; Use ; as an alias to :
  (define-key evil-normal-state-map (kbd ";") 'evil-ex)

  ;; Window movement
  (define-key evil-normal-state-map (kbd "C-M-k") (lambda () (interactive) (mine/swap-windows 'up)))
  (define-key evil-normal-state-map (kbd "C-M-j") (lambda () (interactive) (mine/swap-windows 'down)))
  (define-key evil-normal-state-map (kbd "C-M-h") (lambda () (interactive) (mine/swap-windows 'left)))
  (define-key evil-normal-state-map (kbd "C-M-l") (lambda () (interactive) (mine/swap-windows 'right)))

  ;; Window management
  (define-key evil-normal-state-map (kbd "SPC w") 'mine/close-and-save-buffer)
  (define-key evil-normal-state-map (kbd "SPC q") 'delete-window)
  (define-key evil-normal-state-map (kbd "SPC t") 'mine/toggle-maximize-buffer)
  (define-key evil-normal-state-map (kbd "<M-S-return>") 'mine/toggle-maximize-buffer)
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
  (define-key evil-normal-state-map (kbd "SPC yf") 'mine/get-current-file)
  (define-key evil-normal-state-map (kbd "SPC yl") 'mine/get-current-file-line)

  ;; In ruby, g{ will switch the block between do/end and {/}
  (define-key evil-normal-state-map (kbd "g{") 'enh-ruby-toggle-block)

  ;; Remap g-d to make it act just like C-[
  (define-key evil-normal-state-map (kbd "gd") 'evil-jump-to-tag))

;; Paste using C-S-v on Linux
(when (eq system-type 'gnu/linux)
  (use-package evil
    :defer t
    :config
    (define-key evil-insert-state-map (kbd "C-S-v") 'yank)
    (define-key minibuffer-local-map (kbd "C-S-v") 'yank))
  (use-package ivy
    :defer t
    :config
    (define-key ivy-minibuffer-map (kbd "C-S-v") 'yank)))

(use-package treemacs
  :defer t
  :config
  ;; From within Treemacs
  (define-key evil-treemacs-state-map (kbd "C-h") 'windmove-left)
  (define-key evil-treemacs-state-map (kbd "C-l") 'windmove-right))

(use-package company
  :defer t
  :config
  (define-key company-active-map (kbd "C-w") nil)
  (define-key evil-insert-state-map (kbd "C-SPC") 'company-complete))

(use-package evil-collection
  :defer t
  :config
  (define-key special-mode-map (kbd "S-SPC") nil)
  ;; Help-mode
  (evil-define-key 'normal help-mode-map (kbd "SPC") nil)
  (evil-define-key 'normal help-mode-map (kbd "S-SPC") nil)
  (evil-define-key 'visual help-mode-map (kbd "SPC") nil)
  (evil-define-key 'visual help-mode-map (kbd "S-SPC") nil)
  ;; Debug-mode
  (evil-define-key 'normal debugger-mode-map (kbd "SPC") nil)
  ;; Special-mode
  (evil-define-key 'normal special-mode-map (kbd "C-j") nil)
  (evil-define-key 'normal special-mode-map (kbd "C-k") nil)
  (evil-define-key 'visual special-mode-map (kbd "SPC") nil)
  (evil-define-key 'normal special-mode-map (kbd "SPC") nil)
  ;; Go-mode integration
  (evil-define-key 'normal go-mode-map (kbd "K") nil))

;; The line-break from js2-mode is far superior than emacs/evil's one.
;; Let's just bind RET/o/O to it
(use-package js2-mode
  :defer t
  :config
  (evil-define-key 'normal js2-minor-mode-map (kbd "o")
    (lambda ()
      (interactive)
      (evil-append-line 1)
      (js2-line-break)))
  (evil-define-key 'normal js2-minor-mode-map (kbd "O")
    (lambda ()
      (interactive)
      (evil-previous-line)
      (evil-append-line 1)
      (js2-line-break)))
  (evil-define-key 'insert js2-minor-mode-map (kbd "RET") 'js2-line-break))

(use-package dired
  :defer t
  :config
  (evil-define-key 'normal dired-mode-map (kbd "S-SPC") nil)
  (evil-define-key 'normal dired-mode-map (kbd "SPC") nil))

(use-package smerge-mode
  :defer t
  :config
  ;; Use better bindings for usage with evil-mode
  (define-key evil-normal-state-map (kbd "SPC dp") 'smerge-keep-current))

(use-package format-all
  :defer t
  :config
  (define-key evil-normal-state-map (kbd "SPC f") 'format-all-buffer))

(use-package windsize
  :defer t
  :config
  (global-set-key (kbd "C-S-h") 'windsize-left)
  (global-set-key (kbd "C-S-l") 'windsize-right)
  (global-set-key (kbd "C-S-k") 'windsize-up)
  (global-set-key (kbd "C-S-j") 'windsize-down))

(use-package markdown-mode
  :defer t
  :config
  ;; Disable default keybindings for cycling header visibility
  (define-key markdown-mode-map (kbd "S-<tab>") nil)
  (define-key markdown-mode-map (kbd "S-<iso-lefttab>") nil)
  (define-key markdown-mode-map (kbd "<backtab>") nil))

(use-package enh-ruby-mode
  :defer t
  :config
  (define-key enh-ruby-mode-map (kbd "C-j") nil))

(use-package rspec-mode
  :defer t
  :config
  (define-key evil-normal-state-map (kbd "SPC rl") 'rspec-verify-single)
  (define-key evil-normal-state-map (kbd "SPC rf") 'rspec-verify))

(use-package evil-nerd-commenter
  :defer t
  :config
  (define-key evil-normal-state-map (kbd "gcc") 'evilnc-comment-or-uncomment-lines)
  (define-key evil-visual-state-map (kbd "gc") 'evilnc-comment-or-uncomment-lines))

(use-package evil-numbers
  :defer t
  :config
  (define-key evil-normal-state-map (kbd "C-c a") 'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-c x") 'evil-numbers/dec-at-pt))

(use-package treemacs
  :defer t
  :config
  (define-key evil-normal-state-map "\\" 'treemacs)
  (define-key evil-treemacs-state-map "\\" 'treemacs-quit)
  (define-key evil-treemacs-state-map (kbd "A") 'mine/toggle-maximize-treemacs))

;; evil-args
(use-package evil-args
  :defer t
  :config
  (define-key evil-inner-text-objects-map "," 'evil-inner-arg)
  (define-key evil-outer-text-objects-map "," 'evil-outer-arg))

(use-package diff-hl
  :defer t
  :config
  ; Jump to next/previous hunk
  (define-key evil-normal-state-map "[c" 'diff-hl-previous-hunk)
  (define-key evil-normal-state-map "]c" 'diff-hl-next-hunk)

  ; Unstage current hunk
  (define-key evil-normal-state-map (kbd "SPC hu") 'diff-hl-revert-hunk))

;; Magit
(use-package magit
  :defer t
  :config
  (define-key evil-normal-state-map (kbd "SPC gs") 'magit-status)
  (define-key evil-normal-state-map (kbd "SPC gb") 'magit-blame-addition))

;; Ivy, counsel & swiper
(use-package ivy
  :defer t
  :config
  ;; Define vim keys to navigate on ivy mini buffers
  (define-key ivy-minibuffer-map (kbd "C-j") 'ivy-next-line)
  (define-key ivy-minibuffer-map (kbd "C-k") 'ivy-previous-line)
  (define-key ivy-minibuffer-map (kbd "C-d") 'ivy-scroll-up-command)
  (define-key ivy-minibuffer-map (kbd "C-u") 'ivy-scroll-down-command)
  (define-key ivy-minibuffer-map (kbd "C-w") 'backward-kill-word)
  (define-key ivy-minibuffer-map (kbd "C-x") 'ivy-switch-buffer-kill)

  ;;  Exits with the current input instead of the current candidate
  (define-key ivy-minibuffer-map (kbd "<C-return>") 'ivy-immediate-done)

  ;; On buffer switch buffers, don't use C-k to kill buffers
  (define-key ivy-switch-buffer-map (kbd "C-k") 'ivy-previous-line))

(use-package counsel
  :defer t
  :config
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (define-key evil-normal-state-map (kbd "SPC SPC") 'counsel-projectile-switch-to-buffer)
  (define-key evil-normal-state-map (kbd "C-SPC C-SPC") 'mine/switch-all-buffers)
  (define-key evil-normal-state-map (kbd "SPC a") 'counsel-rg)
  (define-key evil-visual-state-map (kbd "SPC s") 'mine/search-selected-text)
  (define-key evil-normal-state-map (kbd "SPC s") 'mine/search-word-under-cursor))

(use-package swiper
  :defer t
  :config
  (define-key evil-normal-state-map (kbd "C-s") 'swiper-isearch)
  (define-key evil-visual-state-map (kbd "C-s") 'swiper-isearch-thing-at-point))

(use-package counsel-projectile
  :defer t
  :config
  ;; Switch between implementation and test ([p]roject [T]est toggle))
  (define-key evil-normal-state-map (kbd "SPC pt")
    'projectile-toggle-between-implementation-and-test)

  ;; Switch from files in different projects
  (define-key evil-normal-state-map (kbd "C-S-p") 'counsel-projectile-switch-project)
  ;; Switch files in this project
  (define-key evil-normal-state-map (kbd "C-p") 'counsel-projectile-find-file))

(use-package expand-region
  :defer t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(use-package vterm-toggle
  :defer t
  :config
  ;; Toggle the terminal window
  (define-key evil-normal-state-map (kbd "C-\\") 'vterm-toggle)
  (evil-collection-define-key 'insert 'vterm-mode-map (kbd "C-\\") 'vterm-toggle)

  ;; Movement with M-[hjkl] works on insert mode
  (evil-collection-define-key 'insert 'vterm-mode-map (kbd "M-h") 'windmove-left)
  (evil-collection-define-key 'insert 'vterm-mode-map (kbd "M-j") 'windmove-down)
  (evil-collection-define-key 'insert 'vterm-mode-map (kbd "M-k") 'windmove-up)
  (evil-collection-define-key 'insert 'vterm-mode-map (kbd "M-l") 'windmove-right)

  ;; Ctrl-C is sent to terminal
  (evil-collection-define-key 'insert 'vterm-mode-map (kbd "C-c") 'vterm--self-insert)

  ;; C-M-Ret is the same inside normal buffers
  (evil-collection-define-key 'insert 'vterm-mode-map (kbd "<M-S-return>") 'mine/toggle-maximize-buffer))

;;; shortcuts.el ends here
