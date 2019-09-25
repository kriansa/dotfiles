;;; shortcuts.el --- Custom shortcuts file
;;;
;;; Commentary:
;;; This file has every shortcut I use on Emacs.
;;;
;;; Code:

(provide 'mine/shortcuts)

;; Disable shortcuts that aren't helpful
(global-unset-key (kbd "M-c"))

;; These shortcut removal is to mimic closely Vim's behavior
(use-package evil
  :config
  (global-unset-key (kbd "C-l"))
  (global-unset-key (kbd "C-j"))
  (global-unset-key (kbd "C-k"))
  (global-unset-key (kbd "C-/"))
  (global-unset-key (kbd "TAB")))

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
  (define-key evil-normal-state-map (kbd "SPC s") 'counsel-ag)
  (define-key evil-normal-state-map (kbd "SPC SPC") 'ivy-switch-buffer))

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

;;; Emacs general commands

;; Window navigation
(global-set-key (kbd "C-h") 'windmove-left)
(global-set-key (kbd "C-j") 'windmove-down)
(global-set-key (kbd "C-k") 'windmove-up)
(global-set-key (kbd "C-l") 'windmove-right)

; Buffer
(global-set-key (kbd "s-w") 'kill-this-buffer)

;;; shortcuts.el ends here
