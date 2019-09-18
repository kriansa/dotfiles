;;; init.el --- UI stuff
;;;
;;; Commentary:
;;; All UI customization such as themes and modeline should be here
;;;
;;; Code:

(provide 'mine/ui)

;; Icons
(use-package all-the-icons :ensure t)

(use-package zenburn-theme
  :disabled
  :ensure t
  :config
  ;; Load it
  (load-theme 'zenburn t)

  ;;; Custom theme faces
  (zenburn-with-color-variables
    (custom-theme-set-faces
      'zenburn

      ;; Fringe
      `(fringe ((t (:foreground ,zenburn-fg :background ,zenburn-bg))))

      ;; Whitespace
      `(whitespace-space ((t (:foreground nil :background nil :inherit 'whitespace-newline))))
      `(whitespace-empty ((t (:foreground nil :background nil :inherit 'whitespace-newline))))
      `(whitespace-tab ((t (:foreground nil :background nil :inherit 'whitespace-newline))))
      `(whitespace-empty ((t (:foreground nil :background nil :inherit 'whitespace-newline))))
      ; `(whitespace-newline ((t (:foreground ,atom-one-dark-gray :background ,atom-one-dark-bg))))

      ;; Git-gutter
      `(git-gutter:unchanged ((t (:foreground nil :background ,zenburn-bg :inherit 'default))))
      `(git-gutter:modified ((t (:foreground ,zenburn-orange :background ,zenburn-bg :inherit 'default))))
      `(git-gutter:added ((t (:foreground ,zenburn-green+1 :background ,zenburn-bg :inherit 'default))))
      `(git-gutter:deleted ((t (:foreground ,zenburn-red-1 :background ,zenburn-bg :inherit 'default)))))))

;; Theme
(use-package atom-one-dark-theme
  :ensure t
  :config
  (atom-one-dark-with-color-variables
  (custom-theme-set-faces
  'atom-one-dark

  ;; whitespace-mode
  `(whitespace-space ((t (:foreground ,atom-one-dark-gray :background ,atom-one-dark-bg))))
  `(whitespace-empty ((t (:foreground ,atom-one-dark-gray :background ,atom-one-dark-bg))))
  `(whitespace-tab ((t (:foreground ,atom-one-dark-gray :background ,atom-one-dark-bg))))
  `(whitespace-empty ((t (:foreground ,atom-one-dark-gray :background ,atom-one-dark-bg))))
  `(whitespace-newline ((t (:foreground ,atom-one-dark-gray :background ,atom-one-dark-bg))))

  ;; show-paren-mode
  `(show-paren-match ((t (:foreground ,atom-one-dark-red-1 :background ,atom-one-dark-bg
                              :underline t))))

  ;; git-gutter
  `(git-gutter:modified ((t (:foreground ,atom-one-dark-orange-1 :background ,atom-one-dark-bg :inherit 'default))))
  `(git-gutter:added ((t (:foreground ,atom-one-dark-green :background ,atom-one-dark-bg :inherit 'default))))
  `(git-gutter:deleted ((t (:foreground ,atom-one-dark-red-1 :background ,atom-one-dark-bg :inherit 'default))))))

  (load-theme 'atom-one-dark t))

;; General UI settings
;; ===================

(setq-default visible-bell t) ; Disable bell ringing
(setq-default ring-bell-function 'ignore) ; Remove visual bell
(tool-bar-mode -1) ; Disable tool bar (icons)
(menu-bar-mode -1) ; Disable menu bar
(scroll-bar-mode -1) ; Disable the scroll bar
(blink-cursor-mode 0) ; Disable cursor blinking

; Highlight matching parenthesis
(setq-default show-paren-delay 0)
(show-paren-mode 1)

;; Many emacs commands will ask you a “yes/no” question, and you have to type
;; the full word “yes” or “no”. (such as when deleting a file) You can make
;; emacs just ask “y/n” instead.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Clear initial annoyances
;; ========================

(setq inhibit-startup-message t) ; Disable startup message

;; Hides initial startup messages
(defun display-startup-echo-area-message ()
  "Force the built-in Emacs function to do nothing (void).")

;; Makes *scratch* empty.
(setq-default initial-scratch-message "")

;; Remove the *scratch* buffer
(defun mine/remove-scratch-buffer ()
  "Remove the scratch buffer."
  (mine/kill-empty-buffer "*scratch*"))

(add-hook 'after-change-major-mode-hook 'mine/remove-scratch-buffer)

;; Left nav settings
;; =================

;; Line numbers
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(add-hook 'text-mode-hook 'display-line-numbers-mode)

(fringe-mode '(20 . 0)) ; Left border

;; Whitespaces settings
;; ====================
;; This enables Emacs to display hidden characters such as spaces and tabs with
;; visible characters in the code

(require 'whitespace)

;; Which characters we want to see
(setq whitespace-style
  (quote (face spaces tabs newline space-mark tab-mark newline-mark)))

;; Set character replacements
(setq whitespace-display-mappings
  ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
  '(
     (space-mark 32 [183] [46]) ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
     (newline-mark 10 [172 10]) ; LF is replaced by a "¬"
     (tab-mark 9 [9656 32 32] [92 32 32]))) ; tab is replaced by a "▸  "

(global-whitespace-mode)

;; Settings applied to the window
;; ==============================

;; Style titlebar on MacOS
(when (eq system-type 'darwin)
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (setq frame-title-format nil))

;; On Linux, just disable the whole decoration
(when (eq system-type 'gnu/linux)
  (add-to-list 'default-frame-alist '(undecorated . t)))

;; Set a default font
(add-to-list 'default-frame-alist '(font . "Iosevka Term:pixelsize=18:weight=normal:slant=normal:width=normal:spacing=100:scalable=true"))

;; Default start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

(use-package doom-modeline
  :ensure t
  :init
  ;; Show column & line numbers on the bar
  (setq column-number-mode t)

  :config
  (setq doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-def-modeline 'main
    '(bar window-number matches buffer-info remote-host buffer-position selection-info)
    '(objed-state misc-info persp-name irc mu4e github debug input-method buffer-encoding lsp major-mode process vcs checker "     "))

  (doom-modeline-mode t))

;;; ui.el ends here
