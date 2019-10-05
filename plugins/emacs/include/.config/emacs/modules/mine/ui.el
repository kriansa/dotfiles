;;; init.el --- UI stuff
;;;
;;; Commentary:
;;; All UI customization such as themes and modeline should be here
;;;
;;; Code:

(provide 'mine/ui)

;; Icons
(use-package all-the-icons :ensure t)

;; Set theme
(use-package doom-themes :ensure t :config (load-theme 'doom-one t))

;; Available themes
;; (use-package zenburn-theme :ensure t :config (load-theme 'zenburn t))
;; (use-package atom-one-dark-theme :ensure t :config (load-theme 'atom-one-dark t))

;; General UI settings
;; ===================

(setq-default visible-bell t) ; Disable bell ringing
(setq-default ring-bell-function 'ignore) ; Remove visual bell
(tool-bar-mode -1) ; Disable tool bar (icons)
(menu-bar-mode -1) ; Disable menu bar
(scroll-bar-mode -1) ; Disable the scroll bar
(blink-cursor-mode 0) ; Disable cursor blinking

;; Many emacs commands will ask you a “yes/no” question, and you have to type
;; the full word “yes” or “no”. (such as when deleting a file) You can make
;; emacs just ask “y/n” instead.
(defalias 'yes-or-no-p 'y-or-n-p)

;; Add file sizes in human-readable units (KB, MB, etc) to dired buffers.
(setq-default dired-listing-switches "-alh")

;; Clear startup annoyances
;; ========================

(setq inhibit-startup-message t) ; Disable startup message

;; Hides initial startup messages
(defun display-startup-echo-area-message ()
  "Force the built-in Emacs function to do nothing (void).")

;; Makes *scratch* empty.
(setq-default initial-scratch-message "")

;; Remove the *scratch* buffer
(add-hook 'after-change-major-mode-hook
  (defun mine/remove-scratch-buffer ()
    "Remove the scratch buffer."
    (mine/kill-empty-buffer "*scratch*")))

;; Supress dired warnings when using Mac/BSD ls
(when (eq system-type 'darwin)
  (require 'ls-lisp)
  (setq-default ls-lisp-use-insert-directory-program nil))

;; Left nav settings
;; =================

;; Line numbers
;; (add-hook 'prog-mode-hook 'display-line-numbers-mode)
;; (add-hook 'text-mode-hook 'display-line-numbers-mode)

;; Left border
(fringe-mode '(nil . 0))

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

;; Emoji support
(pcase system-type
  ('gnu/linux "emoji")
  ('darwin "Apple Color Emoji"))

(set-fontset-font t 'symbol
  (font-spec :family
    (pcase system-type
      ('gnu/linux "JoyPixels")
      ('darwin "Apple Color Emoji")))
  nil 'prepend)

;; Default start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Modeline
(use-package doom-modeline
  :ensure t
  :init
  ;; Show column & line numbers on the bar
  (setq column-number-mode t)

  ;; Show the path to the files when visiting symlinks
  (setq find-file-visit-truename t)

  :config
  (setq doom-modeline-buffer-file-name-style 'relative-from-project)
  (doom-modeline-def-modeline 'main
    '(bar window-number matches buffer-info remote-host buffer-position selection-info)
    '(objed-state misc-info persp-name irc mu4e github debug input-method buffer-encoding lsp major-mode process vcs checker "  "))

  (doom-modeline-mode t))

;;; ui.el ends here
