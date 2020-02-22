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
(setq-default truncate-lines t) ; Enable line truncation by default

;; Ensure we have line truncation disabled on Help buffers
(add-hook 'help-mode-hook
  (defun mine/disable-line-truncation ()
    "Disable line truncation, even in split windows."
    (let ((inhibit-message t) ; No messages in the echo area
           message-log-max ; No messages in the *Messages* buffer
           truncate-partial-width-windows) ; No truncation in split windows
      (toggle-truncate-lines 0))))

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

;; Supress dired warnings when using Mac/BSD ls
(when (eq system-type 'darwin)
  (require 'ls-lisp)
  (setq-default ls-lisp-use-insert-directory-program nil))

;; Resizing support
(use-package windsize :ensure t)

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
  (setq ns-use-proxy-icon nil)
  (setq frame-title-format nil))

;; On Linux, just disable the whole decoration
(when (eq system-type 'gnu/linux)
  (add-to-list 'default-frame-alist '(undecorated . t)))

(defun mine/frame-setup (frame)
  "Initialize FRAME with sane defaults."
  (with-selected-frame frame
    ;; Set the default font
    (set-frame-font (font-spec :family "Iosevka Term" :size 18) nil t)

    ;; Emoji support 😀
    (set-fontset-font "fontset-default" 'unicode
      (font-spec :family
        (pcase system-type
          ('gnu/linux "JoyPixels")
          ('darwin "Apple Color Emoji")))
      nil 'prepend)))

;; Call on initialize and after first frame setup
(add-hook 'after-make-frame-functions 'mine/frame-setup)
(mine/frame-setup (selected-frame))

;; Default start maximized
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Modeline
(use-package doom-modeline
  :ensure t
  :init
  ;; Show column & line numbers on the bar
  (setq column-number-mode t)

  ;; Set the height
  (setq doom-modeline-height 35)

  ;; Total length of git branches
  (setq doom-modeline-vcs-max-length 24)

  ;; Show the path to the files when visiting symlinks
  (setq find-file-visit-truename t)

  :config
  (setq doom-modeline-buffer-file-name-style 'relative-from-project)

  ;; Ensure we always set this variable at frame-opening time
  (add-hook 'after-make-frame-functions
    (defun mine/setup-doom-modeline-frame (frame)
      (with-selected-frame frame
        (setq doom-modeline-icon (display-graphic-p)))))

  (doom-modeline-mode t))

;;; ui.el ends here
