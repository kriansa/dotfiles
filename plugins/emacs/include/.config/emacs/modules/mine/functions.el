;;; functions.el --- Helper functions
;;;
;;; Commentary:
;;; All UI customization such as themes and modeline should be here
;;;
;;; Code:

(provide 'mine/functions)

(defun mine/kill-empty-buffer (buffer)
  "Kill buffers when they're empty.

  BUFFER - The name of the buffer you want to kill"

  (if (and (get-buffer buffer) (= (buffer-size (get-buffer buffer)) 0))
      (kill-buffer buffer)))

(defun mine/toggle-maximize-buffer ()
  "Maximize/restore buffer."
  (interactive)
  (if (= 1 (length (window-list)))
    (jump-to-register '_)
    (progn
      (window-configuration-to-register '_)
      (delete-other-windows))))

(defun mine/toggle-maximize-treemacs ()
  "Maximize/restore Treemacs buffer."
  (interactive)
  (unless (boundp 'treemacs--original-width)
    (setq treemacs--original-width treemacs-width))
  (with-selected-window (treemacs-get-local-window)
    (setq treemacs-width
      (if (= treemacs-width treemacs--original-width)
        (/ (frame-width) 2)
        treemacs--original-width))
    (treemacs--set-width treemacs-width)))

(defun mine/reset-text-size ()
  "Reset the text size."
  (interactive)
  (text-scale-set 0))

(defun mine/switch-all-buffers ()
  "Switch between all Emacs known buffers, without filters."
  (interactive)
  (let ((ivy-ignore-buffers '("\\` ")))
    (ivy-switch-buffer)))

(defun mine/search-word-under-cursor ()
  "Start searching for the word under the cursor."
  (interactive)
  (counsel-rg (thing-at-point 'word)))

(defun mine/search-selected-text ()
  "Start searching for the selected text."
  (interactive)
  (counsel-rg (buffer-substring-no-properties (mark) (point))))

(defun mine/create-line-below ()
  "Create a new line below and return to normal state."
  (interactive)
  (evil-open-below 1)
  (evil-force-normal-state))

(defun mine/create-line-above ()
  "Create a new line above and return to normal state."
  (interactive)
  (evil-open-above 1)
  (evil-force-normal-state))

(defun mine/split-line ()
  "Splits the line under the cursor."
  (interactive)
  (newline 1 t))

(defun mine/close-and-save-buffer ()
  "Close and save the current buffer."
  (interactive)
  (when buffer-file-name (save-buffer))
  (kill-this-buffer))

(defun mine/get-current-file-line ()
  "Get the current path name relative to the project."
  (interactive)
  (kill-new (concat
              (file-relative-name buffer-file-name (projectile-project-root))
              ":" (format-mode-line "%l")))
  (message "File path copied to clipboard."))

(defun mine/swap-windows (dir)
  "Swap the content between two windows on the DIR direction (up, down, left or right)."
  (interactive)
  (let* ((other-window (windmove-find-other-window dir nil nil))
          (other-buffer (window-buffer other-window))
          (other-bufname (buffer-name other-buffer))
          (this-buffer (window-buffer (selected-window)))
          (this-bufname (buffer-name this-buffer)))
    (if (or (null other-window)
          (string-prefix-p " *Minibuf" other-bufname)
          (string-prefix-p " *Treemacs-Scoped-Buffer" other-bufname)
          (string-prefix-p " *Treemacs-Scoped-Buffer" this-bufname))
        (user-error "No window %s from selected window" dir)
      (progn
        (set-window-buffer (selected-window) other-buffer)
        (set-window-buffer other-window this-buffer)
        (select-window other-window)))))

(defun mine/close-project (&rest _)
  "Save and kill all buffers for the current project."
  (interactive)
  (projectile-save-project-buffers)
  (let ((always-true (lambda (orig-fun &rest args) t)))
    (advice-add 'yes-or-no-p :around always-true)
    (projectile-kill-buffers)
    (advice-remove 'yes-or-no-p always-true)))

;;; functions.el ends here
