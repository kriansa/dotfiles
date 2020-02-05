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
  (counsel-ag (thing-at-point 'word)))

(defun mine/search-selected-text ()
  "Start searching for the selected text."
  (interactive)
  (counsel-ag (buffer-substring-no-properties (mark) (point))))

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

;;; functions.el ends here
