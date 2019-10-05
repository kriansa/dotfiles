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

(defun mine/reset-text-size ()
  "Reset the text size."
  (interactive)
  (text-scale-set 0))

(defun mine/switch-buffer ()
  "Switch between buffers that doesn't start with star."
  (interactive)
  (let ((ivy-ignore-buffers (append ivy-ignore-buffers `("^\*"))))
    (ivy-switch-buffer)))

(defun mine/search-word-under-cursor ()
  "Start searching for the word under the cursor."
  (interactive)
  (counsel-ag (thing-at-point 'word)))

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

(defun mine/select-pasted ()
  "Select the recently pasted content."
  (interactive)
  (let ((start-marker (evil-get-marker ?\[))
         (end-marker (evil-get-marker ?\])))
    (evil-visual-select start-marker end-marker)))

;;; functions.el ends here
