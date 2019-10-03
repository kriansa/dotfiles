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

;;; functions.el ends here
