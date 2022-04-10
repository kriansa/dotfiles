;;; package-setup.el --- Emacs package configuration
;;;
;;; Commentary:
;;; Configure the setup of use-package
;;;
;;; Code:
(provide 'mine/package-setup)

;;; Configure straight.el

;; Ensure we use shallow clones to save bandwidth
(setq straight-vc-git-default-clone-depth 1)

;; Install straight.el
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Ensure we have use-package installed
(straight-use-package 'use-package)

;; Load use-package
(eval-when-compile (require 'use-package))

;;; package-setup.el ends here
