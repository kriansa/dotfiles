;;; custom.el --- Emacs custom file
;;;
;;; Commentary:
;;; This file holds every Emacs customization done on its UI.
;;;
;;; Code:

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (gnu-elpa-keyring-update auto-package-update use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(git-gutter:added ((t (:background nil :inherit (quote default)))))
 '(git-gutter:deleted ((t (:background nil :inherit (quote default)))))
 '(git-gutter:modified ((t (:background nil :inherit (quote default)))))
 '(git-gutter:unchanged ((t (:background nil :inherit (quote default))))))
