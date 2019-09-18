(provide 'my-modeline)

(require 'telephone-line-utils)
(require 'all-the-icons)

(defface doom-modeline-buffer-modified
  '((t (:inherit error :background nil :bold t)))
  "Face used for the 'unsaved' symbol in the mode-line."
  :group '+doom-modeline)

(defface doom-modeline-buffer-file
  '((t (:inherit mode-line-buffer-id)))
  "Face used for the filename part of the mode-line buffer path."
  :group '+doom-modeline)

(defface doom-modeline-warning
  `((t (:inherit warning :bold t)))
  "Face for warnings in the modeline. Used by `*flycheck'"
  :group '+doom-modeline)

(defface doom-modeline-urgent
  `((t (:inherit error :bold t)))
  "Face for errors in the modeline. Used by `*flycheck'"
  :group '+doom-modeline)

(defun +doom-modeline-buffer-file-name ()
  (propertize (file-name-nondirectory buffer-file-name)
          'face
          (let ((face (or (and (buffer-modified-p)
                                'doom-modeline-buffer-modified)
                          (and (active)
                                'doom-modeline-buffer-file))))
            (when face `(:inherit ,face)))))

(telephone-line-defsegment telephone-line-test ()
  (all-the-icons-octicon "lock" :v-adjust -0.05 :height 1.25))

(defun custom-modeline-modified ()
  ((let* ((config-alist
            '(("*" all-the-icons-faicon-family all-the-icons-faicon "chain-broken" :height 1.2 :v-adjust -0.0)
              ("-" all-the-icons-faicon-family all-the-icons-faicon "link" :height 1.2 :v-adjust -0.0)
              ("%" all-the-icons-octicon-family all-the-icons-octicon "lock" :height 1.2 :v-adjust 0.1)))
           (result (cdr (assoc (format-mode-line "%*") config-alist))))
      (propertize (apply (cadr result) (cddr result))
                  'face `(:family ,(funcall (car result)))))))

(defun modeline-buffer-icon ()
  (cond (buffer-read-only
          (concat (all-the-icons-octicon
                    "lock"
                    :face 'doom-modeline-warning
                    :v-adjust -0.05)
                  " "))
        ((buffer-modified-p)
         (concat (all-the-icons-faicon
                   "floppy-o"
                   :face 'doom-modeline-buffer-modified
                   :v-adjust -0.0575)
                 " "))
        ((and buffer-file-name
              (not (file-exists-p buffer-file-name)))
         (concat (all-the-icons-octicon
                   "circle-slash"
                   :face 'doom-modeline-urgent
                   :v-adjust -0.05)
                 " "))
        ((buffer-narrowed-p)
         (concat (all-the-icons-octicon
                   "fold"
                   :face 'doom-modeline-warning
                   :v-adjust -0.05)
                 " "))))

(telephone-line-defsegment* telephone-line-buffer-info ()
  (let ((all-the-icons-scale-factor 1.2))
    (concat (modeline-buffer-icon)
            (if buffer-file-name
                (+doom-modeline-buffer-file-name)
              "%b"))))

(telephone-line-defsegment* telephone-line-fileinfo-segment ()
  `(""
    mode-line-modified
    ))

; (require 'telephone-line-config)
; (telephone-line-flat)
(setq telephone-line-primary-left-separator 'telephone-line-flat
      telephone-line-secondary-left-separator 'telephone-line-nil
      telephone-line-primary-right-separator 'telephone-line-flat
      telephone-line-secondary-right-separator 'telephone-line-nil
      telephone-line-height nil)

(setq telephone-line-lhs '(
  ;; Display the current evil-mode
  (evil   . (telephone-line-evil-tag-segment))
  ;; Test
  (nil . (telephone-line-buffer-info))
  ;; Display the version-control status
  (accent . (telephone-line-vc-segment))
  ;; Display other stuff
  (accent . (telephone-line-test
              telephone-line-erc-modified-channels-segment
              telephone-line-process-segment))
  (nil    . (telephone-line-fileinfo-segment))
  ))
(setq telephone-line-rhs
        '((nil    . (telephone-line-misc-info-segment))
          (accent . (telephone-line-major-mode-segment))
          (evil   . (telephone-line-airline-position-segment))))

(telephone-line-mode t)
; (telephone-line-evil-config)
