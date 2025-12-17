(defun denali-locals ()
  (dolist
      (frame-param '((width . 87)
                     (height . 40)))
    (add-to-list 'default-frame-alist frame-param))
  (set-face-attribute 'default nil :font "IBM Plex Mono" :height 120))

(pcase (downcase (car (split-string (system-name) "\\.")))
  ("denali" (denali-locals)))
