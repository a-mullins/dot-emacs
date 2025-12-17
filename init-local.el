(defun denali-locals ()
  (dolist
      (frame-param '((width . 87)
                     (height . 40)))
    (add-to-list 'default-frame-alist frame-param))
  (set-face-attribute 'default nil :font "IBM Plex Mono" :height 120))

(defun shasta-locals ()
  (dolist
      (frame-param '((width . 80)
                     (height . 41)))
    (add-to-list 'default-frame-alist frame-param)
    (remove-hook 'prog-mode-hook #'display-line-numbers-mode))
  (set-face-attribute 'default nil :font "Menlo" :height 150))

(frame-parameter nil 'width)

(pcase (downcase (car (split-string (system-name) "\\.")))
  ("denali" (denali-locals))
  ("shasta" (shasta-locals)))
