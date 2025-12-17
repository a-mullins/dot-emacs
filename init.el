;; init.el -- emacs config
;; see also local.el


;; --------------------------------------------------------------------------
;; CUSTOM / GLOBALS / MISC
;; --------------------------------------------------------------------------

(setq-default custom-file "~/.emacs.d/custom.el")
(load custom-file t t)
(setq-default indent-tabs-mode nil)

(defun delete-trailing-whitespace-on-save ()
  "Delete trailing whitespace when saving a buffer."
  (unless (or (string= major-mode 'diff-mode)
              (string= major-mode 'hexl-mode))
    (delete-trailing-whitespace)))
(add-hook 'before-save-hook 'delete-whitespace)


;; --------------------------------------------------------------------------
;; FILE BACKUPS / VERSION CONTROL
;; --------------------------------------------------------------------------

;; Save numbered backups by default, ie foo.~1~, foo.~2~, ... instead
;; of just foo~.
(setq-default version-control t)

;; Inhibit file backups if the file is already under version control.
(defun disable-backups-under-vc ()
  "If a file is under version control, inhibit foo~ style backups."
  (when (vc-backend (buffer-file-name))
    ;; (set (make-local-variable 'make-backup-files) t)))
    (set (make-local-variable 'backup-inhibited) t)))
(add-hook 'find-file-hook 'disable-backups-under-vc)


;; --------------------------------------------------------------------------
;; GLOBAL UI / VISUAL
;; --------------------------------------------------------------------------

(tool-bar-mode 0)
;; TODO it would be nice if we can activate scrollbars and match them to the
;; to the theme.
(scroll-bar-mode 0)
;; (setq show-paren-delay 0)


;; --------------------------------------------------------------------------
;; PACKAGES
;; --------------------------------------------------------------------------

(require 'package)
(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
        ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package solarized-theme
  :ensure t
  :init (setq solarized-scale-markdown-headlines t))

;; TODO it would be nice to change the splash screen to alert on
;;      equinox, solstice, sunset & sundown, etc.
(use-package solar
  :config (setq calendar-latitude   36.99
                calendar-longitude -82.53))

(use-package circadian
  :ensure t
  :after solar
  :config
  (setq circadian-themes '((:sunrise . solarized-selenized-light)
                           (:sunset  . solarized-selenized-dark)))
  (circadian-setup))

(defun markdown-hide-urls ()
  "Calls (markdown-toggle-url-hiding t)"
  (markdown-toggle-url-hiding t))

;; TODO export html style tags instead of xhtml and embed css styling
;;      information. ref: markdown-command
(use-package markdown-mode
  :ensure t
  :hook ((markdown-mode . visual-line-mode)
         (markdown-mode . markdown-hide-urls)))

(use-package paredit
  :ensure t
  :hook lisp-data-mode)


;; --------------------------------------------------------------------------
;; MODE SETTINGS & HOOKS
;; --------------------------------------------------------------------------

(defun trunc-long-lines ()
  "Always truncate long lines, calls (toggle-truncate-lines 1)."
  (toggle-truncate-lines 1))

(let ((prog-hooks '(display-line-numbers-mode
                    column-number-mode
                    trunc-long-lines
                    ;; could do show-paren-local-mode instead for hook
                    ;; show-paren-mode
                    )))
  (dolist (f prog-hooks)
    (add-hook 'prog-mode-hook f)))

(defun highlight-sexp ()
  (setq-default show-paren-style 'parenthesis))
(add-hook 'lisp-data-mode-hook 'highlight-sexp)
;; TODO C-h f set-face-attribute says that :style can be wave or line,
;;      but there are more options available, like dots, double-line,
;;      or dashes. Docs should be updated.
(set-face-attribute 'show-paren-match-expression nil
                    :inherit nil :underline '(:color foreground-color :style dots))
(set-face-attribute 'show-paren-match nil :weight 'heavy :reverse-video t)


;; dired mode
(setq dired-listing-switches "-ADFhl --group-directories-first")
(add-hook 'dired-mode-hook 'trunc-long-lines)

(add-hook 'text-mode-hook 'visual-line-mode)


;; --------------------------------------------------------------------------
;; CODE COMPLETION (CORFU AND FRIENDS)
;; --------------------------------------------------------------------------

(load "~/.emacs.d/init-corfu" t t)


;; --------------------------------------------------------------------------
;; C
;; --------------------------------------------------------------------------

(load "~/.emacs.d/init-c" t t)

;; --------------------------------------------------------------------------
;; LOCAL MACHINE OVERRIDES
;; --------------------------------------------------------------------------

(load "~/.emacs.d/init-local" t t)
