;; init.el -- emacs config

;; Don't litter my init.
(setq custom-file "~/.emacs.d/custom-garbage.el")
(add-hook 'kill-emacs-hook
	  (lambda () (delete-file "~/.emacs.d/custom-garbage.el")))

;; -------------------------------------------------------------------
;; PACKAGES
;; -------------------------------------------------------------------
(require 'package)
(setq package-archives
      '(
	("gnu" . "https://elpa.gnu.org/packages/")
	("melpa-stable" . "https://stable.melpa.org/packages/")
	;; ("melpa" . "https://melpa.org/packages/")
	;; ("marmalade" . "https://marmalade-repo.org/packages/")
	))
(package-initialize)
;; Archive listings and installed packages should now be available.

;; If use-package is missing, prompt to download now.
(unless (package-installed-p 'use-package)
  (when (yes-or-no-p "There are missing packages. Fetch them now? ")
    (package-refresh-contents)
    (package-install 'use-package)))

(when (package-installed-p 'use-package)
  (use-package elpy
    :ensure t
    :config (elpy-enable))

  (use-package solarized-theme
    :ensure t
    :config (load-theme 'solarized-dark t))

  (use-package paredit
    :ensure t
    :init
      (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
      (add-hook 'lisp-mode-hook 'paredit-mode))

  (use-package markdown-mode
    :ensure t
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'" . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
    :init (setq markdown-command "multimarkdown"))

  (use-package slime
    :config
    (add-to-list 'slime-contribs 'slime-repl)
    (setq inferior-lisp-program "/usr/bin/clisp"))
) ;; END when (package-installed-p 'use-package)

;; -------------------------------------------------------------------
;; UI / VISUAL
;; -------------------------------------------------------------------

;; set font & size according to which system emacs is running on.
(apply 'set-face-attribute 'default nil
  (pcase (system-name)
    ("denali"    '(:family "inconsolata" :height 130))
    ("Air.local" '(:family "inconsolata" :height 180))
    (_           '(:family "inconsolata" :height 120))))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq show-paren-delay 0)


;; Hooks for prog-mode. This covers python, lisp, etc etc.
(mapc
 (lambda (hook) (add-hook 'prog-mode-hook hook))
   '(
     linum-mode
     column-number-mode
     (lambda () (toggle-truncate-lines 1))
     show-paren-mode
     ))

(push '("\\.cl\\'" . common-lisp-mode) auto-mode-alist)

;; dired mode
(setq dired-listing-switches "-aDFhl")
(add-hook 'dired-mode-hook
	  (lambda () (toggle-truncate-lines 1)))
