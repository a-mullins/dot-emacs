;; init.el -- emacs config

;; --------------------------------------------------------------------------
;; I/O
;;
;; Switch to messages buffer to communicate with user during setup, &c
;; --------------------------------------------------------------------------

;; TODO: pop up temporary frame & switch to temporary buffer only if
;; TODO: diagnostic / status messages need to be displayed.
(setq initial-buffer-choice
      (lambda () (get-buffer "*Messages*")))

;; --------------------------------------------------------------------------
;; PACKAGES
;;
;; Bootstrap package manager.
;; --------------------------------------------------------------------------

(require 'package)

(setq package-archives
      '(("gnu" . "https://elpa.gnu.org/packages/")
	("marmalade" . "https://marmalade-repo.org/packages/")
	("melpa-stable" . "https://stable.melpa.org/packages/")))

(package-initialize)

(unless package-archive-contents
  (message "Fetching package index. This should only take a few moments.")
  (package-refresh-contents))

;; Fetch and install use-package, if it isn't already installed.
(unless (package-installed-p 'use-package)
  (message "Downloading and installing package 'use-package'.")
  (package-install 'use-package))

;; use-package usage:
;; (use-package foo
;;   ;; evaluated BEFORE package is loaded
;;   :init
;;   ;; evaluated AFTER package is loaded
;;   :config
;;   ;; if not available, install with package manager
;;   :ensure t
;;   )

;; Let's give elpy a try.
(use-package elpy
  :ensure t
  :config (elpy-enable))

(message (concat "\n"
		 "!!!   IF WORKING ON A PYTHON PROJECT    !!!\n"
                 "!!!  ENSURE THAT ALL ELPY HELPERS ARE   !!!\n"
		 "!!! INSTALLED BY RUNNING: 'elpy-config' !!!\n"))

(use-package solarized-theme
  :ensure t
  :config (load-theme 'solarized-dark t))

(use-package paredit
  :ensure t
  :config (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

;; (use-package flycheck
;;   :ensure t
;;   :init (global-flycheck-mode))

;; (use-package haskell-mode
;;   :ensure t)

;; -------------------------------------------------------------------
;; UI / VISUAL
;; -------------------------------------------------------------------

;; go to *scratch* instead of splash
;;(setf initial-buffer-choice (lambda () (get-buffer-create "*Messages*")))
(setq inhibit-splash-screen t)

;; set font & size according to which system.
(when (string= (system-name) "denali")
    (set-face-attribute 'default nil :family "inconsolata" :height 130))

;;(tool-bar-mode -1)

;; ;; Hooks.
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'column-number-mode)
(add-hook 'prog-mode-hook 'toggle-truncate-lines)
(add-hook 'prog-mode-hook 'show-paren-mode)

(setq show-paren-delay 0)
