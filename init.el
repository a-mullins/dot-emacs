 ;; Bootstrap packages.
(require 'package)
(add-to-list 'package-archives
	     '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(package-initialize)
(unless (package-installed-p 'use-package)
  ;; FIXME bootstrapping message doesn't appear until after package archive
  ;;       is downloaded.
  (switch-to-buffer "*Messages*")
  (print "Bootstrapping packages. This should only take a minute...")
  (package-refresh-contents)
  (package-install 'use-package))

(use-package paredit
  :ensure t
  :init (add-hook 'emacs-lisp-mode-hook 'paredit-mode))

(use-package flycheck
  :ensure t
  :config (global-flycheck-mode))

(use-package haskell-mode
  :ensure t
  :config (add-hook 'haskell-mode-hook 'interactive-haskell-mode))

(use-package solarized-theme
  :ensure t
  :config (load-theme 'solarized-dark t))

;; UI & VISUALS
(set-face-attribute 'default nil :family "inconsolata" :height 130)
(tool-bar-mode -1)


;; HOOKS
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'column-number-mode)
(add-hook 'prog-mode-hook 'toggle-truncate-lines)
(add-hook 'prog-mode-hook 'show-paren-mode)


;; TRANSPARENCY
;(set-frame-parameter (selected-frame) 'alpha '(85 . 50))
;(add-to-list 'default-frame-alist '(alpha . (85 . 50)))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages (quote (use-package solarized-theme paredit flycheck))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
