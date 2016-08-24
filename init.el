;; Don't display the slpash screen.
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)

;; Use a sexy font, if possible.
(if (member "inconsolata" (font-family-list))
    (set-face-attribute 'default nil :family "inconsolata" :height 135))

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
  :init (global-flycheck-mode))

;; Hooks.
(add-hook 'prog-mode-hook 'linum-mode)
(add-hook 'prog-mode-hook 'column-number-mode)
(add-hook 'prog-mode-hook 'toggle-truncate-lines)
(add-hook 'prog-mode-hook 'show-paren-mode)
