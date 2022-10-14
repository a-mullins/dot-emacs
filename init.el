;; init.el -- emacs config


;; --------------------------------------------------------------------------
;; GLOBALS / MISC
;; --------------------------------------------------------------------------

;; Don't litter my init.
(setq custom-file "~/.emacs.d/custom-garbage.el")
(add-hook 'kill-emacs-hook
          (lambda () (if (file-exists-p "~/.emacs.d/custom-garbage.el")
                         (delete-file "~/.emacs.d/custom-garbage.el"))))

;; Don't insert tab characters.
(setq-default indent-tabs-mode nil)

;; Disable autosave files.
(setq auto-save-default nil)

;; Disable backup files if the file is under version control.
(add-hook 'find-file-hook
          (lambda () (when (vc-backend (buffer-file-name))
                       (make-local-variable 'make-backup-files)
                       (setq make-backup-files nil))))


;; --------------------------------------------------------------------------
;; UI / VISUAL
;; --------------------------------------------------------------------------

;; specify machine-specific faces in local override file instead.
;; see LOCAL OVERRIDES section.
;;
;; (apply 'set-face-attribute 'default nil
;;        (pcase (system-name)
;;          ("denali"    '(:family "inconsolata" :height 140))
;;          ("deimos"    '(:family "monaco" :height 150))
;;          (_           '(:family "courier" :height 150))))

(tool-bar-mode -1)
(scroll-bar-mode -1)
(setq show-paren-delay 0)


;; --------------------------------------------------------------------------
;; PACKAGES
;; --------------------------------------------------------------------------

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
  (when (yes-or-no-p "`use-package` is missing. Fetch it now? ")
    (package-refresh-contents)
    (package-install 'use-package)))

;; Packages are sorted by approximate frequency of use.
(when (package-installed-p 'use-package)
  ;; (use-package solarized-theme
  ;;   :ensure t
  ;;   :config (load-theme 'solarized-dark t))
  ;; (use-package subatomic-theme
  ;;   :ensure t
  ;;   :config (load-theme 'subatomic t))
  (use-package badwolf-theme
    :ensure t
    :config (load-theme 'badwolf t))

  (use-package paredit
    :ensure t
    :init
    (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
    (add-hook 'lisp-mode-hook 'paredit-mode))

  (use-package elpy
    :ensure t
    :defer t
    :init
    (advice-add 'python-mode :before 'elpy-enable)
    (setq
          python-shell-interpreter "python3"
          python-shell-interpreter-args "-i"
          elpy-rpc-python-command "python3")
    ;;(add-to-list 'exec-path (expand-file-name "~/.local/bin"))
    )

  (use-package js-comint
    :ensure t
    :config
    (add-hook 'js-mode-hook (lambda ()
                              (define-key js-mode-map (kbd "C-x e")
                                'js-comint-send-last-sexp)
                              (define-key js-mode-map (kbd "C-c b")
                                'js-comint-send-buffer)
                              (define-key js-mode-map (kbd "C-c r")
                                'js-comint-send-region)
                              )))

  ;; TODO: only turn on when window size is > 80 col?
  ;; (use-package fill-column-indicator
  ;;   :ensure t
  ;;   :init
  ;;   (setq fci-rule-column 80)
  ;;   :config
  ;;   (add-hook 'prog-mode-hook
  ;;             (lambda () (fci-mode 1))
  ;;             ))

  ;; (use-package markdown-mode
  ;;   :ensure t
  ;;   :commands (markdown-mode gfm-mode)
  ;;   :mode (("README\\.md\\'" . gfm-mode)
  ;;          ("\\.md\\'" . markdown-mode)
  ;;          ("\\.markdown\\'" . markdown-mode))
  ;;   :init (setq markdown-command "multimarkdown"))

  ;; (use-package slime
  ;;   :mode ("\\.cl\\'" . common-lisp-mode)
  ;;   :config
  ;;   (add-to-list 'slime-contribs 'slime-repl)
  ;;   (setq inferior-lisp-program "/usr/bin/clisp"))

) ;; END when (package-installed-p 'use-package)


;; --------------------------------------------------------------------------
;; MODE SETTINGS & HOOKS
;; --------------------------------------------------------------------------

;; prog-mode
(mapc
 (lambda (hook) (add-hook 'prog-mode-hook hook))
 '(
   linum-mode
   column-number-mode
   (lambda () (toggle-truncate-lines 1))
   show-paren-mode
   ))

;; dired mode
(setq dired-listing-switches "-DFhlX --group-directories-first")
(add-hook 'dired-mode-hook
          (lambda () (toggle-truncate-lines 1)))

;; --------------------------------------------------------------------------
;; GTD & ORG-MODE
;; --------------------------------------------------------------------------

;; This section uses org mode to implement David Allen's Getting
;; Things Done productivity system. It may not be useful for others
;; since each person's specific GTD setup is likely to be a little
;; different.
;;
;; Parts of this config were inspired by Nicolas Rougier's blog post:
;;   https://www.labri.fr/perso/nrougier/GTD/index.html


;; --=[ 0. Prelude ]=--
(require 'org)
(setq org-directory             "~/Documents/gtd/"
      org-agenda-files          (directory-files-recursively org-directory "\\`[^.].*\\.org\\'")
      org-mobile-directory      "~/Dropbox/Apps/MobileOrg"
      org-mobile-inbox-for-pull "~/Documents/gtd/from-mobile.org")

;; --=[ 1. Capture ]=--
;; note that the inbox entries are not TODOs in the org-mode sense.
;; this is because, in the GTD process, we have not yet decided if
;; the item is even actionable, so they shouldn't show up in
;; TODO-oriented agenda views. We can still collect them
;; using tags.
(define-key global-map (kbd "C-c c") 'org-capture)
(setq org-capture-templates
      '(("i" "Inbox" entry (file "inbox.org")
         "* %?\n%U")))

;; --=[ 2. Process ]=--
;; The inbox should be regularly emptied, with the items being sent
;; to different buckets. The org-refile (bound to C-c C-w) function
;; can be leveraged to accomplish this.
(define-key global-map (kbd "C-c a") 'org-agenda)
(add-hook 'org-agenda-mode-hook 'delete-other-windows)

;; Most common. Refile inbox item to next-actions.org or a project.
;; This could be cleaned up a little.
;; TODO also nice would be to modify the headline after it is moved
;;      so that it becomes a TODO item, depending on its destination.
(setq org-refile-use-outline-path 'file)
(setq org-refile-targets '((org-agenda-files :maxlevel . 1)))

;; --------------------------------------------------------------------------
;; LOCAL OVERRIDES
;; --------------------------------------------------------------------------

(load "~/.emacs.d/local-machine" t t)
