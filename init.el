;; init.el -- emacs config

;; --------------------------------------------------------------------------
;; GLOBALS / MISC
;; --------------------------------------------------------------------------

;; Don't litter my init.
(setq custom-file "/dev/null")

(setq-default indent-tabs-mode nil)

;; Disable backup files if the file is under version control.
;; Could add a lambda, but this keeps the contents of find-file-hook cleaner.
(defun disable-backups-under-vc ()
  (when (vc-backend (buffer-file-name))
    (make-local-variable 'make-backup-files)
    (setq make-backup-files nil)))
(add-hook 'find-file-hook 'disable-backups-under-vc)


;; --------------------------------------------------------------------------
;; UI / VISUAL
;; --------------------------------------------------------------------------

(tool-bar-mode 0)
(scroll-bar-mode 0)
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

;; If use-package is missing, prompt to download now.
(unless (package-installed-p 'use-package)
  (when (yes-or-no-p "`use-package` is missing. Fetch it now? ")
    (package-refresh-contents)
    (package-install 'use-package)))

;; Packages are sorted by approximate frequency of use.
(when (package-installed-p 'use-package)
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
    )

  (use-package js-comint
    :ensure t
    :config
    (defun make-js-mode-map ()
      (define-key js-mode-map (kbd "C-x e") 'js-comint-send-last-sexp)
      (define-key js-mode-map (kbd "C-c b") 'js-comint-send-buffer)
      (define-key js-mode-map (kbd "C-c r") 'js-comint-send-region))
    (add-hook 'js-mode-hook 'make-js-mode-map))
) ;; END when (package-installed-p 'use-package)


;; --------------------------------------------------------------------------
;; MODE SETTINGS & HOOKS
;; --------------------------------------------------------------------------

(defun trunc-long-lines ()
  (toggle-truncate-lines 1))

;; prog-mode
(mapc
 (lambda (f) (add-hook 'prog-mode-hook f))
 '(
   linum-mode
   column-number-mode
   trunc-long-lines
   show-paren-mode
   ))

;; dired mode
(setq dired-listing-switches "-DFhlX --group-directories-first")
(add-hook 'dired-mode-hook 'trunc-long-lines)


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
