;; init.el -- emacs config

;; --------------------------------------------------------------------------
;; GLOBALS / MISC
;; --------------------------------------------------------------------------

;; Don't litter my init.
(setq custom-file "/dev/null")
;; TODO: use a seperate custom-file to save into.

(setq-default indent-tabs-mode nil)

;; Disable backup files if the file is under version control.
;; Could add a lambda, but this keeps the contents of find-file-hook cleaner.
;; TODO: Do not use ‘make-local-variable’ to make a hook variable buffer-local.
;;       Instead, use ‘add-hook’ and specify t for the LOCAL argument.
(defun disable-backups-under-vc ()
  (when (vc-backend (buffer-file-name))
    (make-local-variable 'make-backup-files)
    (setq make-backup-files nil)))
(add-hook 'find-file-hook 'disable-backups-under-vc)


(defun delete-whitespace ()
    (unless (or (string= major-mode 'diff-mode)
                (string= major-mode 'hexl-mode))
      (delete-trailing-whitespace)))

(add-hook 'before-save-hook 'delete-whitespace)


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
        ("melpa" . "https://melpa.org/packages/")
        ;; ("melpa-stable" . "https://stable.melpa.org/packages/")
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
  ;; (use-package solarized-theme
  ;;   :ensure t
  ;;   :config (load-theme 'solarized-dark-high-contrast t))


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
    ;; (setq
    ;;       python-shell-interpreter "python3"
    ;;       python-shell-interpreter-args "-i"
    ;;       elpy-rpc-python-command "python3")
    )

  (use-package js2-mode
    :mode ("\\.[m]?js[mx]?\\'" . js2-mode)
    :interpreter (("nodejs" . js2-mode)
                  ("node" . js2-mode))
    :bind (:map js-mode-map
                ("C-x e" . js-comint-send-last-sexp)
                ("C-c b" . js-comint-send-buffer)
                ("C-c r" . js-comint-send-region))
    :config (setq js-indent-level 2))


  (use-package js-comint
    :bind (:map js-mode-map
                ("C-x e" . js-comint-send-last-sexp)
                ("C-c b" . js-comint-send-buffer)
                ("C-c r" . js-comint-send-region)))

  (use-package web-mode
    :ensure t
    :custom
    (web-mode-markup-indent-offset 2)
    (web-mode-css-indent-offset 2)
    (web-mode-code-indent-offset 2))
) ;; END when (package-installed-p 'use-package)


;; --------------------------------------------------------------------------
;; MODE SETTINGS & HOOKS
;; --------------------------------------------------------------------------

(defun trunc-long-lines ()
  "Always trunc long lines, calls (toggle-truncate-lines 1)."
  (toggle-truncate-lines 1))

;; prog-mode
;; todo: put me in a let block?
(setq prog-hooks '(display-line-numbers-mode
                   column-number-mode
                   trunc-long-lines
                   show-paren-mode))
(dolist (f prog-hooks)
  (add-hook 'prog-mode-hook f))
(makunbound 'prog-hooks)

;; dired mode
(setq dired-listing-switches "-DFhl --group-directories-first")
(add-hook 'dired-mode-hook 'trunc-long-lines)

;; cc mode
(require 'cc-vars) ;; load c-default-style now, else it will clobber ours.
(c-add-style "c-1tbs" '("k&r" (c-basic-offset . 4)))
(setq c-default-style (cons '(c-mode . "c-1tbs") c-default-style))
(add-hook 'c-mode-hook (lambda () (c-toggle-comment-style -1)))
(add-hook 'c-mode-hook (lambda () (electric-pair-mode 1)))
;; (setq c-1tbs (let* ((kr (copy-sequence
;;                          (seq-find (lambda (l) (string= (car l) "k&r")) c-style-alist)))
;;                     ;; (offset (seq-find
;;                     ;;          (lambda (l) (and (listp l)(eq 'c-basic-offset (car l))))
;;                     ;;          kr))
;;                     )
;;                ;;(setcdr offset 4)
;;                (cdr kr)))
;; (apply c-add-style `("c-1tbs" ,@c-1tbs))


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
      ;; Don't include inbox.org in agenda views since it's contents haven't
      ;; been sorted yet.
      org-agenda-files          (remove "~/Documents/gtd/inbox.org"
                                        (directory-files-recursively
                                         org-directory "\\`[^.].*\\.org\\'"))
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
(setq org-deadline-warning-days 3)

;; Most common. Refile inbox item to next-actions.org or a project.
;; This could be cleaned up a little.
;; TODO also nice would be to modify the headline after it is moved
;;      so that it becomes a TODO item, depending on its destination.
(setq org-refile-use-outline-path 'file
      org-refile-targets '((org-agenda-files :maxlevel . 1)
                           ("~/Documents/gtd/someday-maybe.org" :level . 1)))

;; --=[ 3. Do ]=--
;; TODO: a lot of the stuff in section 2 should probably be moved here.
;; (setq org-agenda-prefix-format
;;       '((agenda . " %i %-12:c%?-12t% s")
;;         ;;(agenda . " % s")
;;         (todo   . " %i %-12:c%?-12t% s")
;;         (tags   . " %i %-12:c")
;;         (search . " %i %-12:c")))
(setq org-log-done 'time)
(setq org-agenda-custom-commands
      '(("g"
         "Get Things Done (GTD)"
         (;; (agenda ""
          ;;         ((org-agenda-skip-function
          ;;           '(org-agenda-skip-entry-if 'deadline))
          ;;          (org-deadline-warning-days 0)))
          ;; (todo "NEXT"
          ;;       ((org-agenda-skip-function
          ;;         '(org-agenda-skip-entry-if 'deadline))
          ;;        (org-agenda-prefix-format "  %i %-12:c [%e] ")
          ;;        (org-agenda-overriding-header "\nTasks\n")))
          ;; (tags-todo "SCHEDULED=\"<today>\""
          ;;            ((org-agenda-overriding-header "Scheduled:")))
          (agenda nil
                  ((org-agenda-entry-types '(:deadline :scheduled))
                   (org-agenda-span 'day)
                   ;;(org-agenda-format-date "")
                   (org-agenda-format-date "  %A %d %B %Y")
                   (org-agenda-prefix-format '((agenda . "  % s")))
                   (org-agenda-deadline-leaders '("     Today: "
                                                  "In   %02d d.: "
                                                  "Late %02d d.: "))
                   (org-agenda-scheduled-leaders '(" Scheduled: "
                                                   " Sched.%2dx: "))
                   (org-deadline-warning-days 7)
                   ;; (org-agenda-skip-function
                   ;;  '(org-agenda-skip-entry-if 'notregexp "\\* NEXT"))
                   (org-agenda-overriding-header (concat
                                                  "Remember your priorities!"
                                                  "\n\nToday's Agenda:"))))
          (tags-todo "next"
                     ( ;; TODO why does this prefix-format work?? cf. next one
                      (org-agenda-prefix-format "  %?-12t% s")
                      (org-agenda-skip-function
                       '(org-agenda-skip-entry-if 'deadline))
                      (org-agenda-overriding-header "\nNext Actions:")))
          ;; TODO: also include scheduled items / habits that were
          ;;       completed today.
          (tags "CLOSED>=\"<today>\""
                ((org-agenda-prefix-format '((tags . "  % s")))
                 (org-agenda-overriding-header "\nCompleted Today:"))))
         ((org-agenda-block-separator nil)))))

;; --------------------------------------------------------------------------
;; LOCAL OVERRIDES
;; --------------------------------------------------------------------------

(load "~/.emacs.d/local-machine" t t)
