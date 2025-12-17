(require 'treesit) ; otherwise treesit-language-source-alist is undefined.

(defun ensure-c-grammar ()
  (unless (treesit-language-available-p 'c)
    (add-to-list 'treesit-language-source-alist
                 '(c "https://github.com/tree-sitter/tree-sitter-c" "v0.24.1"))
    (treesit-install-language-grammar 'c)))

(defun ensure-cpp-grammar ()
  (unless (treesit-language-available-p 'cpp)
    (add-to-list 'treesit-language-source-alist
                 '(cpp "https://github.com/tree-sitter/tree-sitter-cpp" "v0.23.4"))
    (treesit-install-language-grammar 'cpp)))

(defun swap-to-ts-mode (elem)
  (cons (car elem) (pcase (cdr elem)
                     ('c-mode 'c-ts-mode)
                     ('c++-mode 'c++-ts-mode)
                     ('c-or-c++-mode 'c-or-c++-ts-mode)
                     (mode mode))))

(defun c-line-comment-style ()
  "Set c-ts-mode default comment style to use // instead of /* */."
  (c-ts-mode-toggle-comment-style -1))

(ensure-c-grammar)
(ensure-cpp-grammar)
(setq auto-mode-alist (mapcar #'swap-to-ts-mode auto-mode-alist)
      c-ts-mode-indent-offset 4
      c-ts-mode-indent-style  'k&r)
(add-hook 'c-ts-base-mode-hook #'c-line-comment-style)
