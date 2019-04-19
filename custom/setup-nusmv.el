(add-to-list 'load-path "~/.emacs.d/lisp/NuSMV")
(require 'nusmv-mode)

(add-to-list 'auto-mode-alist '("\\.smv\\'" . nusmv-mode))

(provide 'setup-nusmv)
