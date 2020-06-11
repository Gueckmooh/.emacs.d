(use-package z3-mode
  :init
  (setq z3-solver-cmd "/usr/bin/z3")
  (add-to-list 'auto-mode-alist '("\\.smt2\\'" . z3-mode))
  )

(provide 'setup-z3)
