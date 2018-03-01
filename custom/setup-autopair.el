(use-package autopair
  :init
  (add-hook 'c-mode-hook 'autopair-mode)
  (add-hook 'c++-mode-hook 'autopair-mode)
  (add-hook 'emacs-lisp-mode-hook 'autopair-mode)

(setq autopair-autowrap t)

(delete-selection-mode 1)

(put 'autopair-insert-opening 'delete-selection t)
(put 'autopair-skip-close-maybe 'delete-selection t)
(put 'autopair-insert-or-skip-quote 'delete-selection t)
(put 'autopair-extra-insert-opening 'delete-selection t)
(put 'autopair-extra-skip-close-maybe 'delete-selection t)
(put 'autopair-backspace 'delete-selection 'supersede)
(put 'autopair-newline 'delete-selection t))

(provide 'setup-autopair)
