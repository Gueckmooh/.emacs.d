(defun desperately-compile ()
  "Traveling up the path, find a Makefile and `compile'."
  (interactive)
  (setq compile-command "make -k")
  (when (locate-dominating-file default-directory "Makefile")
    (with-temp-buffer
      (cd (locate-dominating-file default-directory "Makefile"))
      (call-interactively #'compile))))

(define-key c++-mode-map (kbd "C-c RET") 'desperately-compile)
(define-key c-mode-map (kbd "C-c RET") 'desperately-compile)

(define-key c++-mode-map (kbd "C-x RET RET") 'compile)
(define-key c-mode-map (kbd "C-x RET RET") 'compile)

(provide 'setup-compile)
