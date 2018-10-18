(use-package babel
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((ditaa . t)))
  )

(provide 'setup-babel)
