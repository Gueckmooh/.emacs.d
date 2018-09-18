;;;;;;;;;;;;;;;;;;;;;;;
;;   ___  _ __ __ _  ;;
;;  / _ \| '__/ _` | ;;
;; | (_) | | | (_| | ;;
;;  \___/|_|  \__, | ;;
;;            |___/  ;;
;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :init
  (setq org-log-done t
        org-todo-keywords '((sequence "TODO" "INPROGRESS" "DONE"))
        org-todo-keyword-faces '(("INPROGRESS" . (:foreground "blue" :weight bold))))
  (add-hook 'org-mode-hook
            (lambda ()
              (flyspell-mode)))
  (add-hook 'org-mode-hook
            (lambda ()
              (writegood-mode))))

(use-package ob-async
  :init
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((sh . t)
     (ditaa . t)
     (plantuml . t)
     (dot . t)
     (ruby . t)
     (js . t)
     (C . t)))

  (add-to-list 'org-src-lang-modes (quote ("dot". graphviz-dot)))
  (add-to-list 'org-src-lang-modes (quote ("plantuml" . fundamental)))
  (add-to-list 'org-babel-tangle-lang-exts '("clojure" . "clj"))

  (defvar org-babel-default-header-args:clojure
    '((:results . "silent") (:tangle . "yes")))

  (defun org-babel-execute:clojure (body params)
    (lisp-eval-string body)
    "Done!")

  (setq org-src-fontify-natively t
        org-confirm-babel-evaluate nil)

  (add-hook 'org-babel-after-execute-hook (lambda ()
                                            (condition-case nil
                                                (org-display-inline-images)
                                              (error nil)))
            'append))

(use-package org-ref)

(provide 'setup-org)
