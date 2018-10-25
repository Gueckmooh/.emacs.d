;;;;;;;;;;;;;;;;;;;;;;;
;;   ___  _ __ __ _  ;;
;;  / _ \| '__/ _` | ;;
;; | (_) | | | (_| | ;;
;;  \___/|_|  \__, | ;;
;;            |___/  ;;
;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :init

  ;; (add-hook 'org-mode-hook
  ;;           (lambda ()
  ;;             (flyspell-mode)))
  ;; (add-hook 'org-mode-hook
  ;;           (lambda ()
  ;;             (writegood-mode)))
  (add-hook 'org-mode-hook
            (lambda ()
              yas-minor-mode))

  (setq org-fast-tag-selection-single-key t)
  (setq org-use-fast-todo-selection t)
  (setq org-startup-truncated nil)

  ;; seting up the todo flags
  (setq org-log-done t)
  (setq org-todo-keywords
        '(
          (sequence "TODO(t)" "INPROGRESS(i)" "|" "DONE(d!)")
          (sequence "REPORT(r)" "BUG(b@)" "KNOWNCAUSE(k)" "|" "FIXED(f/!)")
          (sequence "|" "CANCELED(c@/!)")
          (type "LAURENCE(l)" "|" "DONE(d!)")
          ))

  (setq org-todo-keyword-faces
        '(
          ("INPROGRESS" . (:foreground "blue" :weight bold))
          ))

  (setq org-tag-persistent-alist
      '((:startgroup . nil)
        ("HOME" . ?h)
        ("FAC" . ?f)
        ("RESEARCH" . ?r)
        ("TEACHING" . ?t)
        (:endgroup . nil)
        (:startgroup . nil)
        ("OS" . ?o)
        ("DEV" . ?d)
        (:endgroup . nil)
        (:startgroup . nil)
        ("EASY" . ?e)
        ("MEDIUM" . ?m)
        ("HARD" . ?a)
        (:endgroup . nil)
        ("URGENT" . ?u)
        ("KEY" . ?k)
        ("BONUS" . ?b)
        ("noexport" . ?x)
        )
      )

(setq org-tag-faces
      '(
        ("HOME" . (:foreground "GoldenRod" :weight bold))
        ("RESEARCH" . (:foreground "GoldenRod" :weight bold))
        ("TEACHING" . (:foreground "GoldenRod" :weight bold))
        ("FAC" . (:foreground "IndianRed1" :weight bold))
        ("OS" . (:foreground "IndianRed1" :weight bold))
        ("DEV" . (:foreground "IndianRed1" :weight bold))
        ("URGENT" . (:foreground "Red" :weight bold))
        ("KEY" . (:foreground "Red" :weight bold))
        ("EASY" . (:foreground "OrangeRed" :weight bold))
        ("MEDIUM" . (:foreground "OrangeRed" :weight bold))
        ("HARD" . (:foreground "OrangeRed" :weight bold))
        ("BONUS" . (:foreground "GoldenRod" :weight bold))
        ("noexport" . (:foreground "LimeGreen" :weight bold))
        )
)

  (global-set-key (kbd "C-c a") 'org-agenda)
  )

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
     (C . t)
     (awk . t)
     (latex . t)
     (ocaml . t)
     (calc . t)
     ))

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

  (setq org-ditaa-jar-path "/usr/share/ditaa/ditaa.jar")

  (add-hook 'org-babel-after-execute-hook (lambda ()
                                            (condition-case nil
                                                (org-display-inline-images)
                                              (error nil)))
            'append)
  )

(use-package org-ref)

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'org-plus-contrib)
  (package-install 'org-plus-contrib))

(setq org-hide-emphasis-markers t)

(provide 'setup-org)
