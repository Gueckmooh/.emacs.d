(unless (package-installed-p 'auctex)
  (package-install 'auctex))

(require 'tex)
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq TeX-save-query nil)
                                        ;(setq TeX-PDF-mode t)

(use-package company-auctex
  :init
  (company-auctex-init))

;;(require 'flymake)

;;(defun flymake-get-tex-args (file-name)
;;(list "pdflatex"
;;(list "-file-line-error" "-draftmode" "-interaction=nonstopmode" file-name)))

                                        ;(add-hook 'LaTeX-mode-hook 'flymake-mode)

(setq ispell-program-name "aspell") ; could be ispell as well, depending on your preferences
(setq ispell-dictionary "french") ; this can obviously be set to any language your spell-checking program supports

(define-key global-map (kbd "C-c C-s a") (lambda () (interactive)
                                         (ispell-change-dictionary "american")))
(define-key global-map (kbd "C-c C-s f") (lambda () (interactive)
                                         (ispell-change-dictionary "francais")))
(define-key global-map (kbd "C-c C-s r") 'flyspell-region)
(define-key global-map (kbd "C-c C-s b") 'flyspell-buffer)
(define-key global-map (kbd "C-c C-s s") 'flyspell-mode)
(define-key LaTeX-mode-map (kbd "C-x RET RET") 'desperately-compile)

(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-buffer)
(add-hook 'LaTeX-mode-hook 'yas-minor-mode)

(defun turn-on-outline-minor-mode ()
  (outline-minor-mode 1))

(add-hook 'LaTeX-mode-hook 'turn-on-outline-minor-mode)
(add-hook 'latex-mode-hook 'turn-on-outline-minor-mode)
(setq outline-minor-mode-prefix "\C-c \C-o") ; Or something else

(require 'tex-site)
(autoload 'reftex-mode "reftex" "RefTeX Minor Mode" t)
(autoload 'turn-on-reftex "reftex" "RefTeX Minor Mode" nil)
(autoload 'reftex-citation "reftex-cite" "Make citation" nil)
(autoload 'reftex-index-phrase-mode "reftex-index" "Phrase Mode" t)
(add-hook 'latex-mode-hook 'turn-on-reftex) ; with Emacs latex mode
;; (add-hook 'reftex-load-hook 'imenu-add-menubar-index)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)

(setq LaTeX-eqnarray-label "eq"
      LaTeX-equation-label "eq"
      LaTeX-figure-label "fig"
      LaTeX-table-label "tab"
      LaTeX-myChapter-label "chap"
      TeX-auto-save t
      TeX-newline-function 'reindent-then-newline-and-indent
      TeX-parse-self t
      TeX-style-path
      '("style/" "auto/"
        "/usr/share/emacs21/site-lisp/auctex/style/"
        "/var/lib/auctex/emacs21/"
        "/usr/local/share/emacs/site-lisp/auctex/style/")
      LaTeX-section-hook
      '(LaTeX-section-heading
        LaTeX-section-title
        LaTeX-section-toc
        LaTeX-section-section
        LaTeX-section-label))

;; (setq-default TeX-master "main.tex") ; All master files called "main.tex".

(setq tex-scratch-packages '(
                             "\\usepackage[utf8]{inputenc}"
                             "\\usepackage[T1]{fontenc}"
                             "\\usepackage{graphicx}"
                             "\\usepackage{grffile}"
                             "\\usepackage{longtable}"
                             "\\usepackage{wrapfig}"
                             "\\usepackage{rotating}"
                             "\\usepackage[normalem]{ulem}"
                             "\\usepackage{amsmath}"
                             "\\usepackage{textcomp}"
                             "\\usepackage{amssymb}"
                             "\\usepackage{capt-of}"
                             "\\usepackage{hyperref}"
                             "\\usepackage[french, frenchb]{babel}"
                             "\\usepackage[left=3cm, right=3cm, top=3cm, bottom=3cm]{geometry}"
                             "\\usepackage{minted}"
                             "\\usemintedstyle{emacs}"
                             "\\usepackage{hyperref}"
                             "\\usepackage{enumitem}"
                             "\\usepackage{tikz}"
                             "\\usepackage{grafcet}"
                             "\\usepackage{subcaption}"
                             "\\usepackage{multicol}"
                             "\\usepackage{lipsum}"
                             "\\usepackage[french]{algorithm2e}"
                             "\\usepackage{marginnote}"
                             "\\usepackage{float}"
                             "\\usepackage{scrextend}"
                             "\\usepackage{array}"))

(defun make-tex-scratch ()
  (interactive)
  (find-file "/tmp/tex-scratch/scratch.tex")
  (if (equal 0 (buffer-size))
      (progn
        (insert (format "%s\n" "\\documentclass[a4paper, 11pt]{report}"))
        (let ((tmpl tex-scratch-packages))
        (while tmpl (progn (insert (format "%s\n" (pop tmpl))))))
        (insert "\\begin{document}\n\n")
        (let ((pp (point)))
          (progn (insert "\n\n\\end{document}")
                 (goto-char pp))))
  (gnus-make-directory "/tmp/tex-scratch")
  )

;; For minted
(eval-after-load "tex"
  '(setcdr (assoc "LaTeX" TeX-command-list)
          '("%`%l%(mode) -shell-escape%' %t"
          TeX-run-TeX nil (latex-mode doctex-mode) :help "Run LaTeX")
    )
  )

(provide 'setup-latex)
