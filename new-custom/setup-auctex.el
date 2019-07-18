;;; Package --- summary

;;; Commentary:
;;                   _
;;   __ _ _   _  ___| |_ _____  __
;;  / _` | | | |/ __| __/ _ \ \/ /
;; | (_| | |_| | (__| ||  __/>  <
;;  \__,_|\__,_|\___|\__\___/_/\_\
;;
;;; Code:

(use-package tex-mode
  :ensure auctex
  :config
  (require 'tex)
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-save-query nil)

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

  (eval-after-load "tex"
    '(setcdr (assoc "LaTeX" TeX-command-list)
             '("%`%l%(mode) -synctex=1 -interaction=nonstopmode -shell-escape%' %t"
               TeX-run-TeX nil (latex-mode doctex-mode) :help "Run LaTeX")
             )
    )

  (TeX-source-correlate-mode)
  (TeX-PDF-mode)
  (add-to-list 'TeX-view-program-list '("zathura" zathura-forward-search))

  (setq zathura-procs ())
  (defun zathura-forward-search ()
    (interactive)
    (let* ((zathura-launch-buf (get-buffer-create "*Zathura Output*"))
           (pdfname (TeX-master-file "pdf"))
           (zatentry (assoc pdfname zathura-procs))
           (zatproc (if (and zatentry (process-live-p (cdr zatentry)))
                        (cdr zatentry)
                      (progn
                        (let ((proc (progn (message "Launching Zathura")
                                           (start-process "zathura-launch"
                                                          zathura-launch-buf "zathura"
                                                          "-x" "emacsclient +%{line} %{input}" pdfname))))
                          (when zatentry
                            (setq zathura-procs (delq zatentry zathura-procs)))
                          (add-to-list 'zathura-procs (cons pdfname proc))
                          (set-process-query-on-exit-flag proc nil)
                          proc))))
           (pid (process-id zatproc))
           (synctex (format "%s:0:%s"
                            (TeX-current-line)
                            (TeX-current-file-name-master-relative)))
           )
      (start-process "zathura-synctex" zathura-launch-buf "zathura" "--synctex-forward" synctex "--synctex-pid" (int-to-string pid) pdfname)
      ;; (start-process "raise-zathura-wmctrl" zathura-launch-buf "wmctrl" "-a" pdfname)
      ))

  (setq TeX-view-program-selection '(((output-dvi has-no-display-manager)
                                      "dvi2tty")
                                     ((output-dvi style-pstricks)
                                      "dvips and gv")
                                     (output-dvi "xdvi")
                                     (output-pdf "zathura")
                                     (output-html "xdg-open")))
  )

(use-package company-auctex
  :config
  (company-auctex-init)
  (add-hook 'LaTeX-mode-hook 'flycheck-mode)
  )

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
    (make-directory "/tmp/tex-scratch" t)))

(with-eval-after-load 'yasnippet (add-hook 'LaTeX-mode-hook 'yas-minor-mode))

(provide 'setup-auctex)
;;; setup-auctex.el ends here
