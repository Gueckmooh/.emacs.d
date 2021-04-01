;;; setup-org-new.el --- Org mode configuration
;;
;;; Commentary:
;;

;;; Code:

(use-package org
  :commands org
  :defer t

  :bind
  (("C-c a" . org-agenda)
   ("C-c b" . org-switchb)
   ("C-c c" . org-capture)
   ("C-c l" . org-store-link))

  :config

  (setq org-directory (concat (getenv "HOME") "/org"))
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  (setq org-agenda-files (quote ("~/org" "~/git/org")))

  (add-to-list 'auto-mode-alist
               '("\\.\\(org\\|org_archive\\|txt\\)$" . org-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; PRETTY
  (set-terminal-coding-system 'utf-8)
  (set-keyboard-coding-system 'utf-8)
  (prefer-coding-system 'utf-8)

  (setq org-hide-leading-stars t)
  (setq org-alphabetical-lists t)
  (setq org-src-fontify-natively t)  ;; activate coloring in blocks
  (setq org-src-tab-acts-natively t) ;; have completion in blocks
  (setq org-hide-emphasis-markers t) ;; to hide the *,=, or / markers
  (setq org-pretty-entities t)       ;; to have \alpha, \to and others display
  (setq org-hide-macro-markers t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ORG CAPTURE
  (setq org-capture-templates
        (quote (("t" "todo" entry (file "~/org/refile.org")
                 "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)

                ("r" "respond" entry (file "~/org/refile.org")
                 "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n"
                 :clock-in t :clock-resume t :immediate-finish t)

                ("n" "note" entry (file "~/org/refile.org")
                 "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)

                ("j" "Journal" entry (file+datetree "~/org/journal.org")
                 "* %?\n%U\n" :clock-in t :clock-resume t)

                ("w" "org-protocol" entry (file "~/org/refile.org")
                 "* TODO Review %c\n%U\n" :immediate-finish t)

                ("m" "Meeting" entry (file "~/org/refile.org")
                 "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)

                ("p" "Phone call" entry (file "~/org/refile.org")
                 "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)

                ("h" "Habit" entry (file "~/org/refile.org")
                 "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; EXPORT
  (require 'ox-beamer)
  (require 'ox-md)

  ;; Need to install Pygment
  ;; Use sudo easy_install Pygments
  (require 'ox-latex)
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-latex-listings 'minted)

  (setq org-latex-pdf-process
        '("latexmk -pdflatex='pdflatex -interaction nonstopmode -shell-escape -output-directory %o' -pdf -bibtex -f %f"))

  ;; Open PDF -> Use Zathura
  (setq org-file-apps '((auto-mode . emacs)
                        ("\\.mm\\'" . default)
                        ("\\.x?html?\\'" . default)
                        ("\\.png\\'" . "feh %s")
                        ("\\.pdf\\'" . "zathura %s")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; TAGS

  (setq org-agenda-include-all-todo t)
  (setq org-agenda-include-diary t)

    (add-hook 'org-mode-hook
            (lambda ()
              (yas-minor-mode)))

  (setq org-fast-tag-selection-single-key t)
  (setq org-use-fast-todo-selection t)
  (setq org-startup-truncated nil)

  ;; seting up the todo flags
  (setq org-log-done t)
  (setq org-todo-keywords
        '(
          (sequence "TODO(t)" "NEXT(n)" "INPROGRESS(i)" "LATER(l)" "|" "DONE(d!)")
          (sequence "REPORT(r)" "BUG(b@)" "KNOWNCAUSE(k)" "UNDERREVIEW(u)" "|" "FIXED(f/!)" "NAP(g/!)" "NABOF(N/!)" "NOTREPRODUCIBLE(o/!)")
          (sequence "|" "CANCELED(c@/!)")
          (type "|" "DONE(d!)")
          (sequence "PLAN-TO-WATCH(p)" "WATCHING(w)" "HOLD(h)" "|" "WATCHED(x!)")
          ))

  (setq org-todo-keyword-faces
        '(
          ("INPROGRESS" . (:foreground "blue" :weight bold))
          ("LATER" . (:foreground "orange" :weight bold))
          ))

  (setq org-tag-persistent-alist
        '((:startgroup . nil)
          ("HOME" . ?h)
          ("FAC" . ?f)
          ("RESEARCH" . ?r)
          ("TEACHING" . ?t)
          ("MATHWORKS" . ?f)
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
          (:startgroup . nil)
          ("CAT_3" . ?e)
          ("CAT_2" . ?m)
          ("CAT_1" . ?a)
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
          ("MATHWORKS" . (:foreground "GoldenRod" :weight bold))
          ("FAC" . (:foreground "IndianRed1" :weight bold))
          ("OS" . (:foreground "IndianRed1" :weight bold))
          ("DEV" . (:foreground "IndianRed1" :weight bold))
          ("URGENT" . (:foreground "Red" :weight bold))
          ("KEY" . (:foreground "Red" :weight bold))
          ("EASY" . (:foreground "OrangeRed" :weight bold))
          ("MEDIUM" . (:foreground "OrangeRed" :weight bold))
          ("HARD" . (:foreground "OrangeRed" :weight bold))
          ("CAT_3" . (:foreground "Yellow" :weight bold))
          ("CAT_2" . (:foreground "Orange" :weight bold))
          ("CAT_1" . (:foreground "Red" :weight bold))
          ("BONUS" . (:foreground "GoldenRod" :weight bold))
          ("noexport" . (:foreground "LimeGreen" :weight bold))
          )
        )

  (defun gk/insert-date (prefix)
    "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
    (interactive "P")
    (let ((format (cond
                   ((not prefix) "** %Y-%m-%d")
                   ((equal prefix '(4)) "[%Y-%m-%d]"))))
      (insert (format-time-string format))))
  (global-set-key (kbd "C-c d") 'insert-date)


  (defun gk/insert-time-date (prefix)
    "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
    (interactive "P")
    (let ((format (cond
                   ((not prefix) "[%H:%M:%S; %d.%m.%Y]")
                   ((equal prefix '(4)) "[%H:%M:%S; %Y-%m-%d]"))))
      (insert (format-time-string format))))
  (global-set-key (kbd "C-c t") 'insert-time-date)

  (define-key org-mode-map (kbd "C-c <") 'ace-jump-char-mode)
  (define-key org-mode-map (kbd "C-c SPC") 'ace-jump-line-mode)
  )                                     ; Org mode

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; SOME STUFF
(use-package org-ref :ensure org :defer t)

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'org-plus-contrib)
  (package-install 'org-plus-contrib))

(defun flyspell-without-Mtab ()
  (interactive)
  (flyspell-mode-on)
  (define-key flyspell-mode-map (kbd "M-TAB") nil))

;; (add-hook 'org-mode-hook 'flyspell-without-Mtab)

(setq gk/org-template
      '((latex-header .
                      '(
                        "#+LATEX_CLASS_OPTIONS: [a4paper, 11pt]"
                        "#+LATEX_HEADER: \\usepackage[french]{babel}"
                        "#+LATEX_HEADER: \\usepackage[left=3cm, right=3cm, top=3cm, bottom=3cm]{geometry}"
                        "#+LATEX_HEADER: \\usepackage{hyperref}"
                        "#+LATEX_HEADER: \\usepackage{enumitem}"
                        "#+LATEX_HEADER: \\usepackage{tikz}"
                        "#+LATEX_HEADER: \\usepackage{grafcet}"
                        "#+LATEX_HEADER: \\usepackage{subcaption}"
                        "#+LATEX_HEADER: \\usepackage{multicol}"
                        "#+LATEX_HEADER: \\usepackage{lipsum}"
                        "#+LATEX_HEADER: \\usepackage[french]{algorithm2e}"
                        "#+LATEX_HEADER: \\usepackage{marginnote}"
                        "#+LATEX_HEADER: \\usepackage{float}"
                        "#+LATEX_HEADER: \\usepackage{scrextend}"
                        "#+LATEX_HEADER: \\usepackage{array}"
                        "#+LATEX_HEADER_EXTRA: \\usemintedstyle{emacs}"
                        "#+LATEX_HEADER_EXTRA: \\setlength{\\parskip}{0.6em}"
                        "#+LATEX_HEADER_EXTRA: \\setlength{\\itemsep}{.1cm}"
                        "#+LATEX_HEADER_EXTRA: \\setcounter{secnumdepth}{3}"
                        "#+LATEX_HEADER_EXTRA: \\setlist{nolistsep}"
                        "#+LATEX_HEADER_EXTRA: \\usetikzlibrary{arrows}"
                        "#+LATEX_HEADER_EXTRA: \\hypersetup{"
                        "#+LATEX_HEADER_EXTRA:     colorlinks = false,"
                        "#+LATEX_HEADER_EXTRA:     linkbordercolor = {white}"
                        "#+LATEX_HEADER_EXTRA: }"
                        ))))

(defun gk/org-insert-template ()
  "Insert my org templates."
  (interactive)
  (let ((keys
         (loop for (key value) in gk/org-template collect (symbol-name key))))
    (let
        ((choice (completing-read "Select template:" keys)))
      (let ((ll (car (cdr (cdr (assoc (intern choice) gk/org-template))))))
        (while ll (insert (format "%s\n" (pop ll)))))
      )))

(defun my-org-save-buffer ()
  (interactive)
  (save-buffer))

(defadvice my-org-save-buffer (after after-my-org-save-buffer)
  (org-latex-export-to-latex t))
(ad-activate 'my-org-save-buffer)

(provide 'setup-org)

;;; setup-org.el ends here
