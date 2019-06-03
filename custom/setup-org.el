;;;;;;;;;;;;;;;;;;;;;;;
;;   ___  _ __ __ _  ;;
;;  / _ \| '__/ _` | ;;
;; | (_) | | | (_| | ;;
;;  \___/|_|  \__, | ;;
;;            |___/  ;;
;;;;;;;;;;;;;;;;;;;;;;;

(use-package org
  :init

  (setq org-directory "~/org/")
  (setq org-default-notes-file "~/org/notes.org")

  (add-to-list 'auto-mode-alist '("\\.\\(org\\|org_archive\\|txt\\)$" .
                                  org-mode))

  ;; Standard key bindings
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c b") 'org-iswitchb)

  (setq org-agenda-files (quote ("~/org"
                                 "~/git/org")))

  ;; scratch

  ;; Custom Key Bindings
;; (global-set-key (kbd "<f7>") 'bh/set-truncate-lines)
;; (global-set-key (kbd "<f9> g") 'gnus)
;; (global-set-key (kbd "<f9> h") 'bh/hide-other)
;; (global-set-key (kbd "<f9> n") 'bh/toggle-next-task-display)

;; (global-set-key (kbd "C-c C-p I") 'bh/punch-in)
;; (global-set-key (kbd "C-c C-p O") 'bh/punch-out)


;; (global-set-key (kbd "<f9> r") 'boxquote-region)

;; (global-set-key (kbd "<f9> t") 'bh/insert-inactive-timestamp)
;; (global-set-key (kbd "<f9> T") 'bh/toggle-insert-inactive-timestamp)

;; (global-set-key (kbd "<f9> v") 'visible-mode)
;; (global-set-key (kbd "<f9> l") 'org-toggle-link-display)
;; (global-set-key (kbd "<f9> SPC") 'bh/clock-in-last-task)
;; (global-set-key (kbd "C-<f9>") 'previous-buffer)
;; (global-set-key (kbd "M-<f9>") 'org-toggle-inline-images)
;; (global-set-key (kbd "C-x n r") 'narrow-to-region)
;; (global-set-key (kbd "C-<f10>") 'next-buffer)
;; (global-set-key (kbd "<f11>") 'org-clock-goto)
;; (global-set-key (kbd "C-<f11>") 'org-clock-in)
;; (global-set-key (kbd "C-s-<f12>") 'bh/save-then-publish)
;; (global-set-key (kbd "C-c c") 'org-capture)

(defun bh/is-project-p ()
  "Any task with a todo keyword subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task has-subtask))))

(defun bh/is-project-subtree-p ()
  "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
  (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                              (point))))
    (save-excursion
      (bh/find-project-task)
      (if (equal (point) task)
          nil
        t))))

(defun bh/is-task-p ()
  "Any task with a todo keyword and no subtask"
  (save-restriction
    (widen)
    (let ((has-subtask)
          (subtree-end (save-excursion (org-end-of-subtree t)))
          (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
      (save-excursion
        (forward-line 1)
        (while (and (not has-subtask)
                    (< (point) subtree-end)
                    (re-search-forward "^\*+ " subtree-end t))
          (when (member (org-get-todo-state) org-todo-keywords-1)
            (setq has-subtask t))))
      (and is-a-task (not has-subtask)))))

(defun bh/is-subproject-p ()
  "Any task which is a subtask of another project"
  (let ((is-subproject)
        (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
    (save-excursion
      (while (and (not is-subproject) (org-up-heading-safe))
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq is-subproject t))))
    (and is-a-task is-subproject)))

(defun bh/list-sublevels-for-projects-indented ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels 'indented)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defun bh/list-sublevels-for-projects ()
  "Set org-tags-match-list-sublevels so when restricted to a subtree we list all subtasks.
  This is normally used by skipping functions where this variable is already local to the agenda."
  (if (marker-buffer org-agenda-restrict-begin)
      (setq org-tags-match-list-sublevels t)
    (setq org-tags-match-list-sublevels nil))
  nil)

(defvar bh/hide-scheduled-and-waiting-next-tasks t)

(defun bh/toggle-next-task-display ()
  (interactive)
  (setq bh/hide-scheduled-and-waiting-next-tasks (not bh/hide-scheduled-and-waiting-next-tasks))
  (when  (equal major-mode 'org-agenda-mode)
    (org-agenda-redo))
  (message "%s WAITING and SCHEDULED NEXT Tasks" (if bh/hide-scheduled-and-waiting-next-tasks "Hide" "Show")))

(defun bh/skip-stuck-projects ()
  "Skip trees that are not stuck projects"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                nil
              next-headline)) ; a stuck project, has subtasks but no next task
        nil))))

(defun bh/skip-non-stuck-projects ()
  "Skip trees that are not stuck projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (if (bh/is-project-p)
          (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                 (has-next ))
            (save-excursion
              (forward-line 1)
              (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                (unless (member "WAITING" (org-get-tags-at))
                  (setq has-next t))))
            (if has-next
                next-headline
              nil)) ; a stuck project, has subtasks but no next task
        next-headline))))

(defun bh/skip-non-projects ()
  "Skip trees that are not projects"
  ;; (bh/list-sublevels-for-projects-indented)
  (if (save-excursion (bh/skip-non-stuck-projects))
      (save-restriction
        (widen)
        (let ((subtree-end (save-excursion (org-end-of-subtree t))))
          (cond
           ((bh/is-project-p)
            nil)
           ((and (bh/is-project-subtree-p) (not (bh/is-task-p)))
            nil)
           (t
            subtree-end))))
    (save-excursion (org-end-of-subtree t))))

(defun bh/skip-non-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-task-p)
        nil)
       (t
        next-headline)))))

(defun bh/skip-project-trees-and-habits ()
  "Skip trees that are projects"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits-and-single-tasks ()
  "Skip trees that are projects, tasks that are habits, single non-project tasks"
  (save-restriction
    (widen)
    (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((org-is-habit-p)
        next-headline)
       ((and bh/hide-scheduled-and-waiting-next-tasks
             (member "WAITING" (org-get-tags-at)))
        next-headline)
       ((bh/is-project-p)
        next-headline)
       ((and (bh/is-task-p) (not (bh/is-project-subtree-p)))
        next-headline)
       (t
        nil)))))

(defun bh/skip-project-tasks-maybe ()
  "Show tasks related to the current restriction.
When restricted to a project, skip project and sub project tasks, habits, NEXT tasks, and loose tasks.
When not restricted, skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max))))
           (limit-to-project (marker-buffer org-agenda-restrict-begin)))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (not limit-to-project)
             (bh/is-project-subtree-p))
        subtree-end)
       ((and limit-to-project
             (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-project-tasks ()
  "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       ((bh/is-project-subtree-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-project-tasks ()
  "Show project tasks.
Skip project and sub-project tasks, habits, and loose non-project tasks."
  (save-restriction
    (widen)
    (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
           (next-headline (save-excursion (or (outline-next-heading) (point-max)))))
      (cond
       ((bh/is-project-p)
        next-headline)
       ((org-is-habit-p)
        subtree-end)
       ((and (bh/is-project-subtree-p)
             (member (org-get-todo-state) (list "NEXT")))
        subtree-end)
       ((not (bh/is-project-subtree-p))
        subtree-end)
       (t
        nil)))))

(defun bh/skip-projects-and-habits ()
  "Skip trees that are projects and tasks that are habits"
  (save-restriction
    (widen)
    (let ((subtree-end (save-excursion (org-end-of-subtree t))))
      (cond
       ((bh/is-project-p)
        subtree-end)
       ((org-is-habit-p)
        subtree-end)
       (t
        nil)))))

(defun bh/skip-non-subprojects ()
  "Skip trees that are not projects"
  (let ((next-headline (save-excursion (outline-next-heading))))
    (if (bh/is-subproject-p)
        nil
      next-headline)))

(defun bh/hide-other ()
  (interactive)
  (save-excursion
    (org-back-to-heading 'invisible-ok)
    (hide-other)
    (org-cycle)
    (org-cycle)
    (org-cycle)))

(defun bh/set-truncate-lines ()
  "Toggle value of truncate-lines and refresh window display."
  (interactive)
  (setq truncate-lines (not truncate-lines))
  ;; now refresh window display (an idiom from simple.el):
  (save-excursion
    (set-window-start (selected-window)
                      (window-start (selected-window)))))

(defun bh/make-org-scratch ()
  (interactive)
  (find-file "/tmp/publish/scratch.org")
  (gnus-make-directory "/tmp/publish"))
(global-set-key (kbd "C-c o") 'bh/make-org-scratch)


(defun bh/switch-to-scratch ()
  (interactive)
  (create-scratch-buffer))
(global-set-key (kbd "C-c C-c s") 'bh/switch-to-scratch)

;; Capture templates for: TODO tasks, Notes, appointments, phone calls, meetings, and org-protocol
(setq org-capture-templates
      (quote (("t" "todo" entry (file "~/org/refile.org")
               "* TODO %?\n%U\n%a\n" :clock-in t :clock-resume t)
              ("r" "respond" entry (file "~/org/refile.org")
               "* NEXT Respond to %:from on %:subject\nSCHEDULED: %t\n%U\n%a\n" :clock-in t :clock-resume t :immediate-finish t)
              ("n" "note" entry (file "~/org/refile.org")
               "* %? :NOTE:\n%U\n%a\n" :clock-in t :clock-resume t)
              ("j" "Journal" entry (file+datetree "~/org/diary.org")
               "* %?\n%U\n" :clock-in t :clock-resume t)
              ("w" "org-protocol" entry (file "~/org/refile.org")
               "* TODO Review %c\n%U\n" :immediate-finish t)
              ("m" "Meeting" entry (file "~/org/refile.org")
               "* MEETING with %? :MEETING:\n%U" :clock-in t :clock-resume t)
              ("p" "Phone call" entry (file "~/org/refile.org")
               "* PHONE %? :PHONE:\n%U" :clock-in t :clock-resume t)
              ("h" "Habit" entry (file "~/org/refile.org")
               "* NEXT %?\n%U\n%a\nSCHEDULED: %(format-time-string \"%<<%Y-%m-%d %a .+1d/3d>>\")\n:PROPERTIES:\n:STYLE: habit\n:REPEAT_TO_STATE: NEXT\n:END:\n"))))

;; clocking

;;
;; Resume clocking task when emacs is restarted
(org-clock-persistence-insinuate)
;;
;; Show lot of clocking history so it's easy to pick items off the C-F11 list
(setq org-clock-history-length 23)
;; Resume clocking task on clock-in if the clock is open
(setq org-clock-in-resume t)
;; Change tasks to NEXT when clocking in
(setq org-clock-in-switch-to-state 'bh/clock-in-to-next)
;; Separate drawers for clocking and logs
(setq org-drawers (quote ("PROPERTIES" "LOGBOOK")))
;; Save clock data and state changes and notes in the LOGBOOK drawer
(setq org-clock-into-drawer t)
;; Sometimes I change tasks I'm clocking quickly - this removes clocked tasks with 0:00 duration
(setq org-clock-out-remove-zero-time-clocks t)
;; Clock out when moving task to a done state
(setq org-clock-out-when-done t)
;; Save the running clock and all clock history when exiting Emacs, load it on startup
(setq org-clock-persist t)
;; Do not prompt to resume an active clock
(setq org-clock-persist-query-resume nil)
;; Enable auto clock resolution for finding open clocks
(setq org-clock-auto-clock-resolution (quote when-no-clock-is-running))
;; Include current clocking task in clock reports
(setq org-clock-report-include-clocking-task t)

(setq bh/keep-clock-running nil)

(defun bh/clock-in-to-next (kw)
  "Switch a task from TODO to NEXT when clocking in.
Skips capture tasks, projects, and subprojects.
Switch projects and subprojects from NEXT back to TODO"
  (when (not (and (boundp 'org-capture-mode) org-capture-mode))
    (cond
     ((and (member (org-get-todo-state) (list "TODO"))
           (bh/is-task-p))
      "INPROGRESS")
     ((and (member (org-get-todo-state) (list "INPROGRESS"))
           (bh/is-project-p))
      "TODO"))))

(defun bh/find-project-task ()
  "Move point to the parent (project) task if any"
  (save-restriction
    (widen)
    (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
      (while (org-up-heading-safe)
        (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
          (setq parent-task (point))))
      (goto-char parent-task)
      parent-task)))

(defun bh/punch-in (arg)
  "Start continuous clocking and set the default task to the
selected task.  If no task is selected set the Organization task
as the default task."
  (interactive "p")
  (setq bh/keep-clock-running t)
  (if (equal major-mode 'org-agenda-mode)
      ;;
      ;; We're in the agenda
      ;;
      (let* ((marker (org-get-at-bol 'org-hd-marker))
             (tags (org-with-point-at marker (org-get-tags-at))))
        (if (and (eq arg 4) tags)
            (org-agenda-clock-in '(16))
          (bh/clock-in-organization-task-as-default)))
    ;;
    ;; We are not in the agenda
    ;;
    (save-restriction
      (widen)
      ; Find the tags on the current task
      (if (and (equal major-mode 'org-mode) (not (org-before-first-heading-p))
               (eq arg 4))
          (org-clock-in '(16))
        (bh/clock-in-organization-task-as-default)))))

(defun bh/punch-out ()
  (interactive)
  (setq bh/keep-clock-running nil)
  (when (org-clock-is-active)
    (org-clock-out))
  (org-agenda-remove-restriction-lock))

(defun bh/clock-in-default-task ()
  (save-excursion
    (org-with-point-at org-clock-default-task
      (org-clock-in))))

(defun bh/clock-in-parent-task ()
  "Move point to the parent (project) task if any and clock in"
  (let ((parent-task))
    (save-excursion
      (save-restriction
        (widen)
        (while (and (not parent-task) (org-up-heading-safe))
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (if parent-task
            (org-with-point-at parent-task
              (org-clock-in))
          (when bh/keep-clock-running
            (bh/clock-in-default-task)))))))

(defvar bh/organization-task-id "eb155a82-92b2-4f25-a3c6-0304591af2f9")

(defun bh/clock-in-organization-task-as-default ()
  (interactive)
  (org-with-point-at (org-id-find bh/organization-task-id 'marker)
    (org-clock-in '(16))))

(defun bh/clock-out-maybe ()
  (when (and bh/keep-clock-running
             (not org-clock-clocking-in)
             (marker-buffer org-clock-default-task)
             (not org-clock-resolving-clocks-due-to-idleness))
    (bh/clock-in-parent-task)))

(require 'org-id)
(defun bh/clock-in-task-by-id (id)
  "Clock in a task by id"
  (org-with-point-at (org-id-find id 'marker)
    (org-clock-in nil)))

(defun bh/clock-in-last-task (arg)
  "Clock in the interrupted task if there is one
Skip the default task and get the next one.
A prefix arg forces clock in of the default task."
  (interactive "p")
  (let ((clock-in-to-task
         (cond
          ((eq arg 4) org-clock-default-task)
          ((and (org-clock-is-active)
                (equal org-clock-default-task (cadr org-clock-history)))
           (caddr org-clock-history))
          ((org-clock-is-active) (cadr org-clock-history))
          ((equal org-clock-default-task (car org-clock-history)) (cadr org-clock-history))
          (t (car org-clock-history)))))
    (widen)
    (org-with-point-at clock-in-to-task
      (org-clock-in nil))))

(add-hook 'org-clock-out-hook 'bh/clock-out-maybe 'append)

;; (global-set-key (kbd "<f12>") 'org-agenda)
;; (global-set-key (kbd "<f5>") 'bh/org-todo)
;; (global-set-key (kbd "<S-f5>") 'bh/widen)
;; (global-set-key (kbd "<f7>") 'bh/set-truncate-lines)
;; (global-set-key (kbd "<f8>") 'org-cycle-agenda-files)
;; (global-set-key (kbd "<f9> <f9>") 'bh/show-org-agenda)
;; (global-set-key (kbd "<f9> b") 'bbdb)
;; (global-set-key (kbd "<f9> c") 'calendar)
;; (global-set-key (kbd "<f9> f") 'boxquote-insert-file)
;; (global-set-key (kbd "<f9> g") 'gnus)
;; (global-set-key (kbd "<f9> h") 'bh/hide-other)
;; (global-set-key (kbd "<f9> n") 'bh/toggle-next-task-display)

(global-set-key (kbd "<f5> I") 'bh/punch-in)
(global-set-key (kbd "<f5> O") 'bh/punch-out)
(global-set-key (kbd "<f5> D") '(lambda () (interactive)
                                  (bh/clock-in-default-task)))
(global-set-key (kbd "<f5> <f5>") 'org-clock-goto)

;; (global-set-key (kbd "<f9> o") 'bh/make-org-scratch)

;; (global-set-key (kbd "<f9> r") 'boxquote-region)
;; (global-set-key (kbd "<f9> s") 'bh/switch-to-scratch)

;; (global-set-key (kbd "<f9> t") 'bh/insert-inactive-timestamp)
;; (global-set-key (kbd "<f9> T") 'bh/toggle-insert-inactive-timestamp)

;; (global-set-key (kbd "<f9> v") 'visible-mode)
;; (global-set-key (kbd "<f9> l") 'org-toggle-link-display)
(global-set-key (kbd "<f5> SPC") 'bh/clock-in-last-task)
;; (global-set-key (kbd "C-<f9>") 'previous-buffer)
;; (global-set-key (kbd "M-<f9>") 'org-toggle-inline-images)
;; (global-set-key (kbd "C-x n r") 'narrow-to-region)
;; (global-set-key (kbd "C-<f10>") 'next-buffer)
;; (global-set-key (kbd "<f11>") 'org-clock-goto)
;; (global-set-key (kbd "C-<f11>") 'org-clock-in)
;; (global-set-key (kbd "C-s-<f12>") 'bh/save-then-publish)
;; (global-set-key (kbd "C-c c") 'org-capture)


;; end clocking



;; end scratch

;; ;; Defining capture template
;; (setq org-capture-templates
;;       '(
;;         ("j" "Journal Entry"
;;          entry (file+datetree "~/org/journal.org")
;;          "* %?"
;;          :empty-lines 1)
;;         ))

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

  ;; as utf8 http://orgmode.org/manual/Special-symbols.html

  ;; Setup export
  (require 'ox-beamer)
  (require 'ox-md)

  ;; Need to install Pygment
  ;; Use sudo easy_install Pygments
  (require 'ox-latex)
  (add-to-list 'org-latex-packages-alist '("" "minted"))
  (setq org-latex-listings 'minted)

  ;; (setq org-latex-pdf-process
  ;;       '("pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
  ;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"
;;         "pdflatex -shell-escape -interaction nonstopmode -output-directory %o %f"))
(setq org-latex-pdf-process
      '("latexmk -pdflatex='pdflatex -interaction nonstopmode -shell-escape -output-directory %o' -pdf -bibtex -f %f"))

;; (setq org-latex-pdf-process
;;       '("rubber -d --shell-escape %f"))

  ;; Open PDF -> Use Zathura
  (setq org-file-apps '((auto-mode . emacs)
                        ("\\.mm\\'" . default)
                        ("\\.x?html?\\'" . default)
                        ("\\.png\\'" . "feh %s")
                        ("\\.pdf\\'" . "zathura %s")))

  (setq org-agenda-include-all-todo t)
  (setq org-agenda-include-diary t)

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
          (sequence "TODO(t)" "NEXT(n)" "INPROGRESS(i)" "LATER(l)" "|" "DONE(d!)")
          (sequence "REPORT(r)" "BUG(b@)" "KNOWNCAUSE(k)" "|" "FIXED(f/!)")
          (sequence "|" "CANCELED(c@/!)")
          (type "LAURENCE(L)" "|" "DONE(d!)")
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

  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c a") 'org-agenda)
  (global-set-key (kbd "C-c b") 'org-iswitchb)

  (defun insert-date (prefix)
    "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
    (interactive "P")
    (let ((format (cond
                   ((not prefix) "** %Y-%m-%d")
                   ((equal prefix '(4)) "[%Y-%m-%d]"))))
      (insert (format-time-string format))))
  (global-set-key (kbd "C-c d") 'insert-date)


  (defun insert-time-date (prefix)
    "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
    (interactive "P")
    (let ((format (cond
                   ((not prefix) "[%H:%M:%S; %d.%m.%Y]")
                   ((equal prefix '(4)) "[%H:%M:%S; %Y-%m-%d]"))))
      (insert (format-time-string format))))
  (global-set-key (kbd "C-c t") 'insert-time-date)


  )

(if (version<= "25.1" emacs-version)
    (progn
      (use-package ob-async
        :init
        (org-babel-do-load-languages
         'org-babel-load-languages
         '((sh . t)
           (shell . t)
           (ditaa . t)
           (R . t)
           (python . t)
           (perl . t)
           (plantuml . t)
           (org . t)
           (dot . t)
           (ruby . t)
           (js . t)
           (C . t)
           (awk . t)
           (latex . t)
           (ocaml . t)
           (calc . t)
           (maxima . t)
           ))

        (setq org-babel-python-command
              (if (memq system-type '(windows-nt ms-dos))
                  "Python"
                "/home/brignone/anaconda3/bin/python"))

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

        (add-hook 'org-mode-hook 'org-display-inline-images)
        (add-hook 'org-mode-hook 'org-babel-result-hide-all)

        (global-set-key (kbd "C-c S-t") 'org-babel-execute-subtree)

        (add-to-list 'org-structure-template-alist
                     '("ll" "@@latex:?@@" "?"))

        (add-to-list 'org-structure-template-alist
                     '("s" "#+begin_src ?\n\n#+end_src" "<src lang=\"?\">\n\n</src>"))

        (add-to-list 'org-structure-template-alist
                     '("m" "#+begin_src emacs-lisp :tangle init.el\n\n#+end_src" "<src lang=\"emacs-lisp\">\n\n</src>"))

        (add-to-list 'org-structure-template-alist
                     '("r" "#+begin_src R :results output :session *R* :exports both\n\n#+end_src" "<src lang=\"R\">\n\n</src>"))

        (add-to-list 'org-structure-template-alist
                     '("R" "#+begin_src R :results output graphics :file (org-babel-temp-file \"figure\" \".png\") :exports both :width 600 :height 400 :session *R* \n\n#+end_src" "<src lang=\"R\">\n\n</src>"))

        (add-to-list 'org-structure-template-alist
                     '("b" "#+begin_src shell :results output :exports both\n\n#+end_src" "<src lang=\"shell\">\n\n</src>"))

        (add-to-list 'org-structure-template-alist
                     '("RR" "#+begin_src R :results output graphics :file  (org-babel-temp-file (concat (file-name-directory (or load-file-name buffer-file-name)) \"figure-\") \".png\") :exports both :width 600 :height 400 :session *R* \n\n#+end_src" "<src lang=\"R\">\n\n</src>"))

        (add-to-list 'org-structure-template-alist
                     '("p" "#+begin_src python :results output :exports both\n\n#+end_src" "<src lang=\"python\">\n\n</src>"))

        (add-to-list 'org-structure-template-alist
                     '("P" "#+begin_src python :results output :session :exports both\n\n#+end_src" "<src lang=\"python\">\n\n</src>"))

        (add-to-list 'org-structure-template-alist
                     '("PP" "#+begin_src python :results file :session :var matplot_lib_filename=(org-babel-temp-file \"figure\" \".png\") :exports both\nimport matplotlib.pyplot as plt\n\nimport numpy\nx=numpy.linspace(-15,15)\nplt.figure(figsize=(10,5))\nplt.plot(x,numpy.cos(x)/x)\nplt.tight_layout()\n\nplt.savefig(matplot_lib_filename)\nmatplot_lib_filename\n#+end_src" "<src lang=\"python\">\n\n</src>"))

        )
      )
  )

(use-package org-ref)

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
  "Insert my org templates"
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

(define-key org-mode-map (kbd "C-c <") 'ace-jump-char-mode)
(define-key org-mode-map (kbd "C-c SPC") 'ace-jump-line-mode)


(provide 'setup-org)
