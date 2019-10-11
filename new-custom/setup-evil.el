(use-package evil
  :config
  (defun my-evil-ex () (interactive)
         (if mark-active (evil-ex "'<,'>")
           (evil-ex)))

  (defmacro my-evil-virtual-macro (selection doc &rest body)
  (declare (indent defun)
           (debug (&define name stringp
                           [&rest keywordp sexp]
                           def-body)))
  (let* ((name (intern (format "my-evil-virtyal-%s" selection)))
         (message (intern (format "%s-message" name)))
         (type selection)
         arg key string)
    ;; collect keywords
    (while (keywordp (car-safe body))
      (setq key (pop body)
            arg (pop body))
      (cond
       ((eq key :message)
        (setq string arg))
       ((eq key :type)
        (setq type arg))))
    ;; macro expansion
    `(progn
       (add-to-list 'evil-visual-alist (cons ',selection ',name))
       (defvar ,name ',type ,(format "*%s" doc))
       (defvar ,message ,string ,doc)
       (evil-define-command ,name (&optional mark point type message)
         ,@(when doc `(,doc))
         :keep-visual t
         :repeat nil
         (interactive
          (list nil nil
                (if (and (evil-visual-state-p)
                         (eq evil-visual-selection ',selection))
                    'exit ,name) t))
         (if (eq type 'exit)
             (evil-exit-visual-state)
           (setq type (or type ,name)
                 evil-visual-selection ',selection)
           (evil-visual-refresh mark point type message)
           ,@body))
       ',selection)))


  ;; (evil-visual-refresh 1 1 selection))

(my-evil-virtual-macro char
  "Characterwise selection."
  :type inclusive
  :message "-- VISUAL --")

(defadvice next-line (after my-evil-virtual activate)
  "Advice around cursor movement"
  (if mark-active (my-evil-virtyal-char)))
(defadvice previous-line (after my-evil-virtual activate)
  "Advice around cursor movement"
  (if mark-active (my-evil-virtyal-char)))
(defadvice right-char (after my-evil-virtual activate)
  "Advice around cursor movement"
  (if mark-active (my-evil-virtyal-char)))
(defadvice left-char (after my-evil-virtual activate)
  "Advice around cursor movement"
  (if mark-active (my-evil-virtyal-char)))
(defadvice forward-word (after my-evil-virtual activate)
  "Advice around cursor movement"
  (if mark-active (my-evil-virtyal-char)))
(defadvice backward-word (after my-evil-virtual activate)
  "Advice around cursor movement"
  (if mark-active (my-evil-virtyal-char)))
(defadvice set-mark-command (after my-evil-virtual activate)
  (my-evil-virtyal-char (mark) (point)))
  )

(provide 'setup-evil)
