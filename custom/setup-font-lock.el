;;; Package --- summary

;;; Commentary:
;;   __             _     _            _
;;  / _| ___  _ __ | |_  | | ___   ___| | __
;; | |_ / _ \| '_ \| __| | |/ _ \ / __| |/ /
;; |  _| (_) | | | | |_  | | (_) | (__|   <
;; |_|  \___/|_| |_|\__| |_|\___/ \___|_|\_\
;;; Code:

(defface c-cpp-ok-face
  '((t :foreground "#22aa22"))
  "Face to display on @ok"
  :group 'font-lock-faces)

(defface c-cpp-todo-face
  '((t :foreground "#b50303"))
  "Face to display on @todo"
  :group 'font-lock-faces)

(defface c-cpp-easy-face
  '((t :inherit 'c-cpp-ok-face))
  "Face to display on @easy"
  :group 'font-lock-faces)

(defface c-cpp-question-face
  '((t :foreground "#e26302"))
  "Face to display on @medium"
  :group 'font-lock-faces)

(defface c-cpp-medium-face
  '((t :inherit 'c-cpp-question-face))
  "Face to display on @medium"
  :group 'font-lock-faces)

(defface c-cpp-hard-face
  '((t :inherit 'c-cpp-todo-face))
  "Face to display on @hard"
  :group 'font-lock-faces)

(defface c-cpp-progress-face
  '((t :foreground "#053efc"))
  "Face to display on @inprogress"
  :group 'font-lock-faces)

(defun upgrade-font-lock ()
  "Upgrades the font lock with @todo etc..."
  (font-lock-add-keywords
   nil
   '(("@\\<\\(todo\\|fixme\\|X+\\)\\>" 1 'c-cpp-todo-face prepend)
     ("@\\<\\(ok\\|done\\|fixed\\)\\>" 1 'c-cpp-ok-face prepend)
     ("@\\(\\?+\\|dropped\\|drop\\)" 1 'c-cpp-question-face prepend)
     ("@\\<\\(inprogress\\|review\\|note\\)\\>" 1 'c-cpp-progress-face prepend)
     ("@\\<\\(easy\\)\\>" 1 'c-cpp-easy-face prepend)
     ("@\\<\\(medium\\)\\>" 1 'c-cpp-medium-face prepend)
     ("@\\<\\(hard\\)\\>" 1 'c-cpp-hard-face prepend)
     ))
  )

(add-hook 'prog-mode-hook 'upgrade-font-lock)


(provide 'setup-font-lock)
;;; setup-setup-font-lock.el ends here
