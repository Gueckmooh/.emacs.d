(require 'cl)

(defun gk/make-eshell (&rest l)
  (let ((command
         (cond ((equal l nil) "make")
               ((equal l '(nil)) "make")
               ((listp (car l)) (concat "make "
                                        (reduce #'(lambda (a b) (concat a " " b)) (car l))))
               (t (concat "make "
                          (reduce #'(lambda (a b) (concat a " " b)) (car l))))
               )
         ))
    (compile command)
    ))

(defun eshell/find (&rest args)
  (if (and
       (member "-exec" args)
       (= "grep" (car (cdr (member "-exec" args)))))
      (message "Yay")
    (eshell-eval-command (cons "find" args))))

(provide 'setup-eshell)
