(use-package graphviz-dot-mode
  :init
  (setq graphviz-dot-view-command "xdot %s"))

(defun make-dot-scratch ()
  (interactive)
  (find-file "/tmp/dot-scratch/scratch.dot")
  (gnus-make-directory "/tmp/dot-scratch"))

(provide 'setup-dot)
