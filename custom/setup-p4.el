;;; Package --- summary

;;; Commentary:
;;; Code:

(use-package p4
  :commands (p4-opened p4-edit p4-diff)
  :config
  (setq p4-executable "/usr/local/netbin/p4")
  )

(provide 'setup-p4)

;;; setup-p4.el ends here
