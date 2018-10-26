(use-package ess
  :init
  (setq ess-eval-visibly-p nil)
  (setq ess-ask-for-ess-directory nil)
  (require 'ess-eldoc)
  ;;compile the first target in the Makefile in the current directory using F9
  (setq compilation-read-command nil)
  ;;show matching parentheses
  (show-paren-mode 1)
  )

(use-package ess-view)
(use-package ess-smart-equals)
(use-package ess-smart-underscore)

(provide 'setup-R)
