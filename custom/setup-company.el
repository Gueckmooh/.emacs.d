;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   ___ ___  _ __ ___  _ __   __ _ _ __  _   _  ;;
;;  / __/ _ \| '_ ` _ \| '_ \ / _` | '_ \| | | | ;;
;; | (_| (_) | | | | | | |_) | (_| | | | | |_| | ;;
;;  \___\___/|_| |_| |_| .__/ \__,_|_| |_|\__, | ;;
;;                     |_|                |___/  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company
  :init
  (add-hook 'after-init-hook 'global-company-mode))

(use-package company-c-headers
  :init
  (add-to-list 'company-backends 'company-c-headers))

(provide 'setup-company)
