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
  (setq company-backends (delete 'company-clang company-backends))
  (add-to-list 'company-backends 'company-c-headers))

(add-to-list 'company-c-headers-path-system "/usr/include/c++/6.3.0")

(provide 'setup-company)
