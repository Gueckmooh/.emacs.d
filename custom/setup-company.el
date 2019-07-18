;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   ___ ___  _ __ ___  _ __   __ _ _ __  _   _  ;;
;;  / __/ _ \| '_ ` _ \| '_ \ / _` | '_ \| | | | ;;
;; | (_| (_) | | | | | | |_) | (_| | | | | |_| | ;;
;;  \___\___/|_| |_| |_| .__/ \__,_|_| |_|\__, | ;;
;;                     |_|                |___/  ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package company
  :ensure t
  :defer t
  :bind ("C-;" . company-complete-common)
  :config
  (setq company-idle-delay              0
        company-minimum-prefix-length   3
        company-show-numbers            t
        company-tooltip-limit           20
        company-dabbrev-downcase        nil
        )
  )


(provide 'setup-company)
