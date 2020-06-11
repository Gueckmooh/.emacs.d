;; Install the company backend for perl
(use-package company-plsense
  :init
  (add-hook 'perl-mode-hook 'company-mode)
  )

;; Install the flycheck backend for perl
;; (use-package flycheck-perl6
;;   :init
;;   (add-hook 'perl-mode-hook 'flycheck-mode)
;;   )

(add-hook 'perl-mode-hook 'yas-minor-mode)

(provide 'setup-perl)
