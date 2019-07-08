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
  :init (add-hook 'after-init-hook 'global-company-mode)
  :config
  (use-package company-irony :ensure t :defer t)
  (setq company-idle-delay              0
        company-minimum-prefix-length   2
        company-show-numbers            t
        company-tooltip-limit           20
        company-dabbrev-downcase        nil
        company-backends                '((company-irony company-gtags))
        )
  :bind ("C-;" . company-complete-common)
  )

(use-package company-c-headers
  :init
  ;; (setq company-backends (delete 'company-clang company-backends))
  (add-to-list 'company-backends 'company-c-headers)
  (add-to-list 'company-backends 'company-auctex)
  (add-to-list 'company-backends 'company-jedi))


(define-key company-active-map "\t" 'company-yasnippet-or-completion)

(unless (package-installed-p 'yasnippet-snippets)
  (package-install 'yasnippet-snippets))

(defun company-yasnippet-or-completion ()
  (interactive)
  (if (yas/expansion-at-point)
      (progn (company-abort)
             (yas/expand))
    (company-complete-common)))

(defun yas/expansion-at-point ()
  "Tested with v0.6.1. Extracted from `yas/expand-1'"
  (first (yas/current-key)))

(add-to-list 'company-c-headers-path-system "/usr/include/c++/9.1.0")
(add-to-list 'company-c-headers-path-system "/usr/include/libxml2")

(use-package company-jedi
  :init
(defun my/python-mode-hook ()
  (add-to-list 'company-backends 'company-jedi))

(add-hook 'python-mode-hook 'my/python-mode-hook))

(provide 'setup-company)
