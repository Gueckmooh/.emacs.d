;;; Package --- summary

;;; Commentary:
;;   ____
;;  / ___|
;; | |
;; | |___
;;  \____|
;;; Code:

(add-hook 'c++-mode-hook (lambda () (c-set-offset 'innamespace 0)))

;;       _
;;  _ __| |_ __ _  __ _ ___
;; | '__| __/ _` |/ _` / __|
;; | |  | || (_| | (_| \__ \
;; |_|   \__\__,_|\__, |___/
;;                |___/

(use-package rtags
  :defer t
  :commands rtags-start-process-unless-running
  :init
  (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
  (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
  (add-hook 'objc-mode-hook 'rtags-start-process-unless-running)
  :config
  (unless (rtags-executable-find "rc") (error "Binary rc is not installed!"))
  (unless (rtags-executable-find "rdm") (error "Binary rdm is not installed!"))

  (define-key c-mode-base-map (kbd "M-.") 'rtags-find-symbol-at-point)
  (define-key c-mode-base-map (kbd "M-,") 'rtags-find-references-at-point)
  (define-key c-mode-base-map (kbd "M-?") 'rtags-display-summary)
  (rtags-enable-standard-keybindings)

  (add-hook 'kill-emacs-hook 'rtags-quit-rdm)
  )

;; TODO: Has no coloring! How can I get coloring?
(use-package helm-rtags
  :ensure helm
  :after rtags
  :commands rtags-helm-setup
  :init
  (add-hook 'c-mode-hook 'rtags-helm-setup)
  (add-hook 'c++-mode-hook 'rtags-helm-setup)
  (add-hook 'objc-mode-hook 'rtags-helm-setup)
  :config
  (defun rtags-helm-setup ()
    (setq rtags-display-result-backend 'helm))
  )

;; Use rtags for auto-completion.
(use-package company-rtags
  :ensure company
  :after (company rtags)
  :defer t
  :commands rtags-company-setup
  :init
  (add-hook 'c-mode-hook 'rtags-company-setup)
  (add-hook 'c++-mode-hook 'rtags-company-setup)
  (add-hook 'objc-mode-hook 'rtags-company-setup)
  :config
  (defun rtags-company-setup ()
    (setq rtags-autostart-diagnostics t)
    (rtags-diagnostics)
    (setq rtags-completions-enabled t)
    )
  (push 'company-rtags company-backends)
  )

(use-package company-c-headers
  :after company
  :hook ((c-mode . company-c-headers-setup)
         (c++-mode . company-c-headers-setup))
  :config
  (defun company-c-headers-setup ())
  (eval-after-load 'company '(add-to-list 'company-backends 'company-c-headers))
  )

(use-package flycheck-rtags
  :ensure flycheck
  :after flycheck
  :commands my-flycheck-rtags-setup
  :init
  (add-hook 'c-mode-hook #'my-flycheck-rtags-setup)
  (add-hook 'c++-mode-hook #'my-flycheck-rtags-setup)
  (add-hook 'objc-mode-hook #'my-flycheck-rtags-setup)
  :config
  (setq rtags-autostart-diagnostics t)
  (defun my-flycheck-rtags-setup ()
    (flycheck-select-checker 'rtags)
    (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
    (setq-local flycheck-check-syntax-automatically nil)))


(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(provide 'setup-c)
;;; setup-c.el ends here
