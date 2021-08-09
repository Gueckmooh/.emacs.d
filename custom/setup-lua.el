;;; package --- Summary:

;;; Commentary:
;;;;;;;;;;;;;;;;;;;;;
;;  _              ;;
;; | |_   _  __ _  ;;
;; | | | | |/ _` | ;;
;; | | |_| | (_| | ;;
;; |_|\__,_|\__,_| ;;
;;;;;;;;;;;;;;;;;;;;;
;;; Code:

(use-package lua-mode
  :mode "\\.lua\\'"
  :ensure t
  :config
  (add-hook 'lua-mode-hook 'yas-minor-mode)
  (add-hook 'lua-mode-hook 'flycheck-mode)
  (add-hook 'lua-mode-hook 'company-mode)

  (defun custom-lua-repl-bindings ()
    (local-set-key (kbd "C-c C-s") 'lua-show-process-buffer)
    (local-set-key (kbd "C-c C-h") 'lua-hide-process-buffer))

  (defun lua-mode-company-init ()
    (setq-local company-backends '((company-lua
                                    company-etags
                                    company-dabbrev-code))))
  (define-key lua-mode-map (kbd "C-c C-c") 'lua-send-region)
  )

(use-package company-lua
  :commands (custom-lua-repl-bindings lua-mode-company-init)
  :ensure company
  :ensure lua-mode
  :ensure t
  :init
  (add-hook 'lua-mode-hook 'custom-lua-repl-bindings)
  (add-hook 'lua-mode-hook 'lua-mode-company-init)
  :config
  (require 'company)
  (setq lua-indent-level 2)
  (setq lua-indent-string-contents t))

(add-hook 'lua-mode #'lsp-deferred)

(provide 'setup-lua)
;;; setup-lua.el ends here
