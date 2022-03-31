;;; Package --- summary

;;; Commentary:
;;  ____            _
;; |  _ \ _   _ ___| |_
;; | |_) | | | / __| __|
;; |  _ <| |_| \__ \ |_
;; |_| \_\\__,_|___/\__|
;;
;;; Code:

;; (use-package lsp-mode :defer t)

;; (use-package rustic
;;   :ensure flycheck
;;   :mode ("\\.rs\\'" . rustic-mode)
;;   :commands rustic-mode
;;   :defer t
;;   :config
;;   (setq rustic-format-trigger 'on-save)
;;   (setq rustic-lsp-server 'rust-analyzer)
;;   (add-hook 'rustic-mode-hook #'lsp)
;;   ;; (push 'rustic-clippy flycheck-checkers)
;;   ;; (remove-hook 'rustic-mode-hook 'flycheck-mode)
;;   )

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode)
  :commands rust-mode
  :config
  (setq rust-format-on-save t)
  (add-hook 'rust-mode-hook #'lsp))

(provide 'setup-rust)
;;; setup-rust.el ends here
