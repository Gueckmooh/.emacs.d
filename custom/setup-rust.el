;;; Package --- summary

;;; Commentary:
;;  ____            _
;; |  _ \ _   _ ___| |_
;; | |_) | | | / __| __|
;; |  _ <| |_| \__ \ |_
;; |_| \_\\__,_|___/\__|
;;
;;; Code:

(use-package lsp-mode :defer t)

(use-package rustic
  :ensure flycheck
  :mode ("\\.rs\\'" . rustic-mode)
  :commands rustic-mode
  :defer t
  :config
  (setq rustic-format-trigger 'on-save)
  (setq rustic-lsp-server 'rust-analyzer)
  (push 'rustic-clippy flycheck-checkers)
  (remove-hook 'rustic-mode-hook 'flycheck-mode)
  )

(provide 'setup-rust)
;;; setup-rust.el ends here
