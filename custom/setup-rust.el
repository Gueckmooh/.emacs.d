;;; Package --- summary

;;; Commentary:
;;  ____            _
;; |  _ \ _   _ ___| |_
;; | |_) | | | / __| __|
;; |  _ <| |_| \__ \ |_
;; |_| \_\\__,_|___/\__|
;;
;;; Code:

;; (use-package rust-mode
;;   :bind
;;   (("C-c C-c" . rust-run))

;;   :config
;;   (setq rust-format-on-save t)
;;   )

(use-package lsp-mode)

(require 'flycheck)

(use-package rustic

  :config
  (setq rustic-format-trigger 'on-save)
  (setq rustic-lsp-server 'rust-analyzer)
  (push 'rustic-clippy flycheck-checkers)
  (remove-hook 'rustic-mode-hook 'flycheck-mode)
  )

(provide 'setup-rust)
;;; setup-rust.el ends here
