;;; Package --- summary

;;; Commentary:
;;  ____             _
;; |  _ \  __ _ _ __| |_
;; | | | |/ _` | '__| __|
;; | |_| | (_| | |  | |_
;; |____/ \__,_|_|   \__|
;;
;;; Code:

(use-package lsp-dart
  :demand t)

(use-package dart-mode
  :demand t
  :config
  (add-hook 'dart-mode-hook #'lsp-deferred)

  (define-key dart-mode-map (kbd "<backtab>") nil))

(use-package hover)

(provide 'setup-dart)
;;; setup-dart.el ends here
