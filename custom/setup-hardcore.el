;; Use shell-like backspace C-h, rebind help to F1
(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "<f1>") 'help-command)





(require 'hardcore-mode)
(global-hardcore-mode)

(provide 'setup-hardcore)
