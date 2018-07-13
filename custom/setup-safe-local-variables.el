(add-to-list 'safe-local-variable-values
             '(flycheck-clang-args . ("-D__DEBUG"))
             '(flycheck-clang-warnings . ("all" "extra" "shadow")))

(provide 'setup-safe-local-variables)
