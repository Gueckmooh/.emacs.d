(require 'cc-mode)

(setq c-default-style "gnu")

(if (version< "25" emacs-version)
    (use-package fill-column-indicator
      :init
      (setq fci-rule-column 80)
      (setq fci-rule-color "#2b2b2b")

      (add-hook 'c-mode-hook 'fci-mode)
      (add-hook 'c++-mode-hook 'fci-mode)))

(provide 'setup-c)
