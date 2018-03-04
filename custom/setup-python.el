(use-package elpy
  :init
  (elpy-enable))

(use-package anaconda-mode
  :init
  (add-hook 'python-mode-hook 'anaconda-mode)
  (add-hook 'python-mode-hook 'anaconda-eldoc-mode))

(use-package virtualenvwrapper
  :init
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell))

(provide 'setup-python)
