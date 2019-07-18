;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              _   _                  ;;
;;  _ __  _   _| |_| |__   ___  _ __   ;;
;; | '_ \| | | | __| '_ \ / _ \| '_ \  ;;
;; | |_) | |_| | |_| | | | (_) | | | | ;;
;; | .__/ \__, |\__|_| |_|\___/|_| |_| ;;
;; |_|    |___/                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package elpy
  :ensure t
  :config
  (elpy-enable)
  )

(if (version< "25" emacs-version)
    (use-package anaconda-mode
      :ensure t
      :hook
      (python-mode-hook . anaconda-mode)
      (python-mode-hook . anaconda-eldoc-mode)))

(use-package virtualenvwrapper
  :ensure t
  :config
  (venv-initialize-interactive-shells)
  (venv-initialize-eshell))

(setq python-shell-interpreter "/usr/bin/python")
(setq python-indent 2)

(use-package company-jedi
  :ensure t
  :config
  (with-eval-after-load 'company (add-to-list 'company-backends 'company-jedi))
  )

(provide 'setup-python)
