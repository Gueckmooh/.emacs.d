;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;              _   _                  ;;
;;  _ __  _   _| |_| |__   ___  _ __   ;;
;; | '_ \| | | | __| '_ \ / _ \| '_ \  ;;
;; | |_) | |_| | |_| | | | (_) | | | | ;;
;; | .__/ \__, |\__|_| |_|\___/|_| |_| ;;
;; |_|    |___/                        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'python)

(use-package elpy                       ; TODO
  :defer t
  :ensure t
  :commands elpy-enable
  :init
  (with-eval-after-load 'python-mode (elpy-enable)))

(if (version< "25" emacs-version)
    (use-package anaconda-mode
      :defer t
      :ensure t
      :hook
      (python-mode-hook . anaconda-mode)
      (python-mode-hook . anaconda-eldoc-mode)))

(use-package virtualenvwrapper
  :defer t
  :ensure t
  :commands (venv-initialize-interactive-shells venv-initialize-eshell)
  :init
  (with-eval-after-load 'elpy-mode
    (progn
      (venv-initialize-interactive-shells)
      (venv-initialize-eshell))))

(setq python-shell-interpreter "/usr/bin/python")
(setq python-indent-offset 4)
;; (setq python-flymake-command '("flake8" "--ignore=E402,F401" "-"))
(setq python-flymake-command '("flake8" "-"))

(use-package company-jedi
  :demand t
  :ensure t
  :init
  (with-eval-after-load 'company (add-to-list 'company-backends 'company-jedi))
  )

(defun py-exec-file (&optional term)
  "Launches a terminal at the buffer's location.
TERM is the name of the terminal to launch."
  (interactive)
  (let ((python "python ")
        (buf (generate-new-buffer "py-exec-file-output")))
    (call-process-shell-command (concat python (buffer-file-name)) nil buf)))

(use-package python-black
  :defer t
  :after python
  :commands python-black-on-save-mode
  :init
  (add-hook 'python-mode-hook 'python-black-on-save-mode)
  :config
  (setq python-black-extra-args (list "-l" "79")))

(provide 'setup-python)
