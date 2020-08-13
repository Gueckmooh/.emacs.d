;;; Package --- summary

;;; Commentary:
;;   ____
;;  / ___|
;; | |
;; | |___
;;  \____|
;;; Code:

(use-package rtags
  :config
  (progn
    (unless (rtags-executable-find "rc") (error "Binary rc is not installed!"))
    (unless (rtags-executable-find "rdm") (error "Binary rdm is not installed!"))

    (define-key c-mode-base-map (kbd "M-.") 'rtags-find-symbol-at-point)
    (define-key c-mode-base-map (kbd "M-,") 'rtags-find-references-at-point)
    (define-key c-mode-base-map (kbd "M-?") 'rtags-display-summary)
    (rtags-enable-standard-keybindings)

    ;; (setq rtags-use-helm t)

    ;; (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
    ;; (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
    ;; (add-hook 'objc-mode-hook 'rtags-start-process-unless-running)

    ;; Shutdown rdm when leaving emacs.
    ;; (add-hook 'kill-emacs-hook 'rtags-quit-rdm)
    ))

;; TODO: Has no coloring! How can I get coloring?
(use-package helm-rtags
  :config
  (progn
    (setq rtags-display-result-backend 'helm)
    ))

;; Use rtags for auto-completion.
(use-package company-rtags
  :config
  (progn
    (setq rtags-autostart-diagnostics t)
    (rtags-diagnostics)
    (setq rtags-completions-enabled t)
    (push 'company-rtags company-backends)
    ))

(use-package company-c-headers
  :config
  (eval-after-load 'company '(add-to-list 'company-backends 'company-c-headers))
  )

(use-package flycheck-clang-analyzer
  :ensure t
  :config
  (with-eval-after-load 'flycheck (flycheck-clang-analyzer-setup))
  (add-hook 'c++-mode-hook
            (lambda ()
              (setq flycheck-clang-language-standard "c++11")))


  ;; TO USE COMPILE_COMMAND.JSON
  (require 'json)

  (defun gk/get-compile-command-element (vec name)
    (let ((elts vec)
          (file nil))
      (progn (while elts
               (if (equal (car (car elts)) name)
                   (setq file (cdr (car elts))))
               (setq elts (cdr elts)))) file))

  (defun gk/get-compile-command-arguments (vec)
    (let* ((arguments (gk/get-compile-command-element vec 'arguments))
           (directory (gk/get-compile-command-element vec 'directory))
           (args (append arguments nil)))
      (mapcar (lambda (v)
                (if (string-match-p "^-I" v)
                    (concat "-I" directory "/" (substring v 2 nil))
                  v)) args)))

  (defun gk/filter-compile-command-args (args)
    (-filter (lambda (v)
               (or
                (string-match-p "^-I" v)
                (string-match-p "^-D" v)
                )) args))

  (defun gk/filter-compile-command-includes (args)
    (-filter (lambda (v)
               (or
                (string-match-p "^-I" v)
                )) args))


  (defun gk/get-compile-command-from-file (vec filename)
    (let ((clist (append vec nil))
          (command nil))
      (while clist
        (let* ((current (car clist))
               (reg (concat (regexp-quote (gk/get-compile-command-element current 'file)) "$")))
          (if (string-match reg filename)
              (setq command current))
          (setq clist (cdr clist))
          ))
      command))

  (defun gk/remove-I (l)
    (mapcar (lambda (v) (substring v 2 nil)) l))


  (defun gk/set-flycheck-clang-args ()
    (when (locate-dominating-file default-directory "compile_commands.json")
      (let* ((cfile
              (concat (locate-dominating-file default-directory "compile_commands.json") "/compile_commands.json"))
             (compile-commands-list (json-read-file cfile))
             (compile-commands (gk/get-compile-command-from-file compile-commands-list (buffer-file-name)))
             (includes (gk/filter-compile-command-includes (gk/get-compile-command-arguments compile-commands))))
        (make-local-variable 'flycheck-clang-include-path)
        (setq flycheck-clang-include-path (gk/remove-I includes))
        )
      )
    )

  (add-hook 'c++-mode-hook 'gk/set-flycheck-clang-args)

  )

;; (use-package flycheck-clangcheck
;;   :ensure t
;;   :config
;;   (defun my-select-clangcheck-for-checker ()
;;     "Select clang-check for flycheck's checker."
;;     (flycheck-set-checker-executable 'c/c++-clangcheck
;;                                      "/usr/bin/clang-check")
;;     (flycheck-select-checker 'c/c++-clangcheck))

;;   (add-hook 'c-mode-hook #'my-select-clangcheck-for-checker)
;;   (add-hook 'c++-mode-hook #'my-select-clangcheck-for-checker)

;;   ;; enable static analysis
;;   (setq flycheck-clangcheck-analyze t)
;;   )

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(provide 'setup-c)
;;; setup-c.el ends here
