;;;;;;;;;;;;;
;;   ____  ;;
;;  / ___| ;;
;; | |     ;;
;; | |___  ;;
;;  \____| ;;
;;;;;;;;;;;;;

(require 'cc-mode)


(setq c-default-style "gnu")

;; (if (version< "25" emacs-version)
;;     (use-package fill-column-indicator
;;       :init
;;       (setq fci-rule-column 80)
;;       (setq fci-rule-color "#2b2b2b")

;;       (add-hook 'c-mode-hook 'fci-mode)
;;       (add-hook 'c++-mode-hook 'fci-mode)))

(use-package function-args
  :ensure t
  :config
  (fa-config-default)
  (set-default 'semantic-case-fold t)
  (define-key c-mode-map (kbd "M-*") 'fa-show)
  (define-key c++-mode-map (kbd "M-*") 'fa-show)
  (define-key function-args-mode-map (kbd "M-n") nil)
  )

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))


(use-package flycheck
  :ensure t
  :hook ((c-mode-hook . flycheck-mode)
         (c-mode-common-hook . flycheck-mode)
         (c++-mode-hook . flycheck-mode))
  )

(use-package flycheck-clang-analyzer
  :ensure t
  :config
  (with-eval-after-load 'flycheck
    (require 'flycheck-clang-analyzer)
    (flycheck-clang-analyzer-setup)))

;; (add-hook 'c-mode-hook 'flycheck-mode)
;; (add-hook 'c++-mode-hook 'flycheck-mode)
;; (add-hook 'c++-mode-hook
;;           (lambda ()
;;             (setq flycheck-gcc-language-standard "c++11")))
;; (add-hook 'c++-mode-hook
;;           (lambda ()
;;             (setq flycheck-clang-language-standard "c++11")))



;; == irony-mode ==
(use-package irony
  :ensure t
  :defer t
  :init
  (add-hook 'c++-mode-hook 'irony-mode)
  (add-hook 'c-mode-hook 'irony-mode)
  (add-hook 'objc-mode-hook 'irony-mode)
  :config
  ;; replace the `completion-at-point' and `complete-symbol' bindings in
  ;; irony-mode's buffers by irony-mode's function
  (defun my-irony-mode-hook ()
    (define-key irony-mode-map [remap completion-at-point]
      'irony-completion-at-point-async)
    (define-key irony-mode-map [remap complete-symbol]
      'irony-completion-at-point-async))
  (add-hook 'irony-mode-hook 'my-irony-mode-hook)
  (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
  )

;; (use-package company-irony-c-headers
;;   :ensure t
;;   :config
;;   (eval-after-load 'company
;;     '(add-to-list
;;       'company-backends '(company-irony-c-headers company-irony))))

;; ;; I use irony with company to get code completion.
;; (use-package company-irony
;;   ;; :require company irony
;;   :config
;;   (progn
;;     (eval-after-load 'company '(add-to-list 'company-backends 'company-irony))))

;; ;; I use irony with flycheck to get real-time syntax checking.
;; (use-package flycheck-irony
;;   ;; :require flycheck irony
;;   :config
;;   (progn
;;     (eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))))

;; ;; Eldoc shows argument list of the function you are currently writing in the echo area.
;; (use-package irony-eldoc
;;   ;; :require eldoc irony
;;   :config
;;   (progn
;;     (add-hook 'irony-mode-hook #'irony-eldoc)))




;; (use-package electric-spacing
;;   :init
;;   (add-hook 'c-mode-hook 'electric-spacing-mode)
;;   (add-hook 'c++-mode-hook 'electric-spacing-mode))

;; replaced by company :)

;; (use-package auto-complete
;;   :init
;;   (ac-config-default))

;; (use-package ac-c-headers
;;   :init
;;   (add-hook 'c-mode-hook
;;             (lambda ()
;;               (add-to-list 'ac-sources 'ac-source-c-headers)
;;               (add-to-list 'ac-sources 'ac-source-c-header-symbols t))))

;;  ____ _____
;; |  _ \_   _|_ _  __ _ ___
;; | |_) || |/ _` |/ _` / __|
;; |  _ < | | (_| | (_| \__ \
;; |_| \_\|_|\__,_|\__, |___/
;;                 |___/

(use-package rtags
  :ensure t
  :config
  (progn
    (unless (rtags-executable-find "rc") (error "Binary rc is not installed!"))
    (unless (rtags-executable-find "rdm") (error "Binary rdm is not installed!"))

    (define-key c-mode-base-map (kbd "M-.") 'rtags-find-symbol-at-point)
    (define-key c-mode-base-map (kbd "M-,") 'rtags-find-references-at-point)
    (define-key c-mode-base-map (kbd "M-?") 'rtags-display-summary)
    (rtags-enable-standard-keybindings)

    (setq rtags-use-helm t)

    ;; Shutdown rdm when leaving emacs.
    (add-hook 'kill-emacs-hook 'rtags-quit-rdm)
    ))

;; TODO: Has no coloring! How can I get coloring?
(use-package helm-rtags
  :ensure t
  :config
  (progn
    (setq rtags-display-result-backend 'helm)
    ))

;; Use rtags for auto-completion.
(use-package company-rtags
  :ensure t
  :config
  (progn
    (setq rtags-autostart-diagnostics t)
    (rtags-diagnostics)
    (setq rtags-completions-enabled t)
    (push 'company-rtags company-backends)
    ))

;; Live code checking.
(use-package flycheck-rtags
  :ensure t
  :config
  (progn
    ;; ensure that we use only rtags checking
    ;; https://github.com/Andersbakken/rtags#optional-1
    (defun setup-flycheck-rtags ()
      (flycheck-select-checker 'rtags)
      (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
      (setq-local flycheck-check-syntax-automatically nil)
      (rtags-set-periodic-reparse-timeout 2.0)  ;; Run flycheck 2 seconds after being idle.
      )
    (add-hook 'c-mode-hook #'setup-flycheck-rtags)
    (add-hook 'c++-mode-hook #'setup-flycheck-rtags)
    ))

(use-package company-c-headers
  :ensure t
  :config
  (eval-after-load 'company '(add-to-list 'company-backends 'company-c-headers))
  )







(add-hook 'c++-mode-hook 'yas-minor-mode)
(add-hook 'c-mode-hook 'yas-minor-mode)

(defun make-C-scratch ()
  (interactive)
  (find-file "/tmp/C-scratch/src/scratch.c")
  (gnus-make-directory "/tmp/C-scratch")
  (gnus-make-directory "/tmp/C-scratch/src")
  (if (equal 0 (buffer-size))
      (progn
        (insert "#include <stdio.h>\n\nint\nmain (void)\n{\n  ")
        (let ((pp (point)))
          (progn
            (insert "printf (\"Hello, World!\\n\");\n  return 0;\n}")
            (goto-char pp)))))
  (if (not (file-exists-p "/tmp/C-scratch/Makefile"))
      (copy-file (concat (getenv "HOME") "/.emacs.d/templates/C-Makefile")
                 "/tmp/C-scratch/Makefile")))

(defun make-C++-scratch ()
  (interactive)
  (find-file "/tmp/C++-scratch/src/scratch.cc")
  (gnus-make-directory "/tmp/C++-scratch")
  (gnus-make-directory "/tmp/C++-scratch/src")
  (if (equal 0 (buffer-size))
      (progn
        (insert "#include <iostream>\nusing namespace std;")
        (insert "\n\nint\nmain (void)\n{\n  ")
        (let ((pp (point)))
          (progn
            (insert "cout << \"Hello, World!\" << endl;\n  return 0;\n}")
            (goto-char pp)))))
  (if (not (file-exists-p "/tmp/C++-scratch/Makefile"))
      (copy-file (concat (getenv "HOME") "/.emacs.d/templates/C-Makefile")
                 "/tmp/C++-scratch/Makefile")))

(provide 'setup-c)
