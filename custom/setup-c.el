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

;; (use-package function-args
;;   :init
;;   (fa-config-default)
;;   (set-default 'semantic-case-fold t)
;;   (define-key c-mode-map (kbd "M-*") 'fa-show)
;;   (define-key c++-mode-map (kbd "M-*") 'fa-show)
;;   (define-key function-args-mode-map (kbd "M-n") nil))

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))


;; (req-package company
;;   :config
;;   (progn
;;     (add-hook 'after-init-hook 'global-company-mode)
;;     (global-set-key (kbd "M-/") 'company-complete-common-or-cycle)
;;     (setq company-idle-delay 0)))

;; (req-package flycheck
;;   :config
;;   (progn
;;     (global-flycheck-mode)))

(use-package flycheck-clang-analyzer
  :ensure t
  :config
  (with-eval-after-load 'flycheck
    (require 'flycheck-clang-analyzer)
    (flycheck-clang-analyzer-setup)))

(add-hook 'c-mode-hook 'flycheck-mode)
(add-hook 'c++-mode-hook 'flycheck-mode)
(add-hook 'c++-mode-hook
          (lambda ()
            (setq flycheck-gcc-language-standard "c++11")))
(add-hook 'c++-mode-hook
          (lambda ()
            (setq flycheck-clang-language-standard "c++11")))



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


;; (use-package irony
;;   :config
;;   (progn
;;     ;; If irony server was never installed, install it.
;;     (unless (irony--find-server-executable) (call-interactively #'irony-install-server))

;;     (add-hook 'c++-mode-hook 'irony-mode)
;;     (add-hook 'c-mode-hook 'irony-mode)

;;     ;; Use compilation database first, clang_complete as fallback.
;;     (setq-default irony-cdb-compilation-databases '(irony-cdb-libclang
;;                                                     irony-cdb-clang-complete))

;;     (add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
;;     ))

;; I use irony with company to get code completion.
(use-package company-irony
  ;; :require company irony
  :config
  (progn
    (eval-after-load 'company '(add-to-list 'company-backends 'company-irony))))

;; I use irony with flycheck to get real-time syntax checking.
(use-package flycheck-irony
  ;; :require flycheck irony
  :config
  (progn
    (eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))))

;; Eldoc shows argument list of the function you are currently writing in the echo area.
(use-package irony-eldoc
  ;; :require eldoc irony
  :config
  (progn
    (add-hook 'irony-mode-hook #'irony-eldoc)))


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
