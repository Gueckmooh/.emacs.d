;;; Package --- summary

;;; Commentary:
;;   ____
;;  / ___|
;; | |
;; | |___
;;  \____|
;;; Code:

;; (add-hook 'c++-mode-hook (lambda () (c-set-offset 'innamespace 0)))

(c-add-style "my-c-style"
             '((c-basic-offset . 4)     ; Guessed value
               (c-offsets-alist
                (access-label . /)      ; Guessed value
                (arglist-cont . 0)      ; Guessed value
                (arglist-intro . +)     ; Guessed value
                (block-close . 0)       ; Guessed value
                (case-label . 0)        ; Guessed value
                (class-close . 0)       ; Guessed value
                (defun-block-intro . +) ; Guessed value
                (defun-close . 0)       ; Guessed value
                (inclass . +)           ; Guessed value
                (inline-close . 0)      ; Guessed value
                (innamespace . 0)       ; Guessed value
                (member-init-intro . +) ; Guessed value
                (namespace-close . 0)   ; Guessed value
                (statement . 0)         ; Guessed value
                (statement-block-intro . +) ; Guessed value
                (statement-case-intro . +) ; Guessed value
                (statement-cont . +)       ; Guessed value
                (topmost-intro . 0)        ; Guessed value
                (annotation-top-cont . 0)
                (annotation-var-cont . +)
                (arglist-close . c-lineup-close-paren)
                (arglist-cont-nonempty . c-lineup-arglist)
                (block-open . 0)
                (brace-entry-open . 0)
                (brace-list-close . 0)
                (brace-list-entry . 0)
                (brace-list-intro . +)
                (brace-list-open . +)
                (c . c-lineup-C-comments)
                (catch-clause . 0)
                (class-open . 0)
                (comment-intro . c-lineup-comment)
                (composition-close . 0)
                (composition-open . 0)
                (cpp-define-intro c-lineup-cpp-define +)
                (cpp-macro . -1000)
                (cpp-macro-cont . +)
                (defun-open . 0)
                (do-while-closure . 0)
                (else-clause . 0)
                (extern-lang-close . 0)
                (extern-lang-open . 0)
                (friend . 0)
                (func-decl-cont . +)
                (incomposition . +)
                (inexpr-class . +)
                (inexpr-statement . +)
                (inextern-lang . +)
                (inher-cont . c-lineup-multi-inher)
                (inher-intro . +)
                (inlambda . 0)
                (inline-open . +)
                (inmodule . +)
                (knr-argdecl . 0)
                (knr-argdecl-intro . +)
                (label . 2)
                (lambda-intro-cont . +)
                (member-init-cont . c-lineup-multi-inher)
                (module-close . 0)
                (module-open . 0)
                (namespace-open . 0)
                (objc-method-args-cont . c-lineup-ObjC-method-args)
                (objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +)
                (objc-method-intro .
                                   [0])
                (statement-case-open . 0)
                (stream-op . c-lineup-streamop)
                (string . -1000)
                (substatement . +)
                (substatement-label . 2)
                (substatement-open . 0)
                (template-args-cont c-lineup-template-args +)
                (topmost-intro-cont . c-lineup-topmost-intro-cont))))



(defun my-c-mode-hook ()
    (c-set-style "my-c-style")
    ;; (c-set-offset 'substatement-open '0) ; brackets should be at same indentation level as the statements they open
    ;; (c-set-offset 'inline-open '+)
    ;; (c-set-offset 'block-open '0)
    ;; (c-set-offset 'brace-list-open '+)   ; all "opens" should be indented by the c-indent-level
    ;; (c-set-offset 'case-label '0)
    ;; (c-set-offset 'innamespace 0)
    )

(add-hook 'c++-mode-hook 'my-c-mode-hook)

;;       _
;;  _ __| |_ __ _  __ _ ___
;; | '__| __/ _` |/ _` / __|
;; | |  | || (_| | (_| \__ \
;; |_|   \__\__,_|\__, |___/
;;                |___/

;; (use-package rtags
;;   :defer t
;;   :commands rtags-start-process-unless-running
;;   :init
;;   (add-hook 'c-mode-hook 'rtags-start-process-unless-running)
;;   (add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
;;   (add-hook 'objc-mode-hook 'rtags-start-process-unless-running)
;;   :config
;;   (unless (rtags-executable-find "rc") (error "Binary rc is not installed!"))
;;   (unless (rtags-executable-find "rdm") (error "Binary rdm is not installed!"))

;;   (define-key c-mode-base-map (kbd "M-.") 'rtags-find-symbol-at-point)
;;   (define-key c-mode-base-map (kbd "M-,") 'rtags-find-references-at-point)
;;   (define-key c-mode-base-map (kbd "M-?") 'rtags-display-summary)
;;   (rtags-enable-standard-keybindings)

;;   (add-hook 'kill-emacs-hook 'rtags-quit-rdm)
;;   )

;; ;; TODO: Has no coloring! How can I get coloring?
;; (use-package helm-rtags
;;   :ensure helm
;;   :after rtags
;;   :commands rtags-helm-setup
;;   :init
;;   (add-hook 'c-mode-hook 'rtags-helm-setup)
;;   (add-hook 'c++-mode-hook 'rtags-helm-setup)
;;   (add-hook 'objc-mode-hook 'rtags-helm-setup)
;;   :config
;;   (defun rtags-helm-setup ()
;;     (setq rtags-display-result-backend 'helm))
;;   )

;; ;; Use rtags for auto-completion.
;; (use-package company-rtags
;;   :ensure company
;;   :after (company rtags)
;;   :defer t
;;   :commands rtags-company-setup
;;   :init
;;   (add-hook 'c-mode-hook 'rtags-company-setup)
;;   (add-hook 'c++-mode-hook 'rtags-company-setup)
;;   (add-hook 'objc-mode-hook 'rtags-company-setup)
;;   :config
;;   (defun rtags-company-setup ()
;;     (setq rtags-autostart-diagnostics t)
;;     (rtags-diagnostics)
;;     (setq rtags-completions-enabled t)
;;     )
;;   (push 'company-rtags company-backends)
;;   )

(use-package company-c-headers
  :after company
  :hook ((c-mode . company-c-headers-setup)
         (c++-mode . company-c-headers-setup))
  :config
  (defun company-c-headers-setup ())
  (eval-after-load 'company '(add-to-list 'company-backends 'company-c-headers))
  )

;; (use-package flycheck-rtags
;;   :ensure flycheck
;;   :after flycheck
;;   :commands my-flycheck-rtags-setup
;;   :init
;;   (add-hook 'c-mode-hook #'my-flycheck-rtags-setup)
;;   (add-hook 'c++-mode-hook #'my-flycheck-rtags-setup)
;;   (add-hook 'objc-mode-hook #'my-flycheck-rtags-setup)
;;   :config
;;   (setq rtags-autostart-diagnostics t)
;;   (defun my-flycheck-rtags-setup ()
;;     (flycheck-select-checker 'rtags)
;;     (setq-local flycheck-highlighting-mode nil) ;; RTags creates more accurate overlays.
;;     (setq-local flycheck-check-syntax-automatically nil)))


(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode))

(use-package ccls)

(use-package clang-format+
  :hook ((c-mode . clang-format+-mode)
         (c++-mode . clang-format+-mode))
  )

(provide 'setup-c)
;;; setup-c.el ends here
