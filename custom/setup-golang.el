;;; Package --- summary

;;; Commentary:
;;   ____
;;  / ___| ___
;; | |  _ / _ \
;; | |_| | (_) |
;;  \____|\___/
;;; Code:

;; (use-package exec-path-from-shell :demand t)

;; (defun set-exec-path-from-shell-PATH ()
;;   "Bla."
;;   (let ((path-from-shell (replace-regexp-in-string
;;                           "[ \t\n]*$"
;;                           ""
;;                           (shell-command-to-string "$SHELL --login -i -c 'echo $PATH'"))))
;;     (setenv "PATH" path-from-shell)
;;     (setq eshell-path-env path-from-shell) ; for eshell users
;;     (setq exec-path (split-string path-from-shell path-separator))))

;; (when window-system (set-exec-path-from-shell-PATH))

;; (setenv "GOPATH" "/home/brignone/dev/gocode")
;; (add-to-list 'exec-path "/home/brignone/dev/gocode/bin")
;; ;; (add-hook 'before-save-hook 'gofmt-before-save)

;; (use-package flycheck-golangci-lint
;;   :demand t
;;   :ensure t
;;   :hook (go-mode . flycheck-golangci-lint-setup))

(defcustom gobuild-command (purecopy "go build -v && go test -v && go vet")
  "Go build command."
  :type 'string
  :group 'compilation)

(defun go-compile (command &optional comint)
  "Compile the program including the current buffer.  Default: run `make'.
Runs COMMAND, a shell command, in a separate process asynchronously
with output going to the buffer `*compilation*'.

You can then use the command \\[next-error] to find the next error message
and move to the source code that caused it.

If optional second arg COMINT is t the buffer will be in Comint mode with
`compilation-shell-minor-mode'.

Interactively, prompts for the command if the variable
`compilation-read-command' is non-nil; otherwise uses `compile-command'.
With prefix arg, always prompts.
Additionally, with universal prefix arg, compilation buffer will be in
comint mode, i.e. interactive.

To run more than one compilation at once, start one then rename
the `*compilation*' buffer to some other name with
\\[rename-buffer].  Then _switch buffers_ and start the new compilation.
It will create a new `*compilation*' buffer.

On most systems, termination of the main compilation process
kills its subprocesses.

The name used for the buffer is actually whatever is returned by
the function in `compilation-buffer-name-function', so you can set that
to a function that generates a unique name."
  (interactive
   (list
    (let ((command (eval gobuild-command)))
      (if (or compilation-read-command current-prefix-arg)
	        (compilation-read-command command)
	      command))
    (consp current-prefix-arg)))
  (unless (equal command (eval gobuild-command))
    (setq gobuild-command command))
  (save-some-buffers (not compilation-ask-about-save)
                     compilation-save-buffers-predicate)
  (setq-default compilation-directory default-directory)
  (compilation-start command comint))


(use-package go-mode
  :demand t
  :config
  (define-key go-mode-map (kbd "C-x RET RET") 'go-compile))

;; ;; (defun auto-complete-for-go ()
;; ;;   (auto-complete-mode 1))
;; ;; (add-hook 'go-mode-hook 'auto-complete-for-go)

;; (use-package company-go
;;   :demand t
;;   :config
;;   (setq company-go-gocode-command (concat (getenv "HOME") "/go/bin/gocode"))
;;   (add-hook 'go-mode-hook
;;             (lambda ()
;;               (set (make-local-variable 'company-backends) '(company-go))
;;               (company-mode)))
;;   )


;; ;; (use-package go-autocomplete)
;; ;; (defun auto-complete-for-go ()
;; ;; (auto-complete-mode 1))
;; ;;  (add-hook 'go-mode-hook 'auto-complete-for-go)

;; (add-hook 'go-mode-hook 'flycheck-mode)

;; (defun gofmt-before-save ()
;;   "Add this to .emacs to run gofmt on the current buffer when saving:
;; \(add-hook 'before-save-hook 'gofmt-before-save).

;; Note that this will cause ‘go-mode’ to get loaded the first time
;; you save any file, kind of defeating the point of autoloading."

;;   (interactive)
;;   (when (eq major-mode 'go-mode)
;;     (progn (let ((gofmt-command "gofmt")) (gofmt))
;;            (let ((gofmt-command "goimports")) (gofmt)))
;;     ))

;; (defun my-go-mode-hook ()
;;   "Call Gofmt before saving."
;;   (add-hook 'before-save-hook 'gofmt-before-save)
;;                                         ; Godef jump key binding
;;   (local-set-key (kbd "M-.") 'godef-jump)
;;   (local-set-key (kbd "M-*") 'pop-tag-mark)
;;   )
;; (add-hook 'go-mode-hook 'my-go-mode-hook)

;; (setq godef-command "/home/brignone/go/bin/godef")

(add-hook 'go-mode-hook #'lsp-deferred)

;; Set up before-save hooks to format buffer and add/delete imports.
;; Make sure you don't have other gofmt/goimports hooks enabled.
(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(setq lsp-go-use-gofumpt t)

(defface go-template-arg-face
  '((t :foreground "#02c7c7"))
  "Face to display on {{...}}."
  :group 'font-lock-faces)

(font-lock-add-keywords
 'go-mode
 '(("{{\\([^}]*\\)}}" 1 'go-template-arg-face prepend)
   ("{{[^}]*\\<\\(range\\|end\\|if\\|else\\|with\\|template\\|block\\|break\\|continue\\)\\>[^}]*}}" 1 'font-lock-keyword-face prepend)
   ("{{[^}]*\\(\\.\\<[^} ]*\\>\\)[^}]*}}" 1 'font-lock-constant-face prepend)
   ))

(provide 'setup-golang)
;;; setup-golang.el ends here
