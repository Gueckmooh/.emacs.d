;;; Package --- summary

;;; Commentary:
;;  _
;; | |___ _ __
;; | / __| '_ \
;; | \__ \ |_) |
;; |_|___/ .__/
;;       |_|
;;; Code:

(use-package lsp-mode
  :demand t
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-keymap-prefix "C-c l")
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (c++-mode . lsp)
         (c-mode . lsp)
         (go-mode . lsp-deferred)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp
  :config
  (custom-set-faces
   '(lsp-face-highlight-textual
     ((t (:inherit ahs-face))))
   '(lsp-face-highlight-read
     ((t (:inherit ahs-definition-face)))))
  (progn
    (lsp-register-client
     (make-lsp-client :new-connection (lsp-tramp-connection "ccls")
                      :major-modes '(c-mode c++-mode)
                      :remote? t
                      :server-id 'ccls-remote))))

;; optionally
(use-package lsp-ui :commands lsp-ui-mode
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  )
;; if you are helm user
(use-package helm-lsp :commands helm-lsp-workspace-symbol
  :config
  (define-key lsp-mode-map [remap xref-find-apropos] #'helm-lsp-workspace-symbol)
)
;; if you are ivy user
;; (use-package lsp-ivy :commands lsp-ivy-workspace-symbol)
;; (use-package lsp-treemacs :commands lsp-treemacs-errors-list)

;; optionally if you want to use debugger
(use-package dap-mode)
;; (use-package dap-LANGUAGE) to load the dap adapter for your language
(require 'company)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      company-minimum-prefix-length 1
      lsp-lens-enable nil               ;; makes emacs too slow
      lsp-lens-place-position 'end-of-line
      lsp-signature-auto-activate nil)


(provide 'setup-lsp)
;;; setup-lsp.el ends here
