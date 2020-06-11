(use-package tuareg
  :ensure t)

(use-package utop
  :ensure t
  :config
  (autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
  (add-hook 'tuareg-mode-hook 'utop-minor-mode)
  )

(push (concat (getenv "HOME") "/.opam/system/share/emacs/site-lisp") load-path) ; directory containing merlin.el
;; (setq merlin-command (concat (getenv "HOME") "/.opam/system/bin/ocamlmerlin"))  ; needed only if ocamlmerlin not already in your PATH
(setq merlin-command 'opam)
(autoload 'merlin-mode "merlin" "Merlin mode" t)
(add-hook 'tuareg-mode-hook 'merlin-mode)
(add-hook 'caml-mode-hook 'merlin-mode)

(use-package flycheck-ocaml
  :ensure t
  :config

(with-eval-after-load 'merlin
  ;; Disable Merlin's own error checking
  (setq merlin-error-after-save nil)

  ;; Enable Flycheck checker
  (flycheck-ocaml-setup)))

; Make company aware of merlin
(with-eval-after-load 'company
 (add-to-list 'company-backends 'merlin-company-backend))
; Enable company on merlin managed buffers
(add-hook 'merlin-mode-hook 'company-mode)
; Or enable it globally:
                                        ; (add-hook 'after-init-hook 'global-company-mode)

(add-hook 'tuareg-mode-hook 'flycheck-mode)

(defun merlin-switch-to-ml (name)
  "Switch to the ML file corresponding to the module NAME (fallback to MLI if no ML is provided)."
  (interactive (list (completing-read "Module: "
                                          (merlin-switch-list-by-ext '(".ml" ".mli")))))
  (merlin-switch-to name '(".ml" ".mli")))

(defun merlin-switch-to-mli (name)
  "Switch to the MLI file corresponding to the module NAME (fallback to ML if no MLI is provided)."
  (interactive (list (completing-read "Module: "
                                          (merlin-switch-list-by-ext '(".mli" ".ml")))))
  (merlin-switch-to name '(".mli" ".ml")))

(add-to-list 'load-path "/home/brignone/.opam/4.03.0/share/emacs/site-lisp")
(require 'ocp-indent)

(define-key tuareg-mode-map (kbd "C-c C-c") 'desperately-compile)


;; (let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var" "share")))))
;;       (when (and opam-share (file-directory-p opam-share))
;;        ;; Register Merlin
;;        (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
;;        (autoload 'merlin-mode "merlin" nil t nil)
;;        ;; Automatically start it in OCaml buffers
;;        (add-hook 'tuareg-mode-hook 'merlin-mode t)
;;        (add-hook 'caml-mode-hook 'merlin-mode t)
;;        ;; Use opam switch to lookup ocamlmerlin binary
;;        (setq merlin-command 'opam)))

(defun utop-tuareg-next-phrase ()
  "Move to the next phrase after point."
  (tuareg--skip-double-semicolon))

(defun utop-eval-phrase ()
  "Eval the surrounding Caml phrase (or block) in utop."
  (interactive)
  (utop-prepare-for-eval)
  (let ((end))
    (save-excursion
      (let ((triple (funcall utop-discover-phrase)))
        (setq end (cdr triple))
        (utop-eval (car triple) (cdr triple))))
    (when utop-skip-after-eval-phrase
      (goto-char (+ 1 end))
      (funcall utop-next-phrase-beginning))))


(provide 'setup-ocaml)
