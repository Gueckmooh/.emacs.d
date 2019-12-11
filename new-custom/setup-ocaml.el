;;; Package --- summary

;;; Commentary:
;;   ___   ____                _
;;  / _ \ / ___|__ _ _ __ ___ | |
;; | | | | |   / _` | '_ ` _ \| |
;; | |_| | |__| (_| | | | | | | |
;;  \___/ \____\__,_|_| |_| |_|_|
;;
;;; Code:

(require 'util)
(use-package tuareg
  :ensure t)

(use-package utop
  :ensure t
  :init
  (defvar utop-ocaml-preprocessor nil)
  :config
  (autoload 'utop-minor-mode "utop" "Minor mode for utop" t)
  (add-hook 'tuareg-mode-hook 'utop-minor-mode)
  )

(push (concat (getenv "HOME") "/.opam/system/share/emacs/site-lisp") load-path) ; directory containing merlin.el
;; (setq merlin-command (concat (getenv "HOME") "/.opam/system/bin/ocamlmerlin"))  ; needed only if ocamlmerlin not already in your PATH
(autoload 'merlin-mode "merlin" "Merlin mode" t)
(setq merlin-command 'opam)
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

(setq ocaml-home (concat (getenv "HOME") "/.opam/"
                         (gk/get-command-output "opam switch show")))
(add-to-list 'load-path (concat ocaml-home "/share/emacs/site-lisp"))
(if (executable-find "ocp-indent")
    (require 'ocp-indent))

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

(defun make-ocaml-scratch ()
  "Create a scratch file in /tmp/ocaml-scratch and opens it."
  (interactive)
  (find-file "/tmp/ocaml-scratch/scratch.ml")
  (if (equal 0 (buffer-size))
      (progn
        (insert "open Printf\n\nlet _ = printf \"Hello, World!\\n\";;")
        (make-directory "/tmp/ocaml-scratch" t))))

(use-package dune)

(add-to-list 'auto-mode-alist '("\\.mll\\'" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\.cinaps\\'" . tuareg-mode))
(add-to-list 'auto-mode-alist '("\\dune-project\\'" . dune-mode))

(provide 'setup-ocaml)
;;; setup-ocaml.el ends here
