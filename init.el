(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
;; (add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t) ; Org-mode's repository<>>>
(add-to-list 'package-archives '("elpa" . "http://elpa.gnu.org/packages/") t)


;; (setq package-archives '(
;;                          ("melpa" . "https://melpa.org/packages/")))

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
(use-package req-package)

(add-to-list 'load-path "~/.emacs.d/new-custom")
(add-to-list 'load-path "~/.emacs.d/libs")
(add-to-list 'load-path "~/.emacs.d/lisp")
(add-to-list 'load-path "~/.emacs.d/lisp/LoLA")

(require 'util)
;; Set the path in case the instance is a daemon
(setenv "PATH" (gk/get-command-output "bash -c \"echo $PATH\""))

(setq custom-file "~/.emacs.d/custom.el")

;; -------------------- REQUIRES --------------------

(require 'setup-general)
(require 'setup-editing)
(require 'setup-helm)
(require 'setup-god-mode)
(require 'setup-c)
(require 'setup-ocaml)
(require 'setup-python)
(require 'setup-lua)
;; (require 'setup-org)
(require 'setup-org-new)
(require 'setup-auctex)
(require 'setup-debug)
(require 'setup-mu4e)

;; (require 'util)
;; (require 'setup-general)

;; (require 'setup-vhdl)
;; (require 'setup-magit)
;; (require 'setup-autopair)
;; (require 'setup-yasnippet)
;; (require 'setup-flycheck)
;; (require 'setup-c)
;; (require 'setup-bash)
;; (require 'setup-format)
;; (require 'setup-compile)



;; (require 'setup-debug)
;; (require 'setup-latex)
;; (require 'setup-company)
;; (require 'setup-editing)
;; (require 'setup-minimap)
;; (require 'setup-bison)
;; (require 'setup-w3m)
;; (require 'setup-ggtags)
;; (require 'setup-narrow)
;; (require 'setup-emms)
;; (require 'setup-doxymacs)
;; (require 'setup-java)

;; (require 'setup-awk)
;; (require 'setup-safe-local-variables)
;; (require 'setup-rust)

;; (require 'setup-R)
;; (require 'setup-z3)
;; (require 'setup-scheme)
;; (require 'setup-cadp)
;; (require 'setup-perl)
;; (require 'setup-dot)
;; (require 'setup-mu4e)
;; (require 'setup-nusmv)

;; (require 'lola-mode)

;; additional libs
;; (require 'zones)
;; (require 'narrow-indirect)
;; Uncomment the following line to get hardcore mode
;; (require 'setup-hardcore)

(put 'narrow-to-region 'disabled nil)
(put 'LaTeX-narrow-to-environment 'disabled nil)
