;;; Package --- summary

;;; Commentary:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  _       _ _         _  ;;
;; (_)_ __ (_) |_   ___| | ;;
;; | | '_ \| | __| / _ \ | ;;
;; | | | | | | |_ |  __/ | ;;
;; |_|_| |_|_|\__(_)___|_| ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

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

(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t) ; Org-mode's repository<>>>
(add-to-list 'package-archives '("elpa" . "http://elpa.gnu.org/packages/") t)


(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)
;; (use-package req-package)

(add-to-list 'load-path "~/.emacs.d/custom")
;; (add-to-list 'load-path "~/.emacs.d/libs")
(add-to-list 'load-path "~/.emacs.d/lisp")
;; (add-to-list 'load-path "~/.emacs.d/lisp/LoLA")

;; (require 'util)
;; Set the path in case the instance is a daemon
;; (setenv "PATH" (gk/get-command-output "bash -c \"echo $PATH\""))

(setq custom-file "~/.emacs.d/custom.el")

;; -------------------- REQUIRES --------------------

(require 'setup-general)
(require 'setup-editing)
(require 'setup-helm)
(require 'setup-evil)
(require 'setup-god-mode)
(require 'setup-c)
;; (require 'setup-ocaml)
;; (require 'setup-python)
;; (require 'setup-lua)
;; (require 'setup-org-new)
;; (require 'setup-auctex)
;; (require 'setup-debug)
;; (require 'setup-eshell)

;;; init.el ends here
