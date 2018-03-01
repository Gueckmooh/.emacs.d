(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(add-to-list 'load-path "~/.emacs.d/custom")

;; -------------------- REQUIRES --------------------

(require 'setup-general)
(require 'setup-helm)
(require 'setup-vhdl)
(require 'setup-magit)
(require 'setup-autopair)
(require 'setup-editing)
(require 'setup-c)
(require 'setup-format)
(require 'setup-compile)
;; Uncomment this to get
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  _   _    _    ____  ____   ____ ___  ____  _____  ;;
;; | | | |  / \  |  _ \|  _ \ / ___/ _ \|  _ \| ____| ;;
;; | |_| | / _ \ | |_) | | | | |  | | | | |_) |  _|   ;;
;; |  _  |/ ___ \|  _ <| |_| | |__| |_| |  _ <| |___  ;;
;; |_| |_/_/   \_\_| \_\____/ \____\___/|_| \_\_____| ;;
;;                                                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; mode
;;(require 'setup-hardcore)
;;(require 'setup-key-bindings)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-directory-alist (quote (("" . "~/.emacs_backups"))))
 '(package-selected-packages
   (quote
    (fill-column-indicator move-text auto-highlight-symbol autopair magit undo-tree buffer-move ace-jump-mode ace-jump ace-window electric-spacing vhdl-tools helm-projectile helm-swoop helm zygospore beacon powerline sourcerer-theme flycheck use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
