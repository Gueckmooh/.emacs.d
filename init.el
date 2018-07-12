(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t) ; Org-mode's repository<>>>
(add-to-list 'package-archives '("elpa" . "http://elpa.gnu.org/packages/") t)


(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(require 'use-package)
(setq use-package-always-ensure t)

(add-to-list 'load-path "~/.emacs.d/custom")
(add-to-list 'load-path "~/.emacs.d/libs")

;; -------------------- REQUIRES --------------------

(require 'setup-general)
(require 'setup-helm)
(require 'setup-vhdl)
(require 'setup-magit)
(require 'setup-autopair)
(require 'setup-c)
(require 'setup-format)
(require 'setup-compile)
(require 'setup-lua)
(require 'setup-org)
(require 'setup-python)
(require 'setup-debug)
(require 'setup-latex)
(require 'setup-company)
(require 'setup-editing)
(require 'setup-minimap)
(require 'setup-bison)
(require 'setup-w3m)
(require 'setup-ggtags)
(require 'setup-narrow)

;; additional libs
;; (require 'zones)
;; (require 'narrow-indirect)
;; Uncomment the following line to get hardcore mode
;; (require 'setup-hardcore)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-directory-alist (quote (("" . "~/.emacs_backups"))))
 '(custom-safe-themes
   (quote
    ("12dd37432bb454355047c967db886769a6c60e638839405dad603176e2da366b" default)))
 '(package-selected-packages
   (quote
    (multiple-cursors ansi helm-projectile projectile csv-mode xkcd sublimity minimap eshell-z eshell-up eshell-prompt-extras eshell-git-prompt eshell-fringe-status eshell-did-you-mean ess company-jedi smart-compile yasnippet-snippets tex auctex hardcore-mode anaconda-mode virtualenvwrapper elpy company-c-headers company-c-header company ob-async ob ac-c-headers auto-complete function-args lua-mode move-text auto-highlight-symbol autopair magit undo-tree buffer-move ace-jump-mode ace-jump ace-window electric-spacing vhdl-tools helm-swoop helm zygospore beacon powerline sourcerer-theme flycheck use-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(minimap-active-region-background ((t (:background "#2b2b2b")))))
(put 'narrow-to-region 'disabled nil)
