;; -------------------- Theme setup --------------------

(use-package sourcerer-theme
  :init
  (load-theme 'sourcerer t))

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(use-package powerline
  :init
  (powerline-center-theme)
  (setq powerline-default-separator 'contour))


(use-package ace-window
  :init
  (global-set-key (kbd "M-œ") 'ace-window))

(use-package ace-jump-mode
  :init
  (global-set-key (kbd "C-<") 'ace-jump-mode))

(use-package buffer-move
  :init
  (global-set-key (kbd "C-§") 'buf-move))

(use-package undo-tree
  :init
  (global-undo-tree-mode t))

;; -------------------- SET GLOBAL KEYBINDINGS --------------------
(global-set-key (kbd "C-x :") 'eshell)
(global-set-key (kbd "C-x !" ) 'next-error)


(setq gc-cons-threshold 100000000)
(setq inhibit-startup-message t)

(use-package beacon
  :init
  (beacon-mode t))

(setq-default x-stretch-cursor t)
(setq-default cursor-type 'hbar)
(set-cursor-color "#11ffAA")

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook
          (lambda () (interactive)
            (setq show-trailing-whitespace 1)))

;; use space to indent by default
(setq-default indent-tabs-mode nil)

;; set appearance of a tab that is represented by 2 spaces
(setq-default tab-width 2)

(use-package zygospore
  :bind (("C-x 1" . zygospore-toggle-delete-other-windows)
         ("RET" .   newline-and-indent)))

(provide 'setup-general)
