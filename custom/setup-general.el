;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  _  ;;
;;   __ _  ___ _ __   ___ _ __ __ _| | ;;
;;  / _` |/ _ \ '_ \ / _ \ '__/ _` | | ;;
;; | (_| |  __/ | | |  __/ | | (_| | | ;;
;;  \__, |\___|_| |_|\___|_|  \__,_|_| ;;
;;  |___/                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; -------------------- Theme setup --------------------

(use-package sourcerer-theme
  :init
  (if window-system
      (load-theme 'sourcerer t)
    (load-theme 'wombat t))
  )

;; (require 'firebelly-theme)
;;(load-theme 'firebelly t)


(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(use-package powerline
  :init
  ;; (if window-system
  ;;     (powerline-default-theme)
  ;;   ())
  (powerline-default-theme)
  (setq powerline-default-separator 'contour))

(if window-system
    (global-hl-line-mode 1)
  ())

(if window-system
    (set-face-attribute hl-line-face nil :underline t)
  ())

(use-package ace-window
  :init
  (global-set-key (kbd "M-œ") 'ace-window))

(use-package ace-jump-mode
  :init
  (global-set-key (kbd "C-c <") 'ace-jump-mode))

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
;;(setq-default cursor-type 'hbar)
;;(set-cursor-color "#11ffAA")

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook
          (lambda () (interactive)
            (setq show-trailing-whitespace 1)))

(use-package whitespace
  :init
  (setq whitespace-style '(face ;; empty tabs
                                lines-tail ;; trailing
                                ))
  ;; (global-whitespace-mode t)
  (add-hook 'prog-mode-hook
            (lambda () (interactive)
              (whitespace-mode 1))))

;; use space to indent by default
(setq-default indent-tabs-mode nil)

;; set appearance of a tab that is represented by 2 spaces
(setq-default tab-width 2)

(use-package zygospore
  :bind (("C-x 1" . zygospore-toggle-delete-other-windows)
         ("RET" .   newline-and-indent)))

(display-time-mode t)

(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "<f1>") 'help-command)

(use-package xkcd)

(defun display-ansi-colors ()
  (interactive)
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region (point-min) (point-max))))

(provide 'setup-general)
