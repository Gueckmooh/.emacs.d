;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  _  ;;
;;   __ _  ___ _ __   ___ _ __ __ _| | ;;
;;  / _` |/ _ \ '_ \ / _ \ '__/ _` | | ;;
;; | (_| |  __/ | | |  __/ | | (_| | | ;;
;;  \__, |\___|_| |_|\___|_|  \__,_|_| ;;
;;  |___/                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'util)

;; -------------------- Theme setup --------------------

;; (use-package sourcerer-theme
;;   :init
;;   (if window-system
;;       (load-theme 'sourcerer t)
;;     (load-theme 'wombat t))
;;   )

;; (use-package flatui-theme
;;   :init
;;   (load-theme 'flatui t)
;;   )

;; (use-package nimbus-theme
;;   :init
;;   (load-theme 'nimbus t)
;;   )

(use-package material-theme
  :init
  (load-theme 'material t)
  )

;; (use-package gruvbox-theme
;;   :init
;;   (load-theme 'gruvbox-dark-medium t)
;;   )

(defun alpha-on-term ()
  (unless (display-graphic-p (selected-frame))
    (set-face-background 'default "unspecified-bg" (selected-frame))))



;; (add-hook 'window-setup-hook 'alpha-on-term)

;; (require 'firebelly-theme)
;; (load-theme 'firebelly t)

;; (use-package klere-theme
;;   :init
;;   (load-theme 'klere t)
;;   )


(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(use-package powerline
  :init
  (powerline-vim-theme)
  (setq powerline-default-separator nil))

(if window-system
    (global-hl-line-mode 1)
  ())

(if window-system
    (set-face-attribute hl-line-face nil :underline t)
  ())

(use-package ace-window
  :init
  (global-set-key (kbd "M-²") 'ace-window))

(use-package ace-jump-mode
  :init
  (global-set-key (kbd "C-c <") 'ace-jump-char-mode)
  (global-set-key (kbd "C-c SPC") 'ace-jump-line-mode))

(use-package ace-jump-buffer
  :init
  (global-set-key (kbd "C-c j") 'ace-jump-buffer))

(use-package buffer-move
  :init
  (global-set-key (kbd "C-§") 'buf-move))

(use-package undo-tree
  :init
  (global-undo-tree-mode t))

;; -------------------- SET GLOBAL KEYBINDINGS --------------------
(global-set-key (kbd "C-x :") 'eshell)
(global-set-key (kbd "C-x !" ) 'next-error)
(global-set-key (kbd "M-µ") 'query-replace-regexp)
(global-set-key (kbd "M-§") 'shell-command-on-region)

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

(winner-mode t)

(xterm-mouse-mode)
(fset 'menu-bar-open nil)
(fset 'x-menu-bar-open nil)

(visual-line-mode t)

(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)

(if (gk/is-installed-p "xclip")
    (use-package xclip
      :init
      (xclip-mode 1)))

;; Yes or No -> y or n
(defalias 'yes-or-no-p 'y-or-n-p)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(defun launch-terminal (&optional term)
  (interactive)
  (let ((terminal "urxvt"))
    (call-process-shell-command (concat terminal "&") nil 0)))

(global-set-key (kbd "C-x C-:") 'launch-terminal)

(defun create-scratch-buffer nil
   "create a scratch buffer"
   (interactive)
   (setq scratch-not-exists (eq nil (get-buffer "*scratch*")))
   (switch-to-buffer (get-buffer-create "*scratch*"))
   (lisp-interaction-mode)
   (when scratch-not-exists
     (with-current-buffer "*scratch*"
       (insert ";; This buffer is for text that is not saved, and for Lisp evaluation.\n"))
     (with-current-buffer "*scratch*"
       (insert ";; To create a file, visit it with <open> and enter text in its buffer.\n\n")))
   )

(global-set-key (kbd "C-x ,") 'create-scratch-buffer)

(use-package crux
  :bind (("C-a" . crux-move-beginning-of-line)))

(global-set-key (kbd "C-x C-k c") 'kmacro-call-macro)

(defun edit-config ()
  "Open the config directory in dired"
  (interactive)
  (dired "~/.emacs.d"))

(provide 'setup-general)
