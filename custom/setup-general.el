;;; Package --- summary

;;; Commentary:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                                  _  ;;
;;   __ _  ___ _ __   ___ _ __ __ _| | ;;
;;  / _` |/ _ \ '_ \ / _ \ '__/ _` | | ;;
;; | (_| |  __/ | | |  __/ | | (_| | | ;;
;;  \__, |\___|_| |_|\___|_|  \__,_|_| ;;
;;  |___/                              ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

;; -------------------- Theme setup --------------------

;; Liked themes:
;; sourcerer-theme
;; flatui-theme
;; nimbus-theme
;; material-theme
;; dracula-theme
;; jetbrains-darcula-theme
;; gruvbox-theme

(use-package jetbrains-darcula-theme
  :demand t
  :ensure t
  :config
  (load-theme 'jetbrains-darcula t))

(use-package nyan-mode
  :ensure t
  :demand t
  :config
  (setq nyan-wavy-trail t)
  (setq nyan-animate-nyancat t)
  (nyan-mode))

;; (set-default-font "-*-Hack-normal-normal-normal-*-13-*-*-*-m-0-iso10646-1")
;; font size to 14 (for hidpi screen)
;; (set-face-attribute 'default nil :height 131)

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(setq scroll-step 1)                    ;; Previously 0
(setq scroll-margin 7)                  ;; Previously 0

(setq backup-directory-alist '(("" . "~/.emacs_backups")))

(use-package anzu
  :demand t
  :config
  (global-anzu-mode)
  (global-set-key (kbd "M-%") 'anzu-query-replace)
  (global-set-key (kbd "M-ù") 'anzu-query-replace-regexp)
  (setq anzu-cons-mode-line-p nil)
  (set-face-attribute 'anzu-mode-line nil
                      :background "dark magenta" :foreground "white"))

(global-set-key (kbd "C-c C-g") 'revert-buffer)

(global-hl-line-mode 1)

;; (face-attribute mode-line :background)
;; (defface mypowerline-active2 '((t (:inherit mode-line)))

(use-package fill-column-indicator
  :commands (company-turn-off-fci company-maybe-turn-on-fci)
  :init
  (add-hook 'company-completion-started-hook 'company-turn-off-fci)
  (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
  (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)
  :config
  (defvar-local company-fci-mode-on-p nil)

  (defun company-turn-off-fci (&rest ignore)
    (when (boundp 'fci-mode)
      (setq company-fci-mode-on-p fci-mode)
      (when fci-mode (fci-mode -1))))

  (defun company-maybe-turn-on-fci (&rest ignore)
    (when company-fci-mode-on-p (fci-mode 1)))

  ;; To get the highlight background color
  (defvar-local highlight-overlay nil
    "Overlay used by Hl-Line mode to highlight the current line.")

  (defvar-local global-highlight-overlay nil
    "Overlay used by Global-Hl-Line mode to highlight the current line.")

  (defcustom highlight-face 'highlight
    "Face with which to highlight the current line in Highlight mode."
    :type 'face
    :group 'highlight
    :set (lambda (symbol value)
           (set symbol value)
           (dolist (buffer (buffer-list))
             (with-current-buffer buffer
               (when highlight-overlay
                 (overlay-put highlight-overlay 'face highlight-face))))
           (when global-highlight-overlay
             (overlay-put global-highlight-overlay 'face highlight-face))))


  (defun activate-fci ()
    (when (not (daemonp))
      (setq fci-rule-color (face-attribute highlight-face :background)))
    (setq fci-rule-width 3)
    (setq fill-column 80)
    (fci-mode 1))

  ;; (with-eval-after-load 'company (add-hook 'prog-mode-hook 'activate-fci))
  )


;; Movements:
(use-package ace-window
  :ensure t
  :bind
  ("M-²" . ace-window))

(use-package ace-jump-mode
  :ensure t
  :bind
  ("C-c <" . ace-jump-char-mode)
  ("C-c SPC" . ace-jump-line-mode))

(use-package ace-jump-buffer
  :ensure t
  :bind
  ("C-c j" . ace-jump-buffer))

(use-package buffer-move
  :ensure t
  :bind
  ("<C-S-up>" . buf-move-up)
  ("<C-S-down>" . 'buf-move-down)
  ("<C-S-left>" . buf-move-left)
  ("<C-S-right>" . buf-move-right)
  ("C-§" . buf-move))

(use-package undo-tree
  :ensure t
  :demand t
  :config
  (global-undo-tree-mode t))

(use-package volatile-highlights
  :demand t
  :config
  (volatile-highlights-mode t))

;; -------------------- SET GLOBAL KEYBINDINGS --------------------
(global-set-key (kbd "C-x :") 'eshell)
(global-set-key (kbd "C-x !" ) 'next-error)

(setq gc-cons-threshold 100000000)
(setq inhibit-startup-message t)

(use-package beacon
  :demand t
  :ensure t
  :config
  (beacon-mode t))

(setq-default x-stretch-cursor t)
;;(setq-default cursor-type 'hbar)
;;(set-cursor-color "#11ffAA")

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook
          (lambda () (interactive)
            (setq show-trailing-whitespace 1)))

;; (use-package whitespace
;;   :ensure t
;;   :config
;;   (add-hook 'prog-mode-hook 'whitespace-mode)
;;   (setq whitespace-style '(face ;; empty tabs
;;                            lines-tail ;; trailing
;;                            )))

;; use space to indent by default
(setq-default indent-tabs-mode nil)

;; set appearance of a tab that is represented by 2 spaces
(setq-default tab-width 2)

(use-package zygospore
  :demand t
  :ensure t
  :bind (("C-x 1" . zygospore-toggle-delete-other-windows)
         ("RET" .   newline-and-indent)))

(display-time-mode t)

(define-key key-translation-map [?\C-h] [?\C-?])
(global-set-key (kbd "<f1>") 'help-command)

(use-package xkcd
  :commands xkcd)

(defun display-ansi-colors ()
  "Displays ansi colors in buffer."
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

;; Yes or No -> y or n
(defalias 'yes-or-no-p 'y-or-n-p)

(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(defun launch-terminal (&optional term)
  "Launches a terminal at the buffer's location.
  TERM is the name of the terminal to launch."
  (interactive)
  (let ((terminal "termite"))
    (call-process-shell-command (concat terminal "&") nil 0)))

(global-set-key (kbd "C-x C-:") 'launch-terminal)

(defun create-scratch-buffer nil
  "Create a scratch buffer."
  (interactive)
  (let ((scratch-not-exists (eq nil (get-buffer "*scratch*"))))
    (switch-to-buffer (get-buffer-create "*scratch*"))
    (lisp-interaction-mode)
    (when scratch-not-exists
      (with-current-buffer "*scratch*"
        (insert ";; This buffer is for text that is not saved, and for Lisp evaluation.\n"))
  (with-current-buffer "*scratch*"
    (insert ";; To create a file, visit it with <open> and enter text in its buffer.\n\n"))))
  )

(global-set-key (kbd "C-x ,") 'create-scratch-buffer)

(global-set-key (kbd "C-x C-k c") 'kmacro-call-macro)

(if (version< "25.1" emacs-version)
    (use-package magit
      :ensure t
      :bind ("C-c g" . magit-status))
  )

(defun edit-config ()
  "Open the config directory in dired."
  (interactive)
  (dired "~/.emacs.d"))

;; define function to shutdown emacs server instance
(defun server-shutdown ()
  "Save buffers, Quit, and Shutdown (kill) server."
  (interactive)
  (save-some-buffers)
  (kill-emacs)
  )

(use-package ah
  :demand t                             ; TODO
  :config
  (ah-mode 1))

(setq company-minimum-prefix-length 1
      company-idle-delay 0.0) ;; default is 0.2

(setq history-delete-duplicates t)

(use-package which-key
    :config
    (which-key-mode))


(provide 'setup-general)
;;; setup-general.el ends here
