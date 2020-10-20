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

;; (use-package material-theme
;;   :init
;;   (load-theme 'material t)
;;   )

;; (use-package dracula-theme
;;   :demand t
;;   :ensure t
;;   :init
;;   (load-theme 'dracula t)
;;   )

(use-package jetbrains-darcula-theme
  :demand t
  :ensure t
  :config
  (load-theme 'jetbrains-darcula t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-done ((t (:foreground "#6f7478" :box (:line-width 1 :color "#313335") :weight normal)))))


;; (use-package gruvbox-theme
;;   :init
;;   (load-theme 'gruvbox-dark-medium t)
;;   )

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
  (global-set-key (kbd "C-c C-g") 'revert-buffer)
  (setq anzu-cons-mode-line-p nil)
  (set-face-attribute 'anzu-mode-line nil
                      :background "dark magenta" :foreground "white"))

(use-package powerline
  :ensure t
  :demand t
  :config
  (defface mypowerline-active2 '((t (:inherit mode-line)))
    "Powerline face 3."
    :group 'powerline)

  (defface mypowerline-god-active '((t (:background "dark cyan" :foreground "white" :inherit mode-line)))
    "Powerline face 1."
    :group 'powerline)

  (defface mypowerline-god-inactive '((t (:background "dark red" :foreground "white" :inherit mode-line)))
    "Powerline face 1."
    :group 'powerline)

  (defface mypowerline-god-other '((t (:background "dark slate gray" :foreground "white" :inherit mode-line)))
    "Powerline face 1."
    :group 'powerline)

  (defpowerline powerline-god
    (let ((god-str
           (cond (god-local-mode "GOD     ")
                 ((member major-mode
                          '(dired-mode magit-status-mode Man-mode
                                       magit-diff-mode debugger-mode
                                       apropos-mode package-menu-mode)
                          ) "OTHER   ")
                 (t "INSERT  "))))
      god-str))

  (defface mypowerline-anzu-active
    '((t (:background "dark magenta" :foreground "white" :inherit mode-line)))
    "Powerline face 1."
    :group 'powerline)

  (defface mypowerline-buffedit
    '((t (:inherit mode-line :extend t :background "white" :foreground "black")))
    "Powerline face on buffer name when edited."
    :group 'powerline)

  (defpowerline powerline-anzu
    (let ((god-str (anzu--update-mode-line)))
      god-str))

  (defun my-powerline-buffer-id (&optional face pad)
    (powerline-raw
     (format-mode-line
      (propertize
       (format-mode-line mode-line-buffer-identification)
       'face face
       'mouse-face 'mode-line-highlight
       'help-echo "Buffer name\n\ mouse-1: Previous buffer\n\ mouse-3: Next buffer"
       'local-map (let ((map (make-sparse-keymap)))
                    (define-key map [mode-line mouse-1] 'mode-line-previous-buffer)
                    (define-key map [mode-line mouse-3] 'mode-line-next-buffer)
                    map)))
     face pad))


  (defun powerline-custom-theme ()
    "Setup the default mode-line."
    (interactive)
    (setq-default mode-line-format
                  '((:eval
                     (let* ((active (powerline-selected-window-active))
                            (mode-line-buffer-id (if active 'mode-line-buffer-id
                                                   'mode-line-buffer-id-inactive))
                            (mode-line (if active 'mode-line 'mode-line-inactive))
                            (face0 (if active 'powerline-active0 'powerline-inactive0))
                            (face1 (if active 'powerline-active1 'powerline-inactive1))
                            (face2 (if active 'mypowerline-active2 'powerline-inactive2))
                            (face-buf (if active
                                          (if (buffer-modified-p) 'mypowerline-buffedit 'powerline-active0)
                                        (if (buffer-modified-p) 'powerline-inactive2 'powerline-inactive0)))
                            (face-god
                             (if active
                                 (cond (god-local-mode 'mypowerline-god-active)
                                       ((member major-mode
                                                '(dired-mode magit-status-mode Man-mode
                                                             magit-diff-mode debugger-mode
                                                             apropos-mode package-menu-mode)
                                                ) 'mypowerline-god-other)
                                       (t 'mypowerline-god-inactive))
                               'powerline-inactive1)
                             )
                            (face-anzu (if anzu--state 'mypowerline-anzu-active
                                         (if active 'powerline-active0 'powerline-inactive0)))
                            (separator-left (intern (format "powerline-%s-%s"
                                                            (powerline-current-separator)
                                                            (car powerline-default-separator-dir))))
                            (separator-right (intern (format "powerline-%s-%s"
                                                             (powerline-current-separator)
                                                             (cdr powerline-default-separator-dir))))
                            (lhs (list
                                  (powerline-god face-god 'l)
                                  (funcall separator-left face-god face-anzu)
                                  (when anzu--state (powerline-anzu face-anzu 'l))
                                  (when anzu--state (powerline-raw " " face-anzu))
                                  (when anzu--state (funcall separator-left face-anzu face0))
                                  (powerline-raw "%*" face0 'l)
                                  (when powerline-display-buffer-size
                                    (powerline-buffer-size face0 'l))
                                  (when powerline-display-mule-info
                                    (powerline-raw mode-line-mule-info face0 'l))
                                  (funcall separator-left face0 face-buf)
                                  (my-powerline-buffer-id `(,face-buf ,mode-line-buffer-id) 'l)
                                  (powerline-raw " " face-buf)
                                  (funcall separator-left face-buf face1)
                                  (when (and (boundp 'erc-track-minor-mode) erc-track-minor-mode)
                                    (powerline-raw erc-modified-channels-object face1 'l))
                                  (powerline-major-mode face1 'l)
                                  (powerline-process face1)
                                  ;; (powerline-minor-modes face1 'l)
                                  (powerline-narrow face1 'l)
                                  (powerline-raw " " face1)
                                  (funcall separator-left face1 face2)
                                  (powerline-vc face2 'r)))
                            (rhs (list (powerline-raw global-mode-string face2 'r)
                                       (funcall separator-right face2 face1)
                                       (unless window-system
                                         (powerline-raw (char-to-string #xe0a1) face1 'l))
                                       (powerline-raw "%4l" face1 'l)
                                       (powerline-raw ":" face1 'l)
                                       (powerline-raw "%3c" face1 'r)
                                       (funcall separator-right face1 face0)
                                       (powerline-raw " " face0)
                                       (powerline-raw "%6p" face0 'r)
                                       (when powerline-display-hud
                                         (powerline-hud face0 face2))
                                       (powerline-fill face0 0)
                                       )))
                       (concat (powerline-render lhs)
                               (powerline-fill face2 (powerline-width rhs))
                               (powerline-render rhs)))))))

  (which-function-mode 1)
  ;; (eval-after-load "which-func"
  ;; '(setq which-func-modes '(c++-mode org-mode)))

  (setq which-func-unknown "---")

  (defpowerline powerline-func
    (let ((func-str (or (which-function) "---")))
      func-str))


  (defpowerline powerline-buffer-file-name
    (let* ((buffer-str
            (if buffer-file-name
                (replace-regexp-in-string (regexp-quote (getenv "HOME")) "~" buffer-file-name)
              "[No file]"))
           (buffer-width (window-total-width))
           (name-width (length buffer-str))
           )
      (cond ((string= buffer-str "[No file]") buffer-str)
            ((and (< buffer-width 80) (> name-width 35))
             (replace-regexp-in-string ".*/\\([^/]*\\)" "\\1" buffer-str))
            ((and (< buffer-width 120) (> name-width 50))
             (replace-regexp-in-string "\\([^/]+/[^/]+/\\).*\\(/[^/]*/[^/]*\\)" "\\1..\\2" buffer-str))
            ((and (< buffer-width 120) (> name-width 35))
             (replace-regexp-in-string "\\([^/]+/[^/]+/\\).*\\(/[^/]*\\)" "\\1..\\2" buffer-str))
            ((> name-width 55)
             (replace-regexp-in-string "\\([^/]+/[^/]+/\\).*\\(/[^/]*/[^/]*\\)" "\\1..\\2" buffer-str))
            (t buffer-str)
            )
      ))


  (defun powerline-show-func-p ()
    (member major-mode '(
                         c-mode c++-mode lua-mode
                         )))

  (defun header-line-custom-theme ()
    "Setup the default mode-line."
    (interactive)
    (setq header-line-format
          '((:eval
             (let* ((active (powerline-selected-window-active))
                    (face0 (if active 'mypowerline-active2 'powerline-inactive1))
                    (face1 (if active 'powerline-inactive2 'powerline-inactive2 'powerline-inactive1))
                    (face2 'powerline-inactive0)
                    (separator-left (intern (format "powerline-%s-%s"
                                                    (powerline-current-separator)
                                                    (car powerline-default-separator-dir))))
                    (separator-right (intern (format "powerline-%s-%s"
                                                     (powerline-current-separator)
                                                     (cdr powerline-default-separator-dir))))
                    (lhs (list
                          (powerline-buffer-file-name face0 'l)
                          (powerline-raw " " face0)
                          (funcall separator-left face0 face2)
                          ))
                    (chs (list
                          (when (bound-and-true-p nyan-mode)
                            (powerline-raw (list "[" (nyan-create) "]")
                                           face2 'l))
                          ))
                    (rhs (list
                          (when (powerline-show-func-p) (funcall separator-right face2 face1))
                          (when (powerline-show-func-p) (powerline-raw " " face1))
                          (when (powerline-show-func-p) (powerline-func face1 'r))
                          (if (powerline-show-func-p) (funcall separator-right face1 face0)
                            (funcall separator-right face2 face0))
                          (powerline-raw " " face0)
                          (powerline-raw (symbol-name buffer-file-coding-system) face0 'r)
                          ;; (powerline-coding face0 'r)
                          )))
               (concat (powerline-render lhs)
                       (powerline-fill-center face2 (/ (powerline-width chs) 2.0))
                       (powerline-render chs)
                       (powerline-fill face2 (powerline-width rhs))
                       (powerline-render rhs))))))
    )


  (powerline-custom-theme)
  ;; (header-line-custom-theme)
  (defvar powerline-header-bar nil)
  (if powerline-header-bar
      (add-hook 'prog-mode-hook 'header-line-custom-theme))

  (setq powerline-default-separator 'arrow)
  )

(if window-system
    (progn
      (global-hl-line-mode 1)
      ;; (set-face-attribute hl-line-face nil :underline t)
      )
  (progn
    (global-hl-line-mode 1)))

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

(global-set-key (kbd "M-ù") 'query-replace-regexp)
(global-set-key (kbd "M-§") 'shell-command-on-region)

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

(provide 'setup-general)
;;; setup-general.el ends here
