;;; Package --- summary

;;; Commentary:
;;  _____    _ _ _   _
;; | ____|__| (_) |_(_)_ __   __ _
;; |  _| / _` | | __| | '_ \ / _` |
;; | |__| (_| | | |_| | | | | (_| |
;; |_____\__,_|_|\__|_|_| |_|\__, |
;;                           |___/
;;; Code:

(require 'cc-mode)

(setq global-mark-ring-max 5000         ; increase mark ring to contains 5000 entries
      mark-ring-max 5000                ; increase kill ring to contains 5000 entries
      mode-require-final-newline t      ; add a newline to end of file
      )

;; If in terminal, copy paste with xclip
(if (not window-system)
    (use-package xclip
      :config
      (xclip-mode)))

;; Moving the cursor
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-P") 'backward-list)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-N") 'forward-list)

;; GROUP: Editing -> Killing
(setq kill-ring-max 5000 ; increase kill-ring capacity
      kill-whole-line t  ; if NIL, kill whole line and move the next line up
      )

(setq-default indent-tabs-mode nil)
(delete-selection-mode)
(global-set-key (kbd "RET") 'newline-and-indent)

;; kill a line, including whitespace characters until next non-whiepsace character
;; of next line
(defadvice kill-line (before check-position activate)
  (if (member major-mode
              '(emacs-lisp-mode scheme-mode lisp-mode lisp-interaction-mode
                                c-mode c++-mode objc-mode
                                latex-mode plain-tex-mode python-mode
                                lua-mode tuareg-mode))
      (if (and (eolp) (not (bolp)))
          (progn (forward-char 1)
                 (just-one-space 0)
                 (backward-char 1)))))

(show-paren-mode t)
(use-package auto-highlight-symbol
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'auto-highlight-symbol-mode))

(when (version<= "26.0.50" emacs-version )
  (add-hook 'prog-mode-hook (lambda () (setq display-line-numbers 'relative))))

(defun gk/linum-mode ()
  "Activate 'display-line-numbers'."
  (interactive) (setq display-line-numbers 'relative))

(defun remove-trailing-whitespaces ()
  "User defined function, remove all trailing whitespace and lines from the file."
  (interactive)
  (setq delete-trailing-lines t)
  (delete-trailing-whitespace (point-min)))

(defun format-when-save ()
  (add-hook 'before-save-hook
            (lambda ()
              (unless (string= major-mode "org-mode")
                (setq delete-trailing-lines t)
                (delete-trailing-whitespace (point-min))))))

(add-hook 'prog-mode-hook 'format-when-save)

(use-package multiple-cursors
  :ensure t
  :bind
  (("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)
   ("C-c C-<" . mc/mark-all-like-this))
  )

(use-package ace-mc
  :config
  (global-set-key (kbd "C-)") 'ace-mc-add-multiple-cursors)
  (global-set-key (kbd "C-M-)") 'ace-mc-add-single-cursor))


(use-package iedit
  :bind (("C-," . iedit-mode))
  :config
  (setq iedit-toggle-key-default nil))

(use-package aggressive-indent
  :config
  (global-aggressive-indent-mode 1)
  (add-to-list 'aggressive-indent-excluded-modes 'html-mode))

;; (use-package aggressive-fill-paragraph
;;   :config
;;   (afp-setup-recommended-hooks))

;;     _         _                    _
;;    / \  _   _| |_ ___  _ __   __ _(_)_ __
;;   / _ \| | | | __/ _ \| '_ \ / _` | | '__|
;;  / ___ \ |_| | || (_) | |_) | (_| | | |
;; /_/   \_\__,_|\__\___/| .__/ \__,_|_|_|
;;                       |_|


(use-package autopair
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'autopair-mode)

  (setq autopair-autowrap t)
  (delete-selection-mode 1)
  (put 'autopair-insert-opening 'delete-selection t)
  (put 'autopair-skip-close-maybe 'delete-selection t)
  (put 'autopair-insert-or-skip-quote 'delete-selection t)
  (put 'autopair-extra-insert-opening 'delete-selection t)
  (put 'autopair-extra-skip-close-maybe 'delete-selection t)
  (put 'autopair-backspace 'delete-selection 'supersede)
  (put 'autopair-newline 'delete-selection t))


;;   ____
;;  / ___|___  _ __ ___  _ __   __ _ _ __  _   _
;; | |   / _ \| '_ ` _ \| '_ \ / _` | '_ \| | | |
;; | |__| (_) | | | | | | |_) | (_| | | | | |_| |
;;  \____\___/|_| |_| |_| .__/ \__,_|_| |_|\__, |
;;                      |_|                |___/

(use-package company
  :ensure t
  :bind ("C-;" . company-complete-common)
  :config
  (setq company-idle-delay              0
        company-minimum-prefix-length   3
        company-show-numbers            t
        company-tooltip-limit           20
        )
  (add-hook 'prog-mode-hook 'company-mode)
  )

(use-package flycheck
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'flycheck-mode)
  )

(use-package yasnippet
  :ensure t
  :init
  (use-package yasnippet-snippets :ensure t)
  :config
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  )

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package rainbow-identifiers
  :config
  (add-hook 'prog-mode-hook 'rainbow-identifiers-mode))

;;   ____                      _ _ _
;;  / ___|___  _ __ ___  _ __ (_) (_)_ __   __ _
;; | |   / _ \| '_ ` _ \| '_ \| | | | '_ \ / _` |
;; | |__| (_) | | | | | | |_) | | | | | | | (_| |
;;  \____\___/|_| |_| |_| .__/|_|_|_|_| |_|\__, |
;;                      |_|                |___/


(use-package smart-compile
  :ensure t
  :config

  (add-to-list 'smart-compile-alist '("\\.tex\\'" . "rubber -d %f"))

  (defun desperately-compile ()
    "Traveling up the path, find a Makefile and `compile'."
    (interactive)
    (when (locate-dominating-file default-directory "Makefile")
      (with-temp-buffer
        (cd (locate-dominating-file default-directory "Makefile"))
        (call-interactively #'compile))))

  (define-key c++-mode-map (kbd "C-x RET RET") 'desperately-compile)
  (define-key c-mode-map (kbd "C-x RET RET") 'desperately-compile)

  (define-key c++-mode-map (kbd "C-c RET") 'compile)
  (define-key c-mode-map (kbd "C-c RET") 'compile))

(defun smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap move-beginning-of-line]
                'smarter-move-beginning-of-line)

;; For glips
(add-to-list 'auto-mode-alist '("\\.glips\\'" . ada-mode))

(provide 'setup-editing)
;;; setup-editing.el ends here
