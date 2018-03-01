;; GROUP: Editing -> Editing Basics
(setq global-mark-ring-max 5000         ; increase mark ring to contains 5000 entries
      mark-ring-max 5000                ; increase kill ring to contains 5000 entries
      mode-require-final-newline t      ; add a newline to end of file
      )

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
              '(emacs-lisp-mode scheme-mode lisp-mode
                                c-mode c++-mode objc-mode
                                latex-mode plain-tex-mode))
      (if (and (eolp) (not (bolp)))
          (progn (forward-char 1)
                 (just-one-space 0)
                 (backward-char 1)))))

(show-paren-mode t)
(use-package auto-highlight-symbol
  :init
  (add-hook 'prog-mode-hook 'auto-highlight-symbol-mode))

(require 'cc-mode)

(use-package move-text
  :init
  (define-key c-mode-map (kbd "M-p") 'move-text-up)
  (define-key c++-mode-map (kbd "M-p") 'move-text-up)

  (define-key c-mode-map (kbd "M-n") 'move-text-down)
  (define-key c++-mode-map (kbd "M-n") 'move-text-down))

(defun mymajline ()
  (setq linum-format
        (let ((w (length (number-to-string
                          (count-lines (point-min) (point-max))))))
          (concat "%" (number-to-string w) "d\u2502"))))

(add-hook 'linum-before-numbering-hook 'mymajline)

;; (global-linum-mode t)
(defun toggle-linum ()
  (linum-mode 1))

(add-hook 'c-mode-hook 'toggle-linum)
(add-hook 'c++-mode-hook 'toggle-linum)
(add-hook 'emacs-lisp-mode-hook 'toggle-linum)

(provide 'setup-editing)
