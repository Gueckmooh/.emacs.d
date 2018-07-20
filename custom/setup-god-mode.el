(use-package god-mode
  :ensure t
  :config
  (global-set-key (kbd "<escape>") 'god-mode-all)
  (global-set-key (kbd "M-RET") 'god-mode-all)
  (define-key god-local-mode-map (kbd "œ") 'ace-window)
  (define-key god-local-mode-map (kbd "z") 'repeat)
  (define-key god-local-mode-map (kbd "&") 'zygospore-toggle-delete-other-windows)
  (define-key god-local-mode-map (kbd "é") 'split-window-below)
  (define-key god-local-mode-map (kbd "\"") 'split-window-right)
  (define-key god-local-mode-map (kbd "à") 'delete-window)


  (defun c/god-mode-update-cursor ()
    (interactive)
    (let ((limited-colors-p (> 257 (length (defined-colors)))))
      (cond (god-local-mode (progn
                              (set-face-background 'mode-line (if limited-colors-p "white" "#e9e2cb"))
                              (set-face-background 'mode-line-inactive (if limited-colors-p "white" "#e9e2cb"))))
            (t (progn
                 (set-face-background 'mode-line (if limited-colors-p "black" "#0a2832"))
                 (set-face-background 'mode-line-inactive (if limited-colors-p "black" "#0a2832")))))))

  (defun my-update-cursor ()
    (setq cursor-type (if (or god-local-mode buffer-read-only)
                          'hollow
                        'box)))

  (add-hook 'god-mode-enabled-hook 'my-update-cursor)
  (add-hook 'god-mode-disabled-hook 'my-update-cursor)

  (add-hook 'god-local-mode-hook 'c/god-mode-update-cursor)

  ;; (define-key key-translation-map (kbd "ESC") (kbd "<escape>"))
  ;; (define-key key-translation-map (kbd "C-ESC") (kbd "ESC"))

  )


(provide 'setup-god-mode)
