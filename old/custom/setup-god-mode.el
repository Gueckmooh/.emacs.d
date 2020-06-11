;;                  _                           _
;;   __ _  ___   __| |      _ __ ___   ___   __| | ___
;;  / _` |/ _ \ / _` |_____| '_ ` _ \ / _ \ / _` |/ _ \
;; | (_| | (_) | (_| |_____| | | | | | (_) | (_| |  __/
;;  \__, |\___/ \__,_|     |_| |_| |_|\___/ \__,_|\___|
;;  |___/

(use-package god-mode
  :ensure t
  :init
  (require 'god-mode-isearch)
  (defun my/god-mode-update-cursor ()
    (setq cursor-type (if (or god-local-mode buffer-read-only)
                          'hollow
                        'box)))
  :bind (("<escape>" . god-mode-all)
         ("M-RET" . god-mode-all)
         :map god-local-mode-map
         ("X" . helm-M-x)
         ("œ" . ace-window)
         ("²" . ace-window)
         ("z" . repeat)
         ("&" . zygospore-toggle-delete-other-windows)
         ("é" . split-window-below)
         ("\"" . split-window-right)
         ("à" . delete-window)
         ("h" . delete-backward-char)
         ("i" . god-mode-all)
         ("<" . beginning-of-buffer)
         (">" . end-of-buffer)
         ("I" . (lambda () (interactive) (beginning-of-line)
                  (open-line 1) (god-mode-all)))
         ("O" . other-window)
         ("B" . helm-buffers-list)
         ("Q" . quit-window)
         :map isearch-mode-map
         ("<escape>" . god-mode-isearch-activate)
         ("<escape>" . god-mode-isearch-disable)
         )
  :hook ((god-mode-enabled-hook god-mode-disabled-hook) .
         my/god-mode-update-cursor)
  :config
  ;; (setq god-exempt-major-modes nil)
  ;; (setq god-exempt-predicates nil)

  (add-to-list 'god-exempt-major-modes 'eshell-mode)

  (define-key god-local-mode-map (kbd ";") 'comment-dwim)

  (defun god-mode-all-if-not () (if (not god-global-mode) (god-mode-all)))
  (defvar idle-god-mode-timer
    (run-with-idle-timer 10 t 'god-mode-all-if-not)
    "Timer that enables god mode after 10 seconds of inactivity")

  (defun god-activate-idle-timer ()
    (interactive)
    (if (not idle-god-mode-timer)
        (setq idle-god-mode-timer
              (run-with-idle-timer 10 t 'god-mode-all-if-not))))

  (defun god-deactivate-idle-timer ()
    (interactive)
    (if idle-god-mode-timer
        (progn (cancel-timer idle-god-mode-timer)
               (setq idle-god-mode-timer nil))
      ))

  (god-mode)
  )

(provide 'setup-god-mode)
