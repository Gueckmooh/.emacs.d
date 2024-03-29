;;; Package --- summary

;;; Commentary:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  _          _            ;;
;; | |__   ___| |_ __ ___   ;;
;; | '_ \ / _ \ | '_ ` _ \  ;;
;; | | | |  __/ | | | | | | ;;
;; |_| |_|\___|_|_| |_| |_| ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code:

(use-package helm
  :demand t
  :ensure t
  :init
  (global-unset-key (kbd "C-x c"))
  :bind
  (("M-x" . helm-M-x)
   ("M-y" . helm-show-kill-ring)
   ("C-x b" . helm-buffers-list)
   ("C-x C-f" . helm-find-files)
   ("C-x b" . helm-buffers-list)
   ("C-c r" . helm-recentf)
   ("C-h SPC" . helm-all-mark-rings)
   ("C-c h o" . helm-occur)
   ("C-c h o" . helm-occur)
   ("C-c h w" . helm-wikipedia-suggest)
   ("C-c h g" . helm-google-suggest)
   ("C-c h x" . helm-register)
   ("C-c h" . helm-command-prefix)
   ("M-SPC" . helm-all-mark-rings)
   :map helm-map
   ("<tab>" . helm-execute-persistent-action)
   ("C-i" . helm-execute-persistent-action)
   ("C-z" . 'helm-select-action)
   :map helm-grep-mode-map
   ("<return>" . 'helm-grep-mode-jump-other-window)
   ("n" . 'helm-grep-mode-jump-other-window-forward)
   ("p" . 'helm-grep-mode-jump-other-window-backward)
   ;; :map help-command
   ;; ("C-f" . helm-apropos)
   ;; ("r" . helm-info-emacs)
   ;; ("C-l" . helm-locate-library)
   :map minibuffer-local-map
   ("M-p" . helm-minibuffer-history)
   ("M-n" . helm-minibuffer-history))
  :config
  (progn
    (require 'helm-config)
    (require 'helm-grep)
    (add-to-list 'helm-sources-using-default-as-input 'helm-source-man-pages)
    ;; (define-key 'help-command (kbd "C-f") 'helm-apropos)
    ;; (define-key 'help-command (kbd "r") 'helm-info-emacs)
    ;; (define-key 'help-command (kbd "C-l") 'helm-locate-library)

    ;; use helm to list eshell history
    (add-hook 'eshell-mode-hook
              #'(lambda ()
                  (define-key eshell-mode-map (kbd "M-l")  'helm-eshell-history)));; show minibuffer history with Helm
    ;; (define-key minibuffer-local-map (kbd "M-p") 'helm-minibuffer-history)
    ;; (define-key minibuffer-local-map (kbd "M-n") 'helm-minibuffer-history)

    (setq helm-google-suggest-use-curl-p t
          helm-scroll-amount 4 ; scroll 4 lines other window using M-<next>/M-<prior>
          ;; helm-quick-update t ; do not display invisible candidates
          helm-ff-search-library-in-sexp t ; search for library in `require' and `declare-function' sexp.

          ;; you can customize helm-do-grep to execute ack-grep
          ;; helm-grep-default-command "ack-grep -Hn --smart-case --no-group --no-color %e %p %f"
          ;; helm-grep-default-recurse-command "ack-grep -H --smart-case --no-group --no-color %e %p %f"
          helm-split-window-in-side-p t ;; open helm buffer inside current window, not occupy whole other window

          helm-echo-input-in-header-line t

          ;; helm-candidate-number-limit 500 ; limit the number of displayed canidates
          helm-ff-file-name-history-use-recentf t
          helm-move-to-line-cycle-in-source t ; move to end or beginning of source when reaching top or bottom of source.
          helm-buffer-skip-remote-checking t

          helm-buffers-fuzzy-matching t ; fuzzy matching buffer names when non-nil
                                        ; useful in helm-mini that lists buffers
          helm-org-headings-fontify t
          ;; helm-find-files-sort-directories t
          ;; ido-use-virtual-buffers t
          helm-semantic-fuzzy-match t
          helm-M-x-fuzzy-match t
          helm-imenu-fuzzy-match t
          helm-lisp-fuzzy-completion t
          helm-apropos-fuzzy-match t
          helm-buffer-skip-remote-checking t
          helm-locate-fuzzy-match t
          helm-display-header-line nil
          helm-mode-fuzzy-match t

          helm-completion-style 'emacs)
    (add-to-list 'completion-styles 'flex)

    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; PACKAGE: helm-swoop                ;;
    ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
    ;; Locate the helm-swoop folder to your path
    (use-package helm-swoop
      :defer t
      :ensure helm
      :bind (("C-c h o" . helm-swoop)
             ("C-c s" . helm-multi-swoop-all)
             :map isearch-mode-map
             ("M-i" . helm-swoop-from-isearch)
             :map helm-swoop-map
             ("M-i" . helm-multi-swoop-all-from-helm-swoop)
             )
      :config
      ;; Save buffer when helm-multi-swoop-edit complete
      (setq helm-multi-swoop-edit-save t)

      ;; If this value is t, split window inside the current window
      (setq helm-swoop-split-with-multiple-windows t)

      ;; Split direcion. 'split-window-vertically or 'split-window-horizontally
      (setq helm-swoop-split-direction 'split-window-vertically)

      ;; If nil, you can slightly boost invoke speed in exchange for text color
      (setq helm-swoop-speed-or-color t)

      ;; Do not show boring files
      (setq helm-ff-skip-boring-files t))



    (helm-mode 1)

    (defun helm-hide-minibuffer-maybe ()
      (when (with-helm-buffer helm-echo-input-in-header-line)
        (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
          (overlay-put ov 'window (selected-window))
          (overlay-put ov 'face (let ((bg-color (face-background 'default nil)))
                                  `(:background ,bg-color :foreground ,bg-color)))
          (setq-local cursor-type nil))))

    (add-hook 'helm-minibuffer-set-up-hook 'helm-hide-minibuffer-maybe)

    ;; (use-package projectile
    ;;   :init
    ;;   (projectile-global-mode))

    ;; (if (version< "25.1" emacs-version)
    ;;     (use-package helm-projectile
    ;;       :ensure helm
    ;;       :ensure projectile
    ;;       :init
    ;;       (helm-projectile-on)
    ;;       (setq projectile-completion-system 'helm)
    ;;       (setq projectile-indexing-method 'alien)
    ;;       (add-hook 'prog-mode-hook 'projectile-mode)) ())
    ))


(use-package ace-isearch
  :defer t
  :config
  (global-ace-isearch-mode +1)

  (custom-set-variables
   '(ace-isearch-input-length 7)
   '(ace-isearch-jump-delay 0.25)
   '(ace-isearch-function 'avy-goto-char)
   '(ace-isearch-use-jump 'printing-char))

  (define-key isearch-mode-map (kbd "C-,") 'ace-isearch-jump-during-isearch)
  (define-key isearch-mode-map (kbd "C-;") 'ace-isearch-helm-swoop-from-isearch))

;; (use-package helm-xref)
(which-key-mode)


(provide 'setup-helm)
;;; setup-helm.el ends here
