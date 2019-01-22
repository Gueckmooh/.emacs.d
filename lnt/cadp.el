
(setq load-path (cons (expand-file-name (concat "~/.emacs.d/lnt")) load-path))

(autoload 'bes-mode "bes-mode" "BES mode" t nil)
(setq auto-mode-alist (cons '("\\.bes$"   . bes-mode) auto-mode-alist))

(autoload 'exp-mode "exp-mode" "EXP mode" t nil)
(setq auto-mode-alist (cons '("\\.exp$"   . exp-mode) auto-mode-alist))

(autoload 'lotos-mode "lotos-mode" "LOTOS mode" t nil)
(setq auto-mode-alist (cons '("\\.lotos$" . lotos-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.lot$"   . lotos-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.lib$"   . lotos-mode) auto-mode-alist))

(autoload 'lnt-mode "lnt-mode" "LNT mode" t nil)
(setq auto-mode-alist (cons '("\\.lnt$"   . lnt-mode) auto-mode-alist))

(autoload 'mcl-mode "mcl-mode" "MCL mode" t nil)
(setq auto-mode-alist (cons '("\\.mcl$"   . mcl-mode) auto-mode-alist))

(autoload 'rbc-mode "rbc-mode" "RBC mode" t nil)
(setq auto-mode-alist (cons '("\\.rbc$"   . rbc-mode) auto-mode-alist))

(autoload 'nupn-mode "nupn-mode" "NUPN mode" t nil)
(setq auto-mode-alist (cons '("\\.nupn$"  . nupn-mode) auto-mode-alist))

(autoload 'svl-mode "svl-mode" "SVL mode" t nil)
(setq auto-mode-alist (cons '("\\.svl$"   . svl-mode) auto-mode-alist))

(autoload 'xtl-mode "xtl-mode" "XTL mode" t nil)
(setq auto-mode-alist (cons '("\\.xtl$"   . xtl-mode) auto-mode-alist))

(if (string-match "XEmacs" emacs-version 0)
    (progn
      (require 'font-lock)
      (setq-default font-lock-auto-fontify t))
  (global-font-lock-mode t)
)
