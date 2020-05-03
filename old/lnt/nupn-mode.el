;; Emacs mode for NUPN
;; Maintainer:	Syntax Editor Configuration Tool <cadp@inria.fr>
;; Version:	1.2:NUPN+1.8:SECT

;;; ----------------------------------------------------------------------------
;;;                                DEFINITIONS
;;; ----------------------------------------------------------------------------

(defvar nupn-mode-syntax-table nil
   "Syntax table used while in NUPN mode")
(if nupn-mode-syntax-table
      ()
   (setq nupn-mode-syntax-table (make-syntax-table))
   (modify-syntax-entry ?( "()"   nupn-mode-syntax-table)
   (modify-syntax-entry ?) ")("   nupn-mode-syntax-table)
   (modify-syntax-entry ?* "."   nupn-mode-syntax-table)
   (modify-syntax-entry ?+ "."   nupn-mode-syntax-table)
   (modify-syntax-entry ?- "."   nupn-mode-syntax-table)
   (modify-syntax-entry ?= "."   nupn-mode-syntax-table)
   (modify-syntax-entry ?% "."   nupn-mode-syntax-table)
   (modify-syntax-entry ?< "."   nupn-mode-syntax-table)
   (modify-syntax-entry ?> "."   nupn-mode-syntax-table)
   (modify-syntax-entry ?& "."   nupn-mode-syntax-table)
   (modify-syntax-entry ?| "."   nupn-mode-syntax-table)
   (modify-syntax-entry ?_ "w"   nupn-mode-syntax-table)
   (modify-syntax-entry ?! "w"   nupn-mode-syntax-table)
   (modify-syntax-entry ?\" "."   nupn-mode-syntax-table)
)

;;; ----------------------------------------------------------------------------
;;;                                HIGHLIGHTING
;;; ----------------------------------------------------------------------------

(defvar nupn-font-lock-keywords
   (list
      ;; NUPN pragmas
      (cons (regexp-opt '("!creator" "!multiple_arcs" "!multiple_initial_tokens" "!unit_safe") 'words)
             'font-lock-builtin-face)
      ;; NUPN keywords
      (cons (regexp-opt '("initial" "place" "places" "root" "transitions" "unit"
                          "units") 'words)
             'font-lock-keyword-face)
      ;; key symbols
      (cons (regexp-opt '("#" "...") t)
             'font-lock-keyword-face)
   )
   "Keywords used in NUPN mode")

;;; ----------------------------------------------------------------------------
;;;                                MODE
;;; ----------------------------------------------------------------------------

(setq aux-file (expand-file-name (concat "~/.emacs.d/lnt/nupn-aux.el")))
(if (file-exists-p aux-file) (load aux-file))

(defun nupn-mode ()
   "Running NUPN mode"
   (interactive)
   (kill-all-local-variables)
   (setq major-mode 'nupn-mode)
   (setq mode-name "NUPN")
   (set-syntax-table nupn-mode-syntax-table)
   (set (make-local-variable 'font-lock-defaults)
        '(nupn-font-lock-keywords nil nil))
   (if (file-exists-p aux-file) (nupn-entry-point))
   (run-hooks 'nupn-mode-hook)
)

(provide 'nupn-mode)
