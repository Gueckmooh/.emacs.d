;; Emacs mode for BES
;; Maintainer:	Syntax Editor Configuration Tool <cadp@inria.fr>
;; Version:	1.3:BES+1.8:SECT

;;; ----------------------------------------------------------------------------
;;;                                DEFINITIONS
;;; ----------------------------------------------------------------------------

(defvar bes-mode-syntax-table nil
   "Syntax table used while in BES mode")
(if bes-mode-syntax-table
      ()
   (setq bes-mode-syntax-table (make-syntax-table))
   (modify-syntax-entry ?+ "."   bes-mode-syntax-table)
   (modify-syntax-entry ?- "."   bes-mode-syntax-table)
   (modify-syntax-entry ?= "."   bes-mode-syntax-table)
   (modify-syntax-entry ?% "."   bes-mode-syntax-table)
   (modify-syntax-entry ?< "."   bes-mode-syntax-table)
   (modify-syntax-entry ?> "."   bes-mode-syntax-table)
   (modify-syntax-entry ?& "."   bes-mode-syntax-table)
   (modify-syntax-entry ?| "."   bes-mode-syntax-table)
   (modify-syntax-entry ?_ "w"   bes-mode-syntax-table)
   (modify-syntax-entry ?( "() 1"   bes-mode-syntax-table)
   (modify-syntax-entry ?* ". 23"   bes-mode-syntax-table)
   (modify-syntax-entry ?) ")( 4"   bes-mode-syntax-table)
   (modify-syntax-entry ?\" "."   bes-mode-syntax-table)
)

;;; ----------------------------------------------------------------------------
;;;                                HIGHLIGHTING
;;; ----------------------------------------------------------------------------

(defvar bes-font-lock-keywords
   (list
      ;; BES keywords
      (cons (regexp-opt '("block" "end" "is" "mode" "mu" "nu"
                          "unique") 'words)
             'font-lock-keyword-face)
      ;; BES constants
      (cons (regexp-opt '("false" "true") 'words)
             'font-lock-constant-face)
      ;; BES functions
      (cons (regexp-opt '("and" "or") 'words)
             'font-lock-function-name-face)
   )
   "Keywords used in BES mode")

;;; ----------------------------------------------------------------------------
;;;                                MODE
;;; ----------------------------------------------------------------------------

(setq aux-file (expand-file-name (concat "~/.emacs.d/lnt/" "bes-aux.el")))
(if (file-exists-p aux-file) (load aux-file))

(defun bes-mode ()
   "Running BES mode"
   (interactive)
   (kill-all-local-variables)
   (setq major-mode 'bes-mode)
   (setq mode-name "BES")
   (set-syntax-table bes-mode-syntax-table)
   (set (make-local-variable 'comment-start) "(*")
   (set (make-local-variable 'comment-end) "*)")
   (set (make-local-variable 'font-lock-defaults)
        '(bes-font-lock-keywords nil nil))
   (if (file-exists-p aux-file) (bes-entry-point))
   (run-hooks 'bes-mode-hook)
)

(provide 'bes-mode)
