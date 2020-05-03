;; Emacs mode for RBC
;; Maintainer:	Syntax Editor Configuration Tool <cadp@inria.fr>
;; Version:	1.3:RBC+1.8:SECT

;;; ----------------------------------------------------------------------------
;;;                                DEFINITIONS
;;; ----------------------------------------------------------------------------

(defvar rbc-mode-syntax-table nil
   "Syntax table used while in RBC mode")
(if rbc-mode-syntax-table
      ()
   (setq rbc-mode-syntax-table (make-syntax-table))
   (modify-syntax-entry ?( "()"   rbc-mode-syntax-table)
   (modify-syntax-entry ?) ")("   rbc-mode-syntax-table)
   (modify-syntax-entry ?* "."   rbc-mode-syntax-table)
   (modify-syntax-entry ?+ "."   rbc-mode-syntax-table)
   (modify-syntax-entry ?- "."   rbc-mode-syntax-table)
   (modify-syntax-entry ?= "."   rbc-mode-syntax-table)
   (modify-syntax-entry ?% "."   rbc-mode-syntax-table)
   (modify-syntax-entry ?< "."   rbc-mode-syntax-table)
   (modify-syntax-entry ?> "."   rbc-mode-syntax-table)
   (modify-syntax-entry ?& "."   rbc-mode-syntax-table)
   (modify-syntax-entry ?| "."   rbc-mode-syntax-table)
   (modify-syntax-entry ?_ "w"   rbc-mode-syntax-table)
   (modify-syntax-entry ?# "@ 12b"   rbc-mode-syntax-table)
   (modify-syntax-entry ?\n "> b"   rbc-mode-syntax-table)
   (modify-syntax-entry ?\" "."   rbc-mode-syntax-table)
)

;;; ----------------------------------------------------------------------------
;;;                                HIGHLIGHTING
;;; ----------------------------------------------------------------------------

(defvar rbc-font-lock-keywords
   (list
      ;; line comments
      (cons "#.*$" 'font-lock-comment-face)
      ;; RBC keywords
      (cons (regexp-opt '("alternation_free" "equation_length" "false_constant_ratio" "local_variable_ratio" "number_of_blocks" "number_of_variables"
                          "or_variable_alternation" "or_variable_ratio" "shape" "sign" "solve_mode" "strategy"
                          "unique_resolution" "variable_ratio") 'words)
             'font-lock-keyword-face)
      ;; key symbols
      (cons (regexp-opt '("[" "]" "=" "..") t)
             'font-lock-keyword-face)
   )
   "Keywords used in RBC mode")

;;; ----------------------------------------------------------------------------
;;;                                MODE
;;; ----------------------------------------------------------------------------

(setq aux-file (expand-file-name (concat "~/.emacs.d/lnt/rbc-aux.el")))
(if (file-exists-p aux-file) (load aux-file))

(defun rbc-mode ()
   "Running RBC mode"
   (interactive)
   (kill-all-local-variables)
   (setq major-mode 'rbc-mode)
   (setq mode-name "RBC")
   (set-syntax-table rbc-mode-syntax-table)
   (set (make-local-variable 'comment-start) "#")
   (set (make-local-variable 'font-lock-defaults)
        '(rbc-font-lock-keywords nil nil))
   (if (file-exists-p aux-file) (rbc-entry-point))
   (run-hooks 'rbc-mode-hook)
)

(provide 'rbc-mode)
