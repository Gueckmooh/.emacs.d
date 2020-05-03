;; Emacs mode for EXP
;; Maintainer:	Syntax Editor Configuration Tool <cadp@inria.fr>
;; Version:	2.6:EXP+1.8:SECT

;;; ----------------------------------------------------------------------------
;;;                                DEFINITIONS
;;; ----------------------------------------------------------------------------

(defvar exp-mode-syntax-table nil
   "Syntax table used while in EXP mode")
(if exp-mode-syntax-table
      ()
   (setq exp-mode-syntax-table (make-syntax-table))
   (modify-syntax-entry ?+ "."   exp-mode-syntax-table)
   (modify-syntax-entry ?= "."   exp-mode-syntax-table)
   (modify-syntax-entry ?% "."   exp-mode-syntax-table)
   (modify-syntax-entry ?< "."   exp-mode-syntax-table)
   (modify-syntax-entry ?> "."   exp-mode-syntax-table)
   (modify-syntax-entry ?& "."   exp-mode-syntax-table)
   (modify-syntax-entry ?| "."   exp-mode-syntax-table)
   (modify-syntax-entry ?_ "w"   exp-mode-syntax-table)
   (modify-syntax-entry ?( "() 1"   exp-mode-syntax-table)
   (modify-syntax-entry ?* ". 23"   exp-mode-syntax-table)
   (modify-syntax-entry ?) ")( 4"   exp-mode-syntax-table)
   (modify-syntax-entry ?- ". 12b"   exp-mode-syntax-table)
   (modify-syntax-entry ?\n "> b"   exp-mode-syntax-table)
   (modify-syntax-entry ?\' "\"'"   exp-mode-syntax-table)
)

;;; ----------------------------------------------------------------------------
;;;                                HIGHLIGHTING
;;; ----------------------------------------------------------------------------

(defvar exp-font-lock-keywords
   (list
      ;; line comments
      (cons "--.*$" 'font-lock-comment-face)
      ;; EXP keywords
      (cons (regexp-opt '("all" "any" "behavior" "behaviour" "but" "comm"
                          "cut" "end" "gate" "hide" "in" "par"
                          "prio" "rename" "using") 'words)
             'font-lock-keyword-face)
      ;; EXP types
      (cons (regexp-opt '("ccs" "csp" "elotos" "lotos" "mcrl") 'words)
             'font-lock-type-face)
      ;; EXP functions
      (cons (regexp-opt '("label" "multiple" "partial" "single" "total") 'words)
             'font-lock-function-name-face)
      ;; key symbols
      (cons (regexp-opt '("[" "]" "{" "}" ">" "="
                          "|" "/" "*" "#" "_" "->"
                         ) t)
             'font-lock-keyword-face)
   )
   "Keywords used in EXP mode")

;;; ----------------------------------------------------------------------------
;;;                                MODE
;;; ----------------------------------------------------------------------------

(setq aux-file (expand-file-name (concat "~/.emacs.d/lnt" "/exp-aux.el")))
(if (file-exists-p aux-file) (load aux-file))

(defun exp-mode ()
   "Running EXP mode"
   (interactive)
   (kill-all-local-variables)
   (setq major-mode 'exp-mode)
   (setq mode-name "EXP")
   (set-syntax-table exp-mode-syntax-table)
   (set (make-local-variable 'comment-start) "(*")
   (set (make-local-variable 'comment-end) "*)")
   (set (make-local-variable 'font-lock-defaults)
        '(exp-font-lock-keywords nil t))
   (if (file-exists-p aux-file) (exp-entry-point))
   (run-hooks 'exp-mode-hook)
)

(provide 'exp-mode)
