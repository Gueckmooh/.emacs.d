;; Emacs mode for SVL
;; Maintainer:	Syntax Editor Configuration Tool <cadp@inria.fr>
;; Version:	1.8:SVL+1.8:SECT

;;; ----------------------------------------------------------------------------
;;;                                DEFINITIONS
;;; ----------------------------------------------------------------------------

(defvar svl-mode-syntax-table nil
   "Syntax table used while in SVL mode")
(if svl-mode-syntax-table
      ()
   (setq svl-mode-syntax-table (make-syntax-table))
   (modify-syntax-entry ?+ "."   svl-mode-syntax-table)
   (modify-syntax-entry ?= "."   svl-mode-syntax-table)
   (modify-syntax-entry ?% "."   svl-mode-syntax-table)
   (modify-syntax-entry ?< "."   svl-mode-syntax-table)
   (modify-syntax-entry ?> "."   svl-mode-syntax-table)
   (modify-syntax-entry ?& "."   svl-mode-syntax-table)
   (modify-syntax-entry ?| "."   svl-mode-syntax-table)
   (modify-syntax-entry ?_ "w"   svl-mode-syntax-table)
   (modify-syntax-entry ?( "() 1"   svl-mode-syntax-table)
   (modify-syntax-entry ?* ". 23"   svl-mode-syntax-table)
   (modify-syntax-entry ?) ")( 4"   svl-mode-syntax-table)
   (modify-syntax-entry ?- ". 12b"   svl-mode-syntax-table)
   (modify-syntax-entry ?\n "> b"   svl-mode-syntax-table)
   (modify-syntax-entry ?\' "\"'"   svl-mode-syntax-table)
)

;;; ----------------------------------------------------------------------------
;;;                                HIGHLIGHTING
;;; ----------------------------------------------------------------------------

(defvar svl-font-lock-keywords
   (list
      ;; line comments
      (cons "--.*$" 'font-lock-comment-face)
      ;; pragmas lines
      (cons "^[[:space:]]*%.*$" 'font-lock-builtin-face)
      ;; SVL keywords
      (cons (regexp-opt '("abstraction" "all" "bag" "branching" "but" "chaos"
                          "check" "comparison" "cut" "deadlock" "divbranching" "end"
                          "expected" "fifo" "gate" "generation" "hide" "in"
                          "is" "label" "labels" "leaf" "livelock" "multiple"
                          "node" "observational" "of" "par" "partial" "prio"
                          "probabilistic" "property" "reduction" "refined" "rename" "result"
                          "root" "safety" "single" "smart" "stochastic" "stop"
                          "strong" "sync" "tau-compression" "tau-confluence" "tau-divergence" "tau*.a"
                          "total" "trace" "user" "using" "verify" "weak"
                          "with") 'words)
             'font-lock-keyword-face)
      ;; SVL types
      (cons (regexp-opt '("acyclic" "bdd" "bfs" "dfs" "fly" "std"
                         ) 'words)
             'font-lock-type-face)
      ;; SVL constants
      (cons (regexp-opt '("FALSE" "TRUE") 'words)
             'font-lock-constant-face)
      ;; SVL functions
      (cons (regexp-opt '("aldebaran" "bcg_min" "bcg_cmp" "bisimulator" "evaluator" "evaluator3"
                          "evaluator4" "exhibitor" "fc2tools" "reductor" "xtl") 'words)
             'font-lock-function-name-face)
      ;; key symbols
      (cons (regexp-opt '("[" "]" "{" "}" ">" "="
                          ":" "#" "?" "==" ">=" "<="
                          "|=" "->" "||" "|[" "]|" "-||"
                          "-|[" "|||" "-|||") t)
             'font-lock-keyword-face)
   )
   "Keywords used in SVL mode")

;;; ----------------------------------------------------------------------------
;;;                                MODE
;;; ----------------------------------------------------------------------------

(setq aux-file (expand-file-name (concat "~/.emacs.d/lnt/svl-aux.el")))
(if (file-exists-p aux-file) (load aux-file))

(defun svl-mode ()
   "Running SVL mode"
   (interactive)
   (kill-all-local-variables)
   (setq major-mode 'svl-mode)
   (setq mode-name "SVL")
   (set-syntax-table svl-mode-syntax-table)
   (set (make-local-variable 'comment-start) "(*")
   (set (make-local-variable 'comment-end) "*)")
   (set (make-local-variable 'font-lock-defaults)
        '(svl-font-lock-keywords nil nil))
   (if (file-exists-p aux-file) (svl-entry-point))
   (run-hooks 'svl-mode-hook)
)

(provide 'svl-mode)
