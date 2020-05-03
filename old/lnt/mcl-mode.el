;; Emacs mode for MCL
;; Maintainer:	Syntax Editor Configuration Tool <cadp@inria.fr>
;; Version:	1.8:MCL+1.8:SECT

;;; ----------------------------------------------------------------------------
;;;                                DEFINITIONS
;;; ----------------------------------------------------------------------------

(defvar mcl-mode-syntax-table nil
   "Syntax table used while in MCL mode")
(if mcl-mode-syntax-table
      ()
   (setq mcl-mode-syntax-table (make-syntax-table))
   (modify-syntax-entry ?+ "."   mcl-mode-syntax-table)
   (modify-syntax-entry ?- "."   mcl-mode-syntax-table)
   (modify-syntax-entry ?= "."   mcl-mode-syntax-table)
   (modify-syntax-entry ?% "."   mcl-mode-syntax-table)
   (modify-syntax-entry ?< "."   mcl-mode-syntax-table)
   (modify-syntax-entry ?> "."   mcl-mode-syntax-table)
   (modify-syntax-entry ?& "."   mcl-mode-syntax-table)
   (modify-syntax-entry ?| "."   mcl-mode-syntax-table)
   (modify-syntax-entry ?_ "w"   mcl-mode-syntax-table)
   (modify-syntax-entry ?( "() 1"   mcl-mode-syntax-table)
   (modify-syntax-entry ?* ". 23"   mcl-mode-syntax-table)
   (modify-syntax-entry ?) ")( 4"   mcl-mode-syntax-table)
   (modify-syntax-entry ?\' "\"'"   mcl-mode-syntax-table)
)

;;; ----------------------------------------------------------------------------
;;;                                HIGHLIGHTING
;;; ----------------------------------------------------------------------------

(defvar mcl-font-lock-keywords
   (list
      ;; MCL keywords
      (cons (regexp-opt '("among" "any" "case" "choice" "continue" "do"
                          "else" "elsif" "end" "end_library" "end_macro" "exists"
                          "exit" "export" "for" "forall" "from" "if"
                          "in" "let" "library" "loop" "macro" "mu"
                          "nil" "nu" "of" "on" "repeat" "step"
                          "tau" "then" "to" "until" "where" "while"
                         ) 'words)
             'font-lock-keyword-face)
      ;; MCL types
      (cons (regexp-opt '("bool" "nat" "natset" "int" "real" "char"
                          "string") 'words)
             'font-lock-type-face)
      ;; MCL constants
      (cons (regexp-opt '("false" "true") 'words)
             'font-lock-constant-face)
      ;; MCL functions
      (cons (regexp-opt '("abs" "and" "concat" "diff" "empty" "equ"
                          "implies" "index" "insert" "isin" "isalnum" "isalpha"
                          "isdigit" "islower" "isupper" "isxdigit" "inter" "length"
                          "not" "nth" "or" "prefix" "remove" "rindex"
                          "sign" "substr" "succ" "suffix" "tolower" "toupper"
                          "union" "xor") 'words)
             'font-lock-function-name-face)
      ;; key symbols
      (cons (regexp-opt '("+" "-" "=" "!" "/" "^"
                          "%" "|" "<" ">" "[" "]"
                          "@" "*" "?" "<>" "<=" ">="
                          "->" "-|") t)
             'font-lock-keyword-face)
   )
   "Keywords used in MCL mode")

;;; ----------------------------------------------------------------------------
;;;                                MODE
;;; ----------------------------------------------------------------------------

(setq aux-file (expand-file-name (concat "~/.emacs.d/lnt/mcl-aux.el")))
(if (file-exists-p aux-file) (load aux-file))

(defun mcl-mode ()
   "Running MCL mode"
   (interactive)
   (kill-all-local-variables)
   (setq major-mode 'mcl-mode)
   (setq mode-name "MCL")
   (set-syntax-table mcl-mode-syntax-table)
   (set (make-local-variable 'comment-start) "(*")
   (set (make-local-variable 'comment-end) "*)")
   (set (make-local-variable 'font-lock-defaults)
        '(mcl-font-lock-keywords nil t))
   (if (file-exists-p aux-file) (mcl-entry-point))
   (run-hooks 'mcl-mode-hook)
)

(provide 'mcl-mode)
