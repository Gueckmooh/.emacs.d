;; Emacs mode for XTL
;; Maintainer:	Syntax Editor Configuration Tool <cadp@inria.fr>
;; Version:	1.4:XTL+1.8:SECT

;;; ----------------------------------------------------------------------------
;;;                                DEFINITIONS
;;; ----------------------------------------------------------------------------

(defvar xtl-mode-syntax-table nil
   "Syntax table used while in XTL mode")
(if xtl-mode-syntax-table
      ()
   (setq xtl-mode-syntax-table (make-syntax-table))
   (modify-syntax-entry ?+ "."   xtl-mode-syntax-table)
   (modify-syntax-entry ?- "."   xtl-mode-syntax-table)
   (modify-syntax-entry ?= "."   xtl-mode-syntax-table)
   (modify-syntax-entry ?% "."   xtl-mode-syntax-table)
   (modify-syntax-entry ?< "."   xtl-mode-syntax-table)
   (modify-syntax-entry ?> "."   xtl-mode-syntax-table)
   (modify-syntax-entry ?& "."   xtl-mode-syntax-table)
   (modify-syntax-entry ?| "."   xtl-mode-syntax-table)
   (modify-syntax-entry ?_ "w"   xtl-mode-syntax-table)
   (modify-syntax-entry ?( "() 1"   xtl-mode-syntax-table)
   (modify-syntax-entry ?* ". 23"   xtl-mode-syntax-table)
   (modify-syntax-entry ?) ")( 4"   xtl-mode-syntax-table)
   (modify-syntax-entry ?! "w"   xtl-mode-syntax-table)
   (modify-syntax-entry ?\' "\"'"   xtl-mode-syntax-table)
   (modify-syntax-entry ?\` "\"`"   xtl-mode-syntax-table)
)

;;; ----------------------------------------------------------------------------
;;;                                HIGHLIGHTING
;;; ----------------------------------------------------------------------------

(defvar xtl-font-lock-keywords
   (list
      ;; XTL pragmas
      (cons (regexp-opt '("!assignedby" "!comparedby" "!enumeratedby" "!implementedby" "!printedby") 'words)
             'font-lock-builtin-face)
      ;; XTL keywords
      (cons (regexp-opt '("among" "any" "apply" "assert" "case" "current"
                          "def" "else" "else_if" "end_assert" "end_case" "end_def"
                          "end_exists" "end_flag" "end_for" "end_forall" "end_func" "end_if"
                          "end_include" "end_let" "end_library" "end_macro" "end_type" "end_use"
                          "exists" "flag" "for" "forall" "from" "func"
                          "if" "in" "include" "let" "library" "macro"
                          "of" "otherwise" "select" "then" "to" "type"
                          "use" "when" "where" "while") 'words)
             'font-lock-keyword-face)
      ;; XTL types
      (cons (regexp-opt '("action" "boolean" "natural" "number" "integer" "real"
                          "character" "string" "raw" "edge" "edgeset" "label"
                          "labelset" "state" "stateset") 'words)
             'font-lock-type-face)
      ;; XTL constants
      (cons (regexp-opt '("empty" "false" "full" "nop" "null" "number_of_edges"
                          "number_of_labels" "number_of_states" "true") 'words)
             'font-lock-constant-face)
      ;; XTL functions
      (cons (regexp-opt '("and" "and_then" "card" "comp" "diff" "div"
                          "extract" "fby" "iff" "implies" "includes" "init"
                          "insert" "inter" "length" "mod" "not" "or"
                          "or_else" "out" "pred" "print" "printf" "replace"
                          "source" "succ" "target" "union" "visible") 'words)
             'font-lock-function-name-face)
      ;; key symbols
      (cons (regexp-opt '("_" "@" "->" "<|" "|>" "..."
                         ) t)
             'font-lock-keyword-face)
   )
   "Keywords used in XTL mode")

;;; ----------------------------------------------------------------------------
;;;                                MODE
;;; ----------------------------------------------------------------------------

(setq aux-file (expand-file-name (concat "~/.emacs.d/lnt/xtl-aux.el")))
(if (file-exists-p aux-file) (load aux-file))

(defun xtl-mode ()
   "Running XTL mode"
   (interactive)
   (kill-all-local-variables)
   (setq major-mode 'xtl-mode)
   (setq mode-name "XTL")
   (set-syntax-table xtl-mode-syntax-table)
   (set (make-local-variable 'comment-start) "(*")
   (set (make-local-variable 'comment-end) "*)")
   (set (make-local-variable 'font-lock-defaults)
        '(xtl-font-lock-keywords nil nil))
   (if (file-exists-p aux-file) (xtl-entry-point))
   (run-hooks 'xtl-mode-hook)
)

(provide 'xtl-mode)
