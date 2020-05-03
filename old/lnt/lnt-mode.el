;; Emacs mode for LNT
;; Maintainer:	Syntax Editor Configuration Tool <cadp@inria.fr>
;; Version:	1.20:LNT+1.8:SECT

;;; ----------------------------------------------------------------------------
;;;                                DEFINITIONS
;;; ----------------------------------------------------------------------------

(defvar lnt-mode-syntax-table nil
   "Syntax table used while in LNT mode")
(if lnt-mode-syntax-table
      ()
   (setq lnt-mode-syntax-table (make-syntax-table))
   (modify-syntax-entry ?+ "."   lnt-mode-syntax-table)
   (modify-syntax-entry ?= "."   lnt-mode-syntax-table)
   (modify-syntax-entry ?% "."   lnt-mode-syntax-table)
   (modify-syntax-entry ?< "."   lnt-mode-syntax-table)
   (modify-syntax-entry ?> "."   lnt-mode-syntax-table)
   (modify-syntax-entry ?& "."   lnt-mode-syntax-table)
   (modify-syntax-entry ?| "."   lnt-mode-syntax-table)
   (modify-syntax-entry ?_ "w"   lnt-mode-syntax-table)
   (modify-syntax-entry ?( "() 1"   lnt-mode-syntax-table)
   (modify-syntax-entry ?* ". 23"   lnt-mode-syntax-table)
   (modify-syntax-entry ?) ")( 4"   lnt-mode-syntax-table)
   (modify-syntax-entry ?- ". 12b"   lnt-mode-syntax-table)
   (modify-syntax-entry ?! "w"   lnt-mode-syntax-table)
   (modify-syntax-entry ?\n "> b"   lnt-mode-syntax-table)
   (modify-syntax-entry ?\' "\"'"   lnt-mode-syntax-table)
)

;;; ----------------------------------------------------------------------------
;;;                                HIGHLIGHTING
;;; ----------------------------------------------------------------------------

(defvar lnt-font-lock-keywords
   (list
      ;; line comments
      (cons "--.*$" 'font-lock-comment-face)
      ;; LNT pragmas
      (cons (regexp-opt '("!bits" "!card" "!nat_bits" "!nat_inf" "!nat_sup" "!nat_check"
                          "!int_bits" "!int_inf" "!int_sup" "!int_check" "!string_card" "!comparedby"
                          "!external" "!implementedby" "!iteratedby" "!printedby" "!representedby") 'words)
             'font-lock-builtin-face)
      ;; LNT keywords
      (cons (regexp-opt '("any" "array" "as" "assert" "break" "by"
                          "case" "channel" "disrupt" "else" "elsif" "end"
                          "eval" "for" "function" "hide" "if" "in"
                          "is" "list" "loop" "module" "null" "of"
                          "only" "out" "par" "process" "raise" "range"
                          "return" "select" "set" "sorted" "stop" "then"
                          "type" "use" "var" "where" "while" "with"
                         ) 'words)
             'font-lock-keyword-face)
      ;; LNT types
      (cons (regexp-opt '("bool" "nat" "int" "real" "char" "string"
                         ) 'words)
             'font-lock-type-face)
      ;; LNT constants
      (cons (regexp-opt '("false" "true") 'words)
             'font-lock-constant-face)
      ;; LNT functions
      (cons (regexp-opt '("abs" "and" "and_then" "card" "div" "eq"
                          "first" "gcd" "ge" "get" "gt" "implies"
                          "index" "IntToNat" "is_empty" "IsLower" "IsUpper" "IsAlpha"
                          "IsAlnum" "IsDigit" "IsXDigit" "last" "le" "length"
                          "lt" "max" "min" "mod" "NatToInt" "ne"
                          "neg" "not" "nth" "or" "ord" "or_else"
                          "pos" "pred" "prefix" "rem" "rindex" "scm"
                          "set" "sign" "substr" "succ" "suffix" "ToLower"
                          "ToUpper" "update" "val" "xor") 'words)
             'font-lock-function-name-face)
      ;; key symbols
      (cons (regexp-opt '("=" "<" ">" "|" "+" "-"
                          "/" "*" "?" ";" "**" "=="
                          "<>" "/=" "<=" ">=" "=>" "||"
                          "[]" "->") t)
             'font-lock-keyword-face)
   )
   "Keywords used in LNT mode")

;;; ----------------------------------------------------------------------------
;;;                                MODE
;;; ----------------------------------------------------------------------------

(setq aux-file (expand-file-name (concat "~/.emacs.d/lnt/lnt-aux.el")))
(if (file-exists-p aux-file) (load aux-file))

(defun lnt-mode ()
   "Running LNT mode"
   (interactive)
   (kill-all-local-variables)
   (setq major-mode 'lnt-mode)
   (setq mode-name "LNT")
   (set-syntax-table lnt-mode-syntax-table)
   (set (make-local-variable 'comment-start) "(*")
   (set (make-local-variable 'comment-end) "*)")
   (set (make-local-variable 'font-lock-defaults)
        '(lnt-font-lock-keywords nil nil))
   (if (file-exists-p aux-file) (lnt-entry-point))
   (run-hooks 'lnt-mode-hook)
)

(provide 'lnt-mode)
