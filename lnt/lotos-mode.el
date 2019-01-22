;; Emacs mode for LOTOS
;; Maintainer:	Syntax Editor Configuration Tool <cadp@inria.fr>
;; Version:	1.7:LOTOS+1.8:SECT

;;; ----------------------------------------------------------------------------
;;;                                DEFINITIONS
;;; ----------------------------------------------------------------------------

(defvar lotos-mode-syntax-table nil
   "Syntax table used while in LOTOS mode")
(if lotos-mode-syntax-table
      ()
   (setq lotos-mode-syntax-table (make-syntax-table))
   (modify-syntax-entry ?+ "."   lotos-mode-syntax-table)
   (modify-syntax-entry ?- "."   lotos-mode-syntax-table)
   (modify-syntax-entry ?= "."   lotos-mode-syntax-table)
   (modify-syntax-entry ?% "."   lotos-mode-syntax-table)
   (modify-syntax-entry ?< "."   lotos-mode-syntax-table)
   (modify-syntax-entry ?> "."   lotos-mode-syntax-table)
   (modify-syntax-entry ?& "."   lotos-mode-syntax-table)
   (modify-syntax-entry ?| "."   lotos-mode-syntax-table)
   (modify-syntax-entry ?_ "w"   lotos-mode-syntax-table)
   (modify-syntax-entry ?( "() 1"   lotos-mode-syntax-table)
   (modify-syntax-entry ?* ". 23"   lotos-mode-syntax-table)
   (modify-syntax-entry ?) ")( 4"   lotos-mode-syntax-table)
   (modify-syntax-entry ?\" "."   lotos-mode-syntax-table)
)

;;; ----------------------------------------------------------------------------
;;;                                HIGHLIGHTING
;;; ----------------------------------------------------------------------------

(defvar lotos-font-lock-keywords
   (list
      ;; LOTOS pragmas
      (cons (regexp-opt '("atomic" "comparedby" "constructor" "enumeratedby" "external" "implementedby"
                          "iteratedby" "printedby") 'words)
             'font-lock-builtin-face)
      ;; LOTOS keywords
      (cons (regexp-opt '("accept" "actualizedby" "any" "behaviour" "behavior" "choice"
                          "endlib" "endproc" "endspec" "endtype" "eqns" "exit"
                          "for" "forall" "formaleqns" "formalopns" "formalsorts" "hide"
                          "i" "in" "is" "let" "library" "noexit"
                          "of" "ofsort" "opnnames" "opns" "par" "process"
                          "renamedby" "sortnames" "sorts" "specification" "stop" "type"
                          "using" "where") 'words)
             'font-lock-keyword-face)
      ;; LOTOS types
      (cons (regexp-opt '("BasicNaturalNumber" "Bit" "BitNatRepr" "BitString" "Bool" "Boolean"
                          "DecDigit" "DecNatRepr" "DecString" "HexDigit" "HexNatRepr" "HexString"
                          "NatRepresentations" "Nat" "NaturalNumber" "OctDigit" "Octet" "OctetString"
                          "OctNatRepr" "OctString" "Set" "String") 'words)
             'font-lock-type-face)
      ;; LOTOS constants
      (cons (regexp-opt '("false" "true") 'words)
             'font-lock-constant-face)
      ;; LOTOS functions
      (cons (regexp-opt '("and" "Bit1" "Bit2" "Bit3" "Bit4" "Bit5"
                          "Bit6" "Bit7" "Bit8" "Card" "eq" "ge"
                          "gt" "iff" "implies" "Includes" "Insert" "Ints"
                          "IsIn" "IsSubsetOf" "le" "Length" "lt" "Minus"
                          "NatNum" "ne" "not" "NotIn" "or" "Remove"
                          "Reverse" "Succ" "Union" "xor") 'words)
             'font-lock-function-name-face)
      ;; key symbols
      (cons (regexp-opt '("<" ">" "+" "-" "/" "*"
                          "!" "?" ";" "**" "==" "<>"
                          "<=" ">=" "=>" ">>" "[>" "||"
                          "|[" "]|" "[]" "|||") t)
             'font-lock-keyword-face)
   )
   "Keywords used in LOTOS mode")

;;; ----------------------------------------------------------------------------
;;;                                MODE
;;; ----------------------------------------------------------------------------

(setq aux-file (expand-file-name (concat "~/.emacs.d/lnt/lotos-aux.el")))
(if (file-exists-p aux-file) (load aux-file))

(defun lotos-mode ()
   "Running LOTOS mode"
   (interactive)
   (kill-all-local-variables)
   (setq major-mode 'lotos-mode)
   (setq mode-name "LOTOS")
   (set-syntax-table lotos-mode-syntax-table)
   (set (make-local-variable 'comment-start) "(*")
   (set (make-local-variable 'comment-end) "*)")
   (set (make-local-variable 'font-lock-defaults)
        '(lotos-font-lock-keywords nil t))
   (if (file-exists-p aux-file) (lotos-entry-point))
   (run-hooks 'lotos-mode-hook)
)

(provide 'lotos-mode)
