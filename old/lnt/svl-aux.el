;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Fichier     : svl-aux.el
;;; Auteur      : Eric Leo
;;; Version     : 1.5
;;; Description : add-on for SVL mode for CADP
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; The SVL, shell and MCL keywords are displayed in font-lock-keywords-face
;;; The comment are displayed in font-lock-coment-face
;;; The starter of shell line is displayed in font-lock-builtin-face
;;; The specific SVL variables are displayed in font-lock-constant-face
;;; The specific MCL constants are displayed in font-lock-constant-face
;;; The CADP filenames are displayed in font-lock-variable-name-face
;;; The actual strings are displayed in font-lock-string-face
;;; The predefined types are displayed in font-lock-type-face
;;;
;;; Here are key sequences and corresponding commands:
;;;
;;; NORMAL COMMANDS:
;;;
;;; C-c h         svl-add-header
;;;   Add an SCCS header
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; ----------------------------------------------------------------------------
;;;                                DEFINITIONS
;;; ----------------------------------------------------------------------------

(defconst svl-mode-version "1.5"
   "Version of this svl mode")

(defvar svl-mode-map ()
   "Keymap used in svl mode")
(if svl-mode-map
     ()
   (setq svl-mode-map (make-sparse-keymap))
   (define-key svl-mode-map "\C-ch" 'svl-add-header)
)

;;; ----------------------------------------------------------------------------
;;;                                FUNCTIONS
;;; ----------------------------------------------------------------------------

(defun svl-add-header ()
   "Add an SCCS header"
   (interactive)
   (insert "(*---------------------------------------------------------------------------"
           "\n * Module      : \%M\%"
           "\n * Created     : " (current-time-string)
           "\n * Author(s)   : " (user-full-name)
           "\n * Version     : \%R\%.\%L\%"
           "\n * Date        : \%E\% \%U\%"
           "\n * Description : describe module here"
           "\n *---------------------------------------------------------------------------*)\n"))

;;; ----------------------------------------------------------------------------
;;;                                KEYWORDS
;;; ----------------------------------------------------------------------------

(defvar svl-font-lock-keywords-aux
   (list
      ;; 1st step comments : shell comments style
      (cons "\\(#.*$\\)" '(1 font-lock-comment-face))
      ;; constant keywords (only at the beginning of shell lines)
      (cons (concat "\\(% *\\)\\("
      ;; default variables
                    (regexp-opt '("DEFAULT_REDUCTION_TOOL"
                                  "DEFAULT_COMPARISON_TOOL"
                                  "DEFAULT_VERIFY_TOOL"
                                  "DEFAULT_DEADLOCK_TOOL"
                                  "DEFAULT_LIVELOCK_TOOL"
                                  "DEFAULT_REDUCTION_METHOD"
                                  "DEFAULT_COMPARISON_METHOD"
                                  "DEFAULT_VERIFY_METHOD"
                                  "DEFAULT_REDUCTION_RELATION"
                                  "DEFAULT_COMPARISON_RELATION"
                                  "DEFAULT_PROCESS_FILE"
                                  "DEFAULT_SMART_LIMIT"
                                  "DEFAULT_MCL_LIBRARIES"
                                  "DEFAULT_XTL_LIBRARIES"
      ;; tools options
                                  "ALDEBARAN_OPTIONS"
                                  "BCG_CMP_OPTIONS"
                                  "BCG_GRAPH_OPTIONS"
                                  "BCG_IO_OPTIONS_INPUT"
                                  "BCG_IO_OPTIONS_OUTPUT"
                                  "BCG_LABELS_OPTIONS"
                                  "BCG_MIN_OPTIONS"
                                  "BCG_OPEN_OPTIONS"
                                  "BCG_OPEN_CC_OPTIONS"
                                  "CAESAR_ADT_OPTIONS"
                                  "CAESAR_OPEN_OPTIONS"
                                  "CAESAR_OPEN_CC_OPTIONS"
                                  "CAESAR_OPTIONS"
                                  "EVALUATOR_OPTIONS"
                                  "EVALUATOR4_OPTIONS"
                                  "EXHIBITOR_OPTIONS"
                                  "EXP_OPEN_OPTIONS"
                                  "EXP_OPEN_CC_OPTIONS"
                                  "FSP_OPEN_OPTIONS"
                                  "FSP_OPEN_CC_OPTIONS"
                                  "GENERATOR_OPTIONS"
                                  "LNT_OPEN_OPTIONS"
                                  "LNT_OPEN_CC_OPTIONS"
                                  "PROJECTOR_OPTIONS"
                                  "REDUCTOR_OPTIONS"
                                  "SEQ_OPEN_OPTIONS"
                                  "SEQ_OPEN_CC_OPTIONS"
                                  "XTL_OPTIONS"
      ;; executables
                                  "ALDEBARAN_EXECUTABLE"
                                  "BCG_CMP_EXECUTABLE"
                                  "BCG_GRAPH_EXECUTABLE"
                                  "BCG_IO_EXECUTABLE"
                                  "BCG_LABELS_EXECUTABLE"
                                  "BCG_MIN_EXECUTABLE"
                                  "BCG_OPEN_EXECUTABLE"
                                  "BISIMULATOR_EXECUTABLE"
                                  "CAESAR_ADT_EXECUTABLE"
                                  "CAESAR_EXECUTABLE"
                                  "CAESAR_OPEN_EXECUTABLE"
                                  "EVALUATOR_EXECUTABLE"
                                  "EVALUATOR4_EXECUTABLE"
                                  "EXHIBITOR_EXECUTABLE"
                                  "EXP_OPEN_EXECUTABLE"
                                  "FSP_OPEN_EXECUTABLE"
                                  "GENERATOR_EXECUTABLE"
                                  "LNT_OPEN_EXECUTABLE"
                                  "PROJECTOR_EXECUTABLE"
                                  "REDUCTOR_EXECUTABLE"
                                  "SEQ_OPEN_EXECUTABLE"
                                  "XTL_EXECUTABLE"
      ;; properties options
                                  "PROPERTY_DISPLAY_MODE"
                                  "PROPERTY_COMMENT_OPEN"
                                  "PROPERTY_COMMENT_MIDDLE"
                                  "PROPERTY_COMMENT_CLOSE") t)
                    "\\)")
         '((1 font-lock-builtin-face)
           (2 font-lock-constant-face)))

      ;; called filenames (overrides standard strings)
      (cons (concat "\"\\([^\"]*[.]"
                    (regexp-opt '("aut" "bcg" "fc2" "seq" "lnt" "lotos" "lot"
                                  "lts" "exp" "hide" "hid" "cut" "rename"
                                  "ren" "sync" "mcl" "xtl") t)
                    "\\)\"") '(1 font-lock-variable-name-face))

      ;; strings
      (cons "\"[^\"]*\"" 'font-lock-string-face)
      (cons "\'[^\']*\'" 'font-lock-string-face)

      ;; specific shell keywords
      ;; it does not check that keywords are used on a shell line
      (cons (regexp-opt '("!" "break" "case" "continue" "do" "done" "elif"
                          "else" "esac" "exec" "exit" "fi" "for" "function"
                          "if" "in" "return" "then" "trap" "type" "until"
                          "while") 'words)
         'font-lock-keyword-face)

      ;; shell lines (from % to # or end of line)
      ;; fontify only the first symbol (%)
      (cons "\\(^ *%\\)\\([^#\n]*\\)" '(1 font-lock-builtin-face))

   )
   "Additional keywords used in svl mode")

;; load mcl mode file
(setq mcl-file (expand-file-name (concat "~/.emacs.d/lnt/mcl-mode.el")))
(if (file-exists-p mcl-file) (load mcl-file))

(defvar svl-font-lock-keywords-all
   (append
      ;; optional list
      svl-font-lock-keywords-aux
      ;; mcl list
      mcl-font-lock-keywords
      ;; default list
      svl-font-lock-keywords
   )
   "Keywords used in SVL mode")


;;; ----------------------------------------------------------------------------
;;;                                MODE
;;; ----------------------------------------------------------------------------

(defun svl-entry-point ()
   ""
   (use-local-map svl-mode-map)
   ;; remove strings (" and ') and comment line from syntax table
   (if svl-mode-syntax-table
      (progn
         (modify-syntax-entry ?\" "."   svl-mode-syntax-table)
         (modify-syntax-entry ?\' "."   svl-mode-syntax-table)))
   ;; set new syntax table
   (set-syntax-table svl-mode-syntax-table)
   ;; set new font lock variable
   (set (make-local-variable 'font-lock-defaults)
      '(svl-font-lock-keywords-all nil nil))
)
