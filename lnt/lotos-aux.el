
;;; --------------------------------------------------------------------------
;;; LOTOS mode, based on text and C modes.
;;; Copyright (C) 1997 2015 INRIA
;;; Contributed by Mihaela Sighireanu <Mihaela.Sighireanu@inria.fr>
;;; Modified by Eric Leo <Eric.Leo@inria.fr>
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License as
;;; published by the Free Software Foundation; either version 1, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;;; General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to the
;;; Free Software Foundation, 675 Mass Ave, Cambridge, MA 02139, USA.
;;;
;;; -------------------------------- CONTENTS --------------------------------
;;;
;;; lotos-mode: Major mode for editing LOTOS specifications.
;;; Revision: 1.9 - Thu Mar 26 17:03:12 CET 2015
;;;
;;; ------------------------------ INSTRUCTIONS ------------------------------
;;;
;;; This file is included from $CADP/ext/emacs/lotos-mode.el, which performs
;;; syntax-based coloring and is itself included from $CADP/ext/emacs/cadp.el
;;; See file $CADP/ext/emacs/READ_ME for instructions.
;;;
;;; You are assumed to be at least somewhat familiar with the LOTOS
;;; syntax. If you aren't, read about it first (see below).
;;;
;;; Here are key sequences and corresponding commands:
;;;
;;; NORMAL COMMANDS:
;;;
;;; C-c c         lotos-add-comment
;;;   Add a LOTOS comment.
;;;
;;; C-c h         lotos-add-header
;;;   Add an SCCS header.
;;;
;;; C-c C-s       lotos-add-specification
;;;   Introduce a frame for a LOTOS specification. 
;;;   You will be prompted for specification gates, and parameters.
;;;
;;; C-c l         lotos-add-libraries
;;;   Add a library declaration. 
;;;   You will be prompted for the complete path of libraries.
;;;
;;; C-c e         lotos-add-eqns
;;;   Introduces a frame for ACT ONE equation.
;;;   You will be prompted for the sort of the equation.
;;;
;;; C-c o         lotos-add-opns
;;;   Add a list of operations having the same profile. 
;;;   You will be prompted for the CADP comment (if needed), 
;;;   for the list of argument'sorts, and for the result sort.
;;;
;;; C-c s         lotos-add-sort
;;;   Add a list of sort declarations. 
;;;   You will be prompted for the name of sorts and 
;;;   for the CADP comment (if needed).
;;;
;;; C-c t         lotos-add-type
;;;   Add a type declaration. 
;;;   You will be prompted for the kind of the type
;;;   (C for composed, A for actualized, R for renamed).
;;;
;;; C-c p         lotos-add-process
;;;   Introduces a frame for LOTOS process specification.
;;;   You will be prompted for the list of gates and the list of 
;;;   formal parameters.
;;;
;;; C-c C-a       lotos-compile-adt
;;;   Save the current buffer and compile the data part of the specification 
;;;   using caesar.adt. 
;;;   You will be prompted for the options of caesar.adt
;;;
;;; C-c C-b       lotos-compile-beh
;;;   Save the current buffer and compile the behaviour part of the  
;;;   specification using caesar. 
;;;   You will be prompted for the options of caesar.
;;;
;;; COMMANDS THAT OPERATE ON THE CURRENT REGION:
;;;
;;; None
;;;
;;; SPECIAL COMMANDS:
;;;
;;; None
;;; ---------------------------- ADDITIONAL NOTES ----------------------------
;;;
;;; If you are running Epoch or Lucid Emacs, highlighting will be used
;;; to deemphasize LOTOS message elements as they are created.  You can
;;; turn this off; see the variables 'lotos-use-highlighting' and
;;; 'lotos-use-font-lock'.
;;;
;;; -------------------------------- GOTCHAS ---------------------------------
;;;
;;; LOTOS specification can be tricky.  lotos-mode is not smart enough to
;;; enforce correctness or sanity, so you have to do that yourself.
;;;
;;; ------------------------- WHAT LOTOS-MODE IS NOT -------------------------
;;;
;;;
;;; ------------------------------ WHAT LOTOS IS -----------------------------
;;;
;;; LOTOS (Language Of Temporal Ordering Specification) is a ISO 8807 
;;; standard language for the formal description of distributed systems.
;;; It use an algebraic language (ACT ONE) to describe data types, and a
;;; process algebra (based on CCS and CSP) to describe process part.
;;; 
;;; For more informations on LOTOS see http://cadp.inria.fr
;;;
;;; ---------------------------- ACKNOWLEDGEMENTS ----------------------------
;;;
;;; Some code herein was inspired by:
;;;   Alain Kerbrat (VERIMAG)
;;;
;;; --------------------------------------------------------------------------
;;; CADP Archive Entry:
;;; lotos-mode|Mihaela Sighireanu|inria.fr
;;; Major mode for editing LOTOS specifications
;;; Date: 10/02/09 15:09:21|Revision: 1.8|~/lib/emacs/lotos-mode.el|
;;; --------------------------------------------------------------------------

;;; ---------------------------- emacs variations ----------------------------

(defvar lotos-running-lemacs (if (string-match "Lucid" emacs-version) t nil)
  "Non-nil if running Lucid Emacs.")

(defvar lotos-running-epoch (boundp 'epoch::version)
  "Non-nil if running Epoch.")

;;; ------------------------------- variables --------------------------------

(defvar lotos-use-highlighting lotos-running-epoch
  "*Flag to use highlighting for LOTOS in Epoch or Lucid Emacs;
if non-NIL, highlighting will be used.  Default is T if you are running
Epoch; nil otherwise (for Lucid Emacs, font-lock is better; see
lotos-use-font-lock instead).")

(defvar lotos-use-font-lock lotos-running-lemacs
  "*Flag to use font-lock for LOTOS keywords in Lucid Emacs.  If non-NIL,
font-lock will be used.  Default is T if you are running with Lucid Emacs;
NIL otherwise.  This doesn't currently seem to work.  Bummer.  Ten points
to the first person who tells me why not.")

(defvar lotos-deemphasize-color "grey80"
  "*Color for de-highlighting LOTOS keywords in Epoch or Lucid Emacs.")

(defvar lotos-emphasize-color "yellow"
  "*Color for highlighting LOTOS something-or-others in Epoch or Lucid Emacs.")

(defvar lotos-sigusr1-signal-value 16
  "*Value for the SIGUSR1 signal on your system.  See, usually,
/usr/include/sys/signal.h.")

;;; --------------------------------- setup ----------------------------------

(defconst lotos-mode-version "1.9"
  "Version of this lotos mode.")

(defvar lotos-mode-abbrev-table nil
  "Abbrev table used while in lotos mode.")
(define-abbrev-table 'lotos-mode-abbrev-table ())

(defvar lotos-mode-map ()
  "Keymap used in Lotos mode.")
(if lotos-mode-map
    ()
  (setq lotos-mode-map (make-sparse-keymap))
  ;; general commands
  (define-key lotos-mode-map "\t" 'lotos-tab)
  (define-key lotos-mode-map "\C-cc" 'lotos-add-comment)
  (define-key lotos-mode-map "\C-ch" 'lotos-add-header)
  ;; Specification
  (define-key lotos-mode-map "\C-c\C-s" 'lotos-add-specification)
  (define-key lotos-mode-map "\C-cl" 'lotos-add-libraries)
  ;; Data part
  (define-key lotos-mode-map "\C-ce" 'lotos-add-eqns)
  (define-key lotos-mode-map "\C-co" 'lotos-add-opns)
  (define-key lotos-mode-map "\C-cs" 'lotos-add-sort)
  (define-key lotos-mode-map "\C-ct" 'lotos-add-type)
  ;; Process part
  (define-key lotos-mode-map "\C-cp" 'lotos-add-process)
  ;; Compilation
  (define-key lotos-mode-map "\C-c\C-a" 'lotos-compile-adt)
  (define-key lotos-mode-map "\C-c\C-b" 'lotos-compile-beh)
  )

;;; ------------------------------ highlighting ------------------------------

(if (and lotos-running-epoch lotos-use-highlighting)
    (progn
      (defvar lotos-deemphasize-style lotos-deemphasize-color)
      (defvar lotos-emphasize-style lotos-emphasize-color)))

(if (and lotos-running-lemacs lotos-use-highlighting)
    (progn
      (defvar lotos-deemphasize-style (make-face 'lotos-deemphasize-face))
      (set-face-foreground lotos-deemphasize-style lotos-deemphasize-color)
      (defvar lotos-emphasize-style (make-face 'lotos-emphasize-face))
      (set-face-foreground lotos-emphasize-style lotos-emphasize-color)))

(defvar lotos-use-font-lock lotos-running-lemacs
  "*Flag to use font-lock for LOTOS keywords in Lucid Emacs.  If non-NIL,
font-lock will be used.  Default is T if you are running with Lucid Emacs;
NIL otherwise.  This doesn't currently seem to work.  Bummer.  Ten points
to the first person who tells me why not.")

(defvar lotos-deemphasize-color "grey80"
  "*Color for de-highlighting LOTOS keywords in Epoch or Lucid Emacs.")

(defvar lotos-emphasize-color "yellow"
  "*Color for highlighting LOTOS something-or-others in Epoch or Lucid Emacs.")

;;; --------------------------------------------------------------------------
;;; ------------------------ command support routines ------------------------
;;; --------------------------------------------------------------------------

(defun lotos-comment (lotos-comment-type)
 "Insert a LOTOS comment (arg 0),
         a CADT comment for a sort (arg 1) or 
         a CADP comment for an operation (arg 2).
  For a sort, the comment is: 
        implementedby comparedby enumeratedby printedby external
  For an operation, the comment is: 
        implementedby constructor external."
 (if (= lotos-comment-type 0) (insert "(* *)")
     (let ((lotos-comment (read-string "[CADT comment] (yes, no): ")))
       (if (not (or (string-equal lotos-comment "no")
		    (string-equal lotos-comment "n")))
	   (if (= lotos-comment-type 1)
	       (insert " (*! implementedby comparedby enumeratedby printedby external *)")
	     (insert " (*! implementedby constructor external *)"))))
     )
)

(defun lotos-get-gate-list ()
 "Read from the user the gate list for process od specification.
Add brackets unless arguments absent, and insert into buffer."
 (let ((lotos-gates (read-string "[gates]: ")))
   (if (string-equal lotos-gates "")
       (backward-delete-char 0)
     (progn
       (while (string-match ",$" lotos-gates)
	 (insert lotos-gates)
	 (setq lotos-gates (read-string "next gate: ")))
       (insert lotos-gates))))
)

(defun lotos-get-param-list ()
 "Read from the user the parameter list for a process or for a specification.
Add parenthesis unless parameters are absent, and insert into buffer."
 (let ((lotos-params (read-string "(parameters): ")))
   (if (string-equal lotos-params "")
       (backward-delete-char 0)
     (progn
       (while (string-match ",$" lotos-params)
	 (insert lotos-params)
	 (setq lotos-params (read-string "next parameter: ")))
       (insert lotos-params))))
)


(defun lotos-get-type-kind ()
 "Insert appropiate elements for a given type (combined, actualized, renamed)."
 (interactive)
 (let ((lotos-type-kind (read-string "type kind (C, A, R): ")))
   (cond
    ((string-equal "A" lotos-type-kind)
     (progn
       (insert "actualizedby  using \n")
       (insert "   sortnames \n")
       (insert "      for \n")
       (insert "   opnnames \n")
       (insert "      for \n")))
    ((string-equal "R" lotos-type-kind)
     (progn
       (insert "renamedby \n")
       (insert "   sortnames \n")
       (insert "      for \n")
       (insert "   opnnames \n")
       (insert "      for \n")))
   ( t
      (progn
	(insert "\n   formalsorts \n")
	(insert "   formalopns \n")
	(insert "   formaleqns forall \n")
	(insert "   sorts \n")
	(insert "   opns \n")
	(insert "   eqns forall \n"))))
   )
)
 
;;; --------------------------------------------------------------------------
;;; -------------------------------- commands --------------------------------
;;; --------------------------------------------------------------------------

;; C-c c
(defun lotos-add-comment ()
  "Add a LOTOS comment."
  (interactive)
  (lotos-comment 0))

;; C-c h
(defun lotos-add-header ()
  "Add an SCCS header."
  (interactive)
  (insert "\n(*****************************************************
           \n Module:     ")
  (insert (read-string "Module: "))
  (insert "\n Created:    " (current-time-string))
  (insert "\n Author(s):  " (user-full-name))
  (insert "\n            <" (user-login-name) "@" (system-name) ">\n")
  (insert "\n Version:   \%R\%.\%L\%")
  (insert "\n Date:      \%E\% \%U\%")
  (insert "\n *****************************************************)\n")
)

;; C-c s
(defun lotos-add-specification ()
 "Introduce a frame for a LOTOS specification. 
You will be prompted for specification gates, and parameters."
 (interactive)
 (insert "\nspecification ")
 (let ((lotos-spec-name (read-string "Specification name: " )))
   (insert lotos-spec-name )
   (insert " [")
   (lotos-get-gate-list)
   (insert "]")
   (insert " (")
   (lotos-get-param-list)
   (insert ")")
   (insert ": noexit \n")
   (insert "   (* Data Part *) \n")
   (insert "   (* Control Part *) \n")
   (insert "behaviour \n")
   (insert "   where \n")
   (insert "endspec (* " lotos-spec-name " *)")
   )
)

;; C-c l
(defun lotos-add-libraries ()
  "Add a library declaration."
  (interactive)
  (insert "\nlibrary\n")
  (let ((lotos-libs (read-string "Library name: ")))
    (if (string-equal lotos-libs "")
	(backward-delete-char 0)
      (progn
	(while (string-match ",$" lotos-libs)
	 (insert "   " lotos-libs "\n")
	 (setq lotos-libs (read-string "next library: ")))
       (insert "   " lotos-libs "\n"))))
  (insert "endlib\n")
)

;; C-c e

(defun lotos-add-eqns ()
 "Introduces a frame for ACT ONE equation.
You will be prompted for the sort of the equation."
 (interactive)
 (let ((lotos-eqns-sort (read-string "Equation sort: ")))
   (progn
     (if (not (string-equal "" lotos-eqns-sort))
       (insert "\n      ofsort " lotos-eqns-sort))
     (insert "\n          => = ;\n")))
)

;; C-c o
(defun lotos-add-opns ()
  "Add a list of operations having the same profile. 
You will be prompted for the CADP comment (if needed), 
for the list of argument'sorts and for the result sort."
  (interactive)
  (let ((lotos-opns (read-string "Operation name: ")))
    (progn
      (while (not (string-equal "" lotos-opns))
	(insert "\n      " lotos-opns)
	(lotos-comment 2)
	(insert ",")
	(setq lotos-opns (read-string "next operation (with same profile): ")))
      (backward-delete-char 1)
      (insert " : ")
      (lotos-get-param-list)
      (insert " -> ")
      (insert (read-string "result sort: "))
      (insert "\n")))
)
  
;; C-c s
(defun lotos-add-sort ()
  "Add a list of sort declarations. 
You will be prompted for the name of sorts and 
for the CADP comment (if needed)."
  (interactive)
 (let ((lotos-sort (read-string "Sort name: ")))
   (progn
     (while (not (string-equal "" lotos-sort))
       (insert "\n      " lotos-sort)
       (lotos-comment 1)
       (insert ", ")
       (setq lotos-sort (read-string "next sort: ")))
     (backward-delete-char 2)))
)

;; C-c t
(defun lotos-add-type ()
  "Add a type declaration. 
You will be prompted for the kind of the type
(C for composed, A for actualized, R for renamed)."
  (interactive)
  (let ((lotos-type-name (read-string "Type name: " )))
    (insert "\ntype ")
    (insert lotos-type-name " is ")
    (lotos-get-type-kind)
    (insert "endtype (* " lotos-type-name " *)\n"))
)
  
;; C-c p
(defun lotos-add-process ()
  "Introduces a frame for LOTOS process specification.
You will be prompted for the list of gates and the list of 
formal parameters."
  (interactive)
  (insert "\n   process ")
  (let ((lotos-process-name (read-string "Process name: " )))
    (insert lotos-process-name )
    (insert " [")
    (lotos-get-gate-list)
    (insert "]")
    (insert " (")
    (lotos-get-param-list)
    (insert ") ")
    (insert ": exit := \n   where \n")
    (insert "\n   endproc (* " lotos-process-name " *)\n"))
)

;; C-c C-a
(defun lotos-compile-adt ()
  "Save the current buffer and compile the data part of the specification 
using caesar.adt. 
You will be prompted for the options of caesar.adt"
  (interactive)
  (let ((lotos-option (read-string "caesar.adt options: " ))
        (lotos-source-file (buffer-name)))
    (compile
     (concat "caesar.adt " lotos-option lotos-source-file))
    )
)
  
;; C-c C-b
(defun lotos-compile-beh ()
  "Save the current buffer and compile the behaviour part of the  
specification using caesar. 
You will be prompted for the options of caesar"
  (interactive)
  (let ((lotos-option (read-string "caesar options: " ))
        (lotos-source-file (buffer-name)))
    (compile
     (concat "caesar " lotos-option lotos-source-file))
    )
)
  
;;; --------------------------------------------------------------------------
;;; ---------------------------- region commands -----------------------------
;;; --------------------------------------------------------------------------

;;; --------------------------------------------------------------------------
;;; ---------------------------- special commands ----------------------------
;;; --------------------------------------------------------------------------

(defun lotos-tab ()
 "Put a tab."
 (interactive)
 (insert "   ")
)

;;; ------------------------------ lotos-mode --------------------------------

(defun lotos-entry-point ()
  "This is a mode intended to support program development in LOTOS 
(using or not CADP).
Most control constructs and declarations of Lotos can be inserted in the 
buffer by typing Control-C followed by a character mnemonic for the construct.

Key bindings:

\\{lotos-mode-map}

 lotos-add-comment
    Add a LOTOS comment.
 lotos-add-header
    Add an SCCS header.
 lotos-add-specification
    Introduce a frame for a LOTOS specification. 
    You will be prompted for specification gates, and parameters.
 lotos-add-libraries
    Add a library declaration.
 lotos-add-eqns
    Introduces a frame for ACT ONE equation.
    You will be prompted for the sort of the equation.
 lotos-add-opns
    Add a list of operations having the same profile. 
    You will be prompted for the CADP comment (if needed), 
    for the list of argument'sorts and for the result sort.
 lotos-add-sort
    Add a list of sort declarations. 
    You will be prompted for the name of sorts and 
    for the CADP comment (if needed).
 lotos-add-type
    Add a type declaration. 
    You will be prompted for the kind of the type
    (C for composed, A for actualized, R for renamed).
 lotos-add-process
    Introduces a frame for LOTOS process specification.
    You will be prompted for the list of gates and the list of 
    formal parameters.
 lotos-compile-adt
    Save the current buffer and compile the data part of the specification 
    using caesar.adt. 
    You will be prompted for the options of caesar.adt
 lotos-compile-beh
    Save the current buffer and compile the behaviour part of the  
    specification using caesar. 
    You will be prompted for the options of caesar.
   
Please report bugs, comments, and enhancements by e-mail to cadp@inria.fr

"
  (use-local-map lotos-mode-map)
  (setq local-abbrev-table lotos-mode-abbrev-table)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments nil)
  (make-local-variable 'comment-column)
  (setq comment-column 32)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "(\\*+ *")
  )

; lotos-aux.el ends here


