;;; --------------------------------------------------------------------------
;;; LNT mode, based on LOTOS mode 1.7 by M. Sighireanu
;;; Copyright (C) 2010 2015 INRIA
;;; Contributed by Frederic Lang <Frederic.Lang@inria.fr>
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
;;; lnt-mode: Major mode for editing LNT specifications.
;;; Revision: 1.45
;;;
;;; ------------------------------ INSTRUCTIONS ------------------------------
;;;
;;; This file is included from $CADP/ext/emacs/lnt-mode.el, which performs
;;; syntax-based coloring and is itself included from $CADP/ext/emacs/cadp.el
;;; See file $CADP/ext/emacs/READ_ME for instructions.
;;;
;;; You are assumed to be at least somewhat familiar with the LNT
;;; syntax. If you aren't, read about it first (see below).
;;;
;;; Here are key sequences and corresponding commands:
;;;
;;; NORMAL COMMANDS:
;;;
;;; C-c h         lnt-add-header
;;;   Add an SCCS header.
;;;
;;; C-c m         lnt-add-module
;;;   Add a new module.
;;;   You will be prompted for module name.
;;;
;;; C-c t         lnt-add-type
;;;   Add a new type.
;;;   You will be prompted for type name.
;;;
;;; C-c f         lnt-add-function
;;;   Add a new function.
;;;   You will be prompted for function name.
;;;
;;; C-c p         lnt-add-process
;;;   Add a new process.
;;;   You will be prompted for process name.
;;;
;;; C-c c         lnt-add-case
;;;   Add a new case statement.
;;;
;;; C-c w         lnt-add-while
;;;   Add a new while statement.
;;;
;;; C-c i         lnt-add-if
;;;   Add a new if-then-elsif-else statement.
;;;
;;; C-c o         lnt-add-only-if
;;;   Add a new only-if-then-elsif statement.
;;;
;;; C-c v         lnt-add-var
;;;   Add a new variable declaration statement.
;;;
;;; C-c s         lnt-add-sep
;;;   Add a new definition separator
;;;
;;; C-c n         lnt-add-channel
;;;   Add a new channel
;;;   You will be prompt for channel name.
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
;;; LNT specification can be tricky.  lnt-mode is not smart enough 
;;; to enforce correctness or sanity, so you have to do that yourself.
;;;
;; --------------------------------------------------------------------------

;;; ---------------------------- emacs variations ----------------------------

(defvar lnt-running-lemacs (if (string-match "Lucid" emacs-version) t nil)
  "Non-nil if running Lucid Emacs.")

(defvar lnt-running-epoch (boundp 'epoch::version)
  "Non-nil if running Epoch.")

;;; ------------------------------- variables --------------------------------

(defvar lnt-use-highlighting lnt-running-epoch
  "*Flag to use highlighting for LNT in Epoch or Lucid Emacs;
if non-NIL, highlighting will be used.  Default is T if you are running
Epoch; nil otherwise (for Lucid Emacs, font-lock is better; see
lnt-use-font-lock instead).")

(defvar lnt-use-font-lock lnt-running-lemacs
  "*Flag to use font-lock for LNT keywords in Lucid Emacs.  If non-NIL,
font-lock will be used.  Default is T if you are running with Lucid Emacs;
NIL otherwise.  This doesn't currently seem to work.  Bummer.  Ten points
to the first person who tells me why not.")

(defvar lnt-deemphasize-color "grey80"
  "*Color for de-highlighting LNT keywords in Epoch or Lucid Emacs.")

(defvar lnt-emphasize-color "yellow"
  "*Color for highlighting LNT something-or-others in Epoch or Lucid Emacs.")

(defvar lnt-sigusr1-signal-value 16
  "*Value for the SIGUSR1 signal on your system.  See, usually,
/usr/include/sys/signal.h.")

;;; --------------------------------- setup ----------------------------------

(defconst lnt-mode-version "1.45"
  "Version of this lnt mode.")

(defvar lnt-mode-abbrev-table nil
  "Abbrev table used while in lnt mode.")

(define-abbrev-table 'lnt-mode-abbrev-table ())

(defvar lnt-mode-map ()
  "Keymap used in lnt mode.")
(if lnt-mode-map
    ()
  (setq lnt-mode-map (make-sparse-keymap))
  (define-key lnt-mode-map "\t" 'lnt-indent-command)
  (define-key lnt-mode-map [ return ] 'lnt-newline)
  (define-key lnt-mode-map "\C-ch" 'lnt-add-header)
  (define-key lnt-mode-map "\C-cm" 'lnt-add-module)
  (define-key lnt-mode-map "\C-ct" 'lnt-add-type)
  (define-key lnt-mode-map "\C-cf" 'lnt-add-function)
  (define-key lnt-mode-map "\C-cp" 'lnt-add-process)
  (define-key lnt-mode-map "\C-cc" 'lnt-add-case)
  (define-key lnt-mode-map "\C-cw" 'lnt-add-while)
  (define-key lnt-mode-map "\C-ci" 'lnt-add-if)
  (define-key lnt-mode-map "\C-co" 'lnt-add-only-if)
  (define-key lnt-mode-map "\C-cv" 'lnt-add-var)
  (define-key lnt-mode-map "\C-cs" 'lnt-add-sep)
  (define-key lnt-mode-map "\C-cn" 'lnt-add-channel)
)

;;; --------------------------------------------------------------------------
;;; ------------------------ command support routines ------------------------
;;; --------------------------------------------------------------------------

(defun lnt-add-sep ()
  "Add a new definition separator."
  (interactive)
  (insert "-------------------------------------------------------------------------------\n\n"))

(defun lnt-add-var ()
  "Add a new local variable declaration."
  (interactive)
  (insert "var")
  (lnt-indent-command)
  (insert "\n")
  (lnt-indent-command)
  (let ((save-point (point)))
    (insert "\nin")
    (lnt-indent-command)
    (insert "\n\nend var")
    (lnt-indent-command)
    (goto-char save-point)))

(defun lnt-add-if ()
  "Add a new if-then-elsif-else statement."
  (interactive)
  (insert "if ")
  (lnt-indent-command)
  (let ((save-point (point)))
    (insert " then\n\nelsif then")
    (lnt-indent-command)
    (insert "\n\nelse")
    (lnt-indent-command)
    (insert "\n\nend if")
    (lnt-indent-command)
    (goto-char save-point)))

(defun lnt-add-only-if ()
  "Add a new only-if-then-elsif statement."
  (interactive)
  (insert "only if ")
  (lnt-indent-command)
  (let ((save-point (point)))
    (insert " then\n\nelsif then")
    (lnt-indent-command)
    (insert "\n\nend if")
    (lnt-indent-command)
    (goto-char save-point)))

(defun lnt-add-while ()
  "Add a new while statement."
  (interactive)
  (insert "while ")
  (lnt-indent-command)
  (let ((save-point (point)))
    (insert " do\n\nend while")
    (lnt-indent-command)
    (goto-char save-point)))

(defun lnt-add-case ()
  "Add a new case statement."
  (interactive)
  (insert "case ")
  (lnt-indent-command)
  (let ((save-point (point)))
    (insert " is\nvar")
    (lnt-indent-command)
    (insert "\n\nin")
    (lnt-indent-command)
    (insert "\n  ->")
    (lnt-indent-command)
    (insert "\n\n| ->")
    (lnt-indent-command)
    (insert "\n\nend case")
    (lnt-indent-command)
    (goto-char save-point)))

(defun lnt-add-process ()
  "Add a new process."
  (interactive)
  (let ((lnt-process-name (read-string "Process name: ")))
    (insert "process " 
	    lnt-process-name 
	    " [")
    (let ((save-point (point)))
      (insert "]")
      (if (not (equal "MAIN" (upcase lnt-process-name)))
	  (insert " ()"))
      (insert " is\n\nend process -- "
	      lnt-process-name
	      "\n")
      (goto-char save-point))))

(defun lnt-add-function ()
  "Add a new function."
  (interactive)
  (let ((lnt-function-name (read-string "Function name: ")))
  (insert "function " 
	  lnt-function-name 
	  " (")
  (let ((save-point (point)))
    (insert ") is\n\nend function -- "
	    lnt-function-name
	    "\n")
    (goto-char save-point))))

(defun lnt-add-type ()
  "Add a new type."
  (interactive)
  (let ((lnt-type-name (read-string "Type name: ")))
  (insert "type " 
	  lnt-type-name 
	  " is")
  (lnt-indent-command)
  (insert "\n")
  (lnt-indent-command)
  (let ((save-point (point)))
    (insert "\nend type -- "
	    lnt-type-name
	    "\n")
    (goto-char save-point))))

(defun lnt-add-module ()
  "Add a new module."
  (interactive)
  (let* ((lnt-default-module-name (substring (buffer-name) 0 -4)) ;; remove ".lnt" at the end of the buffer name
         (lnt-read-module-name (read-string (concat "Module name (" lnt-default-module-name "): ")))
         (lnt-module-name (if (equal "" lnt-read-module-name)
                                  lnt-default-module-name
                                lnt-read-module-name)))
    (insert "module "
	    lnt-module-name
	    " is")
    (lnt-indent-command)
    (insert "\n")
    (lnt-indent-command)
    (let ((save-point (point)))
      (insert "\nend module -- "
	      lnt-module-name
	      "\n")
      (goto-char save-point))))

(defun lnt-add-channel ()
  "Add a new channel."
  (interactive)
  (let ((lnt-channel-name (read-string "Channel name: ")))
  (insert "channel "
          lnt-channel-name
          " is (")
  (lnt-indent-command)
  (let ((save-point (point)))
    (insert ") end channel -- "
            lnt-channel-name
            "\n")
    (goto-char save-point))))

(defun lnt-add-header ()
  "Add an SCCS header."
  (interactive)
  (let ((lnt-banner (read-string "Banner: ")))
    (insert "(*---------------------------------------------------------------------------\n * "
	    lnt-banner
	    "\n *---------------------------------------------------------------------------"
	    "\n * Module    : \%M\%"
	    "\n * Created   : " (current-time-string)
            "\n * Author(s) : " (user-full-name)
	    "\n *             <" (user-login-name) "@" (system-name) ">"
	    "\n * Version   : \%R\%.\%L\%"
	    "\n * Date      : \%E\% \%U\%"
	    "\n *---------------------------------------------------------------------------*)\n")))

(defun lnt-newline ()
  "Put a newline and indent previous and current line."
  (interactive)
  (lnt-indent-command)
  (newline)
  (lnt-indent-command)
)

(defcustom lnt-tab-always-indent t
  "*Non-nil means TAB in LNT mode should always reindent the current 
   line, regardless of where in the line point is when the TAB command is 
   used."
  :type 'boolean
  :group 'old-c)

(defvar lnt-base-indent 3
  "Basic indentation")

(defun lnt-indent-command (&optional whole-exp)
  "Indent current line as LNT code, or in some cases insert a tab 
   character. If `lnt-tab-always-indent' is non-nil (the default), 
   always indent current line. Otherwise, indent the current line only 
   if point is at the left margin or in the line's indentation; otherwise
   insert a tab.

   A numeric argument, regardless of its value, means indent rigidly all 
   the lines of the expression starting after point so that this line becomes
   properly indented.  The relative indentation among the lines of the
   expression are preserved."
  (interactive "P")
  (if whole-exp
      ;; If arg, always indent this line as C
      ;; and shift remaining lines of expression the same amount.
      (let ((shift-amt (lnt-indent-line))
	    beg end)
	(save-excursion
	  (if lnt-tab-always-indent
	      (beginning-of-line))
	  ;; Find beginning of following line.
	  (save-excursion
	    (forward-line 1) (setq beg (point)))
	  ;; Find first beginning-of-sexp for sexp extending past this line.
	  (while (< (point) beg)
	    (forward-sexp 1)
	    (setq end (point))
	    (skip-chars-forward " \t\n")))
	(if (> end beg)
	    (indent-code-rigidly beg end shift-amt "#")))
      (if (and (not lnt-tab-always-indent)
	     (save-excursion
	       (skip-chars-backward " \t")
	       (not (bolp))))
	(insert-tab)
        (lnt-indent-line))))

(defun lnt-indent-line ()
  "Indent current line with same indentation as previous non-empty 
   line. Return the amount the indentation changed by."
  (let ((indent (calculate-lnt-indent nil))
	beg shift-amt
	(case-fold-search nil)
	(pos (- (point-max) (point))))
    (beginning-of-line)
    (setq beg (point))
    (skip-chars-forward " \t")
    (setq shift-amt (- indent (current-column)))
    (if (zerop shift-amt)
	(if (> (- (point-max) pos) (point))
	    (goto-char (- (point-max) pos)))
      (delete-region beg (point))
      (indent-to indent)
      ;; If initial point was within line's indentation,
      ;; position after the indentation.  Else stay at same point in text.
      (if (> (- (point-max) pos) (point))
	  (goto-char (- (point-max) pos))))
    shift-amt))

(defun lnt-calculate-alt-indent (last-indent)
  "Return appropriate indentation for current line starting with '|'."
  (let ((save-point (point)))
    (save-excursion
      (if (lnt-word-search-backward-out-of-end-comment-string "case")
	  (let ((case-indent (current-column)))
	    (if (lnt-word-search-forward-out-of-end-comment-string "end")
		(if (> (point) save-point)
		    case-indent
		  (let nil
		    (goto-char save-point)
		    (if (lnt-previous-contains "|")
			last-indent
		      (- last-indent (* 2 lnt-base-indent)))))
	      case-indent))
	last-indent))))

(defun calculate-lnt-indent (&optional parse-start)
  "Return appropriate indentation for current line as LNT code."
  (let ((last-indent (save-excursion (calculate-lnt-last-indent))))
    (save-excursion
      (beginning-of-line)
      (cond ((lnt-in-comment)
	     (calculate-lnt-indent-in-comment))
	    ((lnt-in-parentheses)
	     (calculate-lnt-indent-in-parentheses))
	    ((lnt-out-definition) 0)
	    (t (let nil
		 (skip-chars-forward " \t")
		 (cond ((looking-at "end\\>")
			(let nil
			  (cond ((save-excursion
				   (skip-chars-forward "end")
				   (lnt-skip-comments)
				   (looking-at-in
				    '(
				      ;; common to Traian and Lnt2Lotos
				      "function\\>"
				      "module\\>" 
				      "process\\>" 
				      "type\\>"
				      ;; specific to Lnt2Lotos
				      "channel\\>"
				      ;; specific to Traian
				      ;;@ "behaviour\\>"
				      ;;@ "interface\\>"
				      ;;@ "library\\>"
				      ;;@ "specification\\>"
				      ))) 0)
				((save-excursion
				   (skip-chars-forward "end")
				   (lnt-skip-comments)
				   (looking-at "case\\>")) 
				 (calculate-lnt-keep-indent-if-previous
				  (* 2 lnt-base-indent)
				  '("|" "case\\>" "is\\>" "in\\>")))
				(t 
				 (calculate-lnt-keep-indent-if-previous 
				  lnt-base-indent
				  '(
				    ;; common to Traian and Lnt2Lotos
				    "case\\>"
				    "else\\>"
				    "elsif\\>"
				    "function\\>"
				    "hide\\>"
				    "if\\>"
				    "in\\>"
				    "is\\>" 
				    "loop\\>"
				    "module\\>"
				    "par\\>"
				    "process\>>"
				    "select\\>" 
				    "type\\>"
				    "var\>>"
				    "while\\>"
				    "|"
				    ;; specific to Lnt2Lotos
				    "channel\\>"
				    "only\\>"
				    ;; specific to Traian
				    ;;@ "behaviour\\>"
				    ;;@ "interface\\>"
				    ;;@ "library\\>"
				    ;;@ "opns\\>"
				    ;;@ "rename\\>"
				    ;;@ "specification\\>"
				    ;;@ "trap\\>"
				    ))))))
		       ((looking-at-in 
			 '(
			   ;; common to Traian and Lnt2Lotos
			   "function\\>"
			   "module\\>"
			   "process\\>"
			   "type\\>"
			   ;; specific to Lnt2Lotos
			   "channel\\>"
			   ;; specific to Traian
			   ;;@ "behaviour\\>"
			   ;;@ "interface\\>"
			   ;;@ "library\\>"
			   ;;@ "opns\\>"
			   ;;@ "specification\\>"
			   )) 0)
		       ((looking-at "\\[\\]")
			(calculate-lnt-keep-indent-if-previous
			 lnt-base-indent
			 '("\\[\\]" "select")))
		       ((looking-at "([^\\*]")
			last-indent)
		       ((looking-at "in\\>")
			(calculate-lnt-keep-indent-if-previous 
			 lnt-base-indent
			 '(
			   ;; common to Traian and Lnt2Lotos
			   "var\\>"
			   ;; specific to Traian
			   ;;@ "trap\\>"
			   )))
		       ((or (looking-at "else\\>") 
			    (looking-at "elsif\\>"))
			(calculate-lnt-keep-indent-if-previous
			 lnt-base-indent
			 '(
			   ;; common to Traian and Lnt2Lotos
			   "if\\>"
			   "elsif\\>"
			   ;; specific to Lnt2Lotos
			   "only\\>"
			  )))
		       ((looking-at "|")
			(lnt-calculate-alt-indent last-indent))
		       ((looking-at "!")
			last-indent)
		       ((and (or (looking-at "(\\*") (looking-at "--")) (lnt-no-previous-line))
			last-indent)
		       ((lnt-var-of-case)
			last-indent)
		       ((or (lnt-previous-terminate ";")
			    (lnt-previous-terminate ","))
			last-indent)
		       ((lnt-previous-contains "->")
			(let nil
			  (calculate-lnt-last-indent)
			  (if (looking-at "|")
			      (+ last-indent (* 2 lnt-base-indent))
			    (+ last-indent lnt-base-indent))))
		       ((looking-at "$") last-indent)
		       (t (+ last-indent lnt-base-indent)))))))))

(defun lnt-previous-terminate (regexp)
  "Return t if the previous nonempty and noncomment line terminates with
   regexp (skipping blanks, comments, and keywords following end)"
  (save-excursion
    (beginning-of-line)
    (let ((save-point (point)))
      (if (lnt-search-backward-regexp-out-of-end-comment-string regexp)
	  (let nil
	    (re-search-forward regexp nil t 1)
	    (lnt-skip-comments)
	    (>= (point) save-point))
	nil))))

(defun lnt-previous-end (regexp p)
  "Return t if the previous nonempty and noncomment line ends with regexp
   and the matching string is found at a position different from p (skipping
   blanks and comments)"
  (save-excursion
    (beginning-of-line)
    (let ((save-point (point)))
      (if (lnt-search-backward-regexp-out-of-comment-string regexp)
	  (let ((p2 (point)))
	    (re-search-forward regexp nil t 1)
	    (lnt-skip-comments)
	    (and (>= (point) save-point) (not (= p p2))))
	nil))))

(defun lnt-previous-contains (regexp)
  "Return t if the previous nonempty and noncomment line contains
   regexp (skipping blanks and comments)"
  (save-excursion
    (beginning-of-line)
    (let ((save-point (point)))
      (if (lnt-search-backward-regexp-out-of-end-comment-string regexp)
	  (let nil
	    (re-search-forward "$")
	    (lnt-skip-comments)
	    (>= (point) save-point))
	nil))))

(defun lnt-no-previous-line ()
  "Return t if there is no nonempty and nocomment line preceding the current
   line (skipping blanks and comments)"
  (save-excursion
    (beginning-of-line)
    (let ((save-point (point)))
      (goto-line 1)
      (lnt-skip-comments)
      (if (>= (point) save-point) t nil))))

(or (fboundp 'match-string-no-properties)
    (defun match-string-no-properties (number &optional string)
      "Return string of text matched by last search."
      (match-string number string)))

(defun calculate-lnt-keep-indent-if-previous (n list)
  "Calculate the indentation as follows: decrease by n chars
   only if first keyword of previous line is not in list..."
  (save-excursion
    (calculate-lnt-last-indent)
    (let ((cur (current-column)))
      (if (looking-at-in list)
	  (let ((k (match-string-no-properties 0 nil)) (p (point)))
	    (forward-line 1)
	    (if (lnt-previous-end k p)
		(- cur n)
	      cur))
	(- cur n)))))

(defun looking-at-in (keys)
  "Return t if looking at one of the keys"
  (let ((res nil) (k keys))
    (while (and k (not res))
      (setq res (looking-at (car k)))
      (setq k (cdr k)))
    res))

(defun lnt-in-comment ()
  "Return t if point is in comments"
  (or (lnt-in-singleline-comment) (lnt-in-multiline-comment)))

(defun lnt-in-singleline-comment ()
  "Return t if point is in single line comment"
  (save-excursion
    (let ((save-point (point)) (stop nil) (res nil))
      (beginning-of-line)
      (while (not stop)
	(if (search-forward "--" nil t 1)
	    (if (lnt-in-string)
		nil
	      (let nil
		(setq res t)
		(setq stop t)))
	  (setq stop t)))
      (if res
	  (> save-point (point))
	nil))))

(defun lnt-in-multiline-comment ()
  "Return t if point is in multiline comments"
  (save-excursion
    (let ((save-point (point)) (stop nil) (res nil))
      (while (not stop)
	(if (search-backward "(*" nil t 1)
	    (if (lnt-in-string)
		nil
	      (let nil
		(setq res t)
		(setq stop t)))
	  (setq stop t)))
      (if res
	  (let ((stop nil))
	    (while (not stop)
	      (if (search-forward "*)" nil t 1)
		  (if (lnt-in-string)
		      nil
		    (let nil
		      (setq res t)
		      (setq stop t)))
		(setq stop t)))
	    (if (or (not res) (<= save-point (point))) t nil))
	nil))))

(defun lnt-in-parentheses ()
  "Return t if point is parentheses"
  (let ((limit 1))
    (save-excursion
      (if (lnt-word-search-backward-out-of-end-comment-string "end")
	  (setq limit (point)))) 
    (save-excursion
      (lnt-search-backward-unbalanced-parenthesis limit))))

(defun calculate-lnt-indent-in-parentheses ()
  "Calculate indentation inside parentheses (on several lines)"
  (save-excursion
    (let ((base-indent 
	   (save-excursion 
	     (lnt-search-backward-unbalanced-parenthesis)
	     (- (current-column) 1))))
      (skip-chars-forward " \t")
      (cond ((looking-at ")") base-indent)
	    (t (+ base-indent lnt-base-indent))))))

(defun lnt-search-backward-unbalanced-parenthesis (&optional limit)
  "Backward search the first unbalanced parenthesis preceding point.
   Stop after point limit is specified. n is the number of closing/not
   opened parentheses found so far. Position point after the parenthesis.
   Return t if found, nil othewise."
  (let ((n 0) (res nil) (stop nil))
    (while (and (or (not limit) (> (point) limit)) (not stop))
      (if (lnt-search-backward-regexp-out-of-end-comment-string "[()]")
	  (let nil
	    (if (looking-at "(")
		(if (> n 0)
		      (setq n (- n 1))
		  (let nil
		    (forward-char 1)
		    (setq res t)
		    (setq stop t)))
		(setq n (+ n 1))))
	(setq stop t)))
    res))

(defun calculate-lnt-indent-in-comment ()
  "Calculate indentation inside comments"
  (save-excursion
    (skip-chars-forward " \t")
    (current-column)))

(defun lnt-out-definition ()
  "Return t if point is not inside a type, a function, a process..."
  (save-excursion
    (if (= (calculate-lnt-last-indent) 0)
	(let nil
	  (lnt-skip-comments)
	  (or (looking-at "end\\>") 
	      (looking-at "module\\>")
	      (looking-at "$")))
      nil)))

(defun lnt-in-string ()
  "Return t if point in string"
  (save-excursion
    (let ((save-point (point)) (res nil) (stop nil))
      (beginning-of-line)
      (while (not stop)
	(if (re-search-forward "\\\"\\([^\\\\\"]\\|\\\\.\\)*\\\"" nil t 1)
	    (cond ((and (> save-point (match-beginning 0))
			(< save-point (match-end 0))) 
		   (let nil
		     (setq res t)
		     (setq stop t)))
		  ((< save-point (match-beginning 0))
		   (setq stop t))
		  (t (goto-char (match-end 0))))
	  (setq stop t)))
	res)))

(defun lnt-follows-end ()
  "Return t if point follows end keyword"
  (save-excursion
    (let ((save-point (point)))
      (if (word-search-backward "end" nil t 1)
	  (let nil
	    (goto-char (match-end 0))
	    (lnt-skip-comments)
	    (if (= (point) save-point) t nil))
	nil))))

(defun lnt-search-backward-regexp-out-of-end-comment-string (regexp)
  "Backward search the first occurrence of regexp outside the scope of 
   comments, not following the end keyword, and not in string"
  (let ((stop nil) (res nil))
    (while (not stop)
      (if (re-search-backward regexp nil t 1)
	  (if (not (or (lnt-follows-end) 
		       (lnt-in-comment)
		       (lnt-in-string)
		       (looking-at "(\\*")))
	      (let nil
		(setq res t)
		(setq stop t))
	    nil)
	(setq stop t)))
    res))

(defun lnt-search-backward-regexp-out-of-comment-string (regexp)
  "Backward search the first occurrence of regexp outside the scope of 
   comments and not in string"
  (let ((stop nil) (res nil))
    (while (not stop)
      (if (re-search-backward regexp nil t 1)
	  (if (not (or (lnt-in-comment)
		       (lnt-in-string)
		       (looking-at "(\\*")))
	      (let nil
		(setq res t)
		(setq stop t))
	    nil)
	(setq stop t)))
    res))

(defun lnt-word-search-backward-out-of-end-comment-string (word)
  "Backward search the first occurrence of word outside the scope of 
   comments, not following the end keyword, and not in string"
  (let ((res nil) (stop nil))
    (while (not stop)
	(if (word-search-backward word nil t 1)
	    (if (not (or (lnt-follows-end) (lnt-in-comment) (lnt-in-string)))
		(let nil
		  (setq res t)
		  (setq stop t))
	      nil)
	  (setq stop t)))
    res))

(defun lnt-word-search-forward-out-of-end-comment-string (word)
  "Forward search the first occurrence of word outside the scope of comments,
  not following end and outside a string"
  (let ((res nil) (stop nil))
    (while (not stop)
      (if (word-search-forward word nil t 1)
	  (if (not (or (lnt-follows-end) (lnt-in-comment) (lnt-in-string)))
	      (let nil
		(setq res t)
		(setq stop t))
	    nil)
	(setq stop t)))
    res))

(defun lnt-skip-comments ()
  "Skip blank characters [ \t\n\r] and comments"
  (let ((stop nil))
    (while (not stop)
      (skip-chars-forward " \t\n\r")
      (cond ((looking-at "--") (forward-line 1))
	    ((looking-at "(\\*") 
	     (if (not (re-search-forward "\\*)" nil t 1))
		 (setq stop t)
	       nil))
	    (t (setq stop t))))))

(defun lnt-var-of-case ()
  "Return t if next token is var and belongs to case statement"
  (save-excursion
    (if (and (looking-at "var\\b")
	     (not (looking-at "var\\s_")))
	(let ((save-point (point)))
	  (if (lnt-word-search-backward-out-of-end-comment-string "case")
	      (let nil 
		(word-search-forward "case")
		(if (lnt-word-search-forward-out-of-end-comment-string "is")
		    (let nil 
		      (lnt-skip-comments)
		      (if (= save-point (point)) t nil))
		  nil))
	      nil))
	    nil)))

(defun calculate-lnt-last-indent ()
  "Return last non-empty / non-comment / non-parenthesis line indentation"
  (let ((stop nil) (res 0))
    (while (not stop)
      (if (= (forward-line -1) 0)
	  (let nil
	    (skip-chars-forward " \t")
	    (if (not (or (looking-at "$") (lnt-in-comment) (lnt-in-parentheses) (looking-at "--") (looking-at "(")))
		(let nil
		  (setq res (current-column))
		  (setq stop t))
	      nil))
	(setq stop t)))
    res))

;;; ------------------------------ highlighting ------------------------------

(if (and lnt-running-epoch lnt-use-highlighting)
    (progn
      (defvar lnt-deemphasize-style lnt-deemphasize-color)
      (defvar lnt-emphasize-style lnt-emphasize-color)))

(if (and lnt-running-lemacs lnt-use-highlighting)
    (progn
      (defvar lnt-deemphasize-style (make-face 'lnt-deemphasize-face))
      (set-face-foreground lnt-deemphasize-style lnt-deemphasize-color)
      (defvar lnt-emphasize-style (make-face 'lnt-emphasize-face))
      (set-face-foreground lnt-emphasize-style lnt-emphasize-color)))

(defvar lnt-use-font-lock lnt-running-lemacs
  "*Flag to use font-lock for LNT keywords in Lucid Emacs.  If non-NIL,
font-lock will be used.  Default is T if you are running with Lucid Emacs;
NIL otherwise.  This doesn't currently seem to work.  Bummer.  Ten points
to the first person who tells me why not.")

(defvar lnt-font-lock-keywords-aux
  (list
   ;; types and channels : match word *following* a ':' or special keywords
   (list
    (concat
     "\\(" ;; begin group
     ":" ;; character ':' introduce a type or channel identifier
     "\\|" ;; regexp or
     (regexp-opt '(
                   ;; keywords coming right before a channel/type identifier
                   ;; keywords common to Traian and Lnt2Lotos
                   "any"
                   "of"
                   "type"
                   ;; keywords specific to Lnt2Lotos
                   "channel"
                   ) 'words) ;; the produced regexp is in a regexp group
     "\\)" ;; end of group
     "\\s-*" ;; spaces ('\\s-' matches any character with a 'space' syntax)
     "\\(\\w+\\)" ;; in the third group, word to fontify
     )
    '(3 ;; string matching 3rd group
      font-lock-type-face))
   )
  "Additional highlightings for LNT mode")

(defvar lnt-font-lock-keywords-all
   (append
      ;; default list
      lnt-font-lock-keywords-aux
      ;; additional list
      lnt-font-lock-keywords
   )
   "Keywords used in LNT mode")

;;; ------------------------------ lnt-mode ------------------------------

(defun lnt-entry-point ()
  ""
  (use-local-map lnt-mode-map)
  (setq local-abbrev-table lnt-mode-abbrev-table)
  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments nil)
  (make-local-variable 'comment-column)
  (setq comment-column 32)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "(\\*+ *")
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'lnt-indent-line)
  ;; set new font lock variable
  (set (make-local-variable 'font-lock-defaults)
    '(lnt-font-lock-keywords-all nil nil))
)
