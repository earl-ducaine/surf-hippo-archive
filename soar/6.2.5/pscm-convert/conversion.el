;;;; -*- Mode: Emacs-Lisp -*-
;;;; 
;;;; $Source: /usr0/rempel/soar/6.2.5/pscm-convert/RCS/conversion.el,v $
;;;; $Id: conversion.el,v 1.2 1994/07/01 15:48:58 portelli Exp $
;;;; 
;;;; Description       : Code for converting files of productions to NNPSCM.
;;;; Original author(s): Scott Huffman, Michael Hucka
;;;; Organization      : AI Lab, University of Michigan EECS, Michigan, USA
;;;; 

(provide 'conversion)

(defun nnpscm-convert-task (lfile)
  (interactive (list (sde-prompt-for-load-file)))
  (nnpscm-conversion-warning)
  (nnpscm-convert-all-files lfile)
  (message "Conversion completed."))

(defun nnpscm-conversion-warning ()
  (with-output-to-temp-buffer "*sde warning*"
    (princ "WARNING: This is a destructive operation\n"))
  (if (y-or-n-p "Proceed? ")
      (kill-buffer "*sde warning*")
    (error "Conversion aborted.")))

(defun nnpscm-convert-all-files (lfile)
  "Convert the files in the task rooted at the given load file."
  (let (problem-files)
    (with-output-to-temp-buffer "*nnpscm conversion file list*"
      (sde-save-sde-buffers)
      (sde-map-task-files (function
			   (lambda (file)
			     (condition-case ()
				 (progn
				   (princ file)
				   (princ "\n")
				   (nnpscm-convert-file file))
			       (error
				(push file problem-files)))))
			  (sde-get-file-task lfile)))
    ;; Show list of problem files to user.
    (when problem-files
      (with-output-to-temp-buffer "*nnpscm warnings*"
	(princ "====== Problems converting the following files ======\n\n")
	(dolist (file problem-files)
	  (princ file)
	  (princ "\n")))
      (beep))))	  

(defun nnpscm-match-length ()
  (- (match-end 0) (match-beginning 0)))

(defun nnpscm-convert-file (file)
  "Convert productions in FILE.  Store in FILE.  DESTRUCTIVE!"
  ;; (let ((blist (mapcar 'buffer-file-name (buffer-list))))
  (interactive "fFile: ")
  (let ((buffer))
    (save-excursion
      (let* ((buffer (get-file-buffer file))
	     (exists buffer))
	(if buffer
	    (set-buffer buffer)
	  ;; Not anywhere yet.
	  (setq buffer (create-file-buffer file))
	  (set-buffer buffer)
	  (erase-buffer)
	  (sde-mode-internal)
	  (if (not (condition-case ()
		       (insert-file-contents file t)
		     (file-error nil)))
	      (error "Could not read %s" file)))
	;; Now start converting.
	(message "Converting %s" buffer-file-name)
	(goto-char (point-max))
	(insert "\n\n")
	;; Replace any user's "superstate" with "*superstate-user"
	(goto-char (point-min))
	(while (search-forward "superstate" nil t)
	  (replace-match "*superstate-user" nil t))
	(nnpscm-convert-productions-loop)
	(goto-char (point-min))
	(while (search-forward "*superstate-user" nil t)
	  (replace-match "superstate" nil t))
	(if (and exists (buffer-modified-p))
	    (if (y-or-n-p (format "Save %s" (buffer-name buffer)))
		(save-buffer buffer))
	  (save-buffer buffer)
	  (kill-buffer buffer))))))

(defun nnpscm-convert-productions-loop ()
  ;; Loop for converting a file's productions.
  (goto-char (point-min))
  (while (re-search-forward sde-sp-name-regexp nil t)
    (let (here goal-vars)
      (beginning-of-line)
      (setq here (point))
      (setq goal-vars (nnpscm-convert-production))
      (goto-char here)
      (nnpscm-convert-stage-2 goal-vars)
      (sde-end-of-production))))

(defun nnpscm-goal-start-regexp (glist)
  ;; return a regexp for matching a goal condition.
  (concat "[{]*?(\\(state \\)?[ \t]*\\("
	  (mapconcat 'identity glist "\\|")
	  "\\)"))

(defun nnpscm-convert-production ()
  "Convert production immediately following the point."
  (interactive)
  (forward-line 1)
  (down-list 1)				; Move to first "(goal .."
  (beginning-of-line)
  (let ((begin (point))			; Beginning of LHS
	(end (save-excursion		; Beginning of line containing "-->".
	       (search-forward "-->")
	       (beginning-of-line)
	       (point)))
	(goal-starters)
	(goal-regexp))
    ;; Find goal id's and replace "(goal" with "(state".
    (while (re-search-forward "\\((goal\\)\\([ \t]*\\)\\(<[^>]+>\\)" end t)
      (setq goal-starters (cons (sde-buffer-substring 3) goal-starters))
      (replace-match "(state\\2\\3"))
    (setq goal-regexp (nnpscm-goal-start-regexp goal-starters))
    (goto-char begin)
    (while (not (looking-at "[ \t]*-->"))
      (skip-chars-forward " \t")
      (let ((cbegin (point))		; start, end of condition
	    (cend (save-excursion (forward-sexp 1) (point))))
	(if (looking-at goal-regexp)
	    (while (re-search-forward "\\^\\(object.\\)*object " cend t)
	      ;; Make sure it's at the outermost level:
	      (let ((object-pt (point))
		    goto-next)
		(while (and (not goto-next) (re-search-backward "(" (+ cbegin 1) t))
		  (forward-char -1)
		  (forward-sexp 1)
		  (if (> (point) object-pt)
		      (setq goto-next 1)
		    (forward-sexp -1)))
		(if goto-next
		      ;;; NOT AT OUTERMOST LEVEL:
		      ;;; Look for the next one.
		    (progn
		      (goto-char object-pt))
		    ;;; AT OUTERMOST LEVEL.  Extract state var.
		  (progn
		    (goto-char object-pt)
		    (if (re-search-forward "\\([ \t]*\\)\\(<[^>]+>\\)" cend t)
			(progn
			  (setq goal-starters (cons (sde-buffer-substring 2) goal-starters)))))))))
	(goto-char cend)
	(forward-sexp 1)
	(forward-sexp -1)))
    
    (setq goal-regexp (nnpscm-goal-start-regexp goal-starters))

    ;; Loop for converting ^object's
    (goto-char begin)

    (while (not (looking-at "[ \t]*-->"))
      (skip-chars-forward " \t")
      (let ((cbegin (point))		; start, end of condition
	    (cend (save-excursion (forward-sexp 1) (point))))
	
	(if (looking-at goal-regexp)
	    (let (plus-warning)
	      ;; (goto-char cend) (insert " ;;gc\n") (goto-char cbegin)
	      ;; Replace outer ^object's (and dot objects's).
	      (while (re-search-forward "\\^object" cend t)
		;; Make sure it's at the outermost level:
		;; e.g., skip over embedded lists : ^superstate (... ( ..) ^object ...)
		;; and skip ( ... () () ... (... ^object))
		(let ((object-pt (point))
		      goto-next)
		  (while (and (not goto-next) (re-search-backward "(" (+ cbegin 1) t))
		    (forward-char -1)
		    (forward-sexp 1)
		    (if (> (point) object-pt)
			(setq goto-next 1)
			(forward-sexp -1)))
		  (if goto-next
		      (progn
			(goto-char object-pt)
			(forward-sexp 1))
		    (progn
		      (goto-char object-pt)
		      (re-search-backward "\\^object" cbegin t)
		      (replace-match "^superstate")
		      (setq cend (+ cend 4))
		      (while (looking-at "\\.object")
			(replace-match ".superstate")
			(setq cend (+ cend 4)))))))


	      ;; Search for embedded ^objects and replace them.
	      (goto-char cbegin)
	      (while (re-search-forward
		      "\\^superstate\\(\\.superstate\\)*[ \t]+(" cend t)
		(let ((embed-pt (point))
		      object-pt
		      goto-next)
		  (while (re-search-forward "\\^object" cend t) 
		    (setq object-pt (point))
		    (setq goto-next nil)

		    ;; See if it's a direct embedding of the current level.
		    ;; skip over embedded lists : ^superstate (... ( ..) ^object ...)
		    ;; and skip ( ... () () ... (... ^object))
		  (while (and (not goto-next) (re-search-backward "(" (+ embed-pt 1) t))
			(forward-char -1)
			(forward-sexp 1)
			(if (> (point) object-pt)
			    (setq goto-next t)
			  (forward-sexp -1)))

		  (if (not goto-next)
			(progn
			  (goto-char object-pt)
			  (re-search-backward "\\^object" embed-pt t)
			  (replace-match "^superstate")
			  (setq cend (+ cend 4))
			  (while (looking-at "\\.object")
			    (replace-match ".superstate")
			    (setq cend (+ cend 4)))))
		    (goto-char object-pt))
		  (goto-char embed-pt)))

	      ;; Remove +'s following state or problem-space augmentations
	      ;; and flag them.
	      (goto-char cbegin)
	      (while (re-search-forward
		      "\\^\\(superstate\\.\\)*\\(problem-space\\|state\\)[ \t]*<[^>]+>[ \t]+\\+" cend t)
		(setq plus-warning t)
		(let ((pt (point)))
		  (forward-char -1)
		  (skip-chars-backward " \t")
		  (delete-region (point) pt)))
	      (if plus-warning
		  (progn
		    (goto-char cbegin)
		    (insert ";; ** NNPSCM Conversion Warning: acceptable preference test for state \n")
		    (lisp-indent-line)
		    (insert ";; ** or problem-space removed from following condition.\n")
		    (lisp-indent-line)
		    (insert ";; ** THIS MAY ALTER BEHAVIOR.\n")
		    (lisp-indent-line)))))
	(goto-char cend)
	(forward-sexp 1)
	(forward-sexp -1)))
    goal-starters))

(defun nnpscm-skip-comments ()
  (skip-chars-forward " \t\n")
  (while (looking-at ";")
      (forward-line 1)
      (beginning-of-line)))

;; Stage 2: remove intermediate "state" pointer.
(defun nnpscm-convert-stage-2 (state-starters)
  (interactive)

  (forward-line 1)
  (down-list 1)				; Move to first "(state .."
  (beginning-of-line)
  (let ((begin (point))			; Beginning of LHS
	(end (save-excursion		; Beginning of line containing "-->".
	       (search-forward "-->")
	       (beginning-of-line)
	       (point)))
	(real-end (save-excursion (sde-end-of-production)(- (point) 1)))
	(state-vars)
	(state-vars-assoc)
	(state-regexp)
	(state-vars-regexp))

    (setq state-regexp (nnpscm-goal-start-regexp state-starters))

    (goto-char begin)
    (while (not (>= (point) real-end))
      (skip-chars-forward " \t")
      (let ((cbegin (point))		; start, end of condition
	    (cend (save-excursion (forward-sexp 1) (point))))
	
	(if (looking-at state-regexp)
	    (progn
	    ;; Find ^state conditions (^state and ^superstate*.state; NOT embedded ones!)
	    ;; e.g. NOT (<g> ^superstate (<ss> ^state <sss>))
	    ;;  and NOT ^state <s1> <s2>...

	    ;;; Cases:                                   Result:
	    ;;; (1) ^(superstate.)*state <x>             ^(superstate.)* <x>
	    ;;; (2) ^(superstate.)*state.x <x>           ^(superstate.)*.x <x>
	    ;;; (3) ^state.x <x>                         ^x <x>
	    ;;; (4) ^state <x>                           replace <x> with <g> elsewhere.
	    ;;; (5) ^(superstate.)*state (<x> ...)       ^(superstate.)* (<x> ...)
	    ;;;      --handled by (1)
	    ;;; (6) ^(superstate.)*state (etc)           ^(superstate.)* (etc)
	    ;;;       --handled by (1)

	    ;;; (7) ^state (<x> etc)                     etc, and replace <x> with <g> elsewhere.
	    ;;; (8) ^state (etc)                         etc

	    ;;;; CASE (1)/(2):
	    (while (re-search-forward "\\^\\(superstate.\\)+state" cend t)
	      ;; Make sure it's at the outermost level:
	      (let ((object-pt (point))
		    goto-next)
		(while (and (not goto-next) (re-search-backward "(" (+ cbegin 1) t))
		  (forward-char -1)
		  (forward-sexp 1)
		  (if (> (point) object-pt)
		      (setq goto-next 1)
		    (forward-sexp -1)))
		(if goto-next
		      ;;; NOT AT OUTERMOST LEVEL:
		      ;;; Look for the next one.
		    (progn
		      (goto-char object-pt))
		    ;;; AT OUTERMOST LEVEL: CASE 1/2.
		  (progn
		    (goto-char object-pt)
		    (re-search-backward ".state" cbegin t)
		    (replace-match "")
		    (setq cend (- cend 6))
		    (setq real-end (- real-end 6))
		    (setq object-pt (- object-pt 6))
		    (goto-char object-pt)))))

	  ;;;; CASE (3)/(4)/(7)/(8)
          ;;; (3) ^state.x <x>
	  ;;; (4) ^state <x>
	  ;;; (7) ^state (<x> etc)                     etc, and replace <x> with <g> elsewhere.
	  ;;; (8) ^state (etc)                         etc
	  (goto-char cbegin)
	  (while (re-search-forward "\\^state" cend t)
	      ;; Make sure it's at the outermost level:
	      (let ((object-pt (point))
		    goto-next)
		(while (and (not goto-next) (re-search-backward "(" (+ cbegin 1) t))
		  (forward-char -1)
		  (forward-sexp 1)
		  (if (> (point) object-pt)
		      (setq goto-next 1)
		    (forward-sexp -1)))
		(if goto-next
		      ;;; NOT AT OUTERMOST LEVEL:
		      ;;; Look for the next one.
		    (progn
		      (goto-char object-pt))
		    ;;; AT OUTERMOST LEVEL: CASE 3/4.
		  (progn
		    ;; Distinguish cases.
		    (if (looking-at "[ \t]")
			;;; Not case (3)
			(progn
			  ;; Remove "^state".
			  (goto-char object-pt)
			  (re-search-backward "\\^state" cbegin t)
			  (replace-match "")
			  (setq cend (- cend 6))
			  (setq object-pt (- object-pt 6))
			  (setq real-end (- real-end 6))
			  (if (looking-at "\\([ \t]*\\)\\(<[^>]+>\\)")
			      ;; Case (4).
			      (progn
				(setq cend (- cend (nnpscm-match-length)))
				(setq real-end (- real-end (nnpscm-match-length)))
				(setq state-vars (cons (sde-buffer-substring 2) state-vars))
				(replace-match "")
				(goto-char cbegin)
				(re-search-forward "\\([^<]*\\)\\(<[^>]+>\\)" cend t)
				(setq state-vars-assoc (cons (cons (car state-vars) (sde-buffer-substring 2)) state-vars-assoc)))
			    ;; Case (7) or (8).
			    (progn 
			      (if (looking-at "\\([ \t\n]*\\)(\\([ \t\n]*\\)\\(<[^>]+>\\)")
				  ;; Case (7).
				  (progn
				    (setq cend (- cend (+ (nnpscm-match-length) 1)))
				    (setq real-end (- real-end (+ (nnpscm-match-length) 1)))
				    (setq state-vars (cons (sde-buffer-substring 3) state-vars))
				    (save-excursion (forward-sexp 1)(forward-char -1)(delete-char 1))
				    (replace-match "")
				    (goto-char cbegin)
				    (re-search-forward "\\([^<]*\\)\\(<[^>]+>\\)" cend t)
				    (setq state-vars-assoc (cons (cons (car state-vars) (sde-buffer-substring 2)) state-vars-assoc)))
				;; Case (8).
				(if (looking-at "\\([ \t]*\\)(") ;; better be!
				    (progn
				      (setq cend (- cend 2))
				      (setq real-end (- real-end 2))
				      (save-excursion (forward-sexp 1)(forward-char -1)(delete-char 1))
				      (replace-match "\\1"))
				  ;; (error "Error!")
				  )))))
		      ;;; Case (3)
		      (progn
			(goto-char object-pt)
			(forward-char 1)
			(re-search-backward "state." cbegin t)
			(replace-match "")
			(setq cend (- cend 6))
			(setq object-pt (- object-pt 6))
			(setq real-end (- real-end 6))))
		    (goto-char object-pt)))))))
	(goto-char cend)
	(skip-chars-forward " \t\n}")
	(nnpscm-skip-comments)
	(if (and (< (point) real-end)
		 (not (looking-at "[ \t\n]*)")))
	    (progn
	      (forward-sexp 1)
	      (forward-sexp -1))
	  (goto-char real-end))))
    
    (goto-char begin)
    (if state-vars
	(progn
    (setq state-vars-regexp (concat "\\(([ \t]*\\)\\(" (mapconcat 'identity state-vars "\\|") "\\)"))

    (while (not (>= (point) real-end))
      (skip-chars-forward " \t{\n-")
      (let ((cbegin (point))		; start, end of condition
	    (cend (save-excursion (forward-sexp 1) (point))))
	(if (looking-at state-vars-regexp)
	    (progn
	      (replace-match (concat (sde-buffer-substring 1)
				     (cdr (assoc (sde-buffer-substring 2) state-vars-assoc))))))
	(goto-char cend)
	(skip-chars-forward " \t\n}")
	(nnpscm-skip-comments)
	(if (and (< (point) real-end)
		 (not (looking-at "[ \t\n]*)")))
	    (progn
	      (forward-sexp 1)
	      (forward-sexp -1))
	  (goto-char real-end))))


    (goto-char begin)
    (let (svar 
	  svar-regexp
	  (starter-exp (concat "[{]*?(\\(state \\)?[ \t]*\\("
				    (mapconcat 'identity state-starters "\\|")
				    "\\)[ \t]*)[}]*")))

      ;; Look for (state <x> ).  Merge in with other conditions.
      (while (re-search-forward starter-exp real-end t)
	;; Found one.  Remove it, and replace the next condition starting with the same var
	;; with (state <var>.
	(setq svar (cons (sde-buffer-substring 2) svar))
	(setq real-end (- real-end (- (match-end 0)(match-beginning 0))))
	(replace-match ""))
      (if svar
	    (progn
	      (setq svar-regexp
		    (concat "[{]*?([ \t]*\\("
			    (mapconcat 'identity svar "\\|")
			    "\\)"))
	      
	      (goto-char begin)
	      (while (not (>= (point) real-end))
	      ;; (while (not (looking-at "[ \t]*-->"))
		(skip-chars-forward " \t\n")
		(let ((cbegin (point))		; start, end of condition
		      (cend (save-excursion (forward-sexp 1) (point)))
		      (st-str (if (> (point) end)
				  "("
				"(state ")))
		  
		  (if (looking-at svar-regexp)
		      (progn
			(setq cend (+ cend (- (length st-str) 1)))
			(replace-match (concat st-str (sde-buffer-substring 1)))))
		(goto-char cend)
		(skip-chars-forward " \t\n}")
		(nnpscm-skip-comments)
		(if (and (< (point) real-end)
			 (not (looking-at "[ \t\n]*)")))
		    (progn
		      (forward-sexp 1)
		      (forward-sexp -1))
		  (goto-char real-end)))))))))
    (goto-char end)))


;;; Debugging.

(defun cp ()
  (nnpscm-convert-production))

(defun c2 (x)
  (nnpscm-convert-stage-2 x))

(defun ca ()
  (let (x here)
    (setq here (point))
    (setq x (nnpscm-convert-production))
    (goto-char here)
    (c2 x)))
