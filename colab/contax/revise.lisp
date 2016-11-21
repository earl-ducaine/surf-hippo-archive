;;; -*- Base: 10; Package: CONTAX; Mode: LISP; Syntax: CLtL -*-

;********************************************************************
;
;       Revise fuer HAC
;
;*******************************************************************

( in-package "CONTAX" )

( export '( revise hac-check ))

;
;********************************************************************
;
;     Revise fuer ein Constraint und die Menge von Variablen
;     es wird revise-one fuer jede variable an der linkesten 
;     Position untersucht
;     Zurueckgegeben wird die Liste der Constraints, die wieder
;     untersucht werden muessen, wobei duplicate entfernt werden.
;
;     Revise wird fuer jede variable, die am constraint r haengt
;     durchgefuhrt, d.h. es wird ueberprueft, ob fuer alle werte
;     in dieser variable die hierarchische AC erfuellt ist


(defun revise (r var)
  (remove-duplicates
   (mapcar #'(lambda (v d)
		    (revise-one-var r v d)
		    )
	  (each-first var)
	  (each-first (get-parameter-names r))
	  )
   :test 'equal)
  )

;***********************************************************************
;
;  Revise fuer eine Kombination der Variablen
;   left-val enthaelt den aktuellen der stand der linkesten 
;   variablen von var
;   right-val alle moeglichen Kombinationen der werte der variablen
;   die nicht links stehen
;   in ergval stehen die werte, die fuer eine Kombination von werten
;   das Praedikat A erfuellen
;   Falls ein Wert gestrichen wurde, werden die entsprechenden
;   constraints, die wieder betrachtet werden muessen, zurueckgegeben
;   die Variable bekommt den neuen wert 
;   Im Rumpf der Schleife wird getestet ob
;    A erfuellt ist fuer eine Kombination. Wenn ja
;     dann wird der Wert in die Ergebnisliste uebernommen
;    S erfuellt is fuer eine Kombination. Wenn ja
;     dann werden alle Soehne ueberprueft
;   Sonst wird der wert nicht uebernommen, und die Soehne werden
;   nicht geprueft

(defun revise-one-var (r var dom)
 (do ((left-val (var-vals (car var)) left-val)
      (right-val (compute-combinations (mapcar #'var-vals (cdr var)))
		 right-val)
      (erg-val nil erg-val)
      )
     ((null left-val)
      (cond ((not (equal erg-val (var-vals (car var))))
	     (set-var (car var) erg-val)
	     (constraints-with (car var)))
	    (t nil)))
     (cond ((some #'(lambda (w)
			    (look-up-a r dom (cons (car left-val)
						   w)))
		  right-val)
           (setf erg-val (cons (car left-val) erg-val))
	   (setf left-val (cdr left-val)))
	  ((some #'(lambda (w)
			   (look-up-s r dom (cons (car left-val)
						w)))
		 right-val)
	   (setf left-val (union (sons-of (car left-val))
				(cdr left-val) :test 'equal)))
	   (t (setf left-val (cdr left-val))
	      )
	   )
     )
  )

;*******************************************************************
;
;  Aufruf mit Namen eines Constraints
;  Variablen werden als Seiteneffekt veraendert
;  Ergebniswert: *active-constraints* und alle constraints, die wieder
;                betrachtet werden muessen

(defun hac-check (r)
  (remove r (union (revise (name-von r)
		       	   (vars-of (name-von r)))
		   (constraints-of *active-constraints*) :test 'equal)
	  :test 'equal)
  )

;****************************************************************
;
;      funktionen nach contax
;
;***************************************************************

; changed 13-05-92 fs
; removed 28-09-92 fs
;( defun get-val ( var )
;  ( read-intervalls ( var-vals var )))	


;*************************************************************
;
;       methoden von contax
;
;*************************************************************

; changed 13-05-92 fs
(defun set-var (var val)
  (setf (var-vals (find-variable var)) val)
 )

; changed 13-05-92 fs
(defun constraints-with (var)
  (copy-tree (var-cons var ))
 )

(defun name-von (r)
  (cname r)
 )

(defun vars-of (r)
  (mapcar #'vname (mapcar #'(lambda (x)
				   (funcall x (find-constraint r)))
			 (parameter-names (find-constraint r))))
 )













