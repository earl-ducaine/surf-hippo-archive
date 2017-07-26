D,#TD1PsT[Begin using 006 escapes];;; -*- mode: lisp; Syntax: Common-lisp; package: #-parallel surf #+parallel *lisp; base: 10; fonts: CPTFONT ; -*-
;;; (c) Copyright 1985, Don Webber, Thinking Machines Corporation, Inc.
;;; File creation date: 6/09/85 11:57:32
;
; level 1 mos transistor model
;

(defvar debug-mos1 nil)

(defstruct (mos1 :conc-name :named)
  "Model for a mos1 transistor"
  (name ""		:type string)
  (model nil)				; the model instance
  (node-d nil		:type node)
  (node-g nil		:type node)
  (node-s nil		:type node)
  (node-b nil		:type node)
  (core-mos nil		:type core-mos1)
  (params nil)
)

#-parallel
(defstruct (core-mos1 :conc-name :named)
  "Core model for a mos1 transistor"
  (node-d-pointp nil	:type boolean)	; zero if constant, one if node
  (node-d-point nil	:type node)
  (node-d-const ZERO	:type long-float)
  (node-g-pointp nil	:type boolean)	; zero if constant, one if node
  (node-g-point nil	:type node)
  (node-g-const ZERO	:type long-float)
  (node-s-pointp nil	:type boolean)	; zero if constant, one if node
  (node-s-point nil	:type node)
  (node-s-const ZERO	:type long-float)
  (node-b-pointp nil	:type boolean)	; zero if constant, one if node
  (node-b-point nil	:type node)
  (node-b-const ZERO	:type long-float)

  (mat-dg-valid nil	:type boolean)
  (mat-dg-point nil	:type core-off-diag)
  (mat-dg-value nil	:type long-float)
  (mat-ds-valid nil	:type boolean)
  (mat-ds-point nil	:type core-off-diag)
  (mat-ds-value nil	:type long-float)
  (mat-db-valid nil	:type boolean)
  (mat-db-point nil	:type core-off-diag)
  (mat-db-value nil	:type long-float)

  (mat-gd-valid nil	:type boolean)
  (mat-gd-point nil	:type core-off-diag)
  (mat-gd-value nil	:type long-float)
  (mat-gs-valid nil	:type boolean)
  (mat-gs-point nil	:type core-off-diag)
  (mat-gs-value nil	:type long-float)
  (mat-gb-valid nil	:type boolean)
  (mat-gb-point nil	:type core-off-diag)
  (mat-gb-value nil	:type long-float)

  (mat-sd-valid nil	:type boolean)
  (mat-sd-point nil	:type core-off-diag)
  (mat-sd-value nil	:type long-float)
  (mat-sg-valid nil	:type boolean)
  (mat-sg-point nil	:type core-off-diag)
  (mat-sg-value nil	:type long-float)
  (mat-sb-valid nil	:type boolean)
  (mat-sb-point nil	:type core-off-diag)
  (mat-sb-value nil	:type long-float)

  (mat-bd-valid nil	:type boolean)
  (mat-bd-point nil	:type core-off-diag)
  (mat-bd-value nil	:type long-float)
  (mat-bg-valid nil	:type boolean)
  (mat-bg-point nil	:type core-off-diag)
  (mat-bg-value nil	:type long-float)
  (mat-bs-valid nil	:type boolean)
  (mat-bs-point nil	:type core-off-diag)
  (mat-bs-value nil	:type long-float)

  (ids ZERO)
  (cond-d ZERO)
  (cond-g ZERO)
  (cond-s ZERO)
  (cond-b ZERO)
  (charge-d ZERO)
  (charge-g ZERO)
  (charge-s ZERO)
  (charge-b ZERO)
; parameters
  (w ZERO		:type float)
  (l ZERO		:type float)
  (mtype NMOS		:type bit)		; 0 for nmos, 1 for pmos
  (fixed-vto ZERO	:type float)		; has vto and phi built in
  (vfb ZERO		:type float)
  (phi ZERO		:type float)
  (beta ZERO		:type float)
  (lambda ZERO		:type float)
  (gamma ZERO		:type float)
  (cgsub ZERO		:type float)
  (cgs ZERO		:type float)
  (cgd ZERO		:type float)
  (cgb ZERO		:type float)
  (cjas ZERO		:type float)
  (cjad ZERO		:type float)
  (cjass ZERO		:type float)
  (cjasd ZERO		:type float)
)

(defconstant known-mos1-parameters
	     '(mtype w l ld as ad ps pd vto kp gamma phi lambda tox
	       cgdo cgso cgbo cj cjsw mj mjsw pb fc js is vtherm)
  "The list of known model parameters for mos1." )

(defconstant nmos-default-parameters
	     '((mtype . NMOS) (w . 2e-6) (l . 2e-6) (ld . 0.0) 
	       (as . 0.0) (ad . 0.0) (ps . 0.0) (pd . 0.0)
	       (vto . 0.0) (kp . 20e-6) (gamma . 0.0) (phi . 0.6)
	       (lambda . 0.0) (tox . 0.0)
	       (cgdo . 0.0) (cgso . 0.0) (cgbo . 0.0)
	       (cj . 0.0) (cjsw . 0.0) (mj . 0.5) (mjsw . 0.5)
	       (pb . 0.8) (fc . 0.5) (js . 0.0) (is . 1e-14) (vtherm . VTHERMAL))
  "The list of default model parameters for nmos1." )

(defconstant pmos-default-parameters
	     '((mtype . PMOS) (w . 2e-6) (l . 2e-6) (ld . 0.0) 
	       (as . 0.0) (ad . 0.0) (ps . 0.0) (pd . 0.0)
	       (vto . 0.0) (kp . 20e-6) (gamma . 0.0) (phi . 0.6)
	       (lambda . 0.0) (tox . 0.0)
	       (cgdo . 0.0) (cgso . 0.0) (cgbo . 0.0)
	       (cj . 0.0) (cjsw . 0.0) (mj . 0.5) (mjsw . 0.5)
	       (pb . 0.8) (fc . 0.5) (js . 0.0) (is . 1e-14) (vtherm . VTHERMAL))
  "The list of default model parameters for pmos1." )

; These global variables aren't used as globals, their just here because 
;  you can't use 'set' on local variables. ( see create-mos1 )
(defvar mtype)
(defvar w)
(defvar l)
(defvar ld)
(defvar as)
(defvar ad)
(defvar ps)
(defvar pd)
(defvar vto)
(defvar kp)
(defvar gamma)
(defvar phi)
(defvar lambda)
(defvar tox)
(defvar cgdo)
(defvar cgso)
(defvar cgbo)
(defvar cj)
(defvar cjsw)
(defvar mj)
(defvar mjsw)
(defvar pb)
(defvar fc)
(defvar js)
(defvar is)
(defvar vtherm)

(defvar mos1-hash-table ())

(defun create-mos1-model ()
  "Creates a template for all mosfets."
  (let ((template (make-model-template)))
    (setf
      (model-template-name template) (string "mos1")
      (model-template-default-params template) nmos-default-parameters
      (model-template-eval-routine template) #'eval-mos1
      (model-template-print-routine template) #'print-mos1
      (model-template-create-routine template) #'create-mos1
      (model-template-create-core-routine template) #'create-core-mos1
      (model-template-add-off-diag-routine template) #'add-off-diag-mos1
      (model-template-find-coupling-routine template) #'find-coupling-mos1
      (model-template-fix-dc-nodes-routine template) #'mos1-fix-dc-nodes
      *models* (cons template *models*)
      (gethash (string "mos1") *model-hash-table*) template
      mos1-hash-table (make-hash-table :test #'equal))))

(defun create-mos1 (name noded nodeg nodes nodeb model-name parameters)
  "Creates a element of type mos1. Inputs 'name' and the nodes are strings,
   'model-name' is the name of the model-instance, and parameters are the parameters to change."
  (if (gethash name mos1-hash-table)
      (sim-warning (zl:format nil "create-mos1: mos1 ~a  already defined, ignoring"
			   name))
      (let ((m (make-mos1))
	    (model (gethash model-name *model-instance-hash-table*))
	    (node-d (create-node noded))
	    (node-g (create-node nodeg))
	    (node-s (create-node nodes))
	    (node-b (create-node nodeb)))
	(if (null model)
	    (sim-error "Mosfet model ~a not defined" model-name))
	(setf
	  (mos1-model m) model
	  (mos1-name m) name
	  (mos1-node-d m) node-d
	  (mos1-node-g m) node-g
	  (mos1-node-s m) node-s
	  (mos1-node-b m) node-b
	  (node-elements node-d) (cons m (node-elements node-d))
	  (node-elements node-g) (cons m (node-elements node-g))
	  (node-elements node-s) (cons m (node-elements node-s))
	  (node-elements node-b) (cons m (node-elements node-b))
	  (mos1-params m) parameters
	  (gethash name mos1-hash-table) m
	  (model-instance-elements model) (cons m (model-instance-elements model)))
	m)))


; This function creates a core mos1 data structure. In the parallel version, it 
; puts the device on the same processor as the fanout node, which is passed in as the 
; variable 'proc. The serial version ignores this and allocates a struct normally.
; This function only works if the 'nd argument is not a DC source node, because these
; do not have processors allocated to them.

(defun create-core-mos1 (m nd)
  "Creates the core model for a mosfet."
  (let ((core-m nil)
	(noded (mos1-node-d m))
	(nodeg (mos1-node-g m))
	(nodes (mos1-node-s m))
	(nodeb (mos1-node-b m))
	(model (mos1-model m))
	(junk)
	(template-parameters
	  (model-template-default-params (model-instance-model (mos1-model m))))
	(proc #-parallel nil #+parallel (allocate-processor)))
    #+parallel (setf (pref fanout-valid proc) t)
    #+parallel (setf (pref fanout-seg-forward proc) nil)
    (if (mos1-core-mos m)			; 
	(setf core-m (mos1-core-mos m))		; core mos1 has already been allocated
	(progn					; else allocate one
	  #-parallel (setf core-m (make-core-mos1))
	  #+parallel (setf core-m proc)
	  #+parallel (setf (pref *lisp-struct-type core-m) core-mos1)
	  (setf (mos1-core-mos m) core-m)

	  #+parallel
	  (progn
	    (if (eq noded nodes)
		(setf (pref core-mos1-equal-ds core-m) t)
		(setf (pref core-mos1-equal-ds core-m) nil))
	    (if (eq noded nodeg)
		(setf (pref core-mos1-equal-dg core-m) t)
		(setf (pref core-mos1-equal-dg core-m) nil))
	    (if (eq nodes nodeg)
		(setf (pref core-mos1-equal-sg core-m) t)
		(setf (pref core-mos1-equal-sg core-m) nil))
	    (if (eq nodeb nodeg)
		(setf (pref core-mos1-equal-bg core-m) t)
		(setf (pref core-mos1-equal-bg core-m) nil))
	    (if (eq nodes nodeb)
		(setf (pref core-mos1-equal-sb core-m) t)
		(setf (pref core-mos1-equal-sb core-m) nil))
	    (if (eq noded nodeb)
		(setf (pref core-mos1-equal-db core-m) t)
		(setf (pref core-mos1-equal-db core-m) nil)))
	  
;    (zl:format t "Creating mos1 at proc ~a~%" core-m)
;    (zl:format t "nodes are ~a ~a ~a ~a ~%" (node-core-nd noded)
;      (node-core-nd nodeg) (node-core-nd nodes) (node-core-nd nodeb))
	  
; NOTE: the following vars should match the list of *mos-default-parameters which
;  should also match the list known-mos1-parameters.
	  (setf
	    mtype  (eval (cdr (assoc 'mtype template-parameters)))
	    w      (cdr (assoc 'w template-parameters))
	    l      (cdr (assoc 'l template-parameters))
	    ld     (cdr (assoc 'ld template-parameters))
	    as     (cdr (assoc 'as template-parameters))
	    ad     (cdr (assoc 'ad template-parameters))
	    ps     (cdr (assoc 'ps template-parameters))
	    pd     (cdr (assoc 'pd template-parameters))
	    vto    (cdr (assoc 'vto template-parameters))
	    kp     (cdr (assoc 'kp template-parameters))
	    gamma  (cdr (assoc 'gamma template-parameters))
	    phi    (cdr (assoc 'phi template-parameters))
	    lambda (cdr (assoc 'lambda template-parameters))
	    tox    (cdr (assoc 'tox template-parameters))
	    cgdo   (cdr (assoc 'cgdo template-parameters))
	    cgso   (cdr (assoc 'cgso template-parameters))
	    cgbo   (cdr (assoc 'cgbo template-parameters))
	    cj     (cdr (assoc 'cj template-parameters))
	    cjsw   (cdr (assoc 'cjsw template-parameters))
	    mj     (cdr (assoc 'mj template-parameters))
	    mjsw   (cdr (assoc 'mjsw template-parameters))
	    pb     (cdr (assoc 'pb template-parameters))
	    fc     (cdr (assoc 'fc template-parameters))
	    js     (cdr (assoc 'js template-parameters))
	    is     (cdr (assoc 'is template-parameters))
	    vtherm (cdr (assoc 'vtherm template-parameters)))
	  
; change the parameters according to the model instance
	  (dolist (p (model-instance-changed-params model))
	    (if (member (car p) known-mos1-parameters)
		(set (car p) (eval (cdr p)))	; This is tricky, (car p) is a symbol
						;  definded by the above (let). This is
						;  why all the parameter names are declared
						;  as globals.
		(sim-warning (zl:format
			       nil
			       "Parameter ~a from model instance ~a is unknown, ignoring.~%"
			       (car p)
			       (model-instance-name model)))))
	  
; change the parameters according to the element definition
	  (dolist (p (mos1-params m))
	    (if (member (car p) known-mos1-parameters)
		(set (car p) (eval (cdr p)))	; This is tricky, (car p) is a symbol
						;  definded by the above (let)
		(sim-warning (zl:format
			       nil
			       "Parameter ~a from mosfet ~a is unknown, ignoring.~%"
			       (car p)
			       (mos1-name m)))))
	  
; set the real element parameters to thier proper values from the data thats been gathered
	  (if (= mtype PMOS)
	      (setf vto (- vto)))
	  (if (<= phi ZERO)
	      (sim-error (zl:format
			   nil
			   "The parameter 'phi' for element ~a, model ~a must be positive.~%"
			   (mos1-name m)
			   (model-instance-name model))))
	  #-parallel
	  (setf
	    (#.core-mos1-w core-m) w
	    l (- l (* 2 ld))
	    (#.core-mos1-l core-m) l
	    junk (if (<= (#.core-mos1-l core-m) ZERO)
		     (sim-error (zl:format
				  nil
				  "The length of mosfet ~a is less than 2 * ld.~%"
				  (mos1-name m))))
	    (core-mos1-mtype core-m) mtype
	    (#.core-mos1-phi core-m) phi
	    (#.core-mos1-fixed-vto core-m) (- vto
					    (* gamma
					       (sqrt phi)))
	    (#.core-mos1-vfb core-m) (- (#.core-mos1-fixed-vto core-m) phi)
	    (#.core-mos1-beta core-m) (* kp (/ w l))
	    (#.core-mos1-lambda core-m) lambda
	    (core-mos1-gamma core-m) gamma
	    junk (if (= tox ZERO)
		     (setf (core-mos1-cgsub core-m) ZERO)
		     (setf (core-mos1-cgsub core-m) (/ (* w
							   (#.core-mos1-l core-m)
							   EOX
							   100.0)
							tox)))
	    (core-mos1-cgd core-m) (* cgdo w)
	    (core-mos1-cgs core-m) (* cgso w)
	    (core-mos1-cgb core-m) (* cgbo l)
	    (core-mos1-cjas core-m) (* cj as)
	    (core-mos1-cjad core-m) (* cj ad)
	    (core-mos1-cjass core-m) (* cjsw ps)
	    (core-mos1-cjasd core-m) (* cjsw pd))
	  #+parallel
	  (setf
	    (pref #.core-mos1-w core-m) w
	    l (- l (* 2 ld))
	    (pref #.core-mos1-l core-m) l
	    junk (if (<= (pref #.core-mos1-l core-m) ZERO)
		     (sim-error (zl:format
				  nil
				  "The length of mosfet ~a is less than 2 * ld.~%"
				  (mos1-name m))))
	    (pref core-mos1-mtype core-m) mtype
	    (pref #.core-mos1-phi core-m) phi
	    (pref #.core-mos1-fixed-vto core-m) (- vto
						 (* gamma
						    (sqrt phi)))
	    (pref #.core-mos1-vfb core-m) (- (pref #.core-mos1-fixed-vto core-m) phi)
	    (pref #.core-mos1-beta core-m) (* kp (/ w l))
	    (pref #.core-mos1-lambda core-m) lambda
	    (pref core-mos1-gamma core-m) gamma
	    junk (if (= tox ZERO)
		     (setf (pref core-mos1-cgsub core-m) ZERO)
		     (setf (pref core-mos1-cgsub core-m) (/ (* w
								(pref #.core-mos1-l core-m)
								EOX
								100.0)
							     tox)))
	    (pref core-mos1-cgd core-m) (* cgdo w)
	    (pref core-mos1-cgs core-m) (* cgso w)
	    (pref core-mos1-cgb core-m) (* cgbo l)
	    (pref core-mos1-cjas core-m) (* cj as)
	    (pref core-mos1-cjad core-m) (* cj ad)
	    (pref core-mos1-cjass core-m) (* cjsw ps)
	    (pref core-mos1-cjasd core-m) (* cjsw pd))
; also need to create some linear capacitors and some diodes
	  ))

    (cond
      ((and (eq nd noded) (not (#+parallel pref #.core-mos1-node-d-pointp core-m)))
       #-parallel
       (setf
	 (#.core-mos1-node-d-pointp core-m) t
	 (#.core-mos1-node-d-point core-m) (node-core-nd noded))
       #+parallel
       (setf
	 (pref #.core-mos1-node-d-pointp core-m) t
	 (pref #.core-mos1-node-d-point core-m) proc))
      ((and (eq nd nodeg) (not (#+parallel pref #.core-mos1-node-g-pointp core-m)))
       #-parallel
       (setf
	 (#.core-mos1-node-g-pointp core-m) t
	 (#.core-mos1-node-g-point core-m) (node-core-nd nodeg))
       #+parallel
       (setf
	 (pref #.core-mos1-node-g-pointp core-m) t
	 (pref #.core-mos1-node-g-point core-m) proc))
      ((and (eq nd nodes) (not (#+parallel pref #.core-mos1-node-s-pointp core-m)))
       #-parallel
       (setf
	 (#.core-mos1-node-s-pointp core-m) t
	 (#.core-mos1-node-s-point core-m) (node-core-nd nodes))
       #+parallel
       (setf
	 (pref #.core-mos1-node-s-pointp core-m) t
	 (pref #.core-mos1-node-s-point core-m) proc))
      ((and (eq nd nodeb) (not (#+parallel pref #.core-mos1-node-b-pointp core-m)))
       #-parallel
       (setf
	 (#.core-mos1-node-b-pointp core-m) t
	 (#.core-mos1-node-b-point core-m) (node-core-nd nodeb))
       #+parallel
       (setf
	 (pref #.core-mos1-node-b-pointp core-m) t
	 (pref #.core-mos1-node-b-point core-m) proc))
      (t
	(sim-error "Internal error: called create-core on a device with a invalid node")))
    ))

(defun add-off-diag-mos1 (m diag off-diag off-diag-entry)
  "Adds off diagonal entries for this mos1."
  #+off-diag
  (let ((noded (mos1-node-d m))
	(nodeg (mos1-node-g m))
	(nodes (mos1-node-s m))
	(nodeb (mos1-node-b m))
	(proc nil)
	(core-m (mos1-core-mos m)))
    (cond
      ;;; (eq diag noded)
      ((and (eq diag noded) (eq off-diag nodeg)
	    (not (#+parallel pref #.core-mos1-mat-dg-valid core-m)))
       #-parallel
       (setf
	 (#.core-mos1-mat-dg-valid core-m) t
	 (#.core-mos1-mat-dg-point core-m) off-diag-entry)
       #+parallel
       (setf
         proc (allocate-processor)
	 (pref fanout-valid proc) t
	 (pref fanout-seg-forward proc) nil
	 (pref #.core-mos1-mat-dg-valid core-m) t
	 (pref #.core-mos1-mat-dg-point core-m) proc))
      ((and (eq diag noded) (eq off-diag nodes)
	    (not (#+parallel pref #.core-mos1-mat-ds-valid core-m)))
       #-parallel
       (setf
	 (#.core-mos1-mat-ds-valid core-m) t
	 (#.core-mos1-mat-ds-point core-m) off-diag-entry)
       #+parallel
       (setf
         proc (allocate-processor)
	 (pref fanout-valid proc) t
	 (pref fanout-seg-forward proc) nil
	 (pref #.core-mos1-mat-ds-valid core-m) t
	 (pref #.core-mos1-mat-ds-point core-m) proc))
      ((and (eq diag noded) (eq off-diag nodeb)
	    (not (#+parallel pref #.core-mos1-mat-db-valid core-m)))
       #-parallel
       (setf
	 (#.core-mos1-mat-db-valid core-m) t
	 (#.core-mos1-mat-db-point core-m) off-diag-entry)
       #+parallel
       (setf
         proc (allocate-processor)
	 (pref fanout-valid proc) t
	 (pref fanout-seg-forward proc) nil
	 (pref #.core-mos1-mat-db-valid core-m) t
	 (pref #.core-mos1-mat-db-point core-m) proc))

      ;;; (eq diag nodeg)
      ((and (eq diag nodeg) (eq off-diag noded)
	    (not (#+parallel pref #.core-mos1-mat-gd-valid core-m)))
       #-parallel
       (setf
	 (#.core-mos1-mat-gd-valid core-m) t
	 (#.core-mos1-mat-gd-point core-m) off-diag-entry)
       #+parallel
       (setf
         proc (allocate-processor)
	 (pref fanout-valid proc) t
	 (pref fanout-seg-forward proc) nil
	 (pref #.core-mos1-mat-gd-valid core-m) t
	 (pref #.core-mos1-mat-gd-point core-m) proc))
      ((and (eq diag nodeg) (eq off-diag nodes)
	    (not (#+parallel pref #.core-mos1-mat-gs-valid core-m)))
       #-parallel
       (setf
	 (#.core-mos1-mat-gs-valid core-m) t
	 (#.core-mos1-mat-gs-point core-m) off-diag-entry)
       #+parallel
       (setf
         proc (allocate-processor)
	 (pref fanout-valid proc) t
	 (pref fanout-seg-forward proc) nil
	 (pref #.core-mos1-mat-gs-valid core-m) t
	 (pref #.core-mos1-mat-gs-point core-m) proc))
      ((and (eq diag nodeg) (eq off-diag nodeb)
	    (not (#+parallel pref #.core-mos1-mat-gb-valid core-m)))
       #-parallel
       (setf
	 (#.core-mos1-mat-gb-valid core-m) t
	 (#.core-mos1-mat-gb-point core-m) off-diag-entry)
       #+parallel
       (setf
         proc (allocate-processor)
	 (pref fanout-valid proc) t
	 (pref fanout-seg-forward proc) nil
	 (pref #.core-mos1-mat-gb-valid core-m) t
	 (pref #.core-mos1-mat-gb-point core-m) proc))

      ;;; (eq diag nodes)
      ((and (eq diag nodes) (eq off-diag noded)
	    (not (#+parallel pref #.core-mos1-mat-sd-valid core-m)))
       #-parallel
       (setf
	 (#.core-mos1-mat-sd-valid core-m) t
	 (#.core-mos1-mat-sd-point core-m) off-diag-entry)
       #+parallel
       (setf
         proc (allocate-processor)
	 (pref fanout-valid proc) t
	 (pref fanout-seg-forward proc) nil
	 (pref #.core-mos1-mat-sd-valid core-m) t
	 (pref #.core-mos1-mat-sd-point core-m) proc))
      ((and (eq diag nodes) (eq off-diag nodeg)
	    (not (#+parallel pref #.core-mos1-mat-sg-valid core-m)))
       #-parallel
       (setf
	 (#.core-mos1-mat-sg-valid core-m) t
	 (#.core-mos1-mat-sg-point core-m) off-diag-entry)
       #+parallel
       (setf
         proc (allocate-processor)
	 (pref fanout-valid proc) t
	 (pref fanout-seg-forward proc) nil
	 (pref #.core-mos1-mat-sg-valid core-m) t
	 (pref #.core-mos1-mat-sg-point core-m) proc))
      ((and (eq diag nodes) (eq off-diag nodeb)
	    (not (#+parallel pref #.core-mos1-mat-sb-valid core-m)))
       #-parallel
       (setf
	 (#.core-mos1-mat-sb-valid core-m) t
	 (#.core-mos1-mat-sb-point core-m) off-diag-entry)
       #+parallel
       (setf
         proc (allocate-processor)
	 (pref fanout-valid proc) t
	 (pref fanout-seg-forward proc) nil
	 (pref #.core-mos1-mat-sb-valid core-m) t
	 (pref #.core-mos1-mat-sb-point core-m) proc))

      ;;; (eq diag nodeb)
      ((and (eq diag nodeb) (eq off-diag noded)
	    (not (#+parallel pref #.core-mos1-mat-bd-valid core-m)))
       #-parallel
       (setf
	 (#.core-mos1-mat-bd-valid core-m) t
	 (#.core-mos1-mat-bd-point core-m) off-diag-entry)
       #+parallel
       (setf
         proc (allocate-processor)
	 (pref fanout-valid proc) t
	 (pref fanout-seg-forward proc) nil
	 (pref #.core-mos1-mat-bd-valid core-m) t
	 (pref #.core-mos1-mat-bd-point core-m) proc))
      ((and (eq diag nodeb) (eq off-diag nodeg)
	    (not (#+parallel pref #.core-mos1-mat-bg-valid core-m)))
       #-parallel
       (setf
	 (#.core-mos1-mat-bg-valid core-m) t
	 (#.core-mos1-mat-bg-point core-m) off-diag-entry)
       #+parallel
       (setf
         proc (allocate-processor)
	 (pref fanout-valid proc) t
	 (pref fanout-seg-forward proc) nil
	 (pref #.core-mos1-mat-bg-valid core-m) t
	 (pref #.core-mos1-mat-bg-point core-m) proc))
      ((and (eq diag nodeb) (eq off-diag nodes)
	    (not (#+parallel pref #.core-mos1-mat-bs-valid core-m)))
       #-parallel
       (setf
	 (#.core-mos1-mat-bs-valid core-m) t
	 (#.core-mos1-mat-bs-point core-m) off-diag-entry)
       #+parallel
       (setf
         proc (allocate-processor)
	 (pref fanout-valid proc) t
	 (pref fanout-seg-forward proc) nil
	 (pref #.core-mos1-mat-bs-valid core-m) t
	 (pref #.core-mos1-mat-bs-point core-m) proc))
      )))

(defun find-coupling-mos1 (nd m)
  "Returns a pair of (node . value) telling how coupled that node is to nd."
  nil
#|
  (cond
    ((eq nd (mos1-node-g m))
      (list
        (if (or (node-is-dc-source (mos1-node-d m))
		(node-is-pwl-source (mos1-node-d m)))
	  nil
	  (cons (mos1-node-d m) (* alpha (core-mos1-cgd (mos1-core-mos m)))))
        (if (or (node-is-dc-source (mos1-node-s m))
		(node-is-pwl-source (mos1-node-s m)))
	  nil
	  (cons (mos1-node-s m) (* alpha (core-mos1-cgs (mos1-core-mos m)))))))
    ((eq nd (mos1-node-d m))
      (list
        (if (or (node-is-dc-source (mos1-node-g m))
		(node-is-pwl-source (mos1-node-g m)))
	  nil
	  (cons (mos1-node-g m) (+ (#.core-mos1-beta (mos1-core-mos m))
				   (* alpha (core-mos1-cgd (mos1-core-mos m))))))
        (if (or (node-is-dc-source (mos1-node-s m))
		(node-is-pwl-source (mos1-node-s m)))
	  nil
	  (cons (mos1-node-s m) (#.core-mos1-beta (mos1-core-mos m))))))
    ((eq nd (mos1-node-s m))
      (list
        (if (or (node-is-dc-source (mos1-node-d m))
		(node-is-pwl-source (mos1-node-d m)))
	  nil
	  (cons (mos1-node-d m) (#.core-mos1-beta (mos1-core-mos m))))
        (if (or (node-is-dc-source (mos1-node-g m))
		(node-is-pwl-source (mos1-node-g m)))
	  nil
	  (cons (mos1-node-g m) (+ (#.core-mos1-beta (mos1-core-mos m))
				   (* alpha (core-mos1-cgs (mos1-core-mos m))))))))
    (t    ;; bulk: no coupling to bulk for now
     nil))
|#
)

(defun mos1-fix-dc-nodes (m)
  "Fix up the nodes of this element which are connected to dc nodes."
  (if (mos1-core-mos m)
      (progn
	(if (node-is-dc-source (mos1-node-d m))
	    (setf
	      (#+parallel pref #.core-mos1-node-d-pointp (mos1-core-mos m)) nil
	      (#+parallel pref #.core-mos1-node-d-const (mos1-core-mos m))
	      (node-voltage (mos1-node-d m))))
	(if (node-is-dc-source (mos1-node-g m))
	    (setf
	      (#+parallel pref #.core-mos1-node-g-pointp (mos1-core-mos m)) nil
	      (#+parallel pref #.core-mos1-node-g-const (mos1-core-mos m))
	      (node-voltage (mos1-node-g m))))
	(if (node-is-dc-source (mos1-node-s m))
	    (setf
	      (#+parallel pref #.core-mos1-node-s-pointp (mos1-core-mos m)) nil
	      (#+parallel pref #.core-mos1-node-s-const (mos1-core-mos m))
	      (node-voltage (mos1-node-s m))))
	(if (node-is-dc-source (mos1-node-b m))
	    (setf
	      (#+parallel pref #.core-mos1-node-b-pointp (mos1-core-mos m)) nil
	      (#+parallel pref #.core-mos1-node-b-const (mos1-core-mos m))
	      (node-voltage (mos1-node-b m)))))))

(defun print-mos1 (m)
  "Prints out the data associated with a mos1."
  (zl:format t "~a  ~a ~a ~a ~a  ~a~%"
	  (mos1-name m)
	  (node-name (mos1-node-d m))
	  (node-name (mos1-node-g m))
	  (node-name (mos1-node-s m))
	  (node-name (mos1-node-b m))
	  (mos1-params m)))

#-parallel
(defun get-mos1-voltage-d (m)
  (if (#.core-mos1-node-d-pointp m)
      (core-node-voltage-n+1 (#.core-mos1-node-d-point m))
      (#.core-mos1-node-d-const m)))

#-parallel
(defun get-mos1-voltage-g (m)
  (if (#.core-mos1-node-g-pointp m)
      (core-node-voltage-n+1 (#.core-mos1-node-g-point m))
      (#.core-mos1-node-g-const m)))

#-parallel
(defun get-mos1-voltage-s (m)
  (if (#.core-mos1-node-s-pointp m)
      (core-node-voltage-n+1 (#.core-mos1-node-s-point m))
      (#.core-mos1-node-s-const m)))

#-parallel
(defun get-mos1-voltage-b (m)
  (if (#.core-mos1-node-b-pointp m)
      (core-node-voltage-n+1 (#.core-mos1-node-b-point m))
      (#.core-mos1-node-b-const m)))

#-parallel
(defun swap-d-s (m)
  "Swaps the drain and source for a mosfet because its in reverse mode."
  ; for parallel, this memory is shared, be sure not to swap it twice!
  (let ((temp-pointp (#.core-mos1-node-d-pointp m))
	 (temp-point (#.core-mos1-node-d-point m))
	 (temp-const (#.core-mos1-node-d-const m)))
    (setf
      (#.core-mos1-node-d-pointp m) (#.core-mos1-node-s-pointp m)
      (#.core-mos1-node-d-point m) (#.core-mos1-node-s-point m)
      (#.core-mos1-node-d-const m) (#.core-mos1-node-s-const m)
      (#.core-mos1-node-s-pointp m) temp-pointp
      (#.core-mos1-node-s-point m) temp-point
      (#.core-mos1-node-s-const m) temp-const))
  (let ((c))
    (setf
      c (core-mos1-cgd m)
      (core-mos1-cgd m) (core-mos1-cgs m)
      (core-mos1-cgs m) c

      c (core-mos1-cjad m)
      (core-mos1-cjad m) (core-mos1-cjas m)
      (core-mos1-cjas m) c

      c (core-mos1-cjasd m)
      (core-mos1-cjasd m) (core-mos1-cjass m)
      (core-mos1-cjass m) c)))

#-parallel
(defun eval-mos1 (m)
  ; get the voltages
  (let ((core-m (mos1-core-mos m))
	(vds ZERO)
	(vgs ZERO)
	(vbs ZERO)
	(vgst ZERO)
	(gmgs ZERO)
	(gmds ZERO)
	(gmbs ZERO)
	(ndvtdvbs ZERO)
	(temp)
	(junk))
    (if (null core-m)
	(return-from eval-mos1 (values)))
    (setf
      (#.core-mos1-ids core-m) ZERO
      (#.core-mos1-cond-d core-m) ZERO
      (#.core-mos1-cond-g core-m) ZERO
      (#.core-mos1-cond-s core-m) ZERO
      (#.core-mos1-cond-b core-m) ZERO
      (#.core-mos1-charge-d core-m) ZERO
      (#.core-mos1-charge-g core-m) ZERO
      (#.core-mos1-charge-s core-m) ZERO
      (#.core-mos1-charge-b core-m) ZERO)

    #+off-diag
    (if *use-tridiagonal*
	(setf
	  (#.core-mos1-mat-dg-value core-m) ZERO
	  (#.core-mos1-mat-ds-value core-m) ZERO
	  (#.core-mos1-mat-db-value core-m) ZERO
	  
	  (#.core-mos1-mat-gd-value core-m) ZERO
	  (#.core-mos1-mat-gs-value core-m) ZERO
	  (#.core-mos1-mat-gb-value core-m) ZERO
	  
	  (#.core-mos1-mat-sd-value core-m) ZERO
	  (#.core-mos1-mat-sg-value core-m) ZERO
	  (#.core-mos1-mat-sb-value core-m) ZERO
	  
	  (#.core-mos1-mat-bd-value core-m) ZERO
	  (#.core-mos1-mat-bg-value core-m) ZERO
	  (#.core-mos1-mat-bs-value core-m) ZERO))

    (let ((vd (get-mos1-voltage-d core-m))
	  (vs (get-mos1-voltage-s core-m)))
      (setf vds (- vd vs))
      (if (= (core-mos1-mtype core-m) PMOS)
	  (setf vds (- vds)))
      (if (< vds ZERO)
	  (progn
	    (swap-d-s core-m)
	    (setf
	      vds (- vds)
	      vs vd)))				; don't need vd any more
      (setf
	vgs (- (get-mos1-voltage-g core-m) vs)
	vbs (- (get-mos1-voltage-b core-m) vs))
      (if (= (core-mos1-mtype core-m) PMOS)
	  (setf
	    vgs (- vgs)
	    vbs (- vbs))))

    ; calculate thershold voltage
    (setf
      ndvtdvbs (- (#.core-mos1-phi core-m) vbs))
    (if (< ndvtdvbs vabs)
	(setf
	  vgst (- vgs
		  (#.core-mos1-fixed-vto core-m)
		  (* (core-mos1-gamma core-m) (sqrt vabs)))
	  ndvtdvbs ZERO)
	(setf
	  ndvtdvbs (sqrt ndvtdvbs)
	  vgst (- vgs (+ (#.core-mos1-fixed-vto core-m)
			 (* (core-mos1-gamma core-m)
			    ndvtdvbs)))
	  ndvtdvbs (/ (core-mos1-gamma core-m)
		       (+ ndvtdvbs ndvtdvbs))))

    ; calculate the drain current
    (if (> vgst ZERO)
	(progn
	  (setf gmgs (* (#.core-mos1-beta core-m) (+ 1 (* (#.core-mos1-lambda core-m) vds))))
	  (if (< vgst vds)			; stauration region
	      (setf
		gmgs (* gmgs vgst)
		(#.core-mos1-ids core-m) (* 0.5 gmgs vgst)
		gmbs (* ndvtdvbs gmgs)
		gmds (* 0.5 (#.core-mos1-beta core-m) vgst vgst (#.core-mos1-lambda core-m)))
	      (setf				; linear region
		(#.core-mos1-ids core-m) (* gmgs vds (- vgst (* 0.5 vds)))
		gmds (+ (* gmgs (- vgst vds))
			(* (#.core-mos1-beta core-m)
			   (#.core-mos1-lambda core-m)
			   vds
			   (- vgst (* 0.5 vds))))
		gmgs (* gmgs vds)
		gmbs (* ndvtdvbs gmgs)))

	  ; send the currents and conductances back where they go
	  ; ids is the current from drain to source, save it
	  ; accumulate the conductances
	  (if (= (core-mos1-mtype core-m) PMOS)
	      (setf
		(#.core-mos1-ids core-m) (- (#.core-mos1-ids core-m))))
	  (if (not (and (#.core-mos1-node-d-pointp core-m)
			(#.core-mos1-node-s-pointp core-m)
			(eq (#.core-mos1-node-d-point core-m)
			    (#.core-mos1-node-s-point core-m))))
	      (setf
		(#.core-mos1-cond-d core-m) gmds	; don't add since cond-d was previously zero
		(#.core-mos1-cond-s core-m) (+ gmds gmbs gmgs)))
	  (setf
	    (#.core-mos1-mat-db-value core-m) gmbs
	    (#.core-mos1-mat-dg-value core-m) gmgs
	    (#.core-mos1-mat-ds-value core-m) (- (+ gmgs gmbs gmds))

	    (#.core-mos1-mat-sb-value core-m) (- gmbs)
	    (#.core-mos1-mat-sg-value core-m) (- gmgs)
	    (#.core-mos1-mat-sd-value core-m) (- gmds))))
    

    (if debug-mos1
	  (progn
	    (zl:format t "voltages for mosfets are:~%")
	    (zl:format t " processor  drain     gate     source      bulk   ids~%")
	    (zl:format t "~10d  ~8f  ~8f  ~8f  ~8f  ~8f~%"
		       core-m
		       (get-mos1-voltage-d core-m)
		       (get-mos1-voltage-g core-m)
		       (get-mos1-voltage-s core-m)
		       (get-mos1-voltage-b core-m)
		       (#.core-mos1-ids core-m))))

; evaluate the linear parasitic capacitors
;   do the drain gate overlap
    (if (and (or (#.core-mos1-node-d-pointp core-m)
		 (#.core-mos1-node-g-pointp core-m))
		 (not (eq (#.core-mos1-node-d-point core-m)
			  (#.core-mos1-node-g-point core-m))))
	(progn
	  (setf
	    temp (* alpha (core-mos1-cgd core-m))
	    (#.core-mos1-cond-d core-m) (+ (#.core-mos1-cond-d core-m) temp)
	    (#.core-mos1-cond-g core-m) (+ (#.core-mos1-cond-g core-m) temp))
	  #+off-diag
	  (if *use-tridiagonal*
	      (setf
	        (#.core-mos1-mat-dg-value core-m) (- (#.core-mos1-mat-dg-value core-m) temp)
		(#.core-mos1-mat-gd-value core-m) (- (#.core-mos1-mat-gd-value core-m) temp)))

	  (setf
	    temp (* (core-mos1-cgd core-m) (- vds vgs))
	    junk (if (= (core-mos1-mtype core-m) PMOS)
		     (setf temp (- temp)))
	    (#.core-mos1-charge-d core-m) (+ (#.core-mos1-charge-d core-m) temp)
	    (#.core-mos1-charge-g core-m) (- (#.core-mos1-charge-g core-m) temp))))

;   do the source gate overlap
    (if (and (or (#.core-mos1-node-s-pointp core-m)
		 (#.core-mos1-node-g-pointp core-m))
		 (not (eq (#.core-mos1-node-s-point core-m)
			  (#.core-mos1-node-g-point core-m))))
	(progn
	  (setf
	    temp (* alpha (core-mos1-cgs core-m))
	    (#.core-mos1-cond-s core-m) (+ (#.core-mos1-cond-s core-m) temp)
	    (#.core-mos1-cond-g core-m) (+ (#.core-mos1-cond-g core-m) temp))
	  #+off-diag
	  (if *use-tridiagonal*
	      (setf
	        (#.core-mos1-mat-sg-value core-m) (- (#.core-mos1-mat-sg-value core-m) temp)
		(#.core-mos1-mat-gs-value core-m) (- (#.core-mos1-mat-gs-value core-m) temp)))

	  (setf
	    temp (* (core-mos1-cgs core-m) vgs)
	    junk (if (= (core-mos1-mtype core-m) PMOS)
		     (setf temp (- temp)))
	    (#.core-mos1-charge-s core-m) (- (#.core-mos1-charge-s core-m) temp)
	    (#.core-mos1-charge-g core-m) (+ (#.core-mos1-charge-g core-m) temp))))

;   do the bulk gate overlap and cgsub
    (if (and (or (#.core-mos1-node-b-pointp core-m)
		 (#.core-mos1-node-g-pointp core-m))
		 (not (eq (#.core-mos1-node-b-point core-m)
			  (#.core-mos1-node-g-point core-m))))
	(progn
	  (setf
	    temp (* alpha (+ (core-mos1-cgb core-m) (* 0.667 (core-mos1-cgsub core-m))))
	    (#.core-mos1-cond-b core-m) (+ (#.core-mos1-cond-b core-m) temp)
	    (#.core-mos1-cond-g core-m) (+ (#.core-mos1-cond-g core-m) temp))
	  #+off-diag
	  (if *use-tridiagonal*
	      (setf
	        (#.core-mos1-mat-bg-value core-m) (- (#.core-mos1-mat-bg-value core-m) temp)
		(#.core-mos1-mat-gb-value core-m) (- (#.core-mos1-mat-gb-value core-m) temp)))

	  (setf
	    temp (* (+ (core-mos1-cgb core-m) (* 0.667 (core-mos1-cgsub core-m))) (- vbs vgs))
	    junk (if (= (core-mos1-mtype core-m) PMOS)
		     (setf temp (- temp)))
	    (#.core-mos1-charge-b core-m) (+ (#.core-mos1-charge-b core-m) temp)
	    (#.core-mos1-charge-g core-m) (- (#.core-mos1-charge-g core-m) temp))))

;   do source bulk junction ( diode ) should be non-linear
    (if (and (or (#.core-mos1-node-s-pointp core-m)
		 (#.core-mos1-node-b-pointp core-m))
		 (not (eq (#.core-mos1-node-s-point core-m)
			  (#.core-mos1-node-b-point core-m))))
	(progn
	  (setf
	    temp (* alpha (+ (core-mos1-cjas core-m) (core-mos1-cjass core-m)))
	    (#.core-mos1-cond-s core-m) (+ (#.core-mos1-cond-s core-m) temp)
	    (#.core-mos1-cond-b core-m) (+ (#.core-mos1-cond-b core-m) temp))
	  #+off-diag
	  (if *use-tridiagonal*
	      (setf
	        (#.core-mos1-mat-sb-value core-m) (- (#.core-mos1-mat-sb-value core-m) temp)
		(#.core-mos1-mat-bs-value core-m) (- (#.core-mos1-mat-bs-value core-m) temp)))

	  (setf
	    temp (* (+ (core-mos1-cjas core-m) (core-mos1-cjass core-m)) vbs)
	    junk (if (= (core-mos1-mtype core-m) PMOS)
		     (setf temp (- temp)))
	    (#.core-mos1-charge-s core-m) (- (#.core-mos1-charge-s core-m) temp)
	    (#.core-mos1-charge-b core-m) (+ (#.core-mos1-charge-b core-m) temp))))

;   do drain bulk junction ( diode ) should be non-linear
    (if (and (or (#.core-mos1-node-d-pointp core-m)
		 (#.core-mos1-node-b-pointp core-m))
		 (not (eq (#.core-mos1-node-d-point core-m)
			  (#.core-mos1-node-b-point core-m))))
	(progn
	  (setf
	    temp (* alpha (+ (core-mos1-cjad core-m) (core-mos1-cjasd core-m)))
	    (#.core-mos1-cond-d core-m) (+ (#.core-mos1-cond-d core-m) temp)
	    (#.core-mos1-cond-b core-m) (+ (#.core-mos1-cond-b core-m) temp))
	  #+off-diag
	  (if *use-tridiagonal*
	      (setf
	        (#.core-mos1-mat-db-value core-m) (- (#.core-mos1-mat-db-value core-m) temp)
		(#.core-mos1-mat-bd-value core-m) (- (#.core-mos1-mat-bd-value core-m) temp)))

	  (setf
	    temp (* (+ (core-mos1-cjad core-m) (core-mos1-cjasd core-m)) (- vds vbs))
	    junk (if (= (core-mos1-mtype core-m) PMOS)
		     (setf temp (- temp)))
	    (#.core-mos1-charge-d core-m) (+ (#.core-mos1-charge-d core-m) temp)
	    (#.core-mos1-charge-b core-m) (- (#.core-mos1-charge-b core-m) temp))))
    

#||
NOTE: this charge model is not implemented correctly, I am putting this off for now.
    ; calculate the capacitances and charges
    (if (not (= (core-mos1-cgsub core-m) ZERO))
	
	(let ((qg ZERO)
	      (dqdvds ZERO)
	      (dqdvgst ZERO)
	      (temp1 ZERO)
	      (temp2 ZERO)
	      (temp3 ZERO))
	  (if (< vgst ZERO)			; Subthresh or Accum region.
	      (progn
		(setf
		  temp1 (- vgs vbs))		; compute vgb.
		(if (or
		      (and (> (core-mos1-gamma core-m) ZERO)
			   (>= temp1 (#.core-mos1-vfb core-m)))
		      (< temp1 (#.core-mos1-vfb core-m)))
		    (progn
		      (if (<= temp1 (#.core-mos1-vfb core-m))	; Accumulation.
			  (setf
			    qg (* (core-mos1-cgsub core-m)
				  (- temp1 (#.core-mos1-vfb core-m))))
			  (if (= (core-mos1-mtype core-m) PMOS)
			      (setf qg (- qg)))
			  (setf
			    temp1 (* (core-mos1-cgsub core-m) alpha))
			  (setf		; Subthresh.
			    temp1 (sqrt (+ (* (core-mos1-gamma core-m)
					      (core-mos1-gamma core-m))
					   (* 4.0 (- temp1 (#.core-mos1-vfb core-m)))))
			    qg (* 0.5
				  (core-mos1-gamma core-m)
				  (core-mos1-cgsub core-m)
				  (- temp1 (core-mos1-gamma core-m))))
			  (if (= (core-mos1-mtype core-m) PMOS)
			      (setf qg (-qg)))
			  (setf
			    temp1 (/
				    (* alpha
				       (core-mos1-gamma core-m)
				       (core-mos1-cgsub core-m))
				    temp1)))
		      (setf
			(#.core-mos1-charge-g core-m) (+ (#.core-mos1-charge-g core-m) qg)
			(#.core-mos1-cond-g core-m) (+ (#.core-mos1-cond-g core-m) temp1)
			(#.core-mos1-charge-b core-m) (- (#.core-mos1-charge-b core-m) qg)
			(#.core-mos1-cond-b core-m) (+ (#.core-mos1-cond-b core-m) temp1)))))
	      (if (< vgst vds)			; Saturation region.
		  (progn
		    (setf
		      temp1 (/ (core-mos1-cgsub core-m) 1.5)
		      temp2 (* alpha temp1)

		      ; gate charge
		      temp (* (core-mos1-cgsub core-m)
			      (- (- vgs (#.core-mos1-fixed-vto core-m))
				 (/ vgst 3.0))))
		    (if (= (core-mos1-mtype core-m) PMOS)
			(setf temp (- temp)))
		    (setf
		      (#.core-mos1-charge-g core-m) (+ (#.core-mos1-charge-g core-m) temp)
		      (#.core-mos1-cond-g core-m) (+ (#.core-mos1-cond-g core-m) temp2)

		      ; source
		      temp (* temp1 vgst))
		    (if (= (core-mos1-mtype core-m) PMOS)
			(setf temp (- temp)))
		    (setf
		      (#.core-mos1-charge-s core-m) (- (#.core-mos1-charge-s core-m) temp)
		      (#.core-mos1-cond-s core-m) (+ (#.core-mos1-cond-s core-m)
						   (+ temp2 (* temp2 ndvtdvbs)))

		      ; bulk
		      temp (* (core-mos1-cgsub core-m)
			      (+ (- vgst vgs) (#.core-mos1-fixed-vto core-m))))
		    (if (= (core-mos1-mtype core-m) PMOS)
			(setf temp (- temp)))
		    (setf
		      (#.core-mos1-charge-b core-m) (+ (#.core-mos1-charge-b core-m) temp)
		      (#.core-mos1-cond-b core-m) (+ (#.core-mos1-cond-b core-m)
						   (* 1.5 temp2 ndvtdvbs))))
		  (progn
		    (setf
		      temp1 (/ (+ (- (+ vgst vgst) vds) 1e-12))
		      temp2 (* temp1 temp1 vds)
		      temp3 (* vds vds temp1)
		      temp1 (* alpha (core-mos1-cgsub core-m))

		      ; gate
		      temp (* (core-mos1-cgsub core-m)
			      (+ (- (- vgs (#.core-mos1-fixed-vto core-m))
				    (* 0.5 vds))
				 (/ temp3 6.0))))
		    (if (= (core-mos1-mtype core-m) PMOS)
			(setf temp (- temp)))
		    (setf
		      (#.core-mos1-charge-g core-m) (+ (#.core-mos1-charge-g core-m) temp)
;		      dqdvds (* temp1
;				(- (* temp2
;				      (/ (- (* 4.0 vgst) vds)
;					  6.0))
;				   0.5))
		      dqdvgst (- (/ (* temp1 vds temp2) 3.0))
		      (#.core-mos1-cond-g core-m) (+ temp1 dqdvgst)

		      ; drain
		      temp (* (core-mos1-cgsub core-m)
			      (- (- (* 0.75 vds)
				    (* 0.5 vgst))
				 (* 0.25 temp3))))
		    (if (= (core-mos1-mtype core-m) PMOS)
			(setf temp (- temp)))
		    (setf
		      (#.core-mos1-charge-d core-m) (+ (#.core-mos1-charge-d core-m) temp)
		      dqdvds (* temp1
				(- 0.75
				   (* 0.25
				      temp2
				      (- (* 4.0 vgst) 
					 vds))))
;		      dqdvgst = temp1 * (0.5*vds*temp2 - 0.5);
		      (#.core-mos1-cond-d core-m) (+ (#.core-mos1-cond-d core-m) dqdvds)

		      ; source
		      temp (* (core-mos1-cgsub core-m)
			      (- (- (/ temp3 12.0)
				    (* 0.25 vds))
				 (* 0.5 vgst))))
		    (if (= (core-mos1-mtype core-m) PMOS)
			(setf temp (- temp)))
		    (setf
		      (#.core-mos1-charge-s core-m) (+ (#.core-mos1-charge-s core-m) temp)
		      dqdvds (* temp1
				(- (/ (* temp2
					  (- (* 4.0 vgst)
					     vds))
				       12.0)
				   0.25))
		      dqdvgst (* temp1
				 (- (/ (* vds temp2)
					6.0)
				    0.5))
		      (#.core-mos1-cond-s core-m) (- (#.core-mos1-cond-s core-m) (+ (* dqdvgst
					     (+ 1.0 ndvtdvbs))
					  dqdvds))

		      ; bulk
		      temp (* (core-mos1-cgsub core-m)
			      (+ (- vgst vgs)
				 (#.core-mos1-fixed-vto core-m))))
		    (if (= (core-mos1-mtype core-m) PMOS)
			(setf temp (- temp)))
		    (setf
		      (#.core-mos1-charge-b core-m) (+ (#.core-mos1-charge-b core-m) temp)
		      (#.core-mos1-cond-b core-m) (+ (#.core-mos1-cond-b core-m)
						   (* temp1 ndvtdvbs))))
		  )
	      )
	  )
	)
    ; send the results back
||#    
    (if (#.core-mos1-node-d-pointp core-m)
	(if (not (core-node-is-source (#.core-mos1-node-d-point core-m)))
	    (setf
	      (core-node-jacobian (#.core-mos1-node-d-point core-m))
	       (+ (core-node-jacobian (#.core-mos1-node-d-point core-m))
		  (#.core-mos1-cond-d core-m))
	      (core-node-current (#.core-mos1-node-d-point core-m))
	       (+ (core-node-current (#.core-mos1-node-d-point core-m))
		  (#.core-mos1-ids core-m) )
	      (core-node-charge (#.core-mos1-node-d-point core-m))
	       (+ (core-node-charge (#.core-mos1-node-d-point core-m))
		  (#.core-mos1-charge-d core-m)))))
    
    (if (#.core-mos1-node-s-pointp core-m)
	(if (not (core-node-is-source (#.core-mos1-node-s-point core-m)))
	    (setf
	      (core-node-jacobian (#.core-mos1-node-s-point core-m))
	       (+ (core-node-jacobian (#.core-mos1-node-s-point core-m))
		  (#.core-mos1-cond-s core-m))
	      (core-node-current (#.core-mos1-node-s-point core-m))
	       (- (core-node-current (#.core-mos1-node-s-point core-m))
		  (#.core-mos1-ids core-m) )
	      (core-node-charge (#.core-mos1-node-s-point core-m))
	       (+ (core-node-charge (#.core-mos1-node-s-point core-m))
		  (#.core-mos1-charge-s core-m)))))
    
    (if (#.core-mos1-node-g-pointp core-m)
	(if (not (core-node-is-source (#.core-mos1-node-g-point core-m)))
	    (setf
	      (core-node-jacobian (#.core-mos1-node-g-point core-m))
	       (+ (core-node-jacobian (#.core-mos1-node-g-point core-m))
		  (#.core-mos1-cond-g core-m))
	      (core-node-charge (#.core-mos1-node-g-point core-m))
	       (+ (core-node-charge (#.core-mos1-node-g-point core-m))
		  (#.core-mos1-charge-g core-m)))))
    
    (if (#.core-mos1-node-b-pointp core-m)
	(if (not (core-node-is-source (#.core-mos1-node-b-point core-m)))
	    (setf
	      (core-node-jacobian (#.core-mos1-node-b-point core-m))
	       (+ (core-node-jacobian (#.core-mos1-node-b-point core-m))
		  (#.core-mos1-cond-b core-m))
	      (core-node-charge (#.core-mos1-node-b-point core-m))
	       (+ (core-node-charge (#.core-mos1-node-b-point core-m))
		  (#.core-mos1-charge-b core-m)))))

; six of these sends are unnecessary if cgsub equals zero.

    #+off-diag
    (if *use-tridiagonal*
	(progn
	  ;; the 'd' row
	  (if (#.core-mos1-mat-dg-valid core-m)
	      (let ((off-diag-entry (#.core-mos1-mat-dg-point core-m))
		    (node (mos1-node-d m)))
		(if (core-off-diag-lower off-diag-entry)
		    (setf (aref *lower-diag* (node-index node))
			  (+ (aref *lower-diag* (node-index node))
			     (core-mos1-mat-dg-value core-m)))
		  (setf (aref *upper-diag* (node-index node))
			(+ (aref *upper-diag* (node-index node))
			   (core-mos1-mat-dg-value core-m))))))
	  
	  (if (#.core-mos1-mat-ds-valid core-m)
	      (let ((off-diag-entry (#.core-mos1-mat-ds-point core-m))
		    (node (mos1-node-d m)))
		(if (core-off-diag-lower off-diag-entry)
		    (setf (aref *lower-diag* (node-index node))
			  (+ (aref *lower-diag* (node-index node))
			     (core-mos1-mat-ds-value core-m)))
		  (setf (aref *upper-diag* (node-index node))
			(+ (aref *upper-diag* (node-index node))
			   (core-mos1-mat-ds-value core-m))))))
	  
	  (if (#.core-mos1-mat-db-valid core-m)
	      (let ((off-diag-entry (#.core-mos1-mat-db-point core-m))
		    (node (mos1-node-d m)))
		(if (core-off-diag-lower off-diag-entry)
		    (setf (aref *lower-diag* (node-index node))
			  (+ (aref *lower-diag* (node-index node))
			     (core-mos1-mat-db-value core-m)))
		  (setf (aref *upper-diag* (node-index node))
			(+ (aref *upper-diag* (node-index node))
			   (core-mos1-mat-db-value core-m))))))
	  
	  
	  ;; the 'g' row
	  (if (#.core-mos1-mat-gd-valid core-m)
	      (let ((off-diag-entry (#.core-mos1-mat-gd-point core-m))
		    (node (mos1-node-g m)))
		(if (core-off-diag-lower off-diag-entry)
		    (setf (aref *lower-diag* (node-index node))
			  (+ (aref *lower-diag* (node-index node))
			     (core-mos1-mat-gd-value core-m)))
		  (setf (aref *upper-diag* (node-index node))
			(+ (aref *upper-diag* (node-index node))
			   (core-mos1-mat-gd-value core-m))))))
	  
	  (if (#.core-mos1-mat-gs-valid core-m)
	      (let ((off-diag-entry (#.core-mos1-mat-gs-point core-m))
		    (node (mos1-node-g m)))
		(if (core-off-diag-lower off-diag-entry)
		    (setf (aref *lower-diag* (node-index node))
			  (+ (aref *lower-diag* (node-index node))
			     (core-mos1-mat-gs-value core-m)))
		  (setf (aref *upper-diag* (node-index node))
			(+ (aref *upper-diag* (node-index node))
			   (core-mos1-mat-gs-value core-m))))))
	  
	  (if (#.core-mos1-mat-gb-valid core-m)
	      (let ((off-diag-entry (#.core-mos1-mat-gb-point core-m))
		    (node (mos1-node-g m)))
		(if (core-off-diag-lower off-diag-entry)
		    (setf (aref *lower-diag* (node-index node))
			  (+ (aref *lower-diag* (node-index node))
			     (core-mos1-mat-gb-value core-m)))
		  (setf (aref *upper-diag* (node-index node))
			(+ (aref *upper-diag* (node-index node))
			   (core-mos1-mat-gb-value core-m))))))
	  
	  
	  ;; the 's' row
	  (if (#.core-mos1-mat-sd-valid core-m)
	      (let ((off-diag-entry (#.core-mos1-mat-sd-point core-m))
		    (node (mos1-node-s m)))
		(if (core-off-diag-lower off-diag-entry)
		    (setf (aref *lower-diag* (node-index node))
			  (+ (aref *lower-diag* (node-index node))
			     (core-mos1-mat-sd-value core-m)))
		  (setf (aref *upper-diag* (node-index node))
			(+ (aref *upper-diag* (node-index node))
			   (core-mos1-mat-sd-value core-m))))))
	  
	  (if (#.core-mos1-mat-sg-valid core-m)
	      (let ((off-diag-entry (#.core-mos1-mat-sg-point core-m))
		    (node (mos1-node-s m)))
		(if (core-off-diag-lower off-diag-entry)
		    (setf (aref *lower-diag* (node-index node))
			  (+ (aref *lower-diag* (node-index node))
			     (core-mos1-mat-sg-value core-m)))
		  (setf (aref *upper-diag* (node-index node))
			(+ (aref *upper-diag* (node-index node))
			   (core-mos1-mat-sg-value core-m))))))
	  
	  (if (#.core-mos1-mat-sb-valid core-m)
	      (let ((off-diag-entry (#.core-mos1-mat-sb-point core-m))
		    (node (mos1-node-s m)))
		(if (core-off-diag-lower off-diag-entry)
		    (setf (aref *lower-diag* (node-index node))
			  (+ (aref *lower-diag* (node-index node))
			     (core-mos1-mat-sb-value core-m)))
		  (setf (aref *upper-diag* (node-index node))
			(+ (aref *upper-diag* (node-index node))
			   (core-mos1-mat-sb-value core-m))))))
	  
	  
	  ;; the 'b' row
	  (if (#.core-mos1-mat-bd-valid core-m)
	      (let ((off-diag-entry (#.core-mos1-mat-bd-point core-m))
		    (node (mos1-node-d m)))
		(if (core-off-diag-lower off-diag-entry)
		    (setf (aref *lower-diag* (node-index node))
			  (+ (aref *lower-diag* (node-index node))
			     (core-mos1-mat-bd-value core-m)))
		  (setf (aref *upper-diag* (node-index node))
			(+ (aref *upper-diag* (node-index node))
			   (core-mos1-mat-bd-value core-m))))))
	  
	  (if (#.core-mos1-mat-bg-valid core-m)
	      (let ((off-diag-entry (#.core-mos1-mat-bg-point core-m))
		    (node (mos1-node-d m)))
		(if (core-off-diag-lower off-diag-entry)
		    (setf (aref *lower-diag* (node-index node))
			  (+ (aref *lower-diag* (node-index node))
			     (core-mos1-mat-bg-value core-m)))
		  (setf (aref *upper-diag* (node-index node))
			(+ (aref *upper-diag* (node-index node))
			   (core-mos1-mat-bg-value core-m))))))
	  
	  (if (#.core-mos1-mat-bs-valid core-m)
	      (let ((off-diag-entry (#.core-mos1-mat-bs-point core-m))
		    (node (mos1-node-d m)))
		(if (core-off-diag-lower off-diag-entry)
		    (setf (aref *lower-diag* (node-index node))
			  (+ (aref *lower-diag* (node-index node))
			     (core-mos1-mat-bs-value core-m)))
		  (setf (aref *upper-diag* (node-index node))
			(+ (aref *upper-diag* (node-index node))
			   (core-mos1-mat-bs-value core-m))))))
	  ))
    )
  )

#+parallel
(defun get-mos1-voltage-d ()
  (*when (not!! #.core-mos1-node-d-pointp)
    (*set #.core-mos1-node-d-voltage #.core-mos1-node-d-const)))

#+parallel
(defun get-mos1-voltage-g ()
  (*when (not!! #.core-mos1-node-g-pointp)
    (*set #.core-mos1-node-g-voltage #.core-mos1-node-g-const)))

#+parallel
(defun get-mos1-voltage-s ()
  (*when (not!! #.core-mos1-node-s-pointp)
    (*set #.core-mos1-node-s-voltage #.core-mos1-node-s-const)))

#+parallel
(defun get-mos1-voltage-b ()
  (*when (not!! #.core-mos1-node-b-pointp)
    (*set #.core-mos1-node-b-voltage #.core-mos1-node-b-const)))

#+parallel
(defun swap-d-s ()
  "Swaps the drain and source for a mosfet because its in reverse mode."
  (*let
    ((temp-pointp)
     (temp-point)
     (temp-const)
     (temp-volt)
     (temp-eq1)
     (temp-eq2))
    (declare (type (pvar boolean) temp-pointp))
    (declare (type (pvar (unsigned-byte *pointer-length*)) temp-point))
    (declare (type (pvar big-float) temp-const))
    (declare (type (pvar big-float) temp-volt))
    (declare (type (pvar boolean) temp-eq1))
    (declare (type (pvar boolean) temp-eq2))

    (*set temp-pointp #.core-mos1-node-d-pointp)
    (*set temp-point #.core-mos1-node-d-point)
    (*set temp-const #.core-mos1-node-d-const)
    (*set temp-volt #.core-mos1-node-d-voltage)
    (*set temp-eq1 core-mos1-equal-dg)
    (*set temp-eq2 core-mos1-equal-db)

    (*set #.core-mos1-node-d-pointp #.core-mos1-node-s-pointp)
    (*set #.core-mos1-node-d-point #.core-mos1-node-s-point)
    (*set #.core-mos1-node-d-const #.core-mos1-node-s-const)
    (*set #.core-mos1-node-d-voltage #.core-mos1-node-s-voltage)
    (*set core-mos1-equal-dg core-mos1-equal-sg)
    (*set core-mos1-equal-db core-mos1-equal-sb)

    (*set #.core-mos1-node-s-pointp temp-pointp)
    (*set #.core-mos1-node-s-point temp-point)
    (*set #.core-mos1-node-s-const temp-const)
    (*set #.core-mos1-node-s-voltage temp-volt)
    (*set core-mos1-equal-sg temp-eq1)
    (*set core-mos1-equal-sb temp-eq2))

  (*let (c)
    (declare (type (pvar big-float) c))
    (*set c core-mos1-cgd)
    (*set core-mos1-cgd core-mos1-cgs)
    (*set core-mos1-cgs c)

    (*set c core-mos1-cjad)
    (*set core-mos1-cjad core-mos1-cjas)
    (*set core-mos1-cjas c)

    (*set c core-mos1-cjasd)
    (*set core-mos1-cjasd core-mos1-cjass)
    (*set core-mos1-cjass c)))

#+parallel
(defun eval-mos1 ()
  (*select-type (core-mos1)
    ; get the voltages
    (*let
      ((vds (!! zero))
       (vgs (!! zero))
       (vbs (!! zero))
       (vgst (!! zero))
       (gmgs (!! zero))
       (gmds (!! zero))
;       (gmbs (!! zero))
       (ndvtdvbs (!! zero))
       (temp (!! zero)))
      (declare (type (pvar big-float) vds))
      (declare (type (pvar big-float) vgs))
      (declare (type (pvar big-float) vbs))
      (declare (type (pvar big-float) vgst))
      (declare (type (pvar big-float) gmgs))
      (declare (type (pvar big-float) gmds))
;      (declare (type (pvar big-float) gmbs))
      (declare (type (pvar big-float) ndvtdvbs))
      (declare (type (pvar big-float) temp))

      (*set #.core-mos1-ids (!! ZERO))
      (*set #.core-mos1-cond-d (!! ZERO))
      (*set #.core-mos1-cond-g (!! ZERO))
      (*set #.core-mos1-cond-s (!! ZERO))
      (*set #.core-mos1-cond-b (!! ZERO))
      (*set #.core-mos1-charge-d (!! ZERO))
      (*set #.core-mos1-charge-g (!! ZERO))
      (*set #.core-mos1-charge-s (!! ZERO))
      (*set #.core-mos1-charge-b (!! ZERO))

      #+off-diag
      (if *use-tridiagonal*
	  (progn
	    (*set #.core-mos1-mat-dg-value (!! ZERO))
	    (*set #.core-mos1-mat-ds-value (!! ZERO))
	    (*set #.core-mos1-mat-db-value (!! ZERO))
	    
	    (*set #.core-mos1-mat-gd-value (!! ZERO))
	    (*set #.core-mos1-mat-gs-value (!! ZERO))
	    (*set #.core-mos1-mat-gb-value (!! ZERO))
	    
	    (*set #.core-mos1-mat-sd-value (!! ZERO))
	    (*set #.core-mos1-mat-sg-value (!! ZERO))
	    (*set #.core-mos1-mat-sb-value (!! ZERO))
	    
	    (*set #.core-mos1-mat-bd-value (!! ZERO))
	    (*set #.core-mos1-mat-bg-value (!! ZERO))
	    (*set #.core-mos1-mat-bs-value (!! ZERO))))


      (get-mos1-voltage-d)
      (get-mos1-voltage-s)
      (get-mos1-voltage-g)
      (get-mos1-voltage-b)

;      (*set vds (!! 0.0))
;      (dbg)
;      (zl:format t "Vds is ~a~%" (pref vds 11))
      (*set vds (-!! #.core-mos1-node-d-voltage #.core-mos1-node-s-voltage))
;      (zl:format t "Vds is ~a~%" (pref vds 1))
;      (zl:dbg)

      (*when (=!! core-mos1-mtype (!! PMOS))
	(*set vds (-!! vds)))
;      (dbg)
      (*when (float-minusp!! vds)
	(progn
;	  (if (*or t!!)
;		   (zl:format t "swapping some pointers~%"))
	  (swap-d-s)
	  (*set vds (-!! vds))))
      (*set vgs (-!! #.core-mos1-node-g-voltage #.core-mos1-node-s-voltage))
      (*set vbs (-!! #.core-mos1-node-b-voltage #.core-mos1-node-s-voltage))
      (*when (=!! core-mos1-mtype (!! PMOS))
	(*set vgs (-!! vgs))
	(*set vbs (-!! vbs)))

;     calculate thershold voltage
      (*set ndvtdvbs (-!! #.core-mos1-phi vbs))
      (*if (<!! ndvtdvbs (!! vabs))
	   (progn
	     (*set vgst (-!! vgs #.core-mos1-fixed-vto))	; not quite what Relax2 does
	     (*set ndvtdvbs (!! ZERO)))
	   (progn
	     (*set ndvtdvbs (sqrt!! ndvtdvbs))
	     (*set vgst (-!! vgs (+!! #.core-mos1-fixed-vto
				      (*!! core-mos1-gamma ndvtdvbs))))
	     (*set ndvtdvbs (/!! core-mos1-gamma
				  (+!! ndvtdvbs ndvtdvbs)))))
;      (dbg)
;     calculate the drain current
      (*when (float-plusp!! vgst)
	(progn
	  (*set gmgs (*!! #.core-mos1-beta (+!! (*!! #.core-mos1-lambda vds)
						 (!! 1.0))))
;	  (dbg)
	  (*if (<!! vgst vds)			; stauration region
	       (progn
		 (*set gmgs (*!! gmgs vgst))
		 (*set #.core-mos1-ids (*!! (!! 0.5) gmgs vgst))
		 (*set gmds (*!! (!! 0.5) #.core-mos1-beta vgst vgst #.core-mos1-lambda)))
	       (progn					; linear region
		 (*set #.core-mos1-ids (*!! gmgs vds (-!! vgst
							  (*!! (!! 0.5) vds))))
;	         (dbg)
		 (*set gmds (+!! (*!! gmgs (-!! vgst vds))
				 (*!! #.core-mos1-beta
				      #.core-mos1-lambda
				      vds
				      (-!! vgst (*!! (!! 0.5) vds)))))
;	         (dbg)
		 (*set gmgs (*!! gmgs vds))))
;	  (dbg)
;	  (setf
;	    gmbs (*!! ndvtdvbs gmgs))
	  
	  ; save the currents and conductances
	  ; ids is the current from drain to source, save it
	  ; accumulate the conductances
	  (*when (=!! core-mos1-mtype (!! PMOS))
	    (*set #.core-mos1-ids (-!! #.core-mos1-ids)))
	  (*when (not!! (and!! #.core-mos1-node-d-pointp
			       #.core-mos1-node-s-pointp
			       core-mos1-equal-ds))
	    (*set #.core-mos1-cond-d gmds)	; don't add since cond-d was previously zero
	    (*set #.core-mos1-cond-s (+!! gmds gmbs gmgs)))))

      (if debug-mos1
	  (progn
	    (zl:format t "voltages for mosfets are:~%")
	    (zl:format t " processor  drain     gate     source      bulk   ids~%")
	    (do-for-selected-processors (x)
	      (zl:format t "~10d  ~8f  ~8f  ~8f  ~8f  ~8f~%"
		      x
		      (pref #.core-mos1-node-d-voltage x)
		      (pref #.core-mos1-node-g-voltage x)
		      (pref #.core-mos1-node-s-voltage x)
		      (pref #.core-mos1-node-b-voltage x)
		      (pref #.core-mos1-ids x)))))
;      (dbg)
; evaluate the linear parasitic capacitors
;   do the drain gate overlap
      (*when (and!! (or!! #.core-mos1-node-d-pointp
			  #.core-mos1-node-g-pointp)
		    (not!! core-mos1-equal-dg))
	(*set temp (*!! (!! alpha) core-mos1-cgd))
	(*set #.core-mos1-cond-d (+!! #.core-mos1-cond-d temp))
	(*set #.core-mos1-cond-g (+!! #.core-mos1-cond-g temp))
	#+off-diag
	(if *use-tridiagonal*
	    (progn
	      (*set #.core-mos1-mat-dg-value (-!! #.core-mos1-mat-dg-value temp))
	      (*set #.core-mos1-mat-gd-value (-!! #.core-mos1-mat-gd-value temp))))

	(*set temp (*!! core-mos1-cgd (-!! vds vgs)))
	(*if (=!! core-mos1-mtype (!! PMOS))
	     (*set temp (-!! temp)))
	(*set #.core-mos1-charge-d (+!! #.core-mos1-charge-d temp))
	(*set #.core-mos1-charge-g (-!! #.core-mos1-charge-g temp)))
      
;   do the source gate overlap
      (*when (and!! (or!! #.core-mos1-node-s-pointp
			  #.core-mos1-node-g-pointp)
		    (not!! core-mos1-equal-sg))
	(*set temp (*!! (!! alpha) core-mos1-cgs))
	(*set #.core-mos1-cond-s (+!! #.core-mos1-cond-s temp))
	(*set #.core-mos1-cond-g (+!! #.core-mos1-cond-g temp))
	#+off-diag
	(if *use-tridiagonal*
	    (progn
	      (*set #.core-mos1-mat-sg-value (-!! #.core-mos1-mat-sg-value temp))
	      (*set #.core-mos1-mat-gs-value (-!! #.core-mos1-mat-gs-value temp))))

	(*set temp (*!! core-mos1-cgs vgs))
	(*if (=!! core-mos1-mtype (!! PMOS))
	     (*set temp (-!! temp)))
	(*set #.core-mos1-charge-s (-!! #.core-mos1-charge-s temp))
	(*set #.core-mos1-charge-g (+!! #.core-mos1-charge-g temp)))
      
;   do the bulk gate overlap and cgsub
      (*when (and!! (or!! #.core-mos1-node-b-pointp
			  #.core-mos1-node-g-pointp)
		    (not!! core-mos1-equal-bg))
	(*set temp (*!! (!! alpha)
			(+!! core-mos1-cgb
			     (*!! (!! 0.667) core-mos1-cgsub))))
	(*set #.core-mos1-cond-b (+!! #.core-mos1-cond-b temp))
	(*set #.core-mos1-cond-g (+!! #.core-mos1-cond-g temp))
	#+off-diag
	(if *use-tridiagonal*
	    (progn
	      (*set #.core-mos1-mat-bg-value (-!! #.core-mos1-mat-bg-value temp))
	      (*set #.core-mos1-mat-gb-value (-!! #.core-mos1-mat-gb-value temp))))

	(*set temp (*!! (+!! core-mos1-cgb (*!! (!! 0.667) core-mos1-cgsub))
			(-!! vbs vgs)))
	(*if (=!! core-mos1-mtype (!! PMOS))
	     (*set temp (-!! temp)))
	(*set #.core-mos1-charge-b (+!! #.core-mos1-charge-b temp))
	(*set #.core-mos1-charge-g (-!! #.core-mos1-charge-g temp)))
      
;   do source bulk junction ( diode ) should be non-linear
      (*when (and!! (or!! #.core-mos1-node-s-pointp
			  #.core-mos1-node-b-pointp)
		    (not!! core-mos1-equal-sb))
	(*set temp (*!! (!! alpha) (+!! core-mos1-cjas core-mos1-cjass)))
	(*set #.core-mos1-cond-s (+!! #.core-mos1-cond-s temp))
	(*set #.core-mos1-cond-b (+!! #.core-mos1-cond-b temp))
	#+off-diag
	(if *use-tridiagonal*
	    (progn
	      (*set #.core-mos1-mat-sb-value (-!! #.core-mos1-mat-sb-value temp))
	      (*set #.core-mos1-mat-bs-value (-!! #.core-mos1-mat-bs-value temp))))

	(*set temp (*!! (+!! core-mos1-cjas core-mos1-cjass) vbs))
	(*if (=!! core-mos1-mtype (!! PMOS))
	     (*set temp (-!! temp)))
	(*set #.core-mos1-charge-s (-!! #.core-mos1-charge-s temp))
	(*set #.core-mos1-charge-b (+!! #.core-mos1-charge-b temp)))
      
;   do drain bulk junction ( diode ) should be non-linear
      (*when (and!! (or!! #.core-mos1-node-d-pointp
			  #.core-mos1-node-b-pointp)
		    (not!! core-mos1-equal-db))
	(*set temp (*!! (!! alpha) (+!! core-mos1-cjad core-mos1-cjasd)))
	(*set #.core-mos1-cond-d (+!! #.core-mos1-cond-d temp))
	(*set #.core-mos1-cond-b (+!! #.core-mos1-cond-b temp))
	#+off-diag
	(if *use-tridiagonal*
	    (progn
	      (*set #.core-mos1-mat-db-value (-!! #.core-mos1-mat-db-value temp))
	      (*set #.core-mos1-mat-bd-value (-!! #.core-mos1-mat-bd-value temp))))

	(*set temp (*!! (+!! core-mos1-cjad core-mos1-cjasd) (-!! vds vbs)))
	(*if (=!! core-mos1-mtype (!! PMOS))
	     (*set temp (-!! temp)))
	(*set #.core-mos1-charge-d (+!! #.core-mos1-charge-d temp))
	(*set #.core-mos1-charge-b (-!! #.core-mos1-charge-b temp)))
      
      
      #||
      NOTE: this charge model is not implemented correctly, I am putting this off for now.
      ; calculate the capacitances and charges
      (if (not (= core-mos1-cgsub ZERO))
	  
	  (let ((qg ZERO)
		(dqdvds ZERO)
		(dqdvgst ZERO)
		(temp1 ZERO)
		(temp2 ZERO)
		(temp3 ZERO))
	    (if (< vgst ZERO)			; Subthresh or Accum region.
		(progn
		  (setf
		    temp1 (-!! vgs vbs))	; compute vgb.
		  (if (or
			(and (> core-mos1-gamma ZERO)
			     (>= temp1 #.core-mos1-vfb))
			(< temp1 #.core-mos1-vfb))
		      (progn
			(if (<= temp1 #.core-mos1-vfb)	; Accumulation.
			    (setf
			      qg (*!! core-mos1-cgsub
				       (-!! temp1 #.core-mos1-vfb)))
			    (if (= core-mos1-mtype PMOS)
				(setf qg (-!! qg)))
			    (setf
			      temp1 (*!! core-mos1-cgsub alpha))
			    (setf		; Subthresh.
			      temp1 (sqrt (+!! (*!! core-mos1-gamma
						      core-mos1-gamma)
						(*!! 4.0 (-!! temp1 #.core-mos1-vfb))))
			      qg (*!! 0.5
				       core-mos1-gamma
				       core-mos1-cgsub
				       (-!! temp1 core-mos1-gamma)))
			    (if (= core-mos1-mtype PMOS)
				(setf qg (-!!qg)))
			    (setf
			      temp1 (/
				      (*!! alpha
					    core-mos1-gamma
					    core-mos1-cgsub)
				      temp1)))
			(setf
			  #.core-mos1-charge-g (+!! #.core-mos1-charge-g qg)
			  #.core-mos1-cond-g (+!! #.core-mos1-cond-g temp1)
			  #.core-mos1-charge-b (-!! #.core-mos1-charge-b qg)
			  #.core-mos1-cond-b (+!! #.core-mos1-cond-b temp1)))))
		(if (< vgst vds)		; Saturation region.
		    (progn
		      (setf
			temp1 (/ core-mos1-cgsub 1.5)
			temp2 (*!! alpha temp1)
			
						; gate charge
			temp (*!! core-mos1-cgsub
				   (-!! (-!! vgs #.core-mos1-fixed-vto)
					 (/ vgst 3.0))))
		      (if (= core-mos1-mtype PMOS)
			  (setf temp (-!! temp)))
		      (setf
			#.core-mos1-charge-g (+!! #.core-mos1-charge-g temp)
			#.core-mos1-cond-g (+!! #.core-mos1-cond-g temp2)
			
						; source
			temp (*!! temp1 vgst))
		      (if (= core-mos1-mtype PMOS)
			  (setf temp (-!! temp)))
		      (setf
			#.core-mos1-charge-s (-!! #.core-mos1-charge-s temp)
			#.core-mos1-cond-s (+!! #.core-mos1-cond-s
						(+!! temp2 (*!! temp2 ndvtdvbs)))
			
						; bulk
			temp (*!! core-mos1-cgsub
				   (+!! (-!! vgst vgs) #.core-mos1-fixed-vto)))
		      (if (= core-mos1-mtype PMOS)
			  (setf temp (-!! temp)))
		      (setf
			#.core-mos1-charge-b (+!! #.core-mos1-charge-b temp)
			#.core-mos1-cond-b (+!! #.core-mos1-cond-b
						(*!! 1.5 temp2 ndvtdvbs))))
		    (progn
		      (setf
			temp1 (/ (+!! (-!! (+!! vgst vgst) vds) 1e-12))
			temp2 (*!! temp1 temp1 vds)
			temp3 (*!! vds vds temp1)
			temp1 (*!! alpha core-mos1-cgsub)
			
						; gate
			temp (*!! core-mos1-cgsub
				   (+!! (-!! (-!! vgs #.core-mos1-fixed-vto)
					       (*!! 0.5 vds))
					 (/ temp3 6.0))))
		      (if (= core-mos1-mtype PMOS)
			  (setf temp (-!! temp)))
		      (setf
			#.core-mos1-charge-g (+!! #.core-mos1-charge-g temp)
;		      dqdvds (*!! temp1
;				(-!! (*!! temp2
;				      (/ (-!! (*!! 4.0 vgst) vds)
;					  6.0))
;				   0.5))
			dqdvgst (- (/ (*!! temp1 vds temp2) 3.0))
			#.core-mos1-cond-g (+!! temp1 dqdvgst)
			
						; drain
			temp (*!! core-mos1-cgsub
				   (-!! (-!! (*!! 0.75 vds)
					       (*!! 0.5 vgst))
					 (*!! 0.25 temp3))))
		      (if (= core-mos1-mtype PMOS)
			  (setf temp (-!! temp)))
		      (setf
			#.core-mos1-charge-d (+!! #.core-mos1-charge-d temp)
			dqdvds (*!! temp1
				     (-!! 0.75
					   (*!! 0.25
						 temp2
						 (-!! (*!! 4.0 vgst) 
						       vds))))
;		      dqdvgst = temp1 * (0.5*vds*temp2 - 0.5);
			#.core-mos1-cond-d (+!! #.core-mos1-cond-d dqdvds)
			
						; source
			temp (*!! core-mos1-cgsub
				   (-!! (-!! (/ temp3 12.0)
					       (*!! 0.25 vds))
					 (*!! 0.5 vgst))))
		      (if (= core-mos1-mtype PMOS)
			  (setf temp (-!! temp)))
		      (setf
			#.core-mos1-charge-s (+!! #.core-mos1-charge-s temp)
			dqdvds (*!! temp1
				     (-!! (/ (*!! temp2
						     (-!! (*!! 4.0 vgst)
							   vds))
					       12.0)
					   0.25))
			dqdvgst (*!! temp1
				      (-!! (/ (*!! vds temp2)
						6.0)
					    0.5))
			#.core-mos1-cond-s (-!! #.core-mos1-cond-s
						(+!! (*!! dqdvgst
							  (+!! 1.0 ndvtdvbs))
						     dqdvds))
			
						; bulk
			temp (*!! core-mos1-cgsub
				   (+!! (-!! vgst vgs)
					 #.core-mos1-fixed-vto)))
		      (if (= core-mos1-mtype PMOS)
			  (setf temp (-!! temp)))
		      (setf
			#.core-mos1-charge-b (+!! #.core-mos1-charge-b temp)
			#.core-mos1-cond-b (+!! #.core-mos1-cond-b (*!! temp1 ndvtdvbs))))
		    )
		)
	    )
	  )
						; send the results back
      ||#    
      )
    (*set core-device-node3-current (-!! #.core-mos1-ids))
    )
  )
