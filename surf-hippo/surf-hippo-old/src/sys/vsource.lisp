;;; -*- mode: lisp; Syntax: Common-lisp; package: #-parallel surf #+parallel *surf; base: 10; -*-
;;; (c) Copyright 1985, Don Webber, Thinking Machines Corporation, Inc.
;;; alterations (c) Copyright 1990, Lyle Borg-Graham, MIT Center for Biological Information Processing
;;; File creation date: 6/09/85 18:06:25
;
; the voltage source model
;


#-parallel
(in-package "SURF-HIPPO"
	    :use '("COMMON-LISP-USER" "COMMON-LISP")
	    :nicknames '("SURF"))
#+parallel 
(in-package "*SURF")


(defstruct vsource
  "Model for a voltage source"
  (name ""		:type string)   ; not really needed
  (plot-current nil)
  (current-data '())
  (type 0)	; zero if DC, one if PWL, two if controlled.
  node-1
  ; node 2 is always ground
  (value-list '()	:type list)		; for PWL sources
  (function-list )				;for controlled vsources.
  (period zero :type single-float)
  (delay zero :type single-float)
  (voltage zero	:type single-float))	; for DC sources

(defconstant pwl-default-params '((values . ()) (delay . 0.0) (period . 0.0)))

(defun create-vsource-model ()
  "Creates a template for all vsources."
  (let ((template (make-model-template)))
    (setf
      (model-template-name template) (string "vsource")
      (model-template-default-params template) '()
      (model-template-eval-routine template) #'eval-vsource
      (model-template-print-routine template) #'print-vsource
      (model-template-create-routine template) #'create-pwl-vsource
      (model-template-create-core-routine template) #'create-core-vsource
      (model-template-add-off-diag-routine template) #'add-off-diag-vsource
      (model-template-find-coupling-routine template) #'find-coupling-vsource
      (model-template-fix-dc-nodes-routine template) #'vsource-fix-dc-nodes
      *models* (cons template *models*)
      (gethash (string "vsource") *model-hash-table*) template
      vsource-hash-table (make-hash-table :test #'equal))
    (create-model-instance (string "vcont") (model-template-name template) '() )
    (create-model-instance (string "vdc") (model-template-name template) '() )
    (create-model-instance (string "vpwl") (model-template-name template) pwl-default-params )))
	; only need two vsource model instances, so create them now.

(defun print-vsource (vsrc)
  "Prints out the data associated with a vsource."
  (if (= 0 (vsource-type vsrc))
      (format *output-stream "DC Vsource ~a at node ~a : value ~a~%"
	      (vsource-name vsrc)
	      (node-name (vsource-node-1 vsrc))
	      (vsource-voltage vsrc))
      (format *output-stream "PWL Vsource ~a at node ~a : waveform ~a~%"
	      (vsource-name vsrc)
	      (node-name (vsource-node-1 vsrc))
	      (vsource-value-list vsrc))))



(defun create-pwl-vsource (cell-element &key (plot-pane 1))
  "Creates a element of type vsource."
  (let* ((element-name (if (typep cell-element 'soma)(soma-name cell-element)(segment-name cell-element)))
	 (source-name (format nil "~a-vstim" element-name))
	 (node1-name (node-name (if (typep cell-element 'soma)
				    (soma-node cell-element)(segment-node-2 cell-element))))
	 (cell-name (cell-name (if (typep cell-element 'soma)
				   (soma-cell cell-element)(segment-cell cell-element)))))
    (if (gethash source-name vsource-hash-table)
	(sim-warning (format nil "create-vsource: vsource ~a  already defined, ignoring" source-name))
	(let* ((model (gethash "vpwl" *model-instance-hash-table*))
	       (node-1 (create-node node1-name :cell-name cell-name :plot-pane plot-pane))
	       (vsrc (make-vsource :name source-name :node-1 node-1 :plot-current t :type 1))
	       )
	  (setf
	    (node-elements node-1) (cons vsrc (node-elements node-1))
	    (gethash source-name vsource-hash-table) vsrc
	    (model-instance-elements model) (cons vsrc (model-instance-elements model)))
	  vsrc))))

(defun create-core-vsource (vsrc nd)
;  (setf
;    (core-node-voltage-n+1 (vsource-node-1 vsrc)) (vsource-voltage vsrc))
  (declare (ignore vsrc nd)))

(defun add-off-diag-vsource (vsrc diag off-diag off-diag-entry)
  "Adds off diagonal entries for this vsource."
  (declare (ignore vsrc diag off-diag off-diag-entry)))

(defun find-coupling-vsource (nd vsrc)
  "Returns a pair of (node . value) telling how coupled that node is to nd."
  ;; for vsource, nodes are completely coupled, but it must be grounded,
  ;; so don't bother sending anything back.
  (declare (ignore nd vsrc ))
  nil)

(defun vsource-fix-dc-nodes (vsrc)
  "Fix up the nodes of this element which are connected to dc nodes."
  (declare (ignore vsrc)))

#-parallel
(defun eval-vsource (vsrc)
  (declare (ignore vsrc)))

#+parallel
(defun eval-vsource ()
)

#|
(defun vsource-latch-voltages ()
; no voltages get sent to vsources
)
|#

(defun eval-pwl-vsource (pwl-src)
  "Calculates the new voltage in time for a pwl voltage source."
  (let*
    ((value-list (vsource-value-list pwl-src))
     (value-pair (car value-list))
     (prev-pair nil)
     (pwl-time (mod (- *real-time
		       (vsource-delay pwl-src))
		    (vsource-period pwl-src))))
    (do						; repeat until prev-pair is before the 
      ()					;  current time and value-pair is after
      ((or (null value-pair)
	   (> (car value-pair) pwl-time)))
      (setf
	prev-pair value-pair
	value-list (cdr value-list)
	value-pair (car value-list)))
    (if (null prev-pair)
	(sim-error (format
		     nil
		     "Error, malformed pwl waveform for source ~a,~a~%"
		     (vsource-name pwl-src)
		     "the waveform must start at the beginning of time.")))
    (if (null value-pair)
	(progn
	  (setf				; use the last value in the list
	    (vsource-voltage pwl-src) (cdr (car (last (vsource-value-list pwl-src)))))
;	  (format t "~%>>> Value for pwl from end of list is ~a " (vsource-voltage pwl-src))
	  )
	(let ((t0 (car prev-pair))
	      (v0 (cdr prev-pair))
	      (t1 (car value-pair))
	      (v1 (cdr value-pair)))
	  (if (not
		(and (numberp t0)
		     (numberp v0)
		     (numberp t1)
		     (numberp v1)))
	      (sim-error (format
			   nil
			   "Error, malformed pwl waveform for source ~a,~a~%"
			   (vsource-name pwl-src)
			   "one of the entries was not a number.")))
	  (setf
	    (vsource-voltage pwl-src) (+ v0
					 (* (- pwl-time t0)
					    (/ (- v1 v0)
						(- t1 t0)))))
;	  (format t "~%>>> Value for pwl is ~a " (vsource-voltage pwl-src))
	  )))
  (vsource-voltage pwl-src))

(defun set-pwl-sources ()
  "Deals with pwl voltage sources."
  (let ((pwl-model (gethash (string "vpwl") *model-instance-hash-table*)))
    (if (null pwl-model)
	(sim-error "Internal error, pwl sources are not defined.~%"))
    (dolist (pwl-src (model-instance-elements pwl-model))
      (#+parallel *setf #-parallel setf	
	(#+parallel pref core-node-voltage-n+1 (node-core-nd (vsource-node-1 pwl-src)))
	(eval-pwl-vsource pwl-src))))
    )

(defun mark-vsources ()
  (maphash 'mark-a-vsource vsource-hash-table))

(defun mark-a-vsource (name vsrc)
  "Marks the node that this vsource is connected to."
  (declare (ignore name))
  (let ((nd (vsource-node-1 vsrc)))
    (if (or (node-is-dc-source nd)
	    (node-is-pwl-source nd))
	(sim-error (format nil "Two voltage sources are connected to the same node ~a."
			   (node-name nd))))
    (if (= (vsource-type vsrc) 0)
	(setf				; DC source
	  (node-is-dc-source nd) t)
	(setf				; pwl source
	  (node-is-pwl-source nd) t))))

(defun queue-break-points (value-list period delay)
  "Looks at the list of dotted pairs in 'value-list', car is time, cdr is voltage,
   and puts each time on the queue of break points so that the simulation can
   be sure to step there."
  (dotimes (i (ceiling (/ user-stop-time period)))
    (dolist (pair value-list)
      (if (numberp (car pair))
	  (queue-time (+ (car pair)
			 (* i period)
			 delay))
	  (sim-error (format nil "Time value in pwl list was not a number ~a~%"
			     (car pair)))))))

(defun fix-pwl-sources ()
  "To be called after stop-time is known so the pwl break points can be placed."
  (let ((pwl-model (gethash (string "vpwl") *model-instance-hash-table*)))
    (if (null pwl-model)
	(sim-error "Internal error, pwl sources are not defined.~%"))
;    (setq *break-point-list* nil)
    (dolist (pwl-src (model-instance-elements pwl-model))
      (progn
	(if (= (vsource-period pwl-src) 0)
	    (setf (vsource-period pwl-src) (* 2 user-stop-time))) ; Anything over stop-time
								  ; will do. Otherwise 
								  ; (mod stop-time period)
								  ; is zero.
	(queue-break-points (vsource-value-list pwl-src)
			    (vsource-period pwl-src)
			    (vsource-delay pwl-src))))))

#+parallel
(defun vsource-return-current ())
#+parallel
(defun vsource-return-charge ())
#+parallel
(defun vsource-return-conductance ())

#-parallel
(defun vsource-return-current (vsrc)
  (let ((vsource-supplied-current 0)(voltage (core-node-voltage-n+1 (node-core-nd (vsource-node-1 vsrc)))))
    (dolist (elt (node-elements (vsource-node-1 vsrc)))
      (setq vsource-supplied-current
	    (+ vsource-supplied-current
	       (cond ((eq (named-structure-symbol elt) 'isource)
		      (isource-current elt))
		     ((eq (named-structure-symbol elt) 'segment)
		      (let ((core-seg (segment-core-seg elt))
			    v1 v2 v-diff i1 i2)
			(setf v1 (get-segment-voltage-1 core-seg))
			(setf v2 (get-segment-voltage-2 core-seg))
			(setf v-diff (- v1 v2))
						; calculate the current
			(setf i1 (* v-diff (#.core-segment-g-axial core-seg)))
			(setf i2 (+ (- i1) (* (#.core-segment-g-leak core-seg)
					      (- v2 (#.core-segment-v-leak core-seg)))))
			(if (eq (vsource-node-1 vsrc) (segment-node-1 elt))
			    (- i1) (- i2))

			))
    
		     ((eq (named-structure-symbol elt) 'soma)
		      (let ((core-soma (soma-core-soma elt)))	;
;			       (break)		;
			(- (+ (* (#.core-soma-g-shunt core-soma) voltage)
			      (* (#.core-soma-g-leak core-soma)
				 (- voltage (#.core-soma-v-leak core-soma)))))))
		     ((eq (named-structure-symbol elt) 'channel)
		      (core-channel-current (channel-core-ch elt)))
		     (t 0)))))
    vsource-supplied-current))






#-parallel
(defun vsource-return-charge ())
#-parallel
(defun vsource-return-conductance ())



;;; THESE FUNCTIONS NOT SUPPORTED NOW.
;
;(defun create-vsource (name node1 node2 model-name cell-name parameters &key  (plot-pane 1))
;  "Calls create dc vsource or create pwl vsource."
;  (let
;    ((model (gethash model-name *model-instance-hash-table*)))
;    (cond
;      ((equal (model-instance-name model) (string "vdc"))
;       (create-dc-vsource name node1 node2 model-name parameters :cell-name cell-name :plot-pane plot-pane))
;      ((equal (model-instance-name model) (string "vpwl"))
;       (create-pwl-vsource name node1 node2 model-name parameters :cell-name cell-name :plot-pane plot-pane))
;      (t (sim-error (format nil
;			    "Error: don't know about this kind of voltage source ~a~%"
;			    (model-instance-name model)))))))




;
;(defun create-dc-vsource (name node1 node2 model-name parameters
;			  &key (cell-name "Unknown")(plot-pane 1)  )
;  "Creates a element of type vsource. Inputs 'name' 'node1' and 'node2' are strings,
;   'model-name' is the name of the model-instance, and 'parameters' is an a-list."
;  (if (gethash name vsource-hash-table)
;      (sim-warning (format nil "create-vsource: vsource ~a  already defined, ignoring"
;			   name))
;      (let ((vsrc (make-vsource))
;	    (model (gethash model-name *model-instance-hash-table*))
;	    (node-1 (create-node node1 :cell-name cell-name :plot-pane plot-pane))
;	    (node-2 (create-node node2 :cell-name cell-name :plot-pane plot-pane))
;	    voltage
;	    temp-voltage
;	    junk)
;	  (ignore junk) 
;; if I do this, I should also negate the value of the vsource
;;	(if (eq node-1 *ground-node*)
;;	    (let	; swap nodes one and two
;;	      ((temp node-1))
;;	      (setf
;;		node-1 node-2
;;		node-2 temp)))
;	(if (not (equal node-2 *ground-node*))
;	    (sim-error (format nil "voltage source ~a is not grounded." name)))
;	(setf
;	  (vsource-name vsrc) name
;	  voltage (eval (cdr (assoc 'voltage (model-template-default-params
;					      (model-instance-model model)))))
;	  temp-voltage (eval (cdr (assoc 'voltage (model-instance-changed-params model))))
;	  junk (if temp-voltage
;		   (setf
;		     voltage temp-voltage))
;	  temp-voltage (eval (cdr (assoc 'voltage parameters)))
;	  junk (if temp-voltage
;		   (setf
;		     voltage temp-voltage))
;	  (vsource-voltage vsrc) voltage
;	  (vsource-node-1 vsrc) node-1
;	  (vsource-type vsrc) 0
;	  (node-voltage node-1) voltage
;	  (gethash name vsource-hash-table) vsrc
;	  (node-elements node-1) (cons vsrc (node-elements node-1))
;	  (model-instance-elements model) (cons vsrc (model-instance-elements model)))
;	vsrc)))