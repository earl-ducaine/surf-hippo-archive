;;; -*- mode: lisp; Syntax: Common-lisp; package: #-parallel surf #+parallel *surf; base: 10 ; -*-
;;; (c) Copyright 1988, Don Webber, Thinking Machines Corporation, Inc.
;;; alterations (c) Copyright 1990, Lyle Borg-Graham, MIT Center for Biological Information Processing
;;; File creation date: 2/3/88 21:20:12
;
; the concentration dependent gating particle model, part of the neuron model
;


#-parallel
(in-package "SURF-HIPPO"
	    :use '("COMMON-LISP-USER" "COMMON-LISP")
	    :nicknames '("SURF"))
#+parallel 
(in-package "*SURF")


(defstruct conc-part 
  "Model for a concentration dependent gating particle"
  (name ""		:type string)  
  (plot-state nil)
  (state-data '() :type list)
  node
  cnode
  core-prt
  model
  (params nil)
  (channel nil)
  (cell-element nil)				;Type soma or segment
  (alpha 0.0		:type single-float)
  (beta 0.0		:type single-float)
  (power 0 :type fixnum)
  (Q10 1.0 :type single-float)
  )

#-parallel
(defstruct core-conc-part 
  "Core model for a conc-part."
  (node-pointp nil	:type boolean)	; zero if constant, one if node
  node-point
  (node-const zero	:type single-float)

  cnode-point
  cnode-pointp
  cnode-const

  (alpha 0.0		:type single-float)
  (beta 0.0		:type single-float)
  (power 0 :type fixnum)
  (Q10 1.0 :type single-float)
  )


(defun create-conc-part-model ()
  "Creates a template for all conc-parts."
  (let ((template (make-model-template)))
    (setf
      (model-template-name template) (string "conc-part")
      (model-template-default-params template) '()
      (model-template-eval-routine template) #'eval-conc-part
      (model-template-print-routine template) #'print-conc-part
      (model-template-create-routine template) #'create-concentration-particle
      (model-template-create-core-routine template) #'create-core-conc-part
      (model-template-add-off-diag-routine template) #'add-off-diag-conc-part
      (model-template-find-coupling-routine template) #'find-coupling-conc-part
      (model-template-fix-dc-nodes-routine template) #'conc-part-fix-dc-nodes
      *models* (cons template *models*)
      (gethash (string "conc-part") *model-hash-table*) template
      conc-part-hash-table (make-hash-table :test #'equal))
    ; only need one conc-part model instance, so create it now.
    (create-model-instance (string "cprt") (model-template-name template) '() )))

(defun print-conc-part (cprt)
  "Prints out this data associated with a conc-part."
  (format *output-stream "Conc-Part ~a at ~a : alpha ~a beta ~a~%"
	  (conc-part-name cprt)
	  (node-name (conc-part-node cprt))
	  (conc-part-alpha cprt)
	  (conc-part-beta cprt)))

;;; CREATE-CONCENTRATION-PARTICLE 'Channel' is an instance of a channel structure. Name of concentration
;;; particle node is same as concentration particle name.
(defun create-concentration-particle (channel cell-name param-list &key  (save-particle nil)
				      (plot-pane 1)  )
  "Creates a element of type conc-part. Inputs 'name' and 'node' are strings,
   'model-name' is the name of the model-instance, and 'parameters' is an a-list."

  (let* ((particle-name (format nil "~a-~a" (channel-name channel) (first param-list)))
	 (node-name particle-name)
	 (n1 (create-node node-name :cell-name cell-name :plot-pane plot-pane))
	 (n2 (if (channel-pre-synaptic-node channel)
		 (channel-pre-synaptic-node channel)
		 (if (typep (channel-cell-element channel) 'soma)
		     (soma-node (channel-cell-element channel) )
		     (segment-node-2 (channel-cell-element channel) ))))
	 (cell-element (channel-cell-element channel))
	 (cint (if (typep cell-element 'soma)
		   (soma-conc-int cell-element)
		   (segment-conc-int cell-element)))
	 (model (gethash "cprt" *model-instance-hash-table*)))
    (if (null cint)
	(sim-warning "Attempt to create a concentration controlled particle ~a ~% for membrane ~a which is not
		     modelling changes in Ca concentration~%" node-name node-name)
	(let ((c (sixth param-list))
	      conc-node)
	  (cond
	    ((eq c 'shell.1)
	     (setf conc-node (conc-int-node-1 cint)))
	    ((eq c 'shell.2)
	     (setf conc-node (conc-int-node-2 cint)))
	    (t
	     (sim-error "dependence of concentration controlled particle ~a ~% must be specified by the symbol
			'shell.1 or 'shell.2~%" (format nil "~a-~a" (channel-name channel) particle-name))))
	  (setf n2 conc-node)))
    
    (if (not (or (node-is-dc-source n1)
		 (node-is-pwl-source n1)))
	(let ((cprt (make-conc-part)))
	  (setf
	    (conc-part-name cprt) particle-name
	    (conc-part-node cprt) n1
	    (conc-part-cnode cprt) n2
	    (conc-part-model cprt) model
	    (conc-part-channel cprt) channel
	    (conc-part-cell-element cprt)  (channel-cell-element channel)
;;;	      (conc-part-params cprt) parameters
	    (conc-part-alpha cprt) (second param-list)
	    (conc-part-beta cprt) (third param-list)
	    (conc-part-power cprt) (fourth param-list)
	    (conc-part-Q10 cprt) (fifth param-list)
	    (conc-part-plot-state cprt) save-particle    
	    (node-elements n1) (cons cprt (node-elements n1))
	    (node-elements n2) (cons cprt (node-elements n2))
	    (gethash particle-name conc-part-hash-table) cprt
	    (model-instance-elements model) (cons cprt (model-instance-elements model)))
 	  cprt))))

; This function creates a core conc-part data structure. In the parallel version, it 
; puts the device on the same processor as the fanout node, which is passed in as the 
; variable 'proc. The serial version ignores this and allocates a struct normally.
; This function only works if the 'nd argument is not a DC source node, because these
; do not have processors allocated to them.

(defun create-core-conc-part (cprt nd)
  "Creates the core struct for a conc-part."
  (let (core-cprt
	(proc #-parallel nil #+parallel (allocate-processor)))
    #-parallel (declare (ignore proc))
    #+parallel (*setf (pref fanout-valid proc) t)
    #+parallel (*setf (pref fanout-seg-forward proc) nil)
    (if (conc-part-core-prt cprt)			; 
	(setf core-cprt (conc-part-core-prt cprt))	; core conc-part has already been allocated
	(progn						; else allocate one
	  #-parallel (setf core-cprt (make-core-conc-part))
	  #+parallel (*setf core-cprt proc)
	  #+parallel (*setf (pref *lisp-struct-type core-cprt) core-conc-part)
	  (#+parallel *setf #-parallel setf
	    (#+parallel pref #.core-conc-part-alpha core-cprt) (conc-part-alpha cprt)
	    (#+parallel pref #.core-conc-part-beta core-cprt) (conc-part-beta cprt)
	    (#+parallel pref core-conc-part-power core-cprt) (conc-part-power cprt)
	    (#+parallel pref #.core-conc-part-Q10 core-cprt) (conc-part-Q10 cprt))
	  (setf (conc-part-core-prt cprt) core-cprt)))
    
    (let ((node1 (conc-part-node cprt))
	  (node2 (conc-part-cnode cprt)))
      (cond
	((eq nd node1)
	 #-parallel
	 (setf
	   (#.core-conc-part-node-pointp core-cprt) t
	   (#.core-conc-part-node-point core-cprt) (node-core-nd node1))
	 #+parallel
	 (*setf
	   (pref #.core-conc-part-node-pointp core-cprt) t
	   (pref #.core-conc-part-node-point core-cprt) proc))
	((eq nd node2)
	 #-parallel
	 (setf
	   (#.core-conc-part-cnode-pointp core-cprt) t
	   (#.core-conc-part-cnode-point core-cprt) (node-core-nd node2))
	 #+parallel
	 (*setf
	   (pref #.core-conc-part-cnode-pointp core-cprt) t
	   (pref #.core-conc-part-cnode-point core-cprt) proc))
	(t
	 (sim-error "Internal error: called create-core on a device with a invalid node"))
	))))

(defun add-off-diag-conc-part (cprt diag off-diag off-diag-entry)
  (declare (ignore cprt diag off-diag off-diag-entry))
  )

(defun find-coupling-conc-part (nd cprt)
  "Returns a pair of (node . value) telling how coupled that node is to nd."
  (declare (ignore nd cprt ))
  nil)

(defun conc-part-fix-dc-nodes (cprt)
  "Fix up the nodes of this element which are connected to dc nodes."
  (if (conc-part-core-prt cprt)
      (progn
	(if (node-is-dc-source (conc-part-node cprt))
	    (#+parallel *setf #-parallel setf
	      (#+parallel pref #.core-conc-part-node-pointp (conc-part-core-prt cprt)) nil
	      (#+parallel pref #.core-conc-part-node-const (conc-part-core-prt cprt))
	      (node-voltage (conc-part-node cprt))))
	)))

#-parallel
(defun get-conc-part-voltage (cprt)
  (if (#.core-conc-part-node-pointp cprt)
      (core-node-voltage-n+1 (#.core-conc-part-node-point cprt))
      (#.core-conc-part-node-const cprt)))

#-parallel
(defun get-conc-part-cnode-value (cprt)
  (if (#.core-conc-part-cnode-pointp cprt)
      (core-node-voltage-n+1 (#.core-conc-part-cnode-point cprt))
      (#.core-conc-part-cnode-const cprt)))

#-parallel
(defun eval-conc-part (cprt)
  (let (conc
	w
	current
	conductance
	charge
	exp1
	xinf
	taux
	(core-cprt (conc-part-core-prt cprt)))
    (if (null core-cprt)
	(return-from eval-conc-part (values)))

    ; get the voltages
    (setf w (get-conc-part-voltage core-cprt))
    (setf conc (get-conc-part-cnode-value core-cprt))

    (setf exp1 (* (ch-power conc (core-conc-part-power core-cprt))
		  (#.core-conc-part-alpha core-cprt)))

    (setf xinf (/ exp1 (+ exp1 (#.core-conc-part-beta core-cprt))))
    (setf taux (/ (+ exp1 (#.core-conc-part-beta core-cprt))))
    (setf taux (* taux (#.core-conc-part-q10 core-cprt)))

    (setf current (/ (- w xinf) taux))
    (setf conductance (+ alpha (/ taux)))
    (setf charge w)

    ; send the values back where they go
    (if (#.core-conc-part-node-pointp core-cprt)
	(if (not (core-node-is-source (#.core-conc-part-node-point core-cprt)))
	    (setf
	      (core-node-current (#.core-conc-part-node-point core-cprt))
	      (+ (core-node-current (#.core-conc-part-node-point core-cprt))
		 current)
	      (core-node-charge (#.core-conc-part-node-point core-cprt))
	      (+ (core-node-charge (#.core-conc-part-node-point core-cprt))
		 charge)
	      (core-node-jacobian (#.core-conc-part-node-point core-cprt))
	      (+ (core-node-jacobian (#.core-conc-part-node-point core-cprt))
		 conductance))))
    ))

#+parallel
(*defun get-conc-part-voltage (v1)
  (cond!!
    (#.core-conc-part-node-pointp v1)
    (t!! #.core-conc-part-node-const)))

#+parallel
(*defun get-conc-part-cnode-value (v1)
  (cond!!
    (#.core-conc-part-cnode-pointp v1)
    (t!! #.core-conc-part-cnode-const)))

#+parallel
(defun eval-conc-part ()
  (*select-type (core-conc-part)
    (*let
      ((conc nil)
       (w nil)
       (current nil)
       (conductance nil)
       (exp1 nil)
       (xinf nil)
       (taux nil))
      (declare (type (pvar big-float) conc))
      (declare (type (pvar big-float) w))
      (declare (type (pvar big-float) current))
      (declare (type (pvar big-float) conductance))
      (declare (type (pvar big-float) exp1))
      (declare (type (pvar big-float) xinf))
      (declare (type (pvar big-float) taux))

      (*set w (get-conc-part-voltage #.core-conc-part-node-voltage))
      (*set conc (get-conc-part-cnode-value #.core-conc-part-cnode-value))

      (*set exp1 (* (ch-power!! conc core-conc-part-power)
		    #.core-conc-part-alpha))
      
      (*set xinf (/!! exp1 (+!! exp1 #.core-conc-part-beta)))
      (*set taux (/!! (+!! exp1 #.core-conc-part-beta)))
      (*set taux (*!! taux #.core-conc-part-q10))

      (*set current (/!! (-!! w xinf) taux))
      (*set conductance (+!! (!! alpha) (/!! taux)))

      (*set #.core-conc-part-current current)
      (*set #.core-conc-part-charge w)
      (*set #.core-conc-part-conductance conductance)
      )))
