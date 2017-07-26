;;; -*- mode: lisp; Syntax: Common-lisp; package: #-parallel surf #+parallel *surf; base: 10 ; -*-
;;; (c) Copyright 1985, Don Webber, Thinking Machines Corporation, Inc.
;;; alterations (c) Copyright 1990, Lyle Borg-Graham, MIT Center for Biological Information Processing
;;; File creation date: 6/09/85 11:53:54
;
; Initialization routines.
;


#-parallel
(in-package "SURF-HIPPO"
	    :use '("COMMON-LISP-USER" "COMMON-LISP")
	    :nicknames '("SURF"))
#+parallel 
(in-package "*SURF")




;; This may be a convenient hook for changing some globals for the Sun-based simulatorin lieu of a menu.
#+sun
(defun user-set-globals ()
  user-max-step 2.0
  )

(defun initialize-for-simulation ()
  #+parallel (*warm-boot)
  ;;  #+parallel (setq *check-arg-p* nil)
  #+parallel (init-processors)
  (setf
   *integer-time (floor user-start-time)
   *time-list* '()
   *init-value-list* '()
   *break-point-list* '()
   *core-node-list* '()
   *dc-solution-computed* nil
   *total-num-iterations* 0
   *total-num-time-points* 0
   *num-messages-not-sent* 0
   *total-num-messages-not-sent* 0
   )
  (update-qtens)
  )

(defun initialize-for-circuit ()
  #+parallel (init-processors)
  (initialize-globals-for-circuit)
  (declare-ground  "Ground")
  (create-models)
  )



(defun initialize-globals-for-circuit ()
  (setf
   *branch-list* '()
   #+parallel *processor-allocation-counter* #+parallel 0
   *models* '() *model-hash-table* (make-hash-table :test #'equal)
   *synapse-type-models* '() *synapse-type-model-hash-table* (make-hash-table :test #'equal)
   *model-instance-hash-table* (make-hash-table :test #'equal)
   *num-nodes* 0    node-hash-table (make-hash-table :test #'equal)
   *cells* '()  *use-tridiagonal* t
   *model-ca-variation* nil
   *num-unknowns* 0
   *plot-channel-currents* '() *plot-synapse-currents* '() *plot-channel-conductances* '()
   *plot-synapse-conductances* '()
   *plot-isource-currents* '() *plot-vsource-currents* '() *plot-particles* '()
   *plot-conc-ints* '() *plot-nodes* '()  
   *node-order* '()
   *ground-node* nil
   *pseudo-transient-requested* t
   cmin 1e-15 vabs 1e-5 vrel 1e-3 iabs 1e-5 irel 1e-3
   max-voltage-step 1.0 
   lterel .001 lteabs 1e-4
   up-step-ratio 2
   step-3-ratio (* up-step-ratio up-step-ratio up-step-ratio)
   down-step-ratio 8    user-start-time 0.0    user-min-step 1e-6 ; user-max-step 2.0
   *max-num-relax-iterations*  10     *iters-before-sor* 10    *under-relax-factor* 0.5    *over-relax-factor* 1.5))

#|
    *debug-at-time-steps* nil
    *debug-all-iterations* nil
    *print-matrix* t
|#
    

(defun create-models ()
  "Creates the template for each circuit model."
  #+mos1 (create-mos1-model)
  (create-soma-model)
  (create-channel-model)
  (create-conc-int-model)	; This must come after (create-channel-model) here
  (create-particle-model)
  (create-conc-part-model)
  (create-synapse-model)
  (create-segment-model)
  (create-cell-model)
  (create-cell-type-model)
  (create-capacitor-model)
  (create-resistor-model)
  (create-isource-model)
  (create-vsource-model)
)

(defun set-constants ()
  "Sets constants that were dependent on the info in the 
   circuit description."
  (if (<= user-stop-time user-start-time)
      (sim-error "The stop time must be greater than the start time."))
  (setf
   ;;    mrt (/ (float user-start-time) max-integer-time)
   mrt (/ (float (max (abs user-start-time) (abs user-stop-time))) max-integer-time)
   stop-time (truncate (/ user-stop-time mrt))
   start-time (truncate (/ user-start-time mrt))
   )
  (if (<= user-min-step 0.0)
      (setf user-min-step (* mrt 11)))
  (if (<= user-max-step 0.0)
      (setf user-max-step (/ user-stop-time 40.0)))

  (setf
   min-step (max (truncate (/ user-min-step mrt)) 1)
   max-step (truncate (/ user-max-step mrt)))
  (fix-pwl-sources)
  (fix-pwl-isources)
  (fix-break-point-list)
  )

(defun create-model-instance ( name template-name parameters )
  "Creates a instance of any model."
  (if (gethash name *model-instance-hash-table*)
      (sim-warning (format nil
			   "Model instance ~a is already defined, replacing old definition.~%"
			   name)))
  (let ((inst (make-model-instance))
	(template (gethash template-name *model-hash-table*)))
    (if (null template)
	(sim-error (format nil
			   "Don't know what model template '~a' is"
			   template-name)))
    (setf
      (model-instance-name inst) name
      (model-instance-model inst) template
      (model-instance-changed-params inst) parameters
      (gethash name *model-instance-hash-table*) inst
      (model-template-instances template) (cons inst (model-template-instances template)))))

#+parallel
(defun init-processors ()
  "Zeros out the fields of the processors."
  (*set *lisp-struct-type (!! 0))
  (*set fanout-valid nil!!)
  (*set fanout-seg-forward nil!!)
  (*set fanout-seg-backward nil!!)

  (*set core-node-is-source nil!!)

  (*set core-device-node1-valid nil!!)
  (*set core-device-node1-charge (!! 0))
  (*set core-device-node1-current (!! 0))
  (*set core-device-node1-conductance (!! 0))

  (*set core-device-node2-valid nil!!)
  (*set core-device-node2-charge (!! 0))
  (*set core-device-node2-current (!! 0))
  (*set core-device-node2-conductance (!! 0))

  (*set core-device-node3-valid nil!!)
  (*set core-device-node3-charge (!! 0))
  (*set core-device-node3-current (!! 0))
  (*set core-device-node3-conductance (!! 0))

  (*set core-device-node4-valid nil!!)
  (*set core-device-node4-charge (!! 0))
  (*set core-device-node4-current (!! 0))
  (*set core-device-node4-conductance (!! 0))

  (*set core-device-node5-valid nil!!)
  (*set core-device-node6-valid nil!!)

  #+off-diag
  (progn
    (*set core-device-mat-12-valid nil!!)
    (*set core-device-mat-13-valid nil!!)
    (*set core-device-mat-14-valid nil!!)
    
    (*set core-device-mat-21-valid nil!!)
    (*set core-device-mat-23-valid nil!!)
    (*set core-device-mat-24-valid nil!!)
    
    (*set core-device-mat-31-valid nil!!)
    (*set core-device-mat-32-valid nil!!)
    (*set core-device-mat-34-valid nil!!)
    
    (*set core-device-mat-41-valid nil!!)
    (*set core-device-mat-42-valid nil!!)
    (*set core-device-mat-43-valid nil!!)
    )
  )
