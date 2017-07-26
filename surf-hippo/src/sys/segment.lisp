;; -*- mode: lisp; Syntax: Common-lisp; package: surf ; base: 10;  -*-
;;; Lyle Borg-Graham, Equipe Cognisciences, Institut Alfred Fessard, CNRS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              The Surf-Hippo Neuron Simulator                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Surf-Hippo project at the  ;;;
;;; Center for Biological Information Processing, Department of     ;;;
;;; Brain and Cognitive Sciences, Massachusetts Institute of        ;;;
;;; Technology, and currently at the Equipe Cognisciences, Institut ;;;
;;; Alfred Fessard, CNRS and has been placed in the public domain.  ;;;
;;; If you are using this code or any part of Surf-Hippo, please    ;;;
;;; contact surf-hippo@ai.mit.edu to be put on the mailing list.    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; SYS Source file: segment.lisp

					;
; the segment model, for approximating distributed RC lines
;


(in-package "SURF-HIPPO")


(defun color-segment (element color)
  "Assign a COLOR to the cell elements associated with ELEMENT, that will be used in plotting the
circuit histology."
  (element-parameter element 'color color)
  nil)


(defun segment-membrane-resistivity (seg)
  "The membrane resistivity of SEG, in ohms-cm2."
  (let* ((seg (if (segment-p seg) seg (element seg 'segment)))
	 (rm (unless (segment-inherit-parameters-from-type seg)
	       (get-a-value `membrane-resistivity (segment-parameters seg)))))
    (the sf (or rm (cell-type-membrane-resistivity (cell-type (segment-cell seg)))))))
	      

(defun segment-specific-capacitance (seg)
  "The membrane specific capacitance of SEG, in uF/cm2."
  (let* ((seg (if (segment-p seg) seg (element seg 'segment)))
	 (cm (unless (segment-inherit-parameters-from-type seg)
	       (get-a-value `specific-capacitance (segment-parameters seg)))))
    (the sf (or cm (cell-type-dendrite-specific-capacitance (cell-type (segment-cell seg)))))))
	      

(defun segment-cytoplasmic-resistivity (seg)
  "The cytoplasmic resistivity of SEG, in ohms-cm."
  (let* ((seg (if (segment-p seg) seg (element seg 'segment)))
	 (ra (unless (segment-inherit-parameters-from-type seg)
	       (get-a-value 'cytoplasmic-resistivity (segment-parameters seg)))))
    (the sf (or ra (cell-type-cytoplasmic-resistivity (cell-type (segment-cell seg)))))))
	      

(defun segment-v-leak (seg)
  "The reversal potential of the leak resistance of SEG, in mV."
  (let* ((seg (if (segment-p seg) seg (element seg 'segment)))
	 (v-leak (unless (segment-inherit-parameters-from-type seg)
		   (get-a-value `v-leak (segment-parameters seg)))))
    (the sf (or v-leak (cell-type-dendrite-v-leak (cell-type (segment-cell seg)))))))
	      
(defun segment-cytoplasmic-resistivity-coeff (seg)
  "The dimensionless coefficient for the cytoplasmic resistivity of SEG."
  (let ((seg (if (segment-p seg) seg (element seg 'segment))))
    (s-flt (or
	    (get-a-value `ri-coeff (segment-parameters seg))
	    (get-a-value `cytoplasmic-resistivity-coeff (segment-parameters seg))
	    (get-a-value `ra-coeff (segment-parameters seg))
	    1.0))))

(defun segment-ra-coeff (seg)
  "The dimensionless coefficient for the cytoplasmic resistivity of SEG."
  (segment-cytoplasmic-resistivity-coeff seg))


(defun segment-ri-coeff (seg)
  "The dimensionless coefficient for the cytoplasmic resistivity of SEG."
  (segment-cytoplasmic-resistivity-coeff seg))




(defun segment-relative-location (segment)
  (node-relative-location (segment-node-2 segment)))

(defun print-segment (&optional (seg *segment*))
  (let ((seg (element seg 'segment)))
    (format t
	    "Segment ~a (~a, ~a):~A~%"
	    (segment-name seg) (node-name (segment-node-1 seg)) (node-name (segment-node-2 seg))
	    (if *simulation-initialized*
		(format nil " ~,2e mV @ ~,2e ms" (recorded-element-voltage seg) *real-time*)
		""))
    (format t
	    "  v-l ~dmV; g-a ~,2e, g-l ~,2euS; cap ~,2enF; len ~,2e, dia ~,2eum; lat area ~,2eum2, vol ~,2eum3"
	    (segment-v-leak seg) (segment-g-axial seg) (segment-g-leak seg) 
	    (segment-capacitance seg)
	    (segment-length seg) (segment-diameter seg) (element-area seg)
	    (element-volume seg))
    (let ((ri-coeff (or (element-parameter seg 'cytoplasmic-resistivity-coeff)
			(element-parameter seg 'ri-coeff)
			(element-parameter seg 'ra-coeff))))
      (unless (or (not ri-coeff) (= ri-coeff 1.0))
	(format t ", ri-coeff ~,2e" ri-coeff)))

    (format t
	    "~%  Cable electrotonic length ~,2e" (segment-electrotonic-length seg))
    (when (element-parameter seg 'membrane-area-coefficient)
      (format t "~%  Membrane area coefficient: ~,2e" (element-parameter seg 'membrane-area-coefficient)))

    
    (when (element-parameter (element-physical-node seg) 'constant-current)
      (format t "~%  ~,2enA constant current injected into this segment."
	      (element-parameter (element-physical-node seg) 'constant-current)))
    (when (element-parameter seg 'electrode)
      (format t "  This segment is used as an electrode."))
    (when (element-parameter seg 'soma-segment)
      (format t "  This segment is used as part of a virtual soma."))
    (format t "~%")
    (let ((elt-string (PRINT-CELL-ELEMENT-ELEMENTS seg)))
      (when (> (length elt-string) 0) (format t "  This segment has ~A.~%" elt-string)))
    (format t "~%"))
  )

	    
	    


(defun print-segment-location (seg)
  (let ((seg (element seg 'segment)))
    (format t
	    "Segment ~a (~a, ~a):~%" (segment-name seg) (node-name (segment-node-1 seg)) (node-name (segment-node-2 seg)))
    (format t
	    " Proximal node @ [~,2f, ~,2f, ~,2f], distal node @ [~,2f, ~,2f, ~,2f]~%"
	    (nth 0 (node-absolute-location (segment-node-1 seg)))
	    (nth 1 (node-absolute-location (segment-node-1 seg)))
	    (nth 2 (node-absolute-location (segment-node-1 seg)))
	    (nth 0 (node-absolute-location (segment-node-2 seg)))
	    (nth 1 (node-absolute-location (segment-node-2 seg)))
	    (nth 2 (node-absolute-location (segment-node-2 seg))))))

(defun print-segment-relative-location (seg)
  (let ((seg (element seg 'segment)))
    (format t
	    "Segment ~a (~a, ~a):~%" (segment-name seg) (node-name (segment-node-1 seg)) (node-name (segment-node-2 seg)))
    (format t
	    " Proximal node @ [~,2f, ~,2f, ~,2f], distal node @ [~,2f, ~,2f, ~,2f]~%"
	    (nth 0 (node-relative-location (segment-node-1 seg)))
	    (nth 1 (node-relative-location (segment-node-1 seg)))
	    (nth 2 (node-relative-location (segment-node-1 seg)))
	    (nth 0 (node-relative-location (segment-node-2 seg)))
	    (nth 1 (node-relative-location (segment-node-2 seg)))
	    (nth 2 (node-relative-location (segment-node-2 seg))))))


	    
(proclaim '(inline coerce-location-to-float))
(defun coerce-location-to-float (location)
  (if (loop for comp in location when (integerp comp) do (return t) finally (return nil))
      (loop for comp in location collect (float comp))
      location))


#|
(defun find-segment-name (segment-name cell-name &optional dont-check)
  (let ((name
	 (if *use-simple-names*
	     (get-cell-element-simple-name)
	     (if *add-cell-name-to-segs*
		 (format nil "~A-~A" cell-name segment-name)
		 (if (gethash segment-name (soma-hash-table))
		     (format nil "~A-seg" segment-name)
		     (typecase segment-name
		       (string
			(let ((sym (read-from-string segment-name)))
			  (if (numberp sym) sym segment-name)))
		       (t segment-name)))))))
    (unless dont-check
      (when (gethash name (segment-hash-table))
	(sim-error
	 (concatenate
	  'string
	  (format nil "~A already used by a segment in cell ~A!"
		  name
		  (segment-cell (gethash name (segment-hash-table))))
	  (unless *add-cell-name-to-segs*
	    (format nil "~%  Try setting *ADD-CELL-NAME-TO-SEGS* to T." ))))))
					;     (format t "returning the ~A ~A~%" (type-of name) name)
    name))
|#





#|
(defun create-segment (name proximal-node &optional cell
			    &key (diameter 0.0) (length 0.0) (theta 0.0) (phi (* -0.5 pi-single))
			    (relative-location '(0.0 0.0 0.0))
			    relative-location-is-float 
			    absolute-location absolute-location-is-float
			    dummy-proximal-node-location dummy-proximal-node-location-is-float
			    parameter-a-list
			    (cytoplasmic-resistivity-coeff 1.0)
			    (ra-coeff 1.0)
			    (ri-coeff 1.0))
  "Creates an element of type segment. NAME is a string. PROXIMAL-NODE may be either a string \(which
may point to an already created node\), a segment \(whose distal node will be used as the new
segment's proximal node\), a soma or a node. The distal node name is the same as the segment name.
Note that the membrane properties will be set again with the call to
SET-SEGMENTS-MEMBRANE-PARAMETERS since segment dimensions may have to be figured out later when the
segment is defined in terms of its coordinates. RELATIVE-LOCATION is of the segment's distal node
relative to cell soma (microns). The location of the distal node relative to cell soma may also be
determined by including PHI, THETA [radians] and LENGTH [microns] keyword arguments \(see, for
example, the function TREE-CONTROL\). Precedence of location information when segment node locations
are determined is defined in the function LOCATE-DISTAL-NODE. A coefficient for the intracellular
resistivity will be taken from RI-COEFF if it is unequal to 1.0 [the default]. Note that if
*USE-SIMPLE-NAMES* is T the NAME will be ignored (can be NIL)."
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let* ((cell (or (element cell 'cell)
		   (element-cell proximal-node)
		   (sim-error (format nil "create-segment: segment ~a needs a cell reference!" name))))
	 *print-pretty*
	 (cell-name (cell-name cell))
	 (segment-name (find-segment-name name cell-name)))
    (or
     (gethash segment-name (SEGMENT-HASH-TABLE))
     (let* ((n1 (or (element-physical-node proximal-node)
		    (create-node proximal-node :cell cell :is-physical-cell-node t)))
	    (n2 (create-node segment-name :cell cell :is-physical-cell-node t))
	    (ri-coeff (s-flt (if (not (= 1 ri-coeff)) ri-coeff
				 (if (not (= 1 ra-coeff)) ra-coeff
				     cytoplasmic-resistivity-coeff))))
	    (cell-type (cell-type cell)))
       (cond-every
	((and relative-location (not relative-location-is-float))
	 (setq relative-location (coerce-location-to-float relative-location)))
	((and absolute-location (not absolute-location-is-float))
	 (setq absolute-location (coerce-location-to-float absolute-location)))
	((and dummy-proximal-node-location (not dummy-proximal-node-location-is-float))
	 (setq dummy-proximal-node-location (coerce-location-to-float dummy-proximal-node-location))))
       (if (eq n1 n2)
	   (sim-error (format nil "create-segment: duplicate nodes for segment ~a" segment-name))
	   (let ((seg (make-segment :name segment-name :theta (s-flt theta) :phi (s-flt phi)
				    :length (s-flt length) :diameter (s-flt diameter) :node-1 n1 :node-2 n2
				    :parameters parameter-a-list
				    :inherit-parameters-from-type (cell-type-inherit-parameters-from-type cell-type)
				    :dummy-proximal-node-location dummy-proximal-node-location)))
	     (setq *circuit-processed* nil)
	     (setf (node-relative-location n2) relative-location
		   (node-elements n1) (cons seg (node-elements n1))
		   (node-elements n2) (cons seg (node-elements n2))
		   (gethash segment-name (SEGMENT-HASH-TABLE)) seg)
	     (unless (= 1.0 (the sf ri-coeff)) (element-parameter seg 'ri-coeff ri-coeff)) 
	     (when absolute-location (setf (node-absolute-location n2) absolute-location))
	     (setq *make-segment-lists* t)
	     (push seg (cell-segments cell))
	     (setq *segment* seg)
	     seg))))))
|#

#|
(loop for candidate from (+ 1 (hash-table-count (SEGMENT-HASH-TABLE)))
      unless (or (gethash candidate (soma-hash-table)) (gethash candidate (segment-hash-table)))
      do (return candidate))
|#
(defun get-cell-element-simple-name ()
  (loop for candidate from (max 1 *cell-element-simple-name-counter*)
	until (not (or (gethash candidate (soma-hash-table)) (gethash candidate (segment-hash-table))))
	finally (return (setf *cell-element-simple-name-counter* candidate))))




(defun find-segment-name (segment-name cell)
  (or ; (and (gethash segment-name (segment-hash-table)) (segment-name (gethash segment-name (segment-hash-table))))
      (cdr (assoc segment-name (element-parameter cell 'name-map) :test 'equal))))

(defun make-segment-name (segment-name cell &key dont-check)
  (let* ((segment-name (or segment-name (gensym)))
	 (cell (element cell 'cell))
	 synthesized-name)
    (or (find-segment-name segment-name cell)
	(let ((name (cond (*use-simple-names*
			   (setq synthesized-name t)
			   (get-cell-element-simple-name)
					; (incf *simple-name-counter*)
			   )
			  (*add-cell-name-to-segs*
			   (setq synthesized-name t)
			   (format nil "~A-~A" (cell-name cell) segment-name))
			  ((gethash segment-name (soma-hash-table))
			   (setq synthesized-name t)
			   (format nil "~A-seg" segment-name))
			  (t (typecase segment-name
			       (string
				(if (string-has-non-number segment-name)
				    segment-name
				    (progn (setq synthesized-name t)
					   (read-from-string segment-name))))
			       (t segment-name))))))
	  (when synthesized-name
	    (element-parameter cell 'name-map (acons segment-name name (element-parameter cell 'name-map))))
	  (unless dont-check
	    (when (gethash name (segment-hash-table))
	      (sim-error
	       (concatenate
		'string
		(format nil "~A already used by a segment in cell ~A!"
			name
			(segment-cell (gethash name (segment-hash-table))))
		(unless *add-cell-name-to-segs* (format nil "~%  Try setting *ADD-CELL-NAME-TO-SEGS* to T."))))))
	  name))))



(defun create-segment (name proximal-element &optional cell
			    &key (diameter 0.0) (length 0.0) (theta 0.0) (phi (* -0.5 pi-single))
			    (relative-location '(0.0 0.0 0.0))
			    relative-location-is-float 
			    absolute-location absolute-location-is-float
			    dummy-proximal-element-location dummy-proximal-element-location-is-float
			    parameter-a-list
			    (cytoplasmic-resistivity-coeff 1.0)
			    (ra-coeff 1.0)
			    (ri-coeff 1.0))
  (declare (optimize (safety 0) (speed 3) (space 1))) 
  (let* ((cell (or (element cell 'cell) (element-cell proximal-element)
		   (sim-error (format nil "create-segment: segment ~a needs a cell reference!" name))))
	 *print-pretty*
	 (segment-name (make-segment-name name cell)))
    (or
     (gethash segment-name (SEGMENT-HASH-TABLE))
     (let* ((n1 (typecase proximal-element
		  (node proximal-element)
		  ((or segment soma) (element-physical-node proximal-element))
		  (t (cond
		       ((gethash proximal-element (soma-hash-table))
			(soma-node (gethash proximal-element (soma-hash-table))))
		       (t (let* ((proximal-element-name
				  (make-segment-name proximal-element cell :dont-check t))
				 (previous-node (gethash proximal-element-name (node-hash-table))))
			    (or previous-node
				(create-node proximal-element-name :cell cell :is-physical-cell-node t))))))))
	    (n2 (or (gethash segment-name (node-hash-table))
		    (create-node segment-name :cell cell :is-physical-cell-node t)))
	    (ri-coeff (s-flt (if (not (= 1 ri-coeff)) ri-coeff
				 (if (not (= 1 ra-coeff)) ra-coeff
				     cytoplasmic-resistivity-coeff))))
	    (cell-type (cell-type cell)))
       (cond-every
	((and relative-location (not relative-location-is-float))
	 (setq relative-location (coerce-location-to-float relative-location)))
	((and absolute-location (not absolute-location-is-float))
	 (setq absolute-location (coerce-location-to-float absolute-location)))
	((and dummy-proximal-element-location (not dummy-proximal-element-location-is-float))
	 (setq dummy-proximal-element-location (coerce-location-to-float dummy-proximal-element-location))))
       (if (eq n1 n2)
	   (sim-error (format nil "create-segment: duplicate nodes for segment ~a" segment-name))
	   (let ((seg (make-segment :name segment-name :theta (s-flt theta) :phi (s-flt phi)
				    :length (s-flt length) :diameter (s-flt diameter) :node-1 n1 :node-2 n2
				    :parameters parameter-a-list
				    :inherit-parameters-from-type (cell-type-inherit-parameters-from-type cell-type)
				    :dummy-proximal-node-location dummy-proximal-element-location)))
	     (setq *circuit-processed* nil)
	     (setf (node-relative-location n2) relative-location
		   (node-elements n1) (cons seg (node-elements n1))
		   (node-elements n2) (cons seg (node-elements n2))
		   (gethash segment-name (SEGMENT-HASH-TABLE)) seg)
	     (unless (= 1.0 (the sf ri-coeff)) (element-parameter seg 'ri-coeff ri-coeff)) 
	     (when absolute-location (setf (node-absolute-location n2) absolute-location))
	     (setq *make-segment-lists* t)
	     (push seg (cell-segments cell))
	     (setq *segment* seg)
	     (initialize-segment-voltage seg)
	     seg))))))

(proclaim '(inline create-segment-fast))
(defun create-segment-fast (name proximal-element &optional cell
				 &key (diameter 0.0) (length 0.0) (theta 0.0) (phi (* -0.5 pi-single))
				 (relative-location '(0.0 0.0 0.0))
				 absolute-location 
				 dummy-proximal-element-location 
				 parameter-a-list
				 (cytoplasmic-resistivity-coeff 1.0)
				 (ra-coeff 1.0)
				 (ri-coeff 1.0))
  "An optimized version of CREATE-SEGMENT. All numeric arguments are assumed to be single floats,
except if NAME is a number, in which case it must be an integer."
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (single-float diameter length theta phi cytoplasmic-resistivity-coeff ra-coeff ri-coeff))
  (let* (*print-pretty*
	 (cell (if (cell-p cell) cell (element-cell proximal-element)))
	 (cell-name (cell-name cell))
	 (SEGMENT-HASH-TABLE (SEGMENT-HASH-TABLE))
	 (segment-name (make-segment-name name cell-name)))
    (or
     (gethash segment-name SEGMENT-HASH-TABLE)
     (let* ((n1 (typecase proximal-element
		  (node proximal-element)
		  ((or segment soma) (element-physical-node proximal-element))
		  (t (cond
		       ((gethash proximal-element (soma-hash-table))
			(soma-node (gethash proximal-element (soma-hash-table))))
		       (t (let* ((proximal-element-name (make-segment-name proximal-element cell))
				 (previous-node (gethash proximal-element-name (node-hash-table))))
			    (or previous-node
				(create-node proximal-element-name :cell cell :is-physical-cell-node t))))))))
	    (n2 (or (gethash segment-name (node-hash-table))
		    (create-node segment-name :cell cell :is-physical-cell-node t)))
	    (cell-type (cell-type cell)))
       (if (eq n1 n2)
	   (sim-error (format nil "create-segment-fast: duplicate nodes for segment ~a" segment-name))
	   (let ((seg (make-segment :name segment-name :node-1 n1 :node-2 n2 :parameters parameter-a-list
				    :inherit-parameters-from-type (cell-type-inherit-parameters-from-type cell-type)
				    :dummy-proximal-node-location dummy-proximal-element-location))
		 (ri-coeff (cond ((not (= 1 ri-coeff)) ri-coeff)
				 ((not (= 1 ra-coeff)) ra-coeff)
				 (t cytoplasmic-resistivity-coeff))))
	     (setf (segment-length seg) length
		   (segment-diameter seg) diameter
		   (segment-theta seg) theta
		   (segment-phi seg) phi)
	     (setq *circuit-processed* nil)
	     (setf (node-relative-location n2) relative-location
		   (node-elements n1) (cons seg (node-elements n1))
		   (node-elements n2) (cons seg (node-elements n2))
		   (gethash segment-name SEGMENT-HASH-TABLE) seg)
	     (unless (= 1.0 (the sf ri-coeff)) (element-parameter seg 'ri-coeff ri-coeff))
	     (when absolute-location (setf (node-absolute-location n2) absolute-location))
	     (setq *make-segment-lists* t)
	     (push seg (cell-segments cell))
	     (setq *segment* seg)
	     (initialize-segment-voltage seg)
	     seg))))))


(defun set-segment-voltage (segment voltage)
  (set-node-voltage (segment-node-2 segment) voltage))

(defun set-segment-voltage-df (segment voltage)
  (set-node-voltage-double (segment-node-2 segment) voltage))

(defun initialize-segment-voltage (seg &optional v-leak)
  (let ((v-leak (or v-leak (cell-type-dendrite-v-leak (cell-type (segment-cell seg))))))
    (set-segment-voltage seg (or (element-holding-potential seg)
				 (if (= (segment-v-leak seg) v-leak)
				     v-leak
				     (d-flt (segment-v-leak seg)))))))


(defun set-segments-membrane-parameters (&optional ignore-membrane-elements cell-type)
  (loop for seg being the hash-value of (SEGMENT-HASH-TABLE)
	when (or (not cell-type) (eq cell-type (cell-type (segment-cell seg))))
	do (set-segment-membrane-parameters seg ignore-membrane-elements)))


(defun set-segment-membrane-parameters (seg &optional ignore-membrane-elements)
  "Set SEG properties that depend on segment dimensions, and others."
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let* ((proximal-cell-element (PROXIMAL-CELL-ELEMENT seg))
	 (membrane-factor (the sf (or (get-a-value 'membrane-area-coefficient (segment-parameters seg)) 1.0)))
	 (vl (the sf (segment-v-leak seg)))
	 (cm (the sf (segment-specific-capacitance seg)))
	 (rm (the sf (segment-membrane-resistivity seg)))
	 (ra (the sf (segment-cytoplasmic-resistivity seg)))
	 (this-g-axial (the sf (g-axial (segment-length seg)
					(segment-diameter seg)
					(the sf (* ra (segment-ri-coeff seg))))))
	 (g-axial-computation (or (get-a-value 'g-axial-computation (segment-parameters seg))
				  (get-a-value 'g-axial-computation (cell-type-parameters (cell-type (segment-cell seg))))
				  :single-leg))
	 (need-prox-info (case g-axial-computation
			   (:single-leg nil)
			   (t t)))
	 (proximal-seg (when (and need-prox-info (segment-p proximal-cell-element)) proximal-cell-element))
	 (prox-g-axial
	  (when need-prox-info
	    (cond
	      (proximal-seg
	       (g-axial (segment-length proximal-seg)
			(segment-diameter proximal-seg)
			(the sf (* (the sf (segment-cytoplasmic-resistivity proximal-seg))
				   (the sf (segment-ri-coeff proximal-seg))))))
	      (t (the sf (or (and (get-a-value 'soma-cylinder (soma-parameters proximal-cell-element))
				  (get-a-value 'soma-cylinder-g-axial (soma-parameters proximal-cell-element)))
			     0.0)))))))
    (setf (segment-g-axial seg)
	  (case g-axial-computation
	    (:average-g (/ (+ (the sf prox-g-axial) this-g-axial) 2.0d0))
	    (:average-r (if (= 0.0 (the sf prox-g-axial))
			    (coerce this-g-axial 'double-float)
			    (/ 2.0d0 (+ (/ 1.0 prox-g-axial) (/ 1.0 this-g-axial)))))
	    (t (coerce this-g-axial 'double-float))))

    (setf (segment-g-leak seg)
	  (d-flt (g-leak-mem (segment-length seg) (segment-diameter seg) (/ rm membrane-factor))))

    (setf (segment-capacitance seg)
	  (d-flt (cap-mem (segment-length seg) (segment-diameter seg) (* cm membrane-factor))))
    (setf (segment-guts-g-leak*v-leak (segment-guts seg)) (* (segment-g-leak seg) vl))
    (unless ignore-membrane-elements
      (loop for elt in (node-elements (segment-node-2 seg))
	    unless (segment-p elt)
	    do (set-element-membrane-parameters elt t))))
  nil)
  


(defun set-segments-inherit-parameters-from-type (&optional cell)
  "Makes all segments in CELL (if supplied) or in the circuit (else) inherit their properties from
the associated CELL-TYPE."
  (loop for seg in (if cell (segments cell) (segments)) do (setf (segment-inherit-parameters-from-type seg) t)))


(defun set-segment-absolute-parameters (seg capacitance g-axial g-leak)
  "Set cable properties of SEG to the absolute values of CAPACITANCE [nF], G-AXIAL and G-LEAK [uS].
Sets :INHERIT-PARAMETERS-FROM-TYPE of SEG to NIL. If any of the segment parameter arguments are NIL,
then the original value is retained."
  (loop for seg in (coerce-to-list (element seg 'segment)) do
	(cond-every (capacitance (setf (segment-capacitance seg) (d-flt capacitance)))
		    (g-axial (setf (segment-g-axial seg) (d-flt g-axial)))
		    (g-leak (setf (segment-g-leak seg) (d-flt g-leak))))
	(setf (segment-inherit-parameters-from-type seg) nil)
	(element-parameter seg 'g-axial-computation nil) ; default is :single-leg
	(when (element-parameter seg 'ra-COEFF) (element-parameter seg 'ra-COEFF 1.0))
	(when (element-parameter seg 'ri-COEFF) (element-parameter seg 'ri-COEFF 1.0))
	;; Adjust local membrane parameters to reflect absolute value assignment.
	(element-parameter seg 'membrane-resistivity (s-flt (* 1e6 (/ (element-area-cm2 seg) (segment-g-leak seg)))))
	;; (* 1.0e6 pi-single 1.0e-4 0.25) = 78.53982
	(element-parameter seg 'cytoplasmic-resistivity (s-flt (/ (* 78.53982 (segment-diameter seg) (segment-diameter seg))
								  (* (segment-length seg) (segment-g-axial seg))))    )
	(element-parameter seg 'specific-capacitance (s-flt (* 1e-3 (/ (segment-capacitance seg)
								       (element-area-cm2 seg)))))))

(defun init-segments ()
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (loop for seg being the hash-value of (SEGMENT-HASH-TABLE) do
	(setf (segment-guts-g-leak*v-leak (segment-guts seg)) (* (segment-g-leak seg) (segment-v-leak seg)))))


(defun set-segment-parameter (seg parameter value)
  "Set a PARAMETER distinct from the associated cell type for segments associated with SEG, for
example including 'MEMBRANE-RESISTIVITY, 'SPECIFIC-CAPACITANCE, 'V-LEAK, 'CYTOPLASMIC-RESISTIVITY, or
'RI-COEFF. Sets :INHERIT-PARAMETERS-FROM-TYPE for SEG to NIL."
  (loop for seg in (coerce-to-list (element seg 'segment)) do
	(setf (segment-inherit-parameters-from-type seg) nil)
	(element-parameter seg parameter value)
	(set-segment-membrane-parameters seg)))

(defun rename-segments-simple (&optional segments)
  "Rename SEGMENTS [default all segments in circuit] with simple integer names."
  (loop for seg in (coerce-to-list (or segments (segments))) do
	(let ((name (get-cell-element-simple-name)))
	  (set-element-name seg name 'segment)
	  (set-element-name (segment-node-2 seg) name 'node))))



	  

(defun edit-segment (seg)
  (let ((seg (element seg 'segment)))
    (if (electrode-p seg)
	(edit-electrode seg)
	(let* ((dummy1 (segment-membrane-resistivity seg))
	       (dummy2 (segment-specific-capacitance seg))
	       (dummy4 (segment-cytoplasmic-resistivity seg))
	       (dummy6 (segment-v-leak seg))	  
	       (dummy3 (segment-inherit-parameters-from-type seg))
	       (dummy7 (segment-ri-coeff seg))
	       (dummy8 (or (element-parameter seg 'membrane-area-coefficient) 1.0))
	       dummy9 dummy5
	       (cell-type-g-axial-computation (or (element-parameter (cell-type (segment-cell seg)) 'g-axial-computation)
						  :single-leg))
	       (dummy10 (or (element-parameter seg 'g-axial-computation) cell-type-g-axial-computation)))
	  (choose-variable-values
	   `((dummy1 "Dendrite segment membrane resistivity [ohm-cm-sq]" :float) ;
	     (dummy2 "Dendrite segment specific capacitance [uF/cm-sq]" :float)
	     (dummy4 "Dendrite segment cytoplasmic resistivity [ohm-cm] (>0)" :float)
	     (dummy6 "Dendrite segment leak battery [mV]" :float) ;
	     (dummy7 "Cytoplasmic resistivity coefficient [1.0 => no effect]" :float)
	     (dummy3 "Ignore values listed above and inherit from cell type" :boolean)
	     (dummy8 "Membrane area coefficient" :float)
	     ("Single leg method considers only this segment's properties for calculating g-axial." :comment)
	     (dummy10
	      ,(format nil
		"Method for computing compartment coupling (g-axial)~%(Cell type ~A method is ~A):"
		(cell-type-name (cell-type (segment-cell seg))) cell-type-g-axial-computation)
	      :choose (:single-leg :average-r :average-g))
	     (dummy9 "Edit segment absolute values" :boolean) 
	     (dummy5 "Edit cell type" :boolean))
	   :label (format nil "Edit Segment ~a" (segment-name seg)))
	  (clear-z-cable-in-to-soma seg)
	  (when (cond ((not (element-parameter seg 'membrane-area-coefficient))
		       (not (= dummy8 1.0)))
		      ((not (= (element-parameter seg 'membrane-area-coefficient) dummy8))))
	    (element-parameter seg 'membrane-area-coefficient dummy8))
	  (when (cond ((not (element-parameter seg 'ri-COEFF))
		       (not (= dummy7 1.0)))
		      ((not (= (element-parameter seg 'ri-COEFF) dummy7))))
	    (element-parameter seg 'ri-COEFF dummy7))
	  (element-parameter seg 'g-axial-computation dummy10)    
	  (unless dummy3
	    (element-parameter seg 'cytoplasmic-resistivity dummy4)
	    (element-parameter seg 'membrane-resistivity dummy1)
	    (element-parameter seg 'specific-capacitance dummy2)
	    (element-parameter seg 'v-leak dummy6))
	  (setf (segment-inherit-parameters-from-type seg) dummy3)
	  (when dummy9 (edit-segment-absolute seg))
	  (when dummy5 (edit-cell-type (cell-type (segment-cell seg))))
	  (unless dummy9
	    (set-segment-membrane-parameters seg)
	    (update-linear-z-in (segment-cell seg)))))))

(defun edit-segment-absolute (seg &optional (text "") (use-g-axial-p t))
  (let* ((seg (element seg 'segment))
	 (dummy1 (s-flt (segment-capacitance seg)))
	 (dummy2 (if use-g-axial-p
		     (s-flt (segment-g-axial seg))
		     (/ 1.0 (s-flt (segment-g-axial seg)))))
		     
	 (dummy3 (s-flt (segment-g-leak seg)))
	 (cell-type-g-axial-computation (or (element-parameter (cell-type (segment-cell seg)) 'g-axial-computation)
					    :single-leg))
	 (dummy10 (or (element-parameter seg 'g-axial-computation) cell-type-g-axial-computation)))
    (choose-variable-values
     `((dummy1 "Absolute capacitance [nF]" :float)
       (dummy2
	,(if use-g-axial-p
	     "Absolute axial conductance [uS]"
	     "Absolute axial resistance [Mohms, must be >0]")
	:float)
       (dummy3 "Absolute membrane conductance [uS]" :float)
       ("Single leg method considers only this segment's properties for calculating g-axial." :comment)
       (dummy10
	,(format nil
	  "Method for computing compartment coupling (g-axial)~%(Cell type ~A method is ~A):"
	  (cell-type-name (cell-type (segment-cell seg))) cell-type-g-axial-computation)
	:choose (:single-leg :average-r :average-g)))
     :text text :label (format nil "Segment ~a: Edit Absolute Linear Parameters" (segment-name seg)))
    (clear-z-cable-in-to-soma seg)
    (element-parameter seg 'g-axial-computation nil)
    (unless (eq dummy10 :single-leg)
      (element-parameter seg 'g-axial-computation dummy10))
    (set-segment-absolute-parameters seg dummy1
				     (if use-g-axial-p dummy2 (/ 1.0 dummy2))
				     dummy3)
    (set-segment-membrane-parameters seg)
    (update-linear-z-in (segment-cell seg)))
  nil)



(defun print-all-segment-nodes ()
  (maphash 'print-segment-nodes (SEGMENT-HASH-TABLE)))

(defun print-segment-nodes (name seg)
  (format t "Segment ~a: " name)
  (format t "node ~a, location ~A" (node-name (segment-node-1 seg))
	  (node-relative-location (segment-node-1 seg)))
  (format t "node ~a, location ~A~%" (node-name (segment-node-2 seg))
	  (node-relative-location (segment-node-2 seg))))

(defun print-node-segments (name nd)
  (format t "Node ~a: " name)
  (loop for element in (node-elements nd)
	when (eq (named-structure-symbol element) 'segment)
	do (format t " ~a " (segment-name element))))

(defun print-virtually-connected-segments ()
  (loop for seg in (segments)
	when (segment-dummy-proximal-node-location seg)
	do (print-element seg))) 

;;; LOOP-CHECK Find any loops in the circuit trees by sucessive calls to SEGMENTS-OUT. If a loop is
;;; found, then SEGMENTS-OUT signals an error.
(defun loop-check (&optional exclude-segments)
  (loop for seg being the hash-value of (SEGMENT-HASH-TABLE) 
	unless (or (loop for tested-seg-list in tested-seg-lists
			 when (loop for tested-seg in tested-seg-list when (eq seg tested-seg) do (return t))
			 do (return t))
		   (loop for exclude in exclude-segments
			 when (eq seg (element exclude))
			 do (return t)))
	collect (segments-out seg 0) into tested-seg-lists))


;; CONNECT-LOOSE-SEGMENTS Finds all segments which are not connected to anything by their proximal
;; node. Attaches them electrically to the segment or soma which is closest to the proximal node
;; location. If the original proximal node does not have anything else attached to it, then it is
;; destroyed.
(defun connect-loose-segments (&optional cell)
  (loop for cell in (or (and cell (list cell)) (cells)) do
	(let ((candidates (cell-segments cell)))
	  (move-relative-to-absolute cell)
	  (loop for seg in (loose-segments cell) do
		;;		(format t "connecting loose ~A~%" seg)
		(setf (segment-dummy-proximal-node-location seg) (node-relative-location (segment-node-1 seg)))
		(move-proximal-node
		 seg
		 (closest-element seg
				  :exclude-these-elements (segments-out seg)
				  :proximal-measure t :candidates candidates))))))



(defun move-relative-to-absolute (&optional cell)
  (loop for cell in (or (and cell (list cell)) (cells)) do
	(setf (node-absolute-location (soma-node (cell-soma cell)))
	      (node-relative-location (soma-node (cell-soma cell))))
	(loop for seg in (cell-segments cell) do
	      (setf (node-absolute-location (segment-node-1 seg)) (node-relative-location (segment-node-1 seg))
		    (node-absolute-location (segment-node-2 seg)) (node-relative-location (segment-node-2 seg))))))


;; LOOSE-SEGMENTS Returns all segments which are not connected to anything by their proximal node.
(defun loose-segments (&optional cell)
  (loop for cell in (or (and cell (list cell)) (cells))
	nconc (loop for seg in (cell-segments cell)
		 unless (or (eq (segment-node-1 seg) (soma-node (cell-soma cell))) (proximal-segment seg))
		 collect seg)))


(defun clean-up-cell-segments (&optional cells)
  (loop for cell in (or cells (cells)) do
	(setf (cell-segments cell)
	      (loop for seg in (cell-segments cell)
		    when (gethash (segment-name seg) (SEGMENT-HASH-TABLE))
		    collect seg))))


;;; MOVE-PROXIMAL-NODE Move the proximal end of SEGMENT and attach it to NEW-PROXIMAL-CELL-ELEMENT. If
;;; the original proximal node does not have anything else attached to it, then it is destroyed. 
(defun move-proximal-node (segment new-proximal-cell-element)
  (let* ((segment (element segment))
	 (new-proximal-cell-element (element new-proximal-cell-element))
	 (old-proximal-node (segment-node-1 segment)))
    (push segment (node-elements (element-physical-node new-proximal-cell-element)))
    (setf (segment-node-1 segment) (element-physical-node new-proximal-cell-element)
	  (node-elements old-proximal-node) (remove segment (node-elements old-proximal-node)))
    (unless (node-elements old-proximal-node) (erase-element old-proximal-node))))

(defun erase-branch (proximal-segment &optional (top-level t))
  (let* ((proximal-segment (element proximal-segment 'segment))
	 (distal-segments (distal-segments proximal-segment)))
    (erase-element proximal-segment 'segment nil)
    (loop for seg in distal-segments do (erase-branch seg nil))
    (when top-level (CLEAN-UP-CELL-SEGMENTS) (process-circuit-structure t))
    nil))

(defun erase-branch (proximal-segment)
  (erase-element proximal-segment 'segment nil)
  (CLEAN-UP-CELL-SEGMENTS))


;;; LOCATE-ALL-NODES Complete the geometrical description of the cells' nodes, after the circuit has
;;; been loaded.
(defun locate-all-nodes ()
  (setf *branch-list* '())
  (loop for cell being the hash-value of (CELL-HASH-TABLE) do (locate-cell-nodes cell t))
  (maphash 'set-cells-coordinate-extrema (NODE-HASH-TABLE))
  (loop for cell being the hash-value of (CELL-HASH-TABLE) do (UPDATE-CELL-AREA cell)))


(defun locate-unconnected-nodes (&optional cell)
  (loop for cell in (or (and cell (list cell)) (cells))	do
	(loop for seg in (cell-segments cell)
	      unless (or (eq (segment-node-1 seg) (soma-node (cell-soma cell))) (proximal-segment seg))
	      do
	      (locate-distal-node seg)
	      (build-branch-list seg)
	      (get-and-locate-distal-segments seg))))

(proclaim '(notinline compute-segment-length))
(defun compute-segment-length (one-end other-end)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type cons one-end other-end))
  (do* ((num-1 one-end (cdr num-1))
	(num-2 other-end (cdr num-2))
	(diff (- (the sf (car num-2)) (the sf (car num-1)))
	      (- (the sf (car num-2)) (the sf (car num-1))))
	(sum (* diff diff)
	     (+ (* diff diff)
		(the sf sum))))
       ((null num-1) (the sf (if (plusp sum) (sqrt (the sf sum)) 0.0)))))



;;; LOCATE-CELL-NODES Cell nodes are located seqentially from the soma outward.
(defun locate-cell-nodes (cell &optional make-branches look-for-loops)
  ;; Make sure soma is located first.
  (setf (node-absolute-location (soma-node (cell-soma cell)))
        (mapcar '+ (cell-origin cell) (node-relative-location (soma-node (cell-soma cell)))))
  ;; Start at the cell soma, and work on each of the branches that originate there in turn.
  (loop for seg in (get-node-elements-of-type (cell-soma cell) 'segment) do
	(locate-distal-node seg)
	(when make-branches (build-branch-list seg))
	(get-and-locate-distal-segments seg nil make-branches look-for-loops)))
  

;;; DISTAL-SEGMENTS
(defun distal-tip-p (element)
  "Predicate for whether cell element associated with ELEMENT is located on a distal tip of the
dendritic tree."
  (= (length (distal-segments element)) 0))

(defun distal-segments (element &optional include-electrodes)
  "Returns a list of all the segments directly attached to the distal node of segment associated with
ELEMENT, or the trunk segments if ELEMENT is associated with the soma. Electrode segments are not
included, unless INCLUDE-ELECTRODES is T [default NIL]."
  (let ((cell-element (element-cell-element element)))
    (typecase cell-element
      (soma (trunk-segments cell-element))
      (segment
       (loop for element in (node-elements (segment-node-2 cell-element))
	     ;; Collect all segments attached to this one's distal node.
	     when (and (segment-p element) ; Just want segments
		       (or include-electrodes (not (electrode-p element t)))
		       (not (eq cell-element element))) ; Avoid parent segment.
	     collect element)))))

(defun SEGS-UNTIL-BIFURCATION (seg)
  "Given a segment, returns all distal segments, including this one, before the next branch point in
the tree."
  (let ((distal-segs (distal-segments seg)))
    (if (= 1 (length distal-segs))
	(cons seg (segs-until-bifurcation (car distal-segs)))
	(list seg))))


;;; GET-AND-LOCATE-DISTAL-SEGMENTS
#|
(defun get-and-locate-distal-segments (segment &optional originals make-branches look-for-loops)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (format t "finding ~A segments distal to segment ~a ~%" (length (distal-segments segment))(segment-name segment))
  (loop for distal-segment in (distal-segments segment)
	when (member distal-segment originals :test 'equal) do (format t "Found loop at ~A!!!~%" distal-segment) (break)
	do
	(locate-distal-node distal-segment segment)
	(when make-branches (build-branch-list distal-segment))
	(get-and-locate-distal-segments
	 distal-segment (when look-for-loops (cons distal-segment originals)) make-branches look-for-loops)))
|#

(defun get-and-locate-distal-segments (segment &optional originals make-branches look-for-loops)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  ;;  (format t "finding segments distal to segment ~a ~%" (segment-name segment))
  (loop for distal-segment in (distal-segments segment t)
	when (member distal-segment originals :test 'equal) do (format t "Found loop at ~A!!!~%" distal-segment) (break)
	do
	(locate-distal-node distal-segment segment)
	(when make-branches (build-branch-list distal-segment))
	(get-and-locate-distal-segments
	 distal-segment (when look-for-loops (cons distal-segment originals)) make-branches look-for-loops)))


;;; LOCATE-DISTAL-NODE This function is called under the assumption that the location of the
;;; proximal node (the segment :NODE-1) has already been determined. This means that segments' node
;;; locations must be determined from the soma OUT. Segment locations are specified in one of two
;;; ways: 1] in terms of segment length and the phi and theta parameters relative to the proximal
;;; segment, and 2] in term of the coordinates of their nodes, relative to the soma. In the first
;;; case a segment's distal node :RELATIVE-LOCATION will be equal to '(0.0 0.0 0.0); in the second
;;; case this list will have a nonzero coordinate.  When LOCATE-DISTAL-NODE is finished, the segment
;;; distal nodes relative and absolute locations and its length will be set.
(defun locate-distal-node (seg &optional proximal-segment)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let* ((cell (segment-cell seg))
	 (distal-node (segment-node-2 seg))
	 (relative-location (the cons (node-relative-location distal-node)))
	 (proximal-node (segment-node-1 seg))
	 proximal-segment-end
	 (phi-distal 0.0) (theta-distal 0.0)
	 (length (the sf (segment-length seg))))
    (declare (single-float phi-distal theta-distal length))
    ;;    (format t "Proxi of seg ~a is ~a ~%" (segment-name seg) (if proximal-segment (segment-name proximal-segment)))
    (cond (proximal-segment		; If no proximal segment, then this segment comes from soma.
	   (setq proximal-segment-end (node-relative-location proximal-node))
	   (if (equal relative-location '(0.0 0.0 0.0))
	       (let* ((proximal-segment-proximal-node (segment-node-1 proximal-segment))
		      (proximal-segment-start (node-relative-location proximal-segment-proximal-node))
		      (proximal-segment-length (segment-length proximal-segment)))
		 (declare (single-float proximal-segment-length))
		 (setq phi-distal (acos (the sf
					     ;; The bounding between 1 and -1 is for any roundoff
					     ;; errors busting the ACOS.
					     (bound-val (/ (the sf
								(- (the sf (third proximal-segment-end))
								   (the sf (third proximal-segment-start))))
							   proximal-segment-length) 1.0 -1.0))))
		 (setq theta-distal (if (= (the sf (first proximal-segment-end)) (the sf (first proximal-segment-start)))
					(let ((dif (- (the sf (second proximal-segment-end))
						      (the sf (second proximal-segment-start)))))
					  (cond ((= 0.0 dif) 0.0)
						((> dif 0.0) pi-over-2)
						(t (- pi-over-2))))
					(the sf
					     (atan 
					      (- (the sf (second proximal-segment-end))
						 (the sf (second proximal-segment-start)))

					      (- (the sf (first proximal-segment-end))
						 (the sf (first proximal-segment-start))))))))
	       (setf (segment-length seg)
		     (compute-segment-length proximal-segment-end relative-location))))
	  ;; Branches originating from soma are assumed to be coming
	  ;; out from "branches" that lie in the xy plane.
	  (t (setq proximal-segment-end '(0.0 0.0 0.0))
	     (if (equal relative-location '(0.0 0.0 0.0))
		 (setq phi-distal pi-over-2 theta-distal 0.0)
		 (setf (segment-length seg)
		       (compute-segment-length 
			(node-relative-location distal-node)
			(or (segment-dummy-proximal-node-location seg)
			    (node-relative-location proximal-node)))))))
    ;; See if relative location of distal node has been calculated - if not, do so.
    (when (equal relative-location (list 0.0 0.0 0.0))
      (let ((theta (segment-theta seg))
	    (phi (segment-phi seg)))
	(setf (node-relative-location distal-node)
	      (list
	       (+ 
		(* length
		   (sin (+ phi phi-distal))
		   (cos (+ theta theta-distal)))
		(the sf (first proximal-segment-end)))

	       (+ (* length
		     (sin (+ phi phi-distal))
		     (sin (+ theta theta-distal)))
		  (the sf (second proximal-segment-end)))
	       (+ (* length
		     (cos (+ phi phi-distal)))
		  (the sf (third proximal-segment-end)))))))
    (when *calculate-absolute-locations*
      (setf (node-absolute-location distal-node)
	    (if (equal (cell-origin cell) (list 0.0 0.0 0.0))
		(node-relative-location distal-node)
		(list 
		 (+ (the sf (first (cell-origin cell)))
		    (the sf (first (node-relative-location distal-node))))

		 (+ (the sf (second (cell-origin cell)))
		    (the sf (second (node-relative-location distal-node))))

		 (+ (the sf (third (cell-origin cell)))
		    (the sf (third (node-relative-location distal-node))))))))
    nil))

(defun proximal-cell-element (elt)
  "Returns the proximal cell element (segment or soma) associated with the cell element of ELT. If ELT
is on the soma, then the soma is returned."
  (let ((cell-elt (element-cell-element elt)))
    (typecase cell-elt
      (soma cell-elt)
      (segment
       (or (proximal-segment cell-elt)
	   (loop for elt in (node-elements (segment-node-1 cell-elt))
		 when (soma-p elt) do (return elt)))))))


(defun proximal-segment (elt)
  "Returns the proximal segment associated with the cell element of ELT, if there is one."
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((cell-elt (element-cell-element elt)))
    (typecase cell-elt
      (soma nil)
      (segment
       (loop for element in (node-elements (segment-node-1 cell-elt))
	     when (and (segment-p element)
		       (not (electrode-p element))
		       (eq (segment-node-1 cell-elt) (segment-node-2 element)))
	     do (return element))))))

(defun attached-to-soma-p (element)
  "True if ELEMENT is either a soma, a membrane element of a soma, or a trunk segment."
  (let ((element element))
    (typecase element
      (soma t)
      (segment (not (proximal-segment element)))
      (t (soma-p (proximal-cell-element element))))))

(defun find-proximal-segment (seg) (proximal-segment seg))

;;; SET-CELLS-COORDINATE-EXTREMA Update the cells' extreme coordinate values.
(defun set-cells-coordinate-extrema (name node)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (ignore name))
  (unless (equal node *ground-node*)
    (setf (cell-max-x (node-cell node)) (max (the sf (cell-max-x (node-cell node)))
					     (the sf (first (node-absolute-location node))))
	  (cell-max-y (node-cell node)) (max (the sf (cell-max-y (node-cell node)))
					     (the sf (second (node-absolute-location node))))
	  (cell-max-z (node-cell node)) (max (the sf (cell-max-z (node-cell node)))
					     (the sf (third (node-absolute-location node))))
	  (cell-min-x (node-cell node)) (min (the sf (cell-min-x (node-cell node)))
					     (the sf (first (node-absolute-location node))))
	  (cell-min-y (node-cell node)) (min (the sf (cell-min-y (node-cell node)))
					     (the sf (second (node-absolute-location node))))
	  (cell-min-z (node-cell node)) (min (the sf (cell-min-z (node-cell node)))
					     (the sf (third (node-absolute-location node)))))))

;;; Adds off diagonal entries for this segment.
(defun add-off-diag-segment (seg diag off-diag off-diag-entry)
  (declare (ignore diag))
  (let ((node1 (segment-node-1 seg))
	(node2 (segment-node-2 seg)))
    (cond
      ((eq off-diag node1)
       (setf (segment-mat-21-point seg) off-diag-entry))
      ((eq off-diag node2)
       (setf (segment-mat-12-point seg) off-diag-entry)))))

(defun clear-matrix-pointers ()
  (loop for seg being the hash-value of (SEGMENT-HASH-TABLE) do
	(setf (segment-mat-21-point seg) nil
	      (segment-mat-12-point seg) nil)))

(proclaim '(inline get-segment-voltage-1-double))
(defun get-segment-voltage-1-double (seg)
  (the df (node-voltage-n (segment-node-1 seg))))

(proclaim '(inline get-segment-voltage-2))
(defun get-segment-voltage-2 (seg)
  (the df (node-voltage-n (segment-node-2 seg))))

(defun segment-voltage (seg)
  (let ((seg (element seg 'segment)))
    (when seg (s-flt (node-voltage-n (segment-node-2 seg))))))


(proclaim '(inline eval-segment))
(defun eval-segment (segment-guts segment-node-2-floats)
  (declare (optimize (safety 0) (speed 3) (space 0) (debug 0))
	   (type segment-df-array segment-guts)
	   (type node-df-array segment-node-2-floats))
  (let* ((alpha-cap-double (* (the df (*2/delta-t[n]*)) (segment-guts-capacitance segment-guts)))
	 (node-double-floats segment-node-2-floats))
    (declare (double-float alpha-cap-double))

;    (when *debug-segments*
;      (format t " (~f + ~f) => "
;              (* (node-aref-voltage-n node-double-floats) alpha-cap-double)
;              (node-aref-alpha-charge node-double-floats)))
    
    (setf
     (node-aref-alpha-charge node-double-floats)
     (+ (node-aref-alpha-charge node-double-floats) (* alpha-cap-double (node-aref-voltage-n node-double-floats)))
       
     (node-aref-jacobian node-double-floats) (+ (node-aref-jacobian node-double-floats) alpha-cap-double))

;;    (when *debug-segments* (format t " Result: ~f~%" (node-aref-alpha-charge node-double-floats)))
    )
  nil)

(defun eval-all-segments ()
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (loop for segment-guts in *segment-guts-list*
	for segment-node-2-floats in *segment-node-2-floats-list*
	do (eval-segment segment-guts segment-node-2-floats))
  nil)



;;; Calculate the time invariant component of the node jacobians, which consist of segment g-axial
;;; terms, and soma and segment g-leak terms.
(defun initialize-seg-node-jacobians-and-currents ()
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (segment-iterator
   (progn
     (unless (or (node-has-ideal-voltage-source (segment-node-1 seg))
		 (node-has-ideal-voltage-source (segment-node-2 seg)))
       (accumulate-setf (node-const-jacobian (segment-node-1 seg)) (segment-g-axial seg)))

     (unless (node-has-ideal-voltage-source (segment-node-2 seg))
       (accumulate-setf
	(node-const-jacobian (segment-node-2 seg))
	(+ (if (node-has-ideal-voltage-source (segment-node-1 seg)) 0.0d0 (segment-g-axial seg)) (segment-g-leak seg)))
       (deccumulate-setf (node-const-current (segment-node-2 seg)) (segment-guts-g-leak*v-leak (segment-guts seg)))))))



(defun fix-up-off-diags ()
  ;; take care of the off diag arrays
  (add-segs-to-off-diags))

(defun add-segs-to-off-diags ()
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (dotimes (i *num-unknowns*)
    (setf (aref (the (simple-array double-float) *lower-diag-double*) i) 0.0d0
	  (aref (the (simple-array double-float) *upper-diag-double*) i) 0.0d0))
  (loop for seg being the hash-value of (SEGMENT-HASH-TABLE)
	unless (node-has-ideal-voltage-source (segment-node-2 seg))
	do (add-seg-params-to-off-diag seg)))

(defun add-seg-params-to-off-diag (seg)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (when (segment-mat-21-point seg)
    (let ((off-diag-entry (segment-mat-21-point seg))
	  (node2 (segment-node-2 seg)))
      (if (core-off-diag-lower off-diag-entry)
	  (deccumulate-setf (aref *lower-diag-double* (node-index node2)) (segment-g-axial seg))
	  (deccumulate-setf (aref *upper-diag-double* (node-index node2)) (segment-g-axial seg)))))
  (when (segment-mat-12-point seg)
    (let ((off-diag-entry (segment-mat-12-point seg))
	  (node1 (segment-node-1 seg)))
      (if (core-off-diag-lower off-diag-entry)
	  (deccumulate-setf (aref *lower-diag-double* (node-index node1)) (segment-g-axial seg))
	  (deccumulate-setf (aref *upper-diag-double* (node-index node1)) (segment-g-axial seg)))))
  nil)
  

(defun get-segment-name (name seg)
  (declare (ignore seg)) (print name))

(defun get-segment-cell-name (name seg)
  (declare (ignore name)) (print (cell-name (segment-cell seg))))

(defun make-segment-lists ()
  (when *make-segment-lists*
    (loop for segment being the hash-value of (SEGMENT-HASH-TABLE)
	  unless (node-has-ideal-voltage-source (segment-node-2 segment))
	  collect (node-double-floats (segment-node-2 segment)) into segments-node-floats
	  and collect (segment-guts segment) into segments-guts
	  finally (setq *segment-guts-list* segments-guts
			*segment-node-2-floats-list* segments-node-floats))
    (setq *make-segment-lists* nil)))








