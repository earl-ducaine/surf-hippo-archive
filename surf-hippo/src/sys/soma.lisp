;; -*- mode: lisp; Syntax: Common-lisp; package: surf ; base: 10 ; -*-
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


;;; SYS Source file: soma.lisp
(in-package "SURF-HIPPO")

;
; The soma model.
;

;
; There is no soma-type since the soma parameters are defined by the cell-type.
;


(defun soma-membrane-resistivity (soma)
  "The membrane resistivity of SOMA, in ohms-cm2." 
  (let* ((soma (if (soma-p soma) soma (element soma 'soma)))
	 (rm (unless (soma-inherit-parameters-from-type soma)
	       (get-a-value `membrane-resistivity (soma-parameters soma)))))
    (the sf (or rm (cell-type-soma-resistivity (cell-type (soma-cell soma)))))))
	      
	    
(defun soma-specific-capacitance (soma)
  "The membrane specific capacitance of SOMA, in uF/cm2."
  (let* ((soma (if (soma-p soma) soma (element soma 'soma)))
	 (cm (unless (soma-inherit-parameters-from-type soma)
	       (get-a-value `specific-capacitance (soma-parameters soma)))))
    (the sf (or cm (cell-type-soma-specific-capacitance (cell-type (soma-cell soma)))))))
	      

(defun soma-v-leak (soma)
  "The reversal potential of the leak resistance of SOMA, in mV." 
  (let* ((soma (if (soma-p soma) soma (element soma 'soma)))
	 (v-leak (unless (soma-inherit-parameters-from-type soma)
		   (get-a-value `v-leak (soma-parameters soma)))))
    (the sf (or v-leak (cell-type-soma-v-leak (cell-type (soma-cell soma)))))))
	      

(defun add-soma-segment (soma seg-or-segs &optional initialize)
  (let ((soma (element soma 'soma))
	(seg-or-segs
	 (no-nils
	  (loop for seg in (if (consp seg-or-segs) seg-or-segs (list seg-or-segs))
		collect (element seg 'segment)))))
    (when (and soma seg-or-segs)
      (loop for seg in seg-or-segs do (element-parameter seg 'soma-segment t))
      (element-parameter soma 'segments
			     (if initialize
				 seg-or-segs
				 (delete-duplicates (concatenate 'list
								 seg-or-segs
								 (element-parameter soma 'segments))))))))

(defun remove-soma-segment (soma seg)
  (let ((soma (element soma 'soma))
	(seg (element seg 'segment)))
    (element-parameter seg 'soma-segment nil)
    (element-parameter soma 'segments (remove seg (element-parameter soma 'segments)))))

;; in uS
(defun soma-segments-g-leak (&optional soma-or-cell)
  (loop for seg in (soma-segments soma-or-cell) summing (segment-g-leak seg)))

;; in nF
(defun soma-segments-cap (&optional soma-or-cell)
  (loop for seg in (soma-segments soma-or-cell) summing (segment-capacitance seg)))

(defun print-soma (&optional (soma *soma*))
  "Prints data associated with SOMA."
  (format t
	  "Soma ~a (~a):~A~%   ~av-l ~dmV"
	  (soma-name soma)
	  (cell-name (soma-cell soma))
	  (if *simulation-initialized*
	      (format nil " ~,2e mV @ ~,2e ms" (recorded-element-voltage soma) *real-time*)
	      "")
	  (if (and (> (soma-g-shunt soma) 1e-6) (soma-include-shunt soma))
	      (format nil " g-shunt ~,2euS, " 
		      (if (> (soma-g-shunt soma) 1e-6)  (soma-g-shunt soma) 0))
	      "")
	  (soma-v-leak soma))

  (if (<= (soma-diameter soma) 0.1)
      (format t " Model soma parameters negligible - refer to virtual soma segments~%")
      (format t
	      ", g-l ~,2euS, cap ~,2enF, diam ~,2eum, area ~,2eum2, vol ~,2eum3~%"
	      (soma-g-leak soma)
	      (soma-capacitance soma)
	      (soma-diameter soma)
	      (element-area soma)
	      (element-volume soma)))
  (when (element-parameter soma 'adjust-area-for-trunks)
    (format t "   Soma area adjusted for abutting segment trunks.~%"))
  (when (element-parameter soma 'nucleus-diameter)
    (format t "   Soma nucleus diameter ~,2eum, vol ~,2eum3~%"
	    (element-parameter soma 'nucleus-diameter)
	    (sphere-volume-from-diameter (element-parameter soma 'nucleus-diameter))))
  (when (element-parameter soma 'membrane-area-coefficient)
    (format t "  Membrane area coefficient: ~,2e~%" (element-parameter soma 'membrane-area-coefficient)))
  (when (element-parameter soma 'SOMA-CYLINDER)
    (format t "  Soma has cylinder flag set~A~A~A~%"
	    (if (element-parameter soma 'SOMA-CYLINDER-diameter)
		(format nil " - cylinder diameter ~,2eum" (element-parameter soma 'SOMA-CYLINDER-diameter))
		"")
	    (if (element-parameter soma 'length)
		(format nil ", length ~,2eum" (element-parameter soma 'length))
		"")
	    (if (element-parameter soma 'SOMA-CYLINDRICAL-G-AXIAL)
		(format nil ", g-a ~,2euS" (element-parameter soma 'SOMA-CYLINDRICAL-G-AXIAL)
			""))))
  (when (soma-segments soma)
    (format t
	    "   ~A Virtual soma segments: g-l ~,2e uS, cap ~,2e nF, area ~,2eum^2~%"
	    (length (soma-segments soma))
	    (SOMA-SEGMENTS-g-leak soma)
	    (SOMA-SEGMENTS-CAP soma)
	    (element-area soma t)))
  (when (element-parameter (element-physical-node soma) 'constant-current)
    (format t "~% ~,2enA constant current injected into this soma."
	    (element-parameter (element-physical-node soma) 'constant-current)))
  (let ((elt-string (PRINT-CELL-ELEMENT-ELEMENTS soma)))
    (when (> (length elt-string) 0) (format t "   This soma has ~A.~%" elt-string))))
	      


  
(defun soma-center-correction (soma)
  (cond ((element-parameter soma 'soma-center-correction)
	 (element-parameter soma 'soma-center-correction))
	;; If a series of circles parallel to the x-axis (as generated by ntscable) define the soma
	;; outline, then calculate the center of mass for the enclosed volume to estimate the soma center.
	((element-parameter soma 'soma-points)
	 (let ((soma-points		; make copy so sort doesn't screw up original
		(loop for soma-circle in (element-parameter soma 'soma-points)
		      collect (copy-list soma-circle))))
	   (setq soma-points (sort soma-points '< :key 'car))
	   (let* ((last-circle (car soma-points))
		  (temp
		   (loop for soma-circle in (cdr soma-points)
			 collect (* (square (* 0.5 (+ (car (last soma-circle)) (car (last last-circle)))))
				    (- (car soma-circle) (car last-circle)))
			 into volumes
			 collect (loop for i from 0 to 2
				       for this-comp in soma-circle
				       for last-comp in last-circle
				       collect (* 0.5 (+ this-comp last-comp)))
			 into cone-coords
			 do (setq last-circle soma-circle)
			 finally (return (list volumes cone-coords)))))
	     (loop for volume in (first temp)
		   for cone-coords in (second temp)
		   summing (* volume (first cone-coords)) into x-total
		   summing (* volume (second cone-coords)) into y-total
		   summing (* volume (third cone-coords)) into z-total
		   summing volume into denominator
		   finally (return (list
				    (/ x-total denominator)
				    (/ y-total denominator)
				    (/ z-total denominator)))))))
	(t '(0.0 0.0 0.0))))


(defun create-cylinder-soma (cell diameter &key length phi theta soma-name soma-parameters location
				  shunt	; in ohms
				  distal-absolute-location cylinder-soma-segment-name)
  (let* ((cell (find-or-create-cell cell))
	 (soma-name (or soma-name (format nil "~a-soma" (cell-name cell))))
	 (location (or location (cell-origin cell)))
	 (length (or length (compute-segment-length location distal-absolute-location)))
	 (total-area (* pi-single diameter length))
	 (soma (create-soma :cell cell :diameter (sqrt (/ (* 0.5 total-area) pi-single))
			    :name soma-name
			    :parameters soma-parameters
			    :location location))
	 (soma-segment
	  (create-segment (or cylinder-soma-segment-name (format nil "~A-extension" soma-name))
			  soma
			  cell
			  :diameter diameter :length length
			  :theta theta :phi (or phi (* -0.5 pi-single))
			  :absolute-location distal-absolute-location
			  :parameter-a-list (list (cons 'membrane-area-coefficient 0.5)))))
    (when shunt (element-parameter soma 'soma-shunt shunt))
    (setq *circuit-processed* nil)
    (initialize-soma-voltage soma)
    (add-soma-segment soma soma-segment t)
    (element-parameter soma 'cylinder-soma-segment soma-segment)
    soma))

(defun create-symmetric-cylinder-soma (cell diameter 
					    &key location-1 location-2 soma-location length name parameters
					    shunt ; ohms
					    (membrane-area-coefficient 1.0) segment-1-name segment-2-name)
  (let* ((cell (find-or-create-cell cell))
	 (name (or name (format nil "~a-soma" (cell-name cell))))
	 (length (or length (compute-segment-length location-1 location-2)))
	 (soma-location (or soma-location
			    (when (and location-1 location-2)
			      (mapcar #'(lambda (x) (/ x 1)) (mapcar '+ location-1 location-2)))
			    (cell-origin cell)))
	 ;; (total-area (* membrane-area-coefficient pi-single diameter length))
	 (segment-area-factor (* membrane-area-coefficient (/ 2 3.0)))
	 (soma-area-factor (* membrane-area-coefficient (/ length (* diameter 3))))
	 (soma (create-soma :cell cell
			    :diameter diameter
			    :name name
			    :parameters parameters
			    :location soma-location))
	 (soma-segment-1 (create-segment (or segment-1-name (format nil "~A-segment-1" name)) soma cell
					 :diameter diameter :length (/ length 2)
					 ; :theta (* -1 pi-single 0.5)
					 :relative-location location-1
					 :parameter-a-list (list (cons 'membrane-area-coefficient segment-area-factor))))
	 (soma-segment-2 (create-segment (or segment-2-name (format nil "~A-segment-2" name)) soma cell
					 :diameter diameter :length (/ length 2)
					 :relative-location location-2
					 ; :theta (* 1 pi-single 0.5)
					 :parameter-a-list (list (cons 'membrane-area-coefficient segment-area-factor)))))
    (setq *circuit-processed* nil)
    (when shunt (element-parameter soma 'soma-shunt shunt))
    (initialize-soma-voltage soma)
    (element-parameter soma 'membrane-area-coefficient soma-area-factor)
    (add-soma-segment soma soma-segment-1)
    (add-soma-segment soma soma-segment-2)
    (element-parameter soma 'cylinder-soma-segments
			   (list soma-segment-1 soma-segment-2))
    (element-parameter soma 'cylinder-soma-segment-1 soma-segment-1)
    (element-parameter soma 'cylinder-soma-segment-2 soma-segment-2)
    soma))


(defun find-soma-name (soma-name cell enable-automatic-cell-names automatic-name-fixing)
  (let ((candidate-soma-name
	 (if *use-simple-names*
	     (get-cell-element-simple-name)
	     (if soma-name
		 (if (numberp soma-name)
		     (if (gethash soma-name (segment-hash-table)) (format nil "~A-soma" soma-name) soma-name)
		     (let ((candidate (format nil "~A" soma-name)))
		       (if (gethash candidate (segment-hash-table)) (format nil "~A-soma" soma-name) candidate)))
		 (format nil "~a-soma" (or (element-name cell 'cell) "cell"))))))
    (if enable-automatic-cell-names
	(check-element-name candidate-soma-name 'soma :automatic-name-fixing automatic-name-fixing)
	candidate-soma-name)))





(defun create-soma (&key cell (diameter *default-soma-diameter*)
			 name
			 adjust-area-for-trunks
			 parameters
			 (enable-automatic-cell-names *enable-automatic-cell-names*)
			 (automatic-name-fixing *PROMPT-FOR-ALTERNATE-ELEMENT-NAMES*)
			 (location '(0.0 0.0 0.0)) length soma-cylinder-diameter
			 shunt)
  "DIAMETER is in microns. PARAMETERS is an a-list. The CELL argument is either a cell structure
or a string - if not supplied, a cell is created. SHUNT [ohms, default NIL], when non-NIL, is a
non-specific somatic shunt. LOCATION gives the xyz coordinates of the SOMA in microns. When
ADJUST-AREA-FOR-TRUNKS is T [default nil], then the soma area [as returned by the ELEMENT-AREA and
ELEMENT-AREA-CM2 functions] is adjusted for the areas of the faces of any abutting segments."
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let* ((actual-soma-name (find-soma-name name cell enable-automatic-cell-names automatic-name-fixing))
	 (cell (find-or-create-cell (or cell (if name (format nil "~a-cell" name) "soma-cell"))))
	 soma)
    (if (setq soma (gethash actual-soma-name (SOMA-HASH-TABLE)))
	(sim-warning (format nil "CREATE-SOMA: soma ~a already defined, ignoring" actual-soma-name))
	(let ((node (create-node actual-soma-name :cell cell :is-physical-cell-node t))
	      ; (model (gethash "soma" *model-hash-table*))
	      )
	  (setq soma (make-soma :name actual-soma-name :node node :diameter (s-flt diameter) :parameters parameters))
	  (setq *circuit-processed* nil)
	  (setf (cell-soma cell) soma
		(node-elements node) (cons soma (node-elements node))
		(gethash actual-soma-name (SOMA-HASH-TABLE)) soma)
	  (when shunt (element-parameter soma 'soma-shunt shunt))
	  (initialize-soma-voltage soma)
	  (element-parameter soma 'center-correction (soma-center-correction soma))
	  (setf (node-absolute-location node) (or location (cell-origin cell)))
	  (setf (node-relative-location node) (or location (cell-origin cell)))
	  (element-parameter soma 'adjust-area-for-trunks adjust-area-for-trunks)
	  (when length
	    (element-parameter soma 'soma-cylinder t)
	    (element-parameter soma 'length (coerce length 'single-float))
	    (element-parameter soma 'soma-cylinder-diameter (coerce (or soma-cylinder-diameter diameter) 'single-float))
	    (element-parameter soma 'soma-cylindrical-g-axial (g-axial
							       (element-parameter soma 'length)
							       (element-parameter soma 'soma-cylinder-diameter)
							       (cell-type-cytoplasmic-resistivity (cell-type cell)))))))
    (setq *soma* soma)))
 



(defun edit-soma (soma)
  (menu-for-somas soma))

(defun menu-for-somas (&optional soma-arg)
  (let (check-conc-ints)
    (loop for soma in
	  (coerce-to-list (element (or soma-arg
				       (select-hash-values-menu (SOMA-HASH-TABLE) "Select Somas" :punt-if-only-one-entry t))
				   'soma))
	  do
	  (let ((cell-type-name (cell-type-name (cell-type (soma-cell soma))))
		(dummy1 (soma-diameter soma))
		(dummy2 (soma-include-shunt soma))
		(dummy3 (cell-type-soma-shunt (cell-type (soma-cell soma))))
		(dummy4 (or (element-parameter soma 'nucleus-diameter) 0.0))
		(dummy5 (element-parameter soma 'adjust-area-for-trunks)))
	    (choose-variable-values
	     `((dummy1 "Soma diameter [uM]" :float)
	       (dummy4 "Soma nucleus diameter [uM] (used for concentrations)" :float)
	       (dummy5 "Adjust surface area for proximal trunks" :boolean)
	       (dummy2 "Include soma shunt" :boolean)
	       ,(when dummy2 `(dummy3 ,(format nil "~A Cell type soma membrane shunt [ohms]" cell-type-name) :float)))
	     :label (format nil "Setting up parameters of soma ~A" (soma-name soma)))
	    (when (and (not (soma-include-shunt soma)) dummy2)
	      (choose-variable-values
	       `((dummy3 ,(format nil "~A Cell type soma membrane shunt [ohms]" cell-type-name) :float))))
	    (unless (= (or (element-parameter soma 'nucleus-diameter) 0) dummy4)
	      (setq check-conc-ints t))
	    (element-parameter soma 'adjust-area-for-trunks dummy5) 
	    (element-parameter soma 'nucleus-diameter (when (> dummy4 0) dummy4)) 
	    (setf (cell-type-soma-shunt (cell-type (soma-cell soma))) dummy3)
	    (setf (soma-diameter soma) dummy1)
	    (setf (soma-include-shunt soma) dummy2)))
    (set-somas-membrane-parameters)
    (when check-conc-ints (set-conc-integrators-parameters))
    (UPDATE-LINEAR-Z-IN-CELLS)))

(defun set-soma-voltage (soma voltage)
  (set-node-voltage (soma-node soma) voltage))

(defun set-soma-voltage-df (soma voltage)
  (set-node-voltage-double (soma-node soma) voltage))

(defun initialize-soma-voltage (soma &optional v-leak)
  (let ((v-leak (or v-leak (cell-type-soma-v-leak (cell-type (soma-cell soma))))))
    (set-soma-voltage soma (or (element-holding-potential soma) v-leak))))


(defun set-somas-membrane-parameters (&optional ignore-membrane-elements cell-type)
  (loop for soma being the hash-value of (soma-hash-table)
	when (or (not cell-type) (eq cell-type (cell-type (soma-cell soma))))
	do (set-soma-membrane-parameters soma ignore-membrane-elements)))

(defun set-soma-absolute-parameters (soma capacitance g-leak)
  "Set linear membrane properties of SOMA to the absolute values of CAPACITANCE [nF] and G-LEAK
[uS]. Sets :INHERIT-PARAMETERS-FROM-TYPE of SOMA to NIL. If any of the soma parameter arguments are
NIL, then the original value is retained."
  (loop for soma in (coerce-to-list (element soma 'soma)) do
	(cond-every (capacitance (setf (soma-capacitance soma) (s-flt capacitance)))
		    (g-leak (setf (soma-g-leak soma) (s-flt g-leak))))
	(setf (soma-inherit-parameters-from-type soma) nil)
	;; Adjust local membrane parameters to reflect absolute value assignment.
	(element-parameter soma 'membrane-resistivity (s-flt (* 1e6 (/ (element-area-cm2 soma) (soma-g-leak soma)))))
	;; (* 1.0e6 pi-single 1.0e-4 0.25) = 78.53982
	(element-parameter soma 'specific-capacitance (s-flt (* 1e-3 (/ (soma-capacitance soma)
									(element-area-cm2 soma)))))))

(defun set-soma-parameter (soma parameter value)
  "Set a PARAMETER distinct from the associated cell type for somas associated with SOMA, for
example 'MEMBRANE-RESISTIVITY, 'SPECIFIC-CAPACITANCE, or 'V-LEAK. Sets :INHERIT-PARAMETERS-FROM-TYPE
for SOMA to NIL."
  (loop for soma in (coerce-to-list (element soma 'soma)) do
	(setf (soma-inherit-parameters-from-type soma) nil)
	(element-parameter soma parameter value)
	(set-soma-membrane-parameters soma)))


;;; SET-SOMA-MEMBRANE-PARAMETERS Set soma membrane properties, according to the membrane parameters
;;; for the appropriate cell type.
(defun set-soma-membrane-parameters (soma &optional ignore-membrane-elements)
  ;;area is in cm-sq, resistivity is in ohm-cm-sq, conductance is in microS.
  (let* ((cell-type (cell-type (soma-cell soma)))
	 ;;	 (diameter (soma-diameter soma))
	 (length (element-parameter soma 'length))
	 (cylinder-diameter (element-parameter soma 'soma-cylinder-diameter))
	 (soma-cylinder (element-parameter soma 'soma-cylinder))
	 (area (element-area-cm2 soma))) ;cm squared
    (when (soma-inherit-parameters-from-type soma)
      (setf ; (soma-v-leak soma) (cell-type-soma-v-leak cell-type)
	    (soma-g-leak soma) (* 1d6 (/ area (cell-type-soma-resistivity cell-type)))
	    (soma-g-shunt soma) (/ 1e6 (or (element-parameter soma 'soma-shunt)
					   (cell-type-soma-shunt cell-type)))
	    ;;specific capacitance is in microF/cm-sq, capacitance is in nF
	    (soma-capacitance soma) (* 1d3 area (cell-type-soma-specific-capacitance cell-type))))
    (when (and soma-cylinder length cylinder-diameter)
      (element-parameter soma 'soma-cylindrical-g-axial
			 (g-axial length
				  cylinder-diameter
				  (cell-type-cytoplasmic-resistivity (cell-type (soma-cell soma))))))
    (set-g-leak*v-leaks soma)
    (unless ignore-membrane-elements
      (loop for elt in (node-elements (soma-node soma)) unless (eq elt soma)
	    do (set-element-membrane-parameters elt t)))))


(defun get-soma-voltage (soma)
  (coerce (node-voltage-n (soma-node soma)) 'single-float))

(defun soma-voltage (soma)
  (let ((soma (element soma 'soma)))
    (when soma (get-soma-voltage soma))))

(proclaim '(inline get-soma-voltage-double))
(defun get-soma-voltage-double (soma)
  (the df (node-voltage-n (soma-node soma))))

(proclaim '(inline eval-soma))
(defun eval-soma (soma)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type soma soma))
  (let* ((alpha-cap-double (the df (* (the df (*2/delta-t[n]*)) (soma-capacitance soma))))
	 (node (soma-node soma))
	 (node-double-floats (node-double-floats node)))
    (declare (double-float alpha-cap-double))
    (when *debug-segments*
      (format t " Soma: *2/delta-t[n]* = ~f, cap= ~f, alpha-cap=~f,V-n=~f~%"
	      (the df (*2/delta-t[n]*)) (soma-capacitance soma) alpha-cap-double     
	      (node-voltage-n node))         
      (format t " Soma ~A: alpha-charge = [alpha-cap * V-n] + alpha-charge ~%" (soma-name soma))
      (format t "  (~f +  ~f) => "
	      (the df (* (node-voltage-n node) alpha-cap-double))
	      (node-alpha-charge node)))
    (setf
     (node-aref-alpha-charge node-double-floats)
     (the df (+ (node-aref-alpha-charge node-double-floats)
		(the df (* alpha-cap-double (node-aref-voltage-n node-double-floats)))))

     (node-aref-jacobian node-double-floats)
     (the df (+ (node-aref-jacobian node-double-floats) alpha-cap-double)))
    
    (when *debug-segments* (format t "      Result: ~f~%" (node-alpha-charge node)))

    nil))

(defun eval-all-somas ()
  (declare (optimize (safety 0) (speed 3) (space 0) (compilation-speed 0)))
  (do ((i 0 (1+ (the fn i))))
      ((= (the fn i) (the fn *soma-array-length*)))
    (eval-soma (aref (the (simple-array soma (*)) *soma-array*) i))))


(defun make-soma-array ()
  (loop for soma being the hash-value of (soma-hash-table)
	unless (node-has-ideal-voltage-source (soma-node soma))
	collect soma into somas
	finally (setf *soma-array* (list-to-array-generic somas)
		      *soma-array-length* (length somas))))

(defun init-somas ()
    (loop for soma being the hash-value of (soma-hash-table) do
	  (set-g-leak*v-leaks soma)))


(defun set-g-leak*v-leaks (soma)
  (let ((g-linear-membrane (+ (if (soma-include-shunt soma) (soma-g-shunt soma) 0.0) (soma-g-leak soma))))
    (setf (soma-g-leak*v-leak soma) (coerce (* g-linear-membrane (soma-v-leak soma)) 'double-float))))
    

;; Calculate the time invariant component of the node jacobians, which consist of segment g-axial
;; terms, and soma and segment g-leak terms.
(defun initialize-soma-node-jacobians-and-currents ()
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (loop for soma being the hash-value of (soma-hash-table) do
	(unless (node-has-ideal-voltage-source (soma-node soma))
	  (deccumulate-setf (node-const-current (soma-node soma)) (soma-g-leak*v-leak soma))
	  (accumulate-setf (node-const-jacobian (soma-node soma))
			   (the sf (+ (if (soma-include-shunt soma) (soma-g-shunt soma) 0.0) (soma-g-leak soma)))))))
			 
(defun get-soma-dendrite-current (soma)
  (let* ((node (soma-node soma))
	 (voltage (node-voltage-n+1 node)))
    (coerce (loop for elt in (node-elements node) when (when (segment-p elt) (eq node (segment-node-1 elt)))  
		  summing (* (segment-g-axial elt) (- voltage (node-voltage-n+1 (segment-node-2 elt)))))
	    'single-float)))



(defun add-channel-to-soma (type &optional (soma *soma*))
  (add-channel-to-locations (or (soma-segments) soma) type))

(defun soma-segment-attached-to-virtual-soma (soma)
  (if (soma-segments soma)
      (loop for seg in (soma-segments soma)
	    when (eq (soma-node soma)
		     (segment-node-1 seg))
	    do (return seg))
      soma))