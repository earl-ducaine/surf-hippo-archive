;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-
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


;;; SYS Source file: trees.lisp
	



(in-package "SURF-HIPPO")


;; Functions for creating cell dendrite geometries and for consolidating trees.
(defun unless-number-then-string (thing)
  (typecase thing
    (number thing)
    (t (format nil "~A" thing))))
    
(defun create-tree (cell segment-list
			 &key (xy-factor 1.0) (z-factor 1.0) global-extras-list ; conserve-seg-names
			 (add-cell-name-to-segs *add-cell-name-to-segs* add-cell-name-to-segs-supplied-p)
			 (default-soma-diameter *default-soma-diameter*) (default-diameter 0.5))
  "Creates a segment tree according to SEGMENT-LIST, adding the tree to the soma associated with CELL. The
associated cell is returned. SEGMENT-LIST is a list of lists, where the sublist format is as follows:

 (prox-elt-name seg-name x y z &optional diameter extras)

The PROX-ELT-NAME refers to the proximal segment or soma, the SEG-NAME is for the segment to be created,
and X, Y, and Z refer to the coordinates of the distal node of the segment to be created. EXTRAS is a
list of lists for adding channels or synapses to a segment. If GLOBAL-EXTRAS-LIST is supplied, then this
list is used in addition to any extras specific to a given segment. XY-FACTOR and Z-FACTOR are scaling
factors for node coordinates, which may be useful when translating histological renderings into the
sublists. The PROX-ELT-NAME of the first sublist will refer to the soma, which has been created already
with CREATE-SOMA. For example, the segment sublist:

  (soma 1a 7 -1 -5 1.2)

specifies a segment named 1a whose proximal end connects to the node named soma, whose distal node has
coordinates (7*xy-factor, -1*xy-factor, -5*z-factor), and whose diameter is 1.2 microns. Likewise, the
segment sublist:

  (1a 1b 12 -3 -7 0.6 '(KA-HPC))

specifies a segment named 1b whose proximal end connects to the distal node of segment 1a, whose distal
node has coordinates (12*xy-factor, -3*xy-factor, -7*z-factor), and whose diameter is 0.6 microns. In
addition, an KA-HPC type channel is included at the segment's distal node. If the PROX-ELT-NAME and
SEG-NAME are the same, then this is the soma, and the diameter is the soma diameter (which overrides the
previous diameter). This entry will be used to reference the coordinates of the segments, so that they
are created in relative coordinates. The soma origin is set elsewhere if it is to be other than (0 0 0).

If the ADD-CELL-NAME-TO-SEGS keyword is T and the global *USE-SIMPLE-NAMES* is nil, then the cell name is
prepended to the segment names specified in the segment sublists."
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let* ((*add-cell-name-to-segs*
	  (unless *USE-SIMPLE-NAMES* (if add-cell-name-to-segs-supplied-p add-cell-name-to-segs *add-cell-name-to-segs*)))
	 (segment-list (no-nils segment-list))
	 (cell (element-cell cell))
	 (soma (cell-soma cell))
	 (cell-name (element-name cell))
	 (soma-name (element-name soma))
	 (x-reference 0.0) (y-reference 0.0)
	 (segment-list-w/o-soma
	  (loop for segment in segment-list ;look for soma entry for xy reference, soma diameter
		when (equal (nth 0 segment) (nth 1 segment))
		do (setf x-reference (s-flt (nth 2 segment))
			 y-reference (s-flt (nth 3 segment))
			 (soma-diameter soma) (if (nth 5 segment) (s-flt (nth 5 segment)) (the sf default-soma-diameter)))
		else collect segment)))
    (declare (single-float x-reference y-reference))
    (setq default-diameter (s-flt default-diameter)
	  xy-factor (s-flt xy-factor)
	  z-factor (s-flt z-factor))
    (let* ((USE-SIMPLE-NAMES-global-set *USE-SIMPLE-NAMES*) ; Use simple names only after tree is constructed.
	   (*USE-SIMPLE-NAMES* nil)
	   (segments
	    (loop for segment-data in segment-list-w/o-soma collect
		  (let* ((proximal-node-ref (unless-number-then-string
					     (if (eq 'soma (nth 0 segment-data)) soma-name (nth 0 segment-data))))
			 (distal-node-ref (unless-number-then-string (nth 1 segment-data)))
			 (node2-x (- (s-flt (nth 2 segment-data)) x-reference))
			 (node2-y (- (s-flt (nth 3 segment-data)) y-reference))
			 (node2-z (s-flt (nth 4 segment-data)))
			 (diameter (if (nth 5 segment-data) (s-flt (nth 5 segment-data)) (the sf default-diameter)))
			 (seg (create-segment distal-node-ref proximal-node-ref cell-name
					      :diameter diameter 
					      :relative-location (list (* (the sf xy-factor) (the sf node2-x))
								       (* (the sf xy-factor) (the sf node2-y))
								       (* (the sf z-factor) (the sf node2-z))))))
		    (when global-extras-list
		      (let (  (*use-simple-names* USE-SIMPLE-NAMES-global-set)
			    )
			(create-element seg global-extras-list)))
		    (loop for extra in (nth 6 segment-data) when (consp extra)
			  do (case (car extra)
			       ((cytoplasmic-resistivity-coeff ra-coeff ri-coeff)
				(element-parameter seg 'ri-coeff (cadr extra)))
			       (soma-segment (add-soma-segment soma seg)
					     (element-parameter seg 'soma-segment t))
			       (axon-segment (element-parameter seg 'axon-segment t)))
			  else do (when extra (let ( ; (*use-simple-names* USE-SIMPLE-NAMES-global-set)
						    )
						(create-element seg extra))))
		    seg))))
      (destroy-zero-length-segments segments)
      (when USE-SIMPLE-NAMES-global-set (rename-segments-simple segments))
      )
    cell))



(defun segment-chain (proximal-cell-element chain-name total-segs seg-length seg-diam
					    &key (proximal-phi 0.0) (proximal-theta 0.0))

  "Adds a straight chain of segments of the same dimensions to the PROXIMAL-CELL-ELEMENT, returning
the last (distal) segment in the created chain. If CHAIN-NAME is nil, then the segment names are
derived from the cell-name. The PROXIMAL-PHI and PROXIMAL-THETA arguments specify the angle of the
branch chain with respect to the PROXIMAL-CELL-ELEMENT, in radians. The default values of 0.0 for
PROXIMAL-PHI and PROXIMAL-THETA generate a chain that extends from the PROXIMAL-CELL-ELEMENT in the
positive X direction. For a chain of segments that extends in the positive Y direction, include the
key argument:

           :PROXIMAL-THETA (* -0.5 pi)

The orientations of the PROXIMAL-PHI and PROXIMAL-THETA arguments are as follows:


                                         Y
                                 ^      |
          Proximal theta = pi/2  |      |
          Proximal phi = 0.0     |      |     /
                                 |      |    /
                                 |      |   /
                                        |  /
                                        | /
                                        |/
                           -------------/------------X
                                       /|
                                      / |  ------> Proximal Theta 
                                     /  |          and Phi = 0.0
                                    /   |
                                 Z /    |
                                  /     |
                                        |

"
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (when (and chain-name (not (stringp chain-name))) (setq chain-name (format nil "~a" chain-name)))
  (let* ((proximal-cell-element (element-cell-element proximal-cell-element))
	 (*add-cell-name-to-segs* nil)
	 (total-segs (round total-segs))
	 (seg-length (s-flt seg-length))
	 (seg-diam (s-flt seg-diam))
	 (proximal-phi (s-flt proximal-phi))
	 (proximal-theta (s-flt proximal-theta))
	 (cell (element-cell proximal-cell-element))
	 (cell-name (typecase (cell-name cell)
		      (string (cell-name cell))
		      (t (format nil "~A" (cell-name cell)))))
	 (base-seg-name
	  (unless *use-simple-names*
	    (concatenate-strings (the simple-base-string cell-name) "-"
				 (when chain-name
				   (concatenate-strings (the simple-base-string chain-name) "-")))))
	 seg)
    (declare (fixnum total-segs)
	     (single-float seg-length seg-diam proximal-phi proximal-theta))
    (do ((node-number 0 (1+ (the fn node-number))))
	((= node-number (the fn total-segs)) seg)
      (let* ((proximal-node (if (= node-number 0)
				(element-node proximal-cell-element)
				(segment-node-2 seg)))
	     (next-node-name
	      (unless *use-simple-names*
		(concatenate-strings
		 (the simple-base-string base-seg-name)
		 (the simple-base-string (princ-to-string (the fn (1+ node-number))))))))
	(declare (fixnum node-number))
	(setq seg (create-segment-fast next-node-name proximal-node cell))
	(setf (segment-theta seg) (if (= node-number 0) proximal-theta 0.0)
	      (segment-phi seg) (if (= node-number 0) proximal-phi 0.0)
	      (segment-length seg) seg-length
	      (segment-diameter seg) seg-diam)))))



;; CREATE-SEGMENT-CHAIN 0ld. Also adds instances of the synapse types in the list SYNAPSE-TYPES to
;; each segment. If SYNAPSE-SEGS is non-nil (a list of integers) then synapses are added only for
;; the node numbers in the list.
(defun create-segment-chain (proximal-cell-element chain-name total-segs seg-length seg-diam
						   &key (proximal-phi 0.0) (proximal-theta 0.0)
						   synapse-types synapse-segs)
  (let* ((original-segs (segments))
	 (new-distal-seg
	  (segment-chain proximal-cell-element chain-name total-segs seg-length seg-diam
			 :proximal-phi proximal-phi
			 :proximal-theta proximal-theta)))
    (add-synapse-type-to-numbered-synapse-segs (set-exclusive-or original-segs (segments)) synapse-types synapse-segs)
    new-distal-seg))


(defun add-synapse-type-to-numbered-synapse-segs (segments synapse-types synapse-segs)
  (loop for type in synapse-types do
	(loop for seg-count from 1
	      for seg in segments
	      when (or (not synapse-segs) (member seg-count synapse-segs))
	      do (create-synapse seg type))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; BACKWARD COMPATIBILITY
(defun make-segment-chain (proximal-cell-element chain-name total-segs seg-length seg-diam
						 &key (proximal-phi 0.0) (proximal-theta 0.0)
						 synapse-types synapse-segs)
  (create-segment-chain proximal-cell-element chain-name total-segs seg-length seg-diam
			:proximal-phi proximal-phi
			:proximal-theta proximal-theta
			:synapse-types synapse-types
			:synapse-segs synapse-segs))
			
(defun MAKE-SOMA-SEGMENT-CHAIN-FAST (soma chain-name total-segs seg-length seg-diam synapse-types synapse-segs
				     &key (proximal-phi 0.0)
				     (proximal-theta 0.0))
  (make-segment-chain soma chain-name total-segs seg-length seg-diam
		      :synapse-types synapse-types :synapse-segs synapse-segs
		      :proximal-phi proximal-phi
		      :proximal-theta proximal-theta))

(defun make-soma-segment-chain (soma chain-name total-segs seg-length seg-diam synapse-types synapse-segs
				     &key (proximal-phi 0.0)
				     (proximal-theta 0.0))
  (make-segment-chain soma chain-name total-segs seg-length seg-diam
		      :synapse-types synapse-types :synapse-segs synapse-segs
		      :proximal-phi proximal-phi
		      :proximal-theta proximal-theta))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;




(defun tree-control (parent-element depth branch-depth &key
				    (length 50.0)
				    (diameter 1.0) (branch-diameter-decrement 0.8)
				    (branch-angle-decrement 0.02))
  "Generates a binary tree structure recursively with number of bifurcations equal to DEPTH. Segments
are named by their distal node. Each branch of the tree has BRANCH-DEPTH segments, each of LENGTH
[microns]. The diameter of a segment at a given branch level is given by BRANCH-DIAMETER-DECREMENT
times the diameter of the previous level (given the DIAMETER [microns] of the first level branch).
The bifurcation angle at a given branch level is given by:

          2 x PI x BRANCH-ANGLE-DECREMENT x DEPTH

where DEPTH is the number of bifurcations distal to the present one.
"
  (tree-control-internal parent-element depth branch-depth 
			 :length length
;			 :direction direction
			 :diameter diameter
			 :branch-diameter-decrement branch-diameter-decrement
			 :branch-angle-decrement branch-angle-decrement))
  

(defun tree-control-internal (parent-element depth branch-depth &key
					     (diameter 1.0) (branch-diameter-decrement 0.8)
					     (length 50.0) direction (branch-angle-decrement 0.02))
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let* ((cell-name (cell-name (element-cell parent-element)))
	 (branch-diameter-decrement (s-flt branch-diameter-decrement))
	 (diameter (s-flt diameter))
	 (branch-angle-decrement (s-flt branch-angle-decrement))
	 (length (s-flt length))
	 (branch-depth (round branch-depth))
	 (depth (round depth))
	 (leftright (case direction (left "a") (right "b") (t "")))
	 last-element (theta 0.0))
    (declare (fixnum branch-depth depth)
	     (single-float theta branch-diameter-decrement diameter branch-angle-decrement length))
    (unless (= depth 0)
      (do ((branch-number 0 (1+ branch-number)))
	  ((= branch-number branch-depth))
	(declare (fixnum branch-number))
	(let* ((parent-node-name (element-name (element-cell-element parent-element)))
	       (node1
		(if (= branch-number 0)
		    parent-element
		    *segment*	
	;   (if *use-simple-names*
	;       *segment*	
	;	(concatenate-strings parent-node-name "-" leftright (princ-to-string branch-number) "-seg"))
		    ))
	       (node2
		(if *use-simple-names*
		    (gensym)		
		    (concatenate-strings parent-node-name "-" leftright
					 (princ-to-string (the fn (1+ branch-number))) "-seg"))))
	  (cond ((and (= branch-number 0) (eq direction 'left))
		 (setq theta (* pi-single (- branch-angle-decrement) depth)))
		((and (= branch-number 0) (eq direction 'right))
		 (setq theta (* pi-single branch-angle-decrement depth)))
		(t (setq theta 0.0)))
	  (setq last-element (create-segment-fast
			      node2 node1 cell-name
			      :length length :diameter diameter :theta theta :phi 0.0))))
      (tree-control-internal last-element (the fn (1- depth)) branch-depth
			     :length length
			     :direction 'left
			     :diameter (the sf (* diameter branch-diameter-decrement))
			     :branch-angle-decrement branch-angle-decrement
			     :branch-diameter-decrement branch-diameter-decrement)
      (tree-control-internal last-element (the fn (1- depth)) branch-depth
			     :length length
			     :direction 'right 
			     :diameter (the sf (* diameter branch-diameter-decrement))
			     :branch-angle-decrement branch-angle-decrement
			     :branch-diameter-decrement branch-diameter-decrement)
      nil)))

;; Backward compatibility
(defun old-tree-control (cell-name parent-element depth direction branch-depth distance-from-soma &key (length 50.0)
				   (diameter 1.0) (branch-diameter-decrement 0.8)
				   (branch-angle-decrement 0.02))
;  "Generates a binary tree structure recursively with number of bifurcations equal to DEPTH. Segments
; are named by their distal node. Each branch of the tree has BRANCH-DEPTH segments."
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let* ((cell (element-cell parent-element))
	 (branch-diameter-decrement (s-flt branch-diameter-decrement))
	 (diameter (s-flt diameter))
	 (branch-angle-decrement (s-flt branch-angle-decrement))
	 (length (s-flt length))
	 (branch-depth (round branch-depth))
	 (depth (round depth))
	 (distance-from-soma (round distance-from-soma))
	 (leftright
	  (case direction
	    (left "a")
	    (right "b")
	    (t "")))
	 (SEGMENT-HASH-TABLE (SEGMENT-HASH-TABLE))
	 last-node-name (theta 0.0))
    (declare
     (fixnum branch-depth depth distance-from-soma)
     (single-float theta
		   branch-diameter-decrement diameter branch-angle-decrement length))
    (unless (= depth 0)
      (do ((branch-number 0 (1+ branch-number)))
	  ((= branch-number branch-depth))
	(declare (fixnum branch-number))
	(let* ((parent-node-name (element-name (element-cell-element parent-element)))
	       (node1-name
		(if (= branch-number 0)
		    parent-node-name
		  (if *use-simple-names*
		      (hash-table-count SEGMENT-HASH-TABLE)
		    (concatenate-strings parent-node-name "-" leftright (princ-to-string branch-number) "-seg"))))
	       (node2-name
		(if *use-simple-names*
		    (+ 1 (hash-table-count SEGMENT-HASH-TABLE))
		  (concatenate-strings parent-node-name "-" leftright
				       (princ-to-string (the fn (1+ branch-number))) "-seg"))))
	  (cond ((and (= branch-number 0) (eq direction 'left))
		 (setq theta (* pi-single (- branch-angle-decrement) depth)))
		((and (= branch-number 0) (eq direction 'right))
		 (setq theta (* pi-single branch-angle-decrement depth)))
		(t (setq theta 0.0)))
	  (create-segment-fast node2-name node1-name cell
			       :length length :diameter diameter :theta theta :phi 0.0 )
	  (setq last-node-name node2-name)))
      (old-tree-control cell-name last-node-name (the fn (1- depth)) 'left branch-depth
			(the fn (+ distance-from-soma branch-depth))
			:diameter (the sf (* diameter branch-diameter-decrement))
			:branch-angle-decrement branch-angle-decrement
			:branch-diameter-decrement branch-diameter-decrement)
      (old-tree-control cell-name last-node-name (the fn (1- depth)) 'right branch-depth
			(the fn (+ distance-from-soma branch-depth))
			:diameter (the sf (* diameter branch-diameter-decrement))
			:branch-angle-decrement branch-angle-decrement
			:branch-diameter-decrement branch-diameter-decrement)
      nil)))


;;; GROW-SPINES If SEGMENTS is a list of segments, then a MEMBRANE-AREA-COEFFICIENT entry is added to
;;; the :SEGMENT-PARAMETERS a-list of each segment, otherwise if CELL-TYPE is supplied, the
;;; :DENDRITE-SPECIFIC-CAPACITANCE and :MEMBRANE-RESISTIVITY slots of the referred cell type are
;;; adjusted, otherwise (if both keyword args are NIL) the :DENDRITE-SPECIFIC-CAPACITANCE and
;;; :MEMBRANE-RESISTIVITY slots of all the cell types are adjusted.  NECK-LENGTH, NECK-DIAMETER,
;;; HEAD-DIAMETER are in microns. DENSITY is number of spines per square micron of dendrite
;;; (non-spine) membrane. The spine model here is of a cylindrical neck capped by a spherical head.
;;; Spine area is given by the area of the neck (not including ends) plus the area of the head minus
;;; the area of the neck end (to partially compensate for the junction between the head and neck).

(defun grow-spines (neck-length neck-diameter head-diameter density &key cell-type segments (update-linear-parameters t))
  (let* ((head-diameter (max head-diameter neck-diameter))
	 (spine-area			; square microns
	  (+ (* neck-length neck-diameter pi-single)
	     (- (* pi-single head-diameter head-diameter)
		(* pi-single neck-diameter))))
	 (value (+ 1.0 (* spine-area density))))
    (if segments
	(loop for seg in segments do (element-parameter seg 'MEMBRANE-AREA-COEFFICIENT value))
	(loop for type in (or (and cell-type (list (element cell-type))) (cell-types))
	      do
	      (setf (cell-type-DENDRITE-SPECIFIC-CAPACITANCE type) (* (cell-type-DENDRITE-SPECIFIC-CAPACITANCE type) value)
		    (cell-type-MEMBRANE-RESISTIVITY type) (/ (cell-type-MEMBRANE-RESISTIVITY type) value)))))
  (when update-linear-parameters
    (set-segments-membrane-parameters)
    (loop for cell in (or (and cell-type (cell-type-cells (element cell-type))) (cells))
	  do (update-linear-z-in cell))))

    

;; SET-PROXIMAL-THETAS Fans out segments working outward from SEG by distal recursion to the end of
;; the associated branchs, such that the total fan angle at each branch point is equal to
;; TOTAL-FAN-ANGLE + (/ SPREADMOREDISTAL #segments-to-soma).
(defun set-proximal-thetas (seg &optional (total-fan-angle 30.0) (spreadmoredistal 0))
  (let* ((seg (element-cell-element seg))
	 (distal-segs (distal-segments seg)))
    (setq total-fan-angle (+ total-fan-angle (/ spreadmoredistal (length (segments-to-soma seg)))))
    (loop for segment in distal-segs
	  for theta from (if (= (length distal-segs) 1)  0.0 (/ total-fan-angle -2.0))
	  by (if (= (length distal-segs) 1) 0.0 (/ total-fan-angle (1- (length distal-segs))))
	  do (setf (segment-theta segment)  (deg-to-rad theta))
	  (setf (node-relative-location (segment-node-2 segment)) '(0.0 0.0 0.0))
	  (set-proximal-thetas segment total-fan-angle spreadmoredistal))))


(defun get-top-cell ()
  (car (cells)))

(defun branch-dendrite-list (branch &optional count cell)
  (declare (ignore cell))
  (loop for seg in (PROXIMAL-DENDRITE-LIST (GET-PROXIMAL-BRANCH-SEGMENT branch) count nil branch)
	collect (segment-name seg)))

(defun proximal-dendrite-list (&optional cell-or-seg level (count 0) branch)
  (setq count (or count 0))
  (unless branch (setq level (or level 1)))
  ;; Start at the cell soma, and work on each of the branches that originate there in turn. Returns list of dendrite names.
  (let ((cell-or-seg (or cell-or-seg (get-top-cell))))
    (if (if branch
	  (if level (> level count) t)
	  (> level count))
      (typecase cell-or-seg 
	(cell
	 (loop for element in (soma-segments cell-or-seg)
	       nconcing (proximal-dendrite-list element level count branch)))
	(segment
	 (cons (element cell-or-seg)
	       (loop for seg in (distal-segments cell-or-seg)
		     when (or (not branch)
			      (and (search branch (segment-name seg)) (= 0 (search branch (segment-name seg)))))
		     nconcing (proximal-dendrite-list seg level (1+ count) branch))))))))

(defun get-branch-segments (branch)
  (loop for seg being the hash-value of (SEGMENT-HASH-TABLE)
	when (and (search branch (segment-name seg)) (= 0 (search branch (segment-name seg))))
	collect seg))

(defun get-proximal-branch-segment (branch &optional cell seg)
  (let (proximal-branch-segment
	(cell (or (element cell 'cell) (get-top-cell)))
	(seg (element seg 'segment)))
    (if seg
      (or (and (search branch (segment-name seg)) (= 0 (search branch (segment-name seg))) seg)
	  (loop for seg in (distal-segments seg)
		do (setq proximal-branch-segment (get-proximal-branch-segment branch cell seg))
		when proximal-branch-segment
		do (return proximal-branch-segment)))
      (loop for element in (node-elements (soma-node (cell-soma cell)))
	    when (eq (named-structure-symbol element) 'segment)
	    when (and (search branch (segment-name element)) (= 0 (search branch (segment-name element))))
	    do (return element)
	    else do (setq proximal-branch-segment (get-proximal-branch-segment branch cell element))
	    when proximal-branch-segment
	    do (return proximal-branch-segment)))))



;; For circuits with less than 10 cells, prints branch point stats for each cell - otherwise lumps everything together.
(defun print-branch-points (&optional cell)
  (unless *kill-extra-messages*
    (let ((max-#cells-for-discrete-readout 10))
      (format t "~%")
      (loop for cell in (cond (cell (and (element cell 'cell) (list (element cell 'cell))))
			      ((< (hash-table-count (CELL-HASH-TABLE)) max-#cells-for-discrete-readout) (cells))
			      (t '(all)))
	    do
	    (let ((bps (count-branch-points (unless (eq 'all cell) cell))))
	      (if (eq 'all cell)
		(format t " ~A cell~:p, ~A branch point~:p and ~a segment~:p processed.~%"
			(length (cells))
			bps (- (hash-table-count (SEGMENT-HASH-TABLE)) (hash-table-count (electrode-hash-table))))
		(format t " Cell ~A: ~A branch point~:p and ~a segment~:p processed.~%"
			(cell-name cell) bps (length (cell-segments cell))))))
      (when (> (hash-table-count (electrode-hash-table)) 0)
	(format t " ~A electrode~:p processed.~%" (hash-table-count (electrode-hash-table)))))))

(defun count-branch-points (&optional cell)
  (let ((cell (element cell 'cell)))
    (loop for seg in (if cell (cell-segments cell) (segments))
	  when (> (length (distal-segments seg)) 1) sum 1)))

(defun get-branch-points (&optional cell)
  (let ((cell (element cell 'cell)))
    (loop for seg in (if cell (cell-segments cell) (segments))
	  when (> (length (distal-segments seg)) 1)
	  nconcing (loop for seg in (distal-segments seg) collect (segment-name seg)))))	      






(defun plot-lambda-histo (&key bin-width (width 800) (height 400))
  (let* ((lambdas (mapcar 'SEGMENT-ELECTROTONIC-LENGTH
			  (segments (if (= 1 (length (cells)))
				      *cell*
				      (choose-specific-elements (cells) nil :label "Choose Cell to Plot Lambdas")))))
	 (max (max-of-list lambdas))
	 (number-segs (length lambdas))
	 (total-bins (if bin-width (ceiling (/ max bin-width)) 10))
	 (bin-width (or bin-width (ceiling (/ max 10))))
	 (lambda-array (make-array (list total-bins)))
	 (title (format nil "Distribution of Electrotonic Lengths: ~a" *simulation-name*))
	 (win (find-plot-window title :xy)))
    (loop for lambda in lambdas do (incf (aref lambda-array (floor (* (1- total-bins) (/ lambda max))))))

    (plot-histogram (list (loop for x from 0.0 ; (nth 0 min-max)
				by bin-width
				for i from 1 to total-bins
				collect x)
			  (array-to-list lambda-array))
		    bin-width
		    :win (if (and win (go-ahead-menu "Use new histogram window"
						     (format nil "Sending Plot to ~A" (g-value win :title))
						     nil))
			   (get-plot-window :histogram title nil :create-new-plot-windows t)
			   win)
		    :x-min 0 
		    ;; :x-max max
		    :comment (format nil "Total number of segments: ~D" number-segs)
		    :comment-position :upper-right
		    :x-label "Electrotonic Length (Lambda)" :y-label "# Segments"
		    :title title :width width :height height :stipple-percent 50)))


;; *************************************************************************************
;; ****************************** Tree Consolidation Code ******************************
;; *************************************************************************************

(defvar *use-strict-lambda-criterium* t)

(defun pre-processed-segment-zero-length-p (seg)
  (and (= (segment-length seg) 0)
       (equal (or (segment-dummy-proximal-node-location seg) (node-relative-location (segment-node-1 seg)))
	      (node-relative-location (segment-node-2 seg)))))

(defun destroy-zero-length-segment (seg)
  (when (pre-processed-segment-zero-length-p seg)
    (let ((*standard-output* (not (or *KILL-ALL-OUTPUT* *kill-extra-messages*))))
      (format t "Destroying zero length segment ~A~%" (segment-name seg))
      (remove-node-element seg)
      (remove-model-instance seg)
      (transfer-node-elements (segment-node-2 seg) (segment-node-1 seg))
      (loop for distal-seg in (distal-segments seg) do
	    (when (segment-dummy-proximal-node-location seg)
	      (setf (segment-dummy-proximal-node-location distal-seg) (segment-dummy-proximal-node-location seg)))
	    (setf (segment-node-1 distal-seg) (segment-node-1 seg)))
      (remove-entry-from-hash (segment-node-2 seg) (NODE-HASH-TABLE))
      (remove-entry-from-hash seg (SEGMENT-HASH-TABLE)))
    t))

(defun zero-length-segments-p (&optional segments)
  (if segments
    (loop for seg in segments when (pre-processed-segment-zero-length-p seg) do (return t))
    (loop for seg being the hash-value of (SEGMENT-HASH-TABLE)
	  when (pre-processed-segment-zero-length-p seg) do (return t))))
  
(defun destroy-zero-length-segments (&optional segments-list process-circuit)
  (let ((segments nil			; (copy-list segments-list)
		  )
	(destroyed-segments 0))
    (loop while t
	  unless (zero-length-segments-p segments) do (return)
	  do
	  (format t "Checking the length of ~A segments...~%"
		  (if segments (length segments) (hash-table-count (SEGMENT-HASH-TABLE))))
	  (if segments
	    (loop for seg in segments when (destroy-zero-length-segment seg) do (incf destroyed-segments))
	    (loop for seg being the hash-value of (SEGMENT-HASH-TABLE)
		  when (destroy-zero-length-segment seg) do (incf destroyed-segments)))
	  (if segments (setq segments (loop for seg in segments
					    do (format t " looking for ~A in hash...~%"  (segment-name seg))
					    when (gethash (segment-name seg) (SEGMENT-HASH-TABLE))
					    collect seg))))
    (unless (= 0 destroyed-segments) (format t "~A Destroyed segments....~%" destroyed-segments))
    (when process-circuit
      (setq *branch-list* '()
	    *num-nodes* (1- (hash-table-count (NODE-HASH-TABLE)))
	    *num-unknowns* 0)
      (process-circuit-structure t)
      (format t "~%")
      (count-branch-points)
      (maphash 'set-segment-membrane-parameters (SEGMENT-HASH-TABLE)))))


(defun consolidate-cells-tree ()
  (loop for cell in (things-of-names (choose-list-values-from-keys
				      (loop for cell in (cells) when (cell-segments cell) collect (list (cell-name cell) cell))
				      nil :label "Select cells to consolidate"))
	do
	(let ((dummy1 *maximum-electrotonic-length*)
	      dummy2
	      (dummy3 t)
	      (dummy4 0.1)
	      dummy5
	      dummy6
	      (dummy7 10.0))
	  (loop while (or dummy2 dummy3) do
		(setq dummy2 nil dummy3 nil)
		(choose-variable-values
		 '((dummy3 "Plot distribution of electrotonic lengths" :boolean)
		   (dummy4 "Bin width for lambda histogram" :number)
		   (dummy6 "Plot distribution of actual lengths" :boolean)
		   (dummy7 "Bin width for actual length histogram [microns]" :number)
		   (dummy2 "Consolidate cell -> fewer segments" :boolean)
		   (dummy1 "Maximum total lambda for consolidation" :number)
		   (dummy5 "Single step consolidation" :boolean)
		   (*USE-STRICT-LAMBDA-CRITERIUM* :boolean))
		 :label (format nil "Cell ~A: Criteria for consolidating segment pairs" (cell-name cell)))
		(cond-every
		 (dummy3 (plot-lambda-histo :bin-width dummy4))
		 (dummy6 (element-param-distribution 'segment 'length :cell cell :bin-width dummy7))
		 (dummy2
		  (cond ((and *neuron-tree-consolidated* (> *maximum-electrotonic-length* dummy1))
			 (announce-warning (format nil "Trees have already been~%consolidated with L >= ~A~%" *maximum-electrotonic-length*)))
			(t
			 (setq *maximum-electrotonic-length* dummy1)
			 (consolidate-cell-tree :cell cell :single-step dummy5
						:MAXIMUM-ELECTROTONIC-LENGTH *MAXIMUM-ELECTROTONIC-LENGTH*
						:USE-STRICT-LAMBDA-CRITERIUM *USE-STRICT-LAMBDA-CRITERIUM*)))))))))
						

(defun consolidate-cell-tree (&key (cell *cell*) single-step starting-segment
				   (MAXIMUM-ELECTROTONIC-LENGTH *MAXIMUM-ELECTROTONIC-LENGTH*)
				   (USE-STRICT-LAMBDA-CRITERIUM *USE-STRICT-LAMBDA-CRITERIUM*))
  "Consolidate the dendritic tree of CELL, according to the values of MAXIMUM-ELECTROTONIC-LENGTH and
USE-STRICT-LAMBDA-CRITERIUM. If SINGLE-STEP is T, then only one pair of segments are consolidated, otherwise successive segment
pairs are consolidated moving distally and starting from STARTING-SEGMENT, if supplied, otherwise starting from the trunk segments
of the CELL soma. The circuit is processed with PROCESS-CIRCUIT-STRUCTURE at the end of the consolidation."
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (format t "Consolidating cell ~a~%" (cell-name cell))

  (let ((*MAXIMUM-ELECTROTONIC-LENGTH* MAXIMUM-ELECTROTONIC-LENGTH)
	(*USE-STRICT-LAMBDA-CRITERIUM* USE-STRICT-LAMBDA-CRITERIUM))
    ;; Start at the cell soma, and work on each of the branches that originate there in turn.
    (if (element starting-segment 'segment)
	(consolidate-distal-segment (element starting-segment 'segment) single-step)
	(loop for element in (node-elements (soma-node (cell-soma cell)))
	      when (and (eq (named-structure-symbol element) 'segment)
			(consolidate-distal-segment element single-step)
			single-step)
	      do (return)))
    (setf (cell-segments cell) (loop for seg in (segments) when (eq (segment-cell seg) cell) collect seg))
    (setq *branch-list* '()
	  *num-nodes* (1- (hash-table-count (NODE-HASH-TABLE)))
	  *num-unknowns* 0)
    (process-circuit-structure t)
    (format t "~%")
    (setq *neuron-tree-consolidated* t)))


(defvar *segment-consolidation-char* #\^)

;;; "11-22-33" "11-22-34 -> "11-22-33^34"
;;; "11-22-33^34" "11-22-35 -> "11-22-33^35"
;;;                 or
;;; "5942^943" "5941" -> "5942^5941"

#|
(defun create-consolidated-segment-name (proximal-name distal-name)
  (if (not (stringp proximal-name))
    (setq proximal-name (format nil "~A" proximal-name)))
  proximal-name)
|#

(defun create-consolidated-segment-name (proximal-name distal-name &optional simple)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (cond-every
   ((not (stringp proximal-name))
    (setq proximal-name (format nil "~A" proximal-name)))
   ((not (stringp distal-name))
    (setq distal-name (format nil "~A" distal-name))))
  (let ((candidate
	 (concatenate-strings
	  (the SIMPLE-BASE-STRING (string-head proximal-name
					       (or (and (not simple)
							(FIND-TAIL-CHAR proximal-name
									*segment-consolidation-char*))
						   (length (the simple-base-string proximal-name)))))
	  (string *segment-consolidation-char*)
	  (the SIMPLE-BASE-STRING (string-tail distal-name
					       (or
						(and (not simple)
						     (- (length (the simple-base-string distal-name))
							(1+ (FIND-TAIL-CHAR distal-name
									    *segment-consolidation-char*))))
						(length (the simple-base-string distal-name))))))))

    (if (element candidate 'segment)
      (if simple
	(sim-error (format nil "Problem w/CREATE-CONSOLIDATED-SEGMENT-NAME: ~A ~A" proximal-name distal-name))
	(create-consolidated-segment-name proximal-name distal-name t))
      candidate)))



(defun move-node-slightly (amount node)
  (let ((factor (+ 1 amount)))
    (setf (node-relative-location node)
	  (mapcar '* (node-relative-location node)
		  (list factor factor factor)))))
		      
(defun move-distal-seg-slightly-if-coincident (seg distal-seg)
  (when (equal (or (segment-dummy-proximal-node-location seg)
		   (node-relative-location (segment-node-1 seg)))
	       (node-relative-location (segment-node-2 distal-seg)))
    (move-node-slightly 0.01 (segment-node-2 distal-seg))))

(defun consolidate-distal-segment (seg &optional single-step)
					; (declare (optimize (safety 1) (speed 3) (space 1)))
					;  (format t "Consolidate test for segment ~A~%" (segment-name seg))
  (let ((distal-seg-list (distal-segments seg))
	segment-consolidated)
    (when distal-seg-list
      (if (= (the fn (length (the cons distal-seg-list))) 1)
	  (let* ((distal-seg (car distal-seg-list))
		 (L-prox (the sf (SEGMENT-ELECTROTONIC-LENGTH seg)))
		 (L-distal (the sf (SEGMENT-ELECTROTONIC-LENGTH distal-seg)))
		 ;; (distal-name (segment-name distal-seg))
		 (new-length 0.0)
		 (d1l1 0.0) (d1d1 0.0)(d2l2 0.0)(d2d2 0.0)
		 (prox-cyto-coeff (segment-ri-coeff seg))
		 (distal-cyto-coeff (segment-ri-coeff distal-seg))
		 (prox-length (segment-length seg))
		 (distal-length (segment-length distal-seg))
		 ;; (prox-diameter (segment-diameter seg))
		 ;; (distal-diameter (segment-diameter distal-seg))
		 (new-diameter 0.0)
		 (ri-coeff 0.0)
		 (middle-node (segment-node-2 seg))
		 new-name
		 ;; z-distal z-proximal z-prox-w/o-dist
		 )
	    (declare (single-float prox-length
				   prox-cyto-coeff distal-cyto-coeff
				   distal-length
				   ;; prox-diameter distal-diameter
				   new-length d1l1 d1d1 d2l2 d2d2
				   new-diameter ri-coeff l-prox l-distal))

	    (when *use-strict-lambda-criterium*
	      (move-distal-seg-slightly-if-coincident seg distal-seg)
	      (setq new-length		; The new segment length is determined by the endpoints of the original pair.
		    (the sf (compute-segment-length 
			     (or (segment-dummy-proximal-node-location seg) (node-relative-location (segment-node-1 seg)))
			     (node-relative-location (segment-node-2 distal-seg)))))

	      (setq
	       d1l1 (the sf (* (segment-diameter seg) (segment-length seg)))
	       d1d1 (the sf (* (segment-diameter seg) (segment-diameter seg)))
	       d2l2 (the sf (* (segment-diameter distal-seg) (segment-length distal-seg)))
	       d2d2 (the sf (* (segment-diameter distal-seg) (segment-diameter distal-seg)))
	       ;; The new segment diameter is then chosen to conserve membrane area.
	       new-diameter (the sf (/ (the sf (+ d1l1 d2l2)) new-length))
	       ;; We need to adjust the intracellular resistivity to conserve the total g_i.
	       ri-coeff
	       (the sf
		    (/ (the sf
			    (* (the sf (* new-diameter new-diameter))
			       (+ (the sf (/ (the sf (* distal-length distal-cyto-coeff)) d2d2))
				  (the sf (/ (the sf (* prox-length prox-cyto-coeff)) d1d1)))))
		       new-length))))

	    ;; If true, consolidate segment and its distal-segment.
	    (when (< (if *use-strict-lambda-criterium*
			 (the sf (electrotonic-length new-length new-diameter
						      (cell-type-cytoplasmic-resistivity (cell-type (segment-cell seg)))
						      ri-coeff
						      (cell-type-membrane-resistivity (cell-type (segment-cell seg)))))
			 (the sf (+ l-prox L-distal)))
		     (the sf *maximum-electrotonic-length*))

	      (setq segment-consolidated t)
	      (setq new-name (create-consolidated-segment-name (segment-name seg) (segment-name distal-seg)))
	      (when (or single-step *announce-consing-segments*)
		(format t "Consing ~a, ~a into ~A~%" (segment-name seg) (segment-name distal-seg) new-name))
		    
	      (unless *use-strict-lambda-criterium*
		(move-distal-seg-slightly-if-coincident seg distal-seg)
		
			 
			  
		(setq new-length	; The new segment length is determined by the endpoints of the original pair.
		      (the sf (compute-segment-length 
			       (or (segment-dummy-proximal-node-location seg)
				   (node-relative-location (segment-node-1 seg)))
			       (node-relative-location (segment-node-2 distal-seg))))
		      d1l1 (the sf (* (segment-diameter seg) (segment-length seg)))
		      d1d1 (the sf (* (segment-diameter seg) (segment-diameter seg)))
		      d2l2 (the sf (* (segment-diameter distal-seg) (segment-length distal-seg)))
		      d2d2 (the sf (* (segment-diameter distal-seg) (segment-diameter distal-seg)))
		      ;; The new segment diameter is then chosen to conserve membrane area.
		      new-diameter (the sf (/ (the sf (+ d1l1 d2l2)) new-length))
		      ;; We need to adjust the intracellular resistivity to conserve the total g_i.
		      ri-coeff
		      (the sf
			   (/ (the sf
				   (* (the sf (* new-diameter new-diameter))
				      (+ (the sf (/ (the sf (* distal-length distal-cyto-coeff)) d2d2))
					 (the sf (/ (the sf (* prox-length prox-cyto-coeff)) d1d1)))))
			      new-length))))
	      
	      (setq new-name (create-consolidated-segment-name (segment-name seg) (segment-name distal-seg)))
	      (transfer-node-elements middle-node (segment-node-2 distal-seg))
	      (push distal-seg (node-elements (segment-node-1 seg)))
	      (rename-segment distal-seg new-name)
	      (rename-node (segment-node-2 distal-seg) new-name)
	      (when (segment-dummy-proximal-node-location seg)
		(setf (segment-dummy-proximal-node-location distal-seg) (segment-dummy-proximal-node-location seg)))
	      (setf (segment-node-1 distal-seg) (segment-node-1 seg)
		    (segment-diameter distal-seg) new-diameter
		    (segment-length distal-seg) new-length)
	      (unless (= 1.0 ri-coeff)
		(element-parameter distal-seg 'ri-coeff ri-coeff))
	      (SET-SEGMENT-MEMBRANE-PARAMETERS distal-seg nil)
	    
	      (remove-entry-from-hash middle-node (NODE-HASH-TABLE))
	      ; (setf (segment-node-2 seg) nil) ; So erase-element doesn't erase anything else
	      (erase-element seg 'segment nil t))
	  
	    (unless (and segment-consolidated single-step)
	      (setq segment-consolidated (consolidate-distal-segment distal-seg single-step))))

	  (loop for distal-seg in (distal-segments seg)
		when (and (consolidate-distal-segment distal-seg single-step)
			  single-step)
		do (return (setq segment-consolidated t))))
      segment-consolidated)))



(defun rename-node (node new-name)
  (remove-entry-from-hash node (NODE-HASH-TABLE))
  (setf (gethash new-name (NODE-HASH-TABLE)) node)
  (setf (node-name node) new-name)
  (setq *num-nodes* (1+ *num-nodes*))	; REMOVE-ENTRY-FROM-HASH decrements this
  )

(defun rename-segment (seg new-name)
  ;; (unless (stringp new-name) (sim-error (format nil "Name ~A for RENAME-SEGMENT is not a string" new-name)))
  (when (gethash new-name (SEGMENT-HASH-TABLE))
    (sim-error (format nil "Name ~A for RENAME-SEGMENT is already being used" new-name)))
  (remove-entry-from-hash seg (SEGMENT-HASH-TABLE))
  (setf (gethash new-name (SEGMENT-HASH-TABLE)) seg)
  (setf (segment-name seg) new-name))


(defun rename-segments-with-simple-count (&key (cell (cells)) nodes-also)
  (let ((seg-count 1)
	(node-count 1)
	(segments (somas-and-segments (or cell (cells)))))
    (loop for seg in segments do
	  (loop while (element seg-count 'segment) do (incf seg-count))
	  (rename-segment seg seg-count)
	  when nodes-also do
	  (loop while (element node-count 'node) do (incf node-count))
	  (rename-node (segment-node-2 seg) node-count))))

(defvar *segment-name-offset* 0)

;; Parsing the anatomy file format from Guy Major.
(defun parse-gm-format (&optional (cell-name "gm") cell-type)
  (when *initialize-before-next-circuit* (when (= (length *circuit*) 0) (setq *circuit* cell-name)))
  (let* (*use-simple-names*
	 *add-cell-name-to-segs*
	 (segment-name-offset (hash-table-count (SEGMENT-HASH-TABLE)))
	 (cell (create-cell cell-name :cell-type cell-type))
	 (soma (create-soma :cell cell :diameter *gm-soma-diameter))
	 (GM-DENDRITE-DESCRIPTION  *GM-DENDRITE-DESCRIPTION*) 
	 (gm-branch-point-description  *gm-branch-point-description*)
	 (list-w-branchs
	  (loop for dendrite in  GM-DENDRITE-DESCRIPTION
		collect (list (nth 0 dendrite) ; seg name
			      (nth 1 dendrite) ; length
			      (nth 2 dendrite) ; diam
			      (when (= 2 (nth 5 dendrite))	
				(cddr (car gm-branch-point-description))))
		into result
		do (when (= 2 (nth 5 dendrite)) (setq gm-branch-point-description (cdr gm-branch-point-description)))
		finally (return result)))
	 
	 (soma-branch-count 0)
	 (source-list
	  (loop for dendrite in GM-DENDRITE-DESCRIPTION
		collect (list (nth 0 dendrite) ; seg name
			      (nth 1 dendrite) ; length
			      (nth 2 dendrite) ; diam
			      (loop for info-dend in list-w-branchs
				    do (when (member (nth 0 dendrite) ; seg name
						     (cadddr info-dend))
					 (return (car info-dend)))
				    finally (setq soma-branch-count (1+ soma-branch-count)) (return 'soma)))))
	 (soma-branch-index 0))
    (setq *segment-name-offset* segment-name-offset)
    (loop for dendrite in source-list
	  do 
	  (create-segment (+ segment-name-offset (nth 0 dendrite))
			  (if (eq 'soma (car (last dendrite)))
			      (soma-name soma)
			      (+ segment-name-offset (car (last dendrite))))
			  cell
			  :phi  (if (eq 'soma (car (last dendrite)))
				    (progn (setq soma-branch-index (1+ soma-branch-index))
					   (* soma-branch-index 2.0 (/ pi-single soma-branch-count)))
				    (* 0.1  pi-single (- (random 1.0) 0.5)))
			  :length (* 1.0e6 (nth 1 dendrite)) ; length
			  :diameter (* 1.0e6 (nth 2 dendrite)) ; diam
			  ))
    cell))

(defun basic-gm ()
  (parse-gm-format))




;; Function for create structure or add channels with density...

;; create-tree-polar create un tree with the function create-segment, the arguments are:
;;       the cell-name and a list of segment with the pattern
;; theta and phi are in degree !!
;;   '((segment-name proximal-node-name diameter length theta phi spine-membrane-factor extras) ...
;;     (segment-name proximal-node-name diameter length theta phi spine-membrane-factor extras))
;;
;; The 'membrane-area-coefficient or SPINE-MEMBRANE-FACTOR term is optional. The EXTRAS term is optional - when
;; included the segment's :PARAMETER slot is set to this.
(defun create-tree-polar (cell list-segment &optional add-cell-name-to-segs)
  (let (*add-cell-name-to-segs*)
    (do ((segments list-segment (cdr segments)))
	((null segments))
      (when (car segments)
	(let* ((cell (element cell 'cell))
	       (cell-name (element-name cell 'cell))
	       (segment-info (car segments))
	       (segment-name (if add-cell-name-to-segs
				 (format nil "~A-~A" cell-name (nth 0 segment-info))
				 (nth 0 segment-info)))
	       (proximal-node-name (let ((name (or (element-name (nth 1 segment-info))
						    (nth 1 segment-info))))
				     (if (string= name (soma-name (cell-soma (element cell-name))))
					 name
				       (if add-cell-name-to-segs (format nil "~A-~A" cell-name name)
					   name))))
					   
	       (diameter (nth 2 segment-info))
	       (length (nth 3 segment-info))
	       (theta (nth 4 segment-info))
	       (phi (nth 5 segment-info))
	       (spine-membrane-factor (nth 6 segment-info))
	       (extras (nth 7 segment-info))
	       (seg (create-segment segment-name proximal-node-name cell-name
				    :diameter diameter :length length 
				    :theta (/ theta 57.295779513)
				    :phi (/ phi 57.295779513)
				    :parameter-a-list extras)))

	  (when (and spine-membrane-factor (not (= 1.0 spine-membrane-factor)))
	    (element-parameter seg 'membrane-area-coefficient spine-membrane-factor)))))))

;; IT'S "T" LIKE TRUNK, "O" LIKE OBLIQUE, "B" LIKE BASAL, AND "D" LIKE DISTAL

;; function for add channels in the soma with precise density 


;; (defun add-channel-in-soma-with-gbars (soma-name channel-type-list gbar-list)
;;  (loop for chan-type in channel-type-list
;;	for gbar in gbar-list do
;;	(setf (channel-gbar-ref (create-channel (gethash soma-name (SOMA-HASH-TABLE)) chan-type) gbar))))


;; Explicitely set channel-gbar-ref to nil in case the channel type definition had a value for the
;; (absolute) gbar value instead of for the gbar density.

(defun add-channel-in-soma-with-density (soma-name chtype-gbar-density-list &optional (name-prefix ""))
  (let ((soma (typecase soma-name
		(soma soma-name)
		(t (element (if (= 0 (length name-prefix))
			   soma-name
			   (format nil "~A~A" name-prefix soma-name)) 'soma)))))
    (loop for type-density in chtype-gbar-density-list do
	  (unless (= 0 (cadr type-density))
	    (element-parameter (create-channel soma (car type-density))
				   'gbar-density
				 (cadr type-density))))))

;; the segment list is a list of name of segment

(defun add-channel-in-segment-with-density (segment-list chtype-gbar-density-list &optional (name-prefix ""))
  (loop for seg-name in segment-list
	do (let ((seg (element (if (= 0 (length name-prefix))
				   seg-name
				   (format nil "~A~A" name-prefix seg-name)) 'segment)))
	     (when seg
	       (loop for type-density in chtype-gbar-density-list do
		     (unless (= 0 (cadr type-density))
		       (element-parameter
			(create-channel seg (car type-density))
			'gbar-density
			(cadr type-density))))))))




;; motif (ex: motif = "o" and position = 0  for segment like "o121")

(defun add-channel-in-segment-with-density-motif (motif position chtype-gbar-density-list cell)
  (loop for seg in (cell-segments cell) do
	(if (equal (string (char (segment-name seg) position)) motif)
	    (loop for type-density in chtype-gbar-density-list do
		  (unless (= 0 (cadr type-density))
		    (element-parameter (create-channel seg (car type-density))
					   'gbar-density
					 (cadr type-density)))))))







;;; DUMP-TREE-LIST and PUSH-SEGMENT-TREE-LIST-COMPONENT are used to
;;; write a cell-tree based lisp file(s) descibed the loaded cell(s).
(defun push-segment-tree-list-component (mother-symbol segment cell-tree cell-name)
  (let ((mother-symbol (typecase mother-symbol
			 (number (format nil "~A" mother-symbol))
			 (t mother-symbol)))
	(segment-name
	 (typecase (segment-name segment)
	   (string (segment-name segment))
	   (t (format nil "~A" (segment-name segment))))))
    (push (no-nils
	   (list (if (equal 'soma mother-symbol) mother-symbol
		     (string
		      (if *ADD-CELL-NAME-TO-SEGS-FOR-TREE-DUMP*
			(if (search cell-name (string mother-symbol) :end2 (length cell-name))
			  (string mother-symbol)
			  (format nil "~a-~a" cell-name  (string mother-symbol)))
			(string mother-symbol))))
		 (string
		  (if *ADD-CELL-NAME-TO-SEGS-FOR-TREE-DUMP*
		    (if (search cell-name (case (segment-name segment)
					    (string (segment-name segment))
					    (t (format nil "~A" (segment-name segment))))
				:end2 (length cell-name))
		      segment-name
		      (format nil "~a-~a" cell-name
			      segment-name))
		    segment-name))
		 (read-from-string (my-float-format (first (node-relative-location (segment-node-2 segment)))))
		 (read-from-string (my-float-format (second (node-relative-location (segment-node-2 segment)))))
		 (read-from-string (my-float-format (third (node-relative-location (segment-node-2 segment)))))
		 (read-from-string (my-float-format (segment-diameter segment)))
		 (when (or (element-parameter segment 'ri-coeff)
			   (element-parameter segment 'ra-coeff)
			   (element-parameter segment 'cytoplasmic-resistivity-coeff)
			   (element-parameter segment 'axon-segment)
			   (element-parameter segment 'soma-segment))
		   (no-nils
		    (list
		     (when (or (element-parameter segment 'ri-coeff)
			       (element-parameter segment 'ra-coeff)
			       (element-parameter segment 'cytoplasmic-resistivity-coeff))
		       (list 'ri-coeff
			     (read-from-string (my-float-format
						(or (element-parameter segment 'ri-coeff)
						    (element-parameter segment 'ra-coeff)
						    (element-parameter segment 'cytoplasmic-resistivity-coeff))))))
		     (when (element-parameter segment 'axon-segment)
		       (list 'axon-segment t))
		     (when (element-parameter segment 'soma-segment)
		       (list 'soma-segment t)))))))
	  cell-tree)
    (dolist (distal-segment (distal-segments segment))
      (setq cell-tree (push-segment-tree-list-component (segment-name segment) distal-segment cell-tree cell-name)))
    cell-tree))


(defun dump-tree-menu ()
  (let (dummy1 dummy2 dummy3)
    (choose-variable-values
     `((dummy1 "Put all selected cells in one function/file" :boolean)
       (dummy2 "Include channels, synapses, and axons in cell descriptions" :boolean)
       (dummy3 "Convert segment names to simple names" :boolean)
       (*ADD-CELL-NAME-TO-SEGS-FOR-TREE-DUMP* "Add cell name to all segment names in dumped file" :boolean))
     :text (concatenate-strings
	    "Adding cell names to segments can cause misnaming"
	    (format nil "~%")
	    "in channel, synapse and axon references in the file."
	    (format nil "~%")
	    "Such errors may be edited later."))
    (dump-tree-list
     :cells (things-of-names
	     (choose-list-values (names-of-things (cells)) nil :label "Select cell geometries to write to file(s)")
	     'cell)
     :convert-to-simple-names dummy3
     :separate-files (not dummy1)
     :include-membrane-elts dummy2)))



(defun dump-tree-list (&key (cells (cells)) separate-files include-membrane-elts convert-to-simple-names)
  (let* ((pathname-directory (get-surf-data-directory)) 
	 (filename (format nil "~A~A.tree.lisp" pathname-directory *circuit*))
	 (cells (element cells 'cell))
	 *print-pretty*) 
    (unix:unix-mkdir (ext:unix-namestring pathname-directory nil) #o777)
    (when (probe-file (ext:unix-namestring pathname-directory nil))
      (when convert-to-simple-names (rename-segments-simple (somas-and-segments cells nil)))
      (if separate-files
	(loop for cell in cells do
	      (let ((filename (format nil "~A~A.tree.lisp" pathname-directory (cell-name cell))))
		(with-open-stream (stream (open filename :direction :output :if-does-not-exist :create :if-exists :append))
				  (format stream ";;; -*- Package: SURF; Mode: LISP -*-~%~%")
				  (write-loadable-cell-description stream (list cell) (cell-name cell) include-membrane-elts)
				  (format t "File ~a written~%" filename))))
	(with-open-stream (stream (open filename :direction :output :if-does-not-exist :create))
			  (format stream ";;; -*- Package: SURF; Mode: LISP -*-~%~%")
			  (write-loadable-cell-description stream cells *circuit* include-membrane-elts)
			  (format t "File ~a written~%" filename))))))

#|
(defun write-cell-definition-defun-guts (cells stream)
  (loop for cell in (coerce-to-list cellls) do
	(WRITE-LISP-SYM-LIST-TO-STREAM	
	 (read-from-string (format nil "~a-cell-tree" (cell-name cell)))
	 (let ((cell-tree '()))
	   ;;Start at the cell soma, and work on each of the branches that originate there in turn.
	   (loop for element in (node-elements (soma-node (cell-soma cell)))
		 when (eq (named-structure-symbol element) 'segment)
		 do (setq cell-tree
			  (push-segment-tree-list-component 'soma element cell-tree (cell-name cell))))
	   cell-tree)
	 stream 1 2)
	(format stream "~%~%")
	(format stream
		"  (create-tree~%")
	(format stream
		"   (create-soma ~%")
	(format stream		    
		"    :cell (create-cell ~s~%" (cell-name cell))
	(format stream		    
		"            :cell-origin '~a~%" (cell-origin cell))
	(format stream		    
		"            :cell-type (create-celltype ~a~s))~%"
		(if (symbolp (cell-type-name (cell-type cell))) "'" "")
		(cell-type-name (cell-type cell)))
	(format stream
		"    :diameter ~a)    ; Soma diameter in um~%" (soma-diameter (cell-soma cell)))
	(format stream
		"       ~a-cell-tree)~%~%" (cell-name cell))))
|#

(defun write-cell-definition-defun-guts (cells stream)
  (loop for cell in (coerce-to-list cells)
	collect (format nil "~a-cell-tree" (cell-name cell)) into tree-names
	collect	(let ((tree '()))
		  ;; Start at the cell soma, and work on each of the branches that originate there in turn.
		  (loop for element in (node-elements (soma-node (cell-soma cell)))
			when (eq (named-structure-symbol element) 'segment)
			do (setq tree (push-segment-tree-list-component 'soma element tree (cell-name cell))))
		  tree)
	into trees
	finally 
	(format stream
		" (let (")
	(loop for tree-name in tree-names
	      for tree in trees
	      for tree-count from 1 do
	      (format stream
		      "~A(~A~%" (if (= tree-count 1) "" "       ") tree-name)
	      (loop for component in tree
		    for count from 1 do
		    (format stream "        ~A~s~A"
			    (if (= count 1) "`(" " ")
			    (loop for part in component
				  for i from 1
				  when (<= i 2)
				  collect (typecase part
					    (string (if (string-has-non-number part)
						      part
						      (read-from-string part)))
					    (t part))
				  into out
				  else collect part into out
 				  finally (return out))
			    (if (= count (length tree)) ")" (format nil "~%")))
		    finally
		    (format stream (if (= tree-count (length trees)) 
				     "))~%"  ")~%"))))
	(format stream "~%")
	(loop for tree-name in tree-names
	      for cell in cells do
	      (format stream
		      "  (create-tree~%")
	      (format stream
		      "   (create-soma ~%")
	      (format stream		    
		      "    :cell (create-cell ~s~%" (cell-name cell))
	      (format stream		    
		      "                       :cell-origin '~a~%" (cell-origin cell))
	      (format stream		    
		      "                       :cell-type (create-celltype ~a~s))~%"
		      (if (symbolp (cell-type-name (cell-type cell))) "'" "")
		      (cell-type-name (cell-type cell)))
	      (format stream
		      "    :diameter ~a)    ; Soma diameter in um~%" (soma-diameter (cell-soma cell)))
	      (format stream
		      "   ~a)~%~%" tree-name))
	(format stream
		"      )~%")))

(defun write-loadable-cell-description (stream cells funname include-membrane-elts)
  (let ((*standard-output* stream))
    (format t
	    "#|~%~%")
    (if (= (hash-table-count (CELL-HASH-TABLE)) 1)
	(progn
	  (format t
		  " *******  Dump Tree for Cell ~a  ******* ~%~%~%" (cell-name (car cells)))
	  (format t
		  "    Cell originally created from ~A~%~%" (cdr-assoc 'cell-definition (cell-parameters (car cells))))
	  (cdr-assoc 'cell-definition (cell-parameters (car cells))))
	(progn
	  (format t
		  " *******  Dump Trees for Cells:  ******* %~%")
	  (loop for cell in cells do
		(format t " ~A~%" (cell-name cell)))
	  (format t "~%~%")))
    (print-circuit)
    (format t
	    "|#~%~%")
    (when include-membrane-elts
      (loop for synapse-type in (synapse-types)
	    when (loop for synapse in (synapses-of-type synapse-type) when (member (element-cell synapse) cells)
		       do (return t))
	    do (document-synapse-type synapse-type))
      (loop for channel-type in (channel-types)
	    when (loop for channel in (channels-of-type channel-type) when (member (element-cell channel) cells)
		       do (return t))
	    do (document-channel-type channel-type)))
    (loop for type in (delete-duplicates (coerce-to-list (element-type cells))) do (document-cell-type type))
    (format t
	    "(defun ~a ()~%" funname)
    (write-cell-definition-defun-guts cells stream)

    (when include-membrane-elts
      (loop for cell in cells do
	    (print-create-channels-for-cell cell 2)
	    (print-create-axons-for-cell cell 2)
	    (print-create-synapses-for-cell cell 2)
	    (print-create-sources-for-cell cell 2)))
    (format t
	    "  NIL)~%~%")
    (format t
	    "(push '~a *CIRCUIT-FUNCTIONS*)~%~%~%~%" funname)))

#|
(defun write-loadable-cell-description (stream cells funname include-membrane-elts)
  (let ((*standard-output* stream))
    (format t
	    "#|~%~%")
    (if (= (hash-table-count (CELL-HASH-TABLE)) 1)
	(progn
	  (format t
		  " *******  Dump Tree for Cell ~a  ******* ~%~%~%"
		  (cell-name (car cells)))
	  (format t
		  "    Cell originally created from ~A~%~%" (cdr-assoc 'cell-definition
								       (cell-parameters (car cells))))
	  (cdr-assoc 'cell-definition (cell-parameters (car cells))))
	(progn
	  (format t
		  " *******  Dump Trees for Cells:  ******* %~%")
	  (loop for cell in cells do
		(format t " ~A~%" (cell-name cell)))
	  (format t "~%~%")))
    (print-circuit)
    (format t
	    "|#~%~%")

    (when include-membrane-elts
      (loop for synapse-type in (synapse-types)
	    when (loop for synapse in (synapses)
		       when (and (eq synapse-type (synapse-type synapse))
				 (member (element-cell synapse) cells))
		       do (return t))
	    do (document-synapse-type synapse-type))
      (loop for channel-type in (channel-types)
	    when (loop for channel in (channels)
		       when (and (eq channel-type (channel-type channel))
				 (member (element-cell channel) cells))
		       do (return t))
	    do (document-channel-type channel-type))
      )
    (loop for cell-type in (cell-types)
	  when (instance-in-circuit cell-type) do (document-cell-type cell-type))
    (format t
	    "(defun ~a ()~%" funname)
    (loop for cell in cells do
	  (WRITE-LISP-SYM-LIST-TO-STREAM	
	   (read-from-string (format nil "~a-cell-tree" (cell-name cell)))
	   (let ((cell-tree '()))
	     ;;Start at the cell soma, and work on each of the branches that originate there in turn.
	     (loop for element in (node-elements (soma-node (cell-soma cell)))
		   when (eq (named-structure-symbol element) 'segment)
		   do (setq cell-tree
			    (push-segment-tree-list-component 'soma element cell-tree (cell-name cell))))
	     cell-tree)
	   t 1 2)
	  (format t "~%~%")

	  (format t
		  "  (create-tree~%")
	  (format t
		  "   (create-soma ~%")
	  (format t		    
		  "    :cell (create-cell ~s~%" (cell-name cell))
	  (format t		    
		  "            :cell-origin '~a~%" (cell-origin cell))
	  
	  (format t		    
		  "            :cell-type~%")
	  (format t
		  "               (create-celltype ~s)~%" (cell-type-name (cell-type cell)))
	  (format t
		  "      :diameter  ~a)    ; Soma diameter in um~%" (soma-diameter (cell-soma cell)))
	  (format t
		  "       ~a-cell-tree)~%~%" (cell-name cell)))

    (when include-membrane-elts
      (loop for cell in cells do
	    (print-create-channels-for-cell cell 2)
	    (print-create-axons-for-cell cell 2)
	    (print-create-synapses-for-cell cell 2)))

    (format t
	    "       )~%~%")

       
    (format t
	    "(push '~a *CIRCUIT-FUNCTIONS*)~%~%~%~%" funname)))
|#