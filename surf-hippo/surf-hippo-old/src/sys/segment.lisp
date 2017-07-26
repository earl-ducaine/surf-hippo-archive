;; -*- mode: lisp; Syntax: Common-lisp; package: #-parallel surf #+parallel *surf; base: 10;  -*-
;;; (c) Copyright 1988, Don Webber, Thinking Machines Corporation, Inc.
;;; alterations (c) Copyright 1990, Lyle Borg-Graham, MIT Center for Biological Information Processing
;;; File creation date: 2/3/88 20:21:00
;
; the segment model, for approximating distributed RC lines
;

#-parallel
(in-package "SURF-HIPPO"
	    :use '("COMMON-LISP-USER" "COMMON-LISP")
	    :nicknames '("SURF"))
#+parallel 
(in-package "*SURF")


(defstruct segment			;Segment name is same a distal node name.
  "Model for a segment"			;Note that segments are added to the cell from the soma out.
  (name ""		)		;                             g-axial
  node-1				;Proximal node       Prox o---/\/\/\-----+---o Distal
  node-2				;Distal node                             |
					;                                   memb-elements	
					;                                        |
					;                                       Gnd
  cell
  (ipl-stratum nil)			;For retinal cells: 1 to N for dendrites in the inner
					;plexiform layer. NIL if not in IPL.
  (length zero :type single-float)	;microns
  (diameter zero :type single-float)	;microns
  (theta zero :type single-float)	;Elevation angle relative to proximal branch orientation.
  (phi zero :type single-float)		;Azimuth angle relative to proximal branch orientation.
  core-seg
  model
  (g-axial zero :type single-float)	;microsiemens
  (g-leak zero :type single-float)	;microsiemens
  (v-leak zero :type single-float)	;millivolts
  (capacitance zero :type single-float)	;nanofarads
  conc-int				;Concentration integrator
  (branch-node-index nil :type (or null fixnum)) ;If this segment is connected to a Hines node, then this is the
					;index for that node.
  )

#-parallel
(defstruct core-segment
  "Core model for a segment."
  (node-1-pointp nil	)	; zero if constant, one if node
  node-1-point				;core-node
  (node-1-const zero	)
  (node-2-pointp nil	)	; zero if constant, one if node
  node-2-point				;core-node
  (node-2-const zero	)

  (mat-12-valid nil	)
  mat-12-point				;core-off-diag
  (mat-21-valid nil	)
  mat-21-point				; core-off-diag

  (g-axial zero :type single-float)
  (g-leak zero :type single-float)
  (v-leak zero :type single-float)
  (capacitance zero :type single-float)
)

(defun look-at-branchs ()
  (dolist (branch *branch-list*)
    (print "A new branch:")
    (dolist (segment branch)
      (if (eq (named-structure-symbol segment) 'segment)
	  (format t "Branch segment ~a ~%"
		  (segment-name segment))
	  (format t "soma - ~a ~%" (soma-name segment))))))

(defun create-segment-model ()
  "Creates a template for all segments."
  (let ((template (make-model-template)))
    (setf
     (model-template-name template) (string "segment")
     (model-template-default-params template)
     '((g-axial . 0.0) (g-leak . 0.0) (diameter . 0.0) (length . 0.0)(cell . "")
       (v-leak . -70.0) (capacitance . 0.0)(theta . 0.0)(phi . (* -0.5 pi-single)))
     (model-template-eval-routine template) #'eval-segment
     (model-template-print-routine template) #'print-segment
     (model-template-create-routine template) #'create-segment
     (model-template-create-core-routine template) #'create-core-segment
     (model-template-add-off-diag-routine template) #'add-off-diag-segment
     (model-template-find-coupling-routine template) #'find-coupling-segment
     (model-template-fix-dc-nodes-routine template) #'segment-fix-dc-nodes
     *models* (cons template *models*)
     (gethash (string "segment") *model-hash-table*) template
     segment-hash-table (make-hash-table :test #'equal))
					; only need one seg model instance, so create it now.
    (create-model-instance (string "seg") (model-template-name template) '() )))


(defun print-segment (seg)
  "Prints out this data associated with a segment."
  (format
    *output-stream
    "Segment ~a (~a, ~a): g-a ~,2e uS, g-l ~,2e uS, v-l ~2d mV, cap ~,2e nF, length ~4f, diameter ~4f uM~%"
	     (segment-name seg)
	     (node-name (segment-node-1 seg))
	     (node-name (segment-node-2 seg))
	     (segment-g-axial seg)
	     (segment-g-leak seg)
	     (segment-v-leak seg)
	     (segment-capacitance seg)
	     (segment-length seg)
	     (segment-diameter seg)
	     ))


;;; CREATE-SEGMENT Creates a element of type segment. Inputs 'segment-name' and 'proximal-node-name' are strings,
;;; and 'parameters' is an a-list. The distal node name is the same as the segment name.
;;; Note that the membrane properties will be set again with the call to SET-SEGMENTS-MEMBRANE-PARAMETERS since
;;; segment dimensions may have to be figured out later when the segment is defined in terms of its
;;; cooridinates. Relative location is of distal node relative to cell soma. Relative location of distal node
;;; relative to cell soma may also be determined by passing 'phi and 'theta and 'length in 'parameters list
;;; (see, for example, the function TREE-CONTROL in the MISC file). Precedence of location information when
;;; segment node locations are determined is defined in the function LOCATE-DISTAL-NODE (below).
(defun create-segment (segment-name proximal-node-name cell-name parameters
				    &key (plot-pane 2) (relative-location '(0.0 0.0 0.0)) (model-ca-variation nil))
  (if
   (gethash segment-name segment-hash-table)
   (sim-warning (format nil "create-segment: segment ~a  already defined, ignoring" segment-name))
   (let ((n1 (create-node proximal-node-name :cell-name cell-name :plot-pane plot-pane))
	 (n2 (create-node segment-name :cell-name cell-name :plot-pane plot-pane))
	 (model (gethash "seg" *model-instance-hash-table*))
	 (cell (gethash cell-name cell-hash-table))
	 v-leak length diameter theta phi)
     (update-cell-name-list cell-name)
     (if (not (or (eq n1 n2)
		  (and (or (node-is-dc-source n1) (node-is-pwl-source n1))
		       (or (node-is-dc-source n2) (node-is-pwl-source n2)))))
	 (progn
	   (setf theta (set-create-parameters 'theta model parameters))
	   (setf phi (set-create-parameters 'phi model parameters))
	   (setf v-leak (set-create-parameters 'v-leak model parameters))
	   (setf length (set-create-parameters 'length model parameters))
	   (setf diameter (set-create-parameters 'diameter model parameters))
	   (let ((seg
		  (make-segment :name segment-name :cell cell :theta theta :phi phi
				:v-leak v-leak
				:length length :diameter diameter
				:node-1 n1 :node-2 n2 :model model)))
	     (setf
	      (node-relative-location n2) relative-location
	      (node-elements n1) (cons seg (node-elements n1))
	      (node-elements n2) (cons seg (node-elements n2))
	      (gethash segment-name segment-hash-table) seg
	      (model-instance-elements model) (cons seg (model-instance-elements model)))
	     (if model-ca-variation
		 (setf (segment-conc-int seg)
		       (create-conc-int (format nil "~a-conc-int" segment-name) seg)))
	     seg))))))


(defun set-segments-membrane-parameters ()
  (maphash 'set-segment-membrane-parameters segment-hash-table))

;;; SET-SEGMENT-MEMBRANE-PARAMETERS
;; Set segment membrane properties that depend on segment dimensions.
(defun set-segment-membrane-parameters (name seg)
  (declare (ignore name))
  (setf (segment-g-axial seg)
	(g-axial (segment-length seg)
		 (segment-diameter seg)
		 (cell-type-cytoplasmic-resistivity (cell-type (segment-cell seg)))))
  (setf (segment-g-leak seg)
	(g-leak-mem (segment-length seg)
		    (segment-diameter seg)
		    (cell-type-membrane-resistivity (cell-type (segment-cell seg)))))
  (setf (segment-capacitance seg)
	(cap-mem (segment-length seg)
		 (segment-diameter seg)
		 (cell-type-specific-capacitance (cell-type (segment-cell seg))))))


;;; LOCATE-ALL-NODES Complete the geometrical description of the cells' nodes, after the circuit has been
;;; loaded.
(defun locate-all-nodes ()
  (maphash 'locate-cell-nodes cell-hash-table))



;;; ** APPLYING THE METHOD OF HINES TO THE BRANCHED TREE STRUCTURE OF EACH CELL **

;;(Note that for the Hines method, "delta-v" is actually V(t + dt/2).)

;;; This description of branches is derived from Hines, "Efficient computation of branched nerve equations" Int.
;;; J.  Bio-Medical Computing (15) (1984) p69-76. A branch is composed of connected segments. The ends of
;;; the branch are those segments which connect to more than one segment at either end of that segment.
;;; The branches, and in turn the branch segments, are numbered as follows: Choose any branch of the tree which
;;; is connected at one end to the soma. Number the segments of that branch so that the segment connected to the
;;; soma is the last segment. The first segment of this branch ('trunk') will be called the 'branch node'.

;;; The soma is also considered a 'branch node'.

;;; All branches connecting to a branch node have their segments numbered so that their last segment connects to
;;; this node. Their first segments are also called branch nodes (as long as other segments are connected to
;;; them), and this segment-numbering process continues until all the segments are numbered. The last numbered
;;; branches, 'twigs', all have one end (their first segments) unconnected. Each branch node becomes the center
;;; of a Wye network.

;;; Continue this procedure for all other segments connected to the soma.

;;; Branches are numbered as follows: Assume that there are N branches. Starting at soma, number all the
;;; branches connecting to the soma starting with N and decrementing. Continue numbering all the branches that
;;; are connected to the previous set of branches, decrementing the branch number. Continue working out on the
;;; tree until all the branches are labeled.

;;; Segment nodes are ordered according to the branch number and the proximal segment number. For example, if
;;; branch 33 has 2 segments and branch 34 has 3 segments, then the nodes would be ordered: ...33-1, 33-2, 34-1,
;;; 34-2, 34-3... The soma node has the highest node index.

;; BUILD-BRANCH-LIST This is called from LOCATE-CELL-NODES, where cell nodes are located seqentially from the
;; soma outward, and from GET-AND-LOCATE-DISTAL-SEGMENTS, which is called recursively from LOCATE-CELL-NODES
;; (for each branch originating at the soma).
(defun build-branch-list (seg)
  (let ((last-seg (caar *branch-list*))		;Get the last segment added to the *branch-list*
	(last-seg-d-node-segs 0)		;Last seg's prox node segs (which includes last seg).
	(last-seg-d-to-seg nil))		;Is last segment distal to current seg?
    (if last-seg
	(progn
;;;Count the segments attached to the last segment's proximal node. If this is greater than 2, then this segment
;;;was a node of the last branch. 
	  (dolist (elt (node-elements (segment-node-2 last-seg)))
	    (if (eq (named-structure-symbol elt) 'segment)
		(incf last-seg-d-node-segs)))
;;;See if current segment is distal to last segment.
	  (dolist (elt (node-elements (segment-node-2 last-seg)))
	    (if (and (eq (named-structure-symbol elt) 'segment) (eq elt seg))
		(setq last-seg-d-to-seg t)))))    
;;;Now update *branch-list*.
    (setq *branch-list*
	  (if (or (not last-seg-d-to-seg)	;If this seg not connected to last seg,
		  (> last-seg-d-node-segs 2)	;or the last seg was node of the latest branch,
		  (not last-seg))		;or we are at the beginning of *branch-list*,
	      (cons (list seg) *branch-list*)	;then START a new branch with the new seg.
	      (cons (cons seg (car *branch-list*)) (cdr *branch-list*))))	;Else ADD seg to latest branch.   
    (if *debug-hines*
	(format t "BBL: seg ~a; last-ele ~a - dis-segs ~a add-branch-now ~a ~%"
		(segment-name seg) 
		(if last-seg (segment-name last-seg))  last-seg-d-node-segs 
		(or (> last-seg-d-node-segs 2)	;If the *last* seg was node of the latest branch,
		    (not last-seg-d-to-seg) (not last-seg))))))

;;; **** Solving the System ****
;;; Upper triangulization of the matrix is accomplished by repeated applications of the procedure TRIANG.
;;; Define a 'twig' as any branch whose distal end is *not* connected to an un-TRIANGed branch (including no
;;; branch at all). Now upper triangularize the matrix by doing the following until there are only twigs left:
;;; Collect the set of all current twigs, except any twigs which have already been TRIANGed. Apply TRIANG to the
;;; remaining set of twigs, in the order that they appear in the matrix(?). Repeat.

(defun HINES-SOLVE ()
  (loop for branch in *branch-list* do (TRIANG branch))
;Take care of soma node first
  (let ((soma-index (- (length *delta-v*) 1)))
    (declare (fixnum soma-index))
    (setf (aref *delta-V* soma-index)
	       (/ (aref *RHS* soma-index)
		  (aref *diag* soma-index)))
    (if *debug-hines* (format t "V-~a = RHS-~a / A-~a~%" soma-index soma-index soma-index)))
  (loop for branch in (reverse *branch-list*) do (BKSUB branch)))

;; (aref *lower-diag* i) = (aref *A* i i-1)
;; (aref *upper-diag* i) = (aref *A* i i+1)


(defun TRIANG (branch)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (declare (cons branch))
  ;; 'branch' is a list of segments in the branch, in order of their
  ;; node indexes.  Note that for the soma branch, the soma will be
  ;; skipped. We want to go from second segment of branch to last
  ;; segment. If branch has only one segment, then process that one at
  ;; least for its branch-node-index.
  (loop for segment in (if (= (length branch) 1) branch (cdr branch)) do
	(let ((node-index (node-index (segment-node-2 segment)))
	      (node-index-1 (- (node-index (segment-node-2 segment)) 1))
	      (branch-node-index 0))
;;; this doesn't make any difference.  (declare (fixnum node-index node-index-1 branch-node-index))
	  (if (> (length branch) 1)
	      (let ((coefficent  (/ (aref *lower-diag* node-index)(aref *diag* node-index-1))))
	;	(declare (single-float coefficent))
;; (if *debug-hines* (format t "A-~a = A-~a - U-~a * (L-~a / A-~a)~%"
;; node-index node-index node-index-1 node-index node-index-1))
		(setf (aref *diag* node-index) (- (aref *diag* node-index)
						  (* (aref *upper-diag* node-index-1) coefficent)))
		(setf (aref *RHS* node-index)  (- (aref *RHS* node-index)
						  (* (aref *RHS* node-index-1) coefficent)))))
	  (if (and (eq segment (car (last branch)))
		   (setq branch-node-index (segment-branch-node-index segment)))
	      (let ((coefficent (/ (- (segment-g-axial segment)) (aref *diag* node-index))))
		(declare (single-float coefficent))
					;	    (if *debug-hines*			;
					;		(format t "A-~a = A-~a - g-axial-~a * (g-axial-~a / A-~a)~%"
					;			branch-node-index branch-node-index
					;			node-index node-index node-index ))
		(setf (aref *diag* branch-node-index) (- (aref *diag* branch-node-index)
							 (* (- (segment-g-axial segment)) coefficent)))
		(setf (aref *RHS* branch-node-index)  (- (aref *RHS* branch-node-index)
							 (*  (aref *RHS* node-index) coefficent)))
		)))))

;;; Now back-substitute the upper-triangular matrix as follows:
(defun BKSUB (branch)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (declare (cons branch))
  ;; 'branch' is a list of segments in the branch, in order. 
  (loop for segment in (reverse branch) do
    (let ((branch-node-index 0)
	  (node-index (node-index (segment-node-2 segment))))
;;; this doesn't make any difference      (declare (fixnum node-index branch-node-index))
;      (if *debug-hines*
;	  (format t "V-~a = (rhs-~a" node-index node-index))
      (setf (aref *delta-V* node-index)
	    (/ (- (aref *RHS* node-index)
		  (cond
		    ((eq segment (car (last branch)))
		     (if (setq branch-node-index (segment-branch-node-index segment))
;			 (progn;  (if *debug-hines* (format t " - (V-~a * g-axial-~a))/" branch-node-index
				;			   node-index))
				 (* (aref *delta-V* branch-node-index) (- (segment-g-axial segment)))
	;			 )
		;	 (progn (if *debug-hines* (format t ")/"))
				0.0))
		    ;)
		    (t
;		     (if *debug-hines* (format t " - (V-~a * U-~a))/" (+ node-index 1) node-index))
		     (* (aref *delta-V* (+ node-index 1))(aref *upper-diag* node-index)))))
	       (aref *diag* node-index)))
;      (if *debug-hines* (format t "A-~a~%" node-index))
      )))

(defun order-nodes-from-hines-branch-list (cell-name cell)
  (declare (ignore cell-name))
  (loop for branch in *branch-list* do
	(loop for element in branch do
	      (let ((nd (segment-node-2 element)))
		(format *debug-hines* "node ~a index = ~a ~%"   (node-name nd) 
			(setf (node-index nd) *num-unknowns*))
		(setf (node-ordered nd) t
		      (node-hines-circuit-node nd) t
		      (aref *node-array* *num-unknowns*) nd)
		(incf *num-unknowns*))))
  (let ((nd (soma-node (cell-soma cell))))
    (format  *debug-hines* "node ~a index = ~a ~%" (node-name nd)
	     (setf (node-index nd) *num-unknowns*)
	     (setf (node-ordered nd) t)
	     (setf (aref *node-array* *num-unknowns*) nd)
	     (incf *num-unknowns*)))
  ;;The proximal node of the proximal segment in each branch is a "branch node". Store the index of that node in
  ;;the segment structure. 
  (loop for branch in *branch-list* do
	(let ((element (car (last branch))))
	  (setf (segment-branch-node-index element) (node-index (segment-node-1 element))))))


(defun look-at-branch-list ()
  (loop for branch in  *branch-list* do
    (print 'NEW-BRANCH)
    (loop for segment in (reverse branch) do
      (print (segment-name segment))
      (format t " branch-node-index = ~a, " (segment-branch-node-index segment))
      (format t " index = ~a" (node-index (segment-node-2 segment))))))

;;; LOCATE-CELL-NODES Cell nodes are located seqentially from the soma outward.
(defun locate-cell-nodes (name cell)
  (declare (ignore name))
  ;;Make sure soma is located first.
  (setf (node-absolute-location (soma-node (cell-soma cell)))	
	(node-relative-location (soma-node (cell-soma cell)))
	*branch-list* '())
  ;;Start at the cell soma, and work on each of the branches that originate there in turn.
  (dolist (element (node-elements (soma-node (cell-soma cell))))	
    (if (eq (named-structure-symbol element) 'segment)
	(progn (locate-distal-node element)
	       (if *use-hines* (build-branch-list element))
	       (get-and-locate-distal-segments element)))))

;;; GET-AND-LOCATE-DISTAL-SEGMENTS
(defun get-and-locate-distal-segments (segment)
  ;;  (format t "finding segments distal to segment ~a ~%" (segment-name segment))
  (let ((distal-segment-list '())(nd (segment-node-2 segment)))
    (dolist (element (node-elements nd)) ;Collect all segments attached to this one's distal node.
      (if (and (eq (named-structure-symbol element) 'segment)
	       (eq nd (segment-node-1 element))) ;?Are both these tests necessary?
	  (setq distal-segment-list (nconc distal-segment-list (list element)))))
    (if distal-segment-list
	(dolist (segment distal-segment-list)
	  (locate-distal-node segment)
	  (if *use-hines* (build-branch-list segment))
	  (get-and-locate-distal-segments segment)))))

;;; LOCATE-DISTAL-NODE This function is called under the assumption that the location of the proximal node
;;; (node-1) has already been determined. This means that the segment node locations must be determined from the
;;; soma OUT.

;;; We have to deal with cells whose segments are defined in terms of their length and the phi and theta parameters
;;; relative to the segment proximal to them, and segments which are defined in term of the coordinates of their 
;;; nodes, relative to the soma. To distinguish these situtations when this function is called, in the former
;;; case, the segments' distal nodes' (node-relative-location distal-node) will be equal to '(0.0 0.0 0.0), whereas in
;;; the latter case this list will have  nonzero coordinates. In any event this function should result in the
;;; segments' distal nodes relative and absolute locations specified, as well as their length. It is not
;;; necessary to specify phi and theta for cells in which the coordinates are the defining parameters.

;;; This nonsense is to avoid accumulation of rounding errors.
;;;		 (setq phi-distal (acos (/ (round (* 100 (/ (- (third proximal-segment-end)
;;;							       (third proximal-segment-start))
;;;							    proximal-segment-length))) 100.0)))

(defun locate-distal-node (seg)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let* ((cell (segment-cell seg))
	 (distal-node (segment-node-2 seg))
	 (relative-location (node-relative-location distal-node))
	 (proximal-node (segment-node-1 seg))
	 (proximal-segment (find-proximal-segment proximal-node)) ;this finds the segment for which this
	 proximal-segment-proximal-node proximal-segment-start proximal-segment-end
	 (proximal-segment-length 0.0)
	 (phi-distal 0.0) (theta-distal 0.0)
	 (length (the single-float (segment-length seg)))
	 (theta (the single-float (segment-theta seg)))
	 (phi (the single-float (segment-phi seg))))
    (declare (single-float phi-distal theta-distal length theta phi proximal-segment-length))
    ;;    (format t "Proxi of seg ~a is ~a ~%" (segment-name seg) (if proximal-segment (segment-name proximal-segment)))
    (cond (proximal-segment		; If no proximal segment, then this segment comes from soma.
	   (setq proximal-segment-proximal-node (segment-node-1 proximal-segment))
	   (setq proximal-segment-start (node-relative-location proximal-segment-proximal-node))
	   (setq proximal-segment-end (node-relative-location proximal-node))
	   (setq proximal-segment-length (segment-length proximal-segment))
	   (if (equal relative-location '(0.0 0.0 0.0))
	       (progn
		 (setq phi-distal (acos (/ (- (the single-float (third proximal-segment-end))
					      (the single-float (third proximal-segment-start)))
					   proximal-segment-length)))
		 (setq theta-distal (if (= (the single-float (first proximal-segment-end))
					   (the single-float (first proximal-segment-start)))
					(* (signum (- (the single-float (second proximal-segment-end))
						      (the single-float (second proximal-segment-start))))
					   pi-over-2)
					(atan  (- (the single-float (second proximal-segment-end))
						  (the single-float (second proximal-segment-start)))
					       (- (the single-float (first proximal-segment-end))
						  (the single-float (first proximal-segment-start)))))))
	       (setf (segment-length seg)
		     (let ((sum 0.0))
		       (declare (single-float sum))
		       (dolist (i (mapcar '* (mapcar '- proximal-segment-end relative-location )
					  (mapcar '- proximal-segment-end relative-location)))
			 (setq sum (+ sum i)))
		       (sqrt sum)))))
	  ;; Branches originating from soma are assumed to be coming
	  ;; out from "branches" that lie in the xy plane.
	  (t (setq proximal-segment-end '(0.0 0.0 0.0))
	     (if (equal relative-location '(0.0 0.0 0.0))
		 (setq phi-distal pi-over-2 theta-distal 0.0)
		 (setf (segment-length seg)
		       (let ((sum 0.0))
			 (declare (single-float sum))
			 (dolist (i (mapcar '* (mapcar '- (node-relative-location distal-node)
						       (node-relative-location proximal-node))
					    (mapcar '- (node-relative-location distal-node)
						    (node-relative-location proximal-node))))
			   (declare (single-float sum i))
			   (setq sum (+ sum i)))
			 (sqrt sum))))))
    ;; See if relative location of distal node has been calculated - if not, do so.
    (if (equal relative-location '(0.0 0.0 0.0))
	(setf (node-relative-location distal-node)
	      (list
	       (realpart (+ (* length (sin (+ phi phi-distal)) (cos (+ theta theta-distal)))
			    (the single-float (first proximal-segment-end))))
	       (realpart (+ (* length (sin (+ phi phi-distal)) (sin (+ theta theta-distal)))
			    (the single-float (second proximal-segment-end))))
	       (realpart (+ (* length (cos (+ phi phi-distal)))
			    (the single-float (third proximal-segment-end)))))))
    (setf (node-absolute-location distal-node)
	  (list (realpart (+ (the single-float (first (cell-origin cell)))
			     (the single-float (first (node-relative-location distal-node)))))
		(realpart (+ (the single-float (second (cell-origin cell)))
			     (the single-float (second (node-relative-location distal-node)))))
		(realpart (+ (the single-float (third (cell-origin cell)))
			     (the single-float (third (node-relative-location distal-node)))))))))


;;; FIND-PROXIMAL-SEGMENT Returns the segment associated with this node for which the node is "node-2" for that
;;; segment.
(defun find-proximal-segment (nd)
  (let (seg)
    (dolist (element (node-elements nd))
    (if (and (eq  (named-structure-symbol element) 'segment)
	     (eq nd (segment-node-2 element)))
	 (setq seg element)))
    seg))

(defun find-distal-segments (seg)
  (loop for element in (node-elements (segment-node-2 seg))
	collect (if (and (eq (type-of element) 'segment)
			 (not (eq element seg)))
		    element)))



;; This function creates a core segment data structure. In the parallel version, it 
;; puts the device on the same processor as the fanout node, which is passed in as the 
;; variable 'proc. The serial version ignoseg this and allocates a struct normally.
;; This function only works if the 'nd argument is not a DC source node, because these
;; do not have processors allocated to them.

(defun create-core-segment (seg nd)
  "Creates the core struct for a segment."
  (let (core-seg (proc #-parallel nil #+parallel (allocate-processor)))
    #-parallel (declare (ignore proc))
    #+parallel (*setf (pref fanout-valid proc) t)
    #+parallel (*setf (pref fanout-seg-forward proc) nil)
    (if (segment-core-seg seg)			; 
	(setf core-seg (segment-core-seg seg))	; core segment has already been allocated
	(progn					; else allocate one
	  #-parallel (setf core-seg (make-core-segment))
	  #+parallel (setf core-seg proc)
	  #+parallel (*setf (pref *lisp-struct-type core-seg) core-segment)
	  (#+parallel *setf #-parallel setf	
	   (#+parallel pref #.core-segment-g-axial core-seg) (segment-g-axial seg)
	   (#+parallel pref #.core-segment-g-leak core-seg) (segment-g-leak seg)
	   (#+parallel pref #.core-segment-v-leak core-seg) (segment-v-leak seg)
	   (#+parallel pref #.core-segment-capacitance core-seg) (segment-capacitance seg))
	  (setf (segment-core-seg seg) core-seg)
	  ))
    (let ((node1 (segment-node-1 seg))
	  (node2 (segment-node-2 seg)))
      (cond
	((eq nd node1)
	 #-parallel
	 (setf
	   (#.core-segment-node-1-pointp core-seg) 1
	   (#.core-segment-node-1-point core-seg) (node-core-nd node1))
	 #+parallel
	 (*setf
	   (pref #.core-segment-node-1-pointp core-seg) 1
	   (pref #.core-segment-node-1-point core-seg) proc))
	((eq nd node2)
	 #-parallel
	 (setf
	   (#.core-segment-node-2-pointp core-seg) 1
	   (#.core-segment-node-2-point core-seg) (node-core-nd node2))
	 #+parallel
	 (*setf
	   (pref #.core-segment-node-2-pointp core-seg) 1
	   (pref #.core-segment-node-2-point core-seg) proc))
	(t
	 (sim-error "Internal error: called create-core on a device with a invalid node"))
	))))

(defun add-off-diag-segment (seg diag off-diag off-diag-entry)
  "Adds off diagonal entries for this segment."
  (declare (ignore diag))
  #+off-diag
  (let ((node1 (segment-node-1 seg))
	(node2 (segment-node-2 seg))
	(proc nil)
	(core-seg (segment-core-seg seg)))
    #-parallel (declare (ignore proc))
    (cond
      ((eq off-diag node1)
       #-parallel
       (setf
	 (#.core-segment-mat-21-valid core-seg) 1
	 (#.core-segment-mat-21-point core-seg) off-diag-entry)
       #+parallel
       (setf proc (allocate-processor))
       #+parallel
       (*setf	 (pref fanout-valid proc) t
		 (pref fanout-seg-forward proc) nil
		 (pref #.core-segment-mat-21-valid core-seg) t
		 (pref #.core-segment-mat-21-point core-seg) proc))
      ((eq off-diag node2)
       #-parallel
       (setf
	 (#.core-segment-mat-12-valid core-seg) 1
	 (#.core-segment-mat-12-point core-seg) off-diag-entry)
       #+parallel
       (setf proc (allocate-processor))
       #+parallel
       (*setf
	 (pref fanout-valid proc) t
	 (pref fanout-seg-forward proc) nil
	 (pref #.core-segment-mat-12-valid core-seg) t
	 (pref #.core-segment-mat-12-point core-seg) proc))
      )))

(defun get-segment-name (name seg)
  (declare (ignore seg)) (print name))

(defun get-segment-cell-name (name seg)
  (declare (ignore name)) (print (cell-name (segment-cell seg))))



(defun find-coupling-segment (nd seg)
  "Returns a pair of (node . value) telling how coupled that node is to nd."
	  (cond
    ((eq nd (segment-node-1 seg))
      (if (or (node-is-dc-source (segment-node-2 seg))
	      (node-is-pwl-source (segment-node-2 seg)))
	nil
	(cons (segment-node-2 seg) (segment-g-axial seg))))
    (t
      (if (or (node-is-dc-source (segment-node-1 seg))
	      (node-is-pwl-source (segment-node-1 seg)))
	nil
	(cons (segment-node-1 seg) (segment-g-axial seg))))))


(defun segment-fix-dc-nodes (seg)
  "Fix up the nodes of this element which are connected to dc nodes."
  (if (segment-core-seg seg)
      (progn
	(if (node-is-dc-source (segment-node-1 seg))
	  (#+parallel *setf #-parallel setf	
	      (#+parallel pref #.core-segment-node-1-pointp (segment-core-seg seg)) nil
	      (#+parallel pref #.core-segment-node-1-const (segment-core-seg seg))
	      (node-voltage (segment-node-1 seg))))
	(if (node-is-dc-source (segment-node-2 seg))
	  (#+parallel *setf #-parallel setf	
	      (#+parallel pref #.core-segment-node-2-pointp (segment-core-seg seg)) nil
	      (#+parallel pref #.core-segment-node-2-const (segment-core-seg seg))
	      (node-voltage (segment-node-2 seg)))))))

#-parallel
(defun get-segment-voltage-1 (seg)
  (if (#.core-segment-node-1-pointp seg)
      (core-node-voltage-n+1 (#.core-segment-node-1-point seg))
      (#.core-segment-node-1-const seg)))

#-parallel
(defun get-segment-voltage-2 (seg)
  (if (#.core-segment-node-2-pointp seg)
      (if (not *use-hines*)
	  (core-node-voltage-n+1 (#.core-segment-node-2-point seg))
	  (core-node-voltage-n (#.core-segment-node-2-point seg)))
      (#.core-segment-node-2-const seg)))

(proclaim '(single-float alpha))
#-parallel
(defun eval-segment (seg)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let* ((core-seg (segment-core-seg seg))
	 (v-diff 0.0)(v1 0.0)(v2 0.0)(i1 0.0)(i2 0.0)(alpha-cap 0.0))
    (declare (single-float v-diff v1 v2 i1 i2 alpha-cap))
    (if (null core-seg)	(return-from eval-segment (values)))
    (setf v2 (get-segment-voltage-2 core-seg))
    (if (not *use-hines*)
	(progn
	  (setf v1 (get-segment-voltage-1 core-seg))
	  (setf v-diff (- v1 v2))
	  (setf i1 (* v-diff (#.core-segment-g-axial core-seg)))
	  (setf i2 (+ (- i1)
		      (* (#.core-segment-g-leak core-seg)
			 (- v2 (#.core-segment-v-leak core-seg))))))
	(setf i2 (* (#.core-segment-g-leak core-seg)
		    -1 (#.core-segment-v-leak core-seg))))
    (setf alpha-cap (* alpha (#.core-segment-capacitance core-seg)))
    ;; Send the values back where they go
    (setf (core-node-jacobian (#.core-segment-node-1-point core-seg))
	  (+ (core-node-jacobian (#.core-segment-node-1-point core-seg))
	     (#.core-segment-g-axial core-seg)))
    (if (not *use-hines*)
	(setf (core-node-current (#.core-segment-node-1-point core-seg))
	      (+ (core-node-current (#.core-segment-node-1-point core-seg))
		 i1)
	      (core-node-charge (#.core-segment-node-2-point core-seg))
	      (+ (core-node-charge (#.core-segment-node-2-point core-seg))
		 (* v2 (#.core-segment-capacitance core-seg))))
	(setf (core-node-alpha-charge (#.core-segment-node-2-point core-seg))
	      (+ (core-node-alpha-charge (#.core-segment-node-2-point core-seg))
		 (* v2 alpha-cap))))
    (setf (core-node-current (#.core-segment-node-2-point core-seg))
	  (+ (core-node-current (#.core-segment-node-2-point core-seg))
	     i2)
	  (core-node-jacobian (#.core-segment-node-2-point core-seg))
	  (+ (core-node-jacobian (#.core-segment-node-2-point core-seg))
	     (+ (#.core-segment-g-axial core-seg)
		(#.core-segment-g-leak core-seg)
		alpha-cap)))
    #+off-diag
    (if (or (and (not *use-hines*) *use-tridiagonal*)
	    (and *use-hines* (= *real-time 0)))	;Need to do this once only for neuron simulation, since
					;off-diagonals don't change.
	(progn
	  (if (#.core-segment-mat-21-valid core-seg)
	      (let ((off-diag-entry (#.core-segment-mat-21-point core-seg))
		    (node2 (segment-node-2 seg)))
		(if  (core-off-diag-lower off-diag-entry)
		     (setf (aref *lower-diag* (node-index node2))
			   (- (aref *lower-diag* (node-index node2))
			      (#.core-segment-g-axial core-seg)))
		     (setf (aref *upper-diag* (node-index node2))
			   (- (aref *upper-diag* (node-index node2))
			      (#.core-segment-g-axial core-seg))))))
	  (if (#.core-segment-mat-12-valid core-seg)
	      (let ((off-diag-entry (#.core-segment-mat-12-point core-seg))
		    (node1 (segment-node-1 seg)))
		(if  (core-off-diag-lower off-diag-entry)
		     (setf (aref *lower-diag* (node-index node1))
			   (- (aref *lower-diag* (node-index node1))
			      (#.core-segment-g-axial core-seg)))
		     (setf (aref *upper-diag* (node-index node1))
			   (- (aref *upper-diag* (node-index node1))
			      (#.core-segment-g-axial core-seg))))))))))

#+parallel
(*defun get-segment-voltage-1 (v1)
  (cond!!
    (#.core-segment-node-1-pointp v1)
    (t!! #.core-segment-node-1-const)))

#+parallel
(*defun get-segment-voltage-2 (v2)
  (cond!!
    (#.core-segment-node-2-pointp v2)
    (t!! #.core-segment-node-2-const)))

#+parallel
(defun eval-segment ()
  (*select-type (core-segment)
    (*let
      ((v-diff (!! 0))
       (v2  (!! 0) )
       (i1 (!! 0) )
       (i2 (!! 0) ))
      (declare (type (pvar big-float) v-diff))
      (declare (type (pvar big-float) v2))
      (declare (type (pvar big-float) i1))
      (declare (type (pvar big-float) i2))

      ; get the voltages
      (*set v2 (get-segment-voltage-2 #.core-segment-node-2-voltage))
      (*set v-diff (-!! (get-segment-voltage-1 #.core-segment-node-1-voltage)
			v2))
    
      ; calculate the current
      (*set i1 (*!! v-diff #.core-segment-g-axial))
      (*set i2 (+!! (-!! i1) (*!! #.core-segment-g-leak
				  (-!! v2 #.core-segment-v-leak))))
    
      (*set core-device-node1-current i1)
      (*set core-device-node2-current i2)
      (*set core-device-node2-charge (*!! v2 #.core-segment-capacitance))

      (*set core-device-node1-conductance #.core-segment-g-axial)
      (*set core-device-node2-conductance (+!! #.core-segment-g-axial
					       #.core-segment-g-leak
					       (*!! (!! alpha) #.core-segment-capacitance)))
      )))





