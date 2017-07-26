;; -*- Mode: Lisp; Syntax: Common-lisp; Package: SURF ; Base: 10; -*-
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


;;; SYS Source file: ntscable.lisp
(in-package "SURF-HIPPO")

;;; This file contains the functions for processing the SURF-HIPPO hack of NTSCABLE.

;;; The anatomy (lisp) files are generated by the following command:

;;; unix-prompt> ntscable  -x n input-anatomy-file output-lisp-file

;;; The executable "ntscable" file is found in the surf-hippo/ntscable/ntscable directory.

;;; Note: ntscable must be (re)compiled with the SURF-HIPPO hack of write.c (surf-hippo-write.c) and
;;; the associated makefile.


;;; The nts files sometimes have points with the same coordinates. We kill the resulting zero-length
;;; segments.

;;; ntscable assigns each point of the cell the original anatomical coordinates, which may be
;;; referenced to an arbitrary origin. SURF-HIPPO, on the other hand, assigns the origin of each
;;; cell to that cell's soma center. Thus, the NODE-RELATIVE-COORDINATES slots for segments is with
;;; respect to the soma origin. The NODE-ABSOLUTE-COORDINATES slot for the segments and the soma of
;;; a cell are calculated by adding the relative coordinates to the value in the cell's CELL-ORIGIN
;;; slot. Since ntscable assigns essentially absolute coordinates to the cell elements, SURF-HIPPO
;;; adjusts these coordinates, according the location of the soma as defined by ntscable, so that
;;; the soma relative coordinate is at the origin, and everthing in the cell is referenced from
;;; there, as explained.





;;; This variable is setq'd by the file generated by the 
(defvar *ntscable-list*)



;;; These can be setq'd in the file produced by NTSCABLE.

(defvar *nts-radius* 0.0)		; NTSCABLE writes a setq form for this var.

(defvar *nts-cell-type-notes* "")
(defvar *nts-cell-type* "")
(defvar *nts-cell-name* "")
(defvar *nts-r-mem* 40000.0)		;membrane resistivity
(defvar *nts-soma-r-mem* 40000.0)	;soma membrane resistivity
(defvar *nts-r-a* 200.0)		;cytoplasmic resistivity
(defvar *delete-zero-length-segments* t)
(defvar *ntscable-fudge-component* 0)
(defvar *ntscable-fudge-value* 1.05)


(defun process-ntscable-list (&optional (cell-name *nts-cell-name*))
  "Main function for processing ntscable lisp files."
  (nts-cell cell-name))


;;; NTS-CELL This is the compiled function that runs the NTSCABLE generated cell.
(defun nts-cell (&optional (cell-name *nts-cell-name*))
  (when *next-cell-name* (setq cell-name *next-cell-name*))
  (setq *nts-cell-type* (when *nts-cell-type* (string *nts-cell-type*)))
  (create-cell-type (if (> (length *nts-cell-type*) 0) *nts-cell-type* "ntscable")
		    :notes *nts-cell-type-notes*
		    :membrane-resistivity *nts-r-mem*
		    :soma-resistivity *nts-soma-r-mem*
		    :cytoplasmic-resistivity *nts-r-a*)
  (let* ((*nts-cell-name* (or (check-cell-name cell-name) "ntscable"))
	 (*add-cell-name-to-segs*
	  (> (length *loaded-circuit-parts*) 1)
					;(not (equal cell-name *nts-cell-name*))
	   ))
    (translate-ntscable-list
     (create-soma
      :cell (create-cell *nts-cell-name* :cell-type *nts-cell-type*)
      :diameter (* 2 *nts-radius*)
      :parameters
      (concatenate
       'list
       (when *soma-outline*
	 (list (cons 'soma-outline (loop for soma-point in *soma-outline* collect (coerce-location-to-float soma-point)))))
       (when *soma-points*
	 (list (cons 'soma-points
		     (loop for soma-point in *soma-points* collect (coerce-location-to-float soma-point))))))))))

#|
(defvar *ntscable-segment-name-array* (make-array '(1 1 1) :adjustable t))
(proclaim '(notinline construct-nts-segment-name))
(defun construct-nts-segment-name (segment-index-list)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (unless *ntscable-segment-name-array*
    (setq *ntscable-segment-name-array* (make-array '(1 1 1) :adjustable t)))
  (let ((*print-pretty*))
    (if *add-cell-name-to-segs*
	(concatenate-strings
	 (the simple-base-string *nts-cell-name*) "_"
	 (princ-to-string (the fn (car segment-index-list))) "-"
	 (princ-to-string (the fn (cadr segment-index-list))) "-"
	 (princ-to-string (the fn (caddr segment-index-list))))
	(let* ((first (the fn (car segment-index-list)))
	       (second (the fn (cadr segment-index-list)))
	       (third (the fn (caddr segment-index-list)))
	       (old-first (1- (array-dimension (the (simple-array * (* * *)) *ntscable-segment-name-array*) 0)))
	       (old-second (1- (array-dimension (the (simple-array * (* * *)) *ntscable-segment-name-array*) 1)))
	       (old-third (1- (array-dimension (the (simple-array * (* * *)) *ntscable-segment-name-array*) 2)))
	       (new-first (when (> first old-first) first))
	       (new-second (when (> second old-second) second))
	       (new-third (when (> third old-third) third)))
	  (declare (fixnum first second third old-first old-second old-third))
	  (when (or new-first new-second new-third)
	    (adjust-array *ntscable-segment-name-array* (list (1+ (the fn (or new-first old-first)))
							      (1+ (the fn (or new-second old-second)))
							      (1+ (the fn (or new-third old-third))))))
	  (let ((old-entry (aref *ntscable-segment-name-array* first second third)))
	    (typecase old-entry
	      (string old-entry)
	      (t
	       (setf (aref *ntscable-segment-name-array* first second third)
		     (concatenate-strings
		      (princ-to-string first) "-"
		      (princ-to-string second) "-"
		      (princ-to-string third))))))))))



(defvar *ntscable-segment-name-table-count* 0)

|#

(proclaim '(inline construct-nts-segment-name))
(defun construct-nts-segment-name (segment-index-list)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((*print-pretty*))
    (if *add-cell-name-to-segs*

	(concatenate-strings
	 (the simple-base-string *nts-cell-name*) "_"
	 (princ-to-string (the fn (car segment-index-list))) "-"
	 (princ-to-string (the fn (cadr segment-index-list))) "-"
	 (princ-to-string (the fn (caddr segment-index-list))))
	
	(concatenate-strings
	 (princ-to-string (the fn (car segment-index-list))) "-"
	 (princ-to-string (the fn (cadr segment-index-list))) "-"
	 (princ-to-string (the fn (caddr segment-index-list)))))))

(defvar *ntscable-segment-name-table* nil)

(proclaim '(inline construct-nts-segment-name))
(defun construct-nts-segment-name (segment-index-list)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (cons segment-index-list))
  (let ((*print-pretty*))
    (if *add-cell-name-to-segs*
	(concatenate-strings
	 (the simple-base-string *nts-cell-name*) "_"
	 (princ-to-string (the fn (car segment-index-list))) "-"
	 (princ-to-string (the fn (cadr segment-index-list))) "-"
	 (princ-to-string (the fn (caddr segment-index-list))))
	(let ((old-entry (gethash segment-index-list *ntscable-segment-name-table*)))
	  (typecase old-entry
	    (string old-entry)
	    (t
	     (let ((first (the fn (car segment-index-list)))
		   (second (the fn (cadr segment-index-list)))
		   (third (the fn (caddr segment-index-list))))
	       (setf (gethash segment-index-list *ntscable-segment-name-table*)
		     (concatenate-strings
		      (princ-to-string first) "-"
		      (princ-to-string second) "-"
		      (princ-to-string third))))))))))







(defun extract-neurite (nts-segment-name)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((string (the (simple-array * (*)) nts-segment-name))
	VALUE)
    (setq string  (string-remove-tail string (- (length string) (find-tail-- string))))
    (SETQ VALUE (parse-integer (string-remove-tail  string (- (length string) (find-tail-- string)))))
    VALUE))
	 

(defun extract-section (nts-segment-name)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((string (the (simple-array * (*)) nts-segment-name))
	VALUE)
    (setq string (string-remove-tail string (- (length string) (find-tail-- string))))
    (setq value (parse-integer (string-remove-head string (1+ (find-tail-- string)))))
    value))



#|
(defun translate-ntscable-list (soma)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let* ((soma-center-correction (soma-center-correction soma))
	 (cell (soma-cell soma))
	 (cell-name (cell-name cell))
	 (*print-pretty* nil)
	 (temp
	  (loop for segment-list in *ntscable-list*
		when (eq (nth 1 segment-list) 'soma) ; ((1 1 0)  SOMA  ( -12.6   55.7  -20.2) 3.5)
		collect (cons (construct-nts-segment-name (nth 0 segment-list))
			      (cddr segment-list))
		;; ("1-1-0" ( -12.6   55.7  -20.2) 3.5)
		into section-proximal-ends-soma-list
		else
					; ((1 2 0) (BRANCH-PT 1 1 9)  (  -2.5   -140   -2.5) 2.3)
		when (eq (car (nth 1 segment-list)) 'branch-pt) 
		collect (list (construct-nts-segment-name (nth 0 segment-list))
			      (construct-nts-segment-name (cdadr segment-list)))
		;; ("1-2-0" "1-1-9")
		into section-proximal-ends-bp-list
		finally (return (list section-proximal-ends-soma-list section-proximal-ends-bp-list))))
	 (section-proximal-ends-soma-list (nth 0 temp))
	 (section-proximal-ends-bp-list (nth 1 temp))
	 (segments
	  (loop for segment-list in *ntscable-list*
		when (not (or (eq (nth 1 segment-list) 'soma) (eq (car (nth 1 segment-list)) 'branch-pt)))
		;;   ((1 10 5) (1 10 4)  ( -79.5 -197.5   31.5) 0.3)
		collect
		(let* ((proximal-node-name (construct-nts-segment-name (nth 1 segment-list)))
		       (diameter (if (integerp (nth 3 segment-list))
				     (coerce (the fn (nth 3 segment-list)) 'single-float)
				     (nth 3 segment-list)))
		       (soma-proximal-location-and-diameter 
			(loop for section-proximal-end-soma in section-proximal-ends-soma-list
			      ;; ("1-1-0" ( -12.6   55.7  -20.2) 3.5)
			      when (if (and proximal-node-name (car section-proximal-end-soma))
				       (string=
					(the simple-base-string (car section-proximal-end-soma))
					(the simple-base-string proximal-node-name)))
			      do (return (cdr section-proximal-end-soma))))
		       (bp-proximal-location-name 
			(unless soma-proximal-location-and-diameter
			  (loop for section-proximal-end-bp in section-proximal-ends-bp-list
				;; ("1-2-0" "1-1-9")
				when (if (and proximal-node-name (car section-proximal-end-bp))
					 (string=
					  (the simple-base-string (car section-proximal-end-bp))
					  (the simple-base-string proximal-node-name)))
				do (return (cadr section-proximal-end-bp))))))
		  (declare (single-float diameter))
		  (when soma-proximal-location-and-diameter
		    ;; Average the diameters if abutting the soma
		    (setq diameter
			  (the sf
			       (* 0.5 (the sf
					   (+ (nth 1 soma-proximal-location-and-diameter)
					      diameter))))))
					;	       (break)
		  (create-segment-fast (construct-nts-segment-name (nth 0 segment-list))
				       (cond
					 (soma-proximal-location-and-diameter (soma-name soma))
					 (bp-proximal-location-name bp-proximal-location-name)
					 (t (construct-nts-segment-name (nth 1 segment-list))))
				       cell
				       :relative-location
				       (loop for val1 in soma-center-correction
					     for val2 in (nth 2 segment-list)
					     collect (the sf (- (the (or sf fn) val2) (the sf val1))))
				       :diameter diameter
				       :dummy-proximal-node-location
				       (if (car soma-proximal-location-and-diameter)
					   (loop for val1 in (car soma-proximal-location-and-diameter)
						 for val2 in soma-center-correction
						 collect (the sf (- (the (or sf fn) val1) (the sf val2))))))))))
    (setq *ntscable-list* nil)
    ;; Check for zero length segments, and destroy them.
    (destroy-zero-length-segments segments)
    ))
|#

#|
(loop for elt in  *ntscable-list*
      collect (car elt) into out
      collect (nth 1 elt) into out
      finally
      (format t "out length ~A, keys ~A~%"
	      (length out)
	      (length (delete-duplicates out :test 'equal))))
|#

      
(defun translate-ntscable-list (soma)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (setq *ntscable-segment-name-table* (Make-hash-table :test 'equal))
  (let* ((soma-center-correction (soma-center-correction soma))
	 (cell (soma-cell soma))
	 (*print-pretty* nil))
    (loop for segment-list in *ntscable-list*
	  when (eq (nth 1 segment-list) 'soma) ; ((1 1 0)  SOMA  ( -12.6   55.7  -20.2) 3.5)
	  collect (cons (construct-nts-segment-name (nth 0 segment-list))
			(cddr segment-list))
	  ;; ("1-1-0" ( -12.6   55.7  -20.2) 3.5)
	  into section-proximal-ends-soma-list
	  else
					; ((1 2 0) (BRANCH-PT 1 1 9)  (  -2.5   -140   -2.5) 2.3)
	  when (eq (car (nth 1 segment-list)) 'branch-pt) 
	  collect (list (construct-nts-segment-name (nth 0 segment-list))
			(construct-nts-segment-name (cdadr segment-list)))
	  ;; ("1-2-0" "1-1-9")
	  into section-proximal-ends-bp-list
	  finally
;; 	  (format t "section-proximal-ends-bp-list ~A~%" section-proximal-ends-bp-list)
;;	  (break)
	  (destroy-zero-length-segments
	   (loop for segment-list in *ntscable-list*
		 when (not (or (eq (nth 1 segment-list) 'soma) (eq (car (nth 1 segment-list)) 'branch-pt)))
		 ;;   ((1 10 5) (1 10 4)  ( -79.5 -197.5   31.5) 0.3)
		 collect
		 (let* ((proximal-node-name (construct-nts-segment-name (nth 1 segment-list)))
			(diameter (if (integerp (nth 3 segment-list))
				      (coerce (the fn (nth 3 segment-list)) 'single-float)
				      (nth 3 segment-list)))
			(soma-proximal-location-and-diameter 
			 (loop for section-proximal-end-soma in section-proximal-ends-soma-list
			       ;; ("1-1-0" ( -12.6   55.7  -20.2) 3.5)
			       when (if (and proximal-node-name (car section-proximal-end-soma))
					(string=
					 (the simple-base-string (car section-proximal-end-soma))
					 (the simple-base-string proximal-node-name)))
			       do (return (cdr section-proximal-end-soma))))
			(bp-proximal-location-name 
			 (unless soma-proximal-location-and-diameter
			   (loop for section-proximal-end-bp in section-proximal-ends-bp-list
				 ;; ("1-2-0" "1-1-9")
				 when (if (and proximal-node-name (car section-proximal-end-bp))
					  (string=
					   (the simple-base-string (car section-proximal-end-bp))
					   (the simple-base-string proximal-node-name)))
				 do (return (cadr section-proximal-end-bp))))))
		   (declare (single-float diameter))
		   (when soma-proximal-location-and-diameter
		     ;; Average the diameters if abutting the soma
		     (setq diameter
			   (the sf
				(* 0.5 (the sf
					    (+ (nth 1 soma-proximal-location-and-diameter)
					       diameter))))))
					;	       (break)
		   (let ((seg-name (construct-nts-segment-name (nth 0 segment-list))))
			  
		     (create-segment	; -fast
		      seg-name
		      (cond
			(soma-proximal-location-and-diameter (soma-name soma))
			(bp-proximal-location-name bp-proximal-location-name)
			(t (construct-nts-segment-name (nth 1 segment-list))))
		      cell
		      :relative-location
		      (loop for val1 in soma-center-correction
			    for val2 in (nth 2 segment-list)
			    collect (the sf (- (the (or sf fn) val2) (the sf val1))))
		      :diameter diameter
		      :dummy-proximal-node-location
		      (when (car soma-proximal-location-and-diameter)
			  (loop for val1 in (car soma-proximal-location-and-diameter)
				for val2 in soma-center-correction
				collect (the sf (- (the (or sf fn) val1) (the sf val2))))))))))
	  (setq *ntscable-list* nil))))




	  

;;;
;;;(defun add-on-distal-segment-and-length-for-neurite-section (current-segment neurite-section-seg-length-list)
;;;  (let ((last-segment  (nth 0 (first neurite-section-seg-length-list)))
;;;        (length-so-far (nth 1 (first neurite-section-seg-length-list)))
;;;        (distal-segment-from-current-neurite-section
;;;         (get-distal-segment-from-current-neurite-section current-segment)))
;;;    (push (cons (segment-name current-segment)
;;;                (+ length-so-far 
;;;                   (cartesian-distance-3d (segment-relative-location last-segment)
;;;                                          (segment-relative-location current-segment))))
;;;          neurite-section-seg-length-list)
;;;    (if distal-segment-from-current-neurite-section
;;;        (add-on-distal-segment-and-length-for-neurite-section
;;;         distal-segment-from-current-neurite-section neurite-section-seg-length-list)
;;;	neurite-section-seg-length-list)))




;;;(defun get-distal-segment-from-current-neurite-section (current-segment)
;;;  (loop for seg in (distal-segments current-segment)
;;;        when (and (eq (extract-neurite (segment-name seg))
;;;                      (extract-neurite (segment-name current-segment)))
;;;                  (eq (extract-section (segment-name seg))
;;;                      (extract-section (segment-name current-segment))))
;;;        do (return seg)))

;;;(defun translate-ntscable-list (soma)
;;;  (declare (optimize (safety 0) (speed 3) (space 1)))
;;;  (let ((tree '())
;;;        (position-list '())
;;;                                        ;       (nudge-flag nil)
;;;        )
;;;    (do ((segment-list *ntscable-list* (cdr segment-list)))
;;;        ((null segment-list))
;;;      (let* ((segment (car segment-list))
;;;             (this-segment (first segment))
;;;             (segment-neurite (the fn (first this-segment)))
;;;             (segment-section (the fn (second this-segment)))
;;;             (parent-segment (second segment))
;;;             (relative-pos (third segment))
;;;             (diameter (the sf (fourth segment)))
;;;             (root-seg nil)
;;;             par-segment-neurite par-segment-section par-segment-index)
;;;        (if (not (equal parent-segment 'soma))
;;;            (if (equal (car parent-segment) 'branch-pt)
;;;                (progn                  ;(print segment)
;;;                  (setq par-segment-neurite
;;;                        (first parent-segment)
;;;                                        ;(second parent-segment)
;;;                        par-segment-section
;;;                                        ;(1+ (the fn (third parent-segment)))
;;;                        (the fn (second parent-segment))
;;;                        ;; Translate the origin in the root segment parameter generated by
;;;                        ;; NTSCABLE back to the actual segment section.
;;;                        par-segment-index
;;;                        (third parent-segment)
;;;                                        ;(1+ (nth 4 parent-segment)) 
;;;                                        ; (find-index tree parent-segment)
;;;
;;;                        root-seg t))    ;this flag is used below to avoid making an extra segment.
;;;                (setq par-segment-neurite (first parent-segment)
;;;                      par-segment-section (second parent-segment)
;;;                      par-segment-index (third parent-segment))))
;;;        (setq segment (list (first segment)
;;;                            (if (equal parent-segment 'soma)
;;;                                'soma
;;;                                (list par-segment-neurite
;;;                                      par-segment-section
;;;                                      par-segment-index))
;;;                            relative-pos
;;;                            diameter
;;;                            root-seg))
;;;            
;;;        (if (> segment-neurite (length tree))
;;;            (push (list (list (cons segment 0.0))) tree)
;;;            (let ((segment-neurite-list (the cons (nth (- (length tree) segment-neurite) tree))))
;;;              (if (> segment-section (length segment-neurite-list))
;;;                  (push (list (cons segment 0.0))
;;;                        (nth (- (length tree) segment-neurite) tree))
;;;                  (let ((length (+ (the sf
;;;                                        (cartesian-distance-3d-float
;;;                                         relative-pos
;;;                                         (third (caar (nth (- (length segment-neurite-list)
;;;                                                              segment-section) segment-neurite-list)))))
;;;                                   (the sf
;;;                                        (cdar (nth (- (length segment-neurite-list) segment-section)
;;;                                                   segment-neurite-list))))))
;;;                    (push (cons segment length)
;;;                          (nth (- (length segment-neurite-list) segment-section)
;;;                               (nth (- (length tree) segment-neurite) tree)))))))))
;;;    (do ((neurite tree (cdr neurite)))
;;;        ((null neurite))
;;;      (do ((segment-list (car neurite) (cdr segment-list)))
;;;          ((null segment-list))
;;;        (let* ((segment (car segment-list))
;;;               (root-seg
;;;                (loop for section in segment ; find the root of the branch
;;;                      when (nth 4 (car section))
;;;                      do (return section))))
;;;          (do ((section-list segment (cdr section-list)))
;;;              ((null section-list))
;;;            (let ((section (caar section-list)))
;;;                                        ;             (print section)
;;;                                        ;             (break)
;;;              (if (not (nth 4 section)) ; don't generate a segment for the branch point.
;;;                  (let* ((this-section-name-list (nth 0 section))
;;;                         (par-section-name-list (nth 1 section))
;;;                         (relative-position (nth 2 section))
;;;                         ;;                        (original-relative-position relative-position)
;;;                         (diameter (nth 3 section)))
;;;                                        ;                   (break)
;;;                                        ;                   (print section)
;;;                    ;; Check if this position is the same as another segment.
;;;                    (if *CHECK-POSITIONS*
;;;                        (progn
;;;                          (setq position-list
;;;                                (nudge-position relative-position position-list *ntscable-fudge-component*))
;;;                          ;; nudge-position puts the maybe modified relative-position on the
;;;                          ;; front of the position-list.
;;;                          (setq relative-position (car position-list))))
;;;                                        ;                   (print root-seg)
;;;                    ;; The root seg must be specified.
;;;                    (if (equal par-section-name-list (nth 0 (car root-seg)))
;;;                        (setq par-section-name-list (nth 1 (car root-seg))))
;;;                              
;;;                    (let ((this-seg-name 
;;;                           (format nil "~d-~d-~d" (nth 0 this-section-name-list)
;;;                                   (nth 1 this-section-name-list)
;;;                                   (nth 2 this-section-name-list)))
;;;                          (parent-elt-name
;;;                           (if (equal par-section-name-list 'soma)
;;;                               (soma-name soma)
;;;                               (format nil "~d-~d-~d" (nth 0 par-section-name-list)
;;;                                       (nth 1 par-section-name-list)
;;;                                       (nth 2 par-section-name-list)))))
;;;                                        ;                     (break)
;;;                      (create-segment this-seg-name parent-elt-name      
;;;                                      (cell-name (soma-cell soma))
;;;                                      :relative-location relative-position
;;;                                      :diameter diameter)
;;;                      ;;      (if (not (eql original-relative-position relative-position))
;;;                      ;;      (format t "Nudged section ~a from ~a to ~a (~a um)~%"
;;;                      ;;      this-seg-name original-relative-position relative-position
;;;                      ;;      (cartesian-distance-3d original-relative-position
;;;                      ;;      relative-position) ) )
;;;
;;;                      ))))
;;;
;;;            )))))
;;;  (setq  *ntscable-list* nil)
;;;  (if (not *check-positions*)
;;;      (check-zero-length-segments soma))
;;;  )

;;;(defun check-zero-length-segments (soma)
;;;  (dolist (element (node-elements (soma-node soma)))
;;;    (if (eq (named-structure-symbol element) 'segment)
;;;        (progn (push-away-from-parent (node-relative-location (soma-node soma))
;;;                                      element)
;;;               (push-away-segment-children element)))))

;;;(defun push-away-segment-children (segment)
;;;  (loop for distal-segment in (distal-segments segment)
;;;        do
;;;        (push-away-from-parent (node-relative-location (segment-node-2 segment)) distal-segment)
;;;        (push-away-segment-children distal-segment)))

;;;(defun push-away-from-parent (parent-location child)
;;;  (let ((child-node
;;;         (case (type-of child)
;;;           (segment (segment-node-2 child))
;;;           (soma (soma-node child)))))
;;;    (if (equal parent-location (node-relative-location child-node))
;;;        (setf (node-relative-location child-node)
;;;              (loop for i from 0 to 2
;;;                    for comp in (node-relative-location child-node)
;;;                    collecting
;;;                    (if (= i *ntscable-fudge-component*)
;;;                        (+ *ntscable-fudge-value* comp)
;;;                        comp))))))
	    
	     
	    
;;;;;;(defvar *check-positions* nil)
;;;
;;;;; a little recursivity to nudge a position until it's special.
;;;(defun nudge-position (relative-position position-list ntscable-fudge-component)
;;;  (if (find relative-position position-list :test 'equal)
;;;      ;; nudge it
;;;      (let ((relative-position
;;;             (loop for i from 0 to 2
;;;                   for comp in relative-position
;;;                   collecting
;;;                   (if (= i ntscable-fudge-component)
;;;                       (+ *ntscable-fudge-value* comp)
;;;                       comp))))
;;;        (setq position-list
;;;              (nudge-position relative-position
;;;                              position-list
;;;                              (mod (1+ ntscable-fudge-component) 3))))
;;;      ;; Store the position, it is OK.
;;;      (push relative-position position-list))
;;;  position-list)


;;;(defun find-index (tree parent-segment)
;;;  (let* ((par-neurite-list  (nth (- (length tree) (second parent-segment)) tree))
;;;         (par-segment-list (nth (- (length par-neurite-list) (1+ (third parent-segment)))
;;;                                par-neurite-list))
;;;         (target-length (* (nth 3 parent-segment)
;;;                           (cdr (first par-segment-list))))
;;;         (best-match nil)
;;;         (closest-length nil))
;;;;    (print parent-segment)
;;;;    (print par-segment-list)
;;;;    (print (cdr (first par-segment-list)))
;;;;    (print (* (car (last parent-segment))
;;;;             (cdr (first par-segment-list))))
;;;    (loop for section in par-segment-list
;;;          do  ;(print (cdr section))
;;;          when (or (not closest-length)
;;;                   (< (abs (- (cdr section) target-length))
;;;                      closest-length))
;;;          do (setq closest-length (abs (- (cdr section) target-length))
;;;                   best-match section)
;;;          finally (return best-match))
;;;;    (print best-match)
;;;    (nth 2 (caar best-match))))

