;; -*- Mode: Lisp; Syntax: Common-lisp; Package: #-PARALLEL SURF #+PARALLEL *SURF; Base: 10; -*-
;;; (c) Copyright 1990, Lyle Borg-Graham, MIT Center for Biological Information Processing



#-parallel
(in-package "SURF-HIPPO"
	    :use '("COMMON-LISP-USER" "COMMON-LISP")
	    :nicknames '("SURF"))
#+parallel 
(in-package "*SURF")


;;; This is the cell file.

(defstruct cell-type			;not parallel	
  "The cell-types"
  (name "")
  (notes "")
  (cells '())				; a list of cells of this cell-type
  (soma-resistivity zero :type single-float)		;ohms square centimeter
  (soma-shunt 1e20 :type single-float)			;ohms 
  (membrane-resistivity zero :type single-float)		;ohms square centimeter
  (cytoplasmic-resistivity zero :type single-float)	;ohm centimeter
  model
  (specific-capacitance zero :type single-float)		;microfarads per square centimeter
  )


(defstruct cell				; not parallel
  "The data contained at a cell"
  (name "")
  soma					;every cell has a soma 
  (origin '())				;(x, y, z) of soma in brain coordinates.
  graphics-object			;So we can play around with the drawing easily.
  (nodes '())				; a list of nodes connected to this cell
  (segments '())			; a list of segments connected to this cell
  model
  type)

(defvar *soma-shunt 1e30)

;;CREATE-cell-type
(defun create-cell-type (cell-type-name &key (membrane-resistivity *r-mem)
					(cytoplasmic-resistivity *r-a)
					(soma-resistivity *r-mem-soma)
					(soma-shunt *soma-shunt)
					(specific-capacitance *cap-mem)
					(notes ""))
  "Creates a new cell type, if not already defined. Returns the pointer to the type."
  (let* ((cell-type (gethash cell-type-name cell-type-hash-table))
	 (model (gethash "cell-type" *model-instance-hash-table*)))
    (if cell-type
	cell-type
	(setf
	 cell-type (make-cell-type)
	 (cell-type-model cell-type) model
	 (cell-type-name cell-type) cell-type-name
	 (cell-type-notes cell-type) notes
	 (cell-type-soma-resistivity cell-type) soma-resistivity
	 (cell-type-soma-shunt cell-type) soma-shunt
	 (cell-type-membrane-resistivity cell-type) membrane-resistivity
	 (cell-type-cytoplasmic-resistivity cell-type) cytoplasmic-resistivity 
	 (cell-type-specific-capacitance cell-type) specific-capacitance 
	 (model-instance-elements model) (cons cell-type (model-instance-elements model))
	 (gethash cell-type-name cell-type-hash-table) cell-type))
    cell-type))



(defun print-cell-type (cell-type)
  "Prints out this data associated with a cell-type."
  (format *output-stream
	  "Cell-type ~a:~%  Rm ~d, Rm-sm ~d (ohm-cm2)~%  Soma shunt ~d ohms, Ra ~d ohm-cm, Cm ~d uF/cm2 ~%"
	  (cell-type-name cell-type)
	  (cell-type-membrane-resistivity cell-type)
	  (cell-type-soma-resistivity cell-type)
	  (cell-type-soma-shunt cell-type)
	  (cell-type-cytoplasmic-resistivity cell-type)
	  (cell-type-specific-capacitance cell-type))
  (format *output-stream (cell-type-notes cell-type))
  (format *output-stream " Cells of type ~a are: ~%"
	  (cell-type-name cell-type))
  (dolist (cell (cell-type-cells cell-type))
    (format *output-stream "        ~a~%" (cell-name cell)))
  (format *output-stream "~%")
  )



(defun print-cell (cell)
  "Prints out this data associated with a cell."
  (format *output-stream "Cell ~a of type ~a located at x = ~d , y = ~d, z =  ~d ~%"
	  (cell-name cell)
	  (cell-type-name (cell-type cell))
	  (first (cell-origin cell))
	  (second (cell-origin cell))
	  (third (cell-origin cell)))

;  (format *output-stream "    The segments associated with cell ~a are as follows: ~%"
;	  (cell-name cell)	  )
;  (dolist (seg (cell-segments cell))
;	       (format *output-stream "        ~a~%" (segment-name seg)))
;  (format *output-stream "~%~%")
;
;  (format *output-stream "    The nodes associated with cell ~a are as follows: ~%"
;	  (cell-name cell)	  )
;  (dolist (nd (cell-nodes cell))
;    (format *output-stream "        ~a~%" (node-name nd)))
  (format *output-stream "~%~%")
)
 

(defun collect-circuit-objects ()
  	     (collect-cell-types)
	     (collect-cells-nodes)
	     (collect-cells-segments)
	     (collect-cells-somas))

(defun collect-cell-types ()
  (maphash 'collect-cell-type cell-hash-table))

(defun collect-cell-type (name cell)
  (declare (ignore name))
  (let ((cell-type  (cell-type cell)))
    (if cell-type
	(setf (cell-type-cells cell-type)
	      (cons cell (cell-type-cells cell-type))))))

(defun collect-cells-nodes ()
  (maphash 'collect-cell-nodes node-hash-table))

(defun collect-cell-nodes (name nd)
  (declare (ignore name))
  (if (not (eq nd *ground-node*))
      (let ((cell (node-cell nd)))
	(setf (cell-nodes cell)
	      (cons nd (cell-nodes cell))))))

(defun collect-cells-segments ()
  (maphash 'collect-segments-cell segment-hash-table))

(defun collect-segments-cell (name seg)
  (declare (ignore name))
  (let ((cell (segment-cell seg)) )
    (setf (cell-segments cell)
	  (cons seg (cell-segments cell)))))

(defun collect-cells-somas ()
  (maphash 'collect-somas-cell soma-hash-table))

(defun collect-somas-cell (name soma)
  (declare (ignore name))
  (setf (cell-soma (soma-cell soma)) soma))



;;CREATE-cell
(defun create-cell (cell-name  &key cell-type-name (cell-origin '(0 0 0)))
  "Creates a new cell, if not already defined. Returns the pointer to the node."
  (let* ((cell (gethash cell-name cell-hash-table))
	 (cell-type (gethash cell-type-name cell-type-hash-table))
	 (model (gethash "cell" *model-instance-hash-table*)))
    (if cell
	cell
	(setf cell (make-cell)
	      (cell-name cell) cell-name
	      (cell-origin cell) cell-origin
	      (cell-model cell) model
	      (model-instance-elements model) (cons cell (model-instance-elements model))
	      (gethash cell-name cell-hash-table) cell
	      (cell-type cell) (if cell-type-name
				   (if cell-type
				       cell-type
				       (create-cell-type cell-type-name)))))
    cell))

;
;(first (node-cell-origin node))			;x origin
;(second (node-cell-origin node))		;y origin
;(third (node-cell-origin node))			;z origin
;
;(first (node-relative-location node))		;x relative to origin
;(second (node-relative-location node))		;y relative to origin
;(third (node-relative-location node))		;z relative to origin
;


(defun create-cell-model ()
  "Creates a template for all cells."
  (let ((template (make-model-template)))
    (setf
      (model-template-name template) (string "cell")
      (model-template-default-params template) '()
      (model-template-print-routine template) #'print-cell
      (model-template-create-routine template) #'create-cell

      (model-template-eval-routine template) #'null

      (model-template-create-core-routine template) #'null
      (model-template-add-off-diag-routine template) #'null
      (model-template-find-coupling-routine template) #'null
      (model-template-fix-dc-nodes-routine template) #'null


      *models* (cons template *models*)
      (gethash (string "cell") *model-hash-table*) template
      cell-hash-table (make-hash-table :test #'equal))
    (create-model-instance (string "cell") (model-template-name template) '() )))
	; only need one cell model instance, so create it now.

(defun create-cell-type-model ()
  "Creates a template for all cell-types."
  (let ((template (make-model-template)))
    (setf
      (model-template-name template) (string "cell-type")
      (model-template-default-params template) '()
      (model-template-print-routine template) #'print-cell-type
      (model-template-create-routine template) #'create-cell-type

      (model-template-eval-routine template) #'null
      (model-template-create-core-routine template) #'null
      (model-template-add-off-diag-routine template) #'null
      (model-template-find-coupling-routine template) #'null
      (model-template-fix-dc-nodes-routine template) #'null
      *models* (cons template *models*)
      (gethash (string "cell-type") *model-hash-table*) template
      cell-type-hash-table (make-hash-table :test #'equal))
    (create-model-instance (string "cell-type") (model-template-name template) '() )))
	; only need one cell-type model instance, so create it now.




