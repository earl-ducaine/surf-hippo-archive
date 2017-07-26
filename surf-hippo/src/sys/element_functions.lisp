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


;;; SYS Source file: element_functions.lisp

(in-package "SURF-HIPPO")


(defun pre-synaptic-element (element)
  "Return the pre-synaptic cell element of ELEMENT."
  (let ((element (element element)))
    (typecase element
      (channel (channel-pre-synaptic-element element))
      (synapse (synapse-pre-synaptic-element element)))))

(defun update-type-from-definition (element)
  "Updates the ELEMENT type from the most recently loaded library definition. Note that change in
ELEMENT type does not necessarily propogate to elements of that type."
  (let* ((type (element-type element))
	 (type-model (element-model type)))
      ;; The following types have create routines with args:
      ;; (type-symbol &optional actual-type-symbol update-parameters)
      ;; 
      ;; axon-type synapse-type channel-type particle-type conc-particle-type buffer-type pump-type
      ;; conc-int-type, cell-type
      
      (funcall (model-create-routine type-model) type nil t)))



;; Functions for evaluating, transfering, and manipulating cell elements.

(defun elements ()
  "A list of all the elements of type given in *OBJECT-TYPE-SYMBOLS*."
  (loop for sym in *object-type-symbols*
	nconc 
	(hash-table-list (model-HASH-TABLE (type-symbol-model sym)))))


(defun element-type-p (element &optional type)
  "Predicate whether ELEMENT of TYPE is a circuit element type, for example BUFFER-TYPE, PUMP-TYPE, CHANNEL-TYPE,
CONC-INT-TYPE, PARTICLE-TYPE, CONC-PARTICLE-TYPE, SYNAPSE-TYPE, AXON-TYPE, or CELL-TYPE."
  (let ((type-sym (type-of (element element type))))
    (loop for sym in *type-model-symbols*
	  when (eq type-sym sym)
	  do (return t))))

(defun element-model (elt &optional type)
  (let ((type (or type
		  (if (member elt *object-type-symbols*)
		    elt
		    (type-of elt))
		  (case elt
		    ((vsource isource "vsource" "isource") elt)
		    (t (type-of (element elt type)))))))
    (type-symbol-model type)))


(defun type-symbol-child-string (type-symbol)
  (case type-symbol
    (soma "soma")
    (segment "segment")
    (axon-type "axon")
    (cell-type "cell")
    (particle-type "particle")
    (synapse-type "synapse")
    (conc-int-type "conc-int")
    (conc-particle-type "conc-particle")
    (channel-type "channel")
    (pump-type "pump")
    (buffer-type "buffer")))

(defun type-symbol-child-symbol (type-symbol)
  (read-from-string (type-symbol-child-string type-symbol)))

(defun type-symbol-string (type-symbol)
  (string-downcase (replace-char-w-space-in-string (string type-symbol) #\- )))

(defun type-symbol-model (type-symbol)
  (let ((type-symbol (typecase type-symbol
		       (string (read-from-string type-symbol))
		       (t type-symbol))))
    (get type-symbol 'model)))

(defun type-symbol-parent-model (type-symbol)
  (let ((orig-model (type-symbol-model type-symbol)))
    (or (model-parent-model orig-model) orig-model)))
  
(defun retrieve-model-parameters-from-library (type-symbol model)
  (copy-alist (get-a-value type-symbol (MODEL-PARAMETER-type-LIBRARY model) 'equal)))

(defun library-catalog (element)
  "Returns a list of all type symbols in the type library referenced by ELEMENT. Thus, if ELEMENT is
a channel, then the symbols of all library channel types are returned. ELEMENT can also be a symbol
for the element class, e.g. 'channel or 'channel-type."
  (let ((type-symbol-models
	 (coerce-to-list
	  (cond ((and (symbolp element) (type-symbol-model element))
		 (type-symbol-parent-model element))
		(t (loop for type in (coerce-to-list (element-type element))
			 collect ; nconc
			 (type-symbol-model (type-of type))))))))
    (loop for type-symbol-model in type-symbol-models nconc
	  (loop for entry in
		(MODEL-PARAMETER-type-LIBRARY type-symbol-model)
		collect (car entry)))))

;; NAME-ELEMENT Return an element with name NAME, constrained to type TYPE, if TYPE included.
(defun name-element (name &optional type)
  (let ((type (if (eq type 'electrode) 'segment type))
	elt)
    (when name
      (loop for model-name in *OBJECT-TYPE-SYMBOL-STRINGS*
					;	    do (format t "model-name: ~A~%" model-name)
					;	    (setq elt (model-hash-table (gethash model-name *model-hash-table* )))
					; do	    (format t "elt ~A~%" elt)
	    do (setq elt (gethash name (model-hash-table (gethash model-name *model-hash-table*))))
	    when (and
					; (not (string= (model-name model) "node"))
		  elt
		  (progn  ; (format t "elt ~A type ~A type-of ~A ~%" elt type (type-of elt))
			  (or (not type) (equal type (type-of elt)))))
	    do (return elt)))))


(defun name-element (name &optional type)
  (let ((type (if (eq type 'electrode) 'segment type))
	elt)
    (when name
      (loop for model-name in *OBJECT-TYPE-SYMBOL-STRINGS*
	    do (setq elt (gethash name (model-hash-table (gethash model-name *model-hash-table*))))
	    when (and
		  elt
		  (or (not type) (equal type (type-of elt))))
	    do (return elt)))))

(defun name-element (name &optional type)
  (let ((type (if (eq type 'electrode) 'segment type))
	elt)
    (when name
      (loop for model-sym in *OBJECT-TYPE-SYMBOLS*
	    do (setq elt (gethash name (model-hash-table (get model-sym 'model))))
	    when (and
		  elt
		  (or (not type) (equal type (type-of elt))))
	    do (return elt)))))



;; TYPE is a symbol or a string.
(defun get-type-hash-table (type &optional try-erzatz-table-name)
  (let ((type (typecase type
		(string (READ-FROM-STRING type))
		(symbol type))))
    (case type
      (node (NODE-HASH-TABLE))
      (segment (SEGMENT-HASH-TABLE))
      (soma (SOMA-HASH-TABLE))
      (cell (CELL-HASH-TABLE))
      (cell-type (CELL-TYPE-HASH-TABLE))
      (particle (particle-hash-table))
      (particle-type (PARTICLE-TYPE-HASH-TABLE))
      (conc-particle (CONC-PARTICLE-HASH-TABLE))
      (conc-particle-type (CONC-PARTICLE-TYPE-HASH-TABLE))
      (conc-int (CONC-INT-HASH-TABLE))
      (conc-int-type (CONC-INT-TYPE-HASH-TABLE))
      (synapse (SYNAPSE-HASH-TABLE))
      (synapse-type (SYNAPSE-TYPE-HASH-TABLE))
      (channel (CHANNEL-HASH-TABLE))
      (channel-type (CHANNEL-TYPE-HASH-TABLE))
      (axon (AXON-HASH-TABLE))
      (axon-type (AXON-TYPE-HASH-TABLE))
      (isource (ISOURCE-HASH-TABLE))
      (vsource (VSOURCE-HASH-TABLE))
      (pump (PUMP-HASH-TABLE))
      (pump-type (PUMP-TYPE-HASH-TABLE))
      (buffer (BUFFER-HASH-TABLE))
      (buffer-type (BUFFER-TYPE-HASH-TABLE))
      (electrode (ELECTRODE-HASH-TABLE))
      (extracellular-electrode (EXTRACELLULAR-ELECTRODE-HASH-TABLE))
      (t (when try-erzatz-table-name
	   (let* (*print-pretty*
		  (sym (read-from-string (format nil "~A-hash-table" type))))
	     (and (boundp sym) (symbol-value sym))))))))

(defun get-type-from-hash-table (table)
  (let ((table (typecase table
		 (string (READ-FROM-STRING table))
		 (HASH-TABLE table)
		 (symbol table))))
    (case table
      ((NODE-HASH-TABLE) 'node)
      ((SEGMENT-HASH-TABLE) 'segment)
      ((SOMA-HASH-TABLE) 'soma)
      ((CELL-HASH-TABLE) 'cell)
      ((CELL-TYPE-HASH-TABLE) 'cell-type)
      ((PARTICLE-HASH-TABLE) 'particle)
      ((PARTICLE-TYPE-HASH-TABLE) 'particle-type)
      ((CONC-PARTICLE-HASH-TABLE) 'conc-particle)
      ((CONC-PARTICLE-TYPE-HASH-TABLE) 'conc-particle-type)
      ((CONC-INT-HASH-TABLE) 'conc-int)
      ((CONC-INT-TYPE-HASH-TABLE) 'conc-int-type)
      ((SYNAPSE-HASH-TABLE) 'synapse)
      ((SYNAPSE-TYPE-HASH-TABLE) 'synapse-type)
      ((CHANNEL-HASH-TABLE) 'channel)
      ((CHANNEL-TYPE-HASH-TABLE) 'channel-type)
      ((AXON-HASH-TABLE) 'axon)
      ((AXON-TYPE-HASH-TABLE) 'axon-type)
      ((ISOURCE-HASH-TABLE) 'isource)
      ((VSOURCE-HASH-TABLE) 'vsource)
      ((PUMP-HASH-TABLE) 'pump)
      ((PUMP-TYPE-HASH-TABLE) 'pump-type)
      ((BUFFER-HASH-TABLE) 'buffer)
      ((BUFFER-TYPE-HASH-TABLE) 'buffer-type)
      ((ELECTRODE-HASH-TABLE) 'electrode)
      ((EXTRACELLULAR-ELECTRODE-HASH-TABLE) 'extracellular-electrode))))

;; Returns type symbol
(defun get-type-from-hash-table (table)
  (let ((table (typecase table
		 (HASH-TABLE table)
		 (string (READ-FROM-STRING table))
		 (symbol table))))
    (cond
      ((eq table (NODE-HASH-TABLE)) 'node)
      ((eq table (SEGMENT-HASH-TABLE)) 'segment)
      ((eq table (SOMA-HASH-TABLE)) 'soma)
      ((eq table (CELL-HASH-TABLE)) 'cell)
      ((eq table (CELL-TYPE-HASH-TABLE)) 'cell-type)
      ((eq table (PARTICLE-HASH-TABLE)) 'particle)
      ((eq table (PARTICLE-TYPE-HASH-TABLE)) 'particle-type)
      ((eq table (CONC-PARTICLE-HASH-TABLE)) 'conc-particle)
      ((eq table (CONC-PARTICLE-TYPE-HASH-TABLE)) 'conc-particle-type)
      ((eq table (CONC-INT-HASH-TABLE)) 'conc-int)
      ((eq table (CONC-INT-TYPE-HASH-TABLE)) 'conc-int-type)
      ((eq table (SYNAPSE-HASH-TABLE)) 'synapse)
      ((eq table (SYNAPSE-TYPE-HASH-TABLE)) 'synapse-type)
      ((eq table (CHANNEL-HASH-TABLE)) 'channel)
      ((eq table (CHANNEL-TYPE-HASH-TABLE)) 'channel-type)
      ((eq table (AXON-HASH-TABLE)) 'axon)
      ((eq table (AXON-TYPE-HASH-TABLE)) 'axon-type)
      ((eq table (ISOURCE-HASH-TABLE)) 'isource)
      ((eq table (VSOURCE-HASH-TABLE)) 'vsource)
      ((eq table (PUMP-HASH-TABLE)) 'pump)
      ((eq table (PUMP-TYPE-HASH-TABLE)) 'pump-type)
      ((eq table (BUFFER-HASH-TABLE)) 'buffer)
      ((eq table (BUFFER-TYPE-HASH-TABLE)) 'buffer-type)
      ((eq table (ELECTRODE-HASH-TABLE)) 'electrode)
      ((eq table (EXTRACELLULAR-ELECTRODE-HASH-TABLE)) 'extracellular-electrode))))


(defun element-hash-table (element &optional try-erzatz-table-name)
  (or (get-type-hash-table element try-erzatz-table-name)
      (get-type-hash-table (type-of (element element)))))
  
(defun choose-elements (&optional (label "Select Various Elements") (model-instances-only t))
  (loop for object-type-symbol-w-model in (choose-list-values
					   (if model-instances-only
					       (loop for sym in (flatten-list *type-model-symbols* *instance-model-symbols*)
						     when (list-of-all-things sym) collect sym)
					       *object-type-symbols*)
					   nil
					   :text "Select Classes" :do-all-at-once t :label label)
	nconcing (choose-list-values (namelist-of-all-things object-type-symbol-w-model) nil
				     :punt-if-only-one-entry nil :text "Select Instances" :label label)))
				     

(defun list-of-all-things (type &optional cell)			     
  (let* (*print-pretty*
	 (cell-structure (when cell (element cell 'cell)))
	 (table (element-hash-table type t))
	 (result (when table
		   (loop for thing being the hash-value of table
			 when (or (not cell) (eq cell-structure (element-cell thing)))
			 collect thing))))
    (when result
      (case type
	(segment (let ((electrodes (electrodes)))
		   (loop for seg in result unless (member seg electrodes) collect seg) ))
	(t result)))))

(defun list-of-all-things-in-circuit (type &optional cell)
  (loop for thing in (list-of-all-things type cell) when (element-in-circuit thing) collect thing))

(defun things-in-circuit (type &optional cell)
  (list-of-all-things-in-circuit type cell))

(defun hash-table-values (table)
  (loop for thing being the hash-value of table collect thing))

(defun hash-table-has-more-than-one-value-p (table)
  (loop for thing being the hash-value of table
	for count from 1
	when (> count 2) do (return t)))

#|
(defmacro element-collector (hash-table elt)
  `(let* ((all-things (hash-table-list ,hash-table))
	  (element ,elt)
	  (all-elts (and element (T-OR-NIL-P element)))
	  (cells (unless all-elts
		   (coerce-to-list (or (element-cell element) (hash-table-values (cell-hash-table)))))))
    (if (or (not cells) all-elts)
	all-things
	(loop for candidate in all-things when (instance-in-cell candidate nil cells) collect candidate))))
|#
#|
(defmacro element-collector (hash-table elt)
  `(let* ((all-things (hash-table-list ,hash-table))
	  (type (get-type-from-hash-table ,hash-table))
	  (element ,elt)
	  (all-elts (and element (T-OR-NIL-P element))))
    (when all-things
      (if all-elts
	  all-things
	  (cell-element-elements
	   (and (not all-elts)		; cell-elements
		(coerce-to-list (element-cell-element (or element (hash-table-list (cell-hash-table))))))
	   type)))))
|#

(defmacro element-collector (hash-table elt)
  `(let* ((all-things (hash-table-list ,hash-table))
	  (type (get-type-from-hash-table ,hash-table)))
    (when all-things
      (let ((element-cell-element (element-cell-element ,elt)))
	(if element-cell-element
	    (cell-element-elements element-cell-element type)
	    all-things)))))

(defun nodes (&optional element)
  "Returns a list of all nodes associated with the cell elements referenced by ELEMENT. If there are
no such cell elements, then a list of all nodes in circuit is returned."
  (if element
      (loop for elt in (coerce-to-list (element-cell-element element))
	    collect (element-physical-node elt))
      (hash-table-list (NODE-HASH-TABLE))
					;  (element-collector (NODE-HASH-TABLE) element)
      ))

(defun segments (&optional element)
  "Returns a list of all segments associated with the cell elements referenced by ELEMENT. If there
are no such cell elements, then a list of all segments in circuit is returned."
  (element-collector (SEGMENT-HASH-TABLE) element))

(defun somas (&optional element)
  "Returns a list of all somas associated with the cell elements referenced by ELEMENT. If there are
no such cell elements, then a list of all somas in circuit is returned."
  (element-collector (SOMA-HASH-TABLE) element))

(defun cells (&optional element)
  "Returns a list of all cells associated with the cell elements referenced by ELEMENT. If there are
no such cell elements, or ELEMENT is not included, then a list of all cells in circuit is returned."
  (if element
      (coerce-to-list (element-cell element))
					; (element-collector (CELL-HASH-TABLE) element)
      (hash-table-list (CELL-HASH-TABLE))))

(defun cell-types (&optional element)
  "Returns a list of all cell types associated with the cell elements referenced by ELEMENT. If there are
no such cell elements, or ELEMENT is NIL, then a list of all cell-types in circuit is
returned. If ELEMENT is T, then a list of all loaded cell-types are returned, whether part of the
current circuit or not."
  (let ((element-cell (element-cell element)))
    (if element-cell
	(coerce-to-list (element-type (element-cell element)))
	(let ((cell-types (hash-table-list (CELL-TYPE-HASH-TABLE))))
	  (if element cell-types
	      (loop for type in cell-types
		    when (element-in-circuit type)
		    collect type))))))
	      
(defun isources (&optional element)
  "Returns a list of all isources associated with the cell elements referenced by ELEMENT. If there
are no such cell elements, then a list of all isources in circuit is returned."
  (element-collector (ISOURCE-HASH-TABLE) element))

(defun vsources (&optional element)
  "Returns a list of all vsources associated with the cell elements referenced by ELEMENT. If there
are no such cell elements, then a list of all vsources in circuit is returned."
  (element-collector (VSOURCE-HASH-TABLE) element))

(defun synapses (&optional element)
  "Returns a list of all synapses associated with the cell elements referenced by ELEMENT. If there
are no such cell elements, then a list of all synapses in circuit is returned."
  (element-collector (SYNAPSE-HASH-TABLE) element))

(defun channels (&optional element)
  "Returns a list of all channels associated with the cell elements referenced by ELEMENT. If there
are no such cell elements, then a list of all channels in circuit is returned."
  (element-collector (CHANNEL-HASH-TABLE) element))

(defun synapse-types (&optional element)
  "Returns a list of all synapse-types associated with the cell elements referenced by ELEMENT. If
there are such n elements, then a list of all synapse-types in circuit is returned. If ELEMENT is T,
then a list of all loaded synapse-types are returned, whether part of the current circuit or not."
  (element-collector (SYNAPSE-TYPE-HASH-TABLE) element))

(defun channel-types (&optional element)
  "Returns a list of all channel-types associated with the cell elements referenced by ELEMENT. If
there are such n elements, then a list of all channel-types in circuit is returned. If ELEMENT is T,
then a list of all loaded channel-types are returned, whether part of the current circuit or not."
  (element-collector (CHANNEL-TYPE-HASH-TABLE) element))

(defun particles (&optional element)
  "Returns a list of all particles associated with the cell elements referenced by ELEMENT. If there
are no such cell elements, then a list of all particles in circuit is returned.  If ELEMENT is T,
then a list of all loaded particles are returned, whether part of the current circuit or not."
  (element-collector (PARTICLE-HASH-TABLE) element))

(defun conc-particles (&optional element)
  "Returns a list of all conc-particles associated with the cell elements referenced by ELEMENT. If
there such ar elements, then a list of all conc-particles in circuit is returned. If ELEMENT is T,
then a list of all loaded conc-particles are returned, whether part of the current circuit or not."
  (element-collector (CONC-PARTICLE-HASH-TABLE) element))

(defun particle-types (&optional element)
  "Returns a list of all particle-types associated with the cell elements referenced by ELEMENT. If
there such ar elements, then a list of all particle-types in circuit is returned. If ELEMENT is T,
then a list of all loaded particle-types are returned, whether part of the current circuit or not."
  (element-collector (PARTICLE-type-hash-table) ELEMENT))

(defun conc-particle-types (&optional element)
  "Returns a list of all conc-particle-types associated with the cell elements referenced by ELEMENT.
If such ther elements, then a list of all conc-particle-types in circuit is returned. If ELEMENT is
T, then a list of all loaded conc-particle-types are returned, whether part of the current circuit
or not."
  (element-collector (CONC-PARTICLE-TYPE-HASH-TABLE) element))

(defun conc-int-types (&optional element)
  "Returns a list of all conc-int-types associated with the cell elements referenced by ELEMENT. If
such ther elements, then a list of all conc-int-types in circuit is returned. If ELEMENT is T, then
a list of all loaded conc-int-types are returned, whether part of the current circuit or not."
  (element-collector (conc-int-type-HASH-TABLE) element))

(defun conc-ints (&optional element)
  "Returns a list of all conc-ints associated with the cell elements referenced by ELEMENT. If there
are no such cell elements, then a list of all conc-ints in circuit is returned. If ELEMENT is T,
then a list of all loaded conc-ints are returned, whether part of the current circuit or not."
  (element-collector (conc-int-HASH-TABLE) element))


(defun pumps (&optional element)
  "Returns a list of all pumps associated with the cell elements referenced by ELEMENT. If there are
no such cell elements, then a list of all pumps in circuit is returned."
  (element-collector (PUMP-HASH-TABLE) element))

(defun pump-types (&optional element)
  "Returns a list of all pump-types associated with the cell elements referenced by ELEMENT. If there
are no such cell elements, then a list of all pump-types in circuit is returned. If ELEMENT is T,
then a list of all loaded pump-types are returned, whether part of the current circuit or not."
  (element-collector (PUMP-TYPE-HASH-TABLE) element))

(defun buffers (&optional element)
  "Returns a list of all buffers associated with the cell elements referenced by ELEMENT. If there are
no such cell elements, then a list of all buffers in circuit is returned."
  (element-collector (BUFFER-HASH-TABLE) element))

(defun buffer-types (&optional element)
  "Returns a list of all buffer-types associated with the cell elements referenced by ELEMENT. If
there are such n elements, then a list of all buffer-types in circuit is returned. If ELEMENT is T,
then a list of all loaded buffer-types are returned, whether part of the current circuit or not."
  (element-collector (BUFFER-TYPE-HASH-TABLE) element))

(defun axons (&optional element)
  "Returns a list of all axons associated with the cell elements referenced by ELEMENT. If there are
no such cell elements, then a list of all axons in circuit is returned."
  (element-collector (AXON-HASH-TABLE) element))

(defun axon-types (&optional element)
  "Returns a list of all axon-types associated with the cell elements referenced by ELEMENT. If there
are no such cell elements, then a list of all axon-types in circuit is returned. If ELEMENT is T,
then a list of all loaded axon-types are returned, whether part of the current circuit or not."
  (element-collector (AXON-TYPE-HASH-TABLE) element))

(defun electrodes (&optional element)
  "Returns a list of all electrodes associated with the cell elements referenced by ELEMENT. If there
are no such cell elements, then a list of all electrodes in circuit is returned. If ELEMENT is T,
then a list of all loaded electrodes are returned, whether part of the current circuit or not."
  (element-collector (ELECTRODE-HASH-TABLE) element))

(defun extracellular-electrodes (&optional element)
  "Returns a list of all extracellular-electrodes associated with the cell elements referenced by
ELEMENT. If no such cell elements, then a list of all extracellular-electrodes in circuit is
returned. If ELEMENT is T, then a list of all loaded extracellular-electrodes are returned, whether
part of the current circuit or not."
  (element-collector (EXTRACELLULAR-ELECTRODE-HASH-TABLE) element))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun node-names () (coerce-to-list (element-name (nodes))))
(defun electrode-names () (coerce-to-list (element-name (electrodes))))
(defun extracellular-electrode-names () (coerce-to-list (element-name (extracellular-electrodes))))
(defun synapse-names () (coerce-to-list (element-name (synapses))))
(defun channel-names () (coerce-to-list (element-name (channels))))
(defun synapse-type-names () (coerce-to-list (element-name (synapse-types))))
(defun channel-type-names () (coerce-to-list (element-name (channel-types))))
(defun particle-names () (coerce-to-list (element-name (particles))))
(defun conc-particle-names () (coerce-to-list (element-name (conc-particles))))
(defun particle-type-names () (coerce-to-list (element-name (particle-types))))
(defun conc-particle-type-names () (coerce-to-list (element-name (conc-particle-types))))
(defun conc-int-type-names () (coerce-to-list (element-name (conc-int-types))))
(defun conc-int-names () (coerce-to-list (element-name (conc-ints))))
(defun cell-names () (coerce-to-list (element-name (cells))))
(defun cell-type-names () (coerce-to-list (element-name (cell-types))))
(defun pump-names () (coerce-to-list (element-name (pumps))))
(defun pump-type-names () (coerce-to-list (element-name (pump-types))))
(defun buffer-names () (coerce-to-list (element-name (buffers))))
(defun buffer-type-names () (coerce-to-list (element-name (buffer-types))))
(defun axon-names () (coerce-to-list (element-name (axons))))
(defun axon-type-names () (coerce-to-list (element-name (axon-types))))
(defun segment-names () (coerce-to-list (element-name (segments))))
(defun soma-names () (coerce-to-list (element-name (somas))))
(defun isource-names () (coerce-to-list (element-name (isources))))
(defun vsource-names () (coerce-to-list (element-name (vsources))))

(defun namelist-of-all-things (type &optional in-circuit)
  (let (*print-pretty*
	(table (get-type-hash-table type)))
    (loop for thing being the hash-value of table
	  when (or (not in-circuit) (element-in-circuit thing))
	  collect (element-name thing))))

(defun things-of-names (list-of-names &optional type)
  (loop for name in list-of-names collect (element name type)))
  
(defun names-of-things (list-of-things &optional type)
  (loop for thing in list-of-things collect (element-name thing type)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun element-w-string-or-symbol (elt-or-name &optional type fast)
  (loop for pointer in (coerce-to-list elt-or-name)
	collect pointer into out
	when (stringp pointer)
	collect (read-from-string pointer) into out
	when (symbolp pointer)
	collect (format nil "~A" pointer) into out
	finally (return (element (atomize-list out) type fast))))
  
(defun element (elt-or-name &optional type fast)
  "Return either the single structure object \(as an atom\) or objects \(as a list\) that are associated, if any, with ELT-OR-NAME. If none,
then ELEMENT returns NIL. ELT-OR-NAME is either a structure object, a structure name, or a (mixed) list of same. In the case of a name
\(which may be either a string, a symbol, or an integer\), since two or more structure objects of different types may have the same name
the search priority of structure types is given by *OBJECT-TYPE-SYMBOLS*.
"
  (typecase elt-or-name
    (cons (no-nils (flatten-list (loop for elt-or-name-atom in (flatten-list elt-or-name)
				       collect (element-core elt-or-name-atom type fast)))))
    (t (element-core elt-or-name type fast))))


(defun element-core (elt-or-name &optional type fast)
  (when elt-or-name
    (if fast
      elt-or-name
      (cond ((eq elt-or-name 'cell-element) (nconc (list-of-all-things 'soma) (list-of-all-things 'segment)))
	    ((find elt-or-name *object-type-symbols*) (list-of-all-things elt-or-name))
	    (t (let (*print-pretty*)
		 (typecase elt-or-name
		   (string
		    (or (name-element elt-or-name type)
			(unless (STRING-HAS-SPECIAL elt-or-name)
			  (let ((read-string (read-from-string elt-or-name)))
			    (when t	; (numberp read-string)
			      (name-element read-string type))))))
		   (symbol (or (name-element elt-or-name type)
			       (name-element (string elt-or-name) type)))
		   (number (or (name-element elt-or-name type)
			       (name-element (princ-to-string elt-or-name) type)))
		   (t (let ((type-of-elt-or-name (type-of elt-or-name)))
			(if type
			  (when (eq type-of-elt-or-name (if (eq type 'electrode) 'segment type)) elt-or-name)
			  (when (member type-of-elt-or-name *object-type-symbols*)
			    elt-or-name)))))))))))

(defun element-which-is-cell-element (element type) 
  (case type
    ((segment soma cell) (element element type))
    (t (or (element element 'segment)
	   (element element 'soma)
	   (element element 'cell)
	   (element-cell-element element type)))))



;; *********** *********** *********** ***********
;;
;; Generic element model funcalls and slot access
;;
;; *********** *********** *********** ***********

(defun element-funcall (element-function element &optional type arg)
  (let* (*print-pretty*
	 (element (element element type))
	 (function-sym (read-from-string (format nil "~A-~A" element-function (or type (type-of element))))))
    (cond
      ((macro-function function-sym) (eval (if arg
					       (macroexpand (list function-sym element arg))
					       (macroexpand (list function-sym element)))))
      ((fboundp function-sym) (if arg
				  (funcall function-sym element arg)
				  (funcall function-sym element))))))
				  



;; adapted from the Lisp FAQ
(defun element-slot (slot-name element &optional type)
  (let* (*print-pretty*
	 (element (element element type))
	 (accessor-symbol (find-symbol (format nil "~A-~A" (type-of element) slot-name) #.(package-name *package*))))
    (when accessor-symbol
      (funcall (symbol-function accessor-symbol) element))))


(defun element-slot (slot element &optional type)
  (let* (*print-pretty*
	 (element (element element type))
	 (slot-sym (read-from-string (format nil "~A-~A" (type-of element) slot))))
    (cond ((macro-function slot-sym) (eval (macroexpand (list slot-sym element))))
	  ((fboundp slot-sym) (funcall slot-sym element)))))

(defun set-element-voltage (element voltage)
  (let ((node (element-physical-node element)))
    (when node (set-node-voltage node voltage))))


(defun set-element-name (element name &optional type)
  (let* ((element (element element type))
	 (table (element-hash-table element))
	 (element-name (element-name element type)))
    (when (and element-name (not (equal element-name name)))
      (when (gethash name table)
	(sim-error (format nil "~A already is in ~A~%" name table)))
      (remhash element-name table)
      (setf (gethash name table) element)
      (typecase element
	(node (setf (node-name element) name))
	(CELL (setf (CELL-name element) name))  
	(CELL-TYPE (setf (CELL-TYPE-name element) name))  
	(SOMA (setf (SOMa-name element) name)) 
	(SEGMENT (setf (SEGMENT-name element) name))  
	(CHANNEL (setf (CHANNEL-name element) name))  
	(CHANNEL-TYPE (setf (CHANNEL-TYPe-name element) name)) 
	(PARTICLE (setf (PARTICLE-name element) name))  
	(PARTICLE-TYPE (setf (PARTICLE-TYPE-name element) name))  
	(CONC-INT (setf (CONC-INt-name element) name)) 
	(CONC-PARTICLE (setf (CONC-PARTICLE-name element) name))  
	(CONC-PARTICLE-TYPE (setf (CONC-PARTICLE-TYPE-name element) name))  
	(SYNAPSE (setf (SYNAPSe-name element) name)) 
	(SYNAPSE-TYPE (setf (SYNAPSE-TYPE-name element) name))  
	(AXON (setf (AXON-name element) name))  
	(AXON-TYPE (setf (AXON-TYPe-name element) name))
	(VSOURCE (setf (VSOURCE-name element) name))  
	(ISOURCE (setf (ISOURCe-name element) name))
	(PUMP (setf (PUMP-name element) name))  
	(PUMP-TYPE (setf (PUMP-TYPE-name element) name))
	(BUFFER (setf (BUFFER-name element) name))  
	(BUFFER-TYPE (setf (BUFFER-TYPE-name element) name))  
					;	(electrode (setf (electrode-name element) name))
	(extracellular-electrode (setf (extracellular-electrode-name element) name)))
      element)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Element data and plot
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun element-data-units (element &optional data-type type)
  (atomize-list
   (loop for element in (coerce-to-list element) collect
	 (let* ((element (element element type))
		(data-type (or data-type (default-data-type element))))
	   (case data-type
	     ((field-potential reversal-potential voltage) "mV")
	     ((dvdt voltage-derivative) "mV/ms")
	     ;; (axon-voltage "mV")
	     ((current dendrite-current) "nA")
	     (conductance (if *SAVE-CONDUCTANCES-normalized*
			      *NORMALIZED-GBAR-LABEL*
			      "uS"))
	     ((1 2 3 total) "mM")
	     ((markov-state state) "State"))))))



(defun element-data (element &optional data-type type state-index)
  "Given ELEMENT or elements of type TYPE, returns the plot data list [in correct time order,
according to the list of times in CURRENT-SIM-PLOT-TIME-LIST]. or a list of lists [for more than
one element] of type given by DATA-TYPE. The possible DATA-TYPE for the different element types are:

 Element Type              Data Type [first is default]
 ------------              ------------------------------

 SOMA                      'VOLTAGE, 'DVDT, 'DENDRITE-CURRENT
 SEGMENT                   'VOLTAGE, 'DVDT
 EXTRACELLULAR-ELECTRODE   'FIELD-POTENTIAL
 AXON                      'VOLTAGE
 CHANNEL, SYNAPSE          'CURRENT, 'REVERSAL-POTENTIAL,
                           'CONDUCTANCE
 ISOURCE                   'CURRENT
 VSOURCE                   'CURRENT, 'VOLTAGE
 PARTICLE                  'STATE, 'MARKOV-STATE
 CONC-PARTICLE             'STATE
 CONC-INT                   1, 2, 3, 'TOTAL
 BUFFER                    'CONCENTRATION
 PUMP                      'CURRENT

The STATE-INDEX argument is used when retrieving state data of Markov gating particles.
"
  (atomize-list
   (loop for element in (coerce-to-list element) collect
	 (let* ((element (element element type))
		(data-type (or data-type (default-data-type element))))	     
	   (retrieve-single-data element data-type state-index)))))


(defun element-saved-data (element &optional data-type type)
  (let ((elt (element element type)))
    (funcall (model-output-saved-data-function (element-model elt data-type)))) elt)

(proclaim '(inline element-current-value-internal))
(defun element-current-value-internal (element data-type)
  (let* ((model (type-symbol-model (type-of element)))
	 (model-output-current-value-function (model-output-current-value-function model data-type)))
    (when (fboundp model-output-current-value-function)
      (funcall model-output-current-value-function element))))
  
(defun element-current-value (element &optional data-type type)
  (let ((element (element element type)))
    (typecase element
      (cons (loop for elt in element collect (element-current-value-internal elt data-type)))
      (t (element-current-value-internal element data-type)))))

#|
(plot-xy-data (list (list (loop for time from 0.0 by 0.1 
				for count from 1 to (length (element-data-dted *soma* 0.1)) collect time)
			  (element-data-dted *soma* 0.1)) 
		    (list (current-sim-plot-time-list) 		
			  (element-data *soma*))))



|#

(defun element-data-dted (element dt &optional data-type type
				  (reference-time-list (current-sim-plot-time-list))
				  state-index)
  "Given an element or elements in ELEMENT or element type TYPE, returns a plot data list (or lists for more than one element) of type DATA-TYPE [as
is ELEMENT-DATA] sampled on an even time base as given by DT [milliseconds]. The time base for the original data is taken from REFERENCE-TIME-LIST,
which is either a list of numbers or a single number, in which case this is the even time grid of the original data. [Bug] Note that the original time
base must include steps on the order of DT in order for proper sampling during simulation periods of long time steps."
  (atomize-list
   (loop for element in (coerce-to-list element) collect
	 (convert-data-time-lists (element-data element data-type type state-index) reference-time-list dt))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Given a name or instance THING, returns T if THING or something of type THING is part of the
;; current circuit.
(defun element-in-circuit (thing &optional type)
  (instance-in-cell thing type))

(defun instance-in-cell (thing &optional type cells)
  (let ((thing (element thing type))
	(cells (coerce-to-list cells)))
    (or (typecase thing
	  ((or channel particle conc-particle isource vsource axon pump buffer synapse conc-int segment soma) t)
	  (cell-type (if cells
			 (loop for cell in cells when (eq thing (cell-type cell)) do (return t))
			 (consp (cell-type-cells thing))))
	  (cell (or (not cells) (member thing cells)))
	  (axon-type (if cells
			 (loop for axon in (axon-type-axons thing)
			       when (generic-intersection (element-cell axon) cells) do (return t))
			 (consp (axon-type-axons thing))))
	  (pump-type (if cells
			 (loop for pump in (pump-type-pumps thing)
			       when (generic-intersection (element-cell pump) cells) do (return t))
			 (consp (pump-type-pumps thing))))
	  (buffer-type (if cells
			   (loop for buffer in (buffer-type-buffers thing)
				 when (generic-intersection (element-cell buffer) cells) do (return t))
			   (consp (buffer-type-buffers thing))))
	  (channel-type (if cells
			    (loop with ch = (channel-type-first-channel thing)
				  while ch 
				  when (generic-intersection (element-cell ch) cells) do (return t)
				  else do (setq ch (channel-next-channel ch)))
			    (channel-p (channel-type-first-channel thing))))
	  (synapse-type (if cells
			    (loop with syn = (synapse-type-first-synapse thing)
				  while syn 
				  when (generic-intersection (element-cell syn) cells) do (return t)
				  else do (setq syn (synapse-next-synapse syn)))
			    (synapse-p (synapse-type-first-synapse thing))))
	  (particle-type (if cells
			     (loop with prt = (particle-type-first-particle thing)
				   while prt
				   when (generic-intersection (element-cell prt) cells) do (return t)
				   else do (setq prt (particle-next-particle prt)))
			     (consp (particle-type-particles thing))))
	  (conc-particle-type (if cells
				  (loop with prt = (conc-particle-type-first-particle thing)
					while prt
					when (generic-intersection (element-cell prt) cells) do (return t)
					else do (setq prt (conc-particle-next-particle prt)))
				  (consp (conc-particle-type-conc-particles thing))))
	  (conc-int-type (if cells
			     (loop for cint in (conc-int-type-conc-ints thing)
				   when (generic-intersection (element-cell cint) cells) do (return t))
			     (consp (conc-int-type-conc-ints thing))))
	  )
	(element-cell-element thing))))


    
(defun type-instances-in-cell (thing &optional type)
  "Given a name or instance of a type THING, returns a list of instances of that type."
  (let ((thing (element thing type)))
    (typecase thing
      (isource (isources))
      (vsource (vsources))
      (soma (somas))
      (segment (segments))
      (cell (cells))
      (cell-type (cell-type-cells thing))
      (axon-type (axon-type-axons thing))
      (axon (axons))
      (channel-type (channel-type-channels thing))
      (channel (channels))
      (pump-type (pump-type-pumps thing))
      (pump (pumps))
      (buffer-type (buffer-type-buffers thing))
      (buffer (buffers))
      (synapse-type (synapse-type-synapses thing))
      (synapse (synapses))
      (particle-type (particle-type-particles thing))
      (particle (particles))
      (conc-particle-type (conc-particle-type-particles thing))
      (conc-particle (conc-particles))
      (conc-int-type (conc-int-type-conc-ints thing))
      (conc-int (conc-ints))
      (extracellular-electrode (extracellular-electrodes))
					;    (electrode (electrodes))
      )))
    

(defun type-instances-in-circuit (thing &optional type)
  (type-instances-in-cell thing type))

(defun num-type-instances-in-circuit (thing &optional type)
  (let* ((instances (type-instances-in-circuit thing type))
	 (num-enabled (loop for instance in instances
			    when (element-enabled-p instance) sum 1))
	 (total (length instances)))
    (values total num-enabled)))


(defun elements-of-type (type &optional cell-elements)
  "Returns all elements of TYPE (synapse type, channel type, etc.) in circuit, restricted to
segments and somas associated with CELL-ELEMENTS, if included, otherwise all in circuit."
  (let ((cell-elements (coerce-to-list (element-cell-element cell-elements))))
    (loop for type in (coerce-to-list type)
	  nconcing
	  (typecase type
	    ((or soma segment) (list type))
	    (t (elements-of-type-core type cell-elements))))))

(defun elements-of-type-core (type cell-elements)
  (let ((type-instances (type-instances-in-circuit type)))
    (if cell-elements
	(loop for instance in type-instances
	      when (member (element-cell-element instance) cell-elements)
	      collect instance)
	type-instances)))


(defun num-elements-of-type (type)
  (num-type-instances-in-circuit type))


(defun print-num-elements-of-type (type &optional (stream t))
  (multiple-value-bind (number enabled)
      (num-type-instances-in-circuit type)
    (format stream "  There ~a ~a ~a ~a~p~A.~%"
	    (cond ((zerop number) "are")
		  ((> number 1) "are")
		  (t "is"))
	    number
	    (element-name type)
	    (TYPE-SYMBOL-CHILD-STRING (type-of type))
	    number
	    (if (> number enabled)
		(format nil " (~D enabled)" enabled)
		""))))


(defun print-num-elements-sourcefile (type &optional (stream t))
  (format stream "  ") (print-num-elements-of-type type)
  (format stream "  ") (ELEMENT-SOURCEFILE-STRING type stream))


(defun all-things-of-same-type (thing &optional type)
  (let ((thing (element thing type)))
    (typecase thing
      (cell (cell-type-cells (cell-type thing)))
      (cell-type (cell-type-cells thing))
      (axon-type (axon-type-axons thing))
      (axon (axon-type-axons (axon-type thing)))
      (channel-type (channel-type-channels thing))
      (channel (channel-type-channels (channel-type thing)))
      (pump-type (pump-type-pumps thing))
      (pump (pump-type-pumps (pump-type thing)))
      (buffer-type (buffer-type-buffers thing))
      (buffer (buffer-type-buffers (buffer-type thing)))
      (synapse-type (synapse-type-synapses thing))
      (synapse (synapse-type-synapses (synapse-type thing)))
      (particle-type (particle-type-particles thing))
      (particle (particle-type-particles (particle-type thing)))
      (conc-particle-type (conc-particle-type-conc-particles thing))
      (conc-particle (conc-particle-type-conc-particles (conc-particle-type thing)))
      (conc-int-type (conc-int-type-conc-ints thing))
      (conc-int (conc-int-type-conc-ints (conc-int-type thing))))))


(defun check-cell-name (name &key (automatic-name-fixing t))
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let (*print-pretty*)
    (if (gethash name (CELL-HASH-TABLE))
	(do ((i 1 (the fn (+ 1 i))))
	    ((not (gethash (format nil "~a-~D" name i) (CELL-HASH-TABLE)))
	     (if (or automatic-name-fixing
		     (go-ahead-menu 
		      (format nil
			      "Cell ~a already defined - ~%use ~a for name of new cell instead  ~%(otherwise cancel)."
			      name (format nil "~a-~D" name i))
		      "Verify Replacement Cell Name"
		      t))
		 (format nil "~a-~D" name i)))
	  (declare (fixnum i)))
	name)))

(defun check-element-name (name type &key (automatic-name-fixing t))
  ;; If NAME is a hash key for the hash table associated with TYPE, then if AUTOMATIC-NAME-FIXING is T or there is menu authorization a string made
  ;; with NAME followed by a hyphen and an integer is returned - otherwise return NIL. The integer is chosen, starting from 0 and advancing by one,
  ;; when the resulting name string is not an existing hash key for the hash table.
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let (*print-pretty*
	(hash-table (get-type-hash-table type)))
    (if (gethash name hash-table)
	(do ((i 1 (the fn (+ 1 i))))
	    ((not (gethash (format nil "~a-~D" name i) hash-table))
	     (if (or automatic-name-fixing
		     (go-ahead-menu 
		      (format nil
			      "~a ~a already defined - ~%use ~a for name of new ~a instead  ~%(otherwise cancel)."
			      type
			      name (format nil "~a-~D" name i)
			      type)
		      (format nil "Verify Replacement ~A Name" type)
		      t))
		 (format nil "~a-~D" name i)))
	  (declare (fixnum i)))
	name)))


(defun confirm-alternate-name-creation-and-update-*PROMPT-FOR-ALTERNATE-ELEMENT-NAMES* (string)
  (or (not *PROMPT-FOR-ALTERNATE-ELEMENT-NAMES*)
      (when (go-ahead-menu string "Rename Authorization" t t)
	(setq *PROMPT-FOR-ALTERNATE-ELEMENT-NAMES*
	      (not (go-ahead-menu (format nil "Automatically make names for duplicate membrane elements"))))
	t)))

(defun duplicate-element-check (node type)
  ;; True if *allow-duplicate-elements* is NIL and if an element of TYPE is a member of the :ELEMENTS of NODE.
  (and (not *allow-duplicate-elements*)
       (loop for elt in (node-elements node)
	     when (eq (element-type elt) type)
	     do (return t))))

#|
(defun create-element (elt-or-type &rest others)
  "Generic create function for elements. Takes any number of arguments, and considers all the atoms
in the arguments. Given any atom which references an element type, adds an element of that type to
all the cell elements which are referenced in the arguments. If there are no references to cell
elements, then the element types referenced in the arguments are created if they do not already
exist. Returns all created elements or element types as a list if more than one, or as an atom if
only one. For example:

       * (create-element 'NA-hh)
       <Channel Type NA-HH>

       * (create-element 'NA-hh *soma*)
       <Channel Hippo-soma-NA-HH: type NA-HH>

       * (create-element 'NA-hh *soma* 'DR-hh)
       (<Channel Hippo-soma-NA-HH: type NA-HH>
        <Channel Hippo-soma-DR-HH: type DR-HH>)

If either keywords :NO-DUPLICATES or :NO-DUPS is included in the arguments, then no duplicate elements \(for example
the same synapse type on the same cell element\) will be created."
  (let* ((arg-flat-list (flatten-no-nils-list elt-or-type others))
	 (*allow-duplicate-elements* (and (not (member :no-duplicates arg-flat-list))
					  (not (member :no-dups arg-flat-list))))
	 (cell-elements (flatten-no-nils-list (element-cell-element arg-flat-list)))
	 (result
	  (flatten-no-nils-list
	   (loop for type in arg-flat-list collect
		 (if cell-elements
		     (loop for cell-element in cell-elements collect
			   (let* ((type (or (element-type type nil t) type))
				  (type-model (element-model type))
				  (child-model (type-symbol-model (type-symbol-child-string (type-of type))))
				  (create-routine (if child-model (model-create-routine child-model)
						      (when type-model (model-create-routine type-model)))))
			     (if (and (not (cell-element-p type)) create-routine)
				 (if cell-element
				     (case (length (function-required-args create-routine))
				       (1 (funcall create-routine cell-element))
				       (2 (funcall create-routine cell-element type)))
				     (funcall (model-create-routine type-model) type))
				 (unless (cell-element-p type)
				   (sim-error
				    (format nil
					    "~A doesn't refer to an element type nor a cell element!" type))))))
		     (when (not (cell-element-p (element-type type nil t)))
		       (element-type type nil t)))))))
    (atomize-list (delete-duplicates result))))


(defun create-element (thing &rest others)
  "Generic create function for elements. Takes any number of arguments, and considers all the atoms in the arguments. Given any atom which
references an element type, adds an element of that type to all the cell elements which are referenced in the arguments. If any argument refers to a
cell, then that cell's soma is processed as a cell element. If there are no references to cell elements, then the element types referenced in the
arguments are created if they do not already exist. Returns all created elements or element types as a list if more than one, or as an atom if only
one. For example:

       * (create-element 'NA-hh)
       <Channel Type NA-HH>

       * (create-element 'NA-hh *soma*)
       <Channel Hippo-soma-NA-HH: type NA-HH>

       * (create-element 'NA-hh *soma* 'DR-hh)
       (<Channel Hippo-soma-NA-HH: type NA-HH>
        <Channel Hippo-soma-DR-HH: type DR-HH>)

If either keywords :NO-DUPLICATES or :NO-DUPS is included in the arguments, then no duplicate
elements \(for example the same synapse type on the same cell element\) will be created."
  (let* ((arg-flat-list (flatten-no-nils-list thing others))
	 (*allow-duplicate-elements* (and (not (member :no-duplicates arg-flat-list))
					  (not (member :no-dups arg-flat-list))))
	 (cell-elements (flatten-no-nils-list (element-cell-element arg-flat-list))))
    (atomize-list
     (delete-duplicates
      (flatten-no-nils-list
       (loop for type in (remove-all '(:no-duplicates :no-dups) arg-flat-list) collect
	     (if cell-elements
	       (unless (or (cell-p type) (cell-element-p type))
		 (let* ((element-type (or (element-type type nil t) type))
			(element-type-model (element-model element-type))
			(child-model (type-symbol-model (type-symbol-child-string (type-of element-type))))
			(create-routine (if child-model
					  (model-create-routine child-model)
					  (when element-type-model (model-create-routine element-type-model)))))
		   (if (and (not (cell-element-p element-type)) create-routine)
		     (loop for cell-element in cell-elements collect
			   (call-create-element-create-routine create-routine cell-element element-type element-type-model))
		     (unless (cell-element-p element-type)
		       (sim-error (format nil "~A doesn't refer to an element type nor a cell element!" element-type))))))
	       (when (not (cell-element-p (element-type type nil t)))
		 (element-type type nil t)))))))))

|#

(defun call-create-element-create-routine (create-routine cell-element type type-model)
  (if cell-element
      (case (length (function-required-args create-routine))
	(1 (funcall create-routine cell-element))
	(2 (funcall create-routine cell-element type)))
      (funcall (model-create-routine type-model) type)))

(defun create-element (thing &rest others)
  "Generic create function for elements. Takes any number of arguments, and considers all the atoms in these arguments as members of a flat
list ARGS. Given any atom in ARGS which references an element type, CREATE-ELEMENT adds an element of that type to all the cell elements
\(somas and segment\) referenced in ARGS. If any member of ARGS refers to a cell \(for example, a cell name\) then that cell's soma is
processed as a cell element. Any element types referenced in ARGS are created if they do not already exist. Returns either a single object
\(as an atom\) or objects \(as a list\) of all created circuit elements, if any; otherwise then all referenced element types are returned,
whether or not created during the current invocation of CREATE-ELEMENT. For example:
                                         
       * (create-element 'NA-hh)
       <Channel Type NA-HH>
                                   
       * (create-element 'NA-hh *soma*)
       <Channel Hippo-soma-NA-HH: type NA-HH>
                                              
       * (create-element 'NA-hh *soma* 'DR-hh)
       (<Channel Hippo-soma-NA-HH: type NA-HH>
        <Channel Hippo-soma-DR-HH: type DR-HH>)
                                              
If either keywords :NO-DUPLICATES or :NO-DUPS is included in the arguments, then no duplicate elements \(for example the same synapse type
on the same cell element\) will be created. Otherwise duplicates may be generated with or without use interaction depending on the values of
*USE-SIMPLE-NAMES*, *ALLOW-DUPLICATE-ELEMENTS* and *PROMPT-FOR-ALTERNATE-ELEMENT-NAMES*. The arguments 'SEGMENT, 'SOMA and 'CELL-ELEMENT will
reference all the segments, somas, and both segments and somas of the circuit, respectively.

"
  (let* ((ARGS (flatten-no-nils-list thing others))
	 (*allow-duplicate-elements* (and (not (member :no-duplicates ARGS))
					  (not (member :no-dups ARGS))))
	 cell-element)
    (loop for arg in (remove-all '(:no-duplicates :no-dups) ARGS)
	  do (setq cell-element (element-cell-element arg))
	  when cell-element collect cell-element into cell-elements
	  else collect arg into possible-types
	  finally
	  ;; (format t "~a ~a ~%" cell-elements possible-types)
	  ;; (format t "*allow-duplicate-elements* ~a ~%" *allow-duplicate-elements*)
	  
	  (setq cell-elements (flatten-no-nils-list cell-elements))
	  (return
	   (atomize-delete-duplicates-flatten-no-nils-list
	    (loop for type in possible-types collect
		  (if cell-elements
		    (unless (or (cell-p type) (cell-element-p type))
		      (let* ((element-type (or (element-type type nil t) type))
			     (element-type-model (element-model element-type))
			     (child-model (type-symbol-model (type-symbol-child-string (type-of element-type))))
			     (create-routine (if child-model
					       (model-create-routine child-model)
					       (when element-type-model (model-create-routine element-type-model)))))
			(if (and (not (cell-element-p element-type)) create-routine)
			  (loop for cell-element in cell-elements collect
				(call-create-element-create-routine create-routine cell-element element-type element-type-model))
			  (unless (cell-element-p element-type)
			    (sim-error (format nil "~A doesn't refer to an element type nor a cell element!" element-type))))))
		    (when (not (cell-element-p (element-type type nil t)))
		      (element-type type nil t)))))))))

(defun reorder-elements-of-type (type)
  "Reorders all the elements of TYPE for the iterator constructs (applies to synapses, channels,
particles and conc-particles at the moment)."
  (let ((type (element-type type)))
    (typecase type
      (conc-particle-type (reorder-conc-particles-of-type type))
      (particle-type (reorder-particles-of-type type))
      (synapse-type (reorder-synapses-of-type type))
      (channel-type (reorder-channels-of-type type)))))


(defun random-segment (&optional (cell *cell*))
  "Returns a randomly selected (using RANDOM-NTH) segment from CELL (default *CELL*). If CELL is a list, then segments
are chosen from the set of all cells associated with the elements of the list."
  (RANDOM-NTH (typecase cell
		(atom
		 (let ((cell (element-cell cell)))
		   (when cell (cell-segments cell))))
		(cons
		 (no-nils
		  (loop for thing in cell collect
			(let ((cell (element-cell thing)))
			  (when cell (random-nth (cell-segments cell))))))))))
  

(defun object-type-symbol-p (thing)
  "If THING is one of the type symbols for the circuit element classes."
  (true-p (member thing *object-type-symbols*)))
  
(defun element-cell-type (element &optional type)
  (let ((cell (element-cell element type)))
    (when cell (cell-type (if (consp cell) (car cell) cell)))))



;; EXTRACT-CELL-NAME Finds the longest cell name that is embedded in STRING.
(defun extract-cell-name (string)
  (loop for cell-name in (sort (namelist-of-all-things 'cell) '> :key 'length)
	when (search cell-name string)
	do (return cell-name)
	finally (return string)))


(defun element-node (element &optional type)
  "Return the soma or segment circuit node(s) associated with ELEMENT of TYPE."
  (atomize-list
   (loop for cell-element in (coerce-to-list (element-cell-element element type))
	 collect (element-physical-node cell-element type t))))


(defun element-physical-node (element &optional type fast)
  (let ((elt (element element type fast)))
    (when (and elt (not (element-type-p element)))
      (typecase elt
	(segment (segment-node-2 elt))
	(soma (soma-node elt))
	(node elt)
	(cell (soma-node (cell-soma elt)))
	(axon (axon-proximal-node elt))
	(isource (isource-node-2 elt))
	(vsource (vsource-node elt))
	(channel (channel-node elt))
	(synapse (synapse-node elt))
	(pump (element-physical-node (pump-conc-int elt)))
	(t (let ((cell-element (element-slot 'cell-element elt type)))
	     (when cell-element (element-physical-node cell-element))))))))

(defun ELEMENT-PHYSICAL-NODE-FAST (elt &optional type)
  (ELEMENT-PHYSICAL-NODE elt type t))


(defun element-segments (element &optional (exclude-self t))
  (let ((cell-element  (element-cell-element element)))
    (loop for elt in (node-elements (element-physical-node cell-element))
	  when (and (segment-p elt) (or (not exclude-self)
					(not (eq cell-element elt))))
	  collect elt)))
			      

(defun particle-type-channel-type (element)
  (let ((prt (element element `particle)))
    (if prt
	(channel-type (particle-channel prt))
	(let ((prt-type (element-type element)))
	  (when (particle-type-p prt-type) 
	    (loop for channel-type in (channel-types)
		  when (member prt-type (channel-type-particle-types-and-powers channel-type) :key 'car)
		  do (return channel-type)))))))

(defun conc-particle-type-channel-type (element)
  (let ((prt (element element `conc-particle)))
    (if prt
	(channel-type (conc-particle-channel prt))
	(let ((prt-type (element-type element)))
	  (when (conc-particle-type-p prt-type)
	    (loop for channel-type in (channel-types)
		  when (member prt-type (channel-type-conc-particle-types-and-powers channel-type) :key 'car)
		  do (return channel-type)))))))

(defun element-has-concentration-dependence (element)
  (let ((element (element-type element)))
    (typecase element
      (channel-type (channel-type-conc-particle-types-and-powers element))
      (conc-particle-type t))))

      
(defun element-major-ion (element &optional type)
  (let ((elt-type (element-type element type)))
    (typecase elt-type
      (conc-particle-type (conc-int-type-species (conc-particle-type-conc-int-type elt-type)))
      (t 
       (let* ((membrane-element
	       (typecase elt-type
		 ((or synapse-type channel-type) elt-type)
		 (particle-type (particle-type-channel-type elt-type))))
	      (ion-perms (copy-list (element-ion-permeabilities membrane-element))))
	 (caar (sort ion-perms '> :key 'cadr)))))))


(defun element-ion-permeabilities (elt)
  (let ((type (element-type elt)))
    (when type
      (typecase type
	(channel-type (channel-type-ion-permeabilities type))
	(synapse-type (synapse-type-ion-permeabilities type))
	(t (element-slot 'ion-permeabilities type))))))

(defun element-of-ion-type-p (element ion-type)
  "Given ELEMENT, returns T if it is associated with ION-TYPE (e.g. 'NA, 'K, 'CA, 'CL, etc.)."
  (consp (find ion-type (ELEMENT-ION-PERMEABILITIES element) :key 'car)))


(defun element-conc-int-type-params (elt &optional fast)
  (let ((type (if fast elt (element-type elt))))
    (when type
      (typecase type
	(channel-type (channel-type-conc-int-type-params type))
	(synapse-type (synapse-type-conc-int-type-params type))
					; (t (element-slot 'conc-int-type-params type))
	))))


(defun element-conc-ints-params (elt)
  (typecase elt
    (channel (channel-conc-ints-params elt))
    (synapse (synapse-conc-ints-params elt))))


(defun element-has-conc-ints (element)
  (loop for elt in (elements-of-type element)
	when (element-conc-ints-params elt)
	do (return t)))


(defun element-iv-parameters (elt &optional fast)
  (let ((type (if fast elt (element-type elt))))
    (when type
      (typecase type
	(channel-type (channel-type-iv-parameters type))
	(synapse-type (synapse-type-iv-parameters type))
	(t (element-slot 'iv-parameters type))))))

(defun membrane-iv-element-p (element)
  (true-p (type-iv-parameters element)))

(defun element-capacitance (element &optional value)
  "Sets the capacitance of the soma or segment associated with ELEMENT to VALUE, if non-nil, Otherwise
returns the current value."
  (let ((element (element-cell-element element)))
    (typecase element
      (soma (if value
		(setf (soma-capacitance element) (d-flt value))
		(soma-capacitance element)))
      (segment (if value
		   (setf (segment-capacitance element) (d-flt value))
		   (segment-capacitance element)))
      (t 0.0))))


(defun element-current (elt &optional conc-in conc-out valence)
  (let ((elt (element elt)))
    (typecase elt
      (channel (get-channel-current elt :conc-in conc-in :conc-out conc-out :valence valence))
      (synapse (get-synapse-current elt conc-in conc-out valence))
      (t 0.0d0))))

(defun element-voltage (elt)
  (node-voltage-n+1 (element-physical-node elt)))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Element parameters family
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun element-parameters (element &optional type fast)
  (let ((elt (element element type fast)))
    (when elt
      (typecase elt
	(conc-int-type (conc-int-type-parameters elt))
	(conc-int (conc-int-parameters elt))
	(conc-particle-type (conc-particle-type-parameters elt))
	(conc-particle (conc-particle-parameters elt))
	(particle-type (particle-type-parameters elt))
	(particle (particle-parameters elt))
	(channel-type (channel-type-parameters elt))
	(channel (channel-parameters elt))
	(segment (segment-parameters elt))
	(soma (soma-parameters elt))
	(node (node-parameters elt))
	(cell-type (cell-type-parameters elt))
	(cell (cell-parameters elt))
	(pump-type (pump-type-parameters elt))
	(pump (pump-parameters elt))
	(buffer-type (buffer-type-parameters elt))
	(buffer (buffer-parameters elt))
	(axon-type (axon-type-parameters elt))
	(axon (axon-parameters elt))
	(isource (isource-parameters elt))
	(vsource (vsource-parameters elt))
	(synapse-type (synapse-type-parameters elt))
	(synapse (synapse-parameters elt))
	(extracellular-electrode (extracellular-electrode-parameters elt))
;	(electrode (electrode-parameters elt))
	
	(t (element-slot 'parameters elt type))))))

(defun element-parameters-fast (element &optional type)
  (element-parameters element type t))

(defun replace-element-param-acons (element key value)
  (set-element-parameter element key value))

(defun set-element-param (element key &optional value update)
  (set-element-parameter element key value update))

(defun set-element-parameter-fast (element key value parameters)
  (set-element-parameter element key value nil t parameters))

(defun set-element-parameter (element key &optional value update fast parameters)
  (typecase element
    (cons (atomize-list
	   (loop for elt in element do (set-element-parameter-internal elt key value update fast parameters)
		 collect value)))
    (t (set-element-parameter-internal element key value update fast parameters)
       value)))

(defun set-element-parameter-internal (elt key value update fast parameters) 
  (if value
      (let* ((element (element elt nil fast))
	     (params (or parameters (element-parameters element nil fast)))
	     (assoc-result (assoc key params)))
	(if assoc-result 
	    (rplacd assoc-result value)
	    (let ((new-params (acons key value params)))
	      (replace-element-parameters element new-params update))))
      (remove-element-param-acons elt key parameters)))

(defun set-some-element-parameters (element list)
  (loop for key-value in list do (element-parameter element (car key-value) (cadr key-value))))

(defun replace-element-parameters (element new-params &optional update)
  (let ((element (element element)))
    (typecase element
      (CELL (setf (CELL-parameters element) new-params)
	    (when update (set-circuit-elements-parameters)))
      (CELL-TYPE (setf (CELL-TYPE-parameters element) new-params)
		 (when update (set-circuit-elements-parameters)))
      (SOMA (setf (SOMA-parameters element) new-params)
	    (when update (set-soma-membrane-parameters element)))
      (SEGMENT (setf (SEGMENT-parameters element) new-params)
	       (when update (set-segment-membrane-parameters element)))
      (CHANNEL (setf (CHANNEL-parameters element) new-params)
	       (when update (set-CHANNEL-parameters element t)))
      (CHANNEL-TYPE (setf (CHANNEL-TYPE-parameters element) new-params)
		    (when update (set-CHANNEL-TYPE-parameters element)))
      (PARTICLE (setf (PARTICLE-parameters element) new-params))
		 
      (PARTICLE-TYPE (setf (PARTICLE-TYPE-parameters element) new-params)
		     (when update (set-channels-parameters)))
      (CONC-PARTICLE (setf (CONC-PARTICLE-parameters element) new-params))

      (CONC-PARTICLE-TYPE (setf  (CONC-PARTICLE-TYPE-parameters element) new-params)
			  (when update (set-channels-parameters)))
      (CONC-INT (setf (CONC-INT-parameters element) new-params)
		(when update (set-conc-integrators-parameters)))
      (CONC-INT-type (setf (CONC-INT-type-parameters element) new-params)
		     (when update (set-conc-integrators-parameters)))

      (SYNAPSE (setf (SYNAPSE-parameters element) new-params)
	       (when update (set-SYNAPSE-parameters element t)))
      (SYNAPSE-TYPE (setf (SYNAPSE-TYPE-parameters element) new-params)
		    (when update (set-SYNAPSEs-parameters t element)))
      (AXON (setf (AXON-parameters element) new-params)
	    (when update (set-AXON-parameters nil element)))
      (AXON-TYPE (setf (AXON-TYPE-parameters element) new-params)
		 (when update (set-AXONs-parameters)))
      (VSOURCE (setf (VSOURCE-parameters element) new-params))
      (ISOURCE (setf (ISOURCE-parameters element) new-params))
      (PUMP (setf (PUMP-parameters element) new-params))
      (PUMP-TYPE (setf (PUMP-TYPE-parameters element) new-params))
      (BUFFER (setf (BUFFER-parameters element) new-params))
      (BUFFER-TYPE (setf (BUFFER-TYPE-parameters element) new-params))
      (NODE (setf (NODE-parameters element) new-params))
      (extracellular-electrode (setf (extracellular-electrode-parameters element) new-params))))
  new-params)


(defun set-several-element-parameters (elt keys-and-params)
  (loop for key-and-param in keys-and-params do
	(element-parameter elt (car key-and-param) (cadr key-and-param))))

(defun update-element-parameters-with-new-parameters (new-parameters elt)
  (loop for acons in new-parameters do
	(element-parameter elt (car acons) (cdr acons)))
  (element-parameters elt))
    

(defun remove-element-parameter (element key &optional params)
  (remove-element-param-acons element key params))

(defun remove-element-parameters (element keys &optional params)
  (loop for key in keys do (remove-element-parameter element key params)))

(defun remove-element-param-acons (element key &optional params)
  (let* ((params (or params (element-parameters element)))
	 (assoc-result (assoc key params)))
    (when assoc-result (rplacd assoc-result nil))))

(defun remove-element-param-acons (element key &optional params)
  (let* ((params (or params (element-parameters element)))
	 (assoc-result (assoc key params)))
    (when assoc-result
      (if (eq assoc-result (car params))
	  (replace-element-parameters element (cdr params))
	  (my-delete assoc-result params)))))


(defun get-cell-element-param-fast (element key)
  (get-a-value key (typecase element
		     (segment (segment-parameters element))
		     (soma (soma-parameters element)))))

(defun get-element-parameter (element key &optional params type)
  (typecase element
    (cons (atomize-list (loop for elt in element collect (get-element-param elt key params type))))
    (t (get-element-param element key params type))))

(defun get-element-param (element key &optional params type)
  (let ((element (element element type)))
    (when element (get-a-value key (or params (element-parameters element))))))

(defun get-element-parameter-fast (key params)
  (get-a-value key params))

(defun element-parameter-fast (key params)
  (get-a-value key params))

(defun push-onto-element-param-acons (element key value &optional (element-parameters (element-parameters element)))
  (if element-parameters
      (let ((param-a-list-entry (get-a-value key element-parameters)))
	(typecase param-a-list-entry
	  (cons (setf (setfable-get-a-value key element-parameters) (cons value param-a-list-entry)))
	  (null (set-element-parameter-fast element key (list value) element-parameters))
	  ((or symbol number)
	   (setf (setfable-get-a-value key element-parameters) (cons value (list param-a-list-entry))))))
      (set-element-parameter element key (list value))))


(defun push-element-parameter (element key value &optional (element-parameters (element-parameters element)))
  (push-onto-element-param-acons element key value element-parameters))

(defun element-parameter (element parameter &optional (value nil value-supplied-p) update)
  "Returns the value or values associated with PARAMETER for elements in ELEMENT. If VALUE is
supplied, the parameter is set to this new value. For some types of elements and parameters, the
UPDATE flag will cause the parameter to be fully processed."
  (if value-supplied-p
      (set-element-parameter element parameter value update)
      (get-element-parameter element parameter)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-element-array (elt key dims &optional (type 'double-float) from-particle-type always-set-new)
  (or (and (not always-set-new)
	   (if from-particle-type
	       (get-a-value key (particle-type-parameters elt))
	       (element-parameter elt key)))
      (element-parameter elt key (make-array (typecase dims
					       (cons dims)
					       (t (list dims)))
					     :element-type type))))

(defun element-sourcefile-string (element &optional (stream t))
  (when (element-parameter element 'source)
    (format stream "Source file: ~A" (element-parameter element 'source))))


(defun element-type-param (element param &optional value update)
  "Similar to ELEMENT-PARAMETER, but for examining/setting specific parameters of the synapse or
channel type associated with ELEMENT. PARAM can be:

     'PERMEABILITY-SOURCE, 'GBAR-SOURCE (e.g. :absolute or :density)
     'PERMEABILITY-REF [cm3/sec], 'GBAR-REF [uS]
     'GBAR-DENSITY [pS per square micron (0.1mS per square cm)]
     'PERMEABILITY-DENSITY [1.0e-6 cm3/sec per square micron]
     'E-REV [mV]
     'BLOCK [T or NIL]

If no new VALUE follows the PARAM, then the current value of the slot corresponding to PARAM is
returned. Supplying a non-nil value for UPDATE will cause the change to propagate to the appropriate
elements of the type associated with ELEMENT."
  (set-element-type-param element param value update))


(defun set-element-type-param (type param &optional value update)
  (let ((type (element-type type)))
    (typecase type
      (synapse-type (set-synapse-type-param type param value update))
      (channel-type (set-channel-type-param type param value update)))))


(defun revamp-type-parameters ()
  "Go through all the current instances of channel, synapse, or conc-int types, and update them
according to the appropriate type parameter library list."
  (revamp-channel-type-parameters)
  (revamp-synapse-type-parameters)
  (revamp-conc-int-type-parameters))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Gbar Pbar functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
      
(defun independent-element-gbars-p (type)
  (typecase type
    (synapse-type (loop for elt in (synapses-of-type type)
			unless (synapse-inherit-parameters-from-type elt)
			do (return t)))
    (channel-type (loop for elt in (channels-of-type type)
			unless (channel-inherit-parameters-from-type elt)
			do (return t)))))
    
(defun list-total-gbars (&optional type)
  (loop for type in (coerce-to-list (or type (channel-types)))
	collect (list (element-name type)
		      (loop for elt in (elements-of-type type)
			    sum (typecase elt
				  (channel (channel-gbar elt))
				  (synapse (synapse-gbar elt))
				  (t 0.0))))))


(defun convert-gbars-to-densities (type &optional (reference-area (element-area *soma*)))
  "Convert all channel or synapse types, depending on whether TYPE is 'CHANNEL-TYPE or 'SYNAPSE-TYPE,
from absolute gbars to densities, where REFERENCE-AREA is in um2."
  (loop for type in (case type
		      (channel-type (channel-types))
		      (synapse-type (synapse-types)))
	when (eq (element-type-param type 'gbar-source) :absolute)
	do (let ((gbar-density (/ (element-type-param type 'gbar-ref) ; uS
				  reference-area))) ; um^2
	     (set-element-type-param type 'gbar-source :density)
	     (set-element-type-param type 'gbar-density
				     (* 1e6 gbar-density) ; convert uS/um2 to pS/um2
				     t)	; update the associated channels/synapses
	     ))
  nil)

(defun set-element-absolute-gbar-ref (element gbar-ref)
  "Set the gbar for the synapse or channel ELEMENT to an absolute value GBAR-REF (uS). To get the
current value, use ELEMENT-GBAR."
  (set-element-gbar-ref element gbar-ref))

(defun set-element-gbar-ref (elt gbar-ref)
  (let ((elt (element elt))
	(gbar-ref (d-flt (typecase gbar-ref
			   (float gbar-ref)
			   (number (float gbar-ref))
			   (t (sim-error
			       (format nil "SET-ELEMENT-GBAR-REF (called on ~A with arg ~A): GBAR-REF must be a number"
				       elt gbar-ref)))))))
    (typecase elt
      ((or synapse-type channel-type)
       (sim-error "Use ELEMENT-TYPE-PARAM on a type!"))
      (t
       (typecase elt
	 (synapse (setf (synapse-gbar/perm-reference-value elt) gbar-ref
			(synapse-inherit-parameters-from-type elt) nil))
	 (channel (setf (channel-gbar/perm-reference-value elt) gbar-ref
			(channel-inherit-parameters-from-type elt) nil)))))
    (SET-ELEMENT-MEMBRANE-PARAMETERS elt)
    gbar-ref))


#|
;; SET-ELEMENT-DENSITY The DENSITY argument is in pS per square micron (0.1mS per square cm)
(defun set-element-density (element density)
  (set-element-gbar-density element density))

(defun set-element-gbar-density (elt gbar-density)
  (let ((elt (element elt)))
    (typecase elt
      ((or synapse-type channel-type)
       (sim-error "Use ELEMENT-TYPE-PARAM on a type!"))
      (t
       (set-element-parameter
	elt 'gbar-density
	(typecase gbar-density
	  (float (coerce-to-single gbar-density))
	  (number (float gbar-density))
	  (t (sim-error
	      (format nil "SET-ELEMENT-GBAR-DENSITY (called on ~A with arg ~A): GBAR-DENSITY must be a number"
		      elt gbar-density)))))
       (set-element-gbar-source elt :density)
       (typecase elt
	 (synapse (setf (synapse-inherit-parameters-from-type elt) nil))
	 (channel (setf (channel-inherit-parameters-from-type elt) nil)))
       (SET-ELEMENT-MEMBRANE-PARAMETERS elt)
       (element-parameter elt 'gbar-density)))))

|#


(defun gbar-element-cell-element (elt)
  (typecase elt
    (channel (channel-cell-element elt))
    (synapse (synapse-cell-element elt))))

(defun pbar-element-cell-element (elt)
  (gbar-element-cell-element elt))

(defun element-gbar-source (elt &optional element-parameters iv-parameters gbar-inherited)
  (let* ((elt (element elt)))
    (if (not (or gbar-inherited (inherit-parameters-from-type elt)))
	:absolute
	(membrane-element-iv-parameters-gbar-source (or iv-parameters (type-iv-parameters elt))))))

(defun element-pbar-source (elt &optional element-parameters iv-parameters gbar-inherited)
  (element-gbar-source elt element-parameters iv-parameters gbar-inherited))

(defun channel-pbar-source (elt &optional element-parameters iv-parameters gbar-inherited)
  (element-pbar-source elt element-parameters iv-parameters gbar-inherited))

(defun synapse-pbar-source (elt &optional element-parameters iv-parameters gbar-inherited)
  (element-pbar-source elt element-parameters iv-parameters gbar-inherited))

(defun element-gbar-ref (element)
  (let ((elt (element element)))
    (if (inherit-parameters-from-type elt)
	(membrane-element-iv-parameters-gbar-ref (type-iv-parameters elt (element-type elt)))
	(membrane-element-gbar/perm-reference-value elt))))

(defun channel-gbar-ref (element) (element-gbar-ref element))

(defun synapse-gbar-ref (element) (element-gbar-ref element))

(defun element-gbar-density (element)
  (let* ((elt (element element))
	 (gbar-inherited (inherit-parameters-from-type elt))
	 (element-parameters (unless gbar-inherited (element-parameters elt))))
    (or (and (not gbar-inherited) (get-a-value 'gbar-density element-parameters))
	(membrane-element-iv-parameters-gbar-density (type-iv-parameters elt (element-type elt))))))

(defun element-pbar-density (element)
  (element-gbar-density element))

(defun channel-gbar-density (element) (element-gbar-density element))

(defun synapse-gbar-density (element) (element-gbar-density element))

(defun print-element-gbar-string (elt)
  (let* ((elt (element elt))
	 (gbar (element-gbar-slot elt))
	 (iv-parameters (type-iv-parameters elt))
	 (gbar-inherited (inherit-parameters-from-type elt))
	 (iv-relation (membrane-element-iv-parameters-iv-relation iv-parameters))
	 (gbar-source (element-gbar-source elt nil iv-parameters gbar-inherited)))
    (format nil "~a ~,2e ~A ~a~A"
	    (case iv-relation
	      (:CONSTANT-FIELD "pbar")
	      (t "gbar"))
	    gbar
	    (case iv-relation
	      (:CONSTANT-FIELD "cm3/s")
	      (t  "uS"))
	    (if gbar-inherited "(Type " "(")
	    (case iv-relation
	      (:CONSTANT-FIELD
	       (case gbar-source
		 (:density (format nil "Den-ref ~,2e 1.0e-6cm3/s/um^2)" (element-pbar-density elt)))
		 (:absolute (format nil "Ab-ref)"))))
	      (t
	       (case gbar-source
		 (:density (format nil "Dens-ref ~,2e pS/um^2)" (element-gbar-density elt)))
		 (:absolute (format nil "Ab-ref)"))))))))
	    



;; Returns a double float.
(defun membrane-element-gbar/perm-reference-value (element)
  (let ((element (element element)))
    (typecase element
      (synapse (synapse-gbar/perm-reference-value element))
      (channel (channel-gbar/perm-reference-value element)))))


(proclaim '(inline get-element-gbar-reference))
(defun get-element-gbar-reference (element)
  (let* ((elt (element element))
	 (iv-parameters (type-iv-parameters elt (element-type elt)))
	 (gbar-inherited (inherit-parameters-from-type elt))
	 (element-parameters (unless gbar-inherited (element-parameters elt))))
    (the df
	 (if gbar-inherited
	     (d-flt
	      (the sf
		   (case (element-gbar-source elt element-parameters iv-parameters gbar-inherited)
		     ;; sf
		     (:absolute
		      (membrane-element-iv-parameters-gbar-ref iv-parameters))
		     ;; sf
		     (:density
		      (g-element (gbar-element-cell-element elt)
				 (membrane-element-iv-parameters-gbar-density iv-parameters)))
		     (t 0.0))))
	     (membrane-element-gbar/perm-reference-value element) ; df
	     ))))

(proclaim '(inline get-element-pbar-reference))
(defun get-element-pbar-reference (element)
  (get-element-gbar-reference element))

(defun modulate-type-gbar-reference (element modulation)
  (let* ((type (element-type element))
	 (iv-parameters (type-iv-parameters nil type)))
    (case (membrane-element-iv-parameters-gbar-source iv-parameters)
      (:absolute (setf (membrane-element-iv-parameters-gbar-ref iv-parameters)
		       (* modulation (membrane-element-iv-parameters-gbar-ref iv-parameters))))
      (:density (setf (membrane-element-iv-parameters-gbar-density iv-parameters)
		      (* modulation (membrane-element-iv-parameters-gbar-density iv-parameters)))))))

(defun modulate-type-pbar-reference (element modulation)
  (modulate-type-gbar-reference element modulation))


(defun update-g-type-cell-type-gbar-coefficient (type)
  (let ((type (or (element type 'synapse-type)
		  (element type 'channel-type))))
    (when type
      (let ((element-type-parameters (element-parameters type)))
	(loop for cell-type in (cell-types) do
	      (element-type-cell-type-gbar-coefficient type cell-type t element-type-parameters)))))
  nil)

(defun element-type-cell-type-gbar-coefficient (element-type cell-type &optional always-update element-type-parameters)
  (let* ((element-type-parameters (or element-type-parameters (element-parameters element-type)))
	 (gbar-coeffs (element-parameter-fast 'gbar-coefficients element-type-parameters))
	 (assoc-result (assoc cell-type gbar-coeffs)))
    (or (and (not always-update) (cdr assoc-result))
	(let ((result (if *ignore-q10*
			  (cell-type-global-membrane-conductance-factor cell-type)
			  (the sf (* (cell-type-global-membrane-conductance-factor cell-type)
				     (q10-factor (element-type-reference-temp-kelvin element-type)
						  *temperature* (element-type-q10-gbar element-type)))))))
	  (if (cdr assoc-result)
	      (rplacd assoc-result result)
	      (set-element-parameter-fast element-type 'gbar-coefficients (acons cell-type result gbar-coeffs)
					  element-type-parameters))
	  result))))

(defun element-type-cell-type-pbar-coefficient (element-type cell-type &optional always-update element-type-parameters)
  (element-type-cell-type-gbar-coefficient element-type cell-type always-update element-type-parameters))

;; Just checks types in circuit
(defun non-unity-channel-type-gbar-modulation-p ()
  (loop for type in (channel-types)
	unless (or (not (element-in-circuit type)) (= (or (element-parameter type 'gbar-modulation) 1.0) 1.0))
	do (return t)))

(defun non-unity-channel-type-pbar-modulation-p ()
  (non-unity-channel-type-gbar-modulation-p))

(defun channel-types-with-non-unity-gbar-modulation ()
  (loop for type in (channel-types)
	unless (or (not (element-in-circuit type)) (= (or (element-parameter type 'gbar-modulation) 1.0) 1.0))
	collect type))

(defun channel-types-with-non-unity-pbar-modulation ()
  (channel-types-with-non-unity-gbar-modulation))



(defun transfer-type-gbar-modulation-to-gbars (class)
  (let ((reset-flag nil))
    (loop for type in (case class
			(channel-type (channel-types))
			(synapse-type (synapse-types)))
	  unless (or (not (element-in-circuit type)) (= (or (element-parameter type 'gbar-modulation)
							    (element-parameter type 'pbar-modulation)
							    1.0) 1.0))
	  do
	  (modulate-type-gbar-reference type (or (element-parameter type 'gbar-modulation)
						 (element-parameter type 'pbar-modulation)))
	  (cond ((element-parameter type 'gbar-modulation)
		 (element-parameter type 'gbar-modulation 1.0))
		((element-parameter type 'pbar-modulation)
		 (element-parameter type 'pbar-modulation 1.0)))
		
	  (setq reset-flag t))
    (when reset-flag (setq *recheck-circuit-elements-parameters* t))))

(defun transfer-type-pbar-modulation-to-pbars (class)
  (transfer-type-gbar-modulation-to-gbars class))


;; Just resets types in circuit
(defun reset-non-unity-channel-type-gbar-modulation ()
  (reset-non-unity-type-gbar-modulation 'channel-type))


(defun reset-non-unity-type-gbar-modulation (class)
  (let ((reset-flag nil))
    (loop for type in (case class
			(channel-type (channel-types))
			(synapse-type (synapse-types)))
	  unless (or (not (element-in-circuit type)) (= (or (element-parameter type 'gbar-modulation)
							    (element-parameter type 'pbar-modulation)
							    1.0) 1.0))
	  do (cond ((element-parameter type 'gbar-modulation)
		    (element-parameter type 'gbar-modulation 1.0))
		   ((element-parameter type 'pbar-modulation)
		    (element-parameter type 'pbar-modulation 1.0)))
	  (setq reset-flag t))
    (when reset-flag (setq *recheck-circuit-elements-parameters* t))))

;; Just checks types in circuit
(defun non-unity-synapse-type-gbar-modulation-p ()
  (loop for type in (synapse-types)
	unless (or (not (element-in-circuit type)) (= (or (element-parameter type 'gbar-modulation) 1.0) 1.0))
	do (return t)))

(defun non-unity-synapse-type-pbar-modulation-p ()
  (non-unity-synapse-type-gbar-modulation-p))

(defun synapse-types-with-non-unity-gbar-modulation ()
  (loop for type in (synapse-types)
	unless (or (not (element-in-circuit type)) (= (or (element-parameter type 'gbar-modulation) 1.0) 1.0))
	collect type))

(defun synapse-types-with-non-unity-pbar-modulation ()
  (synapse-types-with-non-unity-gbar-modulation))


;; Just resets types in circuit
(defun reset-non-unity-synapse-type-gbar-modulation ()
  (reset-non-unity-type-gbar-modulation 'synapse-type))

(defun element-gbar-slot (element)
  (let ((element (element element)))
    (typecase element
      (channel (channel-gbar element))
      (synapse (synapse-gbar element)))))

(defun element-pbar-slot (element)
  (element-gbar-slot element))


(defun fixup-type-modulation (type)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let ((element-parameters (element-parameters-fast type)))
    (cond-every
     ((get-a-value 'gbar-modulation element-parameters)
      (typecase (get-a-value 'gbar-modulation element-parameters)
	(single-float nil)
	(t (set-element-parameter-fast
	    type
	    'gbar-modulation
	    (s-flt (get-a-value 'gbar-modulation element-parameters))
	    element-parameters))))
     ((get-a-value 'pbar-modulation element-parameters)
      (typecase (get-a-value 'pbar-modulation element-parameters)
	(single-float nil)
	(t (set-element-parameter-fast
	    type
	    'pbar-modulation
	    (s-flt (get-a-value 'pbar-modulation element-parameters))
	    element-parameters))))))
  nil)


    
(proclaim '(inline element-gbar))
(defun element-gbar (element &optional cell-type always-update element-type)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let* ((type (or element-type (element-type element)))
	 (element-type-parameters (element-parameters-fast type))
	 (gbar-modulation (the sf (or (get-a-value 'gbar-modulation element-type-parameters)
				      (get-a-value 'pbar-modulation element-type-parameters)
				      1.0))))
    (unless (membrane-iv-element-p type)
      (sim-error (format nil "~A is not a membrane IV element!" (element-name element))))
    (* gbar-modulation
       (the sf
	    (if cell-type 
		(the sf (element-type-cell-type-gbar-coefficient type cell-type always-update element-type-parameters))
		1.0))
       (the df (get-element-gbar-reference element)))))
       


(defun element-pbar (element &optional cell-type always-update element-type)
  (element-gbar element cell-type always-update element-type))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Element iv/temp parameters
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun type-iv-parameters (elt &optional type)
  (let ((type (element-type (or type elt))))
    (typecase type
      (channel-type (channel-type-iv-parameters type))
      (synapse-type (synapse-type-iv-parameters type)))))

(defun inherit-parameters-from-type (elt &optional type)
  (let ((elt (element elt type)))
    (typecase elt
      (channel (channel-inherit-parameters-from-type elt))
      (synapse (synapse-inherit-parameters-from-type elt))
      (segment (segment-inherit-parameters-from-type elt)))))


(defun element-type-reference-temp (element-type)
  (typecase element-type
    (channel-type (channel-type-reference-temp element-type))
    (synapse-type (synapse-type-reference-temp element-type))
    (t (if (element-type-p element-type)
	   *Temp-celcius*
	   (let ((type (element-type element-type)))
	     (if type (element-type-reference-temp type) *Temp-celcius*))))))

(defun element-type-reference-temp-kelvin (element-type)
  (+ 273.16 (element-type-reference-temp element-type)))

(defun element-type-q10-gbar (element-type)
  (typecase element-type
    (channel-type (channel-type-q10 element-type))
    (synapse-type (synapse-type-q10 element-type))
    (t (if (element-type-p element-type)
	   1.0
	   (let ((type (element-type element-type)))
	     (if type (element-type-q10-gbar type) 1.0))))))

(defun element-type-q10-pbar (element-type)
  (element-type-q10-gbar element-type))


(defun set-synapse-parameters (synapse &optional (update-fixed-e-rev t) (update-gbar t))
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (setf (synapse-gbar synapse)
	(element-gbar synapse (cell-type (synapse-cell synapse)) update-gbar (synapse-type synapse)))
  (when update-fixed-e-rev (update-element-fixed-e-rev synapse))
  nil)

(defun set-channel-parameters (channel &optional (update-fixed-e-rev t) (update-gbar t))
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (setf (channel-gbar channel)
	(element-gbar channel (cell-type (channel-cell channel)) update-gbar (channel-type channel)))
  (when update-fixed-e-rev (update-element-fixed-e-rev channel nil (channel-type channel)))
  nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun soma-or-segment (elt)
  (typecase (element elt)
    ((or soma segment) t)))


(defun member-of-node-plot-lists (elt)
  (let ((node-name (element-name (element-node elt))))
    (loop for plot-list in (list *plot-path-nodes* *plot-nodes* *plot-soma-nodes* *analysis-nodes*)
	  when (string-member node-name plot-list) do (return t))))


(defun somas-and-segments (&optional (cells (cells)) select-plotted (only-segments t) (only-somas nil)
				     only-connected)
  "Returns a list of all segments and somas associated with the cell or cells referenced by CELLS
\(can be a single cell or a list\) [default all cells in circuit]. Additional optional arguments are
self-explanatory."
  (let* ((cells (flatten-no-nils-list (element-cell cells)))
	 (result (unless only-somas
		   (loop for electrode in (electrodes)
			 when (and (member (element-cell electrode) cells)
				   (or (not select-plotted) (member-of-node-plot-lists electrode)))
			 collect electrode))))
    (loop for cell in cells
	  unless only-somas do
	  (loop for segment in (cell-segments cell)
		when (and (or (not only-connected) (proximal-segment segment))
			  (or (not select-plotted) (member-of-node-plot-lists segment)))
		do (push segment result))
	  unless only-segments do
	  (let ((soma (cell-soma cell)))
	    (when (or (not select-plotted) (member-of-node-plot-lists soma)) (push soma result))))
    result))

(defun segments-not-electrodes (&optional (cell *cell*))
  (let* ((electrodes (electrodes))
	 (cell (element cell 'cell)))
    (loop for seg in (if cell (cell-segments cell) (segments))
	  unless (member seg electrodes) collect seg)))



(defun cell-elements (&optional (cells (cells)))
  "Returns a list of all segments and somas associated with the cell or cells referenced by CELLS \(can be a
single cell or a list\) [default all cells in circuit]."
  (loop for cell in (coerce-to-list (element cells 'cell))
	collect (cell-soma cell) into out
	nconc (copy-list (cell-segments cell)) into out
	finally (return out)))


(defun all-cell-elements ()
  (concatenate 'list (copy-list (somas)) (copy-list (segments))))

(defun all-somas-and-segments ()
  (somas-and-segments (cells) nil nil))


;; MAKE-NODE-W/ELEMENTS-ARRAY:
;; Set the global array *NODE-W/ELEMENTS-ARRAY* to point to all nodes with membrane elements or ones
;; for which the voltage dvdt will be plotted. It is these node that are considered in estimating the
;; maximum LTE in CALC-VOLTAGE-LTE-RATIO. Note that CALC-VOLTAGE-LTE-RATIO also updates the voltage derivative, which
;; is used for the particle evaluation.  Since all nodes with particles \(channels\) are included in
;; *NODE-W/ELEMENTS-ARRAY*, then these derivatives will be available for the proper nodes.
(defun make-node-w/elements-array (&optional always)
  (when (or always *make-node-w/elements-array*)
    (let* ((lte-node-criterium (coerce-to-list *lte-node-criterium*))
	   (all-driven-elements (member :all lte-node-criterium))
	   (axons (member :axons lte-node-criterium))
	   (vsource-driven-elements (member :vsources lte-node-criterium))
	   (isource-driven-elements (member :isources lte-node-criterium))
	   (synapse-driven-elements (member :synapses lte-node-criterium))
	   (channel-driven-elements (member :channels lte-node-criterium))
	   (somas (member :somas lte-node-criterium))
	   (explicit-elements (coerce-to-list (element-node lte-node-criterium))))
      (setq *node-w/elements-array*
	    (list-to-array-generic
	     (delete-duplicates
	      (nconc
	       (when (or all-driven-elements vsource-driven-elements)
		 (loop for elt in 
		       (loop for src in (vsources)
			     nconcing (typecase (vsource-cell-element src)
					(segment (cons (proximal-cell-element src) (distal-segments src)))
					(soma (nconc (soma-segments src) (trunk-segments src)))))
		       collect (element-physical-node elt)))
	       (loop for node in (nodes)
		     when (and
			   (not (equal node *ground-node*))
			   (or
			    (member node explicit-elements)
			    (loop for elt in (node-elements node)
				  when (and (soma-p elt) somas) do (return t)
				  unless (or (cell-element-p elt) (not (element-enabled-p elt)))
				  do
				  (typecase elt
				    (synapse (when (and (or all-driven-elements synapse-driven-elements)
							(not (= 0.0 (synapse-gbar elt))))
					       (return t)))
				    (channel (when (and (or all-driven-elements channel-driven-elements)
							(not (= 0.0 (channel-gbar elt))))
					       (return t)))
				    (axon (when (or all-driven-elements axons) (return t)))
				    (vsource (when (or *include-vsource-nodes-in-node-error-est*
						       all-driven-elements vsource-driven-elements
						       ;; If there is only a soma, still include this node.
						       (not (segments)))
					       (return t)))
				    (isource (when (or all-driven-elements isource-driven-elements)
					       (return t))))
				  finally (return nil))))
		     collect node)))))
      (setq *node-w/elements-array-length*  (length *node-w/elements-array* )
	    *make-node-w/elements-array* nil))))


;; ***** ***** ***** ***** ***** ***** *****
;;
;; Anatomy geometric hacks.
;;
;; ***** ***** ***** ***** ***** ***** *****

(defun element-location (element &optional type)
  "Returns the XYZ coordinates [microns] of the cell element node associated with ELEMENT of TYPE."
  (atomize-list (loop for elt in (coerce-to-list element) collect (element-absolute-location elt type))))

(defun where (element &optional type)
    "Returns the XYZ coordinates [microns] of the cell element node associated with ELEMENT of TYPE."
  (element-location element type))

(defun where-val (element type index)
  (let ((location (element-location element type)))
    (typecase (car location)
      (cons (loop for loc in location collect (nth index loc)))
      (t (nth index location)))))

(defun where-x (element &optional type)
  "Returns the X coordinate [microns] of the cell element node associated with ELEMENT of TYPE."
  (where-val element type 0))

(defun where-y (element &optional type)
  "Returns the Y coordinate [microns] of the cell element node associated with ELEMENT of TYPE."
  (where-val element type 1))

(defun where-z (element &optional type)
  "Returns the Z coordinate [microns] of the cell element node associated with ELEMENT of TYPE."
  (where-val element type 2))


(defun element-absolute-location (element &optional type)
  (let ((element-internal (element element type)))
    (typecase element-internal
      (EXTRACELLULAR-ELECTRODE (EXTRACELLULAR-ELECTRODE-absolute-location element-internal))
      (t (let ((node (element-physical-node element-internal type)))
	   (if node
	       (or
		(node-absolute-location node)
		(progn (process-circuit-structure)
		       (or (node-absolute-location node)
			   (sim-error "The cell containing ~A has not been processed. Run PROCESS-CIRCUIT-STRUCTURE."
				      node))))
	       (format t "~A has no node." element)))))))

(defun element-relative-location (element &optional type)
  (atomize-list
   (loop for elt in (coerce-to-list element) collect
	 (let ((node (element-physical-node elt type)))
	   (when node (node-relative-location node))))))

(defun as-the-crow-flies (location-1 location-2)
  "Returns the straight line distance between LOCATION-1 and LOCATION-2, where the arguments can
either be references to circuit elements or explicit location lists \(X Y Z\)."
  (if (and location-1 location-2)
      (cartesian-distance-3d-float
       (typecase location-1
	 (cons location-1) 
	 (t (element-absolute-location (element location-1))))
       (typecase location-2
	 (cons location-2) 
	 (t (element-absolute-location (element location-2)))))
      0.0))

(defun element-cloud (reference-element cloud-radius &optional
					restrict-to-reference-element-cell
					returned-type)
  "Return a list of elements of RETURNED-TYPE [somas and segments if this is NIL, the default] that
are within CLOUD-RADIUS [microns] of REFERENCE-ELEMENT. Candidate returned elements are restricted
to the cell associated with REFERENCE-ELEMENT when RESTRICT-TO-REFERENCE-ELEMENT-CELL is non-nil."
  (let ((reference-element (element-cell-element reference-element))
	(reference-element-cell (element-cell reference-element)))
    (loop for elt in
	  (or (loop for elt in 
		    (or (hash-table-list (get-type-hash-table returned-type))
			(elements-of-type returned-type))
		    when (or (not restrict-to-reference-element-cell)
			     (eq (element-cell elt) reference-element-cell))
		    collect elt)
	      (cell-elements (if restrict-to-reference-element-cell reference-element-cell (cells))))
	  when (< (as-the-crow-flies reference-element elt) cloud-radius)
	  collect elt)))
	
(defun segs-distance-elts (candidates element exclude-these-elements element-point)
  (let ((cell (element-cell element)))
    (sort 
     (loop for reference in (or candidates (if cell (cell-segments cell) (segments)))
	   unless (or (eq reference element)
		      (loop for elt in exclude-these-elements
			    when (eq elt reference) do (return t)))
	   collecting (list (as-the-crow-flies element-point reference) reference))
     '< :key 'car)))


;; CLOSEST-ELEMENT Given a soma or segment ELEMENT, returns the closest soma or segment in the same
;; cell (element distance).
#|
(defun closest-ELEMENT (element &key exclude-these-elements proximal-measure (check-exclusions t) candidates)
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (let* ((element (element-cell-element element))
	 (cell (element-cell element))
	 (exclude-these-elements
	  (if check-exclusions
	      (loop for elt in exclude-these-elements collect (element elt))
	      exclude-these-elements))
	 (element-point (if proximal-measure
			    (node-absolute-location (typecase element
						      (segment (segment-node-1 element))
						      (t (soma-node element))))
			    (element-absolute-location element)))
	 ;; (....(distance-from-point element)...)
	 (segs-distance-elts (segs-distance-elts candidates element exclude-these-elements element-point))
	 (candidate-distance))
    (loop for segs-distance-elt
	  in (if (or (eq (cell-soma cell) element) (member (cell-soma cell) exclude-these-elements :test 'eq))
		 segs-distance-elts
		 (push (list (as-the-crow-flies element-point (cell-soma cell)) (cell-soma cell)) segs-distance-elts))
	  when (or (not candidate-distance)
		   (> (the sf (car candidate-distance)) (the sf (car segs-distance-elt))))
	  do (setq candidate-distance segs-distance-elt)
	  finally (return (values (cadr candidate-distance) (car candidate-distance))))))


;; old working version
(defun closest-ELEMENT (element &key exclude-these-elements proximal-measure (check-exclusions t) candidates)
  "Returns the closest soma or segment to ELEMENT. Also returns the distance between ELEMENT and the
closest cell element."
  (declare (ignore check-exclusions)
	   (optimize (safety 1) (speed 3) (space 1)))
  (let* ((element (element-cell-element element))
	 (exclude-these-elements (coerce-to-list (element exclude-these-elements)))
	 (candidates (or candidates (cell-elements element)))
	 (element-point (if proximal-measure
			    (typecase element
			      (segment (node-absolute-location (segment-node-1 element)))
			      (soma (element-absolute-location element)))
			    (element-absolute-location element)))
	 (distance-elt
	  (car (sort (loop for reference in candidates
			   unless (or (eq reference element)
				      (member reference exclude-these-elements :test 'eq))
			   collecting (list (as-the-crow-flies element-point reference) reference))
		     '< :key 'car))))
    (values (cadr distance-elt) (car distance-elt))))

|#

(defun closest-ELEMENT (element &key exclude-these-elements proximal-measure candidates just-somas)
  "Returns the closest soma or segment to ELEMENT, calculated with AS-THE-CROW-FLIES, taken from cell elements in CANDIDATES and excluding those in
EXCLUDE-THESE-ELEMENTS. ELEMENT may also be an explicit location list \(x y z\), with each value in microns. The second returned value is the distance
between ELEMENT and the closest cell element in microns. If CANDIDATES is not supplied then all segments and the soma of the cell associated with
ELEMENT are used, unless ELEMENT is a location, then all cell elements in the circuit are tested. If PROXIMAL-MEASURE is non-NIL, then the proximal
location of ELEMENT is used, otherwise the distal location is used to calculate the distance metric. If JUST-SOMAS is T, then candidate elements are
restricted to somas."
  (DECLARE (optimize (safety 1) (speed 3) (space 1)))
  (let* ((element (if (consp element) (float-list element) (element-cell-element element)))
	 (exclude-these-elements (coerce-to-list (element exclude-these-elements)))
	 (dummy-candidates (or candidates (cell-elements (if (consp element) (cells) element))))
	 (candidates (if just-somas
		       (loop for elt in dummy-candidates when (soma-p elt) collect elt)
		       dummy-candidates))
	 (element-point (if (consp element)
			  element
			  (if proximal-measure
			    (typecase element
			      (segment (node-absolute-location (segment-node-1 element)))
			      (soma (element-absolute-location element)))
			    (element-absolute-location element))))
	 (ref-distance 0.0)
	 (min-distance 0.0)
	 closest-elt)
    (declare (single-float min-distance ref-distance))
    (loop for reference in candidates
	  unless (or (eq reference element)
		     (member reference exclude-these-elements :test 'eq))
	  do
	  (setq ref-distance (as-the-crow-flies element-point reference))
	  and when (or (< ref-distance min-distance)
		       (not closest-elt))
	  do
	  (setq closest-elt reference
		min-distance ref-distance)
	  finally (return (values closest-elt min-distance)))))
	 



(defun distance-to-soma (element)
  "Given an ELEMENT (name or object), returns the distance along the tree to the soma in microns.
Faster way is to reference the :SEGMENT-DISTANCE-TO-SOMA slot of the segment which is set when the
cell anatomy is first processed."
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((seg (element-cell-element element)))
    (typecase seg
      (segment (if (= (segment-distance-to-soma seg) 0.0) ; This slot hasn't been set yet.
		   (setf (segment-distance-to-soma seg) (+ (segment-length seg)
							   (the sf (distance-to-soma (proximal-segment seg)))))
		   (segment-distance-to-soma seg)))
      (t 0.0))))


(defun tree-radius (&optional (cell *cell*) (defined-as :max))
  "Given the optional CELL (default *CELL*), each of the distal tip segments are compared, and
depending on the setting of DEFINED-AS (default :MAX) the maximum (DEFINED-AS equals :MAX), the
minimum (DEFINED-AS equals :MIN), or the average (DEFINED-AS equals T) of the distances to the soma
of the segments is returned."
  (let ((segments (distal-tips cell)))
    (case defined-as
      (:max (loop for seg in segments maximize (distance-to-soma seg)))
      (:min (loop for seg in segments minimize (distance-to-soma seg)))
      (t				; AVERAGE
       (loop for seg in segments sum (distance-to-soma seg) into sum
	     finally (return (/ sum (length segments))))))))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Parameter display
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun max-segment-area ()
  (loop for seg in (segments) maximize (element-area seg)))



(defun segment-param-distribution (parameter &key cell
					     segments
					     note param-max param-min
					     y-max y-inc
					     bin-width
					     include-y-label
					     (include-simulation-name t)
					     (x-axis-tick-skip  0)
					     x-are-fns
					     (width 800) (height 400) font
					     title-position create-new-window)
  "Plots histogram of properties of all segments either associated with CELL or all segments in
circuit (if CELL is nil). PARAMETER can be:

      'AREA         ->  (element-area seg)
      'DISTANCE     ->  (distance-to-soma seg)
      'LENGTH       ->  (segment-length seg)
      'DIAMETER     ->  (segment-diameter seg)
      'CAPACITANCE

"
  (let* ((*create-new-plot-windows* (or create-new-window *create-new-plot-windows*))
	 (cell (element cell 'cell))
	 (segments (coerce-to-list (or (element segments) (if cell (cell-segments cell) (segments)))))
	 (parameters
	  (loop for seg in segments
		collect (case parameter
			  (area (element-area seg))
			  (distance (DISTANCE-TO-SOMA seg))
			  (length (segment-length seg))
			  (capacitance (segment-capacitance seg))
			  (diameter (segment-diameter seg)))))
	 (min (or param-min (min-of-list parameters)))
	 (max (or param-max (max-of-list parameters)))
	 (bin-width (or bin-width (ceiling (/ max 10))))
	 (total-bins (if bin-width (ceiling (/ max bin-width)) 10))
	 (title (concatenate-strings
			     (when cell (cell-name cell))
			     (when cell " - ")
			     "Segment "
			     (case parameter
			       (distance "Distances to Soma")
			       (t (string-capitalize (format nil "~As" parameter))))
			     (when include-simulation-name (format nil ": ~A" *simulation-name*))))
	 (x-label (case parameter
		    (distance "Distance to Soma [um]")
		    (length "Length [um]")
		    (diameter "Diameter [um]")
		    (area "Membrane Area [um2]")
		    (capacitance "Membrane Capacitance [nF]")))	    
	 (parameter-array (make-array (list total-bins)))
	 (win (find-plot-window :histogram title)))
    (format t "min: ~A max: ~A~%" min max)
    (when win (let (dummy1)
		(choose-variable-values
		 '((dummy1 "Use new histogram window" :boolean))
		 :label (format nil "Menu for ~A" (g-value win :title)))
		(when dummy1 (let ((*create-new-plot-windows* t))
			       (setq win (get-plot-window :histogram title nil))))))
    (loop for parameter in parameters
	  do (let ((index (floor (* (1- total-bins) (/ parameter max)))))
	       (when (< index (length  parameter-array)) (incf (aref parameter-array index)))))
    (plot-histogram (list (loop for x from 0.0 by bin-width
				for i from 1 to total-bins
				collect x)
			  (array-to-list parameter-array))
		    bin-width
		    :stipple-percent 50
		    :x-min 0
		    :x-axis-tick-skip x-axis-tick-skip
		    :x-are-fns x-are-fns
		    :y-max y-max :y-inc y-inc       
		    :x-label x-label :y-label (when include-y-label (format nil "Number of~ASegments"))
		    :font font :title-position title-position :title title :comment note
		    :width width :height height)))



#|
(defun element-param-distribution (type parameter &key cell
					note param-max param-min
					y-max y-inc
					bin-width
					include-y-label
					(include-simulation-name t)
					(x-axis-tick-skip  0)
					x-are-fns
					(width 800) (height 400) font
					title-position create-new-window)
  "Plots histogram of properties of all elements associated with TYPE (associated with CELL, if
supplied, otherwise all in the circuit). TYPE either refers to a class of elements (e.g. 'SEGMENT,
'CHANNEL, 'SYNAPSE) or a specific element type (e.g. a particular channel or synapse type).
PARAMETER must be consistent with TYPE, and for a given ELT of TYPE can include: 

      'AREA         ->  (element-area elt)
      'DISTANCE     ->  (distance-to-soma elt)
      'LENGTH       ->  (segment-length (element-cell-element elt)) [If elt is on a segment]
      'DIAMETER     ->  (element-diameter elt)
      'CAPACITANCE  ->  (element-capacitance elt)
      'GBAR         ->  (element-gbar elt)

"
  (let* ((*create-new-plot-windows* (or create-new-window *create-new-plot-windows*))
	 (cell (element cell 'cell))
	 (elements (elements-of-type (element type)))
	 (parameters
	  (no-nils
	   (loop for elt in elements
		 collect (case parameter
			   (area (element-area elt))
			   (distance (DISTANCE-TO-SOMA elt))
			   (length (element-length elt))
			   (capacitance (element-capacitance elt))
			   (diameter (element-diameter elt))))))
	 (min (or param-min (min-of-list parameters)))
	 (max (or param-max (max-of-list parameters)))
	 (bin-width (or bin-width (ceiling (/ max 10))))
	 (total-bins (if bin-width (ceiling (/ max bin-width)) 10))
	 (title (concatenate-strings
		 (when cell (cell-name cell))
		 (when cell " - ")
		 (format nil "~A " (type-of (car elements)))
		 (case parameter
		   (distance "Distances to Soma")
		   (t (string-capitalize (format nil "~As" parameter))))
		 (when include-simulation-name (format nil ": ~A" *simulation-name*))))
	 (x-label (case parameter
		    (distance "Distance to Soma [um]")
		    (length "Length [um]")
		    (diameter "Diameter [um]")
		    (area "Membrane Area [um2]")
		    (capacitance "Membrane Capacitance [nF]")))	    
	 (parameter-array (make-array (list total-bins)))
	 (win (find-plot-window :histogram title)))
    (format t "min: ~A max: ~A~%" min max)
    (when win (let (dummy1)
		(choose-variable-values
		 '((dummy1 "Use new histogram window" :boolean))
		 :label (format nil "Menu for ~A" (g-value win :title)))
		(when dummy1 (let ((*create-new-plot-windows* t))
			       (setq win (get-plot-window :histogram title nil))))))
    (loop for parameter in parameters
	  do (let ((index (floor (* (1- total-bins) (/ parameter max)))))
	       (when (< index (length  parameter-array)) (incf (aref parameter-array index)))))
    (plot-histogram (list (loop for x from 0.0 by bin-width
				for i from 1 to total-bins
				collect x)
			  (array-to-list parameter-array))
		    bin-width
		    :stipple-percent 50
		    :x-min 0
		    :x-axis-tick-skip x-axis-tick-skip
		    :x-are-fns x-are-fns
		    :y-max y-max :y-inc y-inc       
		    :x-label x-label :y-label (when include-y-label (format nil "Number of~ASegments"))
		    :font font :title-position title-position :title title :comment note
		    :width width :height height)))

|#

(defun element-param-distribution (type parameter &key cell
					note param-max param-min
					y-max y-inc
					bin-width
					(include-simulation-name t)
					(x-axis-tick-skip  0)
					x-are-fns
					(width 800) (height 400) font
					title-position create-new-window)
  "Plots histogram of properties of all elements associated with TYPE (associated with CELL, if
supplied, otherwise all in the circuit). TYPE either refers to a class of elements (e.g. 'SEGMENT,
'CHANNEL, 'SYNAPSE) or a specific element type (e.g. a particular channel or synapse type).
PARAMETER must be consistent with TYPE, and for a given ELT of TYPE can include: 

 'AREA         =>  (element-area elt)
 'DISTANCE     =>  (distance-to-soma elt)
 'DIAMETER     =>  (element-diameter elt)
 'CAPACITANCE  =>  (element-capacitance elt)
 'GBAR         =>  (element-gbar elt)

If ELT is on a segment, then:

 'LENGTH       =>  (segment-length (element-cell-element elt))

"
  (let* ((*create-new-plot-windows* (or create-new-window *create-new-plot-windows*))
	 (cell (element cell 'cell))
	 (elements (elements-of-type (element type)))
	 (parameters (no-nils (loop for elt in elements
				    collect (case parameter
					      (area (element-area elt))
					      (distance (DISTANCE-TO-SOMA elt))
					      (length (element-length elt))
					      (capacitance (element-capacitance elt))
					      (diameter (element-diameter elt)))))))
    (when parameters
      (let* ((min (or param-min (min-of-list parameters)))
	     (max (or param-max (max-of-list parameters)))
	     (bin-width (or bin-width (ceiling (/ max 10))))
	     (total-bins (if bin-width (ceiling (/ max bin-width)) 10))
	     (actual-types
	      (typecase (element type)
		(cons (loop for thing in (element type)
			    when (element-in-circuit thing) collect thing))
		(t (element type))))
	     (title (concatenate-strings
		     (when cell (cell-name cell))
		     (when cell " - ")
		     (typecase actual-types
		       (cons (if (< (length actual-types) 4)
				 (loop for thing in actual-types collect
				       (format nil "~A " (element-name thing)))
				 (format nil "~A " (type-of (car elements)))))
		       (t (format nil "~A " (element-name actual-types))))
		     (case parameter
		       (distance "Distances to Soma")
		       (t (string-capitalize (format nil "~As" parameter))))
		     (when include-simulation-name (format nil ": ~A" *simulation-name*))))
	     (x-label (case parameter
			(distance "Distance to Soma [um]")
			(length "Length [um]")
			(diameter "Diameter [um]")
			(area "Membrane Area [um2]")
			(capacitance "Membrane Capacitance [nF]")))	    
	     (parameter-array (make-array (list total-bins)))
	     (win (find-plot-window :histogram title)))
;;	(format t "min: ~A max: ~A~%" min max)
	(when win (let (dummy1)
		    (choose-variable-values
		     '((dummy1 "Use new histogram window" :boolean))
		     :label (format nil "Menu for ~A" (g-value win :title)))
		    (when dummy1 (let ((*create-new-plot-windows* t))
				   (setq win (get-plot-window :histogram title nil))))))
	(loop for parameter in parameters
	      do (let ((index (floor (* (1- total-bins) (/ parameter max)))))
		   (when (< index (length parameter-array))
		     (incf (aref parameter-array index)))))
	(plot-histogram (list (loop for x from 0.0 by bin-width
				    for i from 1 to total-bins
				    collect x)
			      (array-to-list parameter-array))
			bin-width
			:stipple-percent 50
			:x-min 0 :x-axis-tick-skip x-axis-tick-skip :x-are-fns x-are-fns
			:y-max y-max :y-inc y-inc       
			:x-label x-label
			:y-label "# Elements"
			:font font :title-position title-position :title title :comment note
			:width width :height height)))))


		    
(defun segment-max-info (&optional cell)
  (loop for cell in (coerce-to-list (or (element cell 'cell) (cells))) do
	(loop for segment in (cell-segments cell)
	      collecting (list Segment (segment-length segment)) into segslens
	      collecting (list Segment (element-area segment)) into segsareas
	      collecting (list Segment (segment-diameter segment)) into segsdiams
	      finally
      
	      (format t "**** Segment with Max length ****~%")
	      (print-element (caar (sort segslens `> :key 'cadr)))
	      (format t "~%**** Segment with Max area ****~%")
	      (print-element (caar (sort segsareas `> :key 'cadr)))
	      (format t "~%**** Segment with Max diameter ****~%")
	      (print-element (caar (sort segsdiams `> :key 'cadr))))))


(defun distal-tips (&optional cell)
  "Return a list of all distal tip segments associated with CELL, if supplied, otherwise, all distal
tips in circuit."
  (let ((cell (element cell 'cell)))
    (loop for seg in (if cell (cell-segments cell) (segments))
	  when (= (length (distal-segments seg)) 0) collect seg)))


(defun segments-out (element &optional (segment-skip 0) previous-segs)
  "Starting with the cell element associated with ELEMENT, returns a list of all the segments moving
distally, skipping by SEGMENT-SKIP [default 0]."
  (let* ((segment (element-cell-element element))
	 (result
	  (if (distal-segments segment)
	      (cons segment
		    (loop for seg in (distal-segments segment)
			  nconc
			  (copy-list (segments-out seg segment-skip (cons segment previous-segs)))))
	      (list segment))))
    (if (= segment-skip 0)
	result
	(loop for segment in result
	      for count from 0
	      when (= (mod count (1+ segment-skip)) 0) collect segment))))

(defun segments-in (element &optional (segment-skip 0))
  "Returns an inclusive list of all the segments starting from the segment associated with ELEMENT on
the path to the soma, skipping by SEGMENT-SKIP [default 0]."
  (let ((segment (element-cell-element element)))
    (when (segment-p segment)
      (let ((result (if (proximal-segment segment)
			(nconc (segments-in (proximal-segment segment) nil) (list segment))
			(list segment))))
	(if segment-skip
	    (loop for segment in (reverse result)
		  for count from 0
		  when (= (mod count (1+ segment-skip)) 0)
		  collect segment)
	    result)))))

(defun check-loop-at-seg (segment &optional previous-segs)
  (when segment
    (format t "Proximal-segment for ~A: ~A~%" segment (proximal-segment segment))
    (if (member segment previous-segs)
	(progn (format t "Found loop ~A ~A~%" segment previous-segs)
	       (break))
	(check-loop-at-seg (proximal-segment segment)
			   (if previous-segs
			       (cons segment previous-segs)
			       (list segment))))))

  
(defun segments-to-soma (segment &optional (segment-skip 0))
  (segments-in segment segment-skip))

(defun trunk-segment (element)
  "Returns the trunk segment associated with the dendritic branch that includes ELEMENT."
  (let ((element (element-cell-element element)))
    (if (proximal-segment element)
	(trunk-segment (proximal-segment element))
	element)))

(defun trunk-segments (&optional element)
  "Return a list of trunk segments for the cell associated with ELEMENT, if supplied, otherwise all
in circuit."
  (loop for cell in (coerce-to-list (or (element-cell element) (cells)))
	nconcing
	(let* ((soma (cell-soma cell))
	       (soma-segments (soma-segments soma)))
	  (nconc
	   ;; Pick up segments abutting those assigned to the "soma".
	   (loop for seg in soma-segments nconc
		 (loop for seg in (distal-segments seg)
		       unless (member seg soma-segments)
		       collect seg))
	   ;; Pick up segments abutting soma which are not assigned to the "soma".
	   (true-soma-trunks soma)))))
	   


;; Pick up segments abutting soma which are not assigned to the "soma".
(defun true-soma-trunks (soma)
  (let ((soma (element soma 'soma)))
    (when soma
      (let ((soma-segments (soma-segments soma)))
	(loop for elt in (node-elements (soma-node soma))
	      when (and (segment-p elt) (not (member elt soma-segments)))
	      collect elt)))))

(defun PRIMARY-SEGS (&optional element)   
  "Returns a list of all segments of the cell of ELEMENT, if supplied, otherwise all in circuit, that
are proximal to the first branch point."
  (flet ((primary-segs-internal (cell) (loop for seg in (distal-segments (cell-soma cell))
					     append (segs-until-bifurcation seg))))
    (let ((cell-ref (or (element-cell element) (cells))))
      (if (listp cell-ref)
	  (loop for cell in cell-ref nconc (primary-segs-internal cell))
	  (primary-segs-internal cell-ref)))))



(defun soma-segments (&optional target)
  "Returns a list of segments which are conceptually assigned to the actual cell soma."
  (when *cell* (element-parameter (cell-soma (or (element-cell target) *cell*)) 'segments)))

  
(defun cell-distal-segments (&optional (cell *cell*))
  "Returns a list of all the distal segments of CELL [default *CELL*]."
  (let ((cell (element cell 'cell)))
    (loop for seg in (if cell (cell-segments cell) (segments))
	  unless (distal-segments seg)
	  collect seg)))

(defun count-cell-distal-segments (&optional (cell *cell*))
  (let ((cell (element cell 'cell)))
    (loop for seg in (if cell (cell-segments cell) (segments))
	  unless (distal-segments seg)
	  sum 1)))

(defun random-segments (percent-of-total &optional (cell *cell*))
  (let* ((cell (element cell 'cell))
	 (segments (if cell (cell-segments cell) (segments))))
    (RANDOM-SUBSEQ segments (round (* .01 percent-of-total (length segments))))))




(defun SET-ELEMENT-MEMBRANE-PARAMETERS (element &optional ignore-membrane-elements)
  "Use when ELEMENT dimensions or gbar ref change."
  (typecase element
    (segment (set-segment-membrane-parameters element ignore-membrane-elements))
    (soma (set-soma-membrane-parameters element ignore-membrane-elements))
    (axon (set-axon-parameters nil element))
    (channel (SET-channel-PARAMETERS element))
    (synapse (SET-synapse-pARAMETERS element))
    (conc-int (set-conc-integrator-parameters nil element)))
  nil) 

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-node-elements-of-type (element types)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let* ((nodes (loop for elt in (coerce-to-list (element-cell-element element))
		      collect (typecase elt
				(soma (soma-node elt))
				(segment (segment-node-2 elt)))))
	 (types (typecase types
		  (cons (loop for thing in types collect
			      (or (find thing *object-type-symbols*)
				  (element-type thing)
				  thing)))
		  (t (list (or (find types (the cons *object-type-symbols*))
			       (element-type types)
			       types)))))
	 (things
	  (loop for node in nodes nconc
		(let (value)
		  (loop for elt in (node-elements node)
			collect (loop for type in types
				      when (setq value
						 (typecase type
						   (symbol
						    (cond ((eq (type-of elt) type) elt)
							  ((and (member type *type-model-symbols*)
								(eq (type-of (element-type elt)) type)) (element-type elt))))
						   (t (when (eq (element-type elt) type) elt))))
				      do (return value)))))))
    (clean-up-list things)))
	   

;; Need GET-NODE-ELEMENTS-OF-TYPE since its referenced many places... 7/4/98
(defun cell-element-elements (element types)
    "Returns a list of elements associated with the cell element of ELEMENT whose type is found in TYPES
\(list or atom\). TYPES can include specific element types \(e.g. specific channel or synapse type\)
or classes of types \(e.g. 'SYNAPSE or 'CONC-INT\)."
  (get-node-elements-of-type element types))

(defun node-elements-of-type (element types)
  (get-node-elements-of-type element types))


(defun node-segments (element)
"Return all segments associated with the cell element of ELEMENT"
  (get-node-elements-of-type element 'segment))


(defun node-somas (element)
"Return all somas associated with the cell element of ELEMENT"
  (get-node-elements-of-type element 'soma))


(defun node-channels (element)
"Return all channels associated with the cell element of ELEMENT"
  (get-node-elements-of-type element 'channel))


(defun node-synapses (element)
"Return all synapses associated with the cell element of ELEMENT"
  (get-node-elements-of-type element 'synapse))


(defun node-cells (element)
"Return all cells associated with the cell element of ELEMENT"
  (get-node-elements-of-type element 'cell))


(defun node-buffers (element)
"Return all buffers associated with the cell element of ELEMENT"
  (get-node-elements-of-type element 'buffer))


(defun node-pumps (element)
"Return all pumps associated with the cell element of ELEMENT"
  (get-node-elements-of-type element 'pump))


(defun node-isources (element)
"Return all isources associated with the cell element of ELEMENT"
  (get-node-elements-of-type element 'isource))


(defun node-conc-ints (element)
"Return all conc-ints associated with the cell element of ELEMENT"
  (get-node-elements-of-type element 'conc-int))


(defun node-particles (element)
"Return all particles associated with the cell element of ELEMENT"
  (get-node-elements-of-type element 'particle))


(defun node-conc-particles (element)
"Return all conc-particles associated with the cell element of ELEMENT"
  (get-node-elements-of-type element 'conc-particle))


(defun node-axons (element)
"Return all axons associated with the cell element of ELEMENT"
  (get-node-elements-of-type element 'axon))


(defun node-vsources (element)
"Return all vsources associated with the cell element of ELEMENT"
  (get-node-elements-of-type element 'vsource))


(defun node-extracellular-electrodes (element)
"Return all extracellular-electrodes associated with the cell element of ELEMENT"
  (get-node-elements-of-type element 'extracellular-electrode))


#|
(defun cell-element-element (cell-element type what-kind)
  (let* ((type (or type what-kind))
	 (element-type (coerce-to-list (element-type type))))
    (when element-type
      (atomize-list
       (loop for elt in (node-elements (element-physical-node cell-element))
	     when (and (case what-kind
			 (conc-int (conc-int-p elt))
			 (pump (pump-p elt))
			 (buffer (buffer-p elt))
			 (channel (channel-p elt))
			 (particle (particle-p elt))
			 (conc-particle (conc-particle-p elt)))
		       (member (element-type elt) element-type))
	     collect elt)))))
|#


#|

;;; This isn't quite right....
(loop for sym in *object-type-symbols* collect
      (let ((name (read-from-string (format nil "cell-element-~As-test" sym)))
	    (doc (concatenate-strings
		  (format nil "Returns all the ~As associated with CELL-ELEMENT that are of TYPE," sym)
		  (format nil " if included, otherwise of all ~A types." sym))))
	(eval (list 'defun name
		    '(cell-element  &optional type)
		    doc
		    `(cell-element-elements cell-element (or type ,sym))))))

|#


      
(defun element-isources (element &optional type)
  "Returns all the isources associated with the cell element of ELEMENT that are of TYPE, if included,
otherwise of all isource types."
  (cell-element-elements element (or type 'isource)))

(defun element-vsources (element &optional type)
  "Returns all the vsources associated with the cell element of ELEMENT that are of TYPE, if included,
otherwise of all vsource types."
  (cell-element-elements element (or type 'vsource)))

(defun element-synapses (element &optional type)
  "Returns all the synapses associated with the cell element of ELEMENT that are of TYPE, if included,
otherwise of all synapse types."
  (cell-element-elements element (or type 'synapse)))

(defun element-conc-ints (element &optional type)
  "Returns all the conc-ints associated with the cell element of ELEMENT that are of TYPE, if
included, otherwise of all conc-int types."
  (cell-element-elements element (or type 'conc-int)))

(defun element-buffers (element &optional type)
  "Returns all the buffers associated with the cell element of ELEMENT that are of TYPE, if included,
otherwise of all buffer types."
  (cell-element-elements element (or type 'buffer)))

(defun element-pumps (element &optional type)
  "Returns all the pumps associated with the cell element of ELEMENT that are of TYPE, if included,
otherwise of all pump types."
  (cell-element-elements element (or type 'pump)))

(defun element-channels (element &optional type)
  "Returns all the channels associated with the cell element of ELEMENT that are of TYPE, if included,
otherwise of all channel types."
  (cell-element-elements element (or type 'channel)))

(defun element-particles (element &optional type)
  "Returns all the particles associated with the cell element of ELEMENT that are of TYPE, if
included, otherwise of all particle types."
  (cell-element-elements element (or type 'particle)))

(defun element-conc-particles (element &optional type)
  "Returns all the conc-particles associated with the cell element of ELEMENT that are of TYPE, if
included, otherwise of all conc-particle types."
  (cell-element-elements element (or type 'conc-particle)))
		 
(defun branch-elements-of-type (branch type)
  (loop for seg in (segments)
	when (and (search branch (segment-name seg)) (= 0 (search branch (segment-name seg))))
	collect (get-node-elements-of-type seg type)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun element-data-point (element &optional data-type type)
  (let* ((element (element element type))
	 (data-type (element-data element data-type type)))
    (retrieve-single-data element data-type)))

#|
(defun element-current-value (element &optional type (data-type (default-data-type element type)))
  (let ((elt (element element type)))
    (s-flt
     (typecase elt
       (synapse (synapse-conductance elt))
       (channel (channel-conductance elt))
       (particle (particle-state-n+1-double elt))
       (conc-particle (conc-particle-state-n+1-double elt))
       (segment (SEGMENT-VOLTAGE elt))
       (soma (soma-voltage elt))
       (vsource (get-vsource-voltage elt *real-time*))))))
  
(defun current-element-df-value (element &optional slot type)
  (let ((elt (element element type)))
    (typecase elt
      (segment (get-segment-voltage-2 elt))
      (soma (node-voltage-n (soma-node elt)))
      (vsource (get-vsource-voltage elt *real-time*)))))
|#

(defun element-value (element &key (target-time *real-time*)
			      (time-list (current-sim-plot-time-list))
			      data-type
			      dt
			      data-list)
  "Returns the value of the DATA-TYPE data of ELEMENT associated with TARGET-TIME [ms]. The DATA-TYPE
for ELEMENT must have already been specified for saving, e.g. by an ENABLE-ELEMENT-PLOT call, unless
TARGET-TIME is *REAL-TIME* [the default]. In this case the element's current value is retrieved,
regardless of whether this data was marked for saving. Element data time base is given by TIME-LIST
[default the time list returned by CURRENT-SIM-PLOT-TIME-LIST]. Data values for times between
simulation time points are linear interpolations. Original data can be explicitly resampled by
included the arg DT [ms]. "
  (let* ((elt (element element))
	 (get-current-value-p (= target-time *real-time*)))
    (if get-current-value-p
	(element-current-value elt data-type)
	(let ((data-list
	       (or data-list (if dt
				 (element-data-dted elt dt data-type nil time-list)
				 (element-data elt data-type))))
	      data-1 time-1)
	  (when data-list
	    (loop for data in data-list
		  for time in (if dt (loop for time from 0.0 by dt
					   for count from 1 to (length data-list)
					   collect time)
				  time-list)
		  when (if data-1 (and (<= time-1 target-time) (< target-time time)))
		  do (return (+ data-1
				(* (- target-time time-1)
				   (/ (- data data-1) (- time time-1)))))
		  do
		  (setq time-1 time
			data-1 data)
		  finally (return data-1)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Element collection as function of distance.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun proximals-and-distals (&optional (element-type 'segment) (proximal-distal-distance-cutoff 0.5))
  (let* ((elts (list-of-all-things element-type))
	 (distances (loop for elt in elts collect (distance-to-soma elt)))
	 (cutoff (* proximal-distal-distance-cutoff (loop for distance in distances maximizing distance))))
    (loop for elt in elts
	  for distance in distances
	  when (<= distance cutoff)
	  collect elt into proximals
	  else
	  collect elt into distals
	  finally (return (list proximals distals)))))

(defun distals (&optional (element-type 'segment) (proximal-distal-distance-cutoff 0.5))
  (let* ((elts (list-of-all-things element-type))
	 (distances (loop for elt in elts collect (distance-to-soma elt)))
	 (cutoff (* proximal-distal-distance-cutoff (loop for distance in distances maximizing distance))))
    (loop for elt in elts
	  for distance in distances
	  when (> distance cutoff)
	  collect elt)))

(defun proximals (&optional (element-type 'segment) (proximal-distal-distance-cutoff 0.5) cell)
  (let* ((elts (list-of-all-things element-type))
	 (cell (element cell 'cell))
	 (distances (loop for elt in elts collect (distance-to-soma elt)))
	 (cutoff (* proximal-distal-distance-cutoff (loop for distance in distances maximizing distance))))
    (loop for elt in elts
	  for distance in distances
	  when (and (or (not cell) (eq cell (element-cell elt))) (<= distance cutoff))
	  collect elt)))

(defun distals-farther-than (distal-border &optional (element-type 'segment))
  (let ((elts (list-of-all-things element-type)))
    (loop for elt in elts
	  when (> (distance-to-soma elt) distal-border)
	  collect elt)))

(defun proximals-within (proximal-border &optional (element-type 'segment))
  (let ((elts (list-of-all-things element-type)))
    (loop for elt in elts
	  when (< (distance-to-soma elt) proximal-border)
	  collect elt)))


(defun neighbors (target radius &optional restrict-to-cell-of-target)
  "Returns list of all elements of the same class as TARGET which lie at most RADIUS microns away.
If RESTRICT-TO-CELL-OF-TARGET is T, then only consider elements that are part of the same cell as TARGET."
  (let ((reference (element-absolute-location target))
	(cell (element-cell target)))
    (when reference
      (loop for elt in
	    (typecase (element target)
	      (synapse (synapses-of-type (synapse-type (element target))))
	      (channel (channels-of-type (channel-type (element target))))
	      (particle (particles-of-type (particle-type (element target))))
	      (conc-particle (conc-particles-of-type (conc-particle-type (element target))))
	      (axon (axons-of-type (axon-type (element target))))
	      (segment (segments))
	      (soma (somas))
	      (cell (cells))
	      (t (segments)))
	    when
	    (and (or (not restrict-to-cell-of-target)
		     (eq cell (element-cell elt)))
		 (< (cartesian-distance-3d-float reference (element-absolute-location elt)) radius))
	    collect elt))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Erase element and Hash table hacks
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defun transfer-node-elements (from-node to-node)	
  (let ((from-element (node-cell-element from-node))
	(to-element (node-cell-element to-node)))
    (loop for element in (node-elements from-node)
	  do
	  (typecase element
	    (axon (cond ((eq (axon-proximal-node element) from-node)
			 (setf (axon-proximal-node element) to-node))
			((eq (axon-node element) from-node)
			 (setf (axon-node element) to-node))))
	    (isource (setf (isource-node-2 element) to-node))
	    (vsource (setf (vsource-node element) to-node))
	    (segment (when (eq (segment-node-2 element) from-node)
		       (setf (segment-node-2 element)  to-node))
		     (when (eq (segment-node-1 element) from-node)
		       (setf (segment-node-1 element)  to-node)))
	    (soma (setf (soma-node element) to-node))
	    (channel (cond ((eq (channel-cell-element element) from-element)
			    (setf (channel-cell-element element) to-element))
			   ((eq (channel-pre-synaptic-element element) from-element)
			    (setf (channel-pre-synaptic-element element) to-element))))
	    (synapse (cond ((eq (synapse-cell-element element) from-element)
			    (setf (synapse-cell-element element) to-element))
			   ((eq (synapse-pre-synaptic-element element) from-element)
			    (setf (synapse-pre-synaptic-element element) to-element))))

	    (particle (cond ((eq (particle-vnode-point element) from-node)
			     (setf (particle-vnode-point element) to-node))
			    ((eq (particle-cell-element element) from-element)
			     (setf (particle-cell-element element) to-element))))

	    (conc-particle (cond ((eq (conc-particle-cnode-point element) from-node)
				  (setf (conc-particle-cnode-point element) to-node))
				 ((eq (conc-particle-cell-element element) from-element)
				  (setf (conc-particle-cell-element element) to-element))))
	    (pump (cond ((eq (pump-cell-element element) from-element)
			 (setf (pump-cell-element element) to-element))))
	    (buffer (cond ((eq (buffer-cell-element element) from-element)
			   (setf (buffer-cell-element element) to-element))))
		


	    (conc-int  (cond ((eq (conc-int-cell-element element) from-element)
			      (setf (conc-int-cell-element element) to-element))))))
	
    (setf (node-elements to-node)
	  (delete-duplicates
	   (concatenate 'list
			(node-elements from-node)
			(node-elements to-node))
	   :test #'equal))
    (setf (node-elements from-node) nil)
    ))
#|
(defun get-hash-table (element)
  (loop for table-sym in *object-hash-table-symbols*
	when
	(loop for entry being the hash-value of
	      (symbol-value table-sym) 
	      when
	      (eq element entry)
	      do (return (symbol-value table-sym)))
	do (return (symbol-value table-sym))))
|#

(defun get-hash-table (element)
  (loop for model being the hash-value of *model-hash-table*
	when
	(loop for entry being the hash-value of (model-hash-table model)
	      when (eq element entry)
	      do (return t))
	do (return (model-hash-table model))))

(defun get-hash-table (element)
  (let ((model (get (type-of element) 'model)))
    (when model (model-hash-table model))))
  

(defun remove-entry-from-hash (entry table)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (when (node-p entry) (setq *num-nodes* (1- *num-nodes*)))
  (maphash #'(lambda (key val) (when (eq entry val) (remhash key table))) table)
  nil)


(defun remove-entry-from-hash (entry table)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (when (node-p entry) (setq *num-nodes* (1- *num-nodes*)))
  (let ((name (element-name entry)))
    (if (eq (gethash name table) entry)
	(remhash name table)
	(loop for key being the hash-key of table
	      for value being the hash-value of table
	      when (eq value entry) do (return (remhash key table)))))
  nil)



;;; REMOVE-MODEL-INSTANCE and REMOVE-MODEL-HASH-ELT Run both functions when a circuit object
;;; TARGET is to be removed. These do not remove the object from the hash table for the object type.
(defun remove-model-instance (target)
;;  
;;  (loop for mod being the hash-value of *model-hash-table*
;;        when 
;;        (loop for inst in (model-instances mod) 
;;              when (eq target inst) do (return (setf (model-instances mod) (delete inst (model-instances mod)))))
;;        do (return t))
  )

(defun erase-all-table-elements (hash-table)
  (loop for element being the hash-value of hash-table
	do (erase-element element)))


;; REMOVE-NODE-ELEMENT Removes ELEMENT from the :ELEMENTS slot of all the nodes.
(defun remove-node-element (element &optional nodes)
  (if nodes
      (loop for node in nodes
	    when (eq (car (node-elements node)) element)
	    do (setf (node-elements node) (cdr (node-elements node)))
	    else do (my-delete element (node-elements node)))
      (loop for node being the hash-value of (NODE-HASH-TABLE)
	    when (eq (car (node-elements node)) element)
	    do (setf (node-elements node) (cdr (node-elements node)))
	    else do (my-delete element (node-elements node)))))


(defun remove-element-reference-from-plot-lists (element &optional type)
  (let ((element-name (or (element-name element type) element)))
    (loop for plot-list-info in *plot-lists-info* do
	  (setf (symbol-value (plot-list-info-names plot-list-info))
		(delete element-name (symbol-value (plot-list-info-names plot-list-info)) :test 'equal)))))

(defun remove-instance-from-type (elt)
  (let ((elt (element elt)))
    (typecase elt
;      (channel (setf (channel-type-channels (channel-type elt))
;                     (remove elt (channel-type-channels (channel-type elt)))))
      (pump (setf (pump-type-pumps (pump-type elt))
		     (remove elt (pump-type-pumps (pump-type elt)))))
      (buffer (setf (buffer-type-buffers (buffer-type elt))
		     (remove elt (buffer-type-buffers (buffer-type elt)))))
;      (synapse (setf (synapse-type-synapses (synapse-type elt))
;                     (remove elt (synapse-type-synapses (synapse-type elt)))))
;      (particle (setf (particle-type-particles (particle-type elt))
;		      (remove elt (particle-type-particles (particle-type elt)))))
;      (conc-particle (setf (conc-particle-type-conc-particles (conc-particle-type elt))
;			   (remove elt (conc-particle-type-conc-particles (conc-particle-type elt)))))
      (conc-int (setf (conc-int-type-conc-ints (conc-int-type elt))
		      (remove elt (conc-int-type-conc-ints (conc-int-type elt)))))
      (axon (setf (axon-type-axons (axon-type elt))
		  (remove elt (axon-type-axons (axon-type elt)))))
      (cell (setf (cell-type-cells (cell-type elt))
		  (remove elt (cell-type-cells (cell-type elt))))))))



(defun erase-element-type (elt)
  "Specifically for removing all elements of a given type, where ELT is either a instance of a type
or points to the type itself."
  (let ((type (element-type elt))
	*enable-reorder-particles-of-type*
	*enable-reorder-conc-particles-of-type*
	*enable-reorder-synapses-of-type*
	*enable-reorder-channels-of-type*)
    (loop for elt in (elements-of-type type) do (erase-element elt type))
    (erase-element-core type)))



(defun erase-elements (&rest elements)
;  "Erases all the members of ELEMENTS. ERASE-ELEMENT may also be used on a list of elements."
  (loop for elt in elements do (erase-element-core elt)))

(defun update-top-pointer (element symbol)
  (when (eq element (symbol-value symbol))
    (setf (symbol-value symbol)
	  (loop for elt in (elements-of-type element) unless (eq elt (symbol-value symbol))
		do (return elt)))))

(defun element-symbol-to-elements (symbol &optional type)
  (case symbol
    (segment (segments))
    (synapse (synapses))
    (channel (channels))
    (particle (particles))
    (conc-particle (conc-particles))
    (pump (pumps))
    (buffer (buffers))
    (conc-int (conc-ints))
    (isource (isources))
    (vsource (vsources))
    (t (element symbol type))))

    
  
;; Cell-types, Particle and Conc-particle types are only erased if there are no instances of them
(defun erase-element-core (elements &optional type (remove-segment-from-cell t) just-erase-top-element
					;  (clear-working-arrays-and-lengths)	; Drastic, but safe
				    )
  (let ((*disable-process-circuit-structure* t)
	(*automatic-run* t))
    (loop for element in
	  (flatten-list (loop for element in (flatten-list elements) collect (element element type)
					; (element-symbol-to-elements element type)
			      ))
	  do
	  (when (and element (not (typecase element
				    ((or cell-type particle-type conc-particle-type) (elements-of-type element)))))
	    (remove-element-reference-from-plot-lists element type)
	    (setq *recheck-circuit-elements-parameters* t)
	    (typecase element
	      (cell (setq remove-segment-from-cell nil))
	      (channel-type
	       (unless just-erase-top-element
		 (let ((look-at-cints (element-has-conc-ints element)))
		   (erase-elements
		    (channels-of-type element)
		    (channel-type-particle-types element)
		    (channel-type-conc-particle-types element))
		   (when look-at-cints
		     (loop for cint in (conc-ints) unless (conc-int-pores cint) do (erase-element cint))))))
	      (synapse-type
	       (unless just-erase-top-element 
		 (let ((look-at-cints (element-has-conc-ints element)))
		   (erase-elements (synapses-of-type element))
		   (when look-at-cints
		     (loop for cint in (conc-ints) unless (conc-int-pores cint) do (erase-element cint)))))
	       (setq *synapse-type-list* (remove element *synapse-type-list*)))

	      (channel (unless just-erase-top-element
			 (erase-elements (channel-particles element)
					 (channel-conc-particles element))))
	      (synapse (unless just-erase-top-element (erase-element (synapse-channel element)))
		       (find-and-remove-element-from-event-generators element)))

	    (remove-model-instance element)
	    ;; Do this since electrodes are listed in both the segment and electrode hash table, and we need
	    ;; to get the associated source as well.
	    (when (and (or (not type) (eq type 'electrode) (eq type 'segment))
		       (member element (electrodes)))
	      (unless just-erase-top-element
		(erase-element (car (or (get-node-elements-of-type element 'isource)
					(get-node-elements-of-type element 'vsource))) nil remove-segment-from-cell))
	      (remove-entry-from-hash element (ELECTRODE-HASH-TABLE)))

    
	    ;; First call some custom routines, and get the right hash table.
	    (let ((table
		   (typecase element
		     (node (erase-node element remove-segment-from-cell) (NODE-HASH-TABLE))
		     ((or axon axon-type)
		      (update-top-pointer element '*axon*)
		      (AXON-TYPE-HASH-TABLE))
		     (cell-type
		      (update-top-pointer element '*cell-type*)
		      (CELL-TYPE-HASH-TABLE))
		     (isource
		      (update-top-pointer element '*isource*)
		      (when (member element (node-elements (element-physical-node element)))
			(let ((isrc-node (element-physical-node element)))
			  (element-parameter isrc-node :isources
					     (remove (element-name element)
						     (element-parameter isrc-node :isources)))))
		      (ISOURCE-HASH-TABLE))
		     (vsource
		      (update-top-pointer element '*vsource*)
		      (setf (node-has-ideal-voltage-source (element-node element)) nil)
		      (VSOURCE-HASH-TABLE))
		     (segment
		      (setq *make-segment-lists* t)
		      (update-top-pointer element '*segment*)
		      (remove-soma-segment (cell-soma (segment-cell element)) element)
		      (when remove-segment-from-cell
			(setf (cell-segments (segment-cell element))
			      (remove element (cell-segments (segment-cell element)))))
		      (SEGMENT-HASH-TABLE))
		     (soma
		      (update-top-pointer element '*soma*)
		      (SOMA-HASH-TABLE))
		     (channel
		      (update-top-pointer element '*channel*)
		      (CHANNEL-HASH-TABLE))
		     (channel-type
		      (update-top-pointer element '*channel-type*)
		      (CHANNEL-TYPE-HASH-TABLE))
		     (synapse
		      (update-top-pointer element '*synapse*)
		      (SYNAPSE-HASH-TABLE))
		     (synapse-type
		      (update-top-pointer element '*synapse-type*)
		      (SYNAPSE-TYPE-HASH-TABLE))
		     (particle
		      (update-top-pointer element '*particle*)
		      (PARTICLE-HASH-TABLE))
		     (particle-type
		      (update-top-pointer element '*particle-type*)
		      (element-parameter
		       (element-parameter element 'concentration-particle-type)
		       'reference-particle-type nil)
		      (PARTICLE-TYPE-HASH-TABLE))
		     (conc-particle
		      (update-top-pointer element '*conc-particle*)
		      (CONC-PARTICLE-HASH-TABLE))
		     (conc-particle-type
		      (update-top-pointer element '*conc-particle-type*)
		      (element-parameter (element-parameter element 'reference-particle-type)
					 'concentration-particle-type)
		      (format t "Found a conc-part-type!~%")
		      (CONC-PARTICLE-TYPE-HASH-TABLE))
		     (conc-int (update-top-pointer element '*conc-int*)
			       (remove-conc-int-from-pump element)
			       (remove-conc-int-from-particle element)
			       (CONC-INT-HASH-TABLE))
		     (conc-int-type (unless (conc-int-type-conc-ints element)
				      (update-top-pointer element '*conc-int-type*)
				      (remove-conc-int-type-from-particle-type element)
				      (CONC-INT-TYPE-HASH-TABLE)))
		     (pump
		      (update-top-pointer element '*pump*)
		      (remove-pump-from-conc-int element)
		      (PUMP-HASH-TABLE))
		     (pump-type (unless (pump-type-pumps element)
				  (update-top-pointer element '*pump-type*)
				  (PUMP-TYPE-HASH-TABLE)))
		     (buffer
		      (update-top-pointer element '*buffer*)
		      (BUFFER-HASH-TABLE))
		     (buffer-type (unless (buffer-type-buffers element)
				    (update-top-pointer element '*buffer-type*)
				    (BUFFER-TYPE-HASH-TABLE)))
		     (cell (loop for win in *output-windows* when (member element (g-value win :cells))
				 do (s-value win :cells (remove element (g-value win :cells))))
			   (update-top-pointer element '*cell*)
			   (loop for seg in (cell-segments element) do (erase-element seg 'segment nil))
			   (loop for node in (nodes) when (eq element (node-cell node)) do (erase-node node nil))
			   (CELL-HASH-TABLE))
		     (t (get-type-hash-table element)))))
	      (typecase element
		((or segment soma)
		 (loop for win in *output-windows*
		       when (eq element (g-value win :chosen-one)) do (s-value win :chosen-one nil))))
	      (remove-instance-from-type element)
	      (when table
		(remove-entry-from-hash element table)
		(remove-node-element element (typecase element
					       (synapse (SYNAPSE-IN-ELEMENTS-OF element))
					       (segment (list (segment-node-1 element)
							      (segment-node-2 element)))
					       (soma (list (soma-node element)))))
		(typecase element
		  (channel (reorder-channels-of-type (element-type element)))
		  (synapse (reorder-synapses-of-type (element-type element))))
		    
		(unless just-erase-top-element
		  (typecase element
		    (segment
		     (loop for elt in  (node-elements (segment-node-2 element))
			   do (erase-element elt nil remove-segment-from-cell))
		     ;;		 (mapcar 'erase-element (node-elements (segment-node-2 element)))
		     ;;		 (unless (node-elements (segment-node-2 element))
		     ;;		   (erase-element (segment-node-2 element) 'node remove-segment-from-cell))

		     (erase-element (segment-node-2 element) 'node remove-segment-from-cell)
		     )
		    (soma (erase-element (soma-cell element) 'cell remove-segment-from-cell)
			  (erase-element (soma-node element))
			  )))))
	    (remove-active-element-lists (element-type element)))
	  (setq *make-node-w/elements-array* t))))


(defun remove-active-element-lists (type)
  (loop for type in (coerce-to-list type) do
	(let ((type (element-type type)))
	  (typecase type
	    (pump-type (when (element-parameter type 'active-pumps)
			 (element-parameter type 'active-pumps nil)))
	    (buffer-type (when (element-parameter type 'active-buffers)
			   (element-parameter type 'active-buffers nil)))
	    (conc-int-type (when (element-parameter type 'active-conc-ints)
			     (element-parameter type 'active-conc-ints nil)))
	    ))))


      
(defun find-and-remove-element-from-event-generators (element)
  (let ((element element))
    (typecase element
      (synapse (synapse-type-iterator
		(syn (synapse-type element))
		when (eq element (synapse-event-generator syn))
		do (set-element-event-generator-slot-value syn nil)
		(let ((followers (and (not (eq syn element)) (fast-syn-event-followers syn))))
		  (when followers (fast-set-syn-event-followers syn (remove element followers))))))
      (t (loop for thing in (all-things-of-same-type element)
	       when (eq element (event-generator thing t))
	       do (set-element-event-generator-slot-value thing nil)
	       when (and (not (eq thing element)) (event-followers thing nil))
	       do (event-followers thing (remove element (event-followers thing))))))))

		 
(defun erase-node (node &optional (remove-segment-from-cell t))
  (setq *node-voltage-initializations*
	(loop for pair in *node-voltage-initializations* unless (eq node (car pair)) collect pair))
  (loop for prt in (particles)
	when (eq node (particle-vnode-point prt))
	do (setf (particle-vnode-point prt) nil)
	(format t "Setting Particle ~A Vnode pointer to NIL (was pointing to ~A).~%"
		(particle-name prt) (node-name node))) 
  (loop for elt in (node-elements node)
	do (erase-element elt nil remove-segment-from-cell)))

;                     (NODE-HASH-TABLE CELL-HASH-TABLE CELL-TYPE-HASH-TABLE SYNAPSE-HASH-TABLE
;                     SOMA-HASH-TABLE SEGMENT-HASH-TABLE CHANNEL-HASH-TABLE CHANNEL-TYPE-HASH-TABLE
;                     PARTICLE-HASH-TABLE PARTICLE-TYPE-HASH-TABLE CONC-INT-HASH-TABLE
;                     CONC-PARTICLE-HASH-TABLE VSOURCE-HASH-TABLE ISOURCE-HASH-TABLE
;                     SYNAPSE-TYPE-HASH-TABLE PARTICLE-TYPE-HASH-TABLE
;                     CONC-PARTICLE-TYPE-HASH-TABLE)


(defun no-input-p ()
  (not (or (isources) (vsources) (synapses))))



;; ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* *******
;;
;; Various element functions, taking an optional ELEMENT arg as a single ELT or a list of ELTs. If
;; omitted, a menu will be generated to select instances of the elements. Each atom of the argument can
;; be a string, a symbol that is the name of an element, or a symbol for the general type of element
;; (e.g. 'channel or 'channel-type). In the latter case the function operates on all loaded elements
;; which are of the general type.
;;
;; ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* ******* *******

(defmacro element-wrapper ((element initial-type prompt) &body forms)
  "This macro supplies a list of elements to be processed by FORMS. ELEMENT can be either an atom or a
list, composed of object pointers and/or object names. If INITIAL-TYPE is supplied, then the
elements given to FORMS only consist of those associated with the original ELEMENT arg that are of
type INITIAL-TYPE. If ELEMENT is NIL and PROMPT is a string, the a menu which can access all loaded
elements is generated, included the PROMPT string."
  `(let (internal-type)
    (flatten-if-list-greater-than-1
     (loop for element in (if ,element
			      (coerce-to-list ,element)
			      (if ,initial-type
				  (namelist-of-all-things ,initial-type)
				  (when nil ; ,prompt
				    (choose-elements (format nil "Choose Elements to ~A" ,prompt)))))
      nconc (loop for elt in (coerce-to-list (element element ,initial-type))
		  do (setq internal-type (if (electrode-p elt t) 'electrode (type-of elt)))
		  collect (progn . ,forms))
      into out
      finally (return (atomize-list (no-nils out)))))))
			


(defun element-type (element-type &optional type create)
  "Returns a list \(if more than one\) or atom \(if just one\) of all element types associated with
ELEMENT-TYPE of TYPE. If CREATE is non-NIL [default NIL], then element types associated with symbols
in ELEMENT-TYPE will be created according to their definitions in the parameter library. Uses
ELEMENT-WRAPPER."
  (if (element-type-p element-type)
      (element element-type)
      (or (element-wrapper (element-type type nil)
			   (element-type-core elt internal-type create))
	  (let ((out
		 (no-nils
		  (loop for thing in (coerce-to-list element-type)
			collect (element-type-core thing type create)))))
	    (atomize-list out)))))


(defun element-type-core (element &optional type create)
  (unless (OBJECT-TYPE-SYMBOL-P element)
    (let ((elt (element element type)))
      (if elt
	  (if (element-type-p elt)
	      elt
	      (typecase elt
		(conc-int (conc-int-type elt))
		(conc-particle (conc-particle-type elt))
		(particle (particle-type elt))
		(channel (channel-type elt))
		(cell (cell-type elt))
		(pump (pump-type elt))
		(buffer (buffer-type elt))
		(axon (axon-type elt))
		(synapse (synapse-type elt))))
	  (when create
	    (when (stringp element) (setq element (intern element)))
	    (loop for type-symbol in *object-type-symbols*
		  when (get-a-value element (model-parameter-type-library (type-symbol-model type-symbol)))
		  return (funcall (model-create-routine (type-symbol-model type-symbol)) element)))))))

(defun cell-element (element &optional type)
  "Same as ELEMENT-CELL-ELEMENT."
  (element-cell-element element type))

(defun element-cell-element (element &optional type)
  "Returns a list \(if more than one\) or atom \(if just one\) of all cell elements associated with the
elements associated with ELEMENT of TYPE. Uses ELEMENT-WRAPPER."
  (typecase element
    ((or soma segment) element)
    (t (element-wrapper (element type nil)
			(element-cell-element-core elt internal-type)))))

(defun element-cell-element-core (element &optional type)
  (typecase element
    ((or soma segment) element)
    (t
     (let ((element (element element type)))
       (when element
	 (typecase element
	   (cell (cell-elements element))
	   (segment element)
	   (soma element)
	   (synapse (synapse-cell-element element))
	   (channel (channel-cell-element element))
	   (pump (element-cell-element (pump-conc-int element)))
	   (buffer (buffer-cell-element element))
	   (particle (when (particle-channel element) (channel-cell-element (particle-channel element))))
	   (conc-particle (when (conc-particle-channel element) (channel-cell-element (conc-particle-channel element))))
	   (isource (isource-cell-element element))
	   (vsource (vsource-cell-element element))
	   (node (node-cell-element element))
	   (number (element-cell-element (element element type)))
	   (string (element-cell-element (element element type)))
	   (t (element-slot 'cell-element element type))))))))

(defun element-soma (element &optional type)
  "Returns a list \(if more than one\) or atom \(if just one\) of all soma associated with the
elements associated with ELEMENT of TYPE. Uses ELEMENT-WRAPPER."
  (typecase element
    (cell element)
    (t (element-wrapper (element type nil)
			(cell-soma (element-cell-core elt internal-type))))))

(defun element-cell (element &optional type)
  "Returns a list \(if more than one\) or atom \(if just one\) of all cells associated with the
elements associated with ELEMENT of TYPE. Uses ELEMENT-WRAPPER."
  (typecase element
    (cell element)
    (t (element-wrapper (element type nil)
			(element-cell-core elt internal-type)))))


(defun element-cell-core (element &optional type)
  (typecase element
    (cell element)
    (t (let ((elt (element element type)))
	 (when elt
	   (typecase elt 
	     (cell elt)
	     (node (node-cell elt))
	     (segment (segment-cell elt))
	     (soma (soma-cell elt))
	     (t (if (element-type-p elt)
		    (loop for cell being the hash-value of (cell-hash-table)
			  when (instance-in-cell elt nil cell) collect cell)
		    (let ((node (element-physical-node elt type)))
		      (when node (node-cell node)))))))))))



(defun erase-element (&optional element type (remove-segment-from-cell t) just-erase-top-element)
  "Erase ELEMENT, if it is singular, or the members of ELEMENT, if it is a list, with the qualification that all erased elements
are of type TYPE, if this arg is included. If a segment is to be erased, then be sure to remove it from its cell when
REMOVE-SEGMENT-FROM-CELL is non-nil. For erasing all segments of a cell, it is more efficient to remove the segments from the
cell's :SEGMENTS slot separately. If JUST-ERASE-TOP-ELEMENT is nil [default], then remove all elements which are components of an
erased element (i.e. particles from a channel). Uses ELEMENT-WRAPPER. ELEMENT can also be a symbol such as 'SYNAPSE or 'CHANNEL,
in which case all instances of that type of element will be erased."
  (cond
    (element (erase-element-core element type remove-segment-from-cell just-erase-top-element))
    ((not *automatic-run*)
     (element-wrapper (element type "Erase")
		      (erase-element-core elt internal-type remove-segment-from-cell just-erase-top-element)))))


(defun print-element (&optional element type (stream *standard-output*))
  "Print documentation apropos for ELEMENT. Uses ELEMENT-WRAPPER."
  (unless *circuit-processed* (process-circuit-structure))
  (let ((*standard-output* stream))
    (element-wrapper
     (element type "Print")
     (if (electrode-p elt)
	 (print-electrode elt)
	 (let ((print-routine (model-print-routine (element-model elt internal-type))))
	   (if print-routine
	       (funcall print-routine elt)
	       (element-funcall 'print elt internal-type)))))))


(defun turn-off (&optional element type)
;  "Generic disable for ELEMENT. Uses ELEMENT-WRAPPER."
  (disable-element element type))

(defun disable-element (&optional element type)
  "Generic disable for ELEMENT. Uses ELEMENT-WRAPPER."
  (element-wrapper 
   (element type "Disable")
   (typecase elt
     ((or isource vsource) (turn-off-source elt))
     (channel (setf (channel-block elt) t))
     (channel-type (setf (channel-type-block elt) t))
     (synapse (setf (synapse-block elt) t))
     (synapse-type (setf (synapse-type-block elt) t))
     (pump (setf (pump-enabled elt) nil))
     (pump-type (setf (pump-type-enabled elt) nil))
     (buffer (setf (buffer-enabled elt) nil))
     (buffer-type (setf (buffer-type-enabled elt) nil))
     (conc-int (setf (conc-int-enabled elt) nil))
     (conc-int-type (setf (conc-int-type-enabled elt) nil)))))

(defun turn-on (&optional element type)
;  "Generic enable for ELEMENT. Uses ELEMENT-WRAPPER."
  (enable-element element type))

(defun enable-element (&optional element type)
  "Generic enable for ELEMENT. Uses ELEMENT-WRAPPER."
  (element-wrapper 
   (element type "Enable")
   (typecase elt
     ((or isource vsource) (turn-on-source elt))
     (channel (setf (channel-block elt) nil))
     (channel-type (setf (channel-type-block elt) nil))
     (synapse (setf (synapse-block elt) nil))
     (synapse-type (setf (synapse-type-block elt) nil))
     (pump (setf (pump-enabled elt) t))
     (pump-type (setf (pump-type-enabled elt) t))
     (buffer (setf (buffer-enabled elt) t))
     (buffer-type (setf (buffer-type-enabled elt) t))
     (conc-int (setf (conc-int-enabled elt) t))
     (conc-int-type (setf (conc-int-type-enabled elt) t)))))

(defun element-enabled-p (element &optional type)
  (let ((elt (element element type)))
    (typecase elt
      (isource (isource-enabled elt))
      (vsource (vsource-enabled elt))
      (channel (not (channel-block elt)))
      (channel-type (not (channel-type-block elt)))
      (synapse (not (synapse-block elt)))
      (synapse-type (not (synapse-type-block elt)))
      (pump (pump-enabled elt))
      (pump-type (pump-type-enabled elt))
      (buffer (buffer-enabled elt))
      (buffer-type (buffer-type-enabled elt))
      (conc-int (conc-int-enabled elt))
      (conc-int-type (conc-int-type-enabled elt)))))


(defun print-elements (elements &optional (stream *standard-output*))
  (loop for elt in elements do (print-element elt nil stream)))

(defun document-element (&optional element type)
  "Uses ELEMENT-WRAPPER."
  (element-wrapper
   (element type "Document")
   (element-funcall 'document elt internal-type)))

(defun element-document-extras (element)
  (when (element-parameter element 'nice-name)
    (format t " (nice-name . ~a)~%" (element-parameter element 'nice-name))))

#|
(defun edit-element (element &optional type)
  (loop for element in (coerce-to-list element) do
	(let* ((element (element element type))
	       (edit-routine (when element (model-edit-routine (element-model element)))))
	  (when edit-routine
	    (funcall edit-routine (element element type))))))
|#

      
(defun edit-element (&optional element type)
  "Uses ELEMENT-WRAPPER."
  (element-wrapper
   (element type "Edit")
   (progn (element-funcall 'edit elt internal-type)
	  (when (and (plot-menu-class-enable elt internal-type)
		     (go-ahead-menu (format nil "Edit Plot for ~A ~A"
				       (type-of (element elt internal-type))
				       (element-name elt internal-type))
			       "Authorization" nil))
	    (plot-element-menu elt internal-type)))))

;; ELEMENT NAME

;; This allows for names that are either strings, numbers or symbols.
(defun same-element-names (name1 name2)
  (or (eq name1 name2)
      (and (stringp name1) (stringp name2) (string= name1 name2))
      (and (numberp name1) (numberp name2) (= name1 name2))))
      

(defun element-name-core (element &optional type)
  (let ((element (element element type)))
    (typecase element
      (segment (segment-name element))
      (soma (soma-name element))
      (node (node-name element))
      (cell (cell-name element))
      (cell-type (cell-type-name element))
      (axon (axon-name element))
      (axon-type (axon-type-name element))
      (isource (isource-name element))
      (vsource (vsource-name element))
      (synapse (synapse-name element))
      (synapse-type (synapse-type-name element))
      (channel (channel-name element))
      (channel-type (channel-type-name element))
      (particle (particle-name element))
      (particle-type (particle-type-name element))
      (conc-particle (conc-particle-name element))
      (conc-particle-type (conc-particle-type-name element))
      (conc-int (conc-int-name element))
      (conc-int-type (conc-int-type-name element))
      (pump (pump-name element))
      (pump-type (pump-type-name element))
      (buffer (buffer-name element))
      (buffer-type (buffer-type-name element))
      (t (element-slot 'name element type)))))

(defun element-name (&optional element type)
  "Uses ELEMENT-WRAPPER."
  (element-wrapper (element type "Name") (element-name-core elt internal-type)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Element diameter, area and volume
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun element-diameter (element &optional new-diameter)
  "For the the cell element of ELEMENT, return the diameter of the segment or soma in microns. If
NEW-DIAMETER is a number, then the cell element diameter will be changed to this value, and
*CIRCUIT-PROCESSED* will be set to NIL."
  (let ((cell-element (element-cell-element element)))
    (typecase cell-element
      (soma
       (when (numberp new-diameter)
	 (setf (soma-diameter cell-element) (s-flt new-diameter))
	 (setq *circuit-processed* nil))
       (soma-diameter cell-element))
      (segment
       (when (numberp new-diameter)
	 (setf (segment-diameter cell-element) (s-flt new-diameter))
	 (setq *circuit-processed* nil))
       (segment-diameter cell-element)))))


(defun element-length (element &optional new-length)
  "When the cell element of ELEMENT is a segment, return the length of the segment in microns. If
NEW-LENGTH is a number, then the segment length will be changed to this value, and
*CIRCUIT-PROCESSED* will be set to NIL."
  (let ((cell-element (element-cell-element element)))
    (typecase cell-element
      (segment
       (when (numberp new-length)
	 (setf (segment-length cell-element) (s-flt new-length))
	 (setq *circuit-processed* nil))
       (segment-length cell-element)))))
	       

(defun elements-area (elements &optional virtual-element)
  (loop for element in elements sum (element-area element virtual-element)))


(defun element-area (element &optional virtual-element type)
  "The total area of somas and segments associated with ELEMENT, in square microns \(single float\).
Segment areas do not include the cylinder ends \(only the lateral areas are considered\). If somas
have 'ADJUST-AREA-FOR-TRUNKS parameter, then their area is adjusted for the areas of the faces of
any abutting segments."
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (loop for element in (coerce-to-list element)
	sum (the sf (element-area-core element virtual-element type)) into area single-float
	finally (return area)))

(defun element-area-core (element virtual-element type)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let ((element (element-which-is-cell-element element type)))
    (typecase element			
      (soma (let ((params (soma-parameters element)))
	      (+ (the sf (if (and virtual-element (soma-segments element))
			     (loop for seg in (soma-segments element)
				   summing (the sf (element-area seg)) into area single-float finally (return area))
			     0.0))
		 (let ((length (get-a-value 'length params))
		       (diameter (the sf (if (get-a-value 'soma-cylinder params)
					     (get-a-value 'soma-cylinder-diameter params)
					     (soma-diameter element)))))
		   (the sf (* (the sf (or (get-a-value 'membrane-area-coefficient params) 1.0))
			      (if (and length (get-a-value 'soma-cylinder params))
				  (* pi-single (* diameter (the sf length))) ; Cylinder area, without ends.
				  (* pi-single (* diameter diameter)) ; Sphere area
				  ))))
		 (if (get-a-value 'adjust-area-for-trunks params)
		     (* -1 pi-single (square (/ (loop for seg in (true-soma-trunks element) sum (segment-diameter seg))
						2)))
		     0))))
      (segment (* (the sf (or (get-a-value 'membrane-area-coefficient (segment-parameters element)) 1.0))
		  pi-single (segment-diameter element) (segment-length element)))
      (cell (+ (the sf (if (soma-segments element) 0.0 (the sf (element-area (cell-soma element)))))
	       (the sf (loop for seg in (cell-segments element) sum (the sf (element-area seg)) into area single-float
			     finally (return area)))))
      (t 0.0))))


(defun element-area-cm2 (element &optional virtual-element type)
  (* 1.0e-8 (element-area element virtual-element type)))

(defun element-volume (element &optional virtual-element type)
  "Total volume of cell elements associated with ELEMENT in cubic microns \(single-float\). If a cell
is given by ELEMENT, then the total cell volume is considered."
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (loop for element-internal in (coerce-to-list element)
	summing
	(the sf
	     (let ((element (element-which-is-cell-element element-internal type)))
	       (typecase element			
		 (soma (if (and virtual-element (soma-segments (soma-cell element)))
			   (element-volume (soma-segments (soma-cell element)))
			   (sphere-volume-from-diameter (soma-diameter element))))
		 (segment (cylinder-volume (segment-length element) (segment-diameter element)))
		 (cell (+ (element-volume (cell-segments element))
			  (if (soma-segments (soma-cell element)) 0.0 (element-volume (cell-soma element)))))
		 (t 0.0))))
	into volume single-float
	finally (return volume)))


;; single-float
(defun element-volume-cm3 (element &optional virtual-element type)
  (* 1.0e-4 1.0e-4 1.0e-4		; cm3/um3
     (element-volume element virtual-element type)))


(defun element-sv-ratio (element &optional virtual-element type)
  "Ratio of area divided by volume of the cell elements associated with ELEMENT, in 1/microns \(single-float\)."
  (/ (element-area element virtual-element type)
     (element-volume element virtual-element type)))

;; rectified single-float, um3
(defun element-concentration-volume (element &optional virtual-element type)
  "Returns the volume in um^3 of the cell element associated with ELEMENT, minus the volume of any
nucleus associated with the cell element, as indicated by the element parameter 'nucleus-diameter in
microns."
  (let* ((element (element-cell-element element))
	 (nucleus-diameter (or (element-parameter element 'nucleus-diameter) 0.0)) ; um
	 (nucleus-volume (sphere-volume-from-diameter nucleus-diameter))) ; in um3, if any
    (max 0.0 (- (element-volume element virtual-element type)
		nucleus-volume))))

(defun element-concentration-volume-cm3 (element &optional virtual-element type)
  (* (element-concentration-volume element virtual-element type)
     1.0e-4 1.0e-4 1.0e-4		; cm3/um3
     ))




;;;;;;;;;;;;;


#|
(defun all-data-types (element)
  "Returns a list of symbols corresponding to all possible types of plot data appropriate for ELEMENT."
  (typecase (or (element-type element) (element element))
    (SOMA '(VOLTAGE DVDT DENDRITE-CURRENT))
    (SEGMENT '(VOLTAGE DVDT))
    (EXTRACELLULAR-ELECTRODE '(FIELD-POTENTIAL))
    (AXON '(VOLTAGE))
    ((or CHANNEL-type SYNAPSE-type) '(CURRENT REVERSAL-POTENTIAL CONDUCTANCE))
    ((or ISOURCE VSOURCE) '(CURRENT))
    (PARTICLE-type '(STATE MARKOV-STATE))
    (CONC-PARTICLE-type '(STATE))
;    (CONC-INT-type '(CONCENTRATION-1 CONCENTRATION-2 CONCENTRATION-3 TOTAL-CONCENTRATION))
    (CONC-INT-type '(1 2 3 TOTAL))
    (BUFFER-type '(CONCENTRATION))
    (PUMP-type '(CURRENT))))
|#

(defun all-data-types (element &optional type)
  "Returns a list of symbols corresponding to all possible types of plot data appropriate for ELEMENT."
  (model-output-data-types (element-model (element element type))))



#|
(defun default-data-type (element &optional type)
  "Returns a symbol corresponding the default type of plot data appropriate for ELEMENT. Apropos for
element types with more than one type of data."
  (typecase (or (element-type element type) (element element type))
    ((or extracellular-electrode	; electrode
	 axon-type segment soma node) 'voltage)
    ((or channel-type synapse-type isource vsource pump-type) 'current)
    ((or particle-type conc-particle-type) 'state)
					;    (conc-int-type 'CONCENTRATION-1)
    (conc-int-type 'total)
    (BUFFER-type '(CONCENTRATION))
    (pump-type 'current)))
|#
(defun default-data-type (element &optional type model)
  "Returns a symbol corresponding the default type of plot data appropriate for ELEMENT. Apropos for
element types with more than one type of data."
  (model-output-default-data-type (or model (element-model (element element type)))))


(defun disable-all-element-plot ()
  "Disables plotting of all circuit elements."
  (clear-all-plot-lists))

  
(defun plotted-elements ()
  "Return a list of all element names and the data types for which data is currently saved."
  (loop for plot-list-info in *PLOT-LISTS-INFO*
	when (and (plot-list-info-enable-var plot-list-info)
		  (plot-list-info-structures plot-list-info))
	nconc
	(loop for element in (symbol-value (plot-list-info-structures plot-list-info))
	      collect (list (element-name element) 
			    (plot-list-info-structure-slot plot-list-info)))))

(defun clear-all-plot-lists ()
  (loop for plot-list-info in *plot-lists-info* do
	(setf (symbol-value (plot-list-info-names plot-list-info)) nil)))

(defun non-nil-plot-lists ()
  (loop for plot-list-info in *plot-lists-info* 
	when (symbol-value (plot-list-info-names plot-list-info))
	collect (plot-list-info-names plot-list-info)))
	
(defun toggle-element-plot (elements data-type type plot-p &optional toggle-analysis analysis-p (choose-plot-data t))
  (loop for elt-ref in (flatten-list elements) do
	(loop for element in (if (element-type-p elt-ref)
				 (progn (setq type (type-symbol-model (type-symbol-child-string (type-of type))))
					(elements-of-type elt-ref))
				 (list elt-ref))
	      do
	      (let* ((element (element element type))
		     (data-type (or data-type (DEFAULT-DATA-TYPE element)))
		     (hash-table (element-hash-table element)))
		(when (cell-p element) (setq element (cell-soma element)))
		(loop for data-type in (coerce-to-list data-type) do
		      (if (or (eq data-type 'all) (eq data-type :all))
			  (toggle-element-plot element
					       (all-data-types element) type plot-p toggle-analysis analysis-p
					       nil)
			  (progn
			    (when (eq data-type 'voltage-derivative) (setq data-type 'dvdt) )
			    (when (and (particle-p (element element))
				       (eq data-type 'markov-states)
				       (eq :markov (particle-type-class (element-type element))))
			      (element-parameter element 'plot-markov-states plot-p))
			    (if toggle-analysis 
				(when (cell-element-p element)
				  (let ((name (element-name element)))
				    (if analysis-p
					(unless (member name *analysis-nodes* :test 'equal)
					  (push name *analysis-nodes*))
					(when (member name *analysis-nodes* :test 'equal)
					  (setq *analysis-nodes* (remove name *analysis-nodes*))))))
				(loop for plot-list-info in *plot-lists-info*
				      when (and
					    (find hash-table (plot-list-info-tables plot-list-info))
					    (or (eq data-type (plot-list-info-structure-slot plot-list-info))
						(and (not plot-p)
						     (or (eq data-type 'all) (eq data-type :all))))
					    (not (eq (first plot-list-info) '*plot-path-nodes*))
					    (xor plot-p
						 (member (element-name element type) (symbol-value (plot-list-info-names plot-list-info)) :test 'equal))
					    (or (not (particle-p (element element)))
						(case data-type
						  (state t)
						  (markov-state (eq :markov (particle-type-class (element-type element)))))))
				      do
				      (if plot-p
					  (setf (symbol-value (plot-list-info-enable-var plot-list-info)) t
						(symbol-value (plot-list-info-names plot-list-info))
						(cons (element-name element type)
						      (symbol-value (plot-list-info-names plot-list-info))))
					  (setf (symbol-value (plot-list-info-names plot-list-info))
						(remove (element-name element type)
							(symbol-value (plot-list-info-names plot-list-info))
							:test 'equal)))))))))))
  (when choose-plot-data (choose-plot-data))
  nil)

;; plot toggle
(defun enable-element-plot (&optional element data-type type)
  "Enable plot of DATA-TYPE (as in ELEMENT-DATA) of elements in ELEMENT of TYPE. If ELEMENT is an
element type, then all elements of that type are affected. For elements that can generate more than
one type of simulation data, setting DATA-TYPE to :ALL will enable all plotting of all data types
(except for events). DATA-TYPE may also be a list of data types."
 (toggle-element-plot (element (flatten-list element) type) data-type type t))

(defun disable-element-plot (&optional element data-type type)
  "Disables plot of DATA-TYPE (as in ELEMENT-DATA) of elements in ELEMENT of TYPE. If ELEMENT is an
element type, then all elements of that type are affected. For elements that can generate more than
one type of simulation data, setting DATA-TYPE to :ALL will disable all plotting of all data types.
Setting ELEMENT to :ALL will disable all plotting, period. DATA-TYPE may also be a list of data types."
  (case element
    ((:all all) (clear-all-plot-lists))
    (t (toggle-element-plot (element (flatten-list element) type) data-type type nil))))



(defun setup-plot-total-conductances (spec-list)
  "Set up and enable plotting of total conductances with a SPEC-LIST whose format is described in the
documentation for *PLOT-TOTAL-CONDUCTANCES*. To plot the total conductance of all cells in the
circuit:

   (SETUP-PLOT-TOTAL-CONDUCTANCES :ALL)

"
  (setq *plot-total-conductances* spec-list
	*PLOT-TOTAL-CONDUCTANCES-P t))

(defun clear-plot-total-conductances ()
    "Clear and disable plotting of total conductances."
  (setq *plot-total-conductances* nil
	*PLOT-TOTAL-CONDUCTANCES-P nil))

   
(defun enable-element-analysis (&optional element data-type type)
  "Enables analysis of DATA-TYPE (as in ELEMENT-DATA) of elements in ELEMENT of TYPE. Currently
supports only voltage analysis of segments and somas."
  (when (or (not data-type) (eq data-type 'voltage))
    (toggle-element-plot (element element type) data-type type nil t t)))


(defun disable-element-analysis (&optional element data-type type)
  "Disables analysis of DATA-TYPE (as in ELEMENT-DATA) of elements in ELEMENT of TYPE. Currently
supports only voltage analysis of segments and somas."
  (when (or (not data-type) (eq data-type 'voltage))
    (toggle-element-plot (element element type) data-type type nil t nil)))



;; Plotting characteristics

(defun plot-type (&optional element type)
  "Takes a single element or a list of elements for ELEMENT, and plots the characteristics of the
associated element type. Uses ELEMENT-WRAPPER."
  (element-wrapper (element type "Plot Instance Characteristics")
		   (typecase internal-type
		     (channel-type (plot-channel-types internal-type)))))

(defun plot-types (elements)
  (plot-element elements))

(defun plot-elements (elements)
  (plot-element elements))

(defun plot-element (elements)  
  "Takes a single element or a list of elements for ELEMENTS, and plots the characteristics [not
simulation data] of the associated element type."
  (loop for elt in (coerce-to-list elements)
	do (let ((type (element-type elt)))
	     (or
	      (typecase elt
		((or vsource isource) (plot-source-waveform elt)))
	      (typecase type
		(synapse-type (plot-synapse-type type))
		(particle-type (plot-v-particles type :what :all ))
		(channel-type (plot-channel-types type)))))))

(defun plot-segments-to-soma (element &optional (segment-skip 0) clear-first)
  "Enables plotting on a separate window all the segments on the path from the
ELEMENT to the soma, skipping path segments by SEGMENT-SKIP [default 0]. If CLEAR-FIRST [default
NIL] is T, then any segments previously including in such a plot are cleared first."
  (let ((start-segment (element-cell-element element)))
    (when (segment-p start-segment)
      (setq *plot-path-node-voltages-p t)
      (when clear-first (setq *plot-path-nodes* nil))
      (let ((seg-list (nconc (segments-to-soma start-segment segment-skip) (list (cell-soma (segment-cell start-segment))))))
	(setq *plot-path-nodes*
	      (concatenate 'list 
			   (loop for seg in (reverse seg-list) do
				 (setq *plot-path-nodes* (remove (element-name seg) *plot-path-nodes* :test 'equal))
				 collect (element-name seg))
			   *plot-path-nodes*))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Distributing elements
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun element-distribution (type total-number targets distribution-function &rest distribution-function-args)
  "Add TOTAL-NUMBER elements of TYPE to cell elements associated with TARGETS with a probability
given by DISTRIBUTION-FUNCTION and the optional DISTRIBUTION-FUNCTION-ARGS. DISTRIBUTION-FUNCTION
may be either a function or the keyword :FLAT. In the first case, the function is applied to the
distance to the soma of each cell element. The first argument of DISTRIBUTION-FUNCTION is the cell
element, with possible additional arguments given by
DISTRIBUTION-FUNCTION-ARGS, and DISTRIBUTION-FUNCTION must return a single-float. If
DISTRIBUTION-FUNCTION is set to :FLAT, then the targets that receive an element of TYPE are chosen
with an equal probability." 
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((elts '())
	(total-number (round total-number))
	(targets (or targets (segments))))
    (case DISTRIBUTION-FUNCTION
	  (:flat
	   (create-element type (sequence-head (shuffled-list targets) total-number)))
	  (t 
	   (loop until
		 (loop for target in (shuffled-list targets)
		       when (< (random 1.0)
			       (the sf (apply DISTRIBUTION-FUNCTION
					      (cons target DISTRIBUTION-FUNCTION-ARGS))))
		       do (push (create-element type target) elts)
		       when (= (length elts) (the fn total-number)) do (return t)))))
    (reorder-elements-of-type type)))



(defun propagate-element-distribution (type total-number targets distribution-function &rest distribution-function-args)
  (let ((distribution-function-coefficient
	 (/ total-number
	    (loop for target in targets summing (apply DISTRIBUTION-FUNCTION (cons target DISTRIBUTION-FUNCTION-ARGS))))))
    (format t "Total leftover: ~A~%, distribution-function-coefficient: ~A"
	    (propagate-element-distribution-core
	     targets type 0.0
	     distribution-function distribution-function-args
	     distribution-function-coefficient)
	    distribution-function-coefficient))
  (reorder-elements-of-type type))
  
(defun propagate-element-distribution-core (targets type proximal-leftover
						    distribution-function distribution-function-args
						    distribution-function-coefficient)
  (multiple-value-bind (elts-here elts-there)
      (truncate (the sf (+ proximal-leftover
			   (* distribution-function-coefficient
			      (apply DISTRIBUTION-FUNCTION (cons (car targets) DISTRIBUTION-FUNCTION-ARGS))))))
    (dotimes (i elts-here) (create-element type (car targets)))
    (if (cdr targets)
	(propagate-element-distribution-core
	 (cdr targets) type elts-there
	 distribution-function distribution-function-args
	 distribution-function-coefficient)
	elts-there)))

  
(defun cumulative-pdf-element-distribution (type total-number targets
						 distribution-function &rest distribution-function-args)
  "Add TOTAL-NUMBER elements of TYPE to cell elements associated with TARGETS with a probability given
by DISTRIBUTION-FUNCTION and its DISTRIBUTION-FUNCTION-ARGS, applied to each target. It is assumed
that the first argument of DISTRIBUTION-FUNCTION is the target, with possible additional arguments
given by DISTRIBUTION-FUNCTION-ARGS. DISTRIBUTION-FUNCTION must return a single-float. TOTAL-NUMBER
must be a fixnum.

The cumulative probability distribution function for DISTRIBUTION-FUNCTION is integrated over all
the TARGETS, with a final value of TOTAL-NUMBER, and each target is assigned the value of the cpdf
after taking into account their contribution to the integral (given by applying
DISTRIBUTION-FUNCTION to the given target).

Elements of TYPE are added one at a time, by generating a random number (range given by
TOTAL-NUMBER) THIS-ROLL and finding the target which is associated with THIS-ROLL in the cumulative
PDf."
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (fixnum total-number))
  (let ((targets (or targets (segments)))
	(cumulative-probability-sym (gensym "cumulative-probability")) ; Make a new symbol just for use within this function.
	(cumulative-probability 0.0))
    (declare (single-float cumulative-probability))
    ;; Integrate the cumulative probability distribution function (CPDF) over all the potential
    ;; targets and store the value of this integral in each target after accounting for the target's
    ;; contribution to the integral.
    (loop for target in targets
	  do (setq cumulative-probability
		   (+ cumulative-probability
		      (the sf (apply DISTRIBUTION-FUNCTION (cons target DISTRIBUTION-FUNCTION-ARGS)))))
	  (element-parameter target cumulative-probability-sym cumulative-probability))

    (loop for i fixnum from 1 to total-number do
	  (let* ((search-cumulative-probability 0.0)
		 (new-search-cumulative-probability 0.0)
		 (this-roll (random cumulative-probability)))
	    (declare (single-float search-cumulative-probability new-search-cumulative-probability this-roll))
	    (loop for target in targets
		  do (setq new-search-cumulative-probability
			   (the sf (element-parameter target cumulative-probability-sym)))
		  when (and (< search-cumulative-probability this-roll) (<= this-roll new-search-cumulative-probability))
		  do (create-element type target) (return t)
		  else do (setq search-cumulative-probability new-search-cumulative-probability))))
    ;; Clear out the cumulative-probability entry in the parameter lists.
    (loop for target in targets do (element-parameter target cumulative-probability-sym nil)))
  (reorder-elements-of-type type))


(defun collect-cell-elements-by-distance (elements distance-resolution &optional reverse-order)
  (let ((targets (coerce-to-list (element-cell-element elements)))
	index)
    (loop for target in targets
	  maximize (distance-to-soma target) into maximum-distance
	  finally
	  (return
	    (let* ((bins (loop for distance from 0 to maximum-distance by distance-resolution
			       collect '()))
		   (number-of-bins (length bins)))
	      (loop for target in targets
		    do (setq index (round (* (1- number-of-bins)
					     (/ (distance-to-soma target) maximum-distance))))
		    (push target (nth index bins)))
	      (if reverse-order (reverse bins) bins))))))

(defun sprinkle-elements-exponentially (type targets total-number length-constant distance-resolution)
  (let* ((reverse-order (minusp length-constant))
	 (length-constant (abs (s-flt length-constant)))
	 (binned-targets (collect-cell-elements-by-distance targets distance-resolution reverse-order))
	 (new-elements 0)
	 target)
    (loop until (= new-elements total-number)
	  when (setq target (random-nth (nth (round (/ (exponential-pdf length-constant) distance-resolution))
					     binned-targets)))
	  do (create-element type target)
	  (incf new-elements))))
	  

(defun gaussian-element-distribution (type targets total-number mean-distance sd)
  "Add TOTAL-NUMBER elements of TYPE to cell elements associated with TARGETS with a probability given
by a gaussian of the difference between MEAN-DISTANCE and the distance to the soma of each cell
element, with a standard deviation of SD."
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (single-float mean-distance sd))
  (let ((elts '())
	(targets (or targets (segments)))) 
    (loop until
	  (loop for target in targets
		when (< (random 1.0)
			(the sf (gaussian (the sf (- mean-distance (distance-to-soma target)))
					  0.0
					  (the sf (square sd)))))
		do (push (create-element type target) elts)
		when (= (length elts) total-number) do (return t)))))



