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


;;; SYS Source file: extracellular-electrode.lisp

;;;
;;; All the circuit element structure definitions, with some slot macros.
;;;

(in-package "SURF-HIPPO")

(defun EXTRACELLULAR-ELECTRODE-FIELD-POTENTIAL-VALUE (electrode)
  (let ((electrode (element electrode 'extracellular-electrode)))
    (when electrode (eval-extracellular-electrode electrode))))

(defun save-extracellular-electrode-data ()
  (loop for name in *plot-field-potentials* do
	(let ((electrode (element name 'extracellular-electrode)))
	  (push-onto-element-param-acons
	   electrode
	   :field-potential
	   (eval-extracellular-electrode electrode)))))

(defun update-extracellular-electrode-output-data-flags ()
  (loop for name in *plot-field-potentials* do
	(element-parameter (element name 'extracellular-electrode) 'plot-field-potential t)))

(defun clear-extracellular-electrode-output-data-flags (extracellular-electrode)
  (element-parameter extracellular-electrode 'plot-field-potential))
    
(defun clear-extracellular-electrode-output-data (extracellular-electrode)
  (element-parameter extracellular-electrode :field-potential))

(defun print-extracellular-electrode (extracellular-electrode)
  (format t "Extracellular electrode ~A located at ~A~%"
	  (element-name extracellular-electrode)
	(extracellular-electrode-absolute-location extracellular-electrode)))  

(defun document-extracellular-electrode (extracellular-electrode))

(defun edit-extracellular-electrode (extracellular-electrode)
  (let ((dummy1 (first (extracellular-electrode-absolute-location extracellular-electrode)))
	(dummy2 (second (extracellular-electrode-absolute-location extracellular-electrode)))
	(dummy3 (third (extracellular-electrode-absolute-location extracellular-electrode))))
    (choose-variable-values
     '((dummy1 "X:" :float)
       (dummy2 "Y:" :float)
       (dummy3 "Z:" :float))
     :label (format nil "Edit location for extracellular electrode ~A"
		    (element-name extracellular-electrode)))
    (setf (extracellular-electrode-absolute-location extracellular-electrode) (list dummy1 dummy2 dummy3))
    (setup-extracellular-electrode extracellular-electrode)
    extracellular-electrode))

  
(defun create-extracellular-electrode (name x y z)
  (let ((electrode (element name 'extracellular-electrode)))
    (unless electrode
      (let ((model (gethash "extracellular-electrode" *model-hash-table*)))
	(setq electrode (make-extracellular-electrode :name name))
	(setf (gethash name (EXTRACELLULAR-ELECTRODE-HASH-TABLE)) electrode)))
    (setf (extracellular-electrode-absolute-location electrode) (coerce-location-to-float (list x y z)))
    electrode))
	  

(defun eval-extracellular-electrode (extracellular-electrode)
  (loop for cell-element-distance in
	(element-parameter-fast 'cell-elements-and-distance
				(extracellular-electrode-parameters extracellular-electrode))
	sum (/ (the df
		    (node-current (element-node (car cell-element-distance))))
					; (element-membrane-current (car cell-element-distance))
	       (the sf (cadr cell-element-distance)))
	into weighted-current double-float
	finally (return (/ (* *r-extracellular* weighted-current)
			   (* 4 pi-single)))))

(defun element-membrane-current (element)
  (let ((element (coerce-to-list (element-cell-element element))))
    (loop for node in (element-node element)
	  sum (node-current node) into current double-float
	  finally (return current))))

(defun setup-all-extracellular-electrodes ()
  (loop for extracellular-electrode being the hash-value of (EXTRACELLULAR-ELECTRODE-HASH-TABLE) do
	(setup-extracellular-electrode extracellular-electrode)))


(defun setup-extracellular-electrode (extracellular-electrode)
  (setf (extracellular-electrode-absolute-location extracellular-electrode)
	(coerce-location-to-float (extracellular-electrode-absolute-location extracellular-electrode)))
  (element-parameter
   extracellular-electrode
   'cell-elements-and-distance
   (loop for cell-element in (all-cell-elements)
	 collect (list (element-name cell-element) (as-the-crow-flies cell-element extracellular-electrode))))
  nil)
