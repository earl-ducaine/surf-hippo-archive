;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: SURF; Base: 10 -*-
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


;;; SYS Source file: sparse-data.lisp

(IN-PACKAGE "SURF")



(defun element-sparse-data-keys (element &optional data-type)
  (let* ((element (element element))
	 (model (type-symbol-model (type-of element)))
	 (data-type (or data-type (default-data-type element nil model))))
    (values (model-output-sparse-data-key model data-type)
	    (model-output-ordered-sparse-data-key model data-type))))

(defun element-sparse-data-key (element &optional data-type)
  (let ((element (element element))
	(data-type (or data-type (default-data-type element)))
	(model (element-model element)))
    (model-output-sparse-data-key model data-type)))

(defun element-ordered-sparse-data-key (element &optional data-type)
  (let ((element (element element))
	(data-type (or data-type (default-data-type element)))
	(model (element-model element)))
    (model-output-ordered-sparse-data-key model data-type)))

(defun update-sparse-data (&optional (elements (cell-elements)) (data-type 'voltage))
  (push *real-time* *reverse-sparse-data-times*)
  (store-element-sparse-data elements data-type)
  nil)

(defun store-element-sparse-data (element &optional data-type values)
  (loop for elt in (coerce-to-list element) do
	(let* ((element (element elt))
	       (params (element-parameters element)))
	  (multiple-value-bind (sparse-data-key ordered-sparse-data-key)
	      (element-sparse-data-keys elt data-type)
	    ;; If pushing new data, any ordered data is no longer valid.
	    (set-element-parameter-fast element ordered-sparse-data-key nil params)
	    (if values
		(let ((values-df (double-float-list values)))
		  (set-element-parameter-fast element sparse-data-key (reverse values-df) params)
		  (set-element-parameter-fast element ordered-sparse-data-key values-df params))
		(push-element-parameter element sparse-data-key
					(d-flt (element-current-value element data-type)) params)))))

  nil)

(defun clear-sparse-data (&optional (elements (cell-elements)) (data-type 'voltage))
  (clear-element-sparse-data elements data-type))
			     
(defun clear-element-sparse-data (element &optional data-type)
  (loop for elt in (coerce-to-list element) do
	(let* ((data-type (or data-type (default-data-type elt)))
	       (model (element-model elt))
	       (key (model-output-data-info model data-type 'sparse-data-param-key))
	       (ordered-sparse-data-key (model-output-data-info model data-type 'ordered-sparse-data-param-key)))
	  (when key
	    (element-parameter elt key nil)
	    (element-parameter elt ordered-sparse-data-key nil))))
  nil)

(defun d-flt-sparse-data (elements)
  (loop for elt in elements do
	(element-parameter elt :SPARSE-VOLTAGE-DATA
			   (mapcar #'(lambda (val) (coerce val 'double-float)) 
				   (element-parameter elt :SPARSE-VOLTAGE-DATA))))
  nil)

(defun retrieve-sparse-data (element target-time &optional data-type)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (when (current-sparse-data-times)
    (let* ((target-time (d-flt target-time))
	   (current-sparse-data-times (current-sparse-data-times))
	   (last-current-sparse-data-time (car (last current-sparse-data-times))))
      (declare (double-float target-time))
      (loop with last-data = nil
	    with last-time = nil
	    for time single-float in current-sparse-data-times
	    for data in (element-sparse-data element data-type)

	    when (< time target-time) do (setq last-time time last-data data)
	    else
	    when (> time target-time) do
	    (return (values (interpolate-data-df data (coerce time 'double-float)
						 (or last-data data) (coerce (or last-time target-time) 'double-float)
						 (d-flt target-time))
			    target-time))
	    when (or (= time last-current-sparse-data-time)
		     (= time target-time))
	    do (return (values data time))))))
			  


(proclaim '(inline get-ordered-element-sparse-data))
(defun get-ordered-element-sparse-data (element data-key ordered-data-key)
  (let* ((element (element element))
	 (params (typecase element
		   (segment (segment-parameters element))
		   (soma (soma-parameters element)))))
    (or (get-a-value ordered-data-key params)
	(set-element-parameter-fast element ordered-data-key (reverse (get-a-value data-key params)) params))))


(defun read-element-sparse-data (&key (data-type 'voltage) data-filename)
  "Write sparse data of DATA-TYPE associated with ELEMENTS."
  (with-open-stream (stream (open data-filename :direction :input))
    (let ((data (read stream)))
      ; (format t "read ~A ~%car ~A~%" data (car data))
      (loop for elt-values in data do
	    (store-element-sparse-data (car elt-values) data-type (cadr elt-values)))
      (set-sparse-data-times (cadr (read stream))))))

(defun set-sparse-data-times (times)
  (setq *sparse-data-times* times
	*reverse-sparse-data-times* (reverse times)))

(defun write-element-sparse-data (&key (elements (cell-elements)) (data-type 'voltage) data-filename)
  "Write sparse data of DATA-TYPE associated with ELEMENTS."
  (write-lisp-header data-filename)
  (with-open-stream (stream (open data-filename :direction :output :if-exists :supersede))
    (format stream "(~%")
    (loop for element in (coerce-to-list (element elements)) do
	  (format stream "  (~s~%" (element-name element))
	  (format-list (float-list (element-sparse-data element)) 10 stream 6 6 " " t t)
	  (format stream "  )~%"))
        (format stream ")~%~%~%")
    ;; Now write out the time list.
    (format stream "(current-sparse-data-times~%")
    (format-list (current-sparse-data-times) 10 stream 6 6 " " t t)
    (format stream "  )~%")
    (format t "File ~a written~%" data-filename)))


(defun plot-element-sparse-data (elements &key (data-type 'voltage) (y-label "mV"))
  "Plot sparse data of DATA-TYPE associated with ELEMENTS."
  (let ((elements (coerce-to-list (element elements))))
    (plot-timed-data
     (loop for elt in elements
	   collect (element-sparse-data elt))
     (element-name elements)
     (current-sparse-data-times)
     :x-min 0
     :x-label "ms"
     :y-label y-label
     :title (format nil "~A: Sparse ~A data" *simulation-name* data-type))))
   
   
   
(defun loaded-sparse-data-p (&optional (elements (cell-elements)) data-type)
  (loop for elt in elements
	when (element-sparse-data elt data-type) do (return t)))

(defun element-sparse-data (element &optional data-type)
  "Return a list of sparse data of DATA-TYPE for ELEMENT."
  (let ((element (element element)))
    (multiple-value-bind (sparse-data-key ordered-sparse-data-key)
	(element-sparse-data-keys element data-type)
      (get-ordered-element-sparse-data element sparse-data-key ordered-sparse-data-key))))
