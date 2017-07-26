;;;-*- mode: lisp; Syntax: Common-lisp; package: surf ; base: 10 ; -*-
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


;;; SYS Source file: buffer.lisp
;
; The model to integrate ion buffers.
;


(in-package "SURF-HIPPO")


(defun create-buffer-type (type-symbol &optional actual-type-symbol update-parameters)
  (when (stringp type-symbol) (setq type-symbol (intern type-symbol)))
  (let* ((type (unless actual-type-symbol
		 (if (buffer-type-p type-symbol)
		     type-symbol
		     (gethash (string type-symbol) (BUFFER-TYPE-HASH-TABLE)))))
	 (model (type-symbol-model 'buffer-type))
	 (original-parameters (retrieve-model-parameters-from-library type-symbol model))
	 (parent-type-symbol (get-a-value 'parent-type original-parameters)))
    (unless (and type (not update-parameters))
      (when (eq parent-type-symbol type-symbol)
	(sim-error (format nil "~A has a recursive parent type!" parent-type-symbol)))
      (unless original-parameters (sim-error (format nil "Don't know anything about conc buffer type ~A!" type-symbol)))
      (when actual-type-symbol (setq type-symbol actual-type-symbol))
      (unless type
  	(setq type (if parent-type-symbol
		       (create-BUFFER-TYPE parent-type-symbol type-symbol update-parameters)
		       (make-BUFFER-TYPE :name (string type-symbol)))))
      (setq original-parameters (update-element-parameters-with-new-parameters original-parameters type))    
      (when parent-type-symbol (push-element-parameter type 'parent-types parent-type-symbol))
      (setf (buffer-type-class type) (get-a-value 'class original-parameters))
      (case (buffer-type-class type)
	(:mm)
	(:FIRST-ORDER-TAU-V)
	(:first-order))
      (cond-every
       ((or (assoc 'qten original-parameters) (assoc 'q10 original-parameters))
	(setf (buffer-type-q10 type) (s-flt (cdr (or (assoc 'qten original-parameters) (assoc 'q10 original-parameters))))))
       ((assoc 'reference-temp original-parameters)
	(setf (buffer-type-reference-temp type)
	      (coerce (cdr (assoc 'reference-temp original-parameters)) 'single-float))))
      (unless (member type *make-needed-v-buffer-arrays*) (push type *make-needed-v-buffer-arrays*))
      (setf (gethash (string type-symbol) (BUFFER-TYPE-HASH-TABLE)) type))
    (setq *buffer-type* type)
    type))

(defun get-buffer-simple-name ()
  (loop for candidate from (max 1 *buffer-simple-name-counter*)
	until (not (gethash candidate (buffer-hash-table)))
	finally (return (setf *buffer-simple-name-counter* candidate))))

(defun rename-buffers-simple (&optional (buffers (buffers)))
  "Rename BUFFERS [default all buffers in circuit] with simple integer names."
  (loop for seg in buffers do
	(let ((name (get-buffer-simple-name)))
	  (set-element-name seg name 'buffer))))



(defun total-conc-full-eq (free-conc buffer-total-conc kd)
  (* free-conc (+ 1 (/ buffer-total-conc (+ kd free-conc)))))

(defun total-conc-beta (free-conc buffer-total-conc kd)
  (* free-conc (* free-conc (+ 1 (/ buffer-total-conc kd)))))

#|
(let ((buffer-total-conc 100)
      (label-list '())
      (data-list '()))
  (loop for kd in '(0.01 0.1 1.0)
	do
	(push (loop for free-conc from 0.01 to .5 by 0.01 collect (total-conc-beta free-conc buffer-total-conc kd))
	      data-list)
	(push (format nil "kd: ~A (beta)" kd) label-list)
	(push (loop for free-conc from 0.01 to .5 by 0.01 collect (total-conc-full-eq free-conc buffer-total-conc kd))
	      data-list)
	(push (format nil "kd: ~A (full)" kd) label-list)
	finally (plot-timed-data
		 data-list
		 label-list
		 (loop for free-conc from 0.01 to .5 by 0.01 collect free-conc))))

|#
