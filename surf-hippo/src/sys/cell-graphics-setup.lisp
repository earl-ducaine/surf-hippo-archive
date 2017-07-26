;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: SURF-HIPPO ; Base: 10; -*-
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


;;; SYS Source file: cell-graphics-setup.lisp

;;; Variables, instances and some function definitions fon graphics.

(IN-PACKAGE "SURF-HIPPO")

(defvar *cell-segment-color* nil)
(defvar *CHOSEN-ONE-SHADING* 50)
(defvar *marked-node-fill* (get-opal-color-to-fill 'black 15))
(defvar *plotted-node-fill* (get-opal-color-to-fill 'black 35))

;; These must be fixnums
(defvar *voltage-color-step* 2)		;mV
(defvar *voltage-color-min* -100)		;mV
(defvar *voltage-color-max* 50)		;mV

(defun voltage-color-dimension ()
  (round (/ (- *voltage-color-max* *voltage-color-min*)
	    *voltage-color-step*)))

(defvar *voltage-color-step-df* (coerce *voltage-color-step* 'double-float))
(defvar *voltage-color-max-/step* (round (/ *voltage-color-max* *voltage-color-step*)))
(defvar *voltage-color-min-/step* (round (/ *voltage-color-min* *voltage-color-step*)))
(defvar *dendrite-gray-lines*
  (loop for i from 0 to 20 collect
      (create-instance (read-from-string (format nil "dendrite-~d-gray-thickness" i))
		       opal:line-style
		       (:constant t)
		       (:foreground-color (get-color-from-library 0.8 0.8 0.8))
		       (:line-thickness i))))

(defvar *dendrite-red-lines*
  (loop for i from 0 to 20 collect
      (create-instance (read-from-string (format nil "dendrite-~d-red-thickness" i))
		       opal:line-style
		       (:constant t)
		       (:foreground-color (get-opal-color 'red))
		       (:line-thickness i))))

(defvar *dendrite-light-red-lines*
  (loop for i from 0 to 20 collect
      (create-instance (read-from-string (format nil "dendrite-~d-light-red-thickness" i))
		       opal:line-style
		       (:constant t)
		       (:foreground-color (get-color-from-library 1.0 0.3 0.4))
		       (:line-thickness i))))

(defvar *dendrite-lines*
  (loop for i from 0 to 20 collect
	(create-instance (read-from-string (format nil "dendrite-~d-thickness" i))
			 opal:line-style
			 (:constant t)
			 (:foreground-color (get-color-from-library 0.0 0.0 0.0))
			 (:line-thickness i))))
		       

(defvar *minimum-segment-thickness* 0)	; pixels
(defvar *maximum-segment-thickness* 49)	; pixels

(defun segment-thickness-dimension ()
  (+ 1 (- *maximum-segment-thickness* *minimum-segment-thickness*)))

(proclaim '(fixnum
	    *voltage-color-step* *voltage-color-min* *voltage-color-max*
	    *minimum-segment-thickness* *maximum-segment-thickness*))

(proclaim '(inline color-styles-voltage-index))
(defun color-styles-voltage-index (voltage)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (double-float voltage))
  (let ((voltage/step (the fn (round (/ voltage (the df *voltage-color-step-df*))))))
    (if (<= voltage/step (the fn *voltage-color-min-/step*))
	0				
	(1- (- (the fn (if (< voltage/step (the fn *voltage-color-max-/STEP*))
		       voltage/step
		       (the fn *voltage-color-max-/STEP*)))
	   (the fn *voltage-color-min-/step*))))))


(defvar *cell-color-line-styles* (make-array (list (segment-thickness-dimension) (voltage-color-dimension))))
(defvar *cell-fill-styles* (make-array (list (voltage-color-dimension))))
(defvar *voltage-colors* (make-array (list (voltage-color-dimension))))


(defun fill-*cell-fill-styles* ()
  (loop for voltage from (coerce *voltage-color-min* 'single-float) by *voltage-color-step*
	for color-index from 0 to (1- (voltage-color-dimension))
	do
	(let ((color (get-opal-variable-color voltage)))
					; 	  (format t "Index ~A voltage ~A color ~A~%" color-index voltage color)
	  (loop for thickness from *minimum-segment-thickness* to *maximum-segment-thickness* do
		(when (= 0 thickness)
		  (setf (aref *cell-fill-styles* color-index) (get-opal-color-to-fill color))
		  (setf (aref *voltage-colors* color-index) color))			
		(setf (aref *cell-color-line-styles* thickness color-index)
		      (create-instance nil opal:line-style
				       (:constant nil)
				       (:foreground-color color)
				       (:line-thickness thickness)))))))


(eval-when (load) (fill-*cell-fill-styles*))

(proclaim '(notinline access-*line-styles-array*-for-segments))
(defun access-*line-styles-array*-for-segments (thickness-arg color-or-color-index
							&optional (shading 100.0) dash thickness-arg-is-a-fix)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (access-*line-styles-array* thickness-arg
			(typecase  color-or-color-index
			  (number (aref (the (simple-array SCHEMA (*)) *voltage-colors*) color-or-color-index))
			  (t color-or-color-index))
			shading dash thickness-arg-is-a-fix))


(proclaim '(notinline access-*line-styles-array*-for-segments-fast))
(defun access-*line-styles-array*-for-segments-fast (thickness-arg color shading-argument color-index)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (values schema))
  (access-*line-styles-array*-fast thickness-arg color shading-argument color-index))
			     

(defvar *debug-access-*line-styles-array*-for-segment-voltage* nil)

(proclaim '(inline access-*line-styles-array*-for-segment-voltage))
(defun access-*line-styles-array*-for-segment-voltage (&optional (thickness-arg 0) segment)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (fixnum thickness-arg))
  (let ((ok-voltage-arg (color-styles-voltage-index (the df (if segment (get-segment-voltage-2 segment) 0.0d0)))))
    (if	(<= 0 thickness-arg 49)
	(aref (the (simple-array schema (* *)) *cell-color-line-styles*) thickness-arg ok-voltage-arg)
	(let ((color (not-inline-get-opal-variable-color-segment segment)))
	  (create-instance nil opal:line-style
			   (:join-style :round)
			   (:line-thickness (max 0 thickness-arg))
			   (:line-style :SOLID)
			   (:color color)
			   (:foreground-color color))))))

(defun not-inline-get-opal-variable-color-segment (segment)
  (get-opal-variable-color (s-flt (if segment (get-segment-voltage-2 segment) 0.0d0))))


(proclaim '(inline access-*line-styles-array*-for-soma-voltage))
(defun access-*line-styles-array*-for-soma-voltage (soma thickness-arg)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  opal:default-line-style
;  (aref (the (simple-array schema (* *)) *cell-line-styles*)
;        (min 49 (max 0 (the fn thickness-arg)))
;        (max 0
;             (min 149
;                  (the fn
;                       (+ 100 (the fn (round (the df (node-voltage-n (soma-node soma))))))))))
  )

(proclaim '(notinline access-*fill-styles*-for-soma-voltage))
(defun access-*fill-styles*-for-soma-voltage (soma)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (aref (the (simple-array schema (*)) *cell-fill-styles*)
	(color-styles-voltage-index (node-voltage-n (soma-node soma)))))



