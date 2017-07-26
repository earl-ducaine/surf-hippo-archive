
;;; Copyright (C) 2004-2005 Yannick Gingras <ygingras@ygingras.net>

;;; Yannick Gingras grants you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.

(in-package #:poly-pen)

(defun histogram-occur-scale (max-occur)
  "Returns max-graduation, main-graduation and sub-graduation."
  ;; FIXME: the scales are really not that good, it needs rework.
  (let ((least-pow (expt 10 (floor (log max-occur 10)))))
    (cond ((<= max-occur 5)
	   (values 5 1 1))
	  ((<= max-occur 10)
	   (values 10 5 1))
	  ((eql max-occur least-pow)
	   (values max-occur (/ max-occur 10) (/ max-occur 20)))
	  ((< (floor max-occur least-pow) 3)
	   (values (if (eql max-occur
			    (* least-pow (floor max-occur least-pow)))
		       max-occur
		       (* (1+ (floor max-occur (/ least-pow 2)))
			  (/ least-pow 2)))
		   (/ least-pow 2)
		   (/ least-pow 10)))
	  ((< (floor max-occur least-pow) 7)
	   (values (if (eql max-occur
			    (* least-pow (floor max-occur least-pow)))
		       max-occur
		       (* (1+ (floor max-occur least-pow)) least-pow))
		   least-pow
		   (/ least-pow 2)))
	  (t (values (if (eql max-occur
			      (* least-pow (floor max-occur least-pow)))
			 max-occur
			 (* 10 least-pow))
		     least-pow
		     (/ least-pow 2))))))

(defun histogram-classes (data nb-class &optional discrete)
  (let ((min (first data))
	(max (first data)))
    (dolist (x (rest data))
      (if (< max x)
	  (setf max x))
      (if (> min x)
	  (setf min x)))
    (let* ((range (funcall (if discrete #'ceiling #'identity)
			   (/ (max 1 (- max min)) nb-class)))
	   (nb-class (if discrete
			 (min nb-class (1+ (round (/ (- max min) range))))
			 nb-class))
	   (classes (make-array nb-class
				:element-type 'integer
				:initial-element 0)))
      (dolist (x data)
	(let ((class (floor (- x min) range)))
	  (incf (aref classes
		      (if (eql class nb-class) (1- class) class)))))
      (values classes range min max nb-class))))

(defmethod histogram ((data list)
		      &key (canvas *canvas*)
		      (text-col *default-text-color*)
		      (nb-class 10)
		      (outline-width 2)
		      (back-col (rgb '(1 1 1)))
		      (margin 30)
		      (font *default-font*)
		      cols
		      title
		      title-font
		      discrete
		      (x-label "value")
		      (y-label "frequency")
		      &allow-other-keys)
  ;; FIXME: This is just too ugly an need a massive rework
  (multiple-value-bind (classes range min max nb-class)
      (histogram-classes data nb-class discrete)
    (multiple-value-bind (max-grad main-grad min-grad)
	(histogram-occur-scale (apply #'max (coerce classes 'list)))
      (let* ((*canvas* canvas)
	     (bar-width (/ (- (width canvas)
			      (* 2 margin)
			      (- (* 2 outline-width)))
			   nb-class))	     
	     (bar-height-inc (/ (- (height canvas)
				   (* 2 margin)
				   (* 2 outline-width)) max-grad))
	     (cols (or cols (make-color-spiral nb-class .99 .99 .85 .09 .05)))
	     (title-font (or title-font font)))
	;; background
	(draw (rect (2d-point 0 0)
		    (2d-point (1- (width canvas))
			      (1- (height canvas))))
	      :col back-col
	      :filled t)
	;; bars
	(dotimes (class nb-class)
	  (if (< 0 (elt classes class))
	      (draw (rect (2d-point (+ margin (* class bar-width)
				       outline-width)
				    (+ margin outline-width))
			  (2d-point (+ margin
				       (* class bar-width)
				       bar-width
				       (- outline-width))
				    (max (+ outline-width margin)
					 (+ margin
					    (/ outline-width -2)
					    (* bar-height-inc
					       (elt classes class))))))
		    :outline-width outline-width
		    :outline-col (hsv '(.1 1 .8 ))
		    :map-coords #'round
		    :filled t
		    :col (elt cols class) :colasd (hsv '(.1 .85 .99)))))
	;; y axis
	(draw (add (line '(0 0) `(0 ,(* max-grad bar-height-inc)))
		   (2d-vect margin margin))
	      :col text-col)
	(dotimes (x (1+ (/ max-grad min-grad)))
	  (draw (add (line '(-3 0) '(0 0))
		     (2d-vect margin margin)
		     (2d-vect 0 (* x min-grad bar-height-inc)))
		:map-coords #'round
		:col text-col))
	(dotimes (x (1+ (/ max-grad main-grad)))
	  (let ((offset (add (2d-vect margin margin)
			     (2d-vect 0 (* x main-grad bar-height-inc))))
		(label (format nil "~a" (* x main-grad))))
	    (draw (add (line '(-6 0) '(0 0)) offset)
		  :map-coords #'round
		  :col text-col)
	    (draw (add label offset
		       (2d-vect (- -6
				   (width label :font font))
				(/ (height label :font font) -2)))
		  :map-coords #'round
		  :col text-col )))
	;; x axis
	(draw (add (line '(0 0) `(,(* nb-class bar-width) 0))
		   (2d-vect margin margin))
	      :col text-col)
	(dotimes (x (1+ nb-class))
	  (let ((offset (add (2d-vect margin margin)
			     (2d-vect (* x bar-width) 0)))
		(label (format nil (if discrete "~d" "~1$")
			       (+ min (* x range)))))
	    (draw (add (line '(0 0) '(0 -3)) offset )
		  :col text-col
		  :map-coords #'round)
	    (if (not (and discrete (eql nb-class x) (eql range 1)))
		(draw (add label
			   (2d-vect (if discrete
					(+ (if (eql 1 range) (/ bar-width 2) 0)
					   (/ (width label :font font) -2.5))
					(- (width ".0" :font font)
					   (width label :font font)))
				    (- -3 (height label :font font)))
			   offset )
		      :col text-col
		      :map-coords #'round))))
	;; Title
	(if title
	    (draw (add title
		       (2d-vect (- (/ (width canvas) 2)
				   (/ (width title :font title-font) 2))
				(- (height canvas)
				   (/ margin 2))))
		  :font title-font
		  :map-coords #'round))
	;; labels
	(if x-label
	    (draw (add x-label
		       (2d-vect (- (width canvas)
				   (/ margin 2)
				   (width x-label))
				(/ margin 4)))
		  :col text-col
		  :map-coords #'round))
	(if y-label
	    (draw (add y-label
		       (2d-vect (/ margin 8)
				(- (height canvas)
				   (/ margin 1.1))))
		  :col text-col
		  :map-coords #'round)))))
  canvas)


;; arch-tag: 99502817-9546-48a0-8f92-7e1525d82611
