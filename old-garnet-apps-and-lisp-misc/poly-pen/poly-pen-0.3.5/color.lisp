
;;; Copyright (C) 2004-2005 Yannick Gingras <ygingras@ygingras.net>

;;; Yannick Gingras grants you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.

(in-package #:poly-pen)

(defclass color ()
  ()
  (:documentation "generic color"))

;; TODO: validate ranges
(defclass rgb (color)
  ((r :initform 0 :initarg :r)
   (g :initform 0 :initarg :g)
   (b :initform 0 :initarg :b))
  (:documentation "all channels in [0..1]"))

(defmethod print-object ((rgb rgb) stream)
  (with-slots (r g b) rgb
    (format stream "#<~a ~a ~a ~a>"
	    (class-name (class-of rgb))
	    r g b)))

(defmethod rgb ((params list))
  (destructuring-bind (r g b) params
    (make-instance 'rgb :r r :g g :b b)))

(defmethod rgb ((rgb rgb))
  rgb)

(defmethod add ((rgb rgb) (vect vect) &rest args)
  (with-slots (r g b) rgb
    (let ((rgb (rgb (list (max 0 (min 1 (+ r (x vect))))
			  (max 0 (min 1 (+ g (y vect))))
			  (max 0 (min 1 (+ b (z vect))))))))
      (if args
	  (apply #'add rgb args)
	  rgb))))

(defclass rgb-24 (rgb)
  ((r :initform 0 :initarg :r)
   (g :initform 0 :initarg :g)
   (b :initform 0 :initarg :b))
  (:documentation "all channels in [0..255]"))

(defmethod rgb-24 ((params list))
  (destructuring-bind (r g b) params
    (make-instance 'rgb-24 :r r :g g :b b)))

(defclass hsv (color)
  ((h :initform 0 :initarg :h)
   (s :initform 0 :initarg :s)
   (v :initform 0 :initarg :v))
  (:documentation "all channels in [0..1]"))

(defmethod print-object ((hsv hsv) stream)
  (with-slots (h s v) hsv
    (format stream "#<HSV ~a ~a ~a>" h s v)))

(defmethod hsv ((params list))
  (destructuring-bind (h s v) params
    (make-instance 'hsv :h h :s s :v v)))

(defmethod hsv ((hsv hsv))
  hsv)

(defmethod rgb ((hsv hsv))
  "OO version of hsv->rgb"
  (with-slots (h s v) hsv
    (rgb (hsv->rgb h s v))))

(defmethod hsv ((rgb rgb))
  "OO version of rgb->hsv"
  (with-slots (r g b) rgb
    (hsv (rgb->hsv r g b))))

(defmethod rgb-24 ((rgb rgb))
  (with-slots (r g b) rgb
    (make-instance 'rgb-24
		   :r (round r 1/255)
		   :g (round g 1/255)
		   :b (round b 1/255))))

(defmethod rgb-24 ((rgb-24 rgb-24))
  rgb-24)

(defmethod rgb-24 ((hsv hsv))
  (rgb-24 (rgb hsv)))

(defmethod add ((rgb-24 rgb-24) (vect vect) &rest args)
  (with-slots (r g b) rgb-24
    (let ((rgb-24 (rgb-24 (list (max 0 (min 255 (+ r (x vect))))
				(max 0 (min 255 (+ g (y vect))))
				(max 0 (min 255 (+ b (z vect))))))))
      (if args
	  (apply #'add rgb-24 args)
	  rgb-24))))

(defmethod rgb ((rgb-24 rgb-24))
  (with-slots (r g b) rgb-24
    (make-instance 'rgb
		   :r (/ r 255)
		   :g (/ g 255)
		   :b (/ b 255))))

(defmethod hsv ((rgb-24 rgb-24))
  (hsv (rgb rgb-24)))

(defmethod add ((hsv hsv) (vect vect) &rest args)
  (with-slots (h s v) hsv
    (let ((hsv (hsv (list (rem (+ h (x vect)) 1)
			  (max 0 (min 1 (+ s (y vect))))
			  (max 0 (min 1 (+ v (z vect))))))))
      (if args
	  (apply #'add hsv args)
	  hsv))))

(defun make-color-spiral (nb-col
                          start-rad
			  &optional
                          stop-rad
			  (sat .85)
			  (tilt-angle 0)
                          (nb-loop 1)
                          (clockwise t)
			  (fact 1.0))
  
  "Pick some colors along a spiral path in a tsv color wheel (fixed sat).

  nb-cols: how many colors do you want ?
  tilt-angle: in [0..1], the fraction of a complete loop that leads to
              the starting tint
  start-rad: in [0..1] the starting radius
  stop-rad: in [0..1] the stoping radius
  nb-loop: how many times should we loop around the color wheel?

  Yeah it's a bit obscure until you play enough with it, here is a few
  examples:
    to make a shade from dark red to brigh green: (1000 .2 1 0 .5)
    to make a shade of 2 rainbows, the 1st being brighter, starting
    with green: (1000 .85 .30 .5 2)"
  (let* ((stop-rad (if stop-rad stop-rad start-rad))
	 (angle-inc (* (if clockwise 1.0 -1.0) nb-loop))
	 (val-inc (- stop-rad start-rad)))
    (coerce (loop for i in (make-log-pos nb-col fact) collect
		 (hsv (list (mod (+ tilt-angle (* i angle-inc)) 1)
			    sat
			    (+ start-rad (* i val-inc)))))
	    'vector)))

(defun make-color-log-spiral (nb-cols
			      start-rad
			      &optional
			      stop-rad
			      (sat .85)
			      (tilt-angle 0)
			      (nb-loop 1)
			      (clockwise t)
			      (fact 1.07))
  
  "Like make-color-spiral but the path logarithmic"
  (let* ((colors nil)
	 (stop-rad (if stop-rad stop-rad start-rad))
	 (angle-inc (* (if clockwise -1.0 1.0) nb-loop))
	 (max-val (expt fact nb-cols))
	 
	 (val-inc (- stop-rad start-rad )))
    (dotimes (i nb-cols (coerce (nreverse colors)
				'vector))
      (push (hsv (list (mod (+ tilt-angle
			       (* angle-inc (/ (expt fact (1+ i))
					       max-val)))
			    1)
		       sat
		       (+ start-rad
			  (* val-inc (/ (expt fact (1+ i)) max-val)))))
	    colors))))




(defun make-color-spiral-on-sat (nb-cols
				 start-rad
				 &optional
				 stop-rad
				 (val .85)
				 (tilt-angle 0)
				 (nb-loop 1)
				 (clockwise t))
  
  "Like make-color-spiral but the path is made on the saturation"
  (let* ((colors nil)
	 (stop-rad (if stop-rad stop-rad start-rad))
	 (angle-inc (/ (if clockwise 1.0 -1.0) (/ nb-cols nb-loop)))
	 (sat-inc (/ (- stop-rad start-rad) nb-cols)))
    (dotimes (i nb-cols (coerce (nreverse colors) 'vector))
      (push (hsv (list (mod (+ tilt-angle (* i angle-inc)) 1)
		       (+ start-rad (* i sat-inc))
		       val))
	    colors))))

(defun make-color-log-spiral-on-sat (nb-cols
				     start-rad
				     &optional
				     stop-rad
				     (val .85)
				     (tilt-angle 0)
				     (nb-loop 1)
				     (clockwise t)
				     (fact 1.07))
  
  "Like make-color-spiral but the path logarithmic"
  (let* ((colors nil)
	 (stop-rad (if stop-rad stop-rad start-rad))
	 (angle-inc (* (if clockwise -1.0 1.0) nb-loop))
	 (max-sat (expt fact nb-cols))
	 
	 (sat-inc (- stop-rad start-rad )))
    (dotimes (i nb-cols (coerce (nreverse colors)
				'vector))
      (push (hsv (list (mod (+ tilt-angle
			       (* angle-inc (/ (expt fact (1+ i))
					       max-sat)))
			    1)
		       (+ start-rad
			  (* sat-inc (/ (expt fact (1+ i)) max-sat)))
		       val
		       ))
	    colors))))


;; arch-tag: baf7e125-7228-46d3-a2f5-35e78f3b94b3
