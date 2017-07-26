
;;; Copyright (C) 2004-2005 Yannick Gingras <ygingras@ygingras.net>

;;; Yannick Gingras grants you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.

(in-package #:poly-pen)

(defparameter *coords-storage-type* 'vector
  "the type used by VSE's coords")

(declaim (optimize speed (debug 1)))

(defclass vector-space-element ()
  ((coords :reader coords
	   :initform #(0 0 0)
	   :initarg :coords))
  (:documentation "A vector space element must support 'vector
  addition' and 'scalar multiplication'.  Good exemples include points
  and vectors (refered by 'vect' to prevent clash with CL's builtin
  one-dimension-array)."))

(defmethod coords-repr ((vse vector-space-element))
  (format nil "(~{~a~^, ~})" (coerce (coords vse) 'list)))

(defmethod print-object ((vse vector-space-element) stream)
  (format stream "#<~aD-~A ~a>"
	  (length (coords vse))
	  (class-name (class-of vse))
	  (coords-repr vse)))

(defun x (vse)
  (with-slots (coords) vse
    (declare (type simple-vector coords))
    (elt coords 0)))

(defun y (vse)
  (with-slots (coords) vse
    (declare (type simple-vector coords))
    (elt coords 1)))

(defun z (vse)
  (with-slots (coords) vse
    (declare (type simple-vector coords))
    (elt coords 2)))

(declaim (inline x y z))

(defmethod map-coords ((funct function) (vse vector-space-element) &rest args)
  (with-slots (coords) vse
    (make-instance (class-of vse)
		   :coords (apply 'map *coords-storage-type*
				  funct
				  coords
				  args))))

(defmethod add ((vse1 vector-space-element)
		(vse2 vector-space-element)
		&rest args)
  (apply #'map-coords #'+ vse1 (coords vse2) (mapcar #'coords args)))

(defmethod add ((vse1 vector-space-element)
		(coords sequence)
		&rest args)
  (apply #'map-coords #'+ vse1 coords args))

(defmethod mult ((c number) (vse vector-space-element))
  (map-coords #'(lambda (v) (* c v)) vse))

(defmethod mult ((vse vector-space-element) (c number))
  (mult c vse))

(defmethod inv ((vse vector-space-element))
  (mult -1 vse))

(defmethod div ((vse vector-space-element) (c number))
  (mult (/ 1 c) vse))

(defmethod sub ((vse1 vector-space-element)
		(vse2 vector-space-element)
		&rest args)
  (apply #'map-coords #'-
	 vse1
	 (coords vse2)
	 (mapcar #'coords args)))


(defmethod reduce-coords ((funct function) (vse vector-space-element))
  (apply funct (coords vse)))

(defmethod every-coords ((pred function) (vse vector-space-element))
  (every pred (coords vse)))

(defmethod some-coords ((pred function) (vse vector-space-element))
  (some pred (coords vse)))

(defclass point (vector-space-element)
  ())

(defmethod point ((coords vector))
  (make-instance 'point :coords coords))

(defmethod point ((coords list))
  (make-instance 'point :coords (coerce coords 'vector)))

(defmethod point ((vse vector-space-element))
  (make-instance 'point :coords (coords vse)))

(defmethod 3d-point ((x number) (y number) (z number))
  (make-instance 'point :coords (vector x y z)))

(defmethod 2d-point ((x number) (y number))
  (make-instance 'point :coords (vector x y)))

(defclass vect (vector-space-element)
  ())

(defmethod vect ((coords vector))
  (make-instance 'vect :coords coords))

(defmethod vect ((coords list))
  (make-instance 'vect :coords (coerce coords 'list)))

(defmethod vect ((vse vector-space-element))
  (make-instance 'vect :coords (coords vse)))

(defmethod 3d-vect ((x number) (y number) (z number))
  (make-instance 'vect :coords (list x y z)))

(defmethod 2d-vect ((x number) (y number))
  (make-instance 'vect :coords (list x y)))

(defmethod add ((vect vect) obj &rest args)
  "adding vectors is used for translations, not to get other vectors"
  (apply #'add obj vect args))

(defmethod add ((vect1 vect) (vect2 vect) &rest args)
  "the above method means that we must handle case for 2 vects"
  (let ((vect (map-coords #'+ vect1 (coords vect2))))
    (if args
	(apply #'add vect args)
	vect)))

(defmethod add (obj (vect list) &rest args)
  "convenient shorten notation in many cases: (add obj '(x y))"
  (apply #'add obj (vect vect) args))

(defmethod sub (obj (vect vect) &rest args)
  (apply #'add obj (inv vect) (mapcar #'inv args)))

(defmethod norm ((vect vect))
  (expt (reduce #'+ (map 'list #'(lambda (x) (* x x)) (coords vect)))
	1/2))

(defmethod normalize ((vect vect))
  (div vect (norm vect)))

(defclass line ()
  ((beg :initarg :beg :reader beg)
   (end :initarg :end :reader end))
  (:documentation "a line segment"))

(defmethod print-object ((line line) stream)
  (with-slots (beg end) line
    (format stream "#<~a ~a ~a>"
	    (class-name (class-of line))
	    (coords-repr beg)
	    (coords-repr end))))

(defmethod line ((beg point) (end point))
  ;; TODO: validate the number of coords
  (make-instance 'line :beg beg :end end))

(defmethod line ((beg sequence) (end sequence))
  (line (point beg) (point end)))

(defmethod vect ((line line))
  (with-slots (beg end) line
    (vect (sub end beg))))

(defmethod map-coords ((funct function) (line line) &rest args)
  (with-slots (beg end) line
    (line (apply #'map-coords funct beg args)
	  (apply #'map-coords funct end args))))

(defmethod reduce-coords ((funct function) (line line))
  (funcall funct
	   (apply funct (coords (beg line)))
	   (apply funct (coords (end line)))))

(defmethod every-coords ((pred function) (line line))
  (with-slots (beg end) line
    (and (every-coords pred beg)
	 (every-coords pred end))))

(defmethod some-coords ((pred function) (line line))
  (with-slots (beg end) line
    (or (some-coords pred beg)
	(some-coords pred end))))

(defmethod add ((line line) (vect vect) &rest args)
  (with-slots (beg end) line
    (line (apply #'add vect beg args)
	  (apply #'add vect end args))))

(defmethod norm ((line line))
  (norm (vect line)))

(defmethod middle ((line line))
  (add (beg line)
       (div (vect line) 2)))

(defclass rect ()
  ((top-left :initform 0 :initarg :top-left :reader top-left)
   (width    :initform 0 :initarg :width)
   (height   :initform 0 :initarg :height))
  (:documentation "This is a 2D rectangle flat on the XY plan.  For a
  3D poly, see quad.  Internal representation is in Cartesian
  coordinates."))

(defmethod width ((rect rect) &rest args)
  (slot-value rect 'width))

(defmethod height ((rect rect) &rest args)
  (slot-value rect 'height))

(defmethod print-object ((rect rect) stream)
  (with-slots (top-left width height) rect
    (format stream "#<RECT ~a ~ax~a>"
	    (coords-repr top-left) width height)))

(defmethod rect ((vertex1 point) (vertex2 point))
  "Both vertice must be end points of a diagonal of the rectangle.
  Any diagonal will do."
  (let ((diag (vect (line vertex1 vertex2))))
    (make-instance 'rect
		   :top-left (2d-point (min (x vertex1) (x vertex2))
				       (max (y vertex1) (y vertex2)))
		   :width (abs (x diag))
		   :height (abs (y diag)))))

(defmethod rect ((vertex1 sequence) (vertex2 sequence))
  (rect (point vertex1) (point vertex2)))

(defmethod rect ((top-left point) (size vect))
  "Everything ends up below and right of top-left, no matter what the
  direction of size is."
  (rect top-left (2d-point (+ (x top-left) (abs (x size)))
			   (- (y top-left) (abs (y size))))))

(defmethod rect ((top-left list) (size vect))
  (rect (apply #'2d-point top-left) size))

(defmethod bottom-right ((rect rect))
  (add (top-left rect) (2d-vect (width rect) (- (height rect)))))

(defmethod left ((rect rect))
  (x (top-left rect)))

(defmethod right ((rect rect))
  (x (bottom-right rect)))

(defmethod top ((rect rect))
  (y (top-left rect)))

(defmethod bottom ((rect rect))
  (y (bottom-right rect)))

(defmethod size ((rect rect))
  (2d-vect (width rect) (height rect)))

(defmethod add ((rect rect) (vect vect) &rest args)
  (make-instance 'rect
		 :top-left (apply #'add (top-left rect) vect args)
		 :width  (width rect)
		 :height (height rect)))

(defmethod map-coords ((funct function) (rect rect) &rest args)
  (rect (apply #'map-coords funct (top-left rect) args)
	(apply #'map-coords funct (bottom-right rect) args)))

(defmethod every-coords ((pred function) (rect rect))
  (and (every-coords pred (top-left rect))
       (every-coords pred (bottom-right rect))))

(defmethod some-coords ((pred function) (rect rect))
  (or (some-coords pred (top-left rect))
      (some-coords pred (bottom-right rect))))

(defmethod left-side ((rect rect))
  (with-slots (top-left height) rect
    (line (sub top-left (2d-vect 0 height))
	  top-left)))

(defmethod top-side ((rect rect))
  (with-slots (top-left width) rect
    (line top-left
	  (add top-left (2d-vect width 0)))))

(defmethod right-side ((rect rect))
  (let ((bottom-right (bottom-right rect))
	(height (height rect)))
    (line (add bottom-right (2d-vect 0 height))
	  bottom-right)))

(defmethod bottom-side ((rect rect))
  (let ((bottom-right (bottom-right rect))
	(width (width rect)))
    (line (add bottom-right (2d-vect width 0))
	  bottom-right)))

(defmethod middle ((rect rect))
  (add (top-left rect)
       (2d-vect (/ (width rect) 2)
		(/ (height rect) -2))))

(defclass circle ()
  ((center :initform (2d-point 0 0) :initarg :center :reader center)
   (radius :initform 1 :initarg :radius :reader radius)))

(defmethod print-object ((circle circle) stream)
  (format stream "#<~A ~ax~a>"
	  (class-name (class-of circle))
	  (coords-repr (center circle))
	  (radius circle)))

(defmethod middle ((circle circle))
  (center circle))

(defmethod circle ((center point) (radius number))
  (make-instance 'circle :center center :radius radius))

(defmethod circle ((center list) (radius number))
  (circle (point center) radius))

(defmethod add ((circle circle) (vect vect) &rest args)
  (make-instance 'circle
		 :center (apply #'add (center circle) vect args)
		 :radius (radius circle)))

(defmethod map-coords ((funct function) (circle circle) &rest args)
  (circle (apply #'map-coords funct (center circle) args)
	  (radius circle)))

(defmethod reduce-coords ((funct function) (circle circle))
  (apply funct (coords (center circle))))

(defmethod every-coords ((pred function) (circle circle))
  (every-coords pred (center circle)))

(defmethod some-coords ((pred function) (circle circle))
  (some-coords pred (center circle)))

(defclass text ()
  ((bottom-left :initform (2d-point 0 0) :initarg :bottom-left
		:reader bottom-left)
   (msg :initform "" :initarg :msg :reader msg))
  (:documentation "a translated string"))

(defmethod print-object ((text text) stream)
  (format stream "#<~A at ~a ~s>"
	  (class-name (class-of text))
	  (coords-repr (bottom-left text))
	  (msg text)))

(defmethod add ((msg string) (vect vect) &rest args)
  (make-instance 'text
		 :bottom-left (apply #'add (2d-point 0 0) vect args)
		 :msg msg))

(defmethod add ((text text) (vect vect) &rest args)
  (make-instance 'text
		 :bottom-left (apply #'add (bottom-left text) vect args)
		 :msg (msg text)))

(defmethod map-coords ((funct function) (text text) &rest args)
  (with-slots (bottom-left msg) text
    (make-instance 'text
		   :bottom-left (apply #'map-coords funct bottom-left args)
		   :msg msg)))

;; arch-tag: 47d3b303-0727-4640-8ff2-02acb946d6e5
