
;;; Copyright (C) 2004-2005 Yannick Gingras <ygingras@ygingras.net>

;;; Yannick Gingras grants you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.

(in-package #:poly-pen)

(defvar *canvas* nil "the default canvas to draw on")
(defparameter *default-width*  100)
(defparameter *default-height* 100)
(defparameter *default-font-size* 24)
(defparameter *default-font* nil)
(defparameter *default-backend* :sdl)
(defparameter *default-color* (rgb '(0 0 0)))
(defparameter *default-text-color* (rgb '(.7 .7 .7)))
(defparameter *font-hash* (find-fonts))


(defclass canvas ()
  ((last-text-pos :initform (2d-point 0 0))
   (width  :initarg :width)
   (height :initarg :height)))

(defmethod width ((canvas canvas) &rest args)
  (slot-value canvas 'width))

(defmethod height ((canvas canvas) &rest args)
  (slot-value canvas 'height))

(defclass bitmap-canvas (canvas)
  ())

(defclass vector-canvas (canvas)
  ())

(defmethod dispatch-canvas (width height backend &key &allow-other-keys)
  'not-implemented)

(defmethod canvas (&rest args &key (backend *default-backend*)
		   (width *default-width*)
		   (height *default-height*)
		   &allow-other-keys)
  (apply #'dispatch-canvas width height backend args))

(defmethod free ((canvas canvas))
  "Unfortunatly some backends use structures that dont get GCed"
  'done)

(defmacro with-canvas ((var &rest args) &body body)
  `(let ((*canvas* (canvas ,@args)))
     (prog1
	 (let ((,var *canvas*))
	   ,@body)
       (free *canvas*))))

(defmacro with-default-canvas ((&rest args) &body body)
  `(let ((*canvas* (canvas ,@args)))
     (prog1 (progn ,@body)
       (free *canvas*))))

(defmacro with-default-backend ((backend) &body body)
  `(let ((*default-backend* ,backend ))
     ,@body))

(defmacro with-default-colors ((pen-color &optional text-color) &body body)
  `(let ((*default-color* ,pen-color))
     (let (,@(if text-color `((*default-text-color* ,text-color))))
       ,@body)))

(defmethod dispatch-save ((path pathname) canvas
			  &key &allow-other-keys)
  'not-implemented)

(defmethod save ((path pathname) &rest args
		 &key (canvas *canvas*) &allow-other-keys)
  (apply #'dispatch-save path canvas args))

(defmethod dispatch-save ((stream stream) canvas
			  &key &allow-other-keys)
  'not-implemented)

(defmethod save ((stream stream) &rest args
		 &key (canvas *canvas*) &allow-other-keys)
  (apply #'dispatch-save stream canvas args))

(defmethod dispatch-image ((path pathname) backend
			   &key &allow-other-keys)
  'not-implemented)

(defmethod image ((path pathname) &rest args
		  &key (backend *default-backend*)
		  &allow-other-keys)
  "open an image file"
  (apply #'dispatch-image path backend args))

(defmacro with-image ((var path &rest args) &body body)
  `(let ((*canvas* (image ,path ,@args)))
     (prog1 (let ((,var *canvas*))
	      ,@body)
       (free *canvas*))))

(defmacro with-default-image ((path &rest args) &body body)
  `(let ((*canvas* (image ,path ,@args)))
     (prog1 (progn ,@body)
       (free *canvas*))))

(defmethod dispatch-remap ((point point) canvas
			   &key &allow-other-keys)
  "Map the point in the canvas native coordinate system."
  point)

(defmethod remap ((point point) &rest args
		  &key canvas &allow-other-keys)
  (apply #'dispatch-remap point (if canvas canvas *canvas*) args))

(defmethod draw :around (shape &rest args
			       &key map-coords &allow-other-keys)
  (apply #'call-next-method
	 (if map-coords
	     (map-coords map-coords shape)
	     shape)
	 args))

(declaim (inline draw-on))
(defun draw-on (shape canvas &rest args &key (col *default-color*) 
		&allow-other-keys)
  (apply #'dispatch-draw shape col canvas args))

(defmethod dispatch-draw ((point point) (col color) canvas
			  &key &allow-other-keys)
  'not-implemented)

(defmethod draw ((point point) &rest args
		 &key (canvas *canvas*)
		 (col *default-color*) &allow-other-keys)
  (apply #'dispatch-draw point col canvas args))

(defmethod dispatch-draw ((line line) (col color) canvas
			  &key &allow-other-keys)
  'not-implemented)

(defmethod draw ((line line) &rest args
		 &key (canvas *canvas*)
		 (col *default-color*) &allow-other-keys)
  (apply #'dispatch-draw line col canvas args))

(defmethod dispatch-draw ((rect rect) (col color) canvas
			  &key &allow-other-keys)
  'not-implemented)

(defmethod draw ((rect rect) &rest args
		 &key (canvas *canvas*)
		 (col *default-color*)
		 (outline-col (rgb '(1 1 1)))
		 (outline-width 0) &allow-other-keys)
  (if (< 0 outline-width)
      (let ((shift (2d-vect (- outline-width) outline-width)))
	(draw (rect (add (top-left rect)  shift)
		    (sub (bottom-right rect) shift))
	      :col outline-col :canvas *canvas* :filled t)))
  (apply #'dispatch-draw rect col canvas args))


(defmethod dispatch-draw ((circle circle) (col color) canvas
			  &key &allow-other-keys)
  'not-implemented)

(defmethod draw ((circle circle) &rest args
		 &key (canvas *canvas*)
		 (col *default-color*) &allow-other-keys)
  (apply #'dispatch-draw circle col canvas args))

(defclass font ()
  ())

(defmethod text-width ((string string) font)
  'not-implemented)

(defmethod text-height ((string string) font)
  'not-implemented)

(defmethod ascent (font)
  'not-implemented)

(defmethod descent (font)
  'not-implemented)

(defmethod width ((string string) &rest args &key
		  (font *default-font*)
		  &allow-other-keys)
  (text-width string font))

(defmethod height ((string string) &rest args &key
		   (font *default-font*)
		   &allow-other-keys)
  (text-height string font))

(defmethod dispatch-font ((path pathname) (size number) backend
			  &key &allow-other-keys)
  'not-implemented)

(defmethod font ((path pathname) &rest args &key (backend *default-backend*)
		 (size *default-font-size*) &allow-other-keys)
  (apply #'dispatch-font path size backend args))

(defmethod font ((name string) &rest args &key (backend *default-backend*)
		 (size *default-font-size*) &allow-other-keys)
  (let ((path (gethash name *font-hash*)))
    (if path
	(apply #'dispatch-font path size backend args)
	(error "can't find font, try with full path"))))

(defmethod free ((font font))
  "Unfortunatly some backends use structures that dont get GCed"
  'done)

(defmacro with-font ((var path-or-name &rest args) &body body)
  `(let ((*default-font* (font ,path-or-name ,@args)))
     (prog1 (let ((,var *default-font*))
	      ,@body)
       (free *default-font*))))

(defmacro with-default-font ((path-or-name &rest args) &body body)
  `(let ((*default-font* (font ,path-or-name ,@args)))
     (prog1 (progn ,@body)
       (free *default-font*))))

(defmacro with-defaults ((&key canvas image backend font colors) &body body)
  `(,@(if backend `(with-default-backend (,backend)) `(progn))
      (,@(if canvas `(with-default-canvas (,@canvas)) `(progn))
	 (,@(if image `(with-default-image (,@image)) `(progn))
	    (,@(if colors `(with-default-colors (,@colors)) `(progn))
	       (,@(if font `(with-default-font (,@font)) `(progn))
		  ,@body))))))

(defmethod dispatch-draw ((text text) (col color) canvas
			  &key &allow-other-keys)
  'not-implemented)

(defmethod draw ((text text) &rest args
		 &key (canvas *canvas*)
		 (col *default-text-color*) &allow-other-keys)
  (apply #'dispatch-draw text col canvas args))

(defmethod draw ((text string) &rest args
		 &key (canvas *canvas*) &allow-other-keys)
  (apply #'draw (add text (vect (slot-value canvas 'last-text-pos))) args))

;; arch-tag: 41744eb5-9eb5-43f0-8b5a-1490b8ae177a
