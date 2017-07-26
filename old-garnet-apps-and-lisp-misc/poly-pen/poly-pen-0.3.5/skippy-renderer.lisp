
;;; Copyright (C) 2004-2005 Yannick Gingras <ygingras@ygingras.net>

;;; Yannick Gingras grants you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.

(in-package #:poly-pen)

(defclass skippy-canvas (bitmap-canvas)
  ((surface :initarg :surface)
   (color-map :initarg :color-map)
   (color-hash :initarg :color-hash)))


;;; <Xach> (make-instance 'gif :bpp 8 #(0 0 0  0 255 0  0 0 0) :height 3 :width 3 :color-table #(0 0 0  1 1 1  ... 255 255 255))
;;; <Xach> except the color-table *must* be an (unsigned-byte 8) array

;; TODO: handle something else than 8 bit pallette

(defun hash-color-map (color-map)
  (let ((hash (make-hash-table :test #'equal)))
    (dotimes (x (length color-map))
      (with-slots (r g b) (rgb-24 (elt color-map x))
	(setf (gethash (list r g b) hash) x)))
    hash))

(defun color-map->array (color-map)
  (let ((array (make-array (* 3 256) :element-type '(unsigned-byte 8))))
    (dotimes (x (length color-map))
      (with-slots (r g b) (rgb-24 (elt color-map x))
	(setf (elt array (* x 3)) r)
	(setf (elt array (+ 1 (* x 3))) g)
	(setf (elt array (+ 2 (* x 3))) b)))
    array))

(defmethod dispatch-canvas ((width number)
			    (height number)
			    (backend (eql :skippy))
			    &key (color-map (make-color-spiral 255 .9))
			    &allow-other-keys)
  (make-instance 'skippy-canvas
		 :surface (make-array (* width height)
				      :element-type '(unsigned-byte 8))
		 :color-map color-map
		 :color-hash (hash-color-map color-map)
		 :width width
		 :height height))

(defmethod free ((canvas skippy-canvas))
  'done)

(defmethod dispatch-remap ((point point) (canvas skippy-canvas)
			   &key &allow-other-keys)
  "Map the point in the canvas native coordinate system."
  point)

(defmethod dispatch-save ((path pathname) (canvas skippy-canvas)
			  &key &allow-other-keys)
  (with-slots (surface color-map width height) canvas
    (let ((gif (make-instance
		'skippy:gif :bpp 8 :width width :height height
		:image-data surface
		:color-table (color-map->array color-map))))
      (skippy:write-gif gif path))))

(defmethod dispatch-save ((stream stream) (canvas skippy-canvas)
			  &key &allow-other-keys)
  (let* ((file (tempfile "X.gif"))
	 (path (pathname file)))
      (save path :canvas canvas)
      (let ((buf (make-array 1024))
	    (pos nil))
	(loop while (not (eql pos 0)) do
	     (setf pos (read-sequence buf file))
	     (write-sequence buf stream :end pos)))
      (close file)
      (delete-file file)))

(defmethod dispatch-draw ((point point) (col color) (canvas skippy-canvas)
			  &key &allow-other-keys)
  (with-slots (surface color-hash width height) canvas
    (with-slots (r g b) (rgb-24 col)
      (let ((point (remap point :canvas canvas))
	    (col-idx (gethash (list r g b) color-hash))
	    (x (x point))
	    (y (y point)))
	(declare (type fixnum x)
		 (type fixnum y))
	(setf (elt surface (+ (* y width) x)) col-idx))))
  canvas)

;; arch-tag: ee2a0ce2-ff3a-4270-b773-84a6d5c82fef
