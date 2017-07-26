
;;; Copyright (C) 2004-2005 Yannick Gingras <ygingras@ygingras.net>

;;; Yannick Gingras grants you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.

(in-package #:poly-pen)

(defclass png-canvas (bitmap-canvas)
  ((surface :initarg :surface)))

(defmethod dispatch-canvas ((width number)
			    (height number)
			    (backend (eql :png))
			    &key
			    &allow-other-keys)
  (make-instance 'png-canvas
		 :surface (make-array (* width height 3)
				      :element-type '(unsigned-byte 8))
		 :width width
		 :height height))

(defmethod free ((canvas png-canvas))
  'done)

(defmethod dispatch-remap ((point point) (canvas png-canvas)
			   &key &allow-other-keys)
  "Map the point in the canvas native coordinate system."
  point)

(defmethod dispatch-save ((path pathname) (canvas png-canvas)
			  &key &allow-other-keys)
  (with-slots (surface width height) canvas
    (let ((png (make-instance
		'png:png :color-type :truecolor
		:bpp 8
		:width width :height height
		:image-data surface)))
      (png:write-png png path))))

(defmethod dispatch-save ((stream stream) (canvas png-canvas)
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

(defmethod dispatch-draw ((point point) (col color) (canvas png-canvas)
			  &key &allow-other-keys)
  (declare (optimize speed (debug 0) (safety 0)))
  (with-slots (surface width height) canvas
    (declare (type (simple-array (unsigned-byte 8) (*)) surface)
	     (type fixnum width height))
    (with-slots (r g b) (rgb-24 col)
      (declare (type (unsigned-byte 8) r g b))
      (let* ((point (remap point :canvas canvas))
	     (x (x point))
	     (y (y point))
	     (offset (* 3 (+ (* y width) x))))
	(declare (type fixnum x y offset))
	(setf (elt surface offset) r)
	  (setf (elt surface (+ offset 1)) g)
	  (setf (elt surface (+ offset 2)) b))))
  canvas)



;; arch-tag: 3ee60261-2b53-4a64-b201-4c91ee46bddd
