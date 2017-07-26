
;;; Copyright (C) 2004-2005 Yannick Gingras <ygingras@ygingras.net>

;;; Yannick Gingras grants you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.

(in-package #:poly-pen)

(defvar *gd-default-true-color* t)

(defclass gd-canvas (bitmap-canvas)
  ((surface :initarg :surface)))

(defmethod dispatch-canvas ((width number)
			    (height number)
			    (backend (eql :gd))
			    &key (true-color *gd-default-true-color*)
			    &allow-other-keys)
  (make-instance 'gd-canvas
		 :surface (cl-gd:create-image width height true-color)
		 :width width
		 :height height))

(defmethod free ((canvas gd-canvas))
  (with-slots (surface) canvas
    (cl-gd:destroy-image surface)
    (setf surface nil))  
  'done)

(defmethod dispatch-remap ((point point) (canvas gd-canvas)
			   &key &allow-other-keys)
  "Map the point in the canvas native coordinate system."
  (2d-point (x point) (- (height canvas) 1 (y point))))

(defmethod dispatch-save ((path pathname) (canvas gd-canvas)
			  &key &allow-other-keys)
  ;; TODO check for supported extentions
  (with-slots (surface) canvas
    (cl-gd:write-image-to-file path :if-exists :supersede :image surface))
  canvas)

(defmethod dispatch-save ((stream stream) (canvas gd-canvas)
			  &key (format :png) &allow-other-keys)
  ;; TODO check for supported formats
  (with-slots (surface) canvas
    (cl-gd:write-image-to-stream stream format :image surface))
  canvas)

(defmethod dispatch-image ((path pathname) (backend (eql :gd))
			   &key &allow-other-keys)
  (let ((surface (cl-gd:create-image-from-file path)))
    (make-instance 'gd-canvas
		   :surface surface
		   :width (cl-gd:image-width surface )
		   :height (cl-gd:image-height surface))))

(defmethod dispatch-draw ((point point) (col color) (canvas gd-canvas)
			  &key &allow-other-keys)
  (with-slots (surface) canvas
    (with-slots (r g b) (rgb-24 col)
      (let ((point (remap point :canvas canvas)))
	(cl-gd:with-default-image (surface)
	  (cl-gd:set-pixel (x point) (y point)
			   :color (cl-gd:find-color r g b))))))
  canvas)

(defmethod dispatch-draw ((line line) (col color) (canvas gd-canvas)
			  &key &allow-other-keys)
  (with-slots (surface) canvas
    (with-slots (beg end) line
      (let ((beg (remap beg :canvas canvas))
	    (end (remap end :canvas canvas)))
	(with-slots (r g b) (rgb-24 col)
	  (cl-gd:with-default-image (surface)
	    (cl-gd:draw-line (x beg) (y beg) (x end) (y end)
			     :color (cl-gd:find-color r g b)))))))
  canvas)

(defmethod dispatch-draw ((rect rect) (col color) (canvas gd-canvas)
			  &key filled &allow-other-keys)
  (with-slots (surface) canvas
    (with-slots (r g b) (rgb-24 col)
      (with-slots (height width) rect
	(let ((top-left (remap (top-left rect) :canvas canvas))
	      (bottom-right (remap (bottom-right rect) :canvas canvas)))
	  (cl-gd:with-default-image (surface)
	    (cl-gd:draw-rectangle* (x top-left) (y top-left)
				   (x bottom-right) (y bottom-right)
				   :filled filled
				   :color (cl-gd:find-color r g b)))))))
  canvas)

(defmethod dispatch-draw ((circle circle) (col color) (canvas gd-canvas)
			  &key &allow-other-keys)
  (with-slots (surface) canvas
    (with-slots (r g b) (rgb-24 col)
      (let ((center (remap (center circle) :canvas canvas))
	    (diam (* 2 (radius circle))))
	(cl-gd:with-default-image (surface)  
	  (cl-gd:draw-arc (x center) (y center)
			  diam diam 0 360
			  :center-connect nil
			  :color (cl-gd:find-color r g b))))))
  canvas)

(defclass gd-font (font)
  ((file :initarg :file)
   (size :initarg :size))  
  (:documentation "GD hides the font caching in it's internals.  We
  can't pre-load fonts but we don't have to either.  The major draw
  back is that we don't know if the font is available until we try to
  use it."))

(defmethod dispatch-font ((path pathname) (size number) (backend (eql :gd))
			  &key &allow-other-keys)
  ;; normalize the point size to a 75 DPI resolution
  (make-instance 'gd-font :file path :size (* .75 size)))

(defmethod text-width ((string string) (font gd-font))
  ;; TODO: handle UTF8 and Unicode
  ;; GD has to make a test render on a surface... dont ask
  (let* ((surface (cl-gd:create-image 10 10))
	 (box (cl-gd:draw-freetype-string
	       0 0 string
	       :color 0
	       :point-size (slot-value font 'size)
	       :font-name (slot-value font 'file)
	       :image surface
	       :do-not-draw t))
	 (width (- (elt box 2) (elt box 0))))
    (cl-gd:destroy-image surface)
    width))

(defmethod text-height ((string string) (font gd-font))
  ;; TODO: handle UTF8 and Unicode
  (let* ((surface (cl-gd:create-image 10 10))
	 (box (cl-gd:draw-freetype-string
	       0 0 string
	       :color 0
	       :point-size (slot-value font 'size)
	       :font-name (slot-value font 'file)
	       :image surface
	       :do-not-draw t))
	 (height (- (elt box 1) (elt box 5))))
    (cl-gd:destroy-image surface)
    height))

(defmethod dispatch-draw ((text text) (col color) (canvas gd-canvas)
			  &key (font *default-font*)
			  (blended t) &allow-other-keys)
  (if (not font)
      (error "no font specified"))
  (with-slots (surface last-text-pos) canvas
    (with-slots (bottom-left msg) text
      (with-slots (r g b) (rgb-24 col)
	(let ((mapped-text-pos (remap bottom-left :canvas canvas)))
	  (cl-gd:with-default-image (surface)  
	    (let ((box (cl-gd:draw-freetype-string
			(x mapped-text-pos) 
			(y mapped-text-pos)
			msg
			:anti-aliased blended
			:point-size (slot-value font 'size)
			:font-name (slot-value font 'file)
			:color (cl-gd:find-color r g b))))
	      (setf last-text-pos (2d-point (elt box 2)
					    (y bottom-left)))))))))
  canvas)

;; arch-tag: a0827bf3-c44f-46de-92bc-33e35b9956ea
