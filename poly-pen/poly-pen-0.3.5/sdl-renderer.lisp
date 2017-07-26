
;;; Copyright (C) 2004-2005 Yannick Gingras <ygingras@ygingras.net>

;;; Yannick Gingras grants you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.

(in-package #:poly-pen)

(defparameter *sdl-surface-flags* sdl:+swsurface+)
(defparameter *sdl-default-depth* 32)
(defvar *sdl-ttf-initialized* nil "has the font sub-system been initialized ?")

(defclass sdl-canvas (bitmap-canvas)
  ((surface :initarg :surface)))

(defmethod dispatch-canvas ((width number)
			    (height number)
			    (backend (eql :sdl))
			    &key (depth *sdl-default-depth*)
			    &allow-other-keys)
  (make-instance 'sdl-canvas
		 :surface (sdl:create-rgb-surface *sdl-surface-flags*
						  width height
						  depth 0 0 0 0)
		 :width width
		 :height height))

(defmethod free ((canvas sdl-canvas))
  (with-slots (surface) canvas
    (sdl:free-surface surface)
    (setf surface nil))  
  'done)

(defmethod dispatch-remap ((point point) (canvas sdl-canvas)
			   &key &allow-other-keys)
  "Map the point in the canvas native coordinate system."
  (2d-point (x point) (- (height canvas) 1 (y point))))

(defmethod dispatch-save ((path pathname) (canvas sdl-canvas)
			  &key &allow-other-keys)
  ;; TODO check for supported extentions
  (with-slots (surface) canvas
    (if (eql 0 (sdl:save-bmp surface (namestring path)))
	canvas
	(error "Error while saving"))))

(defmethod dispatch-save ((stream stream) (canvas sdl-canvas)
			  &key (format :bmp) &allow-other-keys)
  ;; save the file, read the content, delete file
  ;; TODO find a way to do it without touching the disk
  (let* ((file (tempfile "X.bmp"))
	 (path (pathname file)))
    (with-slots (surface) canvas
      (save path :canvas canvas)
      (when (not (eql format :bmp))
	(let ((conv-file (tempfile (format nil "X.~a" format))))
	  ;; FIXME: this is way to Unix specific
	  (run-program "/usr/bin/convert"
		       (list "convert"
			     (namestring file)
			     (namestring conv-file)))
	  (close file)
	  (delete-file file)
	  (setf file conv-file)))
      (let ((buf (make-array 1024))
	    (pos nil))
	(loop while (not (eql pos 0)) do
	     (setf pos (read-sequence buf file))
	     (write-sequence buf stream :end pos)))
      (close file)
      (delete-file file)))
  canvas)

(defmethod dispatch-image ((path pathname) (backend (eql :sdl))
			   &key &allow-other-keys)
  (let ((surface (sdl-image:load (namestring path))))
    (make-instance 'sdl-canvas
		   :surface surface
		   :width (sdl:surface-w surface)
		   :height (sdl:surface-h surface))))

(defmethod dispatch-draw ((point point) (col color) (canvas sdl-canvas)
			  &key &allow-other-keys)
  (with-slots (surface) canvas
    (with-slots (r g b) (rgb-24 col)
      (let ((point (remap point :canvas canvas))
	    (x (x point))
	    (y (y point)))
	(declare (type fixnum x)
		 (type fixnum y))
	(sdl:draw-pixel surface (x point) (y point) r g b ))))
  canvas)

(defmethod dispatch-draw ((line line) (col color) (canvas sdl-canvas)
			  &key &allow-other-keys)
  (with-slots (surface) canvas
    (with-slots (beg end) line
      (let ((beg (remap beg :canvas canvas))
	    (end (remap end :canvas canvas)))
	(with-slots (r g b) (rgb-24 col)
	  (sdl:draw-line surface (x beg) (y beg) (x end) (y end) r g b )))))
  canvas)

(defmethod dispatch-draw ((rect rect) (col color) (canvas sdl-canvas)
			  &key filled &allow-other-keys)
  ;; FIXME: check why :filled lose a few pixels
  (with-slots (surface) canvas
    (with-slots (r g b) (rgb-24 col)
      (with-slots (height width) rect
	(let ((top-left (remap (top-left rect) :canvas canvas)))
	  (funcall (if filled
		       #'sdl:draw-filled-rectangle
		       #'sdl:draw-rectangle)
		   surface
		   (x top-left) (y top-left)
		   (if filled (1+ width) width)
		   (if filled (1+ height) height)
		   r g b)))))
  canvas)

(defmethod dispatch-draw ((circle circle) (col color) (canvas sdl-canvas)
			  &key &allow-other-keys)
  (with-slots (surface) canvas
    (with-slots (r g b) (rgb-24 col)
      (let ((center (remap (center circle) :canvas canvas)))
	(sdl:draw-circle surface
			 (x center) (y center) (radius circle)
			 r g b))))
  canvas)

(defclass sdl-font (font)
  ((ttf :initarg :ttf)))

(defmethod dispatch-font ((path pathname) (size number) (backend (eql :sdl))
			  &key &allow-other-keys)
  (when (not *sdl-ttf-initialized*)
    (sdl-ttf:init)
    (setf *sdl-ttf-initialized* t))
  (let ((ttf (sdl-ttf:open-font (namestring path) size)))
    (if (alien:null-alien ttf)
	(error "Can't open font")
	(make-instance 'sdl-font :ttf ttf))))

(defmethod free ((font sdl-font))
  (with-slots (ttf) font
    (sdl-ttf:close-font ttf)
    (setf ttf nil))  
  'done)

(defmethod text-width ((string string) (font sdl-font))
  ;; TODO: handle UTF8 and Unicode
  (with-slots (ttf) font
      (uffi:with-foreign-objects ((w 'sdl:SINT32)
				  (h 'sdl:SINT32))
	(sdl-ttf:size-text ttf string w h)
	(uffi:deref-pointer w 'integer))))

(defmethod text-height ((string string) (font sdl-font))
  ;; TODO: handle UTF8 and Unicode
  (with-slots (ttf) font
    (uffi:with-foreign-objects ((w 'sdl:SINT32)
				(h 'sdl:SINT32))
      (sdl-ttf:size-text ttf string w h)
      (uffi:deref-pointer h 'integer))))

(defmethod ascent ((font sdl-font))
  (with-slots (ttf) font
    (sdl-ttf:font-ascent ttf)))

(defmethod descent ((font sdl-font))
  (with-slots (ttf) font
    (sdl-ttf:font-descent ttf)))

(defmethod dispatch-draw ((text text) (col color) (canvas sdl-canvas)
			  &key (font *default-font*)
			  (blended t) &allow-other-keys)
  (if (not font)
      (error "no font specified"))
  (with-slots (ttf) font
    (with-slots (surface last-text-pos) canvas
      (with-slots (bottom-left msg) text
	(with-slots (r g b) (rgb-24 col)
	  (uffi:with-foreign-objects ((color 'sdl:color)
				      (src 'sdl:rect)
				      (dst 'sdl:rect))
	  
	    (setf (sdl:color-r color) r)
	    (setf (sdl:color-g color) g)
	    (setf (sdl:color-b color) b)

	    (let ((text-surf (funcall (if blended
					  #'sdl-ttf:render-text-blended
					  #'sdl-ttf:render-text-solid)
				      ttf msg color))
		  (mapped-text-pos (remap bottom-left :canvas canvas)))
	      (setf (sdl:rect-x src) 0)
	      (setf (sdl:rect-y src) 0)
	      (setf (sdl:rect-w src) (sdl:surface-w text-surf))
	      (setf (sdl:rect-h src) (sdl:surface-h text-surf))

	      (setf (sdl:rect-x dst) (x mapped-text-pos))
	      (setf (sdl:rect-y dst) (- (y mapped-text-pos)
					(sdl:surface-h text-surf)
					(sdl-ttf:font-descent ttf)))
	      (setf (sdl:rect-w dst) (sdl:surface-w text-surf))
	      (setf (sdl:rect-h dst) (sdl:surface-h text-surf))
	          
	      (sdl:blit-surface text-surf src surface dst)

	      (setf last-text-pos (add bottom-left
				       (2d-vect (sdl:rect-w src) 0)))
	      (sdl:free-surface text-surf)))))))
  canvas)

;; arch-tag: eda9e2ad-5a56-4279-a6ff-8bdcea859865
