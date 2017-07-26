;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: SURF; Base: 10 -*-
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


;;; SYS Source file: histology-xfrms.lisp

(IN-PACKAGE "SURF")


;; Kill this limiting for now....

(defconstant *histology-pixels-min-limit* -2000)
(defconstant *histology-pixels-max-limit* 2000)

(proclaim '(inline histology-pix-from-xy-vals-limit))
#|
(defun histology-pix-from-xy-vals-limit (pixel-value)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (the (signed-byte 32)
       pixel-value))
|#

;; LBG 8.19.99 Actually, keeping it seems to guarantee that the result is (INTEGER -2000 2000) given
;; the constants above, and this contraint propogates so that X-Y-HISTOLOGY-WIN compiles with 0
;; notes instead of 8. Thus VIRTUAL-SEGMENT-CORE, for example, conses half as much.

(defun histology-pix-from-xy-vals-limit (pixel-value)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (fixnum-max *histology-pixels-min-limit* (fixnum-min *histology-pixels-max-limit* pixel-value)))
	    
;;; X-Y-HISTOLOGY-WIN - Translates from data coordinates to histology window coordinates in which
;;; the origin is at the center of the window.  Separate transforms are needed for x and y since the
;;; origin of an opal:window is at the upper left hand corner, thus requiring a flipping of the y
;;; values. The scale factor is in units of microns per pixel, and the x and y args are in microns
;;; (assuming that the window :scale slot is in microns per pixel).

;;; X-Y-HISTOLOGY-WIN returns a list of (x y), in window coordinates, appropriately shifted and rotated
;;; according to the current shift and rotate slots in the window.

;; X-Y-HISTOLOGY-WIN uses the 3x3 transformation matrix (:current-xfrm) to handle rotations and shifts, but
;; scaling is done with :scale parameter. Transformation matrix is derived from Symbolics graphics
;; transformation method.


(proclaim '(inline rotated-histo-x-from-xy))
(defun rotated-histo-x-from-xy (half-win-width x-scaled y-scaled win)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type single-float x-scaled y-scaled half-win-width)
	   (type KR::SCHEMA win))
  (histology-pix-from-xy-vals-limit
   (the (signed-byte 32)
	(kernel:%unary-round (+ half-win-width
				(+ (* x-scaled (the sf (g-value win :current-xfrm-0-0)))
				   (* y-scaled (the sf (g-value win :current-xfrm-1-0)))
				   (the sf (g-value win :current-xfrm-2-0))))))))

(proclaim '(inline rotated-histo-y-from-xy))
(defun rotated-histo-y-from-xy (half-win-height x-scaled y-scaled win)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type single-float x-scaled y-scaled half-win-height)
	   (type KR::SCHEMA win))
  (histology-pix-from-xy-vals-limit
   (the (signed-byte 32)
	(kernel:%unary-round (- half-win-height
				(+ (* x-scaled (the sf (g-value win :current-xfrm-0-1)))
				   (* y-scaled (the sf (g-value win :current-xfrm-1-1)))
				   (the sf (g-value win :current-xfrm-2-1))))))))

(proclaim '(inline non-rotated-histo-x-from-x))
(defun non-rotated-histo-x-from-x (half-win-width x-scaled win)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type single-float x-scaled half-win-width)
	   (type KR::SCHEMA win))
  (histology-pix-from-xy-vals-limit
   (the (signed-byte 32)
	(kernel:%unary-round (+ half-win-width (+ x-scaled (the sf (g-value win :current-xfrm-2-0))))))))
   
(proclaim '(inline non-rotated-histo-y-from-y))
(defun non-rotated-histo-y-from-y (half-win-height y-scaled win)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type single-float y-scaled half-win-height)
	   (type KR::SCHEMA win))
  (histology-pix-from-xy-vals-limit
   (the (signed-byte 32)
	(kernel:%unary-round (- half-win-height (+ y-scaled (the sf (g-value win :current-xfrm-2-1))))))))

(proclaim '(inline x-y-histology-win-values-with-dims))
(defun x-y-histology-win-values-with-dims (x y half-win-width half-win-height win)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type single-float x y half-win-width half-win-height)
	   (type KR::SCHEMA win))
  (let ((x-scaled (/ x (the sf (g-value win :scale))))
	(y-scaled (/ y (the sf (g-value win :scale)))))
    (declare (single-float x-scaled y-scaled))
    (if (g-value win :current-xfrm-rotates)
	(values (rotated-histo-x-from-xy half-win-width x-scaled y-scaled win)
		(rotated-histo-y-from-xy half-win-height x-scaled y-scaled win))
	(values (non-rotated-histo-x-from-x half-win-width x-scaled win)
		(non-rotated-histo-y-from-y half-win-height y-scaled win)))))

;;; X-Y-HISTOLOGY-WIN-VALUES The same as X-Y-HISTOLOGY-WIN but returns X Y as VALUES. 
(proclaim '(inline x-y-histology-win-values))
(defun x-y-histology-win-values (x y win)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (values fixnum)
	   (type single-float x y))
  (x-y-histology-win-values-with-dims x y (schema-half-width win) (schema-half-height win) win))

(proclaim '(inline x-y-histology-win))
(defun x-y-histology-win (x y win)
  (multiple-value-list (x-y-histology-win-values x y win)))

(proclaim '(inline x-y-cons-histology-win))
(defun x-y-cons-histology-win (x-y win)
  (x-y-histology-win (car x-y) (cadr x-y) win))

;;***************;***************;***************;***************;***************;***************
;;***************;***************;***************;***************;***************;***************

(defun x-y-histology-win-inv-values (x-pix y-pix win)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (declare (type fixnum x-pix y-pix)
	   (type KR::SCHEMA win))
  (let ((xfrm (g-value win :current-xfrm))
	(scale (g-value win :scale)))
    (declare (type (simple-array single-float (* *)) xfrm)
	     (single-float scale))
    (let* ((half-height (schema-half-height-fn win))
	   (half-width (schema-half-width-fn win)) 
	   (x (/
	       (+
		(* (- (- half-height (aref xfrm 2 1)) y-pix)
		   (/ (aref xfrm 1 0)
		      (* (aref xfrm 1 1) (aref xfrm 0 0))))
		(/ (- (+ half-width (aref xfrm 2 0)) x-pix)
		   (aref xfrm 0 0)))
	       (+ -1 (/ (* (aref xfrm 0 1) (aref xfrm 1 0))
			(* (aref xfrm 1 1) (aref xfrm 0 0))))))
	   (y (- (/ (- (the (signed-byte 32) (- half-height y-pix)) (aref xfrm 2 1))
		    (aref xfrm 1 1))
		 (* x (/ (aref xfrm 0 1) (aref xfrm 1 1))))))
      (values (* x scale) (* y scale)))))

(defun x-y-histology-win-inv (x-pix y-pix win)
  (multiple-value-list (x-y-histology-win-inv-values x-pix y-pix win)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; COORDINATE TRANSLATION (3d to 2d)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Getting the window X pixel value.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; GET-VIEW-PLANE-X
(proclaim '(inline GET-VIEW-PLANE-X-from-x-y-z))
(defun get-view-plane-x-from-x-y-z (x y z cos-viewing-phi sin-viewing-phi)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (values single-float)
	   (ignore y)
	   (single-float x z cos-viewing-phi sin-viewing-phi))
  (+ (* x cos-viewing-phi)
     (* -1.0 z sin-viewing-phi)))


;;; With args cos-viewing-phi sin-viewing-phi, where PHI is the angle of the X'Y' viewing plane
;;; rotated counter-clockwise along the Y axis, ie when PHI = 90deg, X' = -Z.
(proclaim '(inline GET-VIEW-PLANE-X))
(defun get-view-plane-x (3D-coords cos-viewing-phi sin-viewing-phi)
  (get-view-plane-x-from-x-y-z (first 3D-coords) (second 3D-coords) (the sf (caddr 3D-coords))
			       cos-viewing-phi sin-viewing-phi))

(proclaim '(inline GET-win-VIEW-PLANE-x-from-x-y-z))
(defun get-win-view-plane-x-from-x-y-z (x y z win)
  (get-view-plane-x-from-x-y-z
   x y z (g-value win :cos-phi) (g-value win :sin-phi)))

(proclaim '(inline  GET-win-VIEW-PLANE-X))
(defun get-win-view-plane-x (3D-coords win)
  (get-view-plane-x
   3D-coords (the sf (g-value win :cos-phi)) (the sf (g-value win :sin-phi))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Getting the window Y pixel value.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; GET-VIEW-PLANE-Y
(proclaim '(inline GET-VIEW-PLANE-y-from-x-y-z))
(defun get-view-plane-y-from-x-y-z (x y z sin-viewing-phi*sin-viewing-theta
				      cos-viewing-theta
				      cos-viewing-phi*sin-viewing-theta)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (values single-float)
	   (single-float x y z sin-viewing-phi*sin-viewing-theta cos-viewing-theta cos-viewing-phi*sin-viewing-theta))
  (+ (* -1.0 x sin-viewing-phi*sin-viewing-theta)
     (* y cos-viewing-theta)
     (* -1.0 z cos-viewing-phi*sin-viewing-theta)))


(proclaim '(inline GET-VIEW-PLANE-y))
(defun get-view-plane-y (3D-coords sin-viewing-phi*sin-viewing-theta
					     cos-viewing-theta
					     cos-viewing-phi*sin-viewing-theta)
  (get-view-plane-y-from-x-y-z (first 3D-coords) (second 3D-coords) (third 3D-coords)
			       sin-viewing-phi*sin-viewing-theta cos-viewing-theta cos-viewing-phi*sin-viewing-theta))

;;; With args sin-viewing-phi*sin-viewing-theta cos-viewing-theta cos-viewing-phi*sin-viewing-theta,
;;; where PHI is the angle of the X'Y' viewing plane rotated counter-clockwise along the Y axis, ie
;;; when PHI = 90deg, X' = -Z, and THETA is the angle of the viewing plane perpendicular, with
;;; respect to the XZ plane.

(proclaim '(inline GET-win-VIEW-PLANE-y-from-x-y-z))
(defun get-win-view-plane-y-from-x-y-z (x y z win)
  (get-view-plane-y-from-x-y-z
   x y z (g-value win :sin-phi*sin-theta) (g-value win :cos-theta) (g-value win :cos-phi*sin-theta)))

(proclaim '(inline GET-win-VIEW-PLANE-y))
(defun get-win-view-plane-y (3D-coords win)
  (get-view-plane-y
   3D-coords (g-value win :sin-phi*sin-theta) (g-value win :cos-theta) (g-value win :cos-phi*sin-theta)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some combined translations (X and Y)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-win-view-plane-x-y (3D-coords win)
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (list (get-win-view-plane-x 3D-coords win)
	(get-win-view-plane-y 3D-coords win)))
  
(defun get-win-view-plane-x-y-from-x-y-z (x y z win)
  (list (get-win-view-plane-x-from-x-y-z x y z win)
	(get-win-view-plane-y-from-x-y-z x y z win)))  

(proclaim '(inline x-y-histology-win-from-view))
(defun x-y-histology-win-from-view (data-location win)
  (x-y-histology-win
   (get-win-view-plane-x data-location win) ; start-x
   (get-win-view-plane-y data-location win) ; start-y
   win))

(proclaim '(inline x-y-histology-win-from-view-values))
(defun x-y-histology-win-from-view-values (data-location win)
  (x-y-histology-win-values
   (get-win-view-plane-x data-location win) ; start-x
   (get-win-view-plane-y data-location win) ; start-y
   win))

(proclaim '(inline x-y-histology-win-from-view-values-with-dims))
(defun x-y-histology-win-from-view-values-with-dims (data-location half-win-width half-win-height win)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (single-float half-win-width half-win-height))
  (x-y-histology-win-values-with-dims
   (get-win-view-plane-x data-location win) ; start-x
   (get-win-view-plane-y data-location win) ; start-y
   half-win-width half-win-height
   win))

(proclaim '(inline x-y-histology-win-from-view-x-y-z-values))
(defun x-y-histology-win-from-view-x-y-z-values (x y z win)
  (x-y-histology-win-values
   (get-win-view-plane-x-from-x-y-z x y z win) ; start-x
   (get-win-view-plane-y-from-x-y-z x y z win) ; start-y
   win))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Old Stuff
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; NB. The WITH-GRAPHICS-ROTATION and WITH-GRAPHICS-TRANSLATION macros need to be checked.

;;; For histology applications (or other general graphics), various SURF-HIPPO graphics functions
;;; (add-circle, add-line, add-string, add-arrow) refer to an isotropic data field (e.g. microns,
;;; where the translation to pixels is according to the :scale) with the default origin at the
;;; center of the window, and the xy axises aligned with the window.  If these functions are called
;;; from within a WITH-GRAPHICS-ROTATION or WITH-GRAPHICS-TRANSLATION macro, then they refer to this
;;; field after it has been rotated/translated with respect to the default condition.

;; Various 3x3 array routines are in math.lisp


;;; This macro adds a rotation of theta radians to the theta slot in
;;; the specified window, updates the cos-theta and sin-theta slots,
;;; executes the forms, and then returns the window rotation slots to
;;; the way they were.

;;; (with-graphics-rotation (graphics-window theta)
;;;          (..form 1..)
;;;          (..form 2..)
;;;             etc.
;;; )
(defmacro with-graphics-rotation ((window delta-theta) &body forms)
  `(with-graphics-rotation-internal ,window ,delta-theta #'(lambda () . ,forms)))

(proclaim '(inline rotate-coords))
(defun rotate-coords (window theta)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (single-float theta))
  (let ((array1 (g-value window :dummy-xfrm1))
	(array2 (g-value window :dummy-xfrm2))
	(cos-theta (cos theta))
	(sin-theta (sin theta)))
    (declare (type (simple-array single-float (3 3)) array1 array2)
	     (single-float cos-theta sin-theta))
    (fill-3-by-3-identity array1)
    (setf (aref array1 0 0) cos-theta)
    (setf (aref array1 1 1) cos-theta)
    (setf (aref array1 1 0) (- sin-theta))
    (setf (aref array1 0 1) sin-theta)
    (abtoc (g-value window :current-xfrm) array1 array2)
    (copy-3-by-3-array array2 (g-value window :current-xfrm))
    (TRANSFER-CURRENT-XFRM-TO-XFRM-SLOTS window)))

(defun with-graphics-rotation-internal (window delta-theta continuation)
  (let* ((window (if (eq t window) *STANDARD-GRAPHICS-OUTPUT* (symbol-value window))))
    (rotate-coords window (coerce delta-theta 'single-float))
    (funcall continuation)
    (rotate-coords  window (coerce (- delta-theta) 'single-float))))

;;; This macro adds x and y shifts to the shift slots in the specified
;;; window, executes the forms, and then returns the window shift
;;; slots to the way they were.

;;; (with-graphics-translation (x-shift y-shift graphics-window)
;;;          (..form 1..)
;;;          (..form 2..)
;;;             etc.
;;; )
(defmacro with-graphics-translation ((window x-shift y-shift) &body forms)
  `(with-graphics-translation-internal ,window ,x-shift ,y-shift #'(lambda () . ,forms)))

(proclaim '(inline shift-coords))
(defun shift-coords (window x-shift y-shift)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type single-float x-shift y-shift))
  (let ((array (g-value window :current-xfrm))
	(scale (g-value window :scale)))
    (declare (type (simple-array single-float (3 3)) array)
	     (single-float scale))
    (setf (aref array 2 0) (+ (aref array 2 0) (/ x-shift scale)))
    (setf (aref array 2 1) (+ (aref array 2 1) (/ y-shift scale)))
    (TRANSFER-CURRENT-XFRM-TO-XFRM-SLOTS window)))

(defun with-graphics-translation-internal (window x-shift y-shift continuation)
  (let* ((window (if (eq t window) *STANDARD-GRAPHICS-OUTPUT* (symbol-value window))))
    (shift-coords window x-shift y-shift)
    (funcall continuation)
    (shift-coords window (- x-shift) (- y-shift))))

#|
(defun x-y-histology-win-inv (x-pix y-pix win)
  (let* ((xfrm (g-value win :current-xfrm))
         (scale (g-value win :scale))
         (x
          (/
           (+
            (* (-
                (- (round (/ (g-value win :height) 2))
                   (aref xfrm 2 1))
                y-pix)
               (/ (aref xfrm 1 0)
                  (* (aref xfrm 1 1) (aref xfrm 0 0))))
            (/ (- (+ (round (/ (g-value win :width) 2))
                     (aref xfrm 2 0))
                  x-pix)
               (/ (aref xfrm 0 0) scale)))
           (+ -1
              (/ (* (aref xfrm 0 1)(aref xfrm 1 0))
                 (* (aref xfrm 1 1)(aref xfrm 0 0))))))
         (y
          (-
           (/
            (-
             (*
              (- (round (/ (g-value win :height) 2))
                 y-pix)
              (g-value win :scale))
             (aref xfrm 2 1))
            (aref xfrm 1 1))
           (* x
              (/ (aref xfrm 0 1)
                 (aref xfrm 1 1))))))
    (list x y)))
|#

#|
(defun x-y-histology-win-inv (x-pix y-pix win)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (declare (type fixnum x-pix y-pix)
	   (type KR::SCHEMA win))
  (let ((xfrm (g-value win :current-xfrm))
	(scale (g-value win :scale)))
    (declare (type (simple-array single-float (* *)) xfrm)
	     (single-float scale))
    (let* ((x
	    (/
	     (+
	      (*
	       (-
		(- (round (the fn (/
				       (the fn (g-value win :height))
				       2)))
		   (the sf (aref xfrm 2 1)))
		y-pix)
	       (/ (the sf (aref xfrm 1 0))
		  (the sf (* (the sf (aref xfrm 1 1))
				       (the sf (aref xfrm 0 0))))))
	       (/ (- (+ (round (/ (g-value win :width) 2))
			(the sf (aref xfrm 2 0)))
		     x-pix)
		  (the sf (aref xfrm 0 0)))
	      )
	     (+ -1
		(/ (*  (the sf (aref xfrm 0 1))
		       (the sf (aref xfrm 1 0)))
		   (* (the sf (aref xfrm 1 1))
		      (the sf (aref xfrm 0 0)))))))
	   (y
	    (-
	     (/
	      (-
	       (- (round (/ (the fn (g-value win :height)) 2))
		  y-pix)
	       (the sf (aref xfrm 2 1)))
	      (the sf (aref xfrm 1 1)))
	     (* x
		(/  (the sf (aref xfrm 0 1))
		    (the sf (aref xfrm 1 1)))))))

      (list (the sf (* x scale))
	    (the sf (* y scale))))))

|#
