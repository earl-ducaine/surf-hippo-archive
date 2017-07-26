;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-
;;; Lyle Borg-Graham, Equipe Cogniscience, Institut Alfred Fessard, CNRS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              The Surf-Hippo Neuron Simulator                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Surf-Hippo project at the  ;;;
;;; Center for Biological Information Processing, Department of     ;;;
;;; Brain and Cognitive Sciences, Massachusetts Institute of        ;;;
;;; Technology, and currently at the Equipe Cogniscience, Institut  ;;;
;;; Alfred Fessard, CNRS and has been placed in the public domain.  ;;;
;;; If you are using this code or any part of Surf-Hippo, please    ;;;
;;; contact surf-hippo@ai.mit.edu to be put on the mailing list.    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This file is modified from Garnet source code.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package "OPAL")

;;  Assumes one dimensional gob :array-size.
(defun draw-virtual-circle (line-style-gc filling-style-gc drawable root-window
					  left top diameter ; width height
					  lstyle fstyle x-draw-fn)
;  (declare (optimize (speed 3) (safety 0)))
  (let* ((xlib-gc-line (opal-gc-gcontext line-style-gc))
	 (xlib-gc-fill (opal-gc-gcontext filling-style-gc))
	 (thickness 1)
	 (fill-diameter (the fixnum (- diameter 2))))
    (declare (fixnum left top thickness diameter fill-diameter))
    (when (plusp diameter)		;don't draw anything unless diameter > 0
      (if (not (plusp fill-diameter))	; if circle is too small, just draw black circle
	  (xlib:with-gcontext (xlib-gc-line :fill-style :solid :function x-draw-fn)
	    (xlib:draw-arc drawable xlib-gc-line left top diameter diameter 0.0 *twopi* t))
	  (let ((half-thickness 0)	; (truncate thickness 2)
		(d-mod-2 (mod diameter 2))
		(t-mod-2 (mod thickness 2)))
	    (declare (fixnum half-thickness d-mod-2 t-mod-2))
	    (when fstyle
	      (gem::set-filling-style fstyle filling-style-gc xlib-gc-fill root-window x-draw-fn)
	      (xlib:draw-arc drawable xlib-gc-fill
			     (+ left thickness) (+ top thickness) fill-diameter fill-diameter 0.0 *twopi* t))
	    (when lstyle
	      (gem::set-line-style lstyle line-style-gc xlib-gc-line root-window x-draw-fn)
	      (xlib:draw-arc drawable
			     xlib-gc-line
			     (+ left
				(+ half-thickness
				   (aref (the (simple-array t (2 2 2)) *left-adjustment*) d-mod-2 d-mod-2 t-mod-2)))
			     (+ top
				(+ half-thickness
				   (aref (the (simple-array t (2 2 2)) *top-adjustment*) d-mod-2 d-mod-2 t-mod-2)))
			     (- diameter
				(+ thickness
				   (aref (the (simple-array t (2 2 2)) *width-adjustment*) d-mod-2 d-mod-2 t-mod-2)))
			     (- diameter
				(+ thickness
				   (aref (the (simple-array t (2 2 2)) *height-adjustment*) d-mod-2 d-mod-2 t-mod-2)))
			     0.0 *twopi*)))))))

(defun update-virtual-circle (gob update-info bbox-1 bbox-2  &optional (total-p NIL))
					;  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let* ((dummy (g-value gob :dummy-item))
	 (draw-function (G-VALUE DUMMY :DRAW-FUNCTION))
	 item-bbox
	 (invalid-object (g-value gob :invalid-object))
	 (dirty-p (update-info-dirty-p update-info))
	 (agg-bbox (update-info-old-bbox update-info))
	 (bbox-array (g-value gob :bbox-array))
	 (item-array (g-value gob :item-array))
	 (a-window (g-value gob :window))
	 (colorize (g-value a-window :colorize))
	 (display-info (g-value a-window :display-info))
	 (root-window  (opal::display-info-root-window display-info))
	 (drawable (gem::the-drawable a-window))
	 (drawable-display (xlib::drawable-display drawable))
	 (function (get draw-function :x-draw-function))
	 (line-style-gc (opal::display-info-line-style-gc display-info))
	 (xlib-gc-line (opal::opal-gc-gcontext line-style-gc)))
    (s-value invalid-object :already-on-invalid-objects-list nil)
    (when (or dirty-p
	      total-p
	      (and (bbox-valid-p agg-bbox)
		   (bbox-intersects-either-p agg-bbox bbox-1 bbox-2)))
      (when (and (null bbox-1) (null bbox-2) ; (listp clip-mask)
		 (bbox-valid-p agg-bbox))
	(setq bbox-1 agg-bbox)
	;; (bbox-to-clip-mask agg-bbox clip-mask)
	(bbox-to-clip-mask-components
	 agg-bbox (g-value gob :left) (g-value gob :top) (g-value gob :width) (g-value gob :height)))
      (let ((default-graphics-color (g-value a-window :default-graphics-color))
	    (default-graphics-filling-style (g-value a-window :default-graphics-filling-style))
;;;	    (segment-color-shading (g-value a-window :segment-color-shading))
	    (default-line-style (g-value a-window :default-line-style))
	    (virtual-circle-linestyle
	     (or (g-value gob :line-style) (g-value a-window :default-line-style) ; (g-value opal::circle :line-style)
	      ))
	    (filling-style-gc (opal::display-info-line-style-gc display-info)))


	;(format t "Step 1 .... ~%") (break)
	
	(dotimes (n (the fixnum (g-value gob :next-available-rank)))
	  (declare (fixnum n))
	  (setq item-bbox (aref bbox-array n))
	  (when (and (bbox-valid-p item-bbox)
		     (or (and bbox-1 (bbox-intersect-p bbox-1 item-bbox))
			 (and bbox-2 (bbox-intersect-p bbox-2 item-bbox))))
	    (let* ((item-values (aref (the simple-array item-array) n))
		   (soma (fifth item-values))
;;;                  (seg (aref item-values 6))
;;;                  (thickness (the fixnum (aref item-values 5)))
;;;                  (lstyle
;;;                    (if colorize
;;;                        (sh::access-*line-styles-array*-for-segment-voltage thickness seg)
;;;                        (or (sh::access-*line-styles-array*-for-segments-fast
;;;                             thickness default-graphics-color segment-color-shading
;;;                             ;; Can't use segment-index macro here since it is  not defined yet
;;;                             (sh::node-color-index (sh::segment-node-2 seg))))))
		   )	

		(draw-virtual-circle
		 line-style-gc filling-style-gc drawable root-window
		 ;; item-values -> (xcenter ycenter radius fill soma diameter left top)
		 (the fixnum (seventh item-values)) ; left
		 (the fixnum (eighth item-values)) ; top
		 (the fixnum (sixth item-values)) ;   diameter 
		 virtual-circle-linestyle ; lstyle
		 (if (and (sh::soma-p soma) colorize)
		     (surf-hippo::access-*fill-styles*-for-soma-voltage soma)
		     (or (fourth item-values) default-graphics-filling-style)) ; fstyle
		 function)
		)))))
	      
	    
    (setf (bbox-valid-p (update-info-old-bbox (the UPDATE-INFO (g-value invalid-object :update-info)))) nil)
    (if dirty-p (setf (update-info-dirty-p update-info) NIL))))
