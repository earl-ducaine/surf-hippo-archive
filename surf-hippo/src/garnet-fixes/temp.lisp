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
(defun update-virtual-line (gob update-info bbox-1 bbox-2  &optional (total-p NIL))
  (declare (optimize (safety 0) (speed 3) (space 1)))
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
	 (xlib-gc-line (opal::opal-gc-gcontext line-style-gc))
	 )
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
	    (segment-color-shading (g-value a-window :segment-color-shading))
	    (default-line-style (g-value a-window :default-line-style)))
		
	(dotimes (n (the fixnum (g-value gob :next-available-rank)))
	  (declare (fixnum n))
	  (setq item-bbox (aref bbox-array n))
	  (when (and (bbox-valid-p item-bbox)
		     (or (and bbox-1 (bbox-intersect-p bbox-1 item-bbox))
			 (and bbox-2 (bbox-intersect-p bbox-2 item-bbox))))
	    (let* ((seg (aref item-values 6))
		   (thickness (the fixnum (aref item-values 5)))
		   (item-values (the simple-array (aref (the simple-array item-array) n)))
		   (lstyle
		    (if colorize
			(sh::access-*line-styles*-for-segment-voltage thickess seg)
			(or (sh::access-*line-styles*-for-segments-fast
			     thickness default-graphics-color segment-color-shading)
			    (segment-color-index seg)))))
	      (let ((x1 (the fixnum (aref item-values 0)))
		    (y1 (the fixnum (aref item-values 1)))
		    (x2 (the fixnum (aref item-values 2)))
		    (y2 (the fixnum (aref item-values 3))))
		(gem::x-draw-line-fast a-window x1 y1 x2 y2 function lstyle
				       display-info root-window line-style-gc xlib-gc-line
				       drawable drawable-display)))))))
	      
	    
    (setf (bbox-valid-p (update-info-old-bbox (the UPDATE-INFO (g-value invalid-object :update-info)))) nil)
    (if dirty-p (setf (update-info-dirty-p update-info) NIL))))