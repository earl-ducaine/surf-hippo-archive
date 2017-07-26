;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; SYS Source file: virtual-aggregate-update-method.lisp
;; improvements to some macros and functions

(in-package "OPAL")

(deftype fn () 'fixnum)

(defmacro do-in-clip-rect ((m n agg rect) &body body)
  `(let* ((agg* ,agg)
	  (p-to-r (the compiled-function (g-value agg* :point-to-rank)))
	  (r* ,rect)
	  (array-size* (g-value agg* :array-length))
	  (max-x2* (1- (the fn (first array-size*))))
	  (max-y2* (1- (the fn (second array-size*))))
	  (first* (first r*))
	  (second* (second r*)))
    (declare (fixnum first* second* max-x2* max-y2*)) 
     (multiple-value-bind (x1* y1*)
       		          (funcall p-to-r agg* first* second*)
       (multiple-value-bind (x2* y2*)
			    (funcall p-to-r agg* (the fn (+ first* (the fn (third r*)) -1))
				     (the fn (+ second* (the fn (fourth r*)) -1)))
	 (declare (fixnum x1* x2* y1* y2*)) 
	 (setq x1* (the fn (if x1* (max 0 x1*) 0)))
	 (setq y1* (the fn (if y1* (max 0 y1*) 0)))
	 (setq x2* (the fn (if x2* (min x2* max-x2*) max-x2*)))
	 (setq y2* (the fn (if y2* (min y2* max-y2*) max-y2*)))
	 (when (and (<= x1* x2*) (<= y1* y2*))
	   (do ((,m x1* (1+ (the fn ,m))))
	       ((> (the fn ,m) (the fn x2*)))
	     (do ((,n y1* (1+ (the fn ,n))))
	         ((> (the fn ,n) (the fn y2*)))
	       ,@body)))))))

(proclaim '(inline set-colorize-line-style))
(defun set-colorize-line-style (line-style opal-gc xlib-gc root-window x-draw-fn)
  (declare (optimize (speed 3) (safety 0))
	   (ignore root-window))
  (unless (eq line-style (opal-gc-opal-style opal-gc))
    (set-gc opal-gc xlib-gc :foreground (HP-XOR-hack x-draw-fn (g-value line-style :foreground-color :colormap-index)))
    (set-gc opal-gc xlib-gc :background (HP-XOR-hack x-draw-fn (g-value line-style :background-color :colormap-index)))
    (setf (opal-gc-opal-style opal-gc) line-style)
    (set-gc opal-gc xlib-gc :line-width (g-value line-style :line-thickness))
    (set-gc opal-gc xlib-gc :line-style (g-value line-style :line-style))))


(defun draw-virtual-circle (line-style-gc filling-style-gc drawable root-window
					  left top diameter ; width height
					  lstyle fstyle
					  x-draw-fn)
  (declare (optimize (speed 3) (safety 0)))
  (let* ( ; (update-vals (g-local-value gob :update-slots-values))
	  (xlib-gc-line (opal-gc-gcontext line-style-gc))
	  (xlib-gc-fill (opal-gc-gcontext filling-style-gc))
	  (thickness 1 ; (get-old-thickness gob *circle-lstyle* update-vals)
	    )
	  (fill-diameter (the fn (- diameter 2 ; (* 2 thickness)
			    ))))
    (declare (fixnum left top thickness diameter fill-diameter))
    (when t ; (plusp diameter)		;don't draw anything unless diameter > 0
      (if (not (plusp fill-diameter))	; if circle is too small,
					; just draw black circle
	  (xlib:with-gcontext (xlib-gc-line
			       :fill-style :solid
			       :function x-draw-fn)
	    (xlib:draw-arc drawable xlib-gc-line
			   left top diameter diameter 0.0 *twopi* t))
	  (let ((half-thickness 0 ; (truncate thickness 2)
		  )
		(d-mod-2 (mod diameter 2))
		(t-mod-2 (mod thickness 2)))
	    (declare (fixnum half-thickness d-mod-2 t-mod-2))
	    (when fstyle
	      (set-filling-style fstyle filling-style-gc
				 xlib-gc-fill root-window x-draw-fn)
	      (xlib:draw-arc drawable
			     xlib-gc-fill
			     (+ left thickness)
			     (+ top thickness)
			     fill-diameter fill-diameter
			     0.0 *twopi* t))
	    (when lstyle
	      (set-line-style lstyle line-style-gc
			      xlib-gc-line root-window x-draw-fn)
	      (xlib:draw-arc drawable
			     xlib-gc-line
			     (the fn (+ left
					    (the fn
						 (+ half-thickness
						    (aref (the (simple-array fixnum (* * *)) *left-adjustment*)
							       d-mod-2 d-mod-2 t-mod-2)))))
			     (the fn (+ top
					    (the fn (+ half-thickness
							   (aref (the (simple-array fixnum (* * *)) *top-adjustment*)
								 d-mod-2 d-mod-2 t-mod-2)))))
			     (the fn (- diameter
					    (the fn (+ thickness
							   (aref (the (simple-array fixnum (* * *)) *width-adjustment*)
								 d-mod-2 d-mod-2 t-mod-2)))))
			     (the fn (- diameter
					    (the fn (+ thickness
							   (aref (the (simple-array fixnum (* * *)) *height-adjustment*)
								 d-mod-2 d-mod-2 t-mod-2)))))
			     0.0 *twopi*)))))))


;;(proclaim '(notinline draw-virtual-segment))
#|
(defun draw-virtual-segment (line-style-gc drawable root-window x1 x2 y1 y2
					   line-style-temp xlib-gc-line x-draw-fn
					   colorizing display)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (ignore line-style-gc drawable root-window x1 x2 y1 y2
		   line-style-temp xlib-gc-line x-draw-fn
		   colorizing display)))
|#

(proclaim '(inline draw-virtual-segment))
(defun draw-virtual-segment (line-style-gc drawable root-window x1 x2 y1 y2
					   line-style-temp xlib-gc-line x-draw-fn
					   colorizing display)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (type schema line-style-temp)
	   (fixnum x1 x2 y1 y2))
  (if colorizing
      (progn
	(set-colorize-line-style line-style-temp line-style-gc xlib-gc-line root-window x-draw-fn)
        (xlib::draw-colorized-segment drawable xlib-gc-line x1 y1 x2 y2 display))
      (progn
	(opal::set-line-style line-style-temp line-style-gc xlib-gc-line root-window x-draw-fn)
	(xlib:draw-line drawable xlib-gc-line x1 y1 x2 y2))))





#|
(define-method
    :update opal:virtual-aggregate (gob update-info line-style-gc filling-style-gc drawable root-window
					bbox-1 bbox-2 &optional (total-p NIL))
    (declare (optimize (speed 3) (safety 0)))
    (let* ((display (xlib::drawable-display drawable))
	   (dummy (g-value gob :dummy-item))
	   (is-a-virtual-segment (eq (car (g-value dummy :is-a)) surf-hippo::virtual-segment))
	   (is-a-virtual-circle (eq (car (g-value dummy :is-a)) surf-hippo::virtual-circle))
	   (is-a-virtual-plotting-circle (eq (car (g-value dummy :is-a)) ph::virtual-plotting-circle))
	   (colorizing-update (or (g-value dummy :parent :window :colorize)
				  (g-value dummy :parent :window :lock-color)))
	   (colorize-virtual-thing (and colorizing-update (g-value dummy :parent :colorizeable)))
	   (dummy-slots-list (unless (or is-a-virtual-circle is-a-virtual-plotting-circle is-a-virtual-segment)
			       (g-value dummy :update-slots)))
	   (dummy-update-slots-values (unless (or is-a-virtual-circle is-a-virtual-plotting-circle is-a-virtual-segment)
					(the (simple-array * (*)) (g-local-value dummy :update-slots-values))))
	   (dummy-vals-indx 0)
	   (xlib-gc-line (opal::opal-gc-gcontext line-style-gc))
	   (virtual-thing-draw-fn (get :copy :x-draw-function))
	   (virtual-circle-linestyle (g-value opal::circle :line-style))
	   item-bbox
	   (invalid-object (g-value gob :invalid-object))
	   (dirty-p (update-info-dirty-p update-info))
	   (agg-bbox (update-info-old-bbox update-info))
	   (array-size (g-value gob :array-length))
	   (bbox-array (the (simple-array * (*)) (g-value gob :bbox-array)))
	   (item-array (g-value gob :item-array))
	 ;;; *** Temporary:
	   (clip-mask
	    (or (g-value gob :clip-mask) (s-value gob :clip-mask (list (g-value gob :left)
								       (g-value gob :top)
								       (g-value gob :width)
								       (g-value gob :height))))))
      (declare (fixnum dummy-vals-indx))
      (when (or (not colorizing-update) colorize-virtual-thing (g-value dummy :parent :window :lock-color))
	(s-value invalid-object :already-on-invalid-objects-list nil)
	(when (or dirty-p
		  total-p
		  (and (bbox-valid-p agg-bbox) (bbox-intersects-either-p agg-bbox bbox-1 bbox-2)))
	  (when (and (null bbox-1) (null bbox-2) (listp clip-mask) (bbox-valid-p agg-bbox))
	    (setq bbox-1 agg-bbox)
	    (bbox-to-clip-mask agg-bbox clip-mask))
	  (if (numberp array-size);; one dimensional
	      (let ((item-array (the (simple-array cons (*)) item-array)))
		(do ((n 0 (the fn (1+ (the fn n)))))
		    ((= (the fn n) (the fn (g-value gob :next-available-rank))))
		  (when (or is-a-virtual-circle is-a-virtual-plotting-circle is-a-virtual-segment
			    (and (bbox-valid-p (the opal::bbox (aref bbox-array n)))
				 (or (and bbox-1 (bbox-intersect-p bbox-1 (the opal::bbox (aref bbox-array n))))
				     (and bbox-2 (bbox-intersect-p bbox-2 (the opal::bbox (aref bbox-array n)))))))
		    (unless (or is-a-virtual-circle is-a-virtual-plotting-circle is-a-virtual-segment)
		      (s-value dummy :rank n)
		      (s-value dummy :item-values (aref item-array n)))
		    (cond
		      (is-a-virtual-circle
		       (draw-virtual-circle
			line-style-gc filling-style-gc drawable root-window
			;; item-values -> (xcenter ycenter radius fill soma diameter left top)
			(the fn (seventh (aref item-array n))) ; left
			(the fn (eighth (aref item-array n))) ; top
			(the fn (sixth (aref item-array n))) ;   diameter 
			virtual-circle-linestyle ; lstyle
			(if colorize-virtual-thing
			    (surf-hippo::access-*fill-styles*-for-soma-voltage
			     (fifth (aref item-array n)))
			    (fourth (aref item-array n))) ; fstyle
			virtual-thing-draw-fn))

		      (is-a-virtual-plotting-circle
		       (draw-virtual-circle
			line-style-gc filling-style-gc drawable root-window
			;; item-values -> (left top diameter fill)
			(the fn (first (aref item-array n))) ; left
			(the fn (second (aref item-array n))) ; top
			(the fn (third (aref item-array n))) ;   diameter 
			virtual-circle-linestyle ; lstyle
			(fourth (aref item-array n)) ; fstyle
			virtual-thing-draw-fn))
		    
		      (is-a-virtual-segment
		       (draw-virtual-segment
			line-style-gc drawable root-window
			(the fn (first (aref item-array n)))
			(the fn (third (aref item-array n)))
			(the fn (second (aref item-array n)))
			(the fn (fourth (aref item-array n)))
			(if colorize-virtual-thing
                            (surf-hippo::access-*line-styles-array*-for-segment-voltage
                             (sixth (aref item-array n))
                             (seventh (aref item-array n)))
			    (fifth (aref item-array n)))
			xlib-gc-line virtual-thing-draw-fn colorize-virtual-thing
			display))
		      
		      (t (setq dummy-vals-indx -1)
			 (dolist (slot dummy-slots-list)
			   (incf dummy-vals-indx)
			   (setf (aref dummy-update-slots-values dummy-vals-indx) (g-value dummy slot)))
			 (draw dummy line-style-gc filling-style-gc drawable root-window)))))
		(when colorize-virtual-thing (xlib::display-invoke-after-function display)))
	      (let ((item-array (the (simple-array cons (* *)) item-array)))
		;; two dimensional
		(setq dummy-slots-list (cddr dummy-slots-list))
		(if (fifth clip-mask) (setq clip-mask (cddddr clip-mask)))
		(do-in-clip-rect (m n gob clip-mask)
		  (s-value dummy :rank1 m)
		  (s-value dummy :rank2 n)
		  (s-value dummy :item-values (aref item-array m n))
            ;;; faster than (opal::update-slots-values-changed dummy 2 update-info)
		  (setq dummy-vals-indx 1)
		  (dolist (slot dummy-slots-list)
		    (incf dummy-vals-indx)
		    (setf (aref dummy-update-slots-values dummy-vals-indx)
			  (g-value dummy slot)))
		  (draw dummy line-style-gc filling-style-gc drawable
			root-window)))))
        (setf (bbox-valid-p (update-info-old-bbox (the UPDATE-INFO (g-value invalid-object :update-info)))) nil)
	(if dirty-p (setf (update-info-dirty-p update-info) NIL)))))
|#

(define-method
    :update opal:virtual-aggregate (gob update-info line-style-gc filling-style-gc drawable root-window
					bbox-1 bbox-2 &optional (total-p NIL))
    (declare (optimize (speed 3) (safety 0)))
    (let* ((display (xlib::drawable-display drawable))
	   (dummy (g-value gob :dummy-item))
	   (is-a-virtual-segment (eq (car (g-value dummy :is-a)) surf-hippo::virtual-segment))
	   (is-a-virtual-circle (eq (car (g-value dummy :is-a)) surf-hippo::virtual-circle))
	   (is-a-virtual-dot (eq (car (g-value dummy :is-a)) ph::virtual-dot))
	   (is-a-virtual-plotting-circle (eq (car (g-value dummy :is-a)) ph::virtual-plotting-circle))
	   (colorizing-update (or (g-value dummy :parent :window :colorize)
				  (g-value dummy :parent :window :lock-color)))
	   (colorize-virtual-thing (and colorizing-update (g-value dummy :parent :colorizeable)))
	   (dummy-slots-list (unless (or is-a-virtual-circle is-a-virtual-plotting-circle is-a-virtual-segment
					 is-a-virtual-dot)
			       (g-value dummy :update-slots)))
	   (dummy-update-slots-values (unless (or is-a-virtual-circle is-a-virtual-plotting-circle is-a-virtual-segment
						  is-a-virtual-dot)
					(the (simple-array * (*)) (g-local-value dummy :update-slots-values))))
	   (dummy-vals-indx 0)
	   (xlib-gc-line (opal::opal-gc-gcontext line-style-gc))
	   (virtual-thing-draw-fn (get :copy :x-draw-function))
	   (virtual-circle-linestyle (g-value opal::circle :line-style))
	   item-bbox
	   (invalid-object (g-value gob :invalid-object))
	   (dirty-p (update-info-dirty-p update-info))
	   (agg-bbox (update-info-old-bbox update-info))
	   (array-size (g-value gob :array-length))
	   (bbox-array (the (simple-array * (*)) (g-value gob :bbox-array)))
	   (item-array (g-value gob :item-array))
	 ;;; *** Temporary:
	   (clip-mask
	    (or (g-value gob :clip-mask) (s-value gob :clip-mask (list (g-value gob :left)
								       (g-value gob :top)
								       (g-value gob :width)
								       (g-value gob :height))))))
      (declare (fixnum dummy-vals-indx))
      (when (or (not colorizing-update) colorize-virtual-thing (g-value dummy :parent :window :lock-color))
	(s-value invalid-object :already-on-invalid-objects-list nil)
	(when (or dirty-p
		  total-p
		  (and (bbox-valid-p agg-bbox) (bbox-intersects-either-p agg-bbox bbox-1 bbox-2)))
	  (when (and (null bbox-1) (null bbox-2) (listp clip-mask) (bbox-valid-p agg-bbox))
	    (setq bbox-1 agg-bbox)
	    (bbox-to-clip-mask agg-bbox clip-mask))
	  (if (numberp array-size);; one dimensional
	      (let ((item-array (the (simple-array cons (*)) item-array)))
		(do ((n 0 (the fn (1+ (the fn n)))))
		    ((= (the fn n) (the fn (g-value gob :next-available-rank))))
		  (when (or is-a-virtual-circle is-a-virtual-plotting-circle is-a-virtual-segment is-a-virtual-dot
			    (and (bbox-valid-p (the opal::bbox (aref bbox-array n)))
				 (or (and bbox-1 (bbox-intersect-p bbox-1 (the opal::bbox (aref bbox-array n))))
				     (and bbox-2 (bbox-intersect-p bbox-2 (the opal::bbox (aref bbox-array n)))))))
		    (unless (or is-a-virtual-circle is-a-virtual-plotting-circle
				is-a-virtual-segment is-a-virtual-dot)
		      (s-value dummy :rank n)
		      (s-value dummy :item-values (aref item-array n)))
		    (let ((param-list (aref item-array n)))
		      (cond
			(is-a-virtual-dot
			 (let ((left (the fn (third param-list))))
			   (draw-virtual-circle
			    line-style-gc filling-style-gc drawable root-window
			    ;; item-values -> (x-center y-center x-left x-right y-top y-bottom linestyle color-fill)
			    (the fn left) ; left
			    (the fn (the fn (fifth param-list))) ; top
			    (the fn (- (the fn (fourth param-list)) left)) ;   diameter
			    (seventh param-list) ; lstyle
			    (eighth param-list) ; fstyle
			    virtual-thing-draw-fn)))
			(is-a-virtual-circle
			 (draw-virtual-circle
			  line-style-gc filling-style-gc drawable root-window
			  ;; item-values -> (xcenter ycenter radius fill soma diameter left top)
			  (the fn (seventh param-list)) ; left
			  (the fn (eighth param-list)) ; top
			  (the fn (sixth param-list)) ;   diameter 
			  virtual-circle-linestyle ; lstyle
			  (if colorize-virtual-thing
			      (surf-hippo::access-*fill-styles*-for-soma-voltage
			       (fifth param-list))
			      (fourth param-list)) ; fstyle
			  virtual-thing-draw-fn))

			(is-a-virtual-plotting-circle
			 (draw-virtual-circle
			  line-style-gc filling-style-gc drawable root-window
			  ;; item-values -> (left top diameter fill)
			  (the fn (first param-list)) ; left
			  (the fn (second param-list)) ; top
			  (the fn (third param-list)) ;   diameter 
			  virtual-circle-linestyle ; lstyle
			  (fourth param-list) ; fstyle
			  virtual-thing-draw-fn))
		    
			(is-a-virtual-segment
			 (draw-virtual-segment
			  line-style-gc drawable root-window
			  (the fn (first param-list))
			  (the fn (third param-list))
			  (the fn (second param-list))
			  (the fn (fourth param-list))
			  (if colorize-virtual-thing
			      (surf-hippo::access-*line-styles-array*-for-segment-voltage
			       (sixth param-list)
			       (seventh param-list))
			      (fifth param-list))
			  xlib-gc-line virtual-thing-draw-fn colorize-virtual-thing
			  display))
		      
			(t (setq dummy-vals-indx -1)
			   (dolist (slot dummy-slots-list)
			     (incf dummy-vals-indx)
			     (setf (aref (the simple-array dummy-update-slots-values) dummy-vals-indx) (g-value dummy slot)))
			   (draw dummy line-style-gc filling-style-gc drawable root-window))))))
		(when colorize-virtual-thing (xlib::display-invoke-after-function display)))
	      (let ((item-array (the (simple-array cons (* *)) item-array)))
		;; two dimensional
		(setq dummy-slots-list (cddr dummy-slots-list))
		(if (fifth clip-mask) (setq clip-mask (cddddr clip-mask)))
		(do-in-clip-rect (m n gob clip-mask)
		  (s-value dummy :rank1 m)
		  (s-value dummy :rank2 n)
		  (s-value dummy :item-values (aref item-array m n))
            ;;; faster than (opal::update-slots-values-changed dummy 2 update-info)
		  (setq dummy-vals-indx 1)
		  (dolist (slot dummy-slots-list)
		    (incf dummy-vals-indx)
		    (setf (aref (the simple-array dummy-update-slots-values) dummy-vals-indx)
			  (g-value dummy slot)))
		  (draw dummy line-style-gc filling-style-gc drawable
			root-window)))))
        (setf (bbox-valid-p (update-info-old-bbox (the UPDATE-INFO (g-value invalid-object :update-info)))) nil)
	(if dirty-p (setf (update-info-dirty-p update-info) NIL)))))
	


#|
;; original
(define-method :update opal:virtual-aggregate (gob update-info
                                       line-style-gc filling-style-gc
                                       drawable root-window
                                       bbox-1 bbox-2
                                       &optional (total-p NIL))
  (let* ((dummy (g-value gob :dummy-item))
	 (dummy-slots-list (g-value dummy :update-slots))
	 (dummy-update-slots-values (g-local-value dummy :update-slots-values))
	 dummy-vals-indx
	 item-bbox
	 (invalid-object (g-value gob :invalid-object))
	 (dirty-p (update-info-dirty-p update-info))
	 (agg-bbox (update-info-old-bbox update-info))
	 (array-size (g-value gob :array-length))
	 (bbox-array (g-value gob :bbox-array))
	 (item-array (g-value gob :item-array))
	 ;;; *** Temporary:
	 (clip-mask (list (g-value gob :left)
			  (g-value gob :top)
			  (g-value gob :width)
			  (g-value gob :height))))
    (s-value invalid-object :already-on-invalid-objects-list nil)
    (when
      (or dirty-p
	  total-p
	  (and (bbox-valid-p agg-bbox)
	       (bbox-intersects-either-p agg-bbox bbox-1 bbox-2)))
      (when (and (null bbox-1) (null bbox-2) (listp clip-mask)
		 (bbox-valid-p agg-bbox))
	(setq bbox-1 agg-bbox)
	(bbox-to-clip-mask agg-bbox clip-mask))
      (if (numberp array-size)   ;; one dimensional
        (dotimes (n (g-value gob :next-available-rank))
	  (setq item-bbox (aref bbox-array n))
          (when (and (bbox-valid-p item-bbox)
		     (or (and bbox-1 (bbox-intersect-p bbox-1 item-bbox))
		         (and bbox-2 (bbox-intersect-p bbox-2 item-bbox))))
            (s-value dummy :rank n)
            (s-value dummy :item-values (aref item-array n))
            ;;; faster than (opal::update-slots-values-changed dummy 0 update-info)
	    (setq dummy-vals-indx -1)
	    (dolist (slot dummy-slots-list)
	      (incf dummy-vals-indx)
  	      (setf (aref dummy-update-slots-values dummy-vals-indx)
	        (g-value dummy slot)))
            (draw dummy line-style-gc filling-style-gc drawable root-window)))
        (progn		       ;; two dimensional
	  (setq dummy-slots-list (cddr dummy-slots-list))
	  (if (fifth clip-mask) (setq clip-mask (cddddr clip-mask)))
          (do-in-clip-rect (m n gob clip-mask)
	    (s-value dummy :rank1 m)
	    (s-value dummy :rank2 n)
	    (s-value dummy :item-values (aref item-array m n))
            ;;; faster than (opal::update-slots-values-changed dummy 2 update-info)
	    (setq dummy-vals-indx 1)
	    (dolist (slot dummy-slots-list)
	      (incf dummy-vals-indx)
	      (setf (aref dummy-update-slots-values dummy-vals-indx)
	        (g-value dummy slot)))
	    (draw dummy line-style-gc filling-style-gc drawable
		  root-window)))))
    (setf (bbox-valid-p
	    (update-info-old-bbox
	      (the UPDATE-INFO
                (g-value invalid-object :update-info))))
          nil)
    (if dirty-p (setf (update-info-dirty-p update-info) NIL))))


|#

 