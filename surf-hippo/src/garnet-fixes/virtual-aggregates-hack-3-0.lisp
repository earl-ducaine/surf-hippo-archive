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

;; improvements to some macros and functions, appropriate for Garnet 3.0

;; (in-package "KR")


#|
;; old

(defmacro call-prototype-method (&rest args)
  (let ((entry (gensym)))
    `(let ((first-c-p-m (and (null *kr-send-parent*)
			     (let ((,entry (slot-accessor *kr-send-self*
							  *kr-send-slot*)))
			       (or (null ,entry)
				   (is-inherited (sl-bits ,entry)))))))
      (multiple-value-bind (method new-parent)
	  (find-parent *kr-send-self* *kr-send-slot*)
	(when method
	  (if first-c-p-m
	    (multiple-value-setq (method *kr-send-parent*)
	      (find-parent new-parent *kr-send-slot*))
	    (setf *kr-send-parent* new-parent))
	  (if method
	    (let ((*kr-send-self* *kr-send-parent*))
	      (funcall method ,@args))))))))
;; 3.0
(defmacro call-prototype-method (&rest args)
  (let ((entry (gensym)))
    `(locally (declare ,*special-kr-optimization*)
       (let ((first-c-p-m (and (null *kr-send-parent*)
			     (let ((,entry (slot-accessor *kr-send-self*
							  *kr-send-slot*)))
			       (or (null ,entry)
				   (is-inherited (sl-bits ,entry)))))))
      (multiple-value-bind (method new-parent)
	  (find-parent *kr-send-self* *kr-send-slot*)
	(when method
	  (if first-c-p-m
	    (multiple-value-setq (method *kr-send-parent*)
	      (find-parent new-parent *kr-send-slot*))
	    (setf *kr-send-parent* new-parent))
	  (if method
	    (let ((*kr-send-self* *kr-send-parent*))
	      (funcall method ,@args)))))))))
|#


(in-package "OPAL")


;;; "thing" is a virtual aggregate.
(proclaim '(inline set-things-size))
(defun set-things-size (thing thing-bbox)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (s-value thing :left (bbox-x1 thing-bbox))
  (s-value thing :top (bbox-y1 thing-bbox))
  (s-value thing :width (the fixnum (- (the fixnum (bbox-x2 thing-bbox)) (the fixnum (bbox-x1 thing-bbox)))))
  (s-value thing :height (the fixnum (- (the fixnum (bbox-y2 thing-bbox)) (the fixnum (bbox-y1 thing-bbox))))))

(proclaim '(notinline initialize-item-bbox))
(defun initialize-item-bbox (thing thing-bbox bbox-array dummy item-array rank &optional rank2)
  (declare (optimize (safety 0) (speed 3) (space 1) (debug 0)))
  (let (src-bbox)
    (if rank2
	(progn
	  (s-value dummy :rank1 rank)
	  (s-value dummy :rank2 rank2)
	  (s-value dummy :item-values (aref (the (simple-array cons (* *)) item-array) rank rank2))
	  (setf (aref (the (array opal::bbox (*)) bbox-array) rank rank2) (make-bbox))
	  (setq src-bbox (aref (the (array opal::bbox (*)) bbox-array) rank rank2)))
		
	(progn
	  (s-value dummy :rank rank)
	  (s-value dummy :item-values (aref (the (simple-array cons (*)) item-array) rank))
	  (setf (aref (the (array opal::bbox (* *)) bbox-array) rank) (make-bbox))
	  (setq src-bbox (aref (the (array opal::bbox (* *)) bbox-array) rank))))

    (update-bbox dummy src-bbox)
    (merge-bbox thing-bbox src-bbox)
    (set-things-size thing thing-bbox))
  nil)
    
(defmacro update-bbox-fast (bbox left top left+width top+height)
    `(progn
	(setf (bbox-x1 ,bbox) ,left)
	(setf (bbox-y1 ,bbox) ,top)
	(setf (bbox-x2 ,bbox) ,left+width)
	(setf (bbox-y2 ,bbox) ,top+height)
	(setf (bbox-valid-p ,bbox) T)))

(defun initialize-item-bbox-fast (thing thing-bbox bbox-array dummy item-array rank
					left top left+width top+height
					&optional rank2)
  (declare (optimize (safety 0) (speed 3) (space 1) (debug 0)))
  (let (src-bbox)
    (if rank2
	(progn
	  (s-value dummy :rank1 rank)
	  (s-value dummy :rank2 rank2)
	  (s-value dummy :item-values (aref (the (simple-array cons (* *)) item-array) rank rank2))
	  (setf (aref (the (array opal::bbox (*)) bbox-array) rank rank2) (make-bbox))
	  (setq src-bbox (aref (the (array opal::bbox (*)) bbox-array) rank rank2)))
		
	(progn
	  (s-value dummy :rank rank)
	  (s-value dummy :item-values (aref (the (simple-array cons (*)) item-array) rank))
	  (setf (aref (the (array opal::bbox (* *)) bbox-array) rank) (make-bbox))
	  (setq src-bbox (aref (the (array opal::bbox (* *)) bbox-array) rank))))

    (update-bbox-fast src-bbox left top left+width top+height)
    (merge-bbox thing-bbox src-bbox)
    (set-things-size thing thing-bbox))
  nil)


#|
(defun recalculate-virtual-aggregate-bboxes (thing)
  (let ((dummy (g-value thing :dummy-item))
	(item-array (g-value thing :item-array))
	(bbox-array (g-value thing :bbox-array))
	(array-length (g-value thing :array-length))
        (thing-bbox (update-info-old-bbox (the UPDATE-INFO (g-value thing :update-info)))))
    (if (numberp array-length)  ;; one-dimensional
      (dotimes (n array-length)
        (when (aref item-array n)
          (initialize-item-bbox thing thing-bbox bbox-array dummy item-array n)))
      (dotimes (m (first array-length))
	(dotimes (n (second array-length))
	  (when (aref item-array m n)
            (initialize-item-bbox thing thing-bbox bbox-array dummy item-array m n)))))
    (if (and thing-bbox (bbox-valid-p thing-bbox))
      (set-things-size thing thing-bbox)
      (progn
	(s-value thing :left 0)
	(s-value thing :top 0)
	(s-value thing :width 0)
	(s-value thing :height 0)))))
|#
;: New version
(defun recalculate-virtual-aggregate-bboxes (thing)
  (declare (optimize (safety 0) (speed 3) (space 1)(debug 0)))
  
  (let* ((dummy (g-value thing :dummy-item))
	 (item-array (g-value thing :item-array))
	 (bbox-array (g-value thing :bbox-array))
	 (array-length (g-value thing :array-length))
	 (thing-bbox (update-info-old-bbox (the UPDATE-INFO (g-value thing :update-info))))
;	 (left (g-value dummy :left))
;	 (top  (g-value dummy :top ))
;	 (left+width (the fixnum (+ (the fixnum left) (the fixnum (g-value dummy :width )))))
;	 (top+height (the fixnum (+ (the fixnum top) (the fixnum (g-value dummy :height)))))
	 )
;    (format t "REady ..~%")

    (if (numberp array-length)		; one-dimensional
	(do ((n 0 (the fixnum (1+ n))))
	    ((= (the fixnum n) (the fixnum array-length)))
	  (when (aref (the (simple-array cons (*)) item-array) n)
	    (initialize-item-bbox thing thing-bbox bbox-array dummy item-array n)
	    ; (initialize-item-bbox-fast thing thing-bbox bbox-array dummy item-array n left top left+width top+height)
	    ))
	(do ((m 0 (the fixnum (1+ m))))
	    ((= (the fixnum m) (the fixnum (first array-length))))
	  (do ((n 0 (the fixnum (1+ n))))
	      ((= (the fixnum n) (the fixnum (second array-length))))
	    (when (aref (the (simple-array cons (* *)) item-array) m n)
	      (initialize-item-bbox thing thing-bbox bbox-array dummy item-array m n)))))
    (if (and thing-bbox (bbox-valid-p thing-bbox))
	(set-things-size thing thing-bbox)
	(progn
	  (s-value thing :left 0)
	  (s-value thing :top 0)
	  (s-value thing :width 0)
	  (s-value thing :height 0)))))


#|
(define-method :initialize opal:virtual-aggregate (gob)
  (let ((dummy (create-instance nil (g-value gob :item-prototype)))
	array-length)
    ;; If dummy does not have :draw method, create one.
    (when (and (not (g-value dummy :draw))
	       (g-value dummy :update))
      (s-value dummy :draw
	 #'(lambda (dummy line-style-gc filling-style-gc drawable root-window)
	     (update dummy (g-value dummy :update-info) line-style-gc filling-style-gc
		     drawable root-window NIL NIL T))))	     
    (s-value gob :invalid-object
      (create-instance nil opal::virtual-invalid-object
	(:parent gob)
	(:make-update-think-i-have-changed 0)))
    (s-value dummy :parent gob)
    (s-value dummy :update-slots-values
       (make-array (length (g-value dummy :update-slots)) :initial-element nil))
    (s-value gob :dummy-item dummy)
    (unless (g-value gob :item-array)
      (s-value gob :item-array (make-array 0 :adjustable t :initial-element nil)))
    (setq array-length (array-dimensions (g-value gob :item-array)))
    (if (cdr array-length) ; TWO-DIMENSIONAL
	(progn
	  (s-value gob :add-item NIL)
	  (s-value gob :remove-item NIL))
	(setq array-length (car array-length)))
    (s-value gob :array-length array-length)
    (s-value gob :bbox-array (make-array array-length :element-type 'opal::bbox ; :adjustable t
					 ))
    (when (numberp array-length) ;; one dimensional
      (s-value gob :next-available-rank array-length))
    (call-prototype-method gob)
    (recalculate-virtual-aggregate-bboxes gob)
    (update-slots-values-changed gob 0 (g-local-value gob :update-info))))
|#

#|
  
(define-method :draw opal:line (gob line-style-gc filling-style-gc
				drawable root-window)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (ignore filling-style-gc))
  (let* ((xlib-gc-line (opal::opal-gc-gcontext line-style-gc))
	 (update-vals  (g-local-value gob :update-slots-values))
	 (lstyle       (aref (the (simple-array * (*)) update-vals) OPal::*line-lstyle*))
	 (x-draw-fn    (get (aref (the (simple-array * (*)) update-vals) opal::*line-draw-function*)
			    :x-draw-function)))
  (when lstyle
    (opal::set-line-style lstyle line-style-gc xlib-gc-line root-window x-draw-fn)
    (xlib:draw-line drawable
		    xlib-gc-line
		    (aref (the (simple-array * (*)) update-vals) opal::*line-x1*)
		    (aref (the (simple-array * (*)) update-vals) opal::*line-y1*)
		    (aref (the (simple-array * (*)) update-vals) opal::*line-x2*)
		    (aref (the (simple-array * (*)) update-vals) opal::*line-y2*)))))

|#

(in-package "OPAL")

(define-method :draw opal:line (gob a-window)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let* ((update-vals  (g-local-value gob :update-slots-values))
	 (lstyle (aref (the (simple-array * (*)) update-vals) *line-lstyle*)))
    (if lstyle
	(gem:draw-line a-window
		       (the fixnum (aref (the (simple-array * (*)) update-vals) *line-x1*))
		       (the fixnum (aref (the (simple-array * (*)) update-vals) *line-y1*))
		       (the fixnum  (aref (the (simple-array * (*)) update-vals) *line-x2*))
		       (the fixnum (aref (the (simple-array * (*)) update-vals) *line-y2*))
		       (aref (the (simple-array * (*)) update-vals) *line-draw-function*)
		       lstyle))))

(defmacro do-in-clip-rect ((m n agg rect) &body body)
  `(let* ((agg* ,agg)
	  (p-to-r (g-value agg* :point-to-rank))
	  (r* ,rect)
	  (array-size* (g-value agg* :array-length))
	  (max-x2* (1- (the fixnum (first array-size*))))
	  (max-y2* (1- (the fixnum (second array-size*))))
	  (first* (first r*))
	  (second* (second r*)))
    (declare (fixnum max-x2* max-y2* first* second* max-x2* max-y2*))
     (multiple-value-bind (x1* y1*)
       		          (funcall p-to-r agg* first* second*)
       (multiple-value-bind (x2* y2*)
			    (funcall p-to-r agg* (the fixnum (+ first* (the fixnum (third r*)) -1))
				     (the fixnum (+ second* (the fixnum (fourth r*)) -1)))
	 (declare (fixnum x1* x2* y1* y2*)) 
	 (setq x1* (the fixnum (if x1* (max 0 x1*) 0)))
	 (setq y1* (the fixnum (if y1* (max 0 y1*) 0)))
	 (setq x2* (the fixnum (if x2* (min x2* max-x2*) max-x2*)))
	 (setq y2* (the fixnum (if y2* (min y2* max-y2*) max-y2*)))
	 (when (and (<= x1* x2*) (<= y1* y2*))
	   (do ((,m x1* (1+ (the fixnum ,m))))
	       ((> (the fixnum ,m) (the fixnum x2*)))
	     (do ((,n y1* (1+ (the fixnum ,n))))
	         ((> (the fixnum ,n) (the fixnum y2*)))
	       ,@body)))))))


#|
(defmacro copy-from-buffer-to-drawable (bbox buffer buffer-gc drawable)
  `(let ((x1 (the fixnum (bbox-x1 ,bbox)))
	 (x2 (the fixnum (bbox-x2 ,bbox)))
	 (y1 (the fixnum (bbox-y1 ,bbox)))
	 (y2 (the fixnum (bbox-y2 ,bbox))))
     (xlib:copy-area ,buffer ,buffer-gc x1 y1 (- x2 x1) (- y2 y1)
		     ,drawable x1 y1)))
|#

(defun copy-from-buffer-to-drawable (a-window bbox buffer drawable)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let ((x1 (the fixnum (bbox-x1 bbox)))
	(x2 (the fixnum (bbox-x2 bbox)))
	(y1 (the fixnum (bbox-y1 bbox)))
	(y2 (the fixnum (bbox-y2 bbox))))
    (gem:bit-blit a-window buffer x1 y1 (- x2 x1) (- y2 y1)
		  drawable x1 y1)))


#|
;;; Gives rank of item at point <x,y>.
(define-method :point-to-rank opal:virtual-aggregate (gob x y)
  (let ((item-array (g-value gob :item-array))
        (point-in-item (g-value gob :point-in-item))
        item)
    (do ((rank (1- (g-value gob :next-available-rank)) (1- rank)))
        ((< rank 0) (return nil))
      (setq item (aref item-array rank))
      (when (and item (funcall point-in-item gob item x y))
        (return rank)))))

(define-method :point-to-component opal:virtual-aggregate (a-thing x y &key (type t))
  (when (or (eq type t)
            (opal::my-is-a-p (g-value a-thing :item-prototype) type))
    (let ((dummy (g-value a-thing :dummy-item))
          (rank (point-to-rank a-thing x y)))
      (when rank
        (s-value dummy :rank rank)
        (s-value dummy :item-values (aref (g-value a-thing :item-array) rank))
        dummy))))

(defmacro do-in-clip-rect ((m n agg rect) &body body)
  `(let* ((agg* ,agg)
	  (p-to-r (g-value agg* :point-to-rank))
	  (r* ,rect)
	  (array-size* (g-value agg* :array-length))
	  (max-x2* (1- (first array-size*)))
	  (max-y2* (1- (second array-size*)))
	  (first* (first r*))
	  (second* (second r*)))
     (multiple-value-bind (x1* y1*)
       		          (funcall p-to-r agg* first* second*)
       (multiple-value-bind (x2* y2*)
			    (funcall p-to-r agg* (+ first* (third r*) -1)
						 (+ second* (fourth r*) -1))
	 (setq x1* (if x1* (max 0 x1*) 0))
	 (setq y1* (if y1* (max 0 y1*) 0))
	 (setq x2* (if x2* (min x2* max-x2*) max-x2*))
	 (setq y2* (if y2* (min y2* max-y2*) max-y2*))
	 (when (and (<= x1* x2*) (<= y1* y2*))
	   (do ((,m x1* (1+ ,m)))
	       ((> ,m x2*))
	     (do ((,n y1* (1+ ,n)))
	         ((> ,n y2*))
	       ,@body)))))))

(define-method :destroy-me opal:virtual-aggregate (gob &optional (top-level-p t))
  (call-prototype-method gob top-level-p))
|#


;; These two redefinitions are to avoid consing up a clip-mask
(defmacro bbox-to-clip-mask-components (bb clip-mask-x1 clip-mask-y1 clip-mask-x2 clip-mask-y2)
  `(let (				;(cm ,clip-mask)
	 )
    (setf ,clip-mask-x1			; (car cm)
     (bbox-x1 ,bb))
    (setf ,clip-mask-y1			; (car (setq cm (cdr cm)))
     (bbox-y1 ,bb))
    (setf ,clip-mask-x2			; (car (setq cm (cdr cm)))
     (the fixnum (- (the fixnum (bbox-x2 ,bb)) (the fixnum (bbox-x1 ,bb)))))
    (setf ,clip-mask-y2			; (cadr cm)
     (the fixnum (- (the fixnum (bbox-y2 ,bb)) (the fixnum (bbox-y1 ,bb)))))))


(defmacro do-in-clip-rect-components ((m n agg rect-first rect-second rect-third rect-fourth) &body body)
  `(let* ((agg* ,agg)
	  (p-to-r (g-value agg* :point-to-rank))
	  ; (r* ,rect)
	  (array-size* (g-value agg* :array-length))
	  (max-x2* (1- (first array-size*)))
	  (max-y2* (1- (second array-size*)))
	  (first* ,rect-first)
	  (second* ,rect-second)
	  (third* ,rect-third)
	  (fourth* ,rect-fourth)
	  )
    (declare (fixnum first* second* third* fourth*))
     (multiple-value-bind (x1* y1*)
       		          (funcall p-to-r agg* first* second*)
       (declare (fixnum x1* y1*))
       (multiple-value-bind (x2* y2*)
			    (funcall p-to-r agg* (+ first* third* -1)
						 (+ second* fourth* -1))
	 (declare (fixnum x2* y2*))
	 (setq x1* (if x1* (max 0 x1*) 0))
	 (setq y1* (if y1* (max 0 y1*) 0))
	 (setq x2* (if x2* (min x2* max-x2*) max-x2*))
	 (setq y2* (if y2* (min y2* max-y2*) max-y2*))
	 (when (and (<= x1* x2*) (<= y1* y2*))
	   (do ((,m x1* (1+ ,m)))
	       ((> ,m x2*))
	     (do ((,n y1* (1+ ,n)))
	         ((> ,n y2*))
	       ,@body)))))))

(defvar *enable-generate-update-slots-function* nil)

(defun is-a-colorizable-thing (dummy-item)
  (loop for member in (g-value dummy-item :is-a)
	when (or (eq sh::VIRTUAL-SEGMENT member)
		 (eq sh::VIRTUAL-soma member)
		 (eq sh::VIRTUAL-circle member)
		 )
	do (return t)))

#+GARNET-V3.0
(define-method :update opal:virtual-aggregate (gob update-info bbox-1 bbox-2 &optional (total-p NIL))
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (when (or (not sh::*colorizing-simulation*)
	    (is-a-colorizable-thing (g-value gob :dummy-item)))
    (if (g-value gob :dummy-item :update-function) ; Check for a special update function for the prototype.
	(funcall (g-value gob :dummy-item :update-function) gob update-info bbox-1 bbox-2 total-p)
	(let* ((dummy (g-value gob :dummy-item))
	       (dummy-slots-list (g-value dummy :update-slots))
	       (dummy-update-slots-values (the simple-array (g-local-value dummy :update-slots-values)))
	       (dummy-vals-indx 0)
	       item-bbox
	       (invalid-object (g-value gob :invalid-object))
	       (dirty-p (update-info-dirty-p update-info))
	       (agg-bbox (update-info-old-bbox update-info))
	       (array-size (g-value gob :array-length))
	       (bbox-array (the simple-array (g-value gob :bbox-array)))
	       (item-array (g-value gob :item-array))
	       (a-window (g-value gob :window)))
	  (declare (fixnum dummy-vals-indx))
	  (s-value invalid-object :already-on-invalid-objects-list nil)
	  (when
	      (or dirty-p
		  total-p
		  (and (bbox-valid-p agg-bbox)
		       (bbox-intersects-either-p agg-bbox bbox-1 bbox-2)))
	    (when (and (null bbox-1) (null bbox-2) ; (listp clip-mask)
		       (bbox-valid-p agg-bbox))
	      (setq bbox-1 agg-bbox)
	      ;; (bbox-to-clip-mask agg-bbox clip-mask)
	      (bbox-to-clip-mask-components
	       agg-bbox (g-value gob :left) (g-value gob :top) (g-value gob :width) (g-value gob :height)))
	 
	    (if (numberp array-size)	; one dimensional
		(progn	
		  (dotimes (n (the fixnum (g-value gob :next-available-rank)))
		    (declare (fixnum n))
		    (setq item-bbox (aref bbox-array n))
		    (when (and (bbox-valid-p item-bbox)
			       (or (and bbox-1 (bbox-intersect-p bbox-1 item-bbox))
				   (and bbox-2 (bbox-intersect-p bbox-2 item-bbox))))
		      (s-value dummy :rank n)
		      (s-value dummy :item-values (aref (the simple-array item-array) n))
		      ;; faster than (opal::update-slots-values-changed dummy 0 update-info)
		      (setq dummy-vals-indx -1)
		      (dolist (slot dummy-slots-list)
			(incf dummy-vals-indx)
			(setf (aref dummy-update-slots-values dummy-vals-indx)
			      (g-value dummy slot)))
		      (draw dummy a-window))))
		(progn			; two dimensional
		  (setq dummy-slots-list (cddr dummy-slots-list))
		  (let ((clip-mask (list (g-value gob :left)
					 (g-value gob :top)
					 (g-value gob :width)
					 (g-value gob :height))))
		    (if (fifth clip-mask) (setq clip-mask (cddddr clip-mask)))
		    (do-in-clip-rect (m n gob clip-mask)
		      (s-value dummy :rank1 m)
		      (s-value dummy :rank2 n)
		      (s-value dummy :item-values (aref item-array m n))
		      ;; faster than (opal::update-slots-values-changed dummy 2 update-info)
		      (setq dummy-vals-indx 1)
		      (dolist (slot dummy-slots-list)
			(incf dummy-vals-indx)
			(setf (aref dummy-update-slots-values dummy-vals-indx)
			      (g-value dummy slot)))
		      (draw dummy a-window))))))
	  (setf (bbox-valid-p
		 (update-info-old-bbox
		  (the UPDATE-INFO
		       (g-value invalid-object :update-info))))
		nil)
	  (if dirty-p (setf (update-info-dirty-p update-info) NIL))))))



(defvar *virtual-line* nil)
