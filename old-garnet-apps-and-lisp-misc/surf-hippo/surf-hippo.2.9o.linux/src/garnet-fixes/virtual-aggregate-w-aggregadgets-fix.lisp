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

;;

(in-package "OPAL")

;; Supplied by:
#|
Return-Path: <gilham@csl.sri.com>
To: lyle@cogni.iaf.cnrs-gif.fr (Lyle Borg-Graham)
Subject: Re: More Garnet 3.0 glitches 
In-Reply-To: Your message of "Thu, 27 Aug 1998 02:55:20 EDT."
             <9808270655.AA01096@cogni.iaf.cnrs-gif.fr> 
Date: Thu, 03 Sep 1998 11:19:29 -0700
From: Fred Gilham <gilham@csl.sri.com>
|#

;; Patch to opal/update.lisp:

(define-method :update opal:graphical-object (gob update-info
						  bbox-1 bbox-2
						  &optional (total-p NIL))
  (declare (optimize (speed 3) (safety 0)))
  (let ((old-bbox (update-info-old-bbox update-info))
	(a-window (g-value gob :window)))
    (declare (optimize (speed 3) (safety 0)))
    ;; The following fixes a problem that developed in the change from
    ;; garnet-2.2 to garnet-3.0.  When the item-prototype of a virtual
    ;; aggregate consists of an aggregadget, the :window slot doesn't
    ;; get set in the parts of the aggregate for some reason.  This
    ;; results in a run-time error when the part gets drawn.
    ;; I don't know if this is the right way to fix this, but it seems
    ;; to work and doesn't seem to break anything else so far.
    ;; Also, see the :initialize method in virtual-aggregates.lisp.
    (unless a-window
      (setf a-window (g-value gob :parent :window)))
    (unless (update-info-on-fastdraw-list-p update-info)
      (cond (total-p
	     (update-slots-values-changed gob 0 update-info)
	     (update-bbox gob old-bbox)
	     (draw gob a-window)
	     (setf (update-info-dirty-p update-info) NIL))

	    ((update-info-dirty-p update-info)
	     (when (update-info-force-computation-p update-info)
	       (update-slots-values-changed gob 0 update-info)
	       (update-bbox gob old-bbox))
	     (draw gob a-window)
	     (setf (update-info-dirty-p update-info) NIL))

	    (bbox-2			; 2 valid clip-masks?
	     (when (or (bbox-intersect-p old-bbox bbox-1)
		       (bbox-intersect-p old-bbox bbox-2))
	       (draw gob a-window)))
	    ((bbox-intersect-p old-bbox bbox-1)
	     (draw gob a-window)))
      ;; New line added because of new KR 2.0.10 -- ECP 6/23/92
      ;; Without this line, the Save window in garnetdraw does not update.
      (setf (update-info-invalid-p update-info) nil))))


;; Patch to opal/virtual-aggregates.lisp:


(define-method :initialize opal:virtual-aggregate (gob)
	         (declare (optimize (speed 3) (safety 0)))
  (let ((dummy (create-instance nil (g-value gob :item-prototype)))
	array-length)
    ;; If dummy does not have :draw method, create one.
    (when (and (not (g-value dummy :draw))
	       (g-value dummy :update))

      ;; Start of fix.
       ;; The following change is necessary because the call signature
       ;; of the update function for aggregates no longer matches the
       ;; call signature of the update function for graphical objects.
       ;; Also, the window needs to be propagated.
       (if (is-a-p dummy opal:aggregate)
         (s-value dummy :draw
                  #'(lambda (dummy a-window)
                      ;; (declare (ignore a-window))
                      (s-value dummy :window a-window)
                      (update dummy (g-value dummy :update-info)
                              nil nil
                              NIL NIL T)))
       (s-value dummy :draw
                #'(lambda (dummy a-window)
                    (declare (ignore a-window))
                    (update dummy (g-value dummy :update-info)
                            NIL NIL T)))))
    ;; End of fix.

    (s-value gob :invalid-object
	     (create-instance nil opal::virtual-invalid-object
	       (:parent gob)
	       (:make-update-think-i-have-changed 0)))
    (s-value dummy :parent gob)
    (s-value dummy :update-slots-values
	     (make-array (length (the cons (g-value dummy :update-slots)))
			 :initial-element nil))
    (s-value gob :dummy-item dummy)
    (unless (g-value gob :item-array)
      (s-value gob :item-array
	       (make-array 0 :adjustable t :initial-element nil)))
    (setq array-length (array-dimensions (g-value gob :item-array)))
    (if (cdr array-length)		; TWO-DIMENSIONAL
      (progn
	(s-value gob :add-item NIL)
	(s-value gob :remove-item NIL))
      (setq array-length (car array-length)))
    (s-value gob :array-length array-length)
    
;    (make-virtual-agg-bbox-array gob array-length)

     (s-value gob :bbox-array
	   (make-array (the fixnum array-length) :element-type 'opal::bbox 
		       :adjustable t))
    
    (when (numberp array-length);; one dimensional
      (s-value gob :next-available-rank array-length))
    (call-prototype-method gob)
    (recalculate-virtual-aggregate-bboxes gob)
    (update-slots-values-changed gob 0 (g-local-value gob :update-info))))

(defun make-virtual-agg-bbox-array (gob array-length)
  (s-value gob :bbox-array
	   (make-array (the fixnum array-length) :element-type 'opal::bbox 
		       :adjustable t)))