;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: SURF-HIPPO ; Base: 10; -*-
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


;;; SYS Source file: cell-graphics-virtual-schema.lisp


(IN-PACKAGE "SURF-HIPPO")

(create-instance 'virtual-line opal:line
		 (:constant :visible :fast-redraw-p :filling-style :draw-function)
		 (:visible t)
		 ;; (:fast-redraw-p t)
		 (:filling-style nil)
		 (:draw-function :copy)
		 (:x1 (o-formula (the fn (aref (the simple-array (gvl :item-values)) 0))))
		 (:y1 (o-formula (the fn (aref (the simple-array (gvl :item-values)) 1))))
		 (:x2 (o-formula (the fn (aref (the simple-array (gvl :item-values)) 2))))
		 (:y2 (o-formula (the fn (aref (the simple-array (gvl :item-values)) 3))))
		 (:line-style (o-formula (aref (the simple-array (gvl :item-values)) 4)))
		 (:left (o-formula (- (fixnum-min (gvl :x1) (gvl :x2))
				      (if (eq (gvl :line-style :cap-style) :projecting)
					  (fixnum-max 1 (gvl :line-style :line-thickness))
					  (floor (the fn (gvl :line-style :line-thickness)) 2)) 0)))
		 (:top (o-formula (- (fixnum-min (gvl :y1) (gvl :y2))
				     (if (eq (gvl :line-style :cap-style) :projecting)
					 (fixnum-max 1 (gvl :line-style :line-thickness))
					 (floor (the fn (gvl :line-style :line-thickness)) 2))) 0))
		 (:width (o-formula (+ (abs (- (the fn (gvl :x1)) (the fn (gvl :x2))))
				       (* (if (eq (gvl :line-style :cap-style) :projecting) 2 1)
					  (fixnum-max 1 (gvl :line-style :line-thickness))))))
		 (:height (o-formula (+ (abs (- (the fn (gvl :y1)) (the fn (gvl :y2))))
					(* (if (eq (gvl :line-style :cap-style) :projecting) 2 1)
					   (fixnum-max 1 (gvl :line-style :line-thickness))))))
		 (:update-function 'opal::update-virtual-line))


(create-instance 'virtual-segment virtual-line
		 (:line-style (o-formula (if (and (gvl :parent :window) (gvl :parent :window :colorize))
					     (access-*line-styles-array*-for-segment-voltage
					      (aref (gvl :item-values) 5) ; thickness
					      (aref (gvl :item-values) 6)) ; segment
					     (or (aref (gvl :item-values) 4) ; linestyle
						 (and (gvl :parent :window)
						      (create-instance nil (gvl :parent :window :default-line-style)
								       (:thickness (aref (gvl :item-values) 5)))))))))



;; item-values -> (xcenter ycenter radius fill soma diameter left top)
(create-instance 'virtual-circle  opal:circle
		 (:radius (o-formula (the fn (third (gvl :item-values)))))
		 (:diameter (o-formula (the fn (sixth (gvl :item-values)))))
		 (:left (o-formula (the fn (seventh (gvl :item-values)))))
		 (:top (o-formula (the fn (eighth (gvl :item-values)))))
		 (:width (o-formula (the fn (sixth (gvl :item-values)))))
		 (:height (o-formula (the fn (gvl :width))))
		 (:line-style (o-formula (gvl :parent :window :default-line-style)))
		 (:filling-style (o-formula
				  (or (fourth (gvl :item-values))
				      (gvl :parent :window :default-graphics-filling-style))))
		 (:draw-function :copy)
		 (:update-function 'opal::update-virtual-circle))

(create-instance 'virtual-circle-w/o-border virtual-circle
		 (:line-style nil))

(create-instance 'virtual-cell-element-marker virtual-circle
		 (:line-style (o-formula (or (gvl :parent :window :default-cell-element-marker-line-style)
					     thin-line
					     ))))

(create-instance 'virtual-cell-element-marker-no-border virtual-cell-element-marker
		 (:line-style nil))


(create-instance 'virtual-soma virtual-circle
		 (:line-style (o-formula (access-*line-styles-array*-for-segments-fast
					  0 (gvl :filling-style :FOREGROUND-COLOR) 100.0 nil)))
		 (:filling-style (o-formula (if (and (gvl :parent :window)
						     (gvl :parent :window :colorize) (fourth (gvl :item-values)))
						(access-*fill-styles*-for-soma-voltage (fifth (gvl :item-values)))
						(or (fourth (gvl :item-values))
						    (and (gvl :parent :window)
							 (gvl :parent :window :default-graphics-filling-style)))))))






#|
(defmacro generate-update-slots-function (virtual-thing)
  `(let ((slots (g-value ,virtual-thing :update-slots)))
    `#'(lambda (dummy dummy-update-slots-values)
	 ,@(loop for slot in slots
		 for dummy-vals-indx fixnum from 0
		 collect `(setf (aref dummy-update-slots-values ,dummy-vals-indx)
			   (g-value dummy ,slot))))))

(defmacro generate-update-slots-function (virtual-thing)
  `(let ((slots (g-value ,virtual-thing :update-slots)))
    `#'(lambda (dummy dummy-update-slots-values)
	 ,@(loop for slot in slots
		 for dummy-vals-indx fixnum from 0
		 collect `(setf (aref dummy-update-slots-values ,dummy-vals-indx)
			   (g-value dummy ,slot))))))

(defmacro generate-update-slots-function (virtual-thing)
  #'(lambda (dummy dummy-update-slots-values)
      `(let ((slots (g-value ,virtual-thing :update-slots)))
	,@(loop for slot in slots
	   for dummy-vals-indx fixnum from 0
	   collect `(setf (aref dummy-update-slots-values ,dummy-vals-indx)
		     (g-value dummy ,slot))))))

(s-value virtual-segment :generate-update-slots-function
	 (GENERATE-UPDATE-SLOTS-FUNCTION virtual-segment))

(s-value virtual-segment :generate-update-slots-function
	 #'(LAMBDA (DUMMY DUMMY-UPDATE-SLOTS-VALUES)
	     (SETF (AREF DUMMY-UPDATE-SLOTS-VALUES 0) (G-VALUE DUMMY :VISIBLE))
	     (SETF (AREF DUMMY-UPDATE-SLOTS-VALUES 1) (G-VALUE DUMMY :FAST-REDRAW-P))
	     (SETF (AREF DUMMY-UPDATE-SLOTS-VALUES 2) (G-VALUE DUMMY :X1))
	     (SETF (AREF DUMMY-UPDATE-SLOTS-VALUES 3) (G-VALUE DUMMY :X2))
	     (SETF (AREF DUMMY-UPDATE-SLOTS-VALUES 4) (G-VALUE DUMMY :Y1))
	     (SETF (AREF DUMMY-UPDATE-SLOTS-VALUES 5) (G-VALUE DUMMY :Y2))
	     (SETF (AREF DUMMY-UPDATE-SLOTS-VALUES 6) (G-VALUE DUMMY :LINE-STYLE))
	     (SETF (AREF DUMMY-UPDATE-SLOTS-VALUES 7) (G-VALUE DUMMY :FILLING-STYLE))
	     (SETF (AREF DUMMY-UPDATE-SLOTS-VALUES 8) (G-VALUE DUMMY :DRAW-FUNCTION))
	     ))
|#


