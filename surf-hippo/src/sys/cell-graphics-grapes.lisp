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


;;; SYS Source file: cell-graphics-grapes.lisp


(IN-PACKAGE "SURF-HIPPO")



;; 'GRAPES Given a list of N colors, this aggregadget constructs a radially symmetric (mod N)
;; collection of circles whose center of gravity is given by (CENTER-X, CENTER-Y) in pixels. The
;; diameter of each grape is given by the :GRAPE-SIZE slot in the parent window, or the global
;; variable *GRAPE-SIZE* if this value is NIL. The grapes will have borders if the parent window
;; slot :SUPPRESS-ELEMENT-MARKER-BORDERS is NIL. The line style of the borders is given by the
;; parent window slot :DEFAULT-LINE-STYLE.
(create-instance 'GRAPES opal:aggregadget
		 (:grape-colors '(red))
		 (:center-x 0) (:center-y 0)
		 (:window nil)
		 (:grape-size (o-formula (if (gvl :window) (the fn (or (gvl :window :grape-size) *GRAPE-SIZE*)))))
		 (:borders-p (o-formula (if (gvl :window) (not (gvl :window :suppress-element-marker-borders)) t)))
		 (:line-style (o-formula (when (and (gvl :borders-p) (gvl :window)) (gvl :window :default-line-style))))
		 (:parts (o-formula (let* ((grape-size (or (gvl :grape-size) 10))
					   (half-grape (round (/ grape-size 2)))
					   (color-length (length (gvl :grape-colors)))
					   (angle-inc (/ 360.0 color-length)))
				      (loop for i fixnum from 0
					    for angle single-float from 0.0 by (the sf angle-inc)
					    for color in (gvl :grape-colors)
					    collect `(:grapes ,opal:circle
						      (:width ,grape-size)
						      (:height ,grape-size)
						      (:line-style ,(o-formula (gvl :parent :line-style)))
						      (:filling-style ,(color-to-fill color))
						      (:left ,(+ (the fn (gvl :center-x)) (- (the fn half-grape))
							       (the fn (if (= color-length 0) 0 
									   (round (* (the sf (cos-degrees angle))
										     (the fn half-grape)))))))
						      (:top ,(+ (the fn (gvl :center-y)) (- (the fn half-grape))
							      (the fn (if (= color-length 0) 0 
									  (round (* (the sf (sin-degrees angle))
										    (the fn half-grape)))))))))))))


;;; Old stuff
#|
(create-instance 'grape opal:aggregadget
		 (:grape-colors '(red)) (:left 0) (:top 0)
		 (:window nil)
		 (:line-style (o-formula (if (gvl :window)
					     (if (equal opal::white-fill (gvl :window :default-graphics-background))
						 opal:thin-line
						 thin-white-line)
					     opal:thin-line)))
		 (:parts
                  (o-formula (let* ((half-grape (round (/ (the fn *grape-size*) 2)))
				    (color-length (length (gvl :grape-colors)))
				    (angle-inc (/ 360.0 color-length))
				    out)
			       (do ((i 0 (+ 1 (the fn i)))
				    (angle 0.0 (the sf (+ (the sf angle-inc) (the sf angle))))
				    (colors (gvl :grape-colors) (cdr colors)))
				   ((null colors))
				 (declare (single-float angle) (fixnum i)) 
				 (push 
				  (list `:grapes opal:circle
					(list :width *grape-size*)
					(list :height *grape-size*)
					(list :line-style (o-formula (gvl :parent :line-style)))
					(list :filling-style (color-to-fill (car colors)))
					(list :left (+ (the fn (gvl :left)) (- (the fn half-grape))
						       (the fn (if (= color-length 0) 0 
								   (round (* (the sf (cos-degrees angle))
									     (the fn half-grape)))))))
					(list :top (+ (the fn (gvl :top)) (- (the fn half-grape))
						      (the fn (if (= color-length 0) 0 
								  (round (* (the sf (sin-degrees angle))
									    (the fn half-grape))))))))
				  out))
			       out))))


;; version in tars temp dir
(create-instance 'virtual-grape opal:aggregadget
		 (:grape-colors '(red)) (:left 0) (:top 0)
		 (:parts-spec
                  (o-formula (let* ((half-grape (round (/ (the fn *grape-size*) 2)))
				    (color-length (length (gvl :grape-colors)))
				    (angle-inc (/ 360.0 color-length))
				    out)
			       (declare (single-float angle-inc)
					(fixnum half-grape))
			       (do ((i 0 (+ 1 (the fn i)))
				    (angle 0.0 (the sf (+ (the sf angle-inc) (the sf angle))))
				    (colors (gvl :grape-colors) (cdr colors)))
				   ((null colors))
				 (declare (single-float angle)
					  (fixnum i)) 
				 (push 
				  (list `:grapes opal:circle
					(list :width *grape-size*)
					(list :height *grape-size*)
					(list :filling-style (color-to-fill (car colors)))
					(list :left (the fn
						     (+ (the fn (gvl :left))
							(the fn (- half-grape))
							(the fn
							(if (= color-length 0) 0 
							    (round (*
								    (the sf (cos-degrees angle))
								    (the fn half-grape))))))))
					(list :top (the fn 
						    (+ (the fn (gvl :top))
						       (the fn (- half-grape))
						       (the fn
						       (if (= color-length 0) 0 
							   (round (*
								   (the sf (sin-degrees angle))
								   (the fn half-grape)))))))))
				  out))
			       out)))

		 (:parts (o-formula (gvl :parts-spec))))

|#



#|
(create-instance 'virtual-grape-w/o-borders virtual-grape
		 (:parts
                  (o-formula (let* ((half-grape (round (/ (the fn *grape-size*) 2)))
				    (color-length (length (gvl :grape-colors)))
				    (angle-inc (/ 360.0 color-length)))
			       (loop for i fixnum from 0
				     for angle single-float from 0.0 by (the sf angle-inc)
				     for color in (gvl :grape-colors)
				     collect `(:grapes ,opal:circle
					       (:width ,*grape-size*)
					       (:height ,*grape-size*)
					       (:line-style nil)
					       (:filling-style ,(color-to-fill color))
					       (:left ,(+ (the fn (gvl :left)) (- (the fn half-grape))
							(the fn (if (= color-length 0) 0 
								    (round (* (the sf (cos-degrees angle))
									      (the fn half-grape)))))))
					       (:top ,(+ (the fn (gvl :top)) (- (the fn half-grape))
						       (the fn (if (= color-length 0) 0 
								   (round (* (the sf (sin-degrees angle))
									     (the fn half-grape)))))))))))))
					      
(create-instance 'virtual-grape-w/o-borders opal:aggregadget
		 (:grape-colors '(red))
		 (:left 0)
		 (:top 0)
		 (:parts-spec
                  (o-formula (let* ((half-grape (round (/ (the fn *grape-size*) 2)))
				    (color-length (length (gvl :grape-colors)))
				    (angle-inc (/ 360.0 color-length))
				    out)
			       (declare (single-float angle-inc)
					(fixnum half-grape))
			       (do ((i 0 (+ 1 (the fn i)))
				    (angle 0.0 (the sf (+ (the sf angle-inc) (the sf angle))))
				    (colors (gvl :grape-colors) (cdr colors)))
				   ((null colors))
				 (declare (single-float angle)
					  (fixnum i)) 
				 (push 
				  (list `:grapes opal:circle
					(list :width *grape-size*)
					(list :height *grape-size*)
					(list :filling-style (color-to-fill (car colors)))
					(list :line-style nil)
					(list :left (the fn
							 (+ (the fn (gvl :left))
							    (the fn (- half-grape))
							    (the fn	(if (= color-length 0) 0 
									    (round (* (the sf (cos-degrees angle))
										      (the fn half-grape))))))))
					(list :top (the fn 
							(+ (the fn (gvl :top))
							   (the fn (- half-grape))
							   (the fn (if (= color-length 0) 0 
								       (round (* (the sf (sin-degrees angle))
										 (the fn half-grape)))))))))
				  out))
			       out)))
		 (:parts (o-formula (gvl :parts-spec))))
|#