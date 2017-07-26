;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: SON-OF-PLOT-HACK; Base: 10 -*-
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


;; GUI Source file: virtual-things.lisp



(in-package "SON-OF-PLOT-HACK")
;; item-values -> (left top diameter fill)
(create-instance 'virtual-plotting-circle opal:circle
		 (:filling-style (o-formula (fourth (gvl :item-values))))
		 (:left (o-formula (first (gvl :item-values))))
		 (:top (o-formula (second (gvl :item-values))))
		 (:width (o-formula (third (gvl :item-values))))
		 (:height (o-formula (third (gvl :item-values)))))

(create-instance 'virtual-rectangle  opal:rectangle
		 (:line-style nil)
		 (:filling-style (o-formula (fifth (gvl :item-values))))
		 (:left (o-formula (first (gvl :item-values))))
		 (:top (o-formula (second (gvl :item-values))))
		 (:width (o-formula (third (gvl :item-values))))
		 (:height (o-formula (fourth (gvl :item-values)))))
    
;; (center-x center-y half-width half-height line-style)

(create-instance 'virtual-centered-h-line opal:line
		 (:x1 (o-formula (- (the fn (first (gvl :item-values)))
				    (the fn (third (gvl :item-values))))))
		 (:x2 (o-formula (+ (the fn (first (gvl :item-values)))
				    (the fn (third (gvl :item-values))))))
		 (:y1 (o-formula (second (gvl :item-values))))
		 (:y2 (o-formula (second (gvl :item-values))))
		 (:line-style (o-formula (fifth (gvl :item-values)))))

;; (center-x center-y half-width half-height line-style)
(create-instance 'virtual-centered-v-line opal:line
		 (:y1 (o-formula (- (the fn (second (gvl :item-values)))
				    (the fn (fourth (gvl :item-values))))))
		 (:y2 (o-formula (+ (the fn (second (gvl :item-values)))
				    (the fn (fourth (gvl :item-values))))))
		 (:x1 (o-formula (first (gvl :item-values))))
		 (:x2 (o-formula (first (gvl :item-values))))
   		 (:line-style (o-formula (fifth (gvl :item-values)))))






;; The following are symbols used in scatter plots.

(defmacro virtual-x-center () `(the fn (first (gvl :parent :item-values))))
(defmacro virtual-y-center () `(the fn (second (gvl :parent :item-values))))
(defmacro virtual-x-left () `(the fn (third (gvl :parent :item-values))))
(defmacro virtual-x-right () `(the fn (fourth (gvl :parent :item-values))))
(defmacro virtual-y-top () `(the fn (fifth (gvl :parent :item-values))))
(defmacro virtual-y-bottom () `(the fn (sixth (gvl :parent :item-values))))
(defmacro virtual-linestyle () `(seventh (gvl :parent :item-values)))
;(defmacro virtual-linestyle () wh::thin-black-line)
(defmacro virtual-fillstyle () `(eighth (gvl :parent :item-values)))


(defmacro virtual-immediate-x-center () `(the fn (first (gvl :item-values))))
(defmacro virtual-immediate-y-center () `(the fn (second (gvl :item-values))))
(defmacro virtual-immediate-x-left () `(the fn (third (gvl :item-values))))
(defmacro virtual-immediate-x-right () `(the fn (fourth (gvl :item-values))))
(defmacro virtual-immediate-y-top () `(the fn (fifth (gvl :item-values))))
(defmacro virtual-immediate-y-bottom () `(the fn (sixth (gvl :item-values))))
(defmacro virtual-immediate-linestyle () `(seventh (gvl :item-values)))
(defmacro virtual-immediate-fillstyle () `(eighth (gvl :item-values)))



;; #+GARNET-V3.0
#|
(create-instance 'virtual-dot opal:circle
		 (:line-style (o-formula (when (and (gvl :parent :window)
						    (gvl :parent :window :scatter-symbol-borderp))
					   (virtual-immediate-linestyle))))
		 (:filling-style (o-formula (virtual-immediate-fillstyle)))
		 (:left (o-formula (- (virtual-immediate-x-center)
				      (the fn (round (/ (the fn (gvl :width)) 2))))))
		 (:top (o-formula (- (virtual-immediate-y-center)
				     (the fn (round (/ (the fn (gvl :width)) 2))))))
		 (:width (o-formula (max (the fn (- (virtual-immediate-x-right) (virtual-immediate-x-left)))
					 (the fn (- (virtual-immediate-y-bottom) (virtual-immediate-y-top))))))
		 (:height (o-formula (the fn (gvl :width)))))
|#

;; #-GARNET-V3.0
(create-instance 'virtual-dot opal:aggregadget
		 (:parts `((:dot ,opal:circle
			    (:line-style ,(o-formula (virtual-linestyle)))
			    (:filling-style ,(o-formula (virtual-fillstyle)))
			    (:left ,(o-formula (- (virtual-x-center)
						  (the fn (round (/ (the fn (gvl :height)) 2))))))
			    (:top ,(o-formula (- (virtual-y-center)
						 (the fn (round (/ (the fn (gvl :height)) 2))))))
			    (:height ,(o-formula
				       (let ((val (the fn (+ ; 3
							  (- (virtual-y-bottom) (virtual-y-top))))))
					 val)))
			    (:width ,(o-formula (the fn (gvl :height))))))))


(defvar *virtual-cross-line-style* OPAL:DEFAULT-LINE-STYLE)
(export '(*VIRTUAL-CROSS-LINE-STYLE*))

(create-instance 'virtual-cross opal:aggregadget
		 ;; :item-values = (x-center y-center x-left x-right y-top y-bottom linestyle color-fill)
		 (:parts `((:h-line ,opal:line
				    ; (:line-style ,(o-formula *virtual-cross-line-style*)) ;
				    (:line-style ,(o-formula (virtual-linestyle)))
				    (:x1 ,(o-formula (virtual-x-left)))
				    (:x2 ,(o-formula (virtual-x-right)))
				    (:y1 ,(o-formula (virtual-y-center)))
				    (:y2 ,(o-formula (virtual-y-center))))
			   (:v-line ,opal:line
				    ; (:line-style ,(o-formula *virtual-cross-line-style*)) ;
				    (:line-style ,(o-formula (virtual-linestyle)))
				    (:x1 ,(o-formula (virtual-x-center)))
				    (:x2 ,(o-formula (virtual-x-center)))
				    (:y1 ,(o-formula (virtual-y-top)))
				    (:y2 ,(o-formula (virtual-y-bottom)))))))


(create-instance 'virtual-tilted-cross opal:aggregadget
		 ;; :item-values = (x-center y-center x-left x-right y-top y-bottom linestyle color-fill)
		 (:parts `((:first-line ,opal:line
					(:line-style ,(o-formula *virtual-cross-line-style*)) ; (:line-style ,(o-formula (virtual-linestyle)))
					(:x1 ,(o-formula (virtual-x-left)))
					(:x2 ,(o-formula (virtual-x-right)))
					(:y1 ,(o-formula (virtual-y-top)))
					(:y2 ,(o-formula (virtual-y-bottom))))
			   (:second-line ,opal:line
					 (:line-style ,(o-formula *virtual-cross-line-style*)) ; (:line-style ,(o-formula (virtual-linestyle)))
					 (:x1 ,(o-formula (virtual-x-left)))
					 (:x2 ,(o-formula (virtual-x-right)))
					 (:y1 ,(o-formula (virtual-y-bottom)))
					 (:y2 ,(o-formula (virtual-y-top)))))))



(create-instance 'virtual-up-triangle opal:aggregadget
		 ;; :item-values = (x-center y-center x-left x-right y-top y-bottom linestyle color-fill)		 
		 (:parts `((:body ,opal:polyline
				  (:line-style ,(o-formula (virtual-linestyle))) ; ,(o-formula (when (gvl :parent :borderp) (virtual-linestyle)))
				  (:filling-style ,(o-formula (virtual-fillstyle)))
				  (:point-list ,(o-formula (list (virtual-x-left)
								 (virtual-y-bottom)
								 (virtual-x-center)
								 (virtual-y-top)
								 (virtual-x-right)
								 (virtual-y-bottom)
								 (virtual-x-left)
								 (virtual-y-bottom))))))))

(create-instance 'virtual-down-triangle opal:aggregadget
		 ;; :item-values = (x-center y-center x-left x-right y-top y-bottom linestyle color-fill)		 
		 (:parts `((:body ,opal:polyline
				  (:line-style ,(o-formula (virtual-linestyle))) ; ,(o-formula (when (gvl :parent :borderp) (virtual-linestyle)))
				  (:filling-style ,(o-formula (virtual-fillstyle)))
				  (:point-list ,(o-formula (list (virtual-x-center)
								 (virtual-y-bottom)
								 (virtual-x-left)
								 (virtual-y-top)
								 (virtual-x-right)
								 (virtual-y-top)
								 (virtual-x-center)
								 (virtual-y-bottom))))))))

(create-instance 'virtual-box opal:aggregadget
		 ;; :item-values = (x-center y-center x-left x-right y-top y-bottom linestyle color-fill)		 
		 (:parts `((:body ,opal:polyline
				  (:line-style ,(o-formula (virtual-linestyle))) ; ,(o-formula (when (gvl :parent :borderp) (virtual-linestyle)))
				  (:filling-style ,(o-formula (virtual-fillstyle)))
				  (:point-list ,(o-formula (list (virtual-x-left)
								 (virtual-y-bottom)
								 (virtual-x-left)
								 (virtual-y-top)
								 (virtual-x-right)
								 (virtual-y-top)
								 (virtual-x-right)
								 (virtual-y-bottom)
								 (virtual-x-left)
								 (virtual-y-bottom))))))))



(create-instance 'virtual-diamond opal:aggregadget
		 ;; :item-values = (x-center y-center x-left x-right y-top y-bottom linestyle color-fill)		 
		 (:parts `((:body ,opal:polyline
				  (:line-style ,(o-formula (virtual-linestyle))) ;  ,(o-formula (when (gvl :parent :borderp) (virtual-linestyle)))
				  (:filling-style ,(o-formula (virtual-fillstyle)))
				  (:point-list ,(o-formula (list (virtual-x-center)
								 (virtual-y-bottom)
								 (virtual-x-left)
								 (virtual-y-center)
								 (virtual-x-center)
								 (virtual-y-top)
								 (virtual-x-right)
								 (virtual-y-center)
								 (virtual-x-center)
								 (virtual-y-bottom))))))))

#|
(defvar *scatter-symbols*
  #-GARNET-V3.0
  '(:dot :box :down-triangle :up-triangle :diamond :cross :tilted-cross)
  #-GARNET+V3.0
  '(:dot)
  )
|#
(defvar *scatter-symbols*
  '(:dot :cross :box :down-triangle :up-triangle :diamond :tilted-cross)
  )

(defvar *scatter-symbols-closed-curves* '(:dot :triangle :down-triangle :up-triangle :diamond :box))
(defvar *scatter-symbols-open-curves* '(:triangle :down-triangle))

(defun scatter-symbol-to-prototype (scatter-symbol)
  (case scatter-symbol
;    (#-GARNET-V3.0 :dot #+GARNET-V3.0 t virtual-dot)
    (:dot virtual-dot)
    (:cross virtual-cross)
    ((:triangle :down-triangle) virtual-down-triangle)
    (:up-triangle virtual-up-triangle)
    (:diamond virtual-diamond)
    (:box virtual-box)
    (:tilted-cross virtual-tilted-cross)
    (t virtual-dot)))

(defun scatter-prototype-to-symbol (scatter-prototype)
  (cond 
    ((eq scatter-prototype virtual-dot) :dot)
    ((eq scatter-prototype virtual-cross) :cross)
    ((eq scatter-prototype virtual-down-triangle) :down-triangle)
    ((eq scatter-prototype virtual-up-triangle) :up-triangle)
    ((eq scatter-prototype virtual-diamond) :diamond)
    ((eq scatter-prototype virtual-box) :box)
    ((eq scatter-prototype virtual-tilted-cross) :tilted-cross)
    (t :dot)))

(defun scatter-prototype-to-namestring (scatter-prototype)
  (cond 
    ((eq scatter-prototype virtual-dot) "dot")
    ((eq scatter-prototype virtual-cross) "cross" )
    ((eq scatter-prototype virtual-down-triangle) "triangle")
    ((eq scatter-prototype virtual-up-triangle) "up-triangle" )
    ((eq scatter-prototype virtual-diamond) "diamond" )
    ((eq scatter-prototype virtual-box) "box" )
    ((eq scatter-prototype virtual-tilted-cross) "tilted-cross" )
    (t "dot")))

(create-instance 'virtual-polyline opal:polyline
		 (:point-list (o-formula (first (gvl :item-values))))
		 (:line-style (o-formula (second (gvl :item-values))))
		 (:filling-style (o-formula (third (gvl :item-values)))))

(export '(virtual-polyline
	  *scatter-symbols*
	  *scatter-symbols-closed-curves*
	  *scatter-symbols-open-curves*))