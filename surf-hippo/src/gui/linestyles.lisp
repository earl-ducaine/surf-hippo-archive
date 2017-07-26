;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: WINDOWS-HACK; Base: 10 -*-
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


;; GUI Source file: linestyles.lisp


(IN-PACKAGE "WINDOWS-HACK")

;;; Contains stuff for linestyles for PLOT-HACK system (depends on the Garnet GUI toolset from CMU).

;; Linestyles are stored in the 4 dimensional *LINE-STYLES-ARRAY* and in the list *ALL-LINE-STYLES*. The styles in *LINE-STYLES-ARRAY* are referenced
;; according to thickness (in pixels), dash pattern, color and shading, while the members of *ALL-LINE-STYLES* are named descriptively,
;; e.g. THIN-YELLOW-LINE, DASHED-LINE, DOTTED-LINE.

(defvar *all-line-style-sets* nil)
(defvar	*all-line-styles* nil)

(defvar *dash-patterns* nil)
(setq *dash-patterns* (list nil '(10 7) '(2 3) '(5 7)  '(15 15) '(7 3) '(9 4 3 4) '(2 8)))

(defvar *maximum-line-styles-thickness* 20)
  
(defvar *line-styles-dash-patterns* '(0 (10 10) (5 5) (2 2)))

(defvar *line-styles-colors* '(black red green blue orange cyan purple yellow white))

;; there must be a better way.....
(export ; *line-styles-colors*
 '(black red green blue orange cyan purple yellow white))

(defvar *line-styles-opal-colors* (loop for color in *line-styles-colors* collect (get-opal-color color)))


(defvar *line-styles-shadings* '(100.0 50.0 25.0))

(defvar *line-styles-array* (make-array (list (1+ *maximum-line-styles-thickness*)
					(length *line-styles-dash-patterns*)
					(length *line-styles-colors*)
					(length *line-styles-shadings*))))


(defun debug-*line-styles-array* (&optional print-all)
  (multiple-value-bind (thickness-dimension dash-dimension color-dimension shading-dimension)
      (values-list (array-dimensions *line-styles-array*))
    (dotimes (thickness-index thickness-dimension)
      (dotimes (dash-index dash-dimension)
	(dotimes (color-index color-dimension)
	  (dotimes (shading-index shading-dimension)
	    (let ((line-style (aref *line-styles-array* thickness-index dash-index color-index shading-index)))
	      (when (or print-all (and line-style (not (numberp line-style))))
		(format t "~A at thick ~d, dash ~d, color INDEX ~D, ~d, shade ~d~%"
			line-style
			thickness-index
			(nth dash-index *line-styles-dash-patterns*)
			color-index (nth color-index *line-styles-colors*)
			(nth shading-index *line-styles-shadings*))))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun get-dash-string (dash-pattern)
  (if dash-pattern
    (concatenate-string-list
     (loop for dash in dash-pattern
	   collect (format nil "~D" dash))
     :string-spacer "-")
    "no-dash"))

(create-instance 'thin-line opal:thin-line)
(create-instance 'thin-white-line (create-instance nil opal:line-style (:foreground-color opal::white)))
(create-instance 'dashed-line opal:line-style (:constant t) (:line-style :dash) (:dash-pattern (list 4 4)))
(create-instance 'dashed-line-1 (create-instance nil dashed-line (:constant nil)) (:dash-pattern (list 10 10)))
(create-instance 'dashed-line-2 (create-instance nil dashed-line (:constant nil)) (:dash-pattern (list 5 5)))
(create-instance 'dotted-line opal:line-style (:constant t) (:line-style :dash) (:dash-pattern (list 1 1)))
(create-instance 'thin-dotted-line-2 opal:line-style (:join-style :round) (:line-style :dash) (:dash-pattern (list 2 6)))
(create-instance 'thin-dotted-line  opal:line-style (:join-style :round) (:line-style :dash) (:dash-pattern (list 1 8)))
(create-instance 'thin-line-style opal:line-style (:line-thickness 1) (:join-style :round))
(create-instance 'thick-line-style opal:line-style (:line-thickness 2) (:join-style :round))
(create-instance 'thick-dashed-line thick-line-style (:constant t) (:line-style :dash) (:dash-pattern (list 4 4)))
(create-instance 'thick-dotted-line thick-line-style (:constant t) (:line-style :dash) (:dash-pattern (list 1 1)))
(create-instance 'very-thick-line-style opal:line-style (:join-style :round) (:line-thickness 5))
(create-instance 'very-thick-dashed-line very-thick-line-style (:constant t) (:line-style :dash) (:dash-pattern (list 4 4)))
(create-instance 'very-thick-dotted-line very-thick-line-style (:constant t) (:line-style :dash) (:dash-pattern (list 1 1)))

;; the plot keys are 40 pixels long, so make each dash pattern at most 20 pixels so that we can see at
;; least two cycles

(create-instance 'thick-dashed-line-style opal:line-style (:join-style :round) (:line-thickness 2) (:line-style :dash))
(create-instance 'faint-line opal:line-style (:constant t) (:foreground-color (get-opal-color 'black 50)) (:line-thickness 0))

(defparameter dashed-lines
  (loop for dash-pattern in *dash-patterns*
	collect
	(let ((sym (read-from-string (format nil "thin-~A-line" (get-dash-string dash-pattern)))))
	  (if (and (boundp sym) (schema-p (eval sym)))
	    (eval sym)
	    (create-instance sym thin-line-style (:line-style (if dash-pattern :dash :solid)) (:constant t) (:dash-pattern dash-pattern))))))

(push 'dashed-lines *all-line-style-sets*)

(defparameter thick-dashed-lines
  (loop for dash-line in dashed-lines
	collect
	(let ((sym (read-from-string (format nil "thick-~A-line" (get-dash-string (g-value dash-line :dash-pattern))))))
	  (if (and (boundp sym) (schema-p (eval sym)))
	    (eval sym)
	    (create-instance sym thick-line-style (:constant t) (:line-style (g-value dash-line :line-style))
			     (:dash-pattern (g-value dash-line :dash-pattern)))))))

(push 'thick-dashed-lines *all-line-style-sets*)


(defparameter varying-widths
  (loop for thickness from 1 to 10 collect
	(let ((sym (read-from-string (format nil "width-~A-line" thickness))))
	  (if (and (boundp sym) (schema-p (eval sym)))
	    (eval sym)
	    (create-instance sym (create-instance nil thin-line (:constant nil)) (:line-style :solid) (:constant t) (:line-thickness thickness))))))

(push 'varying-widths *all-line-style-sets*)

(defparameter double-varying-widths
  (loop for style in varying-widths
	for count from 0
	when (evenp count) collect style))

(push 'double-varying-widths *all-line-style-sets*)


(defparameter grey-dashed-lines
  (loop for dash-pattern in *dash-patterns*
	collect
	(let ((sym (read-from-string (format nil "grey-thin-~A-line" (get-dash-string dash-pattern)))))
	  (if (and (boundp sym) (schema-p (eval sym)))
	    (eval sym)
	    (create-instance sym (create-instance nil thin-line (:constant nil)) (:line-style (if dash-pattern :dash :solid))
			     (:constant t) (:dash-pattern dash-pattern)
			     (:foreground-color (get-opal-color 'black 50)))))))

(push 'grey-dashed-lines *all-line-style-sets*)

(defparameter thin-colors
  (nconc
   (loop for color in *line-styles-opal-colors*
	 unless (equal color opal::white)
	 collect
	 (let ((sym (read-from-string (format nil "thin-~A-line" (opal::name-for-schema color)))))
	   (if (and (boundp sym) (schema-p (eval sym))) (eval sym)
	       (create-instance sym
				(create-instance nil thin-line (:constant nil))
				(:constant t) (:foreground-color color)))))
   (list opal:dashed-line opal:dotted-line)))

(push 'thin-colors *all-line-style-sets*)

(defparameter thick-colors
  (nconc
   (loop for color in *line-styles-opal-colors*
	 unless (equal color opal::white)
	 collect
	 (let ((sym (read-from-string (format nil "thick-~A-line" (opal::name-for-schema color)))))
	   (if (and (boundp sym) (schema-p (eval sym)))
	       (eval sym)
	       (create-instance sym
				(create-instance nil thick-line-style (:constant nil))
				(:constant t) (:foreground-color color)))))
   (list thick-dashed-line thick-dotted-line)))

(push 'thick-colors *all-line-style-sets*)

(defparameter very-thick-colors
  (nconc
   (loop for color in *line-styles-opal-colors* 
	 unless (equal color opal::white)
	 collect
	 (let ((sym (read-from-string (format nil "very-thick-~A-line" (opal::name-for-schema color)))))
	   (if (and (boundp sym) (schema-p (eval sym)))
	       (eval sym)
	       (create-instance sym
				(create-instance nil very-thick-line-style (:constant nil))
				(:constant t) (:foreground-color color)))))
   (list very-thick-dashed-line very-thick-dotted-line)))

(push 'very-thick-colors *all-line-style-sets*)

  
(defparameter thin-dotted-colors
  (loop for color-line in thin-colors
	collect
	(let ((sym (read-from-string (format nil "thin-dotted-~A-line"
					     (opal::name-for-schema (g-value color-line :foreground-color))))))
	  (if (and (boundp sym) (schema-p (eval sym))) (eval sym)
	      (create-instance sym
			       (create-instance nil thin-dotted-line (:constant nil))
			       (:constant t)
			       (:foreground-color (g-value color-line :foreground-color)))))))

(push 'thin-dotted-colors *all-line-style-sets*)


(defparameter thin-dotted-2-colors
  (loop for color-line in thin-colors
	collect
	(let ((sym (read-from-string (format nil "thin-dotted-2-~A-line"
					      (opal::name-for-schema (g-value color-line :foreground-color))))))
	  (if (and (boundp sym) (schema-p (eval sym))) (eval sym)
	      (create-instance sym
			       (create-instance nil thin-dotted-line-2 (:constant nil))
			       (:constant t)
			       (:foreground-color (g-value color-line :foreground-color)))))))

(push 'thin-dotted-2-colors *all-line-style-sets*)

(defparameter thin-dashed-1-colors
  (loop for color-line in thin-colors
	collect
	(let ((sym (read-from-string (format nil "thin-dashed-1-~A-line"
					     (opal::name-for-schema (g-value color-line :foreground-color))))))
	  (if (and (boundp sym) (schema-p (eval sym)))
	      (eval sym)
	      (create-instance sym
			       (create-instance nil dashed-line-1 (:constant nil))
			       (:constant t)
			       (:foreground-color (g-value color-line :foreground-color)))))))

(push 'thin-dashed-1-colors *all-line-style-sets*)

(defparameter thin-dashed-2-colors
  (loop for color-line in thin-colors
	collect
	(let ((sym (read-from-string (format nil "thin-dashed-2-~A-line"
					     (opal::name-for-schema (g-value color-line :foreground-color))))))
	  (if (and (boundp sym) (schema-p (eval sym)))
	      (eval sym)
	      (create-instance sym
			       (create-instance nil dashed-line-2 (:constant nil))
			       (:constant t)
			       (:foreground-color (g-value color-line :foreground-color)))))))

(push 'thin-dashed-2-colors *all-line-style-sets*)

(defun create-access-line-style (color shading thickness dash)
  (create-instance nil opal:line-style
		   (:join-style :round)
		   (:line-thickness thickness)
		   (:shading shading)
		   (:line-style (if (or (not dash) (eq 0 dash)) :SOLID :dash))
		   (:dash-pattern (unless (or (not dash) (eq 0 dash)) dash))
		   (:color color)
		   (:foreground-color (get-opal-color color shading))))

(proclaim '(notinline create-access-line-style-fast))
(proclaim '(inline create-access-line-style-fast))
(defun create-access-line-style-fast (color shading thickness dash)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (fixnum thickness shading))
  (create-instance nil opal:line-style
		   (:join-style :round)
		   (:line-thickness thickness)
		   (:shading shading)
		   (:line-style (if (or (not dash) (eq 0 dash)) :SOLID :dash))
		   (:dash-pattern (unless (or (not dash) (eq 0 dash)) dash))
		   (:color color)
		   (:foreground-color (get-opal-color-fast-fn color shading))
		   ))


(defun access-*line-styles-array* (thickness-arg color &optional (shading 100.0) dash thickness-arg-is-a-fix)
  (declare (optimize (safety 1) (speed 3) (space 1)))
  (let* ((thickness (if thickness-arg-is-a-fix thickness-arg (round thickness-arg)))
	 (thickness-index (max 0 (min (the fn thickness) (the fn *maximum-line-styles-thickness*))))
	 (dash-index
	  (if dash
	      (or (position dash (the cons *line-styles-dash-patterns*) :test 'equal)
		  (if (numberp dash) (min (max (the fn dash) 0) (1- (length (the cons *line-styles-dash-patterns*))))
		      (position 0 (the cons *line-styles-dash-patterns*) :test 'equal)))
	      0))
	 (color-index
	  (typecase color
	    (schema			;(position 'black *line-styles-colors* :test 'equal)
	     (position color *line-styles-opal-colors* :test 'equal) )
	    (t
	     (or (position color (the cons *line-styles-colors*) :test 'equal)
		 (position 'black (the cons *line-styles-colors*) :test 'equal)))))
	 (shading (unless (or (not shading) (= 100.0 shading))
		    (the sf (* 100.0 (the sf (expt 2.0 (round (the sf (log (the sf (/ shading 100.0)) 2)))))))))
	 (shading-index
	  (if (or (not shading) (= 100.0 shading)) 0	  
	      (or (position shading *line-styles-shadings*) (position 100.0 *line-styles-shadings*)))))
    (declare (fixnum thickness-index dash-index ; color-index
		     shading-index thickness))
    (if (and color-index (= thickness thickness-index))
	(let ((style (aref (the (simple-array * (*)) *line-styles-array*) thickness-index dash-index (the fn color-index) shading-index)))
	  (when (numberp style)
	    (setq style (create-access-line-style color shading thickness dash))
	    (setf (aref (the (simple-array * (*)) *line-styles-array*) thickness-index dash-index (the fn color-index) shading-index) style))
	  style)
	(create-access-line-style color shading thickness dash))))

(defun access-*line-styles-array*-optimized (thickness color &optional (shading 100.0) dash)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (fixnum thickness)
	   (single-float shading))
  (let* ((thickness-index (max 0 (min (the fn thickness) (the fn *maximum-line-styles-thickness*))))
	 (dash-index
	  (if dash
	      (or (position dash (the cons *line-styles-dash-patterns*) :test 'equal)
		  (if (numberp dash) (min (max (the fn dash) 0) (1- (length (the cons *line-styles-dash-patterns*))))
		      (position 0 (the cons *line-styles-dash-patterns*) :test 'equal)))
	      0))
	 (color-index
	  (typecase color
	    (schema			;(position 'black *line-styles-colors* :test 'equal)
	     (position color (the cons *line-styles-opal-colors*) :test 'equal) )
	    (t
	     (or (position color (the cons *line-styles-colors*) :test 'equal)
		 (position 'black (the cons *line-styles-colors*) :test 'equal)))))
	 (shading (unless (or (not shading) (= 100.0 shading))
		    (the sf (* 100.0 (the sf (expt 2.0 (round (the sf (log (the sf (/ shading 100.0)) 2)))))))))
	 (shading-index
	  (if (or (not shading) (= 100.0 shading)) 0	  
	      (or (position shading (the cons *line-styles-shadings*) :test '=)
		  (position 100.0 (the cons *line-styles-shadings*) :test '=)))))
    (declare (fixnum thickness-index dash-index ; color-index
		     shading-index thickness))
    (if (and color-index (= thickness thickness-index))
	(let ((style (aref (the (simple-array * (*)) *line-styles-array*) thickness-index dash-index (the fn color-index) shading-index)))
	  (when (numberp style)
	    (setq style (create-access-line-style color shading thickness dash))
	    (setf (aref (the (simple-array * (*)) *line-styles-array*) thickness-index dash-index (the fn color-index) shading-index) style))
	  style)
	(create-access-line-style color shading thickness dash))))



(proclaim '(inline access-*line-styles-array*-fast))
(defun access-*line-styles-array*-fast (thickness color shading-arg color-index)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (values schema)
	   (fixnum thickness)
	   (single-float shading-arg))
  (let* ((thickness-index (fixnum-max 0 (fixnum-min (the fn thickness) (the fn *maximum-line-styles-thickness*))))
	 (color-index (or color-index (color-index color)))
	 (shading-arg-fn (round shading-arg))
	 (shading-index 
	  (if (= 100.0 shading-arg) 0 (or (list-position-single-float shading-arg (the cons *line-styles-shadings*)) 0))))
    (declare (fixnum shading-arg-fn thickness-index shading-index))
    (if (and color-index (= thickness thickness-index))
	(let ((style (aref (the (simple-array (or fixnum schema) (* * * *)) *line-styles-array*) thickness-index 0 color-index shading-index)))
	  (when (numberp style)
	    (setq style (create-access-line-style-fast color shading-arg-fn thickness nil))
	    (setf (aref (the (simple-array (or fixnum schema) (* * * *)) *line-styles-array*)
			thickness-index 0 (the fn color-index) shading-index) style))
	  style)
	(create-access-line-style-fast color shading-arg-fn thickness nil))))

			  


    
     
	
	
(defun basic-line-style-access (thickness-index color-index shading-index)
  (declare (optimize (safety 1) (speed 3) (space 1))
	   (values schema)
	   (fixnum thickness-index
		   shading-index color-index))
  (aref (the (simple-array (or fixnum schema) (* * * *)) *line-styles-array*) thickness-index 0 color-index shading-index))
    
	

#|	    
(defun colored-line-style (color &optional thickness dash)
  (cond
    ((or (eql color opal:red)(eql color 'red))
     (case dash
       (1 thin-dashed-1-RED-line)
       (2 thin-dashed-2-RED-line)
       (3 thin-dotted-RED-line)
       (4 thin-dotted-2-RED-line)
       (t (case thickness
	    (5 very-thick-RED-line)
	    (2 thick-RED-line)
	    (t opal:RED-line)))))
    ((or (eql color opal:green)(eql color 'green))
     (case dash
       (1 thin-dashed-1-GREEN-line)
       (2 thin-dashed-2-GREEN-line)
       (3 thin-dotted-GREEN-line)
       (4 thin-dotted-2-GREEN-line)
       (t (case thickness
	    (5 very-thick-GREEN-line)
	    (2 thick-GREEN-line)
	    (t opal:GREEN-line)))))
    ((or (eql color opal:blue)(eql color 'blue))
     (case dash
       (1 thin-dashed-1-BLUE-line)
       (2 thin-dashed-2-BLUE-line)
       (3 thin-dotted-BLUE-line)
       (4 thin-dotted-2-BLUE-line)
       (t (case thickness
	    (5 very-thick-BLUE-line)
	    (2 thick-BLUE-line)
	    (t opal:BLUE-line)))))
    ((or (eql color opal:yellow)(eql color 'yellow))
     (case dash
       (1 thin-dashed-1-YELLOW-line)
       (2 thin-dashed-2-YELLOW-line)
       (3 thin-dotted-YELLOW-line)
       (4 thin-dotted-2-YELLOW-line)
       (t (case thickness
	    (5 very-thick-YELLOW-line)
	    (2 thick-YELLOW-line)
	    (t opal:YELLOW-line)))))
    ((or (eql color opal:orange)(eql color 'orange))
     (case dash
       (1 thin-dashed-1-ORANGE-line)
       (2 thin-dashed-2-ORANGE-line)
       (3 thin-dotted-ORANGE-line)
       (4 thin-dotted-2-ORANGE-line)
       (t (case thickness
	    (5 very-thick-ORANGE-line)
	    (2 thick-ORANGE-line)
	    (t opal:ORANGE-line)))))
    ((or (eql color opal:cyan)(eql color 'cyan))
     (case dash
       (1 thin-dashed-1-CYAN-line)
       (2 thin-dashed-2-CYAN-line)
       (3 thin-dotted-CYAN-line)
       (4 thin-dotted-2-CYAN-line)
       (t (case thickness
	    (5 very-thick-CYAN-line)
	    (2 thick-CYAN-line)
	    (t opal:CYAN-line)))))
    ((or (eql color opal:purple)(eql color 'purple))
     (case dash
       (1 thin-dashed-1-PURPLE-line)
       (2 thin-dashed-2-PURPLE-line)
       (3 thin-dotted-PURPLE-line)
       (4 thin-dotted-2-PURPLE-line)
       (t (case thickness
	    (5 very-thick-PURPLE-line)
	    (2 thick-PURPLE-line)
	    (t opal:PURPLE-line)))))
    (t (case dash
	 (1 dashed-line-1)
	 (2 dashed-line-2)
	 (3 thin-dotted-line)
	 (4 thin-dotted-line-2)
	 (t (case thickness
	      (5  very-thick-line-style)
	      (2 thick-black-line)
	      (t thin-line)))))))
|#


(defun line-style-family-menu (&optional line-style-family-symbol length)
  (let ((dummy1 line-style-family-symbol))
    (choose-variable-values
     '((dummy1 "Choose line style family:"
	:choose (:varying-widths
		   :very-thick-colors
		   :thick-colors
		   :thin-colors
		   :thick-dashed
		   :dashed
		   :thin-dotted-colors
		   :thin-dashed-colors
		   :thin-dashed-2-colors))))
    (get-line-styles dummy1 length)))


(defun get-line-styles (line-style-family-symbol &optional length)
  (case line-style-family-symbol
    (:double-varying-widths (if length
				(loop for count from 1 to length collect (nth (1- count) double-varying-widths))
				double-varying-widths))
    (:varying-widths (if length
			 (loop for count from 1 to length collect (nth (1- count) varying-widths))
			 varying-widths))
    ((:very-thick-colors :very-thick-lines) thick-colors)
    ((:thick-colors :thick-lines) thick-colors)
    ((:thin-colors :thin-lines) thin-colors)
    ((:thick-dashed :thick-dashed-lines) thick-dashed-lines)
    ((:dashed :thin-dashed :thin-dashed-lines :dashed-lines) dashed-lines)
    ((:thin-dotted :thin-dotted-colors) thin-dotted-colors)
    ((:thin-dashed-colors :thin-dashed-1 :thin-dashed-1-colors) thin-dashed-1-colors)
    ((:thin-dashed-2 :thin-dashed-2-colors) thin-dashed-2-colors)))



(defun line-style-menu (&key (default-style thin-line)
			     (label "Choose a line style")
			     text
			     (choose-from-components t)
			     (choose-from-classes t)
			     (do-all-at-once t)
			     (only-one-choice t)
			     (style-options *all-line-style-sets*))
  (let ((dummy1 (cond ((and choose-from-components choose-from-classes) nil)
		      (choose-from-components :components)
		      (choose-from-classes :classes)))
	(default-style (or default-style opal:thin-line)))
    (unless dummy1
      (choose-variable-values
       '((dummy1 "Choose line style from:" :choose (:classes :components)))
       :label label :text text))
    (let ((result
	   (case dummy1
	     (:classes
	      (choose-list-values-from-keys
	       (delete-duplicates
		(loop for style-family in (coerce-to-list style-options)
		      nconcing (loop for style in (eval style-family) collect (list (opal::name-for-schema style) style)))
		:test 'equal)
	       (list default-style)
	       :do-all-at-once do-all-at-once :only-one-choice only-one-choice :label label :text text))
	     (:components
	      (line-styles-from-components-menu :default-style default-style :label label :text text)))))
      (if (and only-one-choice (consp result)) (car result) result))))


(defun line-styles-from-components-menu (&key default-style (label "Menu For Line Style Components") text)
  (let* ((default-style (or default-style (if (= 0 (aref *line-styles-array* 0 0 0 0)) thin-dotted-line (aref *line-styles-array* 0 0 0 0))))
	 (dummy1 (princ-to-string (g-value default-style :dash-pattern)))
	 (dummy2 (g-value default-style :line-thickness))
	 (dummy3 (princ-to-string (or (g-value default-style :shading) 100.0)))
	 (dummy4 (princ-to-string (g-value default-style :color))))
    (choose-variable-values
     `((dummy1 "Choose a dash pattern (0 is solid):" :choose ,(mapcar 'princ-to-string  *line-styles-dash-patterns*) :rank-margin 4)
       (dummy2 "Thickness (0-20)" :integer)
       (dummy3 "Shading (percent)" :choose ,(mapcar 'princ-to-string *line-styles-shadings*) :rank-margin 6)
       (dummy4 "Color:" :choose ,(mapcar 'princ-to-string *line-styles-colors*) :rank-margin 5))
     :text text :label label)
    (access-*line-styles-array* dummy2 (read-from-string dummy4) (read-from-string dummy3) (read-from-string dummy1))))

(defun thick-line-style-menu (&key (default-style thick-line-style)
				   (label "Choose a thick line style")
				   (do-all-at-once t)
				   (only-one-choice t))
  (line-style-menu :default-style default-style
		   :label label
		   :do-all-at-once do-all-at-once
		   :only-one-choice only-one-choice
		   :style-options (list thick-colors thick-dashed-lines)))
	       


(loop for list in (list varying-widths
			double-varying-widths
			dashed-lines grey-dashed-lines
			thick-dashed-lines thin-colors thick-colors very-thick-colors
			thin-dotted-colors thin-dotted-2-colors thin-dashed-1-colors thin-dashed-2-colors)
      do (loop for style in list do (export (read-from-string (opal::name-for-schema style)))))


(setq *all-line-styles* (loop for set in *all-line-style-sets* nconc (copy-list (symbol-value set))))


(export '(get-line-styles 	  LINE-STYLE-FAMILY-MENU
				  debug-*line-styles-array*
	  thin-colors thick-dashed-lines dashed-lines thick-colors very-thick-colors
	  grey-dashed-lines
	  varying-widths
	  DOUBLE-VARYING-WIDTHS
	  thin-line
	  THIN-WHITE-LINE
	  thin-dashed-1-red-line thin-dashed-1-green-line thin-dashed-1-blue-line thin-dashed-1-yellow-line
	  thin-dashed-1-purple-line thin-dashed-1-cyan-line thin-dashed-1-orange-line

	  thin-dashed-2-red-line thin-dashed-2-green-line thin-dashed-2-blue-line thin-dashed-2-yellow-line
	  thin-dashed-2-purple-line thin-dashed-2-cyan-line thin-dashed-2-orange-line

	  THIN-DOTTED-cyan-line THIN-DOTTED-red-line THIN-DOTTED-green-line THIN-DOTTED-blue-line THIN-DOTTED-purple-line

	  THIN-DOTTED-2-RED-LINE THIN-DOTTED-2-GREEN-LINE THIN-DOTTED-2-BLUE-LINE THIN-DOTTED-2-YELLOW-LINE
	  THIN-DOTTED-2-PURPLE-LINE THIN-DOTTED-2-CYAN-LINE THIN-DOTTED-2-ORANGE-LINE
	  
	  thin-dotted-line  thin-dashed-1-colors thin-dashed-2-colors thin-dotted-colors thin-dotted-line-2
	  THIN-DOTTED-2-COLORS
	  
	  faint-line COLORED-LINE-STYLE
	  line-style-menu
	  thick-line-style-menu

	  *maximum-line-styles-thickness* *line-styles-dash-patterns*
	  *line-styles-colors*
	  *line-styles-shadings*

	  *ALL-LINE-STYLE-SETS*
	  *all-line-styles*

	  *line-styles-opal-colors*	  *LINE-STYLES-ARRAY*
	  ACCESS-*LINE-STYLES-ARRAY*
	  ACCESS-*LINE-STYLES-ARRAY*-FAST
	  create-access-line-style
	  

	  ))
