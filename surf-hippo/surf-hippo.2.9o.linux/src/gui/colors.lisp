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


;; GUI Source file: colors.lisp


(IN-PACKAGE "WINDOWS-HACK")

;;; Color and color fill stuff, also including color maps.
;;;
;;; The idea of the *COLOR-LIBRARY* and *COLOR-FILL-LIBRARY* libraries
;;; is twofold: one to save memory and time by having new colors or filling styles made only when
;;; necessary, and two to avoid the color allocation error when there are too many colors in the X
;;; display. The maximum number of colors that can eventually be allocated is determined by the
;;; variable *COLOR-LIBRARY-RESOLUTION*, which sets the resolution for each of the red blue and
;;; green components (0 to 1) of the created colors. Thus, if this variable is 0.02, there can be a
;;; total of 50 * 50 * 50 (= 125000) colors in the library. It remains to be seen what is the maximum.

;;; Another idea is the use of *COLOR-LIBRARY-MAXIMUM*, which punts the creation of new color if the
;;; number of entries in the color library reaches this level.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Variables and Parameters
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *color-library* nil)
(defvar *color-fill-library* nil)
(defvar *color-library-resolution* 0.02)
(defvar *color-library-maximum* 500)

#|
(defvar *basic-opal-colors*		; Leave out yellow since it is hard to see 
  (list opal:RED
	opal:GREEN
	opal:BLUE
	;; opal:YELLOW
	opal:ORANGE
	opal:CYAN
	opal:PURPLE
	opal:black
	opal:white))
|#

(defparameter *basic-opal-colors*	; Leave out yellow since it is hard to see 
  (list opal:black
	opal:red
	opal:green
	opal:blue
	;; opal:yellow
	opal:purple
	opal:cyan
	opal:orange))

(defparameter *basic-opal-colors-w-white* (append *basic-opal-colors* (list opal:white)))


;; Color map variables

(defvar *default-color-map* 'jet-rgb-map) ; Also 'HOT-RGB-MAP, 'SH-ORIGINAL-RGB-map
(defvar *color-map-functions* '(SH-ORIGINAL-RGB-map HOT-RGB-MAP jet-rgb-map GRAY-RGB-MAP PINK-RGB-MAP hot-cold-rgb-map))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun clear-color-libraries ()
  (loop for color in *color-library* do (opal:destroy color))
  (loop for color in *color-fill-library* do (opal:destroy color))
  (setq *color-fill-library* nil *color-library* nil))


(defun map-color-to-library-index (color library-resolution)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (type single-float library-resolution))
  (* library-resolution (round (bound-val color 1.0 0.0) library-resolution)))

(proclaim '(notinline get-color-from-library))
(defun get-color-from-library (red green blue &key (library-resolution *color-library-resolution*) colors-are-indexes)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let ((red-index (if colors-are-indexes red (map-color-to-library-index red library-resolution)))
	(blue-index (if colors-are-indexes blue (map-color-to-library-index blue library-resolution)))
	(green-index (if colors-are-indexes green (map-color-to-library-index green library-resolution)))
	(temp-error-sq 1.0)
	(min-error-sq 1.0)
	(closest-color))
    (declare (type single-float red-index blue-index green-index temp-error-sq min-error-sq))
    (loop for color in *color-library*
	  when (and (= (the sf (g-value color :red)) red-index)
		    (= (the sf (g-value color :green)) green-index)
		    (= (the sf (g-value color :blue)) blue-index))
	  do (return color)
	  else
	  when (< (setq temp-error-sq
			(+ (square (- (the sf (g-value color :red)) red-index))
			   (square (- (the sf (g-value color :green)) green-index))
			   (square (- (the sf (g-value color :blue)) blue-index))))
		  (the sf min-error-sq))
	  do (setq min-error-sq temp-error-sq closest-color color)
	  finally
	  (return
	   (if (> (length (the cons *color-library*)) (the fn *color-library-maximum*))
	     closest-color
	     (let ((color (create-instance nil opal:color (:red red-index) (:green green-index) (:blue blue-index))))
	       (push color *color-library*)
	       color))))))

(proclaim '(inline get-color-fill-from-library))
(defun get-color-fill-from-library (red green blue &key (library-resolution *color-library-resolution*) colors-are-indexes)
  (let ((red-index (if colors-are-indexes red (map-color-to-library-index red library-resolution)))
	(blue-index (if colors-are-indexes blue (map-color-to-library-index blue library-resolution)))
	(green-index (if colors-are-indexes green (map-color-to-library-index green library-resolution))))
    (loop for color-fill in *color-fill-library*
	  when (and (= (g-value color-fill :foreground-color :red)  red-index)
		    (= (g-value color-fill :foreground-color :green)  green-index)
		    (= (g-value color-fill :foreground-color :blue)  blue-index))
	  do (return color-fill)
	  finally
	  (return
	   (let ((color-fill (create-instance nil opal:filling-style
					      (:foreground-color (get-color-from-library red-index green-index blue-index
											 :library-resolution library-resolution
											 :colors-are-indexes t)))))
	     (push color-fill *color-fill-library*)
	     color-fill)))))


(defun get-shaded-color-from-library (reference-color shading-percent)
  (let ((coeff (the sf (* 0.01 shading-percent))))
    (get-color-from-library
     (+ (- 1 coeff) (* coeff (the sf (g-value reference-color :red))))
     (+ (- 1 coeff) (* coeff (the sf (g-value reference-color :green))))
     (+ (- 1 coeff) (* coeff (the sf (g-value reference-color :blue))))
     )))


;; When COLOR is NIL, assume black.
(proclaim '(notinline get-opal-color))
(defun get-opal-color (&optional color shading-percent)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((color (or color opal:black)))
    (cond ((and (not shading-percent)
		(eq 'schema (type-of color))
		(eq opal::color (car (g-value color :is-a))))
	   color)
	  ((or (and (eq 'schema (type-of color))
		    (eq opal::color (car (g-value color :is-a))))
	       (find-symbol (STRING-upcase color) 'opal))
	   (let ((opal-color
		  (if (and (eq 'schema (type-of color))
			   (eq opal::color (car (g-value color :is-a))))
		    color
		    (symbol-value (find-symbol (STRING-upcase color) 'opal)))))
	     (unless (eq opal::color (car (g-value opal-color :is-a)))
	       (setq opal-color opal:black))
	     (if shading-percent (get-shaded-color-from-library opal-color shading-percent) opal-color)))
	  (t opal:black))))

;; (proclaim '(notinline get-opal-color-fast))
(proclaim '(inline get-opal-color-fast))
(defun get-opal-color-fast (color shading-percent)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (single-float shading-percent))
  (cond ((or (and (eq 'schema (type-of color))
		  (eq opal::color (car (g-value color :is-a))))
	     (find-symbol (STRING-upcase color) 'opal))
	 (let ((opal-color
		(if (and (eq 'schema (type-of color))
			 (eq opal::color (car (g-value color :is-a))))
		    color
		    (symbol-value (find-symbol (STRING-upcase color) 'opal)))))
	   (unless (eq opal::color (car (g-value opal-color :is-a)))
	     (setq opal-color opal:black))
	   (get-shaded-color-from-library opal-color shading-percent)))
	(t opal:black)))


(proclaim '(inline get-opal-color-fast-fn))
(defun get-opal-color-fast-fn (color shading-percent)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (fixnum shading-percent))
  (cond ((or (and (eq 'schema (type-of color))
		  (eq opal::color (car (g-value color :is-a))))
	     (find-symbol (STRING-upcase color) 'opal))
	 (let ((opal-color
		(if (and (eq 'schema (type-of color))
			 (eq opal::color (car (g-value color :is-a))))
		  color
		  (symbol-value (find-symbol (STRING-upcase color) 'opal)))))
	   (unless (eq opal::color (car (g-value opal-color :is-a)))
	     (setq opal-color opal:black))
	   (get-shaded-color-from-library opal-color shading-percent)))
	(t opal:black)))


(proclaim '(inline get-opal-color-to-fill))
(defun get-opal-color-to-fill (color &optional shading-percent)
  (let ((found-color (get-opal-color color shading-percent)))
    (get-color-fill-from-library (g-value found-color :red) (g-value found-color :green) (g-value found-color :blue))))


(proclaim '(inline get-gray-fill))
(defun get-gray-fill (value)
  (get-color-fill-from-library value value value))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Color maps
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun rgb-map-values (rgb-map length)
  (let ((length (s-flt length))
	rs gs bs)
    (loop for step from 1.0 to length do
	  (multiple-value-bind (r g b)
	      (funcall rgb-map (/ step length))
	    (push r rs)
	    (push g gs)
	    (push b bs))
	  finally (return (values (reverse rs) (reverse gs) (reverse bs))))))

#|
;; RGB map functions should take a single INDEX argument from 0 to 1, and return as values (red green blue), each between 0 and 1.
;;
;; For example,

(defun foo-rgb-map (index)
  (values
   (cond ((< index 0.5) (/ index 2)			; red
	  t index))
   0				; green
   index))			; blue

|#

(defun hot-cold-rgb-map (index)
  (let ((index (- (* index 2) 1)))
    (values
     ;; Red
     (cond
      ((< index (/ -2.0 3)) (/ (+ (- index) (/ -2.0 3)) (/ 1 3.0)))
      ((< index 0.0) 0.0)
      ((< index (/ 1.0 3))
       (/ index (/ 1.0 3)))
      (t 1.0))
     ;; Green
     (cond
      ((< index (/ -2.0 3)) 1.0)
      ((< index (/ -1.0 3)) (/ (+ (- index) (/ -1.0 3)) (/ 1 3.0)))
      ((< index 0.0) 0.0)
      ((< index (/ 0.0 3)) (/ (+ (- index) (/ 0.0 3)) (/ 1 3.0)))
      ((< index (/ 1.0 3)) 0.0)
      ((< index (* 2 (/ 1.0 3)))
       (/ (- index (/ 1.0 3)) (/ 1.0 3)))
      (t 1.0))
     ;; Blue
     (cond
      ((< index (/ -1.0 3)) 1.0)
      ((< index 0) (* 3 (- index)))
      ((< index (* 2 (/ 1.0 3))) 0.0)
      (t (/ (- index (* 2 (/ 1.0 3))) (/ 1.0 3)))))))




(defun SH-ORIGINAL-RGB-map (index)
  "An ad-hoc mapping."
  (values index
	  (- 1.0 (* 2 (abs (- 0.5 index))))
	  (abs (- 1.0 index))
	  ))

(defun hot-rgb-map (index)
  "Inspired by MATLAB(tm)."
  (values (cond
	    ((< index (/ 1.0 3))
	     (/ index (/ 1.0 3)))
	    (t 1.0))
	  (cond
	    ((< index (/ 1.0 3)) 0.0)
	    ((< index (* 2 (/ 1.0 3)))
	     (/ (- index (/ 1.0 3)) (/ 1.0 3)))
	    (t 1.0))
	  (cond
	    ((< index (* 2 (/ 1.0 3))) 0.0)
	    (t (/ (- index (* 2 (/ 1.0 3))) (/ 1.0 3))))
	  ))


(defun pink-rgb-map (index)
  "Inspired by MATLAB(tm)."
  (values-list
   (mapcar #'(lambda (h g)
	       (sqrt (+ (* 2/3 g) (* 1/3 h))))
	   (multiple-value-list (hot-rgb-map index))
	   (multiple-value-list (gray-rgb-map index)))))

(defun GRAY-RGB-MAP (index)
  (values index index index))

(defun jet-rgb-map (index)
  "Inspired by MATLAB(tm)."
  (values
   (cond
    ((< index (* 3 0.125))
     0.0)
    ((< index (* 5 0.125))
     (/ (- index (* 3 0.125)) 0.25))
    ((< index (* 7 0.125))
     1.0)
    (t
     (- 1 (/ (- index (* 7 0.125)) 0.25))))

   (cond
    ((< index (* 1 0.125))
     0.0)
    ((< index (* 3 0.125))
     (/ (- index (* 1 0.125)) 0.25))
    ((< index (* 5 0.125))
     1.0)
    ((< index (* 7 0.125))
     (- 1 (/ (- index (* 5 0.125)) 0.25)))
    (t
     0.0))
   (cond
    ((< index 0.125)
     (/ (+ index 0.125) 0.25))
    ((< index (* 3 0.125))
     1.0)
    ((< index (* 5 0.125))
     (- 1 (/ (- index (* 3 0.125)) 0.25)))
    (t
     0.0))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (proclaim '(notinline get-opal-variable-color))
(proclaim '(notinline get-opal-variable-color))
(defun get-opal-variable-color (variable &key (min-variable -100.0) (max-variable 50.0) (map *default-color-map*))
  ;;  (declare (single-float min-variable max-variable variable))
  (let ((variable-index (/ (- (the sf (bound-val variable max-variable min-variable)) min-variable)
			   (- max-variable min-variable))))
    (apply 'get-color-from-library (multiple-value-list (funcall map variable-index)))))


  		     

(proclaim '(notinline get-number-fill))
(defun get-number-fill (value colorp)
;; VALUE is between 0 and 1
  (if colorp
      (get-opal-color-to-fill (get-opal-variable-color (coerce value 'single-float) :min-variable 0.0 :max-variable 1.0))
      (get-gray-fill value)))

#|
(create-instance 'red-75 (get-opal-color 'red 75))
(create-instance 'red-50 (get-opal-color 'red 50))
(create-instance 'red-25 (get-opal-color 'red 25))
(create-instance 'red-10 (get-opal-color 'red 10))
|#

(defun color-to-fill (color &optional shading-percent)
  (when color (get-opal-color-to-fill color shading-percent)))
  
(defun colors-to-fill (colors)
  (if (not (listp colors))
    (setq colors (list colors)))
  (unless (null-list-p colors)
    (if (= (length colors) 1)
      (color-to-fill (car colors))
      (let ((name ""))
	(loop for color in colors
	      for i from 0
	      when color
	      summing (g-value color :red) into red
	      and
	      summing (g-value color :blue) into blue
	      and
	      summing (g-value color :green) into green
	      and
	      do (setq name (concatenate 'string name (kr::name-for-schema color)))
	      finally
	      (return (get-color-fill-from-library (/ red i) (/ green i) (/ blue i))))))))


(defmacro symbol-to-opal-color (value)
  (case value
    (red opal:RED)
    (green opal:GREEN )
    (blue opal:BLUE )
    (yellow opal:YELLOW )
    (orange opal:ORANGE )
    (cyan opal:CYAN )
    (purple opal:PURPLE )
    (black opal:black )
    (white opal:white)))

(defun string-to-opal-color (string)
  (let ((string (string-upcase string)))
    (cond ((string= string "RED") opal:RED)
	  ((string= string "GREEN") opal:GREEN )
	  ((string= string "BLUE")  opal:BLUE )
	  ((string= string "YELLOW") opal:YELLOW )
	  ((string= string "ORANGE") opal:ORANGE )
	  ((string= string "CYAN") opal:CYAN )
	  ((string= string "PURPLE") opal:PURPLE )
	  ((string= string "BLACK") opal:black )
	  ((string= string "WHITE") opal:white))))

(defun opal-color-to-string (color)
  (cond ((equal color opal:RED)  "RED")
	((equal color opal:GREEN)  "GREEN")
	((equal color opal:BLUE)  "BLUE")
	((equal color opal:YELLOW)  "YELLOW")
	((equal color opal:ORANGE)  "ORANGE")
	((equal color opal:CYAN)  "CYAN")
	((equal color opal:PURPLE)  "PURPLE")
	((equal color opal:black)  "BLACK")
	((equal color opal:white)  "WHITE")))

;;(proclaim '(notinline color-index))
(proclaim '(inline color-index))
(defun color-index (color)
  (typecase color
    (schema (list-position (the schema color) *line-styles-opal-colors*))
    (t (or (list-position (the symbol color) (the cons *line-styles-colors*))
	   (list-position 'black (the cons *line-styles-colors*))))))






(export '(*color-library* *color-fill-library* *color-library-resolution* *color-library-maximum*
	  symbol-to-opal-color string-to-opal-color *basic-opal-colors* *basic-opal-colors-w-white*
	  color-index
	  OPAL-COLOR-TO-STRING
	  *DEFAULT-COLOR-MAP*
	  *color-map-functions*
	  SH-ORIGINAL-RGB-map
	  JET-RGB-MAP
	  hot-rgb-map
	  hot-cold-rgb-map
	  GRAY-RGB-MAP PINK-RGB-MAP
	  rgb-map-values
	  get-opal-color  GET-OPAL-variable-COLOR
	  get-opal-color-to-fill  GET-COLOR-FROM-LIBRARY
	  GET-COLOR-FILL-FROM-LIBRARY
	  *COLOR-LIBRARY-RESOLUTION*  *color-library*  *color-fill-library*
	  GET-GRAY-FILL
	  get-number-fill
	  clear-color-libraries
	  COLOR-TO-FILL
	  COLORs-TO-FILL

	  ))
	  

#|

	  red green blue yellow orange cyan purple black white ; need these for functions such as COLOR-INDEX which reference
							       ; *line-styles-opal-colors* and *line-styles-colors*
	  


	  red-75  red-50  red-25  red-10  blue-50  blue-25	  green-50  green-25  yellow-50  yellow-25
	  orange-50  orange-25	  cyan-50  cyan-25	  purple-50  purple-25	  black-50  black-25

	  red-fill-75   red-fill-50  red-fill-25  red-fill-10  blue-fill-50  blue-fill-25  green-fill-50  green-fill-25

	  yellow-fill-50 yellow-fill-25  orange-fill-50 orange-fill-25  cyan-fill-50 cyan-fill-25  purple-fill-50
	  purple-fill-25
	  black-fill-50
	  black-fill-25
|#