;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: SURF; Base: 10 -*-
;;;; this assumes that:
;;;; ~/systems/garnet/src/gadgets/scrolling-menu-loader.lisp *yes*
;;;; /home/vi/lyle/systems/garnet/src/gadgets/error-gadget-loader.lisp *not yet*
;;;; have been loaded.


#-parallel
(in-package "SURF-HIPPO"
	    :use '("COMMON-LISP-USER" "COMMON-LISP" "GARNET-GADGETS"  "LISP" "KR")
	    :nicknames '("SURF"))
#+parallel 
(in-package "*SURF")


(defvar *STANDARD-GRAPHICS-OUTPUT* t)

(defun lxx (list)
  (let ((sumx (loop for x in list
		    summing x))
	(sumxx (loop for x in list
		     summing (* x x))))
    (- sumxx (/ (* sumx sumx) (length list)))))

(defun lxy (listx listy)
  (let ((sumx (loop for x in listx
		    summing x))
	(sumy (loop for y in listy
		    summing y))
	(sumxy (loop for x in listx
		     for y in listy
		     summing (* x y))))
    (- sumxy (/ (* sumx sumy) (length listx)))))

(defun r (listx listy)
  (/ (lxy listx listy)(sqrt (* (lxx listx)(lxx listy)))))


(defun lin-reg (list-of-lists)			;x is first list, y is second list
  (let* ((x-list (car list-of-lists))
	 (y-list (cadr list-of-lists))
	 (n (length x-list))
	 (sumx (float (apply '+ x-list)))
	 (sumy (float (apply '+ y-list)))
	 (sumxy (loop for x in x-list
		      for y in y-list
		      summing (* y x)))
	 (sumxx (loop for x in x-list
		      summing (* x x)))
	 (slope (/ (- (* n sumxy) (* sumy sumx))
		   (- (* n sumxx) (* sumx sumx))))
	 (intercept (/ (- sumy (* slope sumx)) n))
	 (meanx (/ sumx n))
	 (meany (/ sumy n))
	 (r (r x-list y-list)))
    (format t "~%mx=~a, varx=~a, my=~a, vary=~a, covxy=~a~%"
	    meanx (lxx x-list) meany (lxx y-list)(lxy x-list y-list))
    (format t "Slp=~a, Intcpt= ~a, r = ~a ~%" slope intercept r)
    (list slope intercept r)))



;(defun wipeout ()                       ;Destroy all the created windows.
;  (dolist (win (garnet-debug:windows))
;    (opal:destroy win)))

(defparameter plotting-window
  (create-instance 'plotting-window
		   opal:window
		   ;;These values are in data coordinates.
		   (:y-max 0) (:y-min 0) (:y-origin 0) (:y-mag 0)
		   (:x-max 0) (:x-min 0) (:x-origin 0) (:x-mag 0)
		   (:y-axis-inc) (:x-axis-inc)
		   ; this is in pixels
		   (:label-height 0)
		   (:y-axis-label) (:x-axis-label)))

(defparameter surf-scroll-menu NIL)
(defparameter scrolling-menu-win nil)
(defparameter scrolling-menu-top-agg nil)

(defun Report-Change (gadget item-object)
  (let ((item-string (g-value item-object :item)))
    (format t "~%Clicked on string ~S in gadget ~S.~%" item-string gadget)))

(defun surf-scroll-Menu (list &key
			      (title "Surf Parameters")
			      (menu-selection-function #'Report-Change))
  (create-instance 'scrolling-menu-win inter:interactor-window
		   (:left 700)(:top 5)(:title "Surf Menu"))
  (s-value scrolling-menu-WIN
	   :aggregate
	   (create-instance 'scrolling-menu-top-agg opal:aggregate))
  (create-instance 'surf-scroll-menu scrolling-menu
		   (:left 0) (:top 0)
		   (:title title) (:items list)
		   (:height (g-value surf-scroll-menu :height))
		   (:num-visible (min 10 (length list)))
		   (:width (g-value surf-scroll-menu :width))
		   (:menu-selection-function menu-selection-function))
  (opal:add-components scrolling-menu-top-agg surf-scroll-menu)
  (opal:update scrolling-menu-WIN))

(defun make-whiz ()
  (create-instance 'my-win opal:window
		   (:left 500)
		   (:top 100)
		   (:width 1000) (:height 500)
		   (:icon-title "Lyle tester"))
  (s-value my-win :title "oh shit")
  (create-instance 'my-agg opal:aggregate)
  (s-value my-win :aggregate my-agg)
  (opal:add-component my-agg
		      (create-instance 'my-rect opal:rectangle 
				       (:left 50)(:top 50)
				       (:width
					(o-formula (round (gv my-win :width) 10)))
				       (:height
					(o-formula (round (gv my-win
							      :height) 20)))
				       (:filling-style opal:dark-gray-fill)))
  (opal:add-component my-agg
		      (create-instance 'my-line opal:line
				       (:x1 400)(:y1 500)
				       (:x2 (o-formula (+ 10 (gv my-rect :left))))
				       (:y2 (o-formula (gv my-rect :top)))))
  (opal:update my-win))

(defun whiz (delay cycles)
  (loop for j from 1 to cycles do
	(loop for i from 0 to 10 do
	      (loop for x from 0 to delay do)
	      (progn (s-value my-rect :left (* i 40))
		     (s-value my-rect :top (* i 2))
		     (opal:update my-win)))))

;;;;;;;;;;;;;;;;;;


;;; NEW-GARNET-STRING This adds an instance of an opal:text to
;;; aggegrate of win, but takes useful parameters. Note that x and y
;;; params are in window coordinates according to the window's :scale
;;; slot (e.g. microns per pixel), referenced to the center of win.
#+garnet
(defun new-garnet-string (string x y &key (win  *STANDARD-GRAPHICS-OUTPUT*)
				 character-style)
  (declare (ignore character-style))
  (let ((x-y (x-y-win x y win)))
    (opal:add-component
     (g-value win :aggregate)
     (create-instance nil opal:text
		      (:left (car x-y))
		      (:top (cadr x-y)) 
		      (:string string)))))

;;; NEW-GARNET-LINE This adds an instance of an opal:line to aggegrate
;;; of win, but takes useful parameters. Note that x1, x2, y1, y2, and
;;; thickness params are in window coordinates according to the
;;; window's :scale slot (e.g. microns per pixel), referenced to the
;;; center of win.
#+garnet
(defun new-garnet-line (x1 y1 x2 y2 &key name style
			   (stipple-percent 50)
			   (thickness 0)(filling opal:black-fill)
			   (win  *STANDARD-GRAPHICS-OUTPUT*))
  "Data args must be a single-float."
  (let* ((x-y-1 (x-y-win x1 y1 win))
	 (x-y-2 (x-y-win x2 y2 win))
	 (thickness (round thickness))
	 (line
	  (create-instance name opal:line
			   (:x1 (car x-y-1)) (:y1 (cadr x-y-1))
			   (:x2 (car x-y-2)) (:y2 (cadr x-y-2))
			   (:draw-function :or)
			   (:line-thickness (round (/ thickness (g-value win :scale))))
			   (:line-style
			    (create-instance
			     nil opal:line-style
			     (:line-thickness  (round (/ thickness (g-value win :scale))))
			     (:stipple
			      (if (eq style 'grey)
				  (create-instance
				   nil opal:bitmap
				   (:line-thickness  (round (/ thickness (g-value win :scale))))
				   (:image (opal:halftone-image stipple-percent)))))
			     (:filling-style filling))))))
    (opal:add-component (g-value win :aggregate) line )
    ))


		    
;;; NEW-GARNET-CIRCLE This adds an instance of an opal:circle to the
;;; aggregate of win, but takes useful parameters. Note that center-x
;;; and center-y params are in window coordinates according to the
;;; window's :scale slot (e.g. microns per pixel), referenced to the
;;; center of win.
#+garnet
(defun new-garnet-circle (center-x center-y radius &key filled
				   (win  *STANDARD-GRAPHICS-OUTPUT*) name)
  (let ((x-y (x-y-win center-x center-y win))
	(radius-in-pixels (round (/ radius
		   (the single-float (g-value win :scale))))))
    (opal:add-component
     (g-value win :aggregate)
     (create-instance name opal:circle
		      (:left (- (car x-y) radius-in-pixels))
		      (:top (- (cadr x-y) radius-in-pixels))
		      (:height (+ radius-in-pixels radius-in-pixels))
		      (:width (+ radius-in-pixels radius-in-pixels))
		      (:filling-style (if filled opal:black-fill))))))


;; This points up, with the arrow tip at (x,y).
(defun new-garnet-arrow (x y length &key (win  *STANDARD-GRAPHICS-OUTPUT*)
			   name (thickness 0))
  (new-garnet-line x y x (- y length)
		   :win win :name name :thickness thickness)
  (new-garnet-line x y (- x (* length .10)) (- y (* length .30))
		   :win win :name name :thickness thickness)
  (new-garnet-line x y (+ x (* length .10)) (- y (* length .30))
		   :win win :name name :thickness thickness))



(defun max-of-list (list)
  (loop for x in list maximize x))

(defun min-of-list (list)
  (loop for x in list minimize x))

(defun a-bit-more-positive (val how-much)
  (if (= val 0)
      val
      (+ val (abs (* how-much val)))))

(defun a-bit-more-negative (val how-much)
  (if (= val 0)
      val
      (- val (abs (* how-much val)))))

;;; BOUND-VAL Returns val after passing it through piece-wise linear
;;; squashing function defined by max and min.
(defun bound-val (val max min)
  (min (max val min) max))




(defun abtoc (a b c)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (loop for i from 0 to 2 do
	(loop for j from 0 to 2 do
	      (setf (aref c i j)
		    (loop for k from 0 to 2
			  sum
			  (* (the single-float (aref a i k))
			     (the single-float (aref b k j))))))))

(defun clear-3-by-3-array (array)
  (loop for i from 0 to 2 do
	(loop for j from 0 to 2 do
	      (setf (aref array i j) 0.0))))

(defun fill-3-by-3-identity (array)
  (clear-3-by-3-array array)
  (loop for i from 0 to 2 do
	(setf (aref array i i) 1.0))
  array)


(defun copy-3-by-3-array (from-array to-array)
  (loop for i from 0 to 2 do
	(loop for j from 0 to 2 do
	      (setf (aref to-array i j) 
		    (aref from-array i j)))))

(defun rotate-coords (window theta)
  (fill-3-by-3-identity (g-value window :dummy-xfrm1))
  (setf (aref (g-value window :dummy-xfrm1) 0 0) (cos theta))
  (setf (aref (g-value window :dummy-xfrm1) 1 1) (cos theta))
  (setf (aref (g-value window :dummy-xfrm1) 1 0) (- (sin theta)))
  (setf (aref (g-value window :dummy-xfrm1) 0 1) (sin theta))
  (abtoc (g-value window :current-xfrm) (g-value window :dummy-xfrm1)
	 (g-value window :dummy-xfrm2))
  (copy-3-by-3-array (g-value window :dummy-xfrm2)
		     (g-value window :current-xfrm)))


(defun shift-coords (window x-shift y-shift)
  (fill-3-by-3-identity (g-value window :dummy-xfrm1))
  (setf (aref (g-value window :dummy-xfrm1) 2 0) x-shift)
  (setf (aref (g-value window :dummy-xfrm1) 2 1) y-shift)
  (abtoc (g-value window :current-xfrm) (g-value window :dummy-xfrm1)
	 (g-value window :dummy-xfrm2))
  (copy-3-by-3-array (g-value window :dummy-xfrm2)
		     (g-value window :current-xfrm)))

;;; X-PLOT-WIN, Y-PLOT-WIN - These translate from data coordinates to plot
;;; window coordinates in such a way as to keep the data contained
;;; within a scaled rectangle that in turn is centered in the plot
;;; window. A separate function is needed for x and y since the
;;; origins of opal:windows are at the upper left hand corner. This
;;; requires a flipping of the y values.
;	 (win-max (- win-width 10))
; 	 (win-min 55)
(proclaim '(function x-plot-win (single-float KR::SCHEMA) fixnum))
(defun x-plot-win (x-dat win)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (declare (type single-float x-dat))
  (+ 55 (round (* (- (- (the fixnum (g-value win :width)) 30) 55)
		  (/ (- x-dat (the single-float (g-value win :x-min)))
		     (the single-float (g-value win :x-mag)))))))

;;;;	 (win-min (- win-height 20))
    ;;	 (win-max 30)
(proclaim '(function y-plot-win (single-float KR::SCHEMA) fixnum))
(defun y-plot-win (y-dat win)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (declare (type single-float y-dat))
  (+ (round (* (- 40 (- (the fixnum (g-value win :height)) (the fixnum (g-value win :label-height))))
		(/ (- y-dat (the single-float (g-value win :y-min)))
		   (the single-float (g-value win :y-mag))))) 
     (- (the fixnum (g-value win :height)) 40)))

;;; This macro adds a rotation of theta radians to the theta slot in
;;; the specified window, updates the cos-theta and sin-theta slots,
;;; executes the forms, and then returns the window rotation slots to
;;; the way they were.

;;; (with-graphics-rotation (graphics-window theta)
;;;          (..form 1..)
;;;          (..form 2..)
;;;             etc.
;;; )
(defmacro with-graphics-rotation ((window delta-theta) &body forms)
  `(with-graphics-rotation-internal ,window ,delta-theta #'(lambda () . ,forms)))

(defun with-graphics-rotation-internal (window delta-theta continuation)
  (let* ((window (if (eq t window) *STANDARD-GRAPHICS-OUTPUT* (eval window))))
    (rotate-coords window (coerce delta-theta 'single-float))
    (funcall continuation)
    (rotate-coords  window (coerce (- delta-theta) 'single-float))))

;(defun with-graphics-rotation-internal (window delta-theta continuation)
;  (let* ((window (if (eq t window)
;                     *STANDARD-GRAPHICS-OUTPUT* (eval window)))
;         (previous-theta (g-value window :theta)))
;    (s-value window :cos-theta (cos (+ previous-theta
;                                       delta-theta)))
;    (s-value window :sin-theta (sin (+ previous-theta
;                                       delta-theta)))
;    (s-value window :theta (+ previous-theta delta-theta))
;    (funcall continuation)
;    (s-value window :cos-theta (cos
;                                previous-theta))
;    (s-value window :sin-theta (sin previous-theta))
;    (s-value window :theta previous-theta)))



;;; This macro adds x and y shifts to the shift slots in the specified
;;; window, executes the forms, and then returns the window shift
;;; slots to the way they were.

;;; (with-graphics-translation (x-shift y-shift graphics-window)
;;;          (..form 1..)
;;;          (..form 2..)
;;;             etc.
;;; )
(defmacro with-graphics-translation ((window x-shift y-shift) &body forms)
  `(with-graphics-translation-internal ,window ,x-shift ,y-shift #'(lambda () . ,forms)))

(defun with-graphics-translation-internal (window x-shift y-shift continuation)
  (let* ((window (if (eq t window) *STANDARD-GRAPHICS-OUTPUT* (eval window))))
    (shift-coords window x-shift y-shift)
    (funcall continuation)
    (shift-coords window (- x-shift) (- y-shift))))
;
;(defun with-graphics-translation-internal (window x-shift y-shift continuation)
;  (let* ((window (if (eq t window)
;                    *STANDARD-GRAPHICS-OUTPUT*
;                    (eval window)))
;                 (previous-x-shift (g-value window :x-shift))
;         (previous-y-shift (g-value window :y-shift)))
;    (s-value window :x-shift (+ previous-x-shift x-shift))
;    (s-value window :y-shift (+ previous-y-shift y-shift))
;    (funcall continuation)
;    (s-value window :x-shift previous-x-shift)
;    (s-value window :y-shift previous-y-shift)))

;;; X-Y-WIN - This translates from data coordinates to histology
;;; window coordinates in which the origin is at the center of the window.
;;; Separate transforms are needed for x and y since the
;;; origin of an opal:window is at the upper left hand corner, thus
;;; requiring a flipping of the y values. The scale factor is in units
;;; of microns per pixel, and the x and y args are in microns
;;; (assuming that the window :scale slot is in microns per pixel).

;;; X-Y-WIN returns a list of (x y), in window coordinates,
;;; appropriately shifted and rotated according to the current shift
;;; and rotate slots in the window.

(proclaim '(function x-y-win (single-float single-float  KR::SCHEMA) cons))
;; Now uses the 3x3 transformation matrix to handle rotations and
;; shifts, but scaling is done with :scale parameter. Transformation
;; matrix is derived from Symbolics graphics transformation method.
(defun x-y-win (x y win)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (declare (type single-float x y))
  (list
   (round (+ (/ (the fixnum (g-value win :width)) 2)
	     (/ (+ (* x (the single-float (aref (g-value win :current-xfrm) 0 0)))
		   (* y (the single-float (aref (g-value win :current-xfrm) 1 0)))
		   (the single-float (aref (g-value win :current-xfrm) 2 0)))
		(the single-float (g-value win :scale)))))
   (round (- (/ (the fixnum (g-value win :height)) 2)
	     (/ (+ (* x (the single-float (aref (g-value win :current-xfrm) 0 1)))
		   (* y (the single-float (aref (g-value win :current-xfrm) 1 1)))
		   (the single-float (aref (g-value win :current-xfrm) 2 1)))
		(g-value win :scale))))))


;(defun x-y-win (x y win)
;  (declare (optimize (safety 0) (speed 3) (space 1)))
;  (declare (type single-float x y))
;  (let ((x-shifted (+ x (the single-float (g-value win :x-shift))))
;        (y-shifted (+ y (the single-float (g-value win :y-shift)))))
;    (declare (type single-float x-shifted y-shifted))
;    (list
;     (round (+ (/ (the fixnum (g-value win :width)) 2)
;               (/ (- (* x-shifted
;                        (the single-float (g-value win :cos-theta)))
;                     (* y-shifted
;                        (the single-float (g-value win :sin-theta))))
;                  (the single-float (g-value win :scale)))))
;     (round (- (/ (the fixnum (g-value win :height)) 2)
;               (/ (+ (* x-shifted
;                        (the single-float (g-value win :sin-theta)))
;                     (* y-shifted
;                        (the single-float (g-value win :cos-theta))))
;                  (g-value win :scale)))))))



;;; CLEAR-AGG Destroys all components of an aggregate, if any.
(defun clear-agg (agg)
  (if agg
    (if (g-value agg :components)
	(loop for component in (g-value agg :components) do (opal:destroy component)))))

(create-instance 'thick-dashed-line opal:line-style
		 (:line-thickness 2)
		 (:line-style :dash)
		 (:dash-pattern '(4 4)))
(create-instance 'thick-dotted-line opal:line-style
		 (:line-thickness 2)
		 (:line-style :dash)
		 (:dash-pattern '(1 1)))
(create-instance 'thick-red-line opal:line-style
		 (:line-thickness 2)
		 (:foreground-color opal:red))
(create-instance 'thick-green-line opal:line-style
		 (:line-thickness 2)
		 (:foreground-color opal:green))
(create-instance 'thick-blue-line opal:line-style
		 (:line-thickness 2)
		 (:foreground-color opal:blue))
(create-instance 'thick-yellow-line opal:line-style
		 (:line-thickness 2)
		 (:foreground-color opal:yellow))
(create-instance 'thick-purple-line opal:line-style
		 (:line-thickness 2)
		 (:foreground-color opal:purple))
(create-instance 'thick-cyan-line opal:line-style
		 (:line-thickness 2)
		 (:foreground-color opal:cyan))
(create-instance 'thick-orange-line opal:line-style
		 (:line-thickness 2)
		 (:foreground-color opal:orange))


(defparameter opal-colors
;    (list opal:thin-line opal:red-line opal:green-line opal:blue-line opal:purple-line
;	  opal:cyan-line opal:dashed-line opal:dotted-line))
    (list opal:line-2 thick-red-line thick-green-line thick-blue-line thick-purple-line
	  thick-cyan-line thick-dashed-line thick-dotted-line))



;;;; SETUP-PLOT Sets up the plotting window parameters according to the plotted data.
;;; This can accept data in one of two formats:
;; As used by PLOT-XY-DATA - 
;;  data-lists =
;; '(((x1 x1 ... x1)(y1 y1 ... y1))
;;   ((x2 x2 ... x2)(y2 y2 ... y2))
;;   ....
;;   ((xn xn ... xn)(yn yn ... yn)))
;;
;; time-list = nil
;; *********************
;; As used by PLOT-TIMED-DATA
;;  data-lists =
;; '((x1 x1 ... x1)(x2 x2 ... x2)
;;    ...
;;   (xn xn ... xn))
;;
;; time-list =
;; '(t0 t1 t2 ..)
;;
(defun setup-plot (win data-lists time-list
		       &key scale x-inc y-inc y-min y-max y-label x-max x-min
		       x-label overlay)
  (cond ((not (is-a-p win plotting-window))
	 (setq win			;If not already, make win a plotting window.
	       (create-instance win plotting-window
				(:left 50) (:top 50) (:width 600) (:height 300)
				(:position-by-hand t)
				(:title (concatenate 'string
						     "Surf-Hippo Plot - " (string win)))
				(:icon-title (concatenate 'string
							  "Surf-Hippo Plot - " (string win)))))
	 (s-value win :aggregate (create-instance nil opal:aggregate)))	 
	((not overlay)			;If no overlay, destroy old agg
	 (if (g-value win :aggregate) (opal:destroy (g-value win :aggregate)))
	 (s-value win :aggregate (create-instance nil opal:aggregate))))
  (s-value win :scale scale) (s-value win :y-axis-label y-label) (s-value win :x-axis-label x-label)
  (let ((time-lists (if time-list (list time-list) (loop for xy-data-list in data-lists
							 collect (car xy-data-list)))))
    (if (not time-list) (setq data-lists
			      (loop for xy-data-list in data-lists
				    collect (cadr xy-data-list))))
    (let* ((data-max (if y-max y-max (a-bit-more-positive-than-max data-lists 0.05)))
	   (data-min (if y-min y-min (a-bit-more-negative-than-min data-lists 0.05)))
	   (time-max (if x-max x-max (a-bit-more-positive-than-max time-lists 0.05)))
	   (time-min (if x-min x-min (a-bit-more-negative-than-min time-lists 0.05))))
      (if (= 0 data-max data-min)	;This generates a reasonable plotting scale for flat data.
	  (setq data-max 1 data-min -1))
      (s-value win :y-max (float data-max)) (s-value win :y-min (float data-min))
      (s-value win :y-mag (- (g-value win :y-max)(g-value win :y-min)))
      (s-value win :x-max (float time-max)) (s-value win :x-min (float time-min))
      (s-value win :x-mag (- (g-value win :x-max)(g-value win :x-min)))
      (s-value win :x-origin
	       (bound-val 0 (g-value win :x-max) (g-value win :x-min)))
      (s-value win :y-origin
	       (bound-val 0 (g-value win :y-max) (g-value win :y-min)))
      (s-value win :y-axis-inc (if y-inc y-inc (/ (- data-max data-min) 5.0)))
      (s-value win :x-axis-inc
	       (if x-inc x-inc (/ (- (g-value win :x-max) (g-value win :x-min)) 5.0)))
      (s-value win :label-height (* 20 (length data-lists)))
    win)))

(defun a-bit-more-positive-than-max (data-lists factor)
  (a-bit-more-positive
   (loop for data-list in data-lists maximize
	 (max-of-list data-list))
   factor))

(defun a-bit-more-negative-than-min (data-lists factor)
  (a-bit-more-negative
   (loop for data-list in data-lists minimize
	 (min-of-list data-list))
   factor))


;; DRAW-AXISES Draw X and Y axis onto plot window WIN so that data
;; will be contained in scaled rectangle which is centered in window.
;; Also adds labeled tick marks.
(defun draw-axises (p-win) 
    (let* ((plot-agg (g-value p-win :aggregate))
	   (y-origin-w (y-plot-win (float (g-value p-win :y-origin)) p-win))
	   (x-origin-w (x-plot-win (float (g-value p-win :x-origin)) p-win))
	   (y-axis-max (float (max (g-value p-win :y-origin) (g-value p-win :y-max))))
	   (y-axis-min (float (min (g-value p-win :y-origin) (g-value p-win :y-min))))
	   (x-axis-max (float (max (g-value p-win :x-origin) (g-value p-win :x-max))))
	   (x-axis-min (float (min (g-value p-win :x-origin) (g-value p-win :x-min)))))
      (declare (fixnum y-origin-w x-origin-w))
      (declare (single-float y-axis-min y-axis-max x-axis-min x-axis-max))
      (opal:add-component		;Y axis
       plot-agg				
       (create-instance nil opal:line
			(:x1 x-origin-w) (:x2 x-origin-w)
			(:y1 (y-plot-win (float y-axis-min) p-win))
			(:y2 (y-plot-win (float y-axis-max) p-win))))
      (loop for y			;Y axis labeled ticks
	    from y-axis-min to y-axis-max
	    by (g-value p-win :y-axis-inc) do
	    (opal:add-component
	     plot-agg
	     (create-instance nil opal:text
			      (:left (- x-origin-w 30))
			      (:top (- (y-plot-win y p-win) 5)) 
			      (:string (format nil "~3,3,1,3,,g" y))))
	    (opal:add-component
	     plot-agg
	     (create-instance nil opal:line
			      (:x1 (- x-origin-w 5)) (:x2 x-origin-w)
			      (:y1 (y-plot-win y p-win)) (:y2 (y-plot-win y p-win)))))
      (opal:add-component		;Label the Y axis
       plot-agg
       (create-instance nil opal:text
			(:left (- x-origin-w 50)) (:top (y-plot-win y-axis-max p-win)) 
			(:string (g-value p-win :y-axis-label))))
      (loop for x			;X axis labeled ticks
	    from x-axis-min to x-axis-max
	    by (g-value p-win :x-axis-inc) do
	    (opal:add-component
	     plot-agg
	     (create-instance nil opal:text
			      (:left (x-plot-win x p-win)) (:top (+ y-origin-w 10))
			      (:string (format nil "~3,3,1,3,,g" x))))
	    (opal:add-component
	     plot-agg
	     (create-instance nil opal:line
			      (:y1 (+ y-origin-w 5)) (:y2 y-origin-w)
			      (:x1 (x-plot-win x p-win)) (:x2 (x-plot-win x p-win)))))
      (opal:add-component		;X axis
       plot-agg	
       (create-instance nil opal:line
			(:x1 (x-plot-win x-axis-min p-win))
			(:x2 (x-plot-win x-axis-max p-win))
			(:y1 y-origin-w) (:y2 y-origin-w)
			(:line-style opal:line-1)))
      (opal:add-component		;Label the x axis
       plot-agg
       (create-instance nil opal:text
			(:left (- (x-plot-win x-axis-max p-win) 20)) (:top (+ y-origin-w 20))
			(:string (g-value p-win :x-axis-label))))))


;;; PLOT-TIMED-DATA
(defun plot-timed-data (data-lists label-list time-list 
				   &key (win `*plot-win*) (scale 0.8)(overlay nil)
				   x-inc y-inc y-min y-max (x-label "mS")(y-label "mV")
				   x-min x-max)
  (let* ((plot-win (setup-plot win data-lists time-list :scale scale :overlay overlay
			       :x-inc x-inc :y-inc y-inc :x-min x-min :x-max x-max
			       :y-min y-min :y-max y-max :y-label y-label :x-label x-label))
	 (plot-agg (g-value plot-win :aggregate)))
    (if (not overlay) (draw-axises plot-win))
    (loop for data-list in data-lists
	  for label in label-list
	  for i from 0 to (length data-lists)
	  do
	  (let ((point-list  (loop for x in time-list
				   for y in data-list
				   collect (x-plot-win x plot-win)
				   collect (y-plot-win y plot-win))))
	    (my-polyline plot-agg point-list
			 :line-style (eval (nth (mod i (length opal-colors)) opal-colors)))
					;Data Points
	    ;;(create-instance nil opal:polyline
	    ;;	      (:point-list point-list)
	    ;;      (:line-style (eval (nth (mod i (length opal-colors)) opal-colors))))
	    )
	  (opal:add-component		;Data Key
	   plot-agg
	   (create-instance nil opal:line
			    (:x1 10) (:y1 (* (1+ i) 12))
			    (:x2 30) (:y2 (* (1+ i) 12))
			    (:line-style (eval (nth (mod i (length opal-colors)) opal-colors)))))
	  (opal:add-component		;Data Label
	   plot-agg
	   (create-instance nil opal:text 
			    (:left 40) (:top (+ 5 (* i 12)))
			    (:string label))))
    (opal:raise-window plot-win)
    (opal:deiconify-window plot-win)
    (opal:update plot-win t)
    ))

;;; PLOT-XY-DATA
;; xy-data-lists =
;; '(((x1 x1 ... x1)(y1 y1 ... y1))
;;   ((x2 x2 ... x2)(y2 y2 ... y2))
;;   ....
;;   ((xn xn ... xn)(yn yn ... yn)))
(defvar *plot-win*  '*plot-win*)
(defun plot-xy-data (xy-data-lists label-list
				   &key (win *plot-win*) (scale 0.8)(overlay nil)
				   (scatter nil) (lin-reg nil)
				   x-inc y-inc y-min y-max x-min x-max
				   (x-label "mS")(y-label "mV"))
  (let* ((plot-win (setup-plot win xy-data-lists nil :scale scale :overlay overlay
			       :x-inc x-inc :y-inc y-inc
			       :y-min y-min :y-max y-max
			       :x-min x-min :x-max x-max
			       :y-label y-label :x-label x-label))
	 (plot-agg (g-value plot-win :aggregate))
	 line-style)
    (if (not overlay) (draw-axises plot-win))
    (loop for xy-data-list in xy-data-lists
	  for label in label-list
	  for i from 0 to (length xy-data-lists)
	  do
	  (setq line-style (eval (nth (mod i (length opal-colors)) opal-colors)))
	  (if scatter
	      (let ((x-cross-width
		     (* 0.02 (g-value plot-win :x-mag)))
		    (y-cross-width
		     (* 0.02 (g-value plot-win :y-mag))))
		(loop for x in (car xy-data-list)
		      for y in (cadr xy-data-list)
		      do
		      (opal:add-component ;Data Points
		       plot-agg		 
		       (create-instance nil opal:line
					(:x1 (x-plot-win (- x x-cross-width) plot-win))
					(:x2 (x-plot-win (+ x x-cross-width) plot-win))
					(:y1 (y-plot-win y plot-win))
					(:y2 (y-plot-win y plot-win))
					(:line-style line-style)))
		      (opal:add-component ;Data Points
		       plot-agg		 
		       (create-instance nil opal:line
					(:y1 (y-plot-win (- y y-cross-width) plot-win))
					(:y2 (y-plot-win (+ y y-cross-width) plot-win))
					(:x1 (x-plot-win x plot-win))
					(:x2 (x-plot-win x plot-win))
					(:line-style line-style))))
		(if lin-reg
		    (progn
		      (format t "Linear Regression Analysis for ~A~%" label)
		      (let ((lin-reg-params (lin-reg xy-data-list)))
			(opal:add-component ;Data Key
			 plot-agg
			 (create-instance nil opal:line
					  (:x1 (x-plot-win (g-value plot-win :x-min) plot-win))
					  (:x2 (x-plot-win (g-value plot-win :x-max) plot-win))
					  (:y1 (y-plot-win (+ (* (nth 0 lin-reg-params)
								 (g-value plot-win :x-min))
							      (nth 1 lin-reg-params))
							   plot-win))
					  (:y2 (y-plot-win (+ (* (nth 0 lin-reg-params)
								 (g-value plot-win :x-max))
							      (nth 1 lin-reg-params))
							   plot-win))
					  (:line-style line-style)))))))		      
	      (opal:add-component	;Data Points
	       plot-agg		 
	       (create-instance nil opal:polyline
				(:point-list (loop for x in (car xy-data-list)
						   for y in (cadr xy-data-list)
						   collect (x-plot-win x plot-win)
						   collect (y-plot-win y plot-win)))
				(:line-style line-style))))
	  (opal:add-component		;Data Key
	   plot-agg
	   (create-instance nil opal:line
			    (:x1 10) (:y1 (* (1+ i) 12))
			    (:x2 30) (:y2 (* (1+ i) 12))
			    (:line-style line-style)))
	  (opal:add-component		;Data Label
	   plot-agg
	   (create-instance nil opal:text 
			    (:left 40) (:top (+ 5 (* i 12)))
			    (:string (string label)))))
    (opal:raise-window plot-win)
    (opal:deiconify-window plot-win)
    (opal:update plot-win t)
    ))

(defvar *tau-plot-win* '*tau-plot-win*)
(defvar *inf-plot-win* '*inf-plot-win*)

(defun plot-channels (channel-names)
  (let ((particle-list 
	 (loop for name in channel-names
	       collect
	       (channel-a-part (gethash name channel-hash-table))
	       collect
	       (channel-i-part (gethash name channel-hash-table)))))
    (plot-xy-data (loop for particle in particle-list
			collect (v-inf-particle-plot-list particle))
		  (loop for particle in particle-list
			collect (particle-name particle))
		  :win *inf-plot-win*
		  :y-max 1.2 :y-min -.2 :y-label "Steady State" :x-label "mV")

    (plot-xy-data (loop for particle in particle-list
			collect (v-tau-particle-plot-list particle))
		  (loop for particle in particle-list
			collect (particle-name particle))
		  :win *tau-plot-win*
		  :y-label "Tau - mS" :x-label "mV")
    ))

;; This generates a series of polylines, each with no more than 100
;; point pairs, so that postscript doesn't choke.
(defun my-polyline (agg point-list &key (line-style  opal:line-1))
  (loop for sub-list in (chop-list point-list 200)
	do
	(opal:add-component
	 agg
	 (create-instance nil opal:polyline
			  (:point-list sub-list)
			  (:line-style line-style)))))




(defun chop-list (list chop-length)
  (let ((count 0)
	(partial-out '())
	(out '()))
    (dotimes (i (length list))
      (push (car list) partial-out)
      (setq list (cdr list)
	    count (1+ count))
      (cond ((= (+ count 2) chop-length)
	     (push (reverse partial-out) out)
	     ;; Retain last xy values so that lines are joined.
 	     (setq partial-out (list (car partial-out)(cadr partial-out))
		   count 0))))
    (if (> (length partial-out) 2) (push (reverse partial-out) out))
    (reverse out)))
