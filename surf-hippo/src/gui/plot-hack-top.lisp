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


;; GUI Source file: plot-hack-top.lisp

;;; This contains the more user-oriented plotting routines.

(in-package "SON-OF-PLOT-HACK")

(defvar *global-plot-comment* nil "When set to a string, this comment is added to the window produced by PLOT-XY-DATA, PLOT-TIMED-DATA, PLOT-POINTS,
PLOT-SCATTER, PLOT-HISTOGRAM, PLOT-HISTOGRAM-LIST, PLOT-POLAR-DATA, PLOT-POLAR-SIMPLE-ARRAY and PLOT-POLAR-VECTORS, at the position given by
*GLOBAL-PLOT-COMMENT-POSITION* [default *DEFAULT-COMMENT-POSITION*]. If this position is the same as the comment position argument of these plotting
functions, then any comment supplied to the plotting function is added to the global plot comment.")

(defvar *global-plot-comment-position* nil)

(defun add-global-plot-comment (win)
  (add-comment win *global-plot-comment* :position *global-plot-comment-position*))


(defun add-local-and-global-comment (comment comment-position win)
  (if (and comment (eq (or *global-plot-comment-position* *default-comment-position*) comment-position))
    (let ((*global-plot-comment* (concatenate 'string *global-plot-comment* (format nil "~%") comment)))
      (add-global-plot-comment win))
    (progn (when comment (add-comment win comment :position comment-position))
	   (add-global-plot-comment win))))

(defvar *default-plot-grid-p* nil "When T functions such as PLOT-TIMED-DATA will draw a grid by default.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  MAJOR PLOTTING FUNCTIONS
;;
;; PLOT-XY-DATA, PLOT-TIMED-DATA, PLOT-POINTS, PLOT-SCATTER, PLOT-HISTOGRAM, PLOT-HISTOGRAM-LIST,
;; PLOT-POLAR-DATA, PLOT-POLAR-SIMPLE-ARRAY, PLOT-POLAR-VECTORS 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
;; PLOT-XY-DATA
;;
;; XY-DATA-LISTS =
;; '(((x1 x1 ... x1)(y1 y1 ... y1))
;;   ((x2 x2 ... x2)(y2 y2 ... y2))
;;   ....
;;   ((xn xn ... xn)(yn yn ... yn)))
;;
;; or, if just one trace,
;;
;; XY-DATA-LISTS =
;; '((x x ... x)(y y ... y))

(defun plot-xy-data (xy-data-lists &optional label-list
				   &key  (title "XY Data") win
				   width height left top ; The window dimensions and position vis-a-vis the screen, in pixels.
				   ; (scale 0.8)
				   data-type

				   prompt-for-overlay (overlay *overlay-all-plots*) (ACCOMODATE-all-overlays *accomodate-all-overlays*)
				   (preserve-plot-layout *preserve-plot-layout*) preserve-window-attributes preserve-window-dimensions
				   (update t) (resurrect t) (visible t)
				   (plot-point-skip nil plot-point-skip-supplied-p)
				   
				   (CANONIC-LABEL "DATA") (x-label "") (y-label "")
				   x-label-horizontal-position (x-label-vertical-position :below)
				   invert-y-axis-label y-label-vertical-position (y-label-horizontal-position :left) 

				   x-are-fns y-are-fns x-axis-number-coefficient y-axis-number-coefficient
				   (x-axis-value-prefix nil x-axis-value-prefix-supplied-p) (x-axis-value-suffix nil x-axis-value-suffix-supplied-p)
				   (y-axis-value-prefix nil y-axis-value-prefix-supplied-p) (y-axis-value-suffix nil y-axis-value-suffix-supplied-p)
				   
				   
				   x-min x-max x-inc x-origin y-min y-max y-inc y-origin x-log y-log log-base
				   axes-type ; (:standard :simple :none)
				   x-axis-root y-axis-root (x-axis-p t) (y-axis-p t)
				   x-scale-l% x-scale-t% y-scale-t% ; for positioning simple axes
				   (simple-axis-x-value-p t) (simple-axis-y-value-p t)
				   consider-y-axis-visible-limit y-axis-visible-max y-axis-visible-min
				   consider-x-axis-visible-limit x-axis-visible-max x-axis-visible-min
				   
				   linear-regression odd-quadrant-diagonal even-quadrant-diagonal
				   
				   (x-origin-tick nil) (y-origin-tick t)
				   reference-ticks-to-origin (include-x-tick-at-0 :follow-window) (include-y-tick-at-0 :follow-window)

				   line-styles use-same-line-style
				   (upper-right-hand-comment "") comment (comment-position *default-comment-position*)
				   
				   UPDATE-FIXED-GAP-PARAMETERS
				   use-fixed-top-gap (fixed-top-gap 0) use-fixed-bottom-gap (fixed-bottom-gap 0)
				   use-fixed-right-gap (fixed-right-gap 0) use-fixed-left-gap (fixed-left-gap 0)
				   
				   (draw-grid *default-plot-grid-p*)
				   (label-traces *label-plot-traces*)
				   (x-data-offset 0.0) (y-data-offset 0.0) (x-trace-offset 0.0) (y-trace-offset 0.0)
				   fix-to-unity-mag-if-so session-name

				   (connect-data-points t) scatter scatter-symbol scatter-symbol-fill
				   scatter-width-heights (scatter-symbol-borderp t) (fill-scatter t)
				   (x-symbol-width *default-scatter-size*) (y-symbol-width *default-scatter-size*)

				   polar (polar-circles-p t) connect-ends
				   waterfall label-waterfall wf-skirt auto-wf-setup (waterfall-trace-label-skip 0) (waterfall-label-offset 0.0)
				   )
  (declare (ignore preserve-window-attributes))
  (when xy-data-lists
    (let ((plot-type (cond (waterfall :waterfall)
			   (polar :polar)
			   (t :xy))))
      (when polar
	(when win (setq polar-circles-p (g-value win :polar-circles-p)))
	(when (and y-inc x-inc) (setq x-inc (max x-inc y-inc) y-inc (max x-inc y-inc)))
	(setq width (max width height) height (max width height)
	      x-origin-tick nil y-origin-tick nil
	      x-label ""
	      x-max (if (and x-max y-max) (max x-max y-max) (or y-max x-max)))
	(unless x-max (error-message "Need to specify x-max or y-max for polar plot"))
	(setq y-max x-max y-min (* -1 x-max) x-min (* -1 x-max)))
      (setq win (get-plot-window plot-type data-type overlay
				 :preserve-window-dimensions preserve-window-dimensions
				 :default-win win :width width :height height :preserve-plot-layout preserve-plot-layout
				 :accomodate-all-overlays accomodate-all-overlays :prompt-for-overlay prompt-for-overlay
				 :left left :top top :name title :session-name session-name))
      (when win
	(when plot-point-skip-supplied-p (s-value win :plot-point-skip plot-point-skip))
	(s-value win :reference-ticks-to-origin reference-ticks-to-origin)
	(when x-axis-number-coefficient (s-value win :x-axis-number-coefficient x-axis-number-coefficient))
	(when y-axis-number-coefficient (s-value win :y-axis-number-coefficient y-axis-number-coefficient))
	(when x-axis-value-prefix-supplied-p (s-value win :x-axis-value-prefix x-axis-value-prefix))
	(when x-axis-value-suffix-supplied-p (s-value win :x-axis-value-suffix x-axis-value-suffix))
	(when y-axis-value-prefix-supplied-p (s-value win :y-axis-value-prefix y-axis-value-prefix))
	(when y-axis-value-suffix-supplied-p (s-value win :y-axis-value-suffix y-axis-value-suffix))
	(s-value win :draw-grid draw-grid)
	(when axes-type (s-value win :axes-type axes-type))
	(s-value win :use-same-line-style (or (g-value win :waterfall) use-same-line-style))
	(s-value win :label-waterfall label-waterfall)
	(s-value win :polar-circles-p (and  polar polar-circles-p))

	(s-value win :scatter (or (g-value win :scatter) scatter))
	(when scatter-symbol (s-value win :scatter-symbol scatter-symbol))
	(when scatter-symbol-fill (s-value win :scatter-symbol-fill scatter-symbol-fill))
	(when scatter-width-heights (s-value win :scatter-width-heights scatter-width-heights))
	(s-value win :scatter-symbol-borderp scatter-symbol-borderp) (s-value win :fill-scatter fill-scatter)
	(s-value win :x-symbol-width x-symbol-width) (s-value win :y-symbol-width y-symbol-width)
	
	(s-value win :consider-y-axis-visible-limit consider-y-axis-visible-limit)
	(s-value win :y-axis-visible-min y-axis-visible-min) (s-value win :y-axis-visible-max y-axis-visible-max)
	(s-value win :consider-x-axis-visible-limit consider-x-axis-visible-limit)
	(s-value win :x-axis-visible-min x-axis-visible-min) (s-value win :x-axis-visible-max x-axis-visible-max)

	(s-value win :waterfall-trace-label-skip waterfall-trace-label-skip)
	(s-value win :waterfall-label-offset waterfall-label-offset)
	(when UPDATE-FIXED-GAP-PARAMETERS
	  (UPDATE-FIXED-GAP-PARAMETERS win use-fixed-top-gap fixed-top-gap use-fixed-bottom-gap fixed-bottom-gap
				       use-fixed-right-gap fixed-right-gap use-fixed-left-gap fixed-left-gap))
	(when axes-type (s-value win :axes-type axes-type))
	(when x-scale-l% (s-value win :x-scale-l% x-scale-l%)) (when x-scale-t% (s-value win :x-scale-t% x-scale-t%))
	(when y-scale-t% (s-value win :y-scale-t% y-scale-t%))
	(s-value win :simple-axis-x-value-p simple-axis-x-value-p)
	(s-value win :simple-axis-y-value-p simple-axis-y-value-p)
	(s-value win :log-base (when (numberp log-base) (s-flt log-base)))
	(s-value win :x-axis-p x-axis-p)
	(s-value win :y-axis-p y-axis-p)
	(add-temp-comment win upper-right-hand-comment)
	(add-local-and-global-comment comment comment-position win)
	(when y-label-horizontal-position (s-value win :y-label-horizontal-position y-label-horizontal-position))
	(when x-label-horizontal-position (s-value win :x-label-horizontal-position x-label-horizontal-position))
	(s-value win :x-lists (parsed-xory-lists
			       (if (consp (caar xy-data-lists))
				 (list (loop for xy-data-list in xy-data-lists collect (car xy-data-list)))
				 (list (list (car xy-data-lists))))))
	(s-value win :y-lists (parsed-xory-lists 
			       (if (consp (caar xy-data-lists))
				 (list (loop for xy-data-list in xy-data-lists collect (cadr xy-data-list)))
				 (list (list (cadr xy-data-lists))))))
	(s-value win :scatter (or (g-value win :scatter) scatter))
	
	(let ((num-curves-per-group (length (car (g-value win :y-lists)))))
	  (unless (= (length (or label-list (g-value win :label-list))) num-curves-per-group)
	    (setq label-list (get-canonic-labels num-curves-per-group canonic-label)))
	  (clear-up-label-list win (or label-list (g-value win :label-list)))

	  (when scatter-symbol (s-value win :scatter-symbol scatter-symbol))
	  (s-value win :x-symbol-width x-symbol-width) (s-value win :y-symbol-width y-symbol-width)

	  (s-value win :consider-y-axis-visible-limit consider-y-axis-visible-limit)
	  (s-value win :y-axis-visible-min y-axis-visible-min) (s-value win :y-axis-visible-max y-axis-visible-max)
	  (s-value win :consider-x-axis-visible-limit consider-x-axis-visible-limit)
	  (s-value win :x-axis-visible-min x-axis-visible-min) (s-value win :x-axis-visible-max x-axis-visible-max)
	  
	  (s-value win :absolute-value-ticks polar)
	  (when  (and (not (g-value win :preserve-plot-layout))
		      (or (not (g-value win :overlay))
			  (and (g-value win :accomodate-all-overlays) (g-value win :overlay))))
	    (setup-plot win :width width :height height
					; :scale scale
			:auto-wf-setup auto-wf-setup :wf-skirt wf-skirt :consider-labels (and label-traces (not waterfall)) ; (g-value win :label-list)
			:x-are-fns x-are-fns :y-are-fns y-are-fns :fix-to-unity-mag-if-so fix-to-unity-mag-if-so
			:x-inc x-inc :x-min-spec x-min :x-max-spec x-max :x-log x-log
			:x-label x-label :x-label-horizontal-position x-label-horizontal-position :x-label-vertical-position x-label-vertical-position 
			:y-label y-label :y-label-horizontal-position y-label-horizontal-position :y-label-vertical-position y-label-vertical-position 
			:invert-y-axis-label invert-y-axis-label			
			:y-inc y-inc :y-min-spec y-min :y-max-spec y-max :y-log y-log
			:x-origin-tick x-origin-tick :y-origin-tick y-origin-tick
			:x-origin x-origin :y-origin y-origin :x-axis-root x-axis-root :y-axis-root y-axis-root
			:x-trace-offset x-trace-offset :x-data-offset (if waterfall (g-value win :waterfall-base-x-offset) x-data-offset)
			:y-trace-offset y-trace-offset :y-data-offset (if waterfall (g-value win :waterfall-base-y-offset) y-data-offset)))
	  (s-value win :connect-data-points connect-data-points)

	  (let* ((plot-agg (get-plot-agg win 'data-plot (not (g-value win :overlay))))
		 (xy-data-lists (if (consp (caar xy-data-lists)) xy-data-lists (list xy-data-lists)))
		 (line-styles (or line-styles (get-line-styles (g-value win :plot-line-style) (unless use-same-line-style (length xy-data-lists)))))
		 (relevant-labels (relevant-labels win num-curves-per-group))
		 (same-line-style (when use-same-line-style (car line-styles))))
					;	    (when (g-value win :scatter) (resurrect-opal-win win :visible visible))
	    (loop for xy-data-list in xy-data-lists
		  for curve-num from 0
		  ; for label in relevant-labels
		  do (let ((scatter-symbol (get-scatter-symbol win curve-num))
			   (label (nth (round curve-num) relevant-labels))
			   (line-style (or same-line-style (nth (mod curve-num (length line-styles)) line-styles))))
		       (when scatter
			 (add-scatter-points-to-plot-from-xy-data-list xy-data-list plot-agg win line-style scatter-symbol :data-points curve-num))
		       (when connect-data-points
			 (add-polyline-to-plot plot-agg win (car xy-data-list) (cadr xy-data-list) line-style :connect-ends (and polar connect-ends)))
		       (when linear-regression
			 (s-value win :linear-regression t)
			 (multiple-value-bind (xfrmed-time-seq xfrmed-data-list)
			     (GET-XFRMED-POINT-LIST (nth 0 xy-data-list) (nth 1 xy-data-list) win)
			   (multiple-value-bind (slope intercept r)
			       (lin-reg (list xfrmed-time-seq xfrmed-data-list))
			     (add-linear-regression win slope intercept r line-style :label label))))))
	    (when (g-value win :label-traces) (label-traces plot-agg (length xy-data-lists) line-styles relevant-labels)))))
      (cond-every (odd-quadrant-diagonal (mark-plot-odd-quadrant-diagonal win))
		  (even-quadrant-diagonal (mark-plot-even-quadrant-diagonal win)))
      (plot-windows-finishing win (cond (resurrect :resurrect)
					(update :update))))))


;; scatter-lists (((x1...)(y1....)) ((x2...)(y2....)) ...)

(defun scatter-statistics-comment (scatter-lists labels win &key (position :upper-right))
  (add-comment win (loop for x-list-y-list in
			 scatter-lists for label in labels collect (format nil "~D(~A)" (length (nth 0 x-list-y-list)) label)
			 into total-number-strings
			 nconc (loop for n from 0 to 1
				     collect (format nil "~A ~A: Mean(sd) ~,2e(~,2e)"
						     label (if (zerop n) (g-value win :x-label) (g-value win :y-label))
						     (mean (nth n x-list-y-list))
						     (std-dev (nth n x-list-y-list))))
			 into mean-std-strings
			 finally (return (concatenate-string-list (list (format nil "N = ~{~a ~}" total-number-strings) mean-std-strings)
								  :lf-count 1))) :position position))

(defun add-scatter-points-to-plot-from-xy-data-list (xy-data-list plot-agg win line-style symbol what-is-it curve-num)
  (add-scatter-points-to-plot plot-agg win (loop for x in (car xy-data-list)
						 for y in (cadr xy-data-list)
						 when (and (numberp x) (numberp y))
						 collect (x-plot-win x win)
						 and collect (y-plot-win y win))
			      line-style symbol what-is-it curve-num))

(defun time-ref-from-data-group-x-lists (data-group-x-lists trace-reference)
  (typecase data-group-x-lists
    (number data-group-x-lists)
    (t (nth (if (= (length data-group-x-lists) 1) 0 (round trace-reference)) data-group-x-lists))))


(defun parse-line-styles-for-plot (line-styles win waterfall use-same-line-style num-curves-per-group)
  (let ((temp-styles
	 (cond 
	  ((consp line-styles) line-styles)
	  ((consp (g-value win :plot-line-style)) (g-value win :plot-line-style))
	  (t (get-line-styles (or line-styles (g-value win :plot-line-style)) (unless (or waterfall use-same-line-style) num-curves-per-group))))))
    (if (or waterfall use-same-line-style)
      (list (car temp-styles))
      temp-styles)))

(defun number-of-overlays (win)
  ;;  should be identical to  (length (g-value win :y-lists))
  (length (g-value win :x-lists)))

  
;; PLOT-TIMED-DATA DATA-LISTS are one or more sets of Y data point sequences; a single set of lists
;; is for adding new data to a window (which may have data already), and multiple sets are for
;; creating overlaid plots from scratch. In the former case, the format is a list of sequences:
;; '((ydata1-seq) (ydata2-seq)...), where each sequence may be a list or an array of Y points. In the
;; latter case, the format is a list of lists of lists: '(((ydata1A-list) (ydata2A-list))
;; ((ydata1B-list) (ydata2B-list))...). In this case the data points must all be in lists. Note that
;; this second format is the way that the y data is stored in the :y-lists slot of the plotting
;; window. The format for the optional TIME-SEQ argument is the same, except that it is possible for
;; there to be only one X data point list corresponding to a set of Y data point lists; this may
;; occur, for example, when there is a common time base for a set of Y data. LABEL-LIST is an
;; optional list of labels, the number of labels typically corresponding to the number of Y data
;; point sequences is the first set in the DATA-LISTS list of sequences of Y data point sequences.
;; If TIME-SEQ is NIL, then the time base is inferred from the DELTA-T argument (default 1.0). If
;; LABEL-LIST is NIL, AND the target window has no label-list, AND :LABEL-TRACES is t (default) then
;; labels of the form "CANONIC-LABEL-i" are used, where i ranges from 1 to the number of data lists,
;; and CANONIC-LABEL is taken from the argument of the same name (default "DATA"). The
;; ACCOMODATE-ALL-OVERLAYS argument causes the plot dimensions to change in order to accomodate
;; subsequent overlaid data.  The data and time points *must* be single floats!!

;; :X-TRACE-OFFSET and :Y-TRACE-OFFSET are applied to the data *after*, and :X-DATA-OFFSET and
;; :Y-DATA-OFFSET are applied to the data *before* log (when :X-LOG and/or :Y-LOG is set).

;; :AXES-TYPE can be either NIL (axes type unchanged for plotting to an old window, with the
;; default :STANDARD) :SIMPLE, :STANDARD or :NONE. You can also enable X or Y axes with the
;; :X-AXIS-P and :Y-AXIS-P arguments (both default to T).
 
;; When the :AUTO-WF-SETUP argument is non-NIL, then a waterfall plot will be made with the
;; traces stacked vertically (spaced by the largest amplitude trace), the trace labels centered
;; vertically to the right of each trace, and the simple axes set in the lower left corner.

;; If the :ERASE-DATA-AFTER-PLOT key argument is T (default NIL), then data that is normally stored
;; in a (standard) plot window is erased after the plot is done, but not the displayed image. This
;; may be useful if there are a lot of plots and memory is getting tight. While a window whose data
;; has been erased may be printed and examined with the cross hairs, zooming, unzooming, restoring,
;; or other rescaling will not be possible.

(defun plot-timed-data (data-lists &optional label-list time-seq
				   &key title win
				   width height left top ; The window dimensions and position vis-a-vis the screen, in pixels.
					; (scale 0.8)
				   data-type ; Optional. May be used to select plot windows according to some aspect of the data. 
				   x-lists y-lists (delta-t 1.0) (delta-t-start 0.0) timed-data 

				   prompt-for-overlay (overlay *overlay-all-plots*) (ACCOMODATE-all-overlays *accomodate-all-overlays*)
				   (preserve-plot-layout *preserve-plot-layout*) preserve-window-attributes preserve-window-dimensions
				   (update t) (resurrect t) (visible t)
				   (plot-point-skip nil plot-point-skip-supplied-p)
				   
				   (CANONIC-LABEL "DATA") (x-label *default-x-label*) (y-label *default-y-label*)
				   x-label-vertical-position x-label-horizontal-position
				   invert-y-axis-label y-label-vertical-position (y-label-horizontal-position :left)
				   (x-label-position nil x-label-position-supplied-p) ; backward comp
				   
				   x-are-fns y-are-fns x-axis-number-coefficient y-axis-number-coefficient
				   (x-axis-value-prefix nil x-axis-value-prefix-supplied-p) (x-axis-value-suffix nil x-axis-value-suffix-supplied-p)
				   (y-axis-value-prefix nil y-axis-value-prefix-supplied-p) (y-axis-value-suffix nil y-axis-value-suffix-supplied-p)
				   
				   x-min x-max x-inc x-origin y-min y-max y-inc y-origin x-log y-log log-base 
				   axes-type ; (:standard :simple :none)
				   x-axis-root y-axis-root (x-axis-p t) (y-axis-p t)
				   x-scale-l% x-scale-t% y-scale-t% ; for positioning simple axes
				   (simple-axis-x-value-p t) (simple-axis-y-value-p t)
				   consider-y-axis-visible-limit y-axis-visible-max y-axis-visible-min
				   consider-x-axis-visible-limit x-axis-visible-max x-axis-visible-min

				   linear-regression
				   
				   (x-origin-tick nil) (y-origin-tick t)
				   reference-ticks-to-origin (include-x-tick-at-0 :follow-window) (include-y-tick-at-0 :follow-window)

				   use-bins bin-width (stipple-percent 100)
				   line-styles use-same-line-style
				   upper-right-hand-comment comment (comment-position *default-comment-position*)

				   use-fixed-top-gap (fixed-top-gap 0) use-fixed-bottom-gap (fixed-bottom-gap 0)
				   use-fixed-right-gap (fixed-right-gap 0) use-fixed-left-gap (fixed-left-gap 0)
				   update-fixed-gap-parameters
				   
				   (draw-grid *default-plot-grid-p*) grid-line-style 
				   (label-traces *label-plot-traces*) 
				   (x-data-offset 0.0) (y-data-offset 0.0) (x-trace-offset 0.0) (y-trace-offset 0.0)
				   fix-to-unity-mag-if-so session-name

				   (connect-data-points t) scatter scatter-symbol-fill
				   scatter-symbol ; This can be a single symbol or a list, as is *scatter-symbols* (default)
				   scatter-width-heights (scatter-symbol-borderp t) (fill-scatter t)
				   (x-symbol-width *default-scatter-size*) (y-symbol-width *default-scatter-size*)
				   
				   replot-win-point-list restore-plot unzoom revise-plot erase-data-after-plot
				   polar (polar-circles-p t)
				   (save-markers t)
				   waterfall label-waterfall wf-skirt auto-wf-setup (waterfall-trace-label-skip 0) (waterfall-label-offset 0.0))
					;  (declare (optimize (safety 0) (speed 3) (space 0)))

  (when x-label-position-supplied-p (setq x-label-vertical-position x-label-position)) ; backward comp

  (when (or data-lists y-lists
	    replot-win-point-list restore-plot unzoom revise-plot)

    (let ((plot-type (cond (waterfall :waterfall)
			   (polar :polar)
			   (win (g-value win :plot-type))
			   (t :xy))))

      (case plot-type
	((:xy :waterfall) (setq polar nil))
	(:polar (setq polar t)))

      (when win (setq width (or width (g-value win :width)) height (or height (g-value win :height))))
      
      (when polar
	(when win (setq polar-circles-p (g-value win :polar-circles-p)))
	(when (and y-inc x-inc) (setq x-inc (max x-inc y-inc) y-inc (max x-inc y-inc)))
	(setq width (max width height) height (max width height)
	      x-origin-tick nil y-origin-tick nil
	      x-label "" x-max (if (and x-max y-max) (max x-max y-max) (or y-max x-max)))
	(unless x-max (error-message "Need to specify x-max or y-max for polar plot"))
	(setq y-max x-max y-min (- x-max) x-min (- x-max)))

      (setq win (get-plot-window
		 plot-type data-type overlay :default-win win :left left :top top
		 :preserve-window-dimensions preserve-window-dimensions :width width :height height
		 :name (format nil "~A" (or title (and win (g-value win :title)) "Data Plot"))
		 :prompt-for-overlay prompt-for-overlay :session-name session-name :save-markers save-markers
		 :preserve-plot-layout preserve-plot-layout :accomodate-all-overlays accomodate-all-overlays))

      (when win

	(when line-styles (s-value win :plot-line-style line-styles))

	(unless preserve-window-attributes
	  (when x-axis-number-coefficient (s-value win :x-axis-number-coefficient x-axis-number-coefficient))
	  (when y-axis-number-coefficient (s-value win :y-axis-number-coefficient y-axis-number-coefficient))
	  (when x-axis-value-prefix-supplied-p (s-value win :x-axis-value-prefix x-axis-value-prefix))
	  (when x-axis-value-suffix-supplied-p (s-value win :x-axis-value-suffix x-axis-value-suffix))
	  (when y-axis-value-prefix-supplied-p (s-value win :y-axis-value-prefix y-axis-value-prefix))
	  (when y-axis-value-suffix-supplied-p (s-value win :y-axis-value-suffix y-axis-value-suffix))
	  (s-value win :use-bins use-bins)
	  (s-value win :bin-width (s-flt (or bin-width 0)))
	  (s-value win :draw-grid draw-grid)
	  (s-value win :polar-circles-p (and  polar polar-circles-p))
	  (setq width (g-value win :width) height (g-value win :height))
	  (when (and label-list (not (consp label-list))) (setq label-list (list label-list)))
	  
	  (s-value win :scatter (or (g-value win :scatter) scatter))
	  (s-value win :scatter-symbol-fill (or (g-value win :scatter-symbol-fill) scatter-symbol-fill))
	  (when scatter-symbol (s-value win :scatter-symbol scatter-symbol))
	  (when scatter-width-heights (s-value win :scatter-width-heights scatter-width-heights))
	  (s-value win :scatter-symbol-borderp scatter-symbol-borderp) (s-value win :fill-scatter fill-scatter)
	  (s-value win :x-symbol-width x-symbol-width) (s-value win :y-symbol-width y-symbol-width)
	  
	  (s-value win :consider-y-axis-visible-limit consider-y-axis-visible-limit)
	  (s-value win :y-axis-visible-min y-axis-visible-min) (s-value win :y-axis-visible-max y-axis-visible-max)
	  (s-value win :consider-x-axis-visible-limit consider-x-axis-visible-limit)
	  (s-value win :x-axis-visible-min x-axis-visible-min) (s-value win :x-axis-visible-max x-axis-visible-max))

	(when update-fixed-gap-parameters
	  (UPDATE-FIXED-GAP-PARAMETERS win use-fixed-top-gap fixed-top-gap use-fixed-bottom-gap fixed-bottom-gap
				       use-fixed-right-gap fixed-right-gap use-fixed-left-gap fixed-left-gap))

	(unless (g-value win :data-erased)    
	  (unless preserve-window-attributes
	    (when delta-t-start (s-value win :delta-t-start (s-flt delta-t-start))))
	  (when (and (eq :waterfall (g-value win :plot-type))
		     (or (eq waterfall :auto) auto-wf-setup (g-value win :auto-setup)))
	    (setq auto-wf-setup t
		  label-waterfall t
		  waterfall t wf-skirt t))
	  (when (eq :waterfall (g-value win :plot-type))
	    (s-value win :waterfall (cond (auto-wf-setup :auto)
					  (waterfall t))))

	  (if preserve-window-attributes
	    (setq plot-point-skip (g-value win :plot-point-skip)
		  x-are-fns (g-value win :x-are-fns)
		  y-are-fns (g-value win :y-are-fns)
		  x-label-horizontal-position (g-value win :x-label-horizontal-position)
		  x-label-vertical-position (g-value win :x-label-vertical-position)
		  y-label-horizontal-position (g-value win :y-label-horizontal-position)
		  y-label-vertical-position (g-value win :y-label-vertical-position)
		  label-traces (g-value win :label-traces)
		  draw-grid (g-value win :draw-grid)
		  log-base (g-value win :log-base)
		  use-bins (g-value win :use-bins)
		  waterfall-trace-label-skip (g-value win :waterfall-trace-label-skip)
		  waterfall-label-offset (g-value win :waterfall-label-offset))
	    (progn
	      (when plot-point-skip-supplied-p (s-value win :plot-point-skip plot-point-skip))
	      (s-value win :reference-ticks-to-origin reference-ticks-to-origin)
	      (s-value win :stipple-percent stipple-percent )
	      (s-value win :absolute-value-ticks polar)
	      (s-value win :draw-grid draw-grid)
	      (when axes-type (s-value win :axes-type axes-type))
	      (s-value win :use-same-line-style (or (g-value win :waterfall) use-same-line-style))
	      (s-value win :label-waterfall label-waterfall)

	      (s-value win :use-same-line-style (or (g-value win :waterfall) (g-value win :use-same-line-style)))
	      (when y-label-horizontal-position (s-value win :y-label-horizontal-position y-label-horizontal-position))
	      (when grid-line-style (s-value win :grid-line-style grid-line-style))
	      (s-value win :log-base (when (numberp log-base) (s-flt log-base)))
	      (s-value win :x-axis-p x-axis-p)
	      (s-value win :y-axis-p y-axis-p)))

	  (s-value win :waterfall-trace-label-skip waterfall-trace-label-skip)
	  (s-value win :waterfall-label-offset waterfall-label-offset)
	  (when upper-right-hand-comment (add-temp-comment win upper-right-hand-comment))
	  (add-local-and-global-comment comment comment-position win)

	  (if replot-win-point-list
	    (setq restore-plot nil unzoom nil) ; just to be safe.
	    (when restore-plot (setq unzoom nil)))

	  (s-value win :simple-axis-x-value-p simple-axis-x-value-p)
	  (s-value win :simple-axis-y-value-p simple-axis-y-value-p)
	
	  (when x-scale-l% (s-value win :x-scale-l% x-scale-l%))
	  (when x-scale-t% (s-value win :x-scale-t% x-scale-t%))
	  (when y-scale-t% (s-value win :y-scale-t% y-scale-t%))

	  
	  (if (or replot-win-point-list restore-plot unzoom revise-plot)
	    
	    (progn			; Extract arguments from the window.
	      (s-value win :overlay nil)
	      (when (or replot-win-point-list restore-plot unzoom)
		(setq x-trace-offset (g-value win :x-trace-offset) y-trace-offset (g-value win :y-trace-offset)
		      waterfall (eq :waterfall (g-value win :plot-type)) wf-skirt (g-value win :wf-skirt)
		      x-label nil y-label nil connect-data-points (g-value win :connect-data-points))))
	   
	    (progn			; Otherwise new plot, maybe overlay.
	      (s-value win :connect-data-points connect-data-points)

	      ;; For the Y values, the precedence is :y-lists argument > data-lists
	      (cond			
	       (y-lists (s-value win :y-lists y-lists))
	       (data-lists (load-y-lists win data-lists)))
		   
	      ;; For the X values, the precedence is :x-lists argument > time-seq > delta-t
	      (unless (or (and (not data-lists) (g-value win :x-lists)) time-seq)
		(s-value win :delta-t (setq time-seq delta-t)))
	      (cond
	       (x-lists	(s-value win :x-lists x-lists))
	       (time-seq (load-x-lists win time-seq)))))
	  

	  (let ((linear-regression (or (g-value win :linear-regression) linear-regression))
		(num-curves-per-group (length (car (g-value win :y-lists)))))

	    (when linear-regression (s-value win :linear-regression t))

	    (unless (= (length (or label-list (g-value win :label-list))) num-curves-per-group)
	      (setq label-list (get-canonic-labels num-curves-per-group canonic-label)))
	    (clear-up-label-list win (or label-list (g-value win :label-list)))

	    (unless (and (g-value win :has-been-setup)
			 (or (g-value win :preserve-plot-layout) (and (g-value win :overlay) (not (g-value win :accomodate-all-overlays)))))
	      (setup-plot win :width width :height height 
					; :scale scale
			  :auto-wf-setup auto-wf-setup :wf-skirt wf-skirt :timed-data timed-data :consider-labels (and label-traces (not waterfall))
			  :x-are-fns x-are-fns :y-are-fns y-are-fns :fix-to-unity-mag-if-so fix-to-unity-mag-if-so
			  :x-inc x-inc :x-min-spec x-min :x-max-spec x-max :x-log x-log
			  :x-label x-label
			  :x-label-horizontal-position x-label-horizontal-position :x-label-vertical-position x-label-vertical-position
			  :y-label y-label
			  :y-label-horizontal-position y-label-horizontal-position :y-label-vertical-position y-label-vertical-position 
			  :invert-y-axis-label invert-y-axis-label
			  :y-inc y-inc :y-min-spec y-min :y-max-spec y-max :y-log y-log
			  :x-origin-tick x-origin-tick :y-origin-tick y-origin-tick
			  :x-origin x-origin :y-origin y-origin :x-axis-root x-axis-root :y-axis-root y-axis-root
			  :x-trace-offset x-trace-offset :x-data-offset (if waterfall (g-value win :waterfall-base-x-offset) x-data-offset)
			  :y-trace-offset y-trace-offset :y-data-offset (if waterfall (g-value win :waterfall-base-y-offset) y-data-offset)
			  :unzoom unzoom :restore-plot restore-plot :replot-win-point-list replot-win-point-list :revise-plot revise-plot))

	    (let ((plot-agg (get-plot-agg win 'data-plot t))
		  (line-styles (parse-line-styles-for-plot line-styles win waterfall use-same-line-style num-curves-per-group))
		  (waterfall-base-x-offset (g-value win :waterfall-base-x-offset))
		  (waterfall-base-y-offset (g-value win :waterfall-base-y-offset))
		  (trace-references (no-nils (or (g-value win :trace-order) (list-of-nums num-curves-per-group))))
		  (relevant-labels (relevant-labels win num-curves-per-group)))
	      (declare (single-float waterfall-base-x-offset waterfall-base-y-offset))
	      (loop for data-group fixnum from 0 
		    for data-group-y-lists in (g-value win :y-lists)
		    for data-group-x-lists in (g-value win :x-lists)
		    do
		    (loop for trace-reference in trace-references
			  for curve-num fixnum from 0
			  do
			  (let* ((curve-num-*-x-offset (* curve-num (the sf (g-value win :x-trace-offset))))
				 (curve-num-*-y-offset (* curve-num (the sf (g-value win :y-trace-offset))))
				 (y-list (nth (round trace-reference) data-group-y-lists))
				 (x-ref (time-ref-from-data-group-x-lists data-group-x-lists trace-reference))
				 (label (nth (round trace-reference) (g-value win :label-list)))
				 (scatter-symbol (get-scatter-symbol win curve-num))
				 (line-style (if (consp line-styles) (nth (mod curve-num (length line-styles)) line-styles) line-styles))
				 (waterfall-x-offset (when waterfall (+ waterfall-base-x-offset curve-num-*-x-offset)))
				 (waterfall-y-offset (when waterfall (+ waterfall-base-y-offset curve-num-*-y-offset)))
				 (points
				  (get-plot-point-list x-ref y-list win :x-trace-offset waterfall-x-offset :y-trace-offset waterfall-y-offset)))
			    (declare (single-float curve-num-*-x-offset curve-num-*-y-offset))

			    (cond-every
			       
			     (linear-regression
			      (multiple-value-bind (xfrmed-time-seq xfrmed-data-list) (GET-XFRMED-POINT-LIST x-ref y-list win)
				(multiple-value-bind (slope intercept r) (lin-reg (list xfrmed-time-seq xfrmed-data-list))
				  (add-linear-regression win slope intercept r line-style :label label))))

			     ((or waterfall (g-value win :connect-data-points))
			      (if (g-value win :use-bins)
				(add-histo-bins plot-agg points (when waterfall (+ waterfall-base-y-offset curve-num-*-y-offset)))
				(new-add-polyline-to-plot plot-agg points line-style polar)))

			     ((and waterfall wf-skirt)
			      (add-wf-skirt plot-agg win points)) ; Blanked area under curve for waterfall plots.

			     ((g-value win :scatter)
			      (let ((scatter-points (get-plot-point-list x-ref y-list win :only-visible t
									 :x-trace-offset waterfall-x-offset :y-trace-offset waterfall-y-offset)))
				(add-scatter-points-to-plot plot-agg win scatter-points line-style scatter-symbol :data-points curve-num)))

			     ((and (= data-group 0)
				   label waterfall label-waterfall
				   (= 0 (the fn (mod curve-num (1+ (the fn (g-value win :waterfall-trace-label-skip)))))))
			      (add-waterfall-label plot-agg label curve-num-*-x-offset curve-num-*-y-offset))))))
			    
	      (when (g-value win :label-traces) (label-traces plot-agg num-curves-per-group (the cons line-styles) relevant-labels)))
	    (when erase-data-after-plot (erase-plot-data win))
	    (plot-windows-finishing win (cond (resurrect :resurrect)
					      (update :update)))))))))



;; PLOT-POINTS For plotting a set or sets of XY points.
;;
;; LIST-OF-POINT-LISTS = '((x y)(x y)(x y)(x y)...) for one set, or
;;                       '(((x1 y1)(x1 y1)...(x1 y1)) ((x2 y2)(x2 y2)...(x2 y2)) ...) for more than
;;                       one set.
;;
(defun plot-points (list-of-point-lists &optional label-list &key win ; (scale 0.8)
					(overlay nil)(title "XY Data")
					(connect-data-points t) scatter scatter-symbol scatter-width-heights
					(scatter-symbol-borderp t) (fill-scatter t) 
					x-symbol-width y-symbol-width
					even-quadrant-diagonal odd-quadrant-diagonal linear-regression
					x-axis-value-prefix x-axis-value-suffix
					y-axis-value-prefix y-axis-value-suffix

					(x-label "") x-label-vertical-position x-label-horizontal-position
					(y-label "") y-label-vertical-position (y-label-horizontal-position :left)

					(draw-grid *default-plot-grid-p*)
					
					(x-origin-tick nil) (y-origin-tick t)
					line-styles

					x-are-fns y-are-fns
					axes-type ; (:standard :simple :none)
					x-scale-l% x-scale-t% y-scale-t%
					(simple-axis-x-value-p t) (simple-axis-y-value-p t)
					
					(label-traces t)
					update-fixed-gap-parameters
					use-fixed-top-gap (fixed-top-gap 0)
					use-fixed-bottom-gap (fixed-bottom-gap 0)
					use-fixed-right-gap (fixed-right-gap 0)
					use-fixed-left-gap (fixed-left-gap 0)
					use-same-line-style
					polar connect-ends waterfall
					comment (upper-right-hand-comment "") (comment-position *default-comment-position*)
					session-name
					left top (width 400) (height 400) 
					x-inc y-inc y-min y-max x-min x-max  x-origin y-origin)
  (plot-xy-data
   (loop for point-list in (if (consp (caar list-of-point-lists)) list-of-point-lists (list list-of-point-lists))
	 collect (loop for point in point-list
		       when point collect (car point) into x-list and collect (cadr point) into y-list
		       finally (return (no-nils (list x-list y-list)))))
   label-list
   :win win ; :scale scale
   :overlay overlay :title title 
   :connect-data-points connect-data-points
   :scatter scatter :scatter-symbol scatter-symbol
   :fill-scatter fill-scatter :scatter-width-heights scatter-width-heights :scatter-symbol-borderp scatter-symbol-borderp
   :even-quadrant-diagonal even-quadrant-diagonal :odd-quadrant-diagonal odd-quadrant-diagonal
   :linear-regression linear-regression
   :x-label x-label :y-label y-label
   :x-label-vertical-position x-label-vertical-position :x-label-horizontal-position x-label-horizontal-position
   :y-label-vertical-position y-label-vertical-position :y-label-horizontal-position y-label-horizontal-position
   :x-axis-value-prefix x-axis-value-prefix :x-axis-value-suffix x-axis-value-suffix
   :y-axis-value-prefix y-axis-value-prefix :y-axis-value-suffix y-axis-value-suffix
   :draw-grid draw-grid
   :x-origin-tick x-origin-tick :y-origin-tick y-origin-tick
   :x-scale-l% x-scale-l% :x-scale-t% x-scale-t% :y-scale-t% y-scale-t%
   :axes-type axes-type :simple-axis-x-value-p simple-axis-x-value-p :simple-axis-y-value-p simple-axis-y-value-p
   :update-fixed-gap-parameters update-fixed-gap-parameters
   :use-fixed-top-gap use-fixed-top-gap :fixed-top-gap fixed-top-gap
   :use-fixed-bottom-gap use-fixed-bottom-gap :fixed-bottom-gap fixed-bottom-gap
   :use-fixed-right-gap use-fixed-right-gap :fixed-right-gap fixed-right-gap
   :use-fixed-left-gap use-fixed-left-gap :fixed-left-gap fixed-left-gap
   :width width :height height :left left :top top
   :line-styles line-styles :use-same-line-style use-same-line-style
   :label-traces label-traces
   :polar polar :connect-ends connect-ends :waterfall waterfall 
   :x-symbol-width x-symbol-width :upper-right-hand-comment upper-right-hand-comment :comment comment
   :comment-position comment-position
   :y-symbol-width y-symbol-width :session-name session-name
   :x-are-fns x-are-fns :y-are-fns y-are-fns
   :x-inc x-inc :y-inc y-inc :y-min y-min :y-max y-max :x-min x-min :x-max x-max :x-origin x-origin :y-origin y-origin))

(defun plot-scatter (list-of-point-lists &optional label-list &key win
					 ; (scale 0.8)
					 (overlay nil) (title "XY Data")
					 connect-data-points
					 (scatter t) (scatter-symbol :filled-dot) (x-symbol-width *default-scatter-size*) y-symbol-width
					 even-quadrant-diagonal odd-quadrant-diagonal linear-regression
					 (x-label "") x-label-vertical-position x-label-horizontal-position 
					 (y-label "") y-label-vertical-position (y-label-horizontal-position :left)
					 (draw-grid *default-plot-grid-p*)
					 (x-origin-tick nil)(y-origin-tick t)
					 line-styles
					 x-axis-value-prefix x-axis-value-suffix
					 y-axis-value-prefix y-axis-value-suffix
					 
					 x-are-fns y-are-fns
					 axes-type ; (:standard :simple :none)
					 x-scale-l% x-scale-t% y-scale-t%
					 (simple-axis-x-value-p t) (simple-axis-y-value-p t)

					 UPDATE-FIXED-GAP-PARAMETERS
					 use-fixed-top-gap (fixed-top-gap 0)
					 use-fixed-bottom-gap (fixed-bottom-gap 0)
					 use-fixed-right-gap (fixed-right-gap 0)
					 use-fixed-left-gap (fixed-left-gap 0)
					
					 (label-traces t)
					 use-same-line-style
					 
					 comment (upper-right-hand-comment "") (comment-position *default-comment-position*)
					 session-name
					 left top (width 400) (height 400) 
					 x-inc y-inc y-min y-max x-min x-max  x-origin y-origin)
  (plot-points list-of-point-lists label-list
	       :win win ; :scale (or scale (and win (g-value win :scale)))
	       :overlay overlay :title title
	       :connect-data-points connect-data-points
	       :scatter scatter :scatter-symbol scatter-symbol :x-symbol-width x-symbol-width :y-symbol-width (or y-symbol-width x-symbol-width)
	       :even-quadrant-diagonal even-quadrant-diagonal :odd-quadrant-diagonal odd-quadrant-diagonal
	       :linear-regression linear-regression
	       :x-label x-label :y-label y-label
	       :x-label-vertical-position x-label-vertical-position
	       :x-label-horizontal-position x-label-horizontal-position
	       :y-label-vertical-position y-label-vertical-position
	       :y-label-horizontal-position y-label-horizontal-position

	       :x-axis-value-prefix x-axis-value-prefix :x-axis-value-suffix x-axis-value-suffix
	       :y-axis-value-prefix y-axis-value-prefix :y-axis-value-suffix y-axis-value-suffix
	       
	       :x-origin-tick x-origin-tick :y-origin-tick y-origin-tick
	       :line-styles line-styles :use-same-line-style use-same-line-style
	       :label-traces label-traces
	       :x-are-fns x-are-fns :y-are-fns y-are-fns
	       :axes-type axes-type :simple-axis-x-value-p simple-axis-x-value-p :simple-axis-y-value-p simple-axis-y-value-p
	       :x-scale-l% x-scale-l% :x-scale-t% x-scale-t% :y-scale-t% y-scale-t%
	       :draw-grid draw-grid
	       :update-fixed-gap-parameters update-fixed-gap-parameters
	       :use-fixed-top-gap use-fixed-top-gap :fixed-top-gap fixed-top-gap
	       :use-fixed-bottom-gap use-fixed-bottom-gap :fixed-bottom-gap fixed-bottom-gap
	       :use-fixed-right-gap use-fixed-right-gap :fixed-right-gap fixed-right-gap
	       :use-fixed-left-gap use-fixed-left-gap :fixed-left-gap fixed-left-gap
	       :comment comment :comment-position comment-position :upper-right-hand-comment upper-right-hand-comment
	       :session-name session-name
	       :width width :height height :left left :top top
	       :x-inc x-inc :y-inc y-inc :y-min y-min :y-max y-max :x-min x-min :x-max x-max
	       :x-origin x-origin :y-origin y-origin))

(defun basic-histo (list)
  (let* ((max (ph::maximize-val-or-seq list))
	 (histo-array (make-array (list (+ 1 (round max))) :initial-element 0.0)))
    (loop for len in list do (setf (aref histo-array (round len))
				   (+ 1.0 (aref histo-array (round len)))))
    (plot-xy-data (list (list (list-of-nums (+ 1.0 (float (round max)))) (array-to-list histo-array))) (list "histo")
		  :x-label "" :y-label "")
    (print (/ (loop for len in list summing len) (length list)))
    (print max)))

;;; PLOT-HISTOGRAM
;; xy-data-list =
;; '((x1 x1 ... x1)(y1 y1 ... y1)))
;; Histogram are constructed with vertical bars whose left edges are defined by the X entries in
;; XY-DATA-LIST, and whose widths are defined by BIN-WIDTH. While the Y entries may be real or
;; integer, the Y axis is marked with integer values.
(defun plot-histogram (xy-data-list bin-width &key win
				    ; (scale 0.8)
				    (title "XY Data") title-position data-type
				    (x-label "") (y-label "")
				    (x-axis-tick-skip 0) (x-axis-tick-mark-skip 0)
				    (y-axis-tick-skip 0) (y-axis-tick-mark-skip 0)
				    x-are-fns
				    x-label-vertical-position x-label-horizontal-position y-label-vertical-position y-label-horizontal-position
				    stipple-percent
				    (bar-border-p t)
				    (x-origin-tick t)(y-origin-tick t)
				    overlay accomodate-all-overlays prompt-for-overlay
				    axes-type ; (:standard :simple :none)
				    x-scale-l% x-scale-t% y-scale-t%
				    (simple-axis-x-value-p t) (simple-axis-y-value-p t)
				    UPDATE-FIXED-GAP-PARAMETERS
				    use-fixed-top-gap (fixed-top-gap 0)
				    use-fixed-bottom-gap (fixed-bottom-gap 0)
				    use-fixed-right-gap (fixed-right-gap 0)
				    use-fixed-left-gap (fixed-left-gap 0)
				    left top
				    font comment (comment-position *default-comment-position*) (upper-right-hand-comment "") 
				    (width 400) (height 400) 
				    y-inc (y-min 0.0) y-max x-min x-max  x-origin y-origin)
  (unless win (setq win (get-plot-window :xy data-type overlay :name title :mode :histogram
					 :accomodate-all-overlays accomodate-all-overlays :prompt-for-overlay prompt-for-overlay
					 :left left :top top)))
  (when win
    (when axes-type (s-value win :axes-type axes-type))
    (when x-scale-l% (s-value win :x-scale-l% x-scale-l%))
    (when x-scale-t% (s-value win :x-scale-t% x-scale-t%))
    (when y-scale-t% (s-value win :y-scale-t% y-scale-t%))
    (s-value win :simple-axis-x-value-p simple-axis-x-value-p)
    (s-value win :simple-axis-y-value-p simple-axis-y-value-p)

    (when UPDATE-FIXED-GAP-PARAMETERS
      (UPDATE-FIXED-GAP-PARAMETERS win use-fixed-top-gap fixed-top-gap use-fixed-bottom-gap fixed-bottom-gap
				   use-fixed-right-gap fixed-right-gap use-fixed-left-gap fixed-left-gap))
    (when font
      (s-value win :comment-font font)
      (s-value win :font font)
      (s-value win :plot-axis-font font))
    (s-value win :x-axis-tick-skip x-axis-tick-skip)
    (s-value win :x-axis-tick-mark-skip x-axis-tick-mark-skip)
    (s-value win :y-axis-tick-skip y-axis-tick-skip)
    (s-value win :y-axis-tick-mark-skip y-axis-tick-mark-skip)
    (add-temp-comment win upper-right-hand-comment)
    (add-local-and-global-comment comment comment-position win)
    (s-value win :bin-width (float bin-width))
    (s-value win :bar-border-p bar-border-p)
    (when y-label-horizontal-position (s-value win :y-label-horizontal-position y-label-horizontal-position))
    (s-value win :stipple-percent stipple-percent)  (s-value win :x-label-horizontal-position :center) 
    
    (unless y-max (setq y-max (float (max-of-list (cadr xy-data-list)))))  (unless y-inc (setq y-inc (/ y-max 5)))
    (load-x-lists win (list (car xy-data-list)))
    (load-y-lists win (list (cadr xy-data-list)))
    (unless (and (g-value win :overlay) (not (g-value win :accomodate-all-overlays)))
      (setup-plot win ; :scale scale
		  :x-origin x-origin :y-origin y-origin
		  :x-label-vertical-position x-label-vertical-position
		  :x-label-horizontal-position x-label-horizontal-position
		  :y-label-vertical-position y-label-vertical-position
		  :y-label-horizontal-position y-label-horizontal-position
		  :x-origin-tick x-origin-tick :y-origin-tick y-origin-tick
		  :x-inc (if (> (length (car xy-data-list)) 1)
			   (- (nth 1 (car xy-data-list)) (nth 0 (car xy-data-list)))
			   bin-width)
		  :x-min-spec x-min :x-max-spec (if x-max x-max (+ (max-of-list (car xy-data-list)) bin-width))
		  :x-label x-label :y-label y-label
		  :x-are-fns x-are-fns :y-are-fns t 
		  :y-inc y-inc :y-min-spec y-min :y-max-spec y-max :width width :height height :consider-labels t))
    (let* ((plot-agg (get-plot-agg win 'data-plot t))
	   (stipple-percent (or (g-value win :stipple-percent) (unless (g-value win :bar-border-p) 50)))
	   (line-style (when (g-value win :bar-border-p) wh::thick-line-style))
	   (filling-style  (when stipple-percent (get-opal-color-to-fill 'black stipple-percent))))
      (loop for x-list in (g-value win :x-lists)
	    for y-list in (g-value win :y-lists)
	    do (loop for x in (car x-list) ; (car xy-data-list)
		     for y in (car y-list) ; (cadr xy-data-list)
		     when (>= (- (g-value win :x-max) bin-width) x) 
		     do (add-histo-bin plot-agg x y nil nil line-style filling-style))))
    (when title-position (add-title win :position title-position))
    (plot-windows-finishing win)))

(defun histogram-sequence-values-to-array (sequence &key (bins 10) bin-width min max)
  (let* ((array (make-array (list bins)))
	 (list (sequence-to-list sequence))
	 (min (float (or min (min-of-list list))))
	 (max (float (cond
		      ((and bins bin-width)
		       (+ min (* bins bin-width)))
		      (max max)
		      (t (max-of-list list)))))
	 (bin-width (or bin-width (if (= 0 (- max min))
				    (progn (setq min 0.0) max)
				    (/ (- max min) bins))))
	 (base (* bin-width bins)))
    ;;    (format t "b-wid ~A base ~A min ~A, max ~A ~% " bin-width base min max)
    (loop for val in list do
	  ;; (format t "val ~A max ~A~%" val max)
	  (let ((bin (if (= val max) (1- bins) (floor (* bins (/ (- val min) base))))))
	    ;;	    (format t " bin ~A~%" bin)
	    (when (and (>= bin 0) (< bin bins)) (incf (aref array bin)))))
    ;;    (format t "array ~A~%" array)
    array))

(defun limit-sig-figs (number sig-figs &optional (rounding :round))
  (if (zerop number)
    number
    (let* ((num-power (floor (log (abs number) 10.0)))
	   (sig-figs (1- sig-figs)))
      (* (case rounding
	   (:round (round number (expt 10 (- num-power sig-figs))))
	   (:floor (floor number (expt 10 (- num-power sig-figs))))
	   (:ceiling (ceiling number (expt 10 (- num-power sig-figs)))))
	 (expt 10.0 (- num-power sig-figs))))))

(defun plot-histogram-list (list &key (bins 10) bin-width annotate-with-mean-and-std-dev (title "Histogram") width height
				 y-inc (y-min 0.0) y-max x-min x-max (x-origin :use-min) y-origin
				 limit-sig-figs fix-min-max-values (fix-min-max-values-factor 1)
				 (x-are-fns t) (x-axis-tick-skip 0) (x-axis-tick-mark-skip 0)
				 (y-axis-tick-skip 0) (y-axis-tick-mark-skip 0)
				 axes-type ; (:standard :simple :none)
				 x-scale-l% x-scale-t% y-scale-t%
				 (simple-axis-x-value-p t) (simple-axis-y-value-p t)
				 overlay accomodate-all-overlays prompt-for-overlay
				 (stipple-percent 10)
				 x-label-vertical-position x-label-horizontal-position
				 y-label-vertical-position y-label-horizontal-position
				 (x-label "") (y-label ""))
  (let* ((list (flatten-list list))
	 (temp-min (float (or x-min (min-of-list list))))
	 (min-limit-sig-figs (limit-sig-figs temp-min 3 :floor))
	 (min (cond ((and limit-sig-figs fix-min-max-values) (* fix-min-max-values-factor (floor min-limit-sig-figs fix-min-max-values-factor)))
		    (limit-sig-figs min-limit-sig-figs)
		    (t temp-min)))
	 (temp-max (float (cond ((and bins bin-width) (+ min (* bins bin-width)))
				(x-max x-max)
				(t (max-of-list list)))))
	 (max-limit-sig-figs (limit-sig-figs temp-max 3 :ceiling))
	 (max (cond ((and limit-sig-figs fix-min-max-values) (* fix-min-max-values-factor (ceiling max-limit-sig-figs fix-min-max-values-factor)))
		    (limit-sig-figs max-limit-sig-figs)
		    (t temp-max)))
	 (bins (ceiling (or bins (/ (- max min) bin-width))))
	 (bin-width (or bin-width (if (= 0 (- max min))
				    (progn (setq min 0.0) max)
				    (/ (- max min) bins)))))
    ;; (format t "min ~A ~A, max ~A ~A, bins ~A. bin-width ~A~%"
    ;;    min-limit-sig-figs min max-limit-sig-figs max bins bin-width)
    ;; (format t "~A~%" (histogram-sequence-values-to-array list :bins bins :min min))
    (if (= bin-width 0)
      (format t "Nothing to Histogram!~%")
      (let* ((array (histogram-sequence-values-to-array list :bins bins :min min :max max))
	     (plot-win
	      (plot-histogram (list (loop for x from min by bin-width
					  for i from 1 to bins
					  collect x)
				    (array-to-list array))
			      bin-width
			      :title title :width width :height height
			      :x-label-vertical-position x-label-vertical-position
			      :x-label-horizontal-position x-label-horizontal-position
			      :y-label-vertical-position y-label-vertical-position
			      :y-label-horizontal-position y-label-horizontal-position
			      :x-are-fns x-are-fns :stipple-percent stipple-percent
			      :x-axis-tick-skip x-axis-tick-skip :x-axis-tick-mark-skip x-axis-tick-mark-skip
			      :y-axis-tick-skip y-axis-tick-skip :y-axis-tick-mark-skip y-axis-tick-mark-skip
			      :axes-type axes-type
			      :x-scale-l% x-scale-l% :x-scale-t% x-scale-t% :y-scale-t% y-scale-t%
			      :simple-axis-x-value-p simple-axis-x-value-p :simple-axis-y-value-p simple-axis-y-value-p
			      :x-label x-label :y-label y-label
			      :overlay overlay :prompt-for-overlay prompt-for-overlay :accomodate-all-overlays accomodate-all-overlays
			      :x-min (or x-min min) :x-max (or x-max (+ min (* bin-width bins)))
			      :y-inc y-inc :y-min y-min :y-max y-max :y-origin y-origin
			      :x-origin (case x-origin
					  (:use-min min)
					  (t x-origin)))))
	(when annotate-with-mean-and-std-dev
	  (add-comment plot-win (format nil "Mean ~,2f, Std-dev ~,2f" (mean list) (std-dev list)) :position :upper-right))
	(let ((*automatic-run* t)) (histogram-menu plot-win)) ; bogosity
	plot-win))))	 
				  
(defun plot-polar-data (rtheta-lists label-list &key win (overlay nil) (title "R Theta Data")
				     LIne-styles comment (comment-position *default-comment-position*)
				     (label-traces t) (polar-circles-p t)
				     use-same-line-style
				     axes-type (simple-axis-x-value-p t) (simple-axis-y-value-p t)
				     x-scale-l% x-scale-t% y-scale-t%
				     (scatter nil) (r-label "") ; cross-width
				     x-label-vertical-position x-label-horizontal-position
				     y-label-vertical-position (y-label-horizontal-position :left)
				     (width 500) (height 500) r-inc r-max)
  (plot-xy-data
   (loop for rtheta-list in rtheta-lists
	 collect 
	 (loop for r in (car rtheta-list)
	       for theta in (cadr rtheta-list)
	       collect (* r (cos-degrees theta)) into x-list
	       collect (* r (sin-degrees theta)) into y-list
	       finally (return (list x-list y-list))))
   label-list
   :win win :title title :comment comment :comment-position comment-position
   :width width :height height
   :x-label-vertical-position x-label-vertical-position
   :x-label-horizontal-position x-label-horizontal-position
   :y-label-vertical-position y-label-vertical-position
   :y-label-horizontal-position y-label-horizontal-position
   :line-styles line-styles :use-same-line-style use-same-line-style
   :label-traces label-traces
   :axes-type axes-type :x-scale-l% x-scale-l% :x-scale-t% x-scale-t% :y-scale-t% y-scale-t%
   :simple-axis-x-value-p simple-axis-x-value-p :simple-axis-y-value-p simple-axis-y-value-p
   :overlay overlay
   :polar t :polar-circles-p polar-circles-p :connect-data-points t :connect-ends t :scatter scatter
   :y-label r-label
   :y-max (or r-max (max-of-list (mapcar #'(lambda (x) (max-of-list (car x))) rtheta-lists)))
   :y-inc r-inc :x-inc r-inc))

(defun plot-polar-simple-array (rtheta-lists label-list &key win (overlay nil)(title "R Theta Data")
					     x-label-vertical-position x-label-horizontal-position
					     y-label-vertical-position (y-label-horizontal-position :left)
					     (scatter nil) (r-label "") ; cross-width
					     (polar-circles-p t)
					     (width 500) (height 500) r-inc r-max)
  (plot-xy-data
   (loop for rtheta-list in rtheta-lists
	 collect (loop for theta in (cadr rtheta-list)
		       for r in (car rtheta-list)
		       collect 0.0 into x-list
		       collect (* r (cos-degrees theta)) into x-list
		       collect 0.0 into y-list
		       collect (* r (sin-degrees theta)) into y-list
		       finally (return (list x-list y-list))))
   label-list
   :win win :title title :width width :height height
   :x-label-vertical-position x-label-vertical-position
   :x-label-horizontal-position x-label-horizontal-position
   :y-label-vertical-position y-label-vertical-position
   :y-label-horizontal-position y-label-horizontal-position
   
   :overlay overlay
   :polar-circles-p polar-circles-p :polar t :connect-data-points t :connect-ends t :scatter scatter
   :y-label r-label
   :y-max (or r-max (max-of-list (mapcar #'(lambda (x) (max-of-list (car x))) rtheta-lists))) :y-inc r-inc :x-inc r-inc ))

(defun plot-polar-vectors (r-theta-lists label-list &key win title
					 x-label-vertical-position x-label-horizontal-position
					 y-label-vertical-position (y-label-horizontal-position :left)
					 (polar-circles-p t) (width 350) (height 350) overlay)
  (let ((new-r-theta-lists
	 (loop for r-theta-list in r-theta-lists
	       collect (list (loop for r in (car r-theta-list)
				   collect r
				   collect 0.0)
			     (loop for theta in (cadr r-theta-list)
				   collect theta
				   collect theta)))))
    (plot-polar-data new-r-theta-lists label-list :win win :title title :width width :height height
		     :x-label-vertical-position x-label-vertical-position
		     :x-label-horizontal-position x-label-horizontal-position
		     :y-label-vertical-position y-label-vertical-position
		     :y-label-horizontal-position y-label-horizontal-position
		     :overlay overlay :polar-circles-p polar-circles-p :line-styles very-THICK-COLORS)))

(defun add-trace (new-trace new-label &optional win-or-title new-timebase)
  "This function adds NEW-TRACE with reference NEW-LABEL to an existing :STANDARD-PLOT plot window, using the time base of the window unless a
sequence NEW-TIMEBASE is supplied. WIN-OR-TITLE can be the (string) title of an existing window, a window, or NIL. If NIL, then a menu is provided. "
  (let ((win (typecase win-or-title
	       (string (loop for win in (standard-plot-windows)
			     when (string= win-or-title (g-value win :title)) return win))
	       (schema (when (eq (g-value win-or-title :mode) :standard-plot) win-or-title))
	       (t (WINDOW-SELECTION-MENU "Pick One Window to Add Trace" (standard-plot-windows) nil t)))))
    (unless win (sim-error (format nil "~A does not reference a window!" win-or-title)))
    (plot-timed-data (cons new-trace (car (g-value win :y-lists)))
		     (cons new-label (g-value win :label-list))
		     (no-nils (cons (sequence-to-list new-timebase) (plot-window-top-x-lists win)))
		     :win win)))


;; Functions for saving plots to lisp files.
(defun dump-plot-to-lisp-file (&optional wins filename (directory *plot-code-directory*) force)
  "Given optional WINS \(list or atom\), FILENAME \(string\), and DIRECTORY [default *PLOT-CODE-DIRECTORY*] write lisp files that recreate a plot
window. Menus prompt for missing arguments. Already existing files with same name will be overwritten. FORCE disables all prompts except initial
window menu if needed."
  (loop for win in (coerce-to-list (or wins (win-menu "Select Plot Windows to Dump" (standard-plot-windows))))
	do (let* ((*automatic-run* force)
		  (dummy1 (or filename (format nil "~A.lisp" (make-nice-filename (strip-displayed-host-name-from-title (g-value win :title))))))
		  (dummy2 directory)
		  dummy3)
	     (choose-variable-values
	      `((dummy1 "Filename" :string)
		(dummy2 "Directory" :string)
		(dummy3 "CANCEL" :boolean))
	      :title (format nil "Write Plot Window Lisp File of ~A" (g-value win :title)))
	     (unless dummy3
	       (let* ((pathname-directory (fixup-pathname-directory dummy2))
		      (filename (format nil "~A~A" pathname-directory dummy1)))
		 (setq *plot-code-directory* pathname-directory)
		 (when (write-file-overwrite-authorization filename)
		   (unix:unix-mkdir (ext:unix-namestring pathname-directory nil) #o777)
		   (when (probe-file (ext:unix-namestring pathname-directory nil))
		     (with-open-stream (*standard-output* (open filename :direction :output :if-does-not-exist :create))
				       (write-window-plot-form win))
		     (format t ";; File ~a written~%" filename))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Plot Data Extraction and Analysis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun PLOT-WINDOW-OVERLAYS (win)
  (length (g-value win :y-lists)))

(defun add-trace-analysis-to-plot-menu (&optional win)
  (let* ((windows (coerce-to-list (or win (win-menu))))
	 (dummy1 nil)			; append-to-existing-comment
	 (dummy2 nil)			; return-strings
	 (dummy3 0)			; trace
	 (dummy4 0.0)			; y-base
	 (dummy5 :upper-right)		; position
	 (dummy6 0)			; overlay-index
	 dummy7)
    (flatten-no-nils-list
     (loop for win in windows do
	   (choose-variable-values
	    `((dummy1 "Append to existing comment" :boolean)
	      (dummy2 "Return strings" :boolean)
	      (dummy3 "Trace" :integer)
	      (dummy7 "All traces (=> ignore above spec)" :boolean)
	      (dummy4 "Y base for integral" :number)
	      (dummy5 "Comment position" :choose ,*comment-positions*)
	      (dummy6 "Overlay index" :integer))
	    :label (format nil "Analysis of Plot Traces") :text (g-value win :title))
	   collect (add-trace-analysis-to-plot win :append-to-existing-comment dummy1
					       :return-strings dummy2
					       :trace (if dummy7 :all dummy3)
					       :y-base dummy4
					       :position dummy5
					       :overlay-index dummy6)))))
			      
(defun add-trace-analysis-to-plot (&optional win &key append-to-existing-comment return-strings trace (y-base 0.0) (position :upper-right) (overlay-index 0))
  "Adds analysis result of traces in plotting WIN [if NIL then a menu is given] at POSITION [default :UPPER-RIGHT], including integral relative to
Y-BASE [default 0], maximum and minimum. Chosen traces are determined by the TRACE [default NIL] and OVERLAY-INDEX [default 0] arguments, as described
for the function EXTRACT-PLOT-WINDOW-DATA. If RETURN-STRINGS is T then a list of all generated strings is returned."
  (loop for win in (or (coerce-to-list win) (win-menu)) collect
	(let* ((overlays (case overlay-index
			   (:all (list-of-nums (plot-window-overlays win) 0 1))
			   (t (coerce-to-list overlay-index))))
	       (strings
		(concatenate-string-list 
		 (loop for overlay in overlays nconcing
		       (multiple-value-bind (data labels)
			   (extract-plot-window-data win trace overlay)
			 (loop for xy-lists in data
			       for label in labels
			       when data
			       collect (plot-trace-analysis-string (cadr xy-lists)
								   (when (> (length (g-value win :label-list)) 1) label)
								   (car xy-lists) y-base))))
		 :lf-count 1)))
	  (add-comment win strings :append-to-old-comment append-to-existing-comment :position position)
	  strings) into out
	finally (when return-strings (return out))))
	 
(defun plot-trace-analysis-string (data-list label time-list y-base)
  (format nil "~AInt ~,2e~A, Max ~,2e, Min ~,2e"
	  (if label (format nil "~A: " label) "")
	  (integrate-x-y data-list time-list :y-base y-base)
	  (if (and y-base (/= y-base 0)) (format nil "[~,2e]" y-base) "")
	  (max-of-list data-list)
	  (min-of-list data-list)))
  
(defun integrate-plot-window-data (&key win trace x-max x-min (y-base 0.0) average)
  (values-list
   (loop for xy-lists in (extract-plot-window-data win trace)
	 collect (integrate-x-y (cadr xy-lists) (car xy-lists) :average average :x-max x-max :x-min x-min :y-base y-base))))

(defun extract-plot-window-y-list (win trace overlay-index)
  (nth trace (nth overlay-index (g-value win :y-lists))))

(defun extract-plot-window-x-list (win trace overlay-index)
  (let ((y-list (extract-plot-window-y-list win trace overlay-index)))
    (when y-list
      (let ((overlay-x-lists (nth overlay-index (g-value win :x-lists))))
	(if (numberp overlay-x-lists)
	    (list-of-nums (length y-list) (g-value win :delta-t-start) overlay-x-lists)
	    (nth (min (1- (length overlay-x-lists)) trace) overlay-x-lists))))))


(defun extract-plot-window-data (&optional win (trace :menu) (overlay-index 0))
  "Extract one or more plot data lists from WIN. Prompts for non-specfied WIN. For overlayed plots, retrieves the overlay according to OVERLAY-INDEX,
default 0, referenced from the last overlay. Returns as values DATA and LABELS. DATA is of the form:

        (((x-list) (y-list)) ... ((x-list) (y-list))) 

If TRACE is nil then the first trace is returned. Otherwise, if TRACE is an integer or a list of integers, then the traces corresponding to those
numbers [starting from 1] are returned.  If TRACE is :ALL, then all data lists are included. If TRACE is :MENU, the default, then a menu for the
traces is given."
  (let ((win (or win (win-menu "Select Plot Window for Data" (standard-plot-windows) nil t))))
    (when win
      (let* ((sequence-numbers
	      (case trace
		(:all (list-of-nums (length (car (g-value win :y-lists))) 0 1))
		(:menu (coerce-to-list
			(choose-list-values-from-keys
			 (loop for y-seq in (car (g-value win :y-lists)) for count from 0
			       collect (list (or (when (> (length (nth count (g-value win :label-list))) 0)
						   (nth count (g-value win :label-list)))
						 (format nil "Trace ~A" (+ count 1)))
					     (+ 0 count)))
			 nil		; :only-one-choice trace
			 :label (format nil "Choose Trace Data From ~A" (g-value win :title)))))
		(t (if trace (coerce-to-list trace) '(0)))))
	     (labels (loop for trace in sequence-numbers
			   collect (nth trace (g-value win :label-list))))
	     (y-data (no-nils (loop for trace in sequence-numbers collect (extract-plot-window-y-list win trace overlay-index))))
	     (x-data
	      (no-nils
	       (loop for trace in sequence-numbers
		     collect
		     (let ((x (extract-plot-window-x-list win trace overlay-index)))
		       (typecase x
			 (cons x)
			 (number
			  (list-of-nums (length (extract-plot-window-y-list win trace overlay-index)) 0 x))))))))
	(values (loop for x in x-data for y in y-data
		      collect (list x y))
		(when (>= (length labels) (length x-data))  
		  (loop for x in x-data 
			for label in labels
			collect label)))))))

(defun density-plot (array &key win (title "2D Plot")
			   ; (scale 0.8)
			   (overlay nil) (element-aspect-ratio 1.0)
			   (color nil) (x-inc 1) (y-inc 1) (width 400) (height 400)
			   border
			   (vertical-border 50)
			   (side-border 50)
			   left-border
			   right-border
			   (x-axis-tick-skip 0) (x-axis-tick-mark-skip 0)
			   (y-axis-tick-skip 0) (y-axis-tick-mark-skip 0)
			   (x-axis-p t) (y-axis-p t)
			   x-are-fns y-are-fns
			   x-min x-max y-min y-max (x-label "") (y-label "") z-max z-min
			   invert)
  "BORDER, WIDTH, HEIGHT are in pixels."
  (declare				; (optimize (safety 0) (speed 3) (space 0))
   (type (array single-float (* *)) array)
   (fixnum width height			; x-inc y-inc
	   ))
  (let ((overlay (and overlay (or win (FIND-2dPLOT-WINDOW title))))
	(vertical-border (round (or border vertical-border)))
	(side-border-new (round
			  (if (and left-border right-border)
			    (/ (+ left-border right-border) 2)			  
			    (or border side-border))))
	(element-aspect-ratio (s-flt element-aspect-ratio)))

    (unless win (setq win (get-plot-window :2dplot nil overlay :name title :mode :2dplot :width width :height height :preserve-window-dimensions t)))
    (when win
      (s-value win :x-axis-p x-axis-p) 			   
      (s-value win :y-axis-p y-axis-p) 			   
      (s-value win :x-axis-tick-skip x-axis-tick-skip)
      (s-value win :x-axis-tick-mark-skip x-axis-tick-mark-skip)
      (s-value win :y-axis-tick-skip y-axis-tick-skip)
      (s-value win :y-axis-tick-mark-skip y-axis-tick-mark-skip)
      (s-value win :x-are-fns x-are-fns) (s-value win :y-are-fns y-are-fns)
      (let* ((width (the fn (g-value win :width)))
	     (height (the fn (g-value win :height)))
	     (array-width-x (array-dimension array 0))
	     (array-width-y (array-dimension array 1))
	     (z-max (s-flt (or z-max (2d-array-max array))))
	     (z-min (s-flt (or z-min (2d-array-min array))))
	     (z-amp (the sf (- z-max z-min)))
	     (rect-width-ref (/ (- width (* 2 side-border-new)) (the fn array-width-x)))
	     (rect-height-ref (/ (- height (* 2 vertical-border)) (the fn array-width-y)))
	     (rect-width (round (if (> element-aspect-ratio 1)
				  (/ rect-width-ref element-aspect-ratio)
				  rect-width-ref)))
	     (rect-height (round (if (> element-aspect-ratio 1)
				   rect-height-ref
				   (/ rect-height-ref element-aspect-ratio))))
	     (left-border (round (or left-border side-border-new)))
	     (top-border vertical-border) ; In pixels
	     (x-min (or x-min 0))
	     (y-min (or y-min 0))
	     (x-max (or x-max
			(* x-inc array-width-x)))
	     (y-max (or y-max
			(* y-inc array-width-y))))
	(declare (fixnum rect-width rect-height left-border top-border array-width-x array-width-y))
	(when (> z-amp 0)
	  (s-value win :y-origin-tick t)
	  (resurrect-opal-win win :visible t :update t)
	  (remove-virtual-aggs win)
	  (s-value win :width width) (s-value win :height height)
	  (resurrect-opal-win win)
	  (add-density-plot-axes win x-min x-max y-min y-max x-label y-label
				 array-width-x array-width-y
				 left-border rect-width
				 top-border rect-height)
	  (add-2d-density-array win array array-width-x array-width-y left-border top-border
				rect-width rect-height z-min z-amp color invert)
	  win)))))

(defun plot-2d-array (array &key win (title "2-D Data Plot")
			    ; (scale 0.8)
			    (overlay nil)
			    (x-inc 1) (y-inc 1) (width 400)(height 400)  (border 50)
			    (element-aspect-ratio 1) comment)
  (let ((win (density-plot array :win win :title title ; :scale scale
			   :overlay overlay
			   :x-inc x-inc :y-inc y-inc :width width :height height :border border
			   :element-aspect-ratio element-aspect-ratio)))
    (add-local-and-global-comment comment nil win)))

(defun density-histo-plot (x-data x-incs y-data y-incs &key
				  (dynamic-range 100)
				  (increment 1)
				  (white-is-maximum-p t)
				  x-are-fns y-are-fns
				  (element-aspect-ratio 1)
				  (border 75)
				  print-out-max-mins
				  (title "Histo Plot")
				  z-max x-min x-max y-min y-max
				  (x-axis-tick-skip 0) (x-axis-tick-mark-skip 0)
				  (y-axis-tick-skip 0) (y-axis-tick-mark-skip 0)
				  (width 400) (height 400)
				  (x-label "") (y-label ""))
  (density-plot (make-2d-histo-array x-data y-data x-incs y-incs
				     :x-min x-min :x-max x-max :y-min y-min :y-max y-max
				     :increment increment :print-out-max-mins print-out-max-mins)
		:title title :border border :width width :height height 
		:invert (not white-is-maximum-p)
		:z-min 0 :z-max z-max :x-min x-min :x-max x-max :y-min y-min :y-max y-max
		:element-aspect-ratio element-aspect-ratio
		:x-axis-tick-skip x-axis-tick-skip :x-axis-tick-mark-skip x-axis-tick-mark-skip
		:y-axis-tick-skip y-axis-tick-skip :y-axis-tick-mark-skip y-axis-tick-mark-skip
		:x-are-fns x-are-fns :y-are-fns y-are-fns
		:x-label x-label :y-label y-label))

(export '(add-global-plot-comment
	  NUMBER-OF-OVERLAYS
	  limit-sig-figs
	  ADD-TRACE-ANALYSIS-TO-PLOT
	  PLOT-TRACE-ANALYSIS-STRING
	  *DEFAULT-PLOT-GRID-P*
	  *global-plot-comment*
	  *global-plot-comment-position*
	  scatter-statistics-comment
	  plot-xy-data plot-timed-data
	  PLOT-POINTS
	  plot-scatter
	  BASIC-HISTO 
	  plot-histogram
	  plot-histogram-list
	  plot-polar-data
	  plot-polar-vectors
	  plot-polar-simple-array
	  ADD-TRACE
	  dump-plot-to-lisp-file
	  extract-plot-window-data
	  extract-plot-window-y-list
	  extract-plot-window-x-list
	  
	  grab-and-store-plot-data
	  integrate-plot-window-data
	  density-plot
	  density-histo-plot
	  plot-2d-array))
