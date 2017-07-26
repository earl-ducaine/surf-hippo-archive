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


;; GUI Source file: plot-hack.lisp

;;; This contains some basic plotting routines, inspired by the PLOT-HACK system written by Patrick
;;; O'Donnell at the MIT AI Lab for the Symbolics window system, and built upon the Garnet GUI
;;; toolset from CMU. This system requires that WINDOW-HACK and MENU-HACK be loaded. See the
;;; plotting.doc file for more information.

(in-package "SON-OF-PLOT-HACK")

(defvar *label-plot-traces* t "Default enable for trace labels in plots.")
(defvar *gap-btwn-x-label-and-tick-marks* 10) ; pixels

(defvar *x-axis-tick-mark-length* 5 "In pixels. If negative, X axis tick marks will point away from tick labels.")
(defvar *y-axis-tick-mark-length* 5 "In pixels. If negative, Y axis tick marks will point away from tick labels.")


(defvar *trace-keys-top* 10)
(defvar *trace-keys-left* 10)
(defvar *trace-keys-middle* 45)
(defvar *trace-keys-right* 60)
;; (defvar *trace-labels-left* 70)
(defvar *trace-key-gap* 4)		; A gap in pixels between each trace label key.
(proclaim '(fixnum *trace-key-gap*
	    *trace-keys-top* *trace-keys-middle* *trace-keys-left* *trace-keys-right* ;; *trace-labels-left*
	    ))


(defvar *accomodate-all-overlays* nil)
(defvar *preserve-plot-layout* nil)
(defvar *overlay-all-plots* nil)

(defvar *default-x-label* "")
(defvar *default-y-label* "")

(defvar *plot-scale-bar-horizontal-space* 20)
(defvar *plot-scale-bar-vertical-space* 45)


(defvar *x-plot-left-gap* 65)		; The distance in pixels of the scaled rectangle from the left of the window.
(defvar *x-plot-right-gap* 50)		; The distance in pixels of the scaled rectangle from the right of the window.
(proclaim '(fixnum *x-plot-left-gap* *x-plot-right-gap*))

(defvar *y-plot-bottom-gap* 50)		; The distance in pixels of the scaled rectangle off the bottom of
					; the window. Note that the top gap is variable, depending on the
					; number of trace labels required (:label-height).
(proclaim '(fixnum *y-plot-bottom-gap*))

(defvar *default-y-plot-top-gap-extra-waterfall* 20)

(defvar *x-plot-fudge* 0.05)
(defvar *y-plot-fudge* 0.05)

(defvar *x-plot-win-minimum-margin* -20)
(defvar *x-plot-win-maximum-margin* 20)
(defvar *y-plot-win-minimum-margin* -20)
(defvar *y-plot-win-maximum-margin* 20)

(defvar *create-new-plot-windows* nil)	; Create a new set of plot windows.
(defvar *create-new-plot-window-types* nil)

(defvar *GET-PLOT-POINT-LIST-margin* 100)

(defvar *x-trace-offset* 0.0) ; used for automatic waterfall setup
(defvar *x-plot-left-gap-waterfall* 30)
(defvar *auto-waterfall-y-trace-overlap* 0.0)
(defvar *waterfall-fixed-y-max* nil)
(defvar *waterfall-fixed-y-min* nil)
  
(defvar *simple-axis-x* nil)
(defvar *simple-axis-y* nil)

(defvar *plot-line-style* :thin-colors)
;; (defvar *default-plot-line-style* :thin-colors)

(defvar *connect-data-points t)		; Connect the dots in the data plots.
(defvar *default-scatter-size* 7)	; pixels
(defvar *plot-axis-font* opal:default-font)

(defvar *use-my-float-format-for-plots* t)

(defvar *default-grid-line-style*
  (create-instance nil (create-instance nil dashed-line (:constant nil)) (:dash-pattern (list 1 8))))

(defvar *default-axis-line-style* opal:line-1)

(defun window-plot-axis-font (win)
  (if win
      (or (g-value win :plot-axis-font) (g-value win :font))
      (opal:get-standard-font :serif :bold-italic :medium)))

(defun plot-window-font (win)
  (window-plot-axis-font win))

(defun window-comment-font (win)
  (if win
      (or (g-value win :comment-font) (g-value win :font))
      (opal:get-standard-font :serif :bold-italic :medium)))

(defun plot-window-string-width (win string)
  (opal::string-width (window-plot-axis-font win) string))


(create-instance 'plot-window
		 basic-graphics-window
		 (:mode :standard-plot)
		 (:plot-type :xy)
		 (:data-type nil)	; This will be used to keep track of where to put data.
		 
		 (:view-angle-comment-p t)
		 
		 (:plot-line-style (o-formula *plot-line-style*)) (:connect-data-points t)
		 (:scatter-symbol :dot) (:fill-scatter t) (:scatter-symbol-borderp t) (:scatter-symbol-units-in-pixels t)
		 (:y-symbol-width *default-scatter-size*) (:x-symbol-width *default-scatter-size*)

		 (:axes-type :standard)
		 (:plot-axis-font (o-formula *plot-axis-font*))

		 ;; Axes labels
		 (:y-label "") (:x-label "")
		 (:label-height 0)	; this is in pixels
		 (:x-label-vertical-position :below)
		 (:x-label-horizontal-position :center) ; :left :center :right
		 (:y-label-vertical-position :two-thirds-up)
		 (:y-label-horizontal-position :left)
		 
		 ;; For simple axes
		 (:x-scale-t% 20) (:y-scale-t% 20) (:x-scale-l% 80)
		 (:y-scale-bar-left-percent 80)

		 ;; These gaps in pixels frame the plotting area.
		 (:x-plot-left-gap 0)		 (:x-plot-right-gap 0)
		 (:y-plot-bottom-gap 0)		 (:x-plot-right-gap-extra 0)
		 
		 ;;These values are in data coordinates, and should be floats.
		 (:y-max 0.0) (:y-min 0.0) (:y-origin 0.0) (:y-mag 0.0)
		 (:x-max 0.0) (:x-min 0.0) (:x-origin 0.0) (:x-mag 0.0)
		 (:y-inc) (:x-inc)

		 (:include-border-points t)		 (:apply-horizontal-borders t)		 (:apply-vertical-borders nil)

		 ;; For regular axes
		 (:include-x-tick-at-0 t)		 (:include-y-tick-at-0 t)
		 (:x-axis-p t)		 (:y-axis-p t)
		 (:x-axis-tick-skip 0)		 (:y-axis-tick-skip 0)
		 (:x-axis-tick-mark-skip 0)		 (:y-axis-tick-mark-skip 0)

		 ;; If these are negative, axis tick marks will point away from tick labels.
		 (:x-axis-tick-mark-length *x-axis-tick-mark-length*)		 (:y-axis-tick-mark-length *y-axis-tick-mark-length*)

		 ;; For waterfall plots
		 (:waterfall-trace-label-skip 0)		 (:gap-between-trace-and-waterfall-label 20)		 (:skirt-to-window-border t)
					      
		 ;; These slots (pixel units) are for clipping transformed polylines just outside viewing.
		 (:y-bound-max (o-formula (+ (gvl :height) 100)))
		 (:y-bound-min -100)
		 (:x-bound-max (o-formula (+ (gvl :width) 100)))
		 (:x-bound-min -100)
		 (:min-max-lists '())	; Used for zooming/unzooming.

		 (:current-xfrm (3-by-3-identity)) ; For 3d plots

		 ;; Must be single-floats
		 
		 (:x-trace-offset 0.0) (:y-trace-offset 0.0) ; For waterfall plots
		 (:x-data-offset 0.0) (:y-data-offset 0.0)
		 (:waterfall-base-x-offset 0.0) (:waterfall-base-y-offset 0.0))


(create-instance 'axis-text opal:aggregadget
		 (:string "")
		 (:label-position :center) ; Determines horizontal position (:LEFT) of text relative to the :LEFT slot.
		 (:parts
                  `((:label ,window-hack-text
			    (:orientation ,(o-formula (gvl :parent :orientation)))
			    (:text ,(o-formula (gvl :parent :string)))
			    (:left ,(o-formula (- (the fn (gvl :parent :left))
						  (case (gvl :parent :label-position)
						    ((:LEFT-UP :LEFT-DOWN :LEFT) (the fn (gvl :width)))
						    ((:RIGHT-UP :RIGHT-DOWN :RIGHT) 0)
						    ;; :CENTER
						    (t (round (* 0.5 (the fn (gvl :width)))))))))
			    (:top ,(o-formula (the fn (gvl :parent :top))))
			    (:font ,(o-formula (window-plot-axis-font (gvl :window)))))))
		 (:interactors
		  `((:text-inter ,inter:text-interactor
				 (:window ,(o-formula (gv-local :self :operates-on :window)))
				 (:feedback-obj nil)
				 (:start-where ,(o-formula (list :in (gvl :operates-on :label))))
				 (:abort-event :CONTROL-\g)
				 (:stop-event (:leftdown))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;  Functions for accessing plot windows.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun standard-plot-windows ()
  (windows-of-mode :standard-plot))

(defun all-plot-windows ()
  (windows-of-mode '(:scanner :standard-plot :histogram :2dplot :3dplot)))

;; FIND-PLOT-WINDOW Return a window according to spec if there is one in *OUTPUT-WINDOWS*.
(defun find-plot-window (plot-type data-type &optional name (modes-to-search '(:scanner :standard-plot :histogram :2dplot :3dplot)))
  (unless (or *create-new-plot-windows* *create-new-plot-window-types*)
    (loop for window in (clean-up-*output-windows*) ; Is there already the right kind of window?
	  when (and
		(not (g-value window :locked))
		(member (g-value window :mode) modes-to-search)
		(or (not plot-type) (equal plot-type (g-value window :plot-type)))
		(cond
		 ((and (stringp data-type) (stringp (g-value window :data-type)))
		  (string-equal data-type (g-value window :data-type)))
		 (data-type (eq data-type (g-value window :data-type)))
		 (t t))
		(or (not name)
		    (string-equal name (g-value window :title))
		    (string-equal name (g-value window :session-name))))
	  do (return window))))

(defun create-plot-window (&key width height mode plot-type data-type prototype)
  (let ((win (create-instance nil plot-window (:icon-title "SH Plot")
			      (:mode mode) (:data-type data-type) (:plot-type plot-type)
			      (:omit-title-bar-p *omit-title-bar*)
			      (:aggregate (create-instance nil opal:aggregate))
			      (:plot-line-style *plot-line-style*))))
    (when prototype (TRANSFER-basic-plot-window-SLOTS prototype win))
    (initialize-graphics-window win :width width :height height)
    (add-plotting-interactors win)
    (clear-and-add-plot-agg win `data-plot)
    (push win *output-windows*)
    (update-*Twin*)
    win))

(defun get-plot-window (plot-type data-type overlay &key name session-name width height left top (mode :standard-plot)
				  prototype default-win prompt-for-overlay (save-markers t)
				  (*create-new-plot-windows* *create-new-plot-windows*)
				  preserve-window-dimensions preserve-plot-layout accomodate-all-overlays
				  (child-number 0))
  (let* ((win (or default-win
		  (unless *create-new-plot-windows* (find-plot-window plot-type data-type (if (> (length session-name) 0) session-name name)))))
	 (width (if (and win (or (not width) preserve-window-dimensions)) (g-value win :width) width))
	 (height (if (and win (or (not height) preserve-window-dimensions)) (g-value win :height) height))
	 cancel)
    (when (and prompt-for-overlay win)
      (let ((dummy1 (cond (accomodate-all-overlays :overlay_and_accomodate_new_data)
			  (overlay :overlay_and_retain_coordinates)
			  (*create-new-plot-windows* :make_new_window)
			  (preserve-plot-layout :erase_and_retain_coordinates)
			  (t :erase_old_data))))
	(choose-variable-values
	 `((dummy1 ""
		   :choose (:overlay_and_accomodate_new_data :overlay_and_retain_coordinates
							     :erase_and_retain_coordinates :make_new_window :erase_old_data :cancel_plot)
		   :vertical :rank-margin 6))
	 :text (format nil "~A~%Plot Overlay Options" (g-value win :title)) :label "Plot Overlay Menu")
	(case dummy1
	  (:cancel_plot (setq cancel t))
	  (:erase_old_data (setq *create-new-plot-windows* nil
				 accomodate-all-overlays nil
				 preserve-plot-layout nil
				 overlay nil))
	  (:erase_and_retain_coordinates (setq preserve-plot-layout t overlay nil))
	  (:overlay_and_accomodate_new_data (setq *create-new-plot-windows* nil
						  accomodate-all-overlays t
						  overlay t))
	  (:overlay_and_retain_coordinates (setq *create-new-plot-windows* nil
						 accomodate-all-overlays nil
						 overlay t))
	  (:make_new_window (setq *create-new-plot-windows* t
				  accomodate-all-overlays nil
				  overlay nil)))
	(setq overlay (and win overlay))))
    (unless cancel
      (cond ((or (not win) *create-new-plot-windows*)
	     (setq win (create-plot-window :data-type data-type :mode mode :prototype prototype :plot-type plot-type))
	     (increment-plotting-window-top)
	     (set-window-title win (or name data-type) prototype))
	    ((not overlay)		;If no overlay, destroy old agg
	     (clear-and-add-plot-agg win `data-plot)))

      (s-value win :data-to-points-function (plot-data-to-points-function win))
      (s-value win :plot-type plot-type)
      (when data-type (s-value win :data-type data-type))
      (s-value win :overlay overlay)
      (s-value win :child-number child-number)
      (unless overlay
	(when width (s-value win :width width))
	(when height (s-value win :height height)))
      (when session-name (s-value win :session-name  session-name))
      (s-value win :preserve-plot-layout (and (> (length (g-value win :x-lists)) 0) (not accomodate-all-overlays) preserve-plot-layout))
      (s-value win :overlay (and (> (length (g-value win :x-lists)) 0) (or (g-value win :overlay) overlay)))
      (s-value win :accomodate-all-overlays (and ; (> (number-of-overlays win) 0)
					     accomodate-all-overlays))
      (when left (s-value win :left (round left)))
      (when top  (s-value win :top (round top)))
      (s-value win :save-markers save-markers)
      (unless (or (g-value win :overlay) save-markers) (remove-all-markers win)))
    win))

(defun get-child-plot-window (parent &optional (same-size t))
  (let* ((*create-new-plot-windows* t)
	 (child (create-instance nil parent
				 (:name (format nil "~a-~d" (g-value parent :title) (1+ (g-value parent :child-number))))
				 (:prototype parent)
				 (:aggregate (create-instance nil opal:aggregate)))))
    (set-window-title child (g-value child :name) parent)
    (initialize-graphics-window child)
    (add-plotting-interactors child)
    (clear-and-add-plot-agg child `data-plot)
    (push child *output-windows*)
    (update-*Twin*)
    (s-value child :data-to-points-function (plot-data-to-points-function child))
    (s-value child :child-number (1+ (g-value parent :child-number)))
    (resurrect-opal-win child)
    child))

(defun find-2dplot-window (plot-type data-type &optional name)
  (find-plot-window plot-type data-type name '(:2dplot)))

(defun set-window-title (win title &optional prototype)
  (s-value win :title (If prototype (string title) (GET-win-TITLE-STRING (string title)))))

(defun increment-plotting-window-top ()
  (setq *plotting-window-top* (if *output-windows* (max 20 (mod (+ 20 *plotting-window-top*) 500)) 20)))


(defun TRANSFER-basic-plot-window-SLOTS (prototype win)
  (TRANSFER-SCHEMA-SLOTS prototype win
			 '(:x-log :y-log :log-base
			   :y-are-fns :x-are-fns :grid-line-style
			   :data-type :plot-type :mode
			   :preserve-plot-layout :overlay :accomodate-all-overlays
			   :comment-font
			   :y-label :x-label
			   :x-label-horizontal-position :x-label-vertical-position
			   :y-label-horizontal-position :y-label-vertical-position
			   :plot-line-style :axes-type
			   :label-height :label-list :label-traces
			   :x-lists :y-lists
			   :x-data-offset :y-data-offset :x-trace-offset :y-trace-offset :y-inc :x-inc :y-max :x-max
			   :x-axis-p :x-axis-tick-skip :x-axis-tick-mark-skip
			   :y-axis-tick-skip :y-axis-tick-mark-skip :waterfall-trace-label-skip
			   :min-max-lists
			   :x-trace-offset :y-trace-offset
			   :x-scale-t% :y-scale-t% :x-scale-l% :y-scale-bar-left-percent
			   :x-plot-right-gap-extra
			   :scatter-symbol :scatter-symbol-units-in-pixels
			   :plot-axis-font :y-symbol-width :x-symbol-width)))

(defun erase-plot-data (window)
  (s-value window :x-lists nil)
  (s-value window :y-lists nil)
  (s-value window :data-erased t))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Coordinate transformation
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; X-PLOT-WIN, Y-PLOT-WIN - These translate from data coordinates to plot window coordinates in
;;; such a way as to keep the data contained within a scaled rectangle that in turn is centered in
;;; the plot window. A separate function is needed for x and y since the origin of opal:windows is
;;; at the upper left hand corner. This requires a flipping of the y values. The returned values are
;;; clipped by the :Y-BOUND-MAX, :Y-BOUND-MIN, :X-BOUND-MAX, :X-BOUND-MIN slots in WIN so that we
;;; don't get wrap-around from illegal window coordinates.

(proclaim '(inline x-plot-win-float-unbounded))
(defun x-plot-win-float-unbounded (x-dat win)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type KR::SCHEMA win)
	   (single-float x-dat)
	   (values fixnum))
  (the fn (+ (the fn (g-value win :x-plot-left-gap))
	     (the fn (round (* (/ (- x-dat (the sf (g-value win :x-min))) (the sf (g-value win :x-mag)))
			       (the fn (g-value win :plot-area-width))))))))

(proclaim '(inline x-plot-win-float-unbounded-w-win-args))
(defun x-plot-win-float-unbounded-w-win-args (x-dat x-plot-left-gap x-min x-mag plot-area-width)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (single-float x-dat x-mag x-min)
	   (FIxnum plot-area-width  x-plot-left-gap)
	   (values fixnum))
  (+ (the fn (round (* (/ (- x-dat x-min) x-mag)
		       plot-area-width)))
     x-plot-left-gap))
     

(proclaim '(inline x-plot-win-float-bounded-w-win-args))
(defun x-plot-win-float-bounded-w-win-args (x-dat x-min-limit x-max-limit
						  x-bound-min x-bound-max
						  width x-plot-left-gap x-min x-mag plot-area-width)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (single-float x-dat x-min-limit x-max-limit)
	   (fixnum width x-bound-min x-bound-max)
	   (values fixnum))
  (cond
   ((> x-dat x-max-limit) (+ (the fn *x-plot-win-maximum-margin*) width))
   ((> x-min-limit x-dat) *x-plot-win-minimum-margin*)
   (t (bound-int-val
       (x-plot-win-float-unbounded-w-win-args x-dat x-plot-left-gap x-min x-mag plot-area-width)
       x-bound-max x-bound-min))))

(proclaim '(inline x-plot-win-float-bounded))
(defun x-plot-win-float-bounded (x-dat win)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type KR::SCHEMA win)
	   (single-float x-dat)
	   (values fixnum))
  (cond
    ((> x-dat (the sf (g-value win :x-max-limit))) (+ (the fn *x-plot-win-maximum-margin*) (the fn (g-value win :width))))
    ((> (the sf (g-value win :x-min-limit)) x-dat) *x-plot-win-minimum-margin*)
    (t (bound-int-val
	(x-plot-win-float-unbounded x-dat win) (the fn (g-value win :x-bound-max)) (the fn (g-value win :x-bound-min))))))

(proclaim '(inline x-plot-win-float))
(defun x-plot-win-float (x-dat win &optional boundit)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type KR::SCHEMA win)
	   (single-float x-dat)
	   (values fixnum))
  (if boundit (x-plot-win-float-bounded x-dat win) (x-plot-win-float-unbounded x-dat win)))

(proclaim '(inline x-plot-win))
(defun x-plot-win (x-dat win &optional boundit)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type KR::SCHEMA win)
	   (values fixnum))
  (let ((x-dat (typecase x-dat
		 (single-float x-dat)
		 (t (coerce x-dat 'single-float)))))
    (declare (single-float x-dat))
    (x-plot-win-float x-dat win boundit)))


(defun x-plot-win-ok (x-dat win)
  (case (type-of (round (* (-  (-  (g-value win :width)
				   (g-value win :x-plot-right-gap))
			       (g-value win :x-plot-left-gap))
			   (/ (- (g-value win :x-max) (g-value win :x-min))
			      (g-value win :x-mag)))))
    (fixnum t)
    (t nil)))


(defun x-plot-win-inv (x-win win)
  (let ((width (g-value win :width))
	(min (g-value win :x-min))
	(mag (g-value win :x-mag)))
    (+ min
       (* (/ mag (- (- width  (g-value win :x-plot-right-gap)) (g-value win :x-plot-left-gap)))
	  (- x-win (g-value win :x-plot-left-gap))))))


(proclaim '(inline y-plot-win-float-unbounded))
(defun y-plot-win-float-unbounded (y-dat win)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type KR::SCHEMA win)
	   (single-float y-dat)
	   (values fixnum))
  (+ (the fn (round (* (the fn (g-value win :plot-area-height))
		       (/ (- (the sf (g-value win :y-min)) y-dat)
			  (the sf (g-value win :y-mag))))))
     (- (the fn (g-value win :height))
	(the fn (g-value win :y-plot-bottom-gap)))))

(proclaim '(inline y-plot-win-float-unbounded-w-win-args))
(defun y-plot-win-float-unbounded-w-win-args (y-dat y-plot-bottom-gap y-min y-mag plot-area-height height)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (single-float y-dat y-min y-mag)
	   (fixnum y-plot-bottom-gap height plot-area-height)
	   (values fixnum))
  (+ (the fn (round (* (the fn plot-area-height)
		       (/ (- y-min y-dat) y-mag))))
     (- height y-plot-bottom-gap)))

(proclaim '(inline y-plot-win-float-bounded))
(defun y-plot-win-float-bounded (y-dat win)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type KR::SCHEMA win)
	   (single-float y-dat)
	   (values fixnum))
  (cond
    ((> y-dat (the sf (g-value win :y-max-limit))) *y-plot-win-minimum-margin*)
    ((> (the sf (g-value win :y-min-limit)) y-dat) (+ *y-plot-win-maximum-margin* (the fn (g-value win :height))))
    (t (bound-int-val
	(y-plot-win-float-unbounded y-dat win) (the fn (g-value win :y-bound-max)) (the fn (g-value win :y-bound-min))))))

(proclaim '(inline y-plot-win-float-bounded-w-win-args))
(defun y-plot-win-float-bounded-w-win-args (y-dat y-min-limit y-max-limit
						  y-bound-min y-bound-max
						  height y-plot-bottom-gap y-min y-mag plot-area-height)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (single-float y-dat y-min-limit y-max-limit)
	   (fixnum y-bound-min y-bound-max height plot-area-height)
	   (values fixnum))
  (cond
   ((> y-dat y-max-limit) (the fn *y-plot-win-minimum-margin*))
   ((> y-min-limit y-dat) (+ *y-plot-win-maximum-margin* height))
   (t (bound-int-val
       (y-plot-win-float-unbounded-w-win-args y-dat y-plot-bottom-gap y-min y-mag plot-area-height height)
       y-bound-max y-bound-min))))


(proclaim '(inline y-plot-win-float))
;(proclaim '(notinline y-plot-win-float))
(defun y-plot-win-float (y-dat win &optional boundit)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type KR::SCHEMA win)
	   (single-float y-dat)
	   (values fixnum))
  (if boundit (y-plot-win-float-bounded y-dat win) (y-plot-win-float-unbounded y-dat win)))


(proclaim '(inline y-plot-win))
(defun y-plot-win (y-dat win &optional boundit)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (type KR::SCHEMA win)
	   (values fixnum))
  (let ((y-dat (typecase y-dat
		 (single-float y-dat)
		 (t (coerce y-dat 'single-float)))))
    (declare (single-float y-dat))
    (y-plot-win-float y-dat win boundit)))


(defun y-plot-win-inv (y-win win)
  (let ((height (g-value win :height))
	(label-height (g-value win :label-height))
	(min (g-value win :y-min))
	(mag (g-value win :y-mag)))
    (+ min
       (* (/ mag (- (g-value win :y-plot-bottom-gap) ; *y-plot-bottom-gap* FIXED, 8/27/94 lbg 
		    (- height label-height)))
	  (- y-win (- height (g-value win :y-plot-bottom-gap)))))))


(defun xy-plot-win-to-points (x-dat y-dat win)
  (list (x-plot-win x-dat win t) (y-plot-win y-dat win t)))

(s-value plot-window :x-graphics-win-function #'x-plot-win)
(s-value plot-window :y-graphics-win-function #'y-plot-win)

(defun x-plot-win-distance (distance win)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (abs (- (x-plot-win distance win nil) (x-plot-win-float 0.0 win nil))))

(defun x-plot-win-distance-float (distance win)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (single-float distance))
  (abs (- (x-plot-win-float distance win nil) (x-plot-win-float 0.0 win nil))))

(defun y-plot-win-distance (distance win)
  (abs (- (y-plot-win distance win nil) (y-plot-win-float 0.0 win nil))))

(defun y-plot-win-distance-float (distance win)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (single-float distance))
  (abs (- (y-plot-win-float distance win nil) (y-plot-win-float 0.0 win nil))))

(defun plot-window-x-number-format (value win &key range decimals)
  (another-nice-float-format value :range (unless decimals (or range (/ (g-value win :x-inc) 100)))
					:decimals decimals ; (g-value win :x-tick-decimal)
			     ))

(defun plot-window-y-number-format (value win &key range decimals)
  (another-nice-float-format value :range (unless decimals
					    (or range (/ (g-value win :y-inc) 100)))
					:decimals decimals ; (g-value win :y-tick-decimal)
			     ))

(defun a-bit-more-sub-domain (y-seqs how-much x-seqs x-min x-max win)
  (when x-min (setq x-min (s-flt x-min)))
  (when x-max (setq x-max (s-flt x-max)))
  (typecase (car y-seqs)
    (cons (loop for y-seq in y-seqs maximize (a-bit-more-sub-domain y-seq how-much x-seqs x-min x-max win)))
    (t (let ((max (if (numberp x-seqs)
		      (loop for x from (or (g-value win :delta-t-start) 0) by x-seqs
			    for y in y-seqs
			    when (in-middle-kludge x-min x x-max) maximize y)
		      (loop for x in (car x-seqs)
			    for y in y-seqs
			    when (in-middle-kludge x-min x x-max) maximize y))))
	 (+ max (abs (* how-much max)))))))


(defun a-bit-less-sub-domain (y-seqs how-much x-seqs x-min x-max win)
  (when x-min (setq x-min (s-flt x-min)))
  (when x-max (setq x-max (s-flt x-max)))
  (typecase (car y-seqs)
    (cons (loop for y-seq in y-seqs minimize (a-bit-less-sub-domain y-seq how-much x-seqs x-min x-max win)))
    (t (let ((min (if (numberp x-seqs)
		      (loop for x from (or (g-value win :delta-t-start) 0) by x-seqs
			    for y in y-seqs
			    when (in-middle-kludge x-min x x-max) minimize y)
		      (loop for x in (car x-seqs)
			    for y in y-seqs
			    when (in-middle-kludge x-min x x-max) minimize y))))
	 (+ min (abs (* how-much min)))))))


;;;; SETUP-PLOT Sets up the plotting window parameters according to the plotted data.
;;; This can accept data in one of two formats:

;;   ** As used by PLOT-XY-DATA **
;;
;;  data-lists =
;;
;;  '(((x1 x1 ... x1)(y1 y1 ... y1))
;;    ((x2 x2 ... x2)(y2 y2 ... y2))
;;    ...
;;    ((xn xn ... xn)(yn yn ... yn))
;;   )
;;
;;  time-seq = nil

;; ** As used by PLOT-TIMED-DATA **
;;
;;  data-lists =
;;
;;  '((x1 x1 ... x1)
;;    (x2 x2 ... x2)
;;    ...
;;    (xn xn ... xn)
;;   )
;;
;;  time-seq = '(t0 t1 t2 ..) or an equivalent simple-array.

;; Also draws labelled axes.

;; Signals an error if a dimension that is to be logarithmically displayed contains a value less than or
;; equal to 0. 

(defun check-plot-log-parameters (win)
  (or
   (and (or (not (g-value win :x-log)) (positive-p (g-value win :x-overall-data-min)))
	(or (not (g-value win :y-log)) (positive-p (g-value win :y-overall-data-min))))
   (cond-every
    ((and (g-value win :x-log) (<= (g-value win :x-data-min) 0.0))
     (let ((dummy1 nil)
	   (dummy2 (* 1.000001 (- (g-value win :x-data-offset) (g-value win :x-data-min)))))
       (choose-variable-values
	`((:comment ,(format nil "Minimum X value: ~A" (- (g-value win :x-data-offset) (g-value win :x-data-min))))
	  (dummy2 "X offset" :float)
	  (dummy1 "Cancel X log" :boolean))
	:title (format nil "X Log error in ~A" (g-value win :title)))
       (cond ((not dummy1)
	      (s-value win :X-MAX-MIN-SPECIFIED nil)
	      (s-value win :x-data-offset dummy2))
	     (t (s-value win :x-log nil)))))
    ((and (g-value win :y-log) (<= (g-value win :y-data-min) 0.0))
     (let ((dummy1 nil)
	   (dummy2 (* 1.000001 (- (g-value win :y-data-offset) (g-value win :y-data-min)))))
       (choose-variable-values
	`((:comment ,(format nil "Minimum Y value: ~A" (- (g-value win :y-data-offset) (g-value win :y-data-min))))
	  (dummy2 "Y offset" :float)
	  (dummy1 "Cancel Y log" :boolean))
	:title (format nil "Y Log error in ~A" (g-value win :title)))
       (cond ((not dummy1)
	      (s-value win :Y-MAX-MIN-SPECIFIED nil)
	      (s-value win :y-data-offset dummy2))
	     (t (s-value win :y-log nil)))))
    (t nil))))

(defun num-curves-per-group (win)
  (length (car (g-value win :y-lists))))

(defun num-groups (win)
  (length (g-value win :y-lists)))

(defun get-plot-window-trace-order (win)
  (or (g-value win :trace-order)
      (list-of-nums (num-curves-per-group win))))

(defun parse-plot-y-seqs (win)
  (loop for trace-reference in (get-plot-window-trace-order win)
	when trace-reference collect (nth (round trace-reference) (car (g-value win :y-lists)))))

(defun parse-all-plot-x-seqs (win)
  (loop for trace-reference in (get-plot-window-trace-order win)
	when trace-reference nconc
	(loop for collection in (g-value win :x-lists) collect
	      (if (consp (car collection))
		(nth (round trace-reference) collection)
		collection))))

(defun parse-all-plot-y-seqs (win)
  (loop for trace-reference in (get-plot-window-trace-order win)
	when trace-reference nconc
	(loop for collection in  (g-value win :y-lists) collect
	      (nth (round trace-reference) collection))))

(defun x-data-max (win &optional all)
  (let ((x-seqs (if nil (parse-all-plot-x-seqs win) (car (g-value win :x-lists)))))
    (+ (g-value win :x-data-offset)
       (if (numberp x-seqs)
	   (+ (or (g-value win :delta-t-start) 0)
	      (* (1- (g-value win :data-length)) x-seqs))
	   (a-bit-more x-seqs)		; Just the actual max.
	   ))))
  
(defun x-data-min (win &optional all)
  (let ((x-seqs (if nil (parse-all-plot-x-seqs win) (car (g-value win :x-lists)))))
    (+ (g-value win :x-data-offset)
       (if (numberp x-seqs)
	 (or (g-value win :delta-t-start) 0)
	 (a-bit-less x-seqs)		; Just the actual min.
	 ))))

(defun y-data-max (win display-sub-domain x-min-spec x-max-spec &optional (all (g-value win :accomodate-all-overlays)))
  (let ((x-seqs (car (g-value win :x-lists)))
	(y-seqs (if all (parse-all-plot-y-seqs win) (parse-plot-y-seqs win))))
    (+ (g-value win :y-data-offset)
       (if display-sub-domain
	   (a-bit-more-sub-domain y-seqs 0.0 x-seqs x-min-spec x-max-spec win)
	   (a-bit-more y-seqs)		; Just the actual max.
	   ))))

(defun y-data-min (win display-sub-domain x-min-spec x-max-spec &optional (all (g-value win :accomodate-all-overlays)))
  (let ((x-seqs (car (g-value win :x-lists)))
	(y-seqs (if all (parse-all-plot-y-seqs win) (parse-plot-y-seqs win))))
    (+ (g-value win :y-data-offset)
       (if display-sub-domain
	 (a-bit-less-sub-domain y-seqs 0.0 x-seqs x-min-spec x-max-spec win)
	 (a-bit-less y-seqs)		; Just the actual min.
	 ))))

;; Sets :X-DATA-MAX, :X-DATA-MIN, :Y-DATA-MAX, :Y-DATA-MIN, and if :AUTO-WF-SETUP, :X-TRACE-OFFSET,
;; :Y-TRACE-OFFSET and :WATERFALL-LABEL-OFFSET. Returns when LOGS-OK.
(defun setup-xy-data-limits (win display-sub-domain timed-data x-min-spec x-max-spec)
  (let ((display-sub-domain (and display-sub-domain (or (not timed-data) (and x-min-spec (> x-min-spec 0.0)))))
	logs-ok)
    (loop until logs-ok do
	  (s-value win :x-data-max (x-data-max win))
	  (s-value win :x-data-min (x-data-min win))
	  (s-value win :x-overall-data-min (x-data-min win t))			
	  (setq display-sub-domain (or display-sub-domain (and x-min-spec (> x-min-spec (g-value win :x-data-min)))))
	  (s-value win :y-data-max (y-data-max win display-sub-domain x-min-spec x-max-spec))
	  (s-value win :y-data-min (y-data-min win display-sub-domain x-min-spec x-max-spec))
	  (s-value win :y-overall-data-min (y-data-min win display-sub-domain x-min-spec x-max-spec t))

	  (when (g-value win :auto-wf-setup)
	    (when *waterfall-fixed-y-max* (s-value win :y-data-max *waterfall-fixed-y-max*))
	    (when *waterfall-fixed-y-min* (s-value win :y-data-min *waterfall-fixed-y-min*))

	    (s-value win :x-trace-offset *x-trace-offset*)
	    (s-value win :y-trace-offset (* (- 1.0 (bound-val *auto-waterfall-y-trace-overlap* 1.0 0.0))
					    (- (g-value win :y-data-max) (g-value win :y-data-min))))
	      
	    (s-value win :waterfall-label-offset 0.0))

	  ;; If log used, test for a bad log domain value - menu if so...
	  (setq logs-ok (check-plot-log-parameters win)))))

      
(defun set-xy-max/min-spec-and-origins (win replot-win-point-list restore-plot unzoom
					    X-INC X-MAX-SPEC X-MIN-SPEC X-ORIGIN Y-INC Y-MAX-SPEC Y-MIN-SPEC Y-ORIGIN)
  ;; In case of a REPLOT-WIN-POINT-LIST, RESTORE-PLOT, or UNZOOM, the X/Y specified max/mins must be set.
  (s-value win :X-MAX-MIN-SPECIFIED (or x-max-spec x-min-spec))
  (s-value win :Y-MAX-MIN-SPECIFIED (or y-max-spec y-min-spec))
  (cond (replot-win-point-list		; Converted mouse coordinates (left top width height) to data coordinates. 
	 (let* ((replot-x-min (x-plot-win-inv (first replot-win-point-list) win))
		(replot-x-max (x-plot-win-inv (+ (first replot-win-point-list) (third replot-win-point-list)) win))
		(replot-y-min (y-plot-win-inv (+ (second replot-win-point-list) (fourth replot-win-point-list)) win))
		(replot-y-max (y-plot-win-inv (second replot-win-point-list) win)))		  
	   (if (or (= replot-x-min replot-x-max) (= replot-y-min replot-y-max)) ; Avoid over zooming - see below
	     (setq x-min-spec (g-value win :x-min) x-max-spec (g-value win :x-max)
		   y-min-spec (g-value win :y-min) y-max-spec (g-value win :y-max))
	     (let* ((x-seqs (car (g-value win :x-lists)))
		    (min-x-mag (if (numberp x-seqs) ; This means a :delta-t spec
				 x-seqs
				 (/ (- (car (last (car x-seqs))) (first (car x-seqs)))
				    (g-value win :data-length))))
		    (min-maxs (list (g-value win :x-min) (g-value win :y-min)
				    (g-value win :x-max) (g-value win :y-max))))
	       (unless (eq min-maxs (car (g-value win :min-max-lists))) ; Save if new values are really new.
		 (push min-maxs (g-value win :min-max-lists)))
	       (setq x-min-spec replot-x-min
		     y-min-spec replot-y-min
		     x-max-spec replot-x-max
		     y-max-spec replot-y-max)
	       (when nil		; (< (- x-max-spec x-min-spec) min-x-mag) ; Avoid too small zooming.
		 (if (= x-min-spec (g-value win :orig-x-min))
		   (setq x-max-spec (+ x-min-spec min-x-mag))
		   (setq x-min-spec (- x-max-spec min-x-mag))))))))
	((or restore-plot unzoom)
	 (let ((min-maxs (pop (g-value win :min-max-lists))))
	   (if (and unzoom min-maxs)
	     (setq x-min-spec (first min-maxs) y-min-spec (second min-maxs)
		   x-max-spec (third min-maxs) y-max-spec (fourth min-maxs))
	     (setq x-min-spec (g-value win :orig-x-min) x-max-spec (g-value win :orig-x-max)
		   y-min-spec (g-value win :orig-y-min) y-max-spec (g-value win :orig-y-max)
		   x-origin (g-value win :orig-x-origin) x-inc (g-value win :orig-x-axis-inc)
		   y-origin (g-value win :orig-y-origin) y-inc (g-value win :orig-y-axis-inc)))))
	((g-value win :auto-wf-setup)
	 (s-value win :y-plot-top-gap-extra *default-y-plot-top-gap-extra-waterfall*)
	 (s-value win :waterfall-base-y-offset 0.0)
	 (s-value win :waterfall-base-x-offset 0.0)
	 (s-value win :x-data-offset 0.0) (s-value win :y-data-offset 0.0)))
  (values x-min-spec x-max-spec y-min-spec y-max-spec x-origin y-origin))


(defun setup-plot (win &key width height ; scale
		       auto-wf-setup wf-skirt timed-data		   
		       x-inc x-min-spec x-max-spec x-log x-label
		       x-label-vertical-position x-label-horizontal-position
		       y-inc y-min-spec y-max-spec y-log y-label
		       y-label-vertical-position (y-label-horizontal-position :left)
		       invert-y-axis-label x-are-fns y-are-fns (include-x-tick-at-0 t) (include-y-tick-at-0 t)
		       fix-to-unity-mag-if-so
		       (x-origin-tick nil) (y-origin-tick t) x-origin y-origin  x-axis-root y-axis-root
		       (x-trace-offset 0.0) (y-trace-offset 0.0) (x-data-offset 0.0) (y-data-offset 0.0)
		       (consider-labels t) unzoom restore-plot replot-win-point-list revise-plot)

  (setup-plot-preliminaries win wf-skirt width height consider-labels
			    x-label-horizontal-position x-label-vertical-position 
			    y-label-horizontal-position y-label-vertical-position
			    x-label y-label (or restore-plot unzoom replot-win-point-list)
			    x-origin-tick y-origin-tick
					;  scale
			    y-trace-offset x-trace-offset x-are-fns y-are-fns
			    include-x-tick-at-0 include-y-tick-at-0 invert-y-axis-label)

  (when x-axis-root (s-value win :x-axis-root x-axis-root))
  (when y-axis-root (s-value win :y-axis-root y-axis-root))
  (cond-every ((g-value win :auto-x-scaling) (setq x-inc nil x-max-spec nil x-min-spec nil x-origin nil))
	      ((g-value win :auto-y-scaling) (setq y-inc nil y-max-spec nil y-min-spec nil y-origin nil)))

  (let* ((replotting (or replot-win-point-list restore-plot unzoom))
	 (y-seqs (parse-plot-y-seqs win))
	 (x-seqs (car (g-value win :x-lists)))	   
	 (accomodate-all (and (> (number-of-overlays win) 1) ; Only OK for windows that have data.
			      (g-value win :overlay) (g-value win :accomodate-all-overlays)))
	 display-sub-domain)
    
    (s-value win :auto-wf-setup (and auto-wf-setup (not replotting)))
    (s-value win :data-length (apply 'max (mapcar 'length y-seqs)))

    (multiple-value-bind (x-min-spec x-max-spec y-min-spec y-max-spec x-origin y-origin)
	(set-xy-max/min-spec-and-origins win replot-win-point-list restore-plot unzoom
					 X-INC X-MAX-SPEC X-MIN-SPEC X-ORIGIN Y-INC Y-MAX-SPEC Y-MIN-SPEC Y-ORIGIN)
    
      (unless replotting
	(s-value win :x-log x-log) (s-value win :y-log y-log)
	(unless (g-value win :auto-wf-setup)
	  (s-value win :x-data-offset x-data-offset)
	  (s-value win :y-data-offset y-data-offset)))

      (when (and timed-data (not (g-value win :x-log))) ; This is to make the end of the time a little neater.
	(let ((plot-window-top-x (plot-window-top-x win)))
	  (setq x-max-spec (or x-max-spec plot-window-top-x)
		display-sub-domain (< x-max-spec plot-window-top-x))))

      ;; If X-MIN-SPEC is not specified and the minimum value over the x-sequences is 0, then fix X-MIN-SPEC = 0.
      (unless x-min-spec (if (numberp x-seqs)
			   (setq x-min-spec (or (g-value win :delta-t-start) 0))
			   (when (= (a-bit-less x-seqs) 0) (setq x-min-spec 0.0))))

      ;; Sets :X-DATA-MAX, :X-DATA-MIN, :Y-DATA-MAX, :Y-DATA-MIN, and, if :AUTO-WF-SETUP, :X-TRACE-OFFSET,
      ;; :Y-TRACE-OFFSET and :WATERFALL-LABEL-OFFSET. Returns when LOGS-OK.
      (setup-xy-data-limits win display-sub-domain timed-data x-min-spec x-max-spec)
					;      (format t "***************  Y-max-spec ~A~%" Y-max-spec)
      (let* ((log-base (or (g-value win :log-base) e))
	     (number-of-traces (length (or (g-value win :trace-order) y-seqs)))
	     
	     (xfrmd-x-data-max (if (g-value win :x-log) (log (g-value win :x-data-max) log-base) (g-value win :x-data-max)))
	     (xfrmd-x-data-min (if (g-value win :x-log) (log (g-value win :x-data-min) log-base) (g-value win :x-data-min)))
	     (a-bit-more-than-the-x (if accomodate-all (max (g-value win :x-axis-max) xfrmd-x-data-max) xfrmd-x-data-max))
	     (a-bit-less-than-the-x (if accomodate-all (min (g-value win :x-axis-max) xfrmd-x-data-min) xfrmd-x-data-min))

	     (x-axis-max (if (and	; (not (g-value win :x-log))
			      (not (g-value win :waterfall)) x-max-spec)
			   x-max-spec
			   a-bit-more-than-the-x))
	     (x-axis-min (if (and	; (not (g-value win :x-log))
			      (not (g-value win :waterfall)) x-min-spec)
			   x-min-spec
			   (if (or (numberp x-seqs) timed-data)
			     (or (g-value win :delta-t-start) 0)
			     a-bit-less-than-the-x)))
	     (x-max (+ x-axis-max
		       (if (and (g-value win :waterfall) (> (g-value win :x-trace-offset) 0))
			 (* (g-value win :x-trace-offset) (1- number-of-traces))
			 0.0)))
	     (x-min (+ x-axis-min
		       (if (and (g-value win :waterfall) (< (g-value win :x-trace-offset) 0))
			 (* (g-value win :x-trace-offset) (1- number-of-traces))
			 0.0)))
	     
	     (xfrmd-y-data-max (if (g-value win :y-log) (log (g-value win :y-data-max) log-base) (g-value win :y-data-max)))
	     (xfrmd-y-data-min (if (g-value win :y-log) (log (g-value win :y-data-min) log-base) (g-value win :y-data-min)))
	     (xfrmd-y-data-mag (abs (- xfrmd-y-data-max xfrmd-y-data-min)))
	     (a-bit-more-than-the-y (if accomodate-all (max (g-value win :y-axis-max) xfrmd-y-data-max) xfrmd-y-data-max))
	     (a-bit-less-than-the-y (if accomodate-all (min (g-value win :y-axis-min) xfrmd-y-data-min) xfrmd-y-data-min))
	     (y-axis-max (cond ((and fix-to-unity-mag-if-so (and (= 1.0 xfrmd-y-data-mag) (= 1.0 xfrmd-y-data-max))) 1.0)
			       ((and (not (g-value win :waterfall)) y-max-spec) y-max-spec)
			       (t a-bit-more-than-the-y)))
	     (y-axis-min (cond ((and fix-to-unity-mag-if-so (and (= 1.0 xfrmd-y-data-mag) (= 0.0 xfrmd-y-data-min))) 0.0)
			       ((and (not (g-value win :waterfall)) y-min-spec) y-min-spec)
			       (t a-bit-less-than-the-y)))
	     (y-max (+ (if (and (g-value win :waterfall)
				(g-value win :waterfall-y-data-max)
				(g-value win :use-waterfall-y-data-max))
			 (g-value win :waterfall-y-data-max)
			 y-axis-max)
		       (if (and (g-value win :waterfall) (> (g-value win :y-trace-offset) 0))
			 (* (g-value win :y-trace-offset) (1- number-of-traces))
			 0.0)))
	     (y-min (+ y-axis-min
		       (if (and (g-value win :waterfall) (< (g-value win :y-trace-offset) 0))
			 (* (g-value win :y-trace-offset) (1- number-of-traces))
			 0.0))))

	(s-value win :xfrmd-x-data-min xfrmd-x-data-min) (s-value win :xfrmd-x-data-max xfrmd-x-data-max)
	(s-value win :xfrmd-x-data-mag (- xfrmd-x-data-max xfrmd-x-data-min)) 
	(s-value win :xfrmd-y-data-min xfrmd-y-data-min) (s-value win :xfrmd-y-data-max xfrmd-y-data-max)
	(s-value win :xfrmd-y-data-mag (- xfrmd-y-data-max xfrmd-y-data-min)) 
	
	(when (g-value win :waterfall)
	  (unless (eq (g-value win :axes-type) :none) (s-value win :axes-type :simple))
	  (if (> (g-value win :y-trace-offset) 0)
	    (setq y-axis-max y-max)
	    (when (< (g-value win :y-trace-offset) 0) (setq y-axis-min y-min)))
	  (if (> (g-value win :x-trace-offset) 0)
	    (setq x-axis-max x-max)
	    (when (< (g-value win :x-trace-offset) 0) (setq x-axis-min x-min))))
	
	(set-plot-x-y-max-min win x-max y-max x-axis-max y-axis-max x-min y-min x-axis-min y-axis-min x-origin y-origin)
	(set-plot-incs-origin-label-position win x-origin y-origin x-inc y-inc
					     x-label-vertical-position x-label-horizontal-position)

	(unless (or replotting revise-plot) (set-plot-win-orig-parameters win)) ; This is a first time plot.

	(set-plot-gaps win)
	(draw-all-axes win)
	(s-value win :auto-x-scaling nil) (s-value win :auto-y-scaling nil)))
    (when (or (<= (g-value win :x-max) (g-value win :x-min))
	      (<= (g-value win :y-max) (g-value win :y-min)))
      (sim-error (format t "Plot window ~A was specified with bogus ~A"
			 (g-value win :title)
			 (cond ((and (<= (g-value win :x-max) (g-value win :x-min))
				     (<= (g-value win :y-max) (g-value win :y-min)))
				(format nil "X/Y max (~a/~a) min (~A/~a) values.~%"
					(g-value win :x-max) (g-value win :y-max) (g-value win :x-min) (g-value win :y-min)))
			       ((<= (g-value win :x-max) (g-value win :x-min))
				(format nil "X max (~a) min (~a) values.~%" (g-value win :x-max) (g-value win :x-min)))
			       (T (format nil "Y max (~a) min (~a) values.~%" (g-value win :Y-max) (g-value win :Y-min)))))))
    (s-value win :has-been-setup t)))



(defun setup-plot-preliminaries (win wf-skirt width height consider-labels
				     x-label-horizontal-position x-label-vertical-position 
				     y-label-horizontal-position y-label-vertical-position 
				     x-label y-label dont-reset-labels x-origin-tick y-origin-tick
				     ; scale
				     y-trace-offset x-trace-offset x-are-fns y-are-fns
				     include-x-tick-at-0 include-y-tick-at-0 invert-y-axis-label) 
  (s-value win :wf-skirt wf-skirt)
  (when width (s-value win :width width)) (when height (s-value win :height height))
  (s-value win :label-traces consider-labels)
  (when x-label-vertical-position (s-value win :x-label-vertical-position x-label-vertical-position))
  (when x-label-horizontal-position (s-value win :x-label-horizontal-position x-label-horizontal-position))
  (when y-label-vertical-position (s-value win :y-label-vertical-position y-label-vertical-position))
  (when y-label-horizontal-position (s-value win :y-label-horizontal-position y-label-horizontal-position))
  (unless dont-reset-labels
    ; (when scale (s-value win :scale scale))
    (s-value win :x-label (when x-label (if (stringp x-label) x-label (format nil "~A" x-label))))
    (s-value win :y-label (when y-label (if (stringp y-label) y-label (format nil "~A" y-label)))))
  (s-value win :x-origin-tick x-origin-tick)
  (s-value win :y-origin-tick y-origin-tick)
  (s-value win :waterfall-base-x-offset (s-flt (or (g-value win :waterfall-base-x-offset) 0.0)))
  (s-value win :waterfall-base-y-offset (s-flt (or (g-value win :waterfall-base-y-offset) 0.0)))
  (s-value win :y-trace-offset (float y-trace-offset))
  (s-value win :x-trace-offset (float x-trace-offset))
  (s-value win :x-are-fns x-are-fns) (s-value win :y-are-fns y-are-fns)
  (s-value win :include-x-tick-at-0 include-x-tick-at-0)
  (s-value win :include-y-tick-at-0 include-y-tick-at-0)
  (s-value win :invert-y-axis-label invert-y-axis-label))


(defun plot-window-top-x-lists (win)
  (car (g-value win :x-lists)))


(defun plot-window-top-y-lists (win)
  (car (g-value win :y-lists)))


(defun plot-window-top-x (win)
  (let* ((x-seqs (plot-window-top-x-lists win))
	 (y-seqs (plot-window-top-y-lists win))
	 (data-length (1- (loop for y-seq in y-seqs maximize (length y-seq)))))
    (get-nice-mag (if (numberp x-seqs)
		      (+ (or (g-value win :delta-t-start) 0) (* data-length x-seqs))
		      (max-of-list (car x-seqs))))))
		       

;; Sets :X-MIN, :X-MAX, :X-MAG, :X-MIN-LIMIT, :X-MAX-LIMIT, :Y-MIN, :Y-MAX, :Y-MAG, :Y-MIN-LIMIT,
;; :Y-MAX-LIMIT, :X-AXIS-MIN, :X-AXIS-MAX, :Y-AXIS-MIN, :Y-AXIS-MAX. Note that :X-MAX, :X-MIN,
;; :Y-MAX and :Y-MIN are eventually set by the corresponding :*-AXIS-* values. The appropriate
;; values as supplied by the function args are only changed when there is flat data. In addition,
;; unless :*-MAX-MIN-SPECIFIED, all of these values are cleaned up with GET-NICE-MAG.

;; :x-min, :x-max, :y-min, :y-max now track :x-axis-min, :x-axis-max, :y-axis-min, :y-axis-max

(defun set-plot-x-y-max-min (win x-max y-max x-axis-max y-axis-max x-min y-min x-axis-min y-axis-min x-origin y-origin)
  (let ((x-max (s-flt x-max))
	(y-max (s-flt y-max))
	(x-axis-max (s-flt x-axis-max))
	(y-axis-max (s-flt y-axis-max))
	(x-min (s-flt x-min))
	(y-min (s-flt y-min))
	(x-axis-min (s-flt x-axis-min))
	(y-axis-min (s-flt y-axis-min)))
    
    (cond-every ((= y-max y-min) (setq y-max (+ 1.0 y-max) y-min (- y-min 1.0) y-axis-max y-max y-axis-min y-min))
		((= x-max x-min) (setq x-max (+ 1.0 x-max) x-min (- x-min 1.0) x-axis-max x-max x-axis-min x-min)))

    (s-value win :y-axis-max (if y-origin
			       (max (g-value win :y-origin)
				    (if (g-value win :Y-MAX-MIN-SPECIFIED) y-axis-max (get-nice-mag y-axis-max (- y-max y-min))))
			       (if (g-value win :Y-MAX-MIN-SPECIFIED) y-axis-max (get-nice-mag y-axis-max (- y-max y-min)))))
    (s-value win :y-axis-min (if y-origin
			       (min (g-value win :y-origin)
				    (if (g-value win :Y-MAX-MIN-SPECIFIED) y-axis-min (get-nice-mag y-axis-min (- y-max y-min))))
			       (if (g-value win :Y-MAX-MIN-SPECIFIED) y-axis-min (get-nice-mag y-axis-min (- y-max y-min)))))
    (s-value win :x-axis-max (if x-origin
			       (max (g-value win :x-origin)
				    (if (g-value win :X-MAX-MIN-SPECIFIED) x-axis-max (get-nice-mag x-axis-max (- x-max x-min))))
			       (if (g-value win :X-MAX-MIN-SPECIFIED) x-axis-max (get-nice-mag x-axis-max (- x-max x-min)))))
    (s-value win :x-axis-min (if x-origin
			       (min (g-value win :x-origin)
				    (if (g-value win :X-MAX-MIN-SPECIFIED) x-axis-min (get-nice-mag x-axis-min (- x-max x-min))))
			       (if (g-value win :X-MAX-MIN-SPECIFIED) x-axis-min (get-nice-mag x-axis-min (- x-max x-min)))))

;    (s-value win :y-axis-max (if (g-value win :Y-MAX-MIN-SPECIFIED) y-axis-max (get-nice-mag y-axis-max (- y-max y-min))))
;    (s-value win :y-axis-min (if (g-value win :Y-MAX-MIN-SPECIFIED) y-axis-min (get-nice-mag y-axis-min (- y-max y-min))))
;    (s-value win :x-axis-max (if (g-value win :X-MAX-MIN-SPECIFIED) x-axis-max (get-nice-mag x-axis-max (- x-max x-min))))
;    (s-value win :x-axis-min (if (g-value win :X-MAX-MIN-SPECIFIED) x-axis-min (get-nice-mag x-axis-min (- x-max x-min))))

    (cond (nil				; (g-value win :waterfall)
	   (s-value win :y-max y-max)  (s-value win :y-min y-min)
	   (s-value win :x-max x-max)  (s-value win :x-min x-min))
	  (t
	   ;; LBG change 28.08.00

	   (s-value win :y-max y-axis-max) (s-value win :y-min y-axis-min)
	   (s-value win :x-max x-axis-max) (s-value win :x-min x-axis-min)

	   ;;(s-value win :y-max (g-value win :y-axis-max)) (s-value win :y-min (g-value win :y-axis-min))
	   ;;(s-value win :x-max (g-value win :x-axis-max)) (s-value win :x-min (g-value win :x-axis-min))

	   ))
	  
    (s-value win :y-mag (- (g-value win :y-max) (g-value win :y-min)))
    (s-value win :y-max-limit (+ y-axis-max (g-value win :y-mag)))
    (s-value win :y-min-limit (- y-axis-min (g-value win :y-mag)))

    (s-value win :x-mag (- (g-value win :x-max) (g-value win :x-min)))
    (s-value win :x-max-limit (+ x-axis-max (g-value win :x-mag)))
    (s-value win :x-min-limit (- x-axis-min (g-value win :x-mag)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Plot gap/layout functions
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun set-plot-gaps (win)
  (set-plot-label-height win)
  (set-plot-left-gap win)
  (set-plot-right-gap win)
  (set-plot-bottom-gap win)
  (set-plot-area-hw win))


(defun set-plot-label-height (win)
  (loop for y-seq in (car (g-value win :y-lists))
	for label in (g-value win :label-list)
	when (> (length label) 0) collect y-seq into y-seqs
	finally
	(let* (				; (y-seqs (car (g-value win :y-lists)))
	 
	       (number-of-curves (if (g-value win :trace-order)
				   (min (length (g-value win :trace-order))
					(length y-seqs))
				   (length y-seqs))))
	  ;; The :label-height has to take into account both the trace labels and the y-axis label,
	  (s-value win :label-height
		   (if (and (g-value win :use-fixed-top-gap) (numberp (g-value win :fixed-top-gap)))
		     (round (g-value win :fixed-top-gap))
		     (+ (or (g-value win :y-plot-top-gap-extra) 0)
			;; For either the Y axis label or the X axis tick marks
			(if (g-value win :waterfall)
			  0
			  (+ (y-label-near-trace-labels win)
			     (case (g-value win :x-label-vertical-position)
			       ;; For the X axis tick marks
			       (:ABOVE
				;; only account for x marks if ordinate is near top 
				(if (< (/ (- (g-value win :y-origin) (g-value win :y-min)) (g-value win :y-mag)) 0.8)
				  (+ 5 (g-value (window-plot-axis-font win) :font-height))
				  (+ 13 *gap-btwn-x-label-and-tick-marks*
				     (* 2 (g-value (window-plot-axis-font win) :font-height)))))
			       ;; Just for the Y axis label
			       (t	; :BELOW
				(+ 5 (y-axis-label-height win)))
			       )))
			(if (and (g-value win :label-traces) (non-null-plot-win-labels win))
			  (+		 (g-value (window-plot-axis-font win) :font-height)
					 (loop for curve-num from 1 to number-of-curves sum (key-and-label-height win curve-num)))
			
			  5)))))))

(defun y-axis-label-height (win)
  (* (g-value (window-plot-axis-font win) :font-height)
     (+ 1 (NUMBER-OF-NEWLINES (get-plot-y-axis-label win)))))


(defun key-and-label-height (win curve-num)
  (round (+ *trace-key-gap*
	    (max (g-value (window-plot-axis-font win) :font-height)
		 (scatter-symbol-height-width win (get-scatter-symbol win curve-num) curve-num)))))



(defun y-axis-labeled-ticks-below-0-width (win)
  (loop for y downfrom (- (g-value win :y-origin) (g-value win :y-inc))
	to (min (g-value win :y-origin) (g-value win :y-axis-min))
	by (g-value win :y-inc)
	for count from 0
	maximize
	(+ 5 10 5			; for tick mark, gap,  and a little extra
	   (plot-window-string-width win (y-axis-number-string win y)))
	into max
	when (or (<= y (min (g-value win :y-origin) (g-value win :y-axis-min)))
		 (and (> count 0)
		      (= y (- (g-value win :y-origin) (g-value win :y-inc)))))
	do (return max)
	finally (return max)))


(defun y-axis-labeled-ticks-above-0-width (win)
  (loop for y upfrom (g-value win :y-origin) ; Y axis labeled ticks, above 0.0
	to (max (g-value win :y-origin) (g-value win :y-axis-max))
	by (g-value win :y-inc)
	for count from 0
	maximize
	(+ 5 10 5			; for tick mark, gap,  and a little extra
	   (plot-window-string-width win (y-axis-number-string win y)))
	into max
	when (or (>= y (max (g-value win :y-origin) (g-value win :y-axis-max)))
		 (and (> count 0) (= y (g-value win :y-origin))))
	do (return max)
	finally (return max)))


(defun y-axis-label-width (win)
  (plot-window-string-width win (get-plot-y-axis-label win)))



(defun y-label-to-the-left-of-axis (win)
  (case (g-value win :y-label-vertical-position)
    ((:upper-left :center-left :lower-left :two-thirds-up) 0)
    ((:center-center :upper-center :lower-center)
     (/ (y-axis-label-width win) 2))
    (t					; :center-right :upper-right :lower-right
     (+ 10 (y-axis-label-width win)))))

(defun y-label-near-trace-labels (win)
  (case (g-value win :y-label-vertical-position)
    ((:center-right  :lower-right :center-center :center-left  :lower-center :lower-left :two-thirds-up) 0)
    ((:upper-left :upper-center :upper-right)
     (g-value (window-plot-axis-font win) :font-height))
    (t 0)))


(defun set-plot-left-gap (win)
  (let ((y-seqs (car (g-value win :y-lists))))
    ;; The distance in pixels of the scaled data rectangle from the left of the window.
    (s-value win :x-plot-left-gap
	     (round
	      (if (and (g-value win :use-fixed-left-gap) (numberp (g-value win :fixed-left-gap)))
		(round (g-value win :fixed-left-gap))
		(+ (or (g-value win :x-plot-left-gap-extra) 0)
		   (case (g-value win :plot-type)
		     (:polar (g-value win :label-height))
		     (:waterfall *x-plot-left-gap-waterfall*)
		     (t (+ (case (g-value win :y-label-vertical-position)
			     (:two-thirds-up
			      (+ 7 (y-axis-label-width win)))
			     (t;; :upper-right :center-right :lower-right :upper-left :center-left :lower-left
			      0))
			   (max *x-plot-right-gap*
				;; Go through all the y axis tick mark numbers to find the widest.
				(- (max (y-axis-labeled-ticks-below-0-width win) 
					(y-axis-labeled-ticks-above-0-width win)
					(y-label-to-the-left-of-axis win))					       
				   (* (g-value win :width) ; 0
				      (/  (- (g-value win :x-origin) (g-value win :x-min))
				       ; (max (g-value win :x-origin) (g-value win :x-min))
					 (g-value win :x-mag))))))))))))))


(defun set-plot-right-gap (win)
  ;;  The distance in pixels of the scaled data rectangle from the right side of the window.
  (s-value win :x-plot-right-gap
	   (if (and (g-value win :use-fixed-right-gap) (numberp (g-value win :fixed-right-gap)))
	       (round (g-value win :fixed-right-gap))
	       (+
		(or (g-value win :x-plot-right-gap-extra) 0)
		(case (g-value win :plot-type)
		  (:polar (g-value win :label-height))
		  (:waterfall
		   (+ 
		    (if (g-value win :label-waterfall)
			(loop for label in (reverse (g-value win :label-list))
			      for curve-num from 0
			      when (= 0 (mod curve-num (1+ (g-value win :waterfall-trace-label-skip))))
			      maximize (- (plot-window-string-width win label)
					  (x-plot-win-distance (* curve-num (g-value win :x-trace-offset)) win)))
			0)
		    *x-plot-right-gap*
		    (if (and (g-value win :label-waterfall) (g-value win :gap-between-trace-and-waterfall-label))
			(g-value win :gap-between-trace-and-waterfall-label)
			0)))
		  (t (+ 
		      (max *x-plot-right-gap*
			   ;; Go through all the y axis tick mark numbers to find the widest.
			   (case (g-value win :y-label-vertical-position)
			     ((:upper-left :upper-center :upper-right) 0)
			     ((:two-thirds-up :center-right :upper-right :lower-right) 0)
			     (t ;; :upper-left :center-left :lower-left
			      0 ; (+ 10 (y-axis-label-width win))
			      ))))))))))


(defun set-plot-bottom-gap (win)
  (let ((y-seqs (car (g-value win :y-lists))))
    ;; The distance in pixels of the scaled data rectangle off the bottom of the window.
    (s-value win :y-plot-bottom-gap
	     (if (and (g-value win :use-fixed-bottom-gap) (numberp (g-value win :fixed-bottom-gap)))
		 (round (g-value win :fixed-bottom-gap))
		 (max
		  (+ (max 0 (g-value win :x-axis-tick-mark-length))
		     (g-value (window-plot-axis-font win) :font-height)
		     (g-value (window-comment-font win) :font-height) ; Leave room for at least title
		     10			; fudge
		     )
		  (case (g-value win :plot-type)
		    (:polar (g-value win :label-height))
		    (t (if (eq :auto (g-value win :waterfall))
			   (+ *y-plot-bottom-gap* 10)
			   *y-plot-bottom-gap*))))))))

(defun set-plot-incs-origin-label-position (win x-origin y-origin x-inc y-inc x-label-vertical-position x-label-horizontal-position)
  (s-value win :x-origin
	   (if x-origin
	     (float x-origin)
	     (get-nice-mag (bound-val 0.0 (g-value win :x-axis-max) (g-value win :x-axis-min))
			   (- (g-value win :x-axis-max) (g-value win :x-axis-min)))))
  (s-value win :y-origin
	   (if y-origin
	     (float y-origin)
	     (get-nice-mag (bound-val 0.0 (g-value win :y-axis-max) (g-value win :y-axis-min))
			   (- (g-value win :y-axis-max) (g-value win :y-axis-min)))))
  (s-value win :y-inc
	   ;; this is to avoid small y-incs which do not advance axis tick positions in draw-full-cartesian-axes
	   (s-flt (max (case (g-value win :mode)
			 (:histogram 1)
			 (t (abs (* .01 (g-value win :y-mag)))))
		       (if (and y-inc (not (= 0 y-inc)))
			 (float y-inc)
			 (/ (msd-round	; get-nice-mag
			     (if (g-value win :waterfall)
			       (g-value win :xfrmd-y-data-mag)
			       (- (g-value win :y-axis-max) (g-value win :y-axis-min))))
			    (case (g-value win :plot-type)
			      (polar 4.0)
			      (:waterfall 3.0)
			      (t 5.0)))))))
  (s-value win :x-inc
	   (s-flt (case (g-value win :plot-type)
		    (:polar (g-value win :y-inc))
		    (t (if (and x-inc (not (= 0 x-inc)))
			 (float x-inc)
			 (/ (msd-round	; get-nice-mag
			     (if (g-value win :waterfall)
			       (g-value win :xfrmd-x-data-mag)
			       (- (g-value win :x-axis-max) (g-value win :x-axis-min))))
			    5.0))))))
  (when x-label-horizontal-position (s-value win :x-label-horizontal-position x-label-horizontal-position))

  (s-value win :x-label-vertical-position
	   (or x-label-vertical-position
	       (if (> (- (g-value win :y-axis-max) (g-value win :y-origin))
		      (- (g-value win :y-origin) (g-value win :y-axis-min)))
		 :below :above))))


(defun set-plot-area-hw (win)
  (s-value win :plot-area-width (- (g-value win :width) (+ (g-value win :x-plot-left-gap) (g-value win :x-plot-right-gap))))
  (s-value win :plot-area-height (- (g-value win :height) (+ (g-value win :label-height) (g-value win :y-plot-bottom-gap)))))


(defun UPDATE-FIXED-GAP-PARAMETERS (win use-fixed-top-gap fixed-top-gap use-fixed-bottom-gap fixed-bottom-gap
					use-fixed-right-gap fixed-right-gap use-fixed-left-gap fixed-left-gap)
  (s-value win :use-fixed-top-gap (and use-fixed-top-gap (numberp fixed-top-gap)))
  (s-value win :fixed-top-gap fixed-top-gap)
  (s-value win :use-fixed-bottom-gap (and use-fixed-bottom-gap (numberp fixed-bottom-gap)))
  (s-value win :fixed-bottom-gap fixed-bottom-gap)
  (s-value win :use-fixed-right-gap (and use-fixed-right-gap (numberp fixed-right-gap)))
  (s-value win :fixed-right-gap fixed-right-gap)
  (s-value win :use-fixed-left-gap (and use-fixed-left-gap (numberp fixed-left-gap)))
  (s-value win :fixed-left-gap fixed-left-gap))

;; SET-PLOT-WIN-ORIG-PARAMETERS These are useful for restoring the plot to its original state.
(defun set-plot-win-orig-parameters (win)
  (s-value win :orig-x-min (g-value win :x-min))
  (s-value win :orig-x-max (g-value win :x-max))
  (s-value win :orig-x-origin (g-value win :x-origin))
  (s-value win :orig-x-axis-inc (g-value win :x-inc))

  (s-value win :orig-y-min (g-value win :y-min))
  (s-value win :orig-y-max (g-value win :y-max))
  (s-value win :orig-y-origin (g-value win :y-origin))
  (s-value win :orig-y-axis-inc (g-value win :y-inc)))

#|
(create-instance 'plot-scale-bar-background
                 opal:rectangle
                 (:visible t)
		 (:filling-style opal::white-fill)
                 (:left 15)
                 (:width (o-formula (+ (round (/ (float (gvl :parent :window :x-scale-bar-length)) (gvl :parent :window :scale))) 10)))
		 (:top (o-formula (- (gvl :parent :window :height) (+ *plot-scale-bar-vertical-space* 20))))
		 (:height 30)
		 (:line-style nil))
|#


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Axes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun draw-all-axes (win)
  (case (g-value win :axes-type)
    (:none (get-plot-agg win 'data-axes t))
    (:simple (draw-simple-axes win :auto-setup (g-value win :auto-wf-setup))
	     (when (g-value win :draw-grid) (draw-full-cartesian-axes win :draw-axes nil)))
    (:standard (draw-full-cartesian-axes win)))
  (unless (g-value win :draw-grid) (get-plot-agg win 'data-grid t))
  nil)


(defun refresh-axes (win)
  (if (g-value win :draw-grid)
      (draw-full-cartesian-axes win :draw-axes nil)
      (get-plot-agg win 'data-grid t)))


(defun draw-simple-axes (win &key auto-setup)
  (when auto-setup
    (s-value win :y-scale-t% 92.0)
    (s-value win :x-scale-l% 12.0)
    (s-value win :x-scale-t% 92.0))     
  (let* ((axes-agg (get-plot-agg win 'data-axes t))
	 (x-body-x1 (round (* (g-value win :width) .01 (g-value win :x-scale-l%))))
	 (x-body-x2 (+ (round (* (g-value win :width) .01 (g-value win :x-scale-l%)))
		       (-  (x-plot-win-float (+ (the sf (g-value win :x-axis-min))
						(the sf (or *simple-axis-x* (g-value win :x-inc)))) win nil)
			   (x-plot-win-float (g-value win :x-axis-min) win nil))))
	 (x-body-y1 (round (* (the fn (g-value win :height)) .01 (g-value win :x-scale-t%))))
	 (x-body-y2 (round (* (the fn (g-value win :height)) .01 (g-value win :x-scale-t%))))
	 (y-body-x1 (round (* (the fn (g-value win :width)) .01 (g-value win :x-scale-l%))))
	 (y-body-x2 (round (* (the fn (g-value win :width)) .01 (g-value win :x-scale-l%))))
	       
	 (y-body-y1 (round (* (the fn (g-value win :height)) .01 (g-value win :y-scale-t%))))
	 (y-body-y2 (+ (round (* (the fn (g-value win :height)) .01 (g-value win :y-scale-t%)))
		       (- (y-plot-win-float (+ (the sf (g-value win :y-axis-min)) (the sf (or *simple-axis-y* (g-value win :y-inc)))) win nil)
			  (y-plot-win-float (g-value win :y-axis-min) win nil))))

	 (x-label (create-instance nil axis-text
				   (:label-position (o-formula (or (gvl :window :x-label-vertical-position) :center)))
				   (:left (round (+ (the fn x-body-x1) (* 0.5 (- (the fn x-body-x2) (the fn x-body-x1))))))))
	 
	 (y-label (create-instance nil axis-text
				   (:label-position (o-formula (or (gvl :window :y-label-horizontal-position) :center)))
				   (:left (o-formula (case (gvl :window :y-label-horizontal-position)
						       (:right (the fn (+ (the fn y-body-x1) 10)))
						       (:left (the fn (- (the fn y-body-x1) 10)))
						       (t y-body-x1))))))

	 (gadget
	  (create-instance
	   nil
	   opal:aggregadget
	   (:parts
	    `((:x-body			
	       ,opal:line
	       (:visible ,(o-formula (gvl :window :x-axis-p))) (:line-style ,thick-black-line)
	       (:x1 ,x-body-x1) (:x2 ,x-body-x2) (:y1 ,x-body-y1) (:y2 ,x-body-y2))
	      (:y-body			
	       ,opal:line
	       (:visible ,(o-formula (gvl :window :y-axis-p)))
	       (:line-style ,thick-black-line)
	       (:x1 ,y-body-x1) (:x2 ,y-body-x2) (:y1 ,y-body-y1) (:y2 ,y-body-y2)))))))
    (s-value x-label :window win)
    (s-value y-label :window win)
    (opal:add-component axes-agg gadget)
    (opal:add-component axes-agg y-label)
    (opal:add-component axes-agg x-label)
    
    (s-value y-label :visible (o-formula (gvl :parent :window :y-axis-p)))
    (s-value y-label :string (format nil "~a~A~a"
				     (if (g-value win :simple-axis-y-value-p)
				       (y-axis-number-string win (or *simple-axis-y* (g-value win :y-inc)))
				       "")
				     (if (and (g-value win :y-label) (g-value win :simple-axis-y-value-p))
				       " " "")
				     (g-value win :y-label)))
    (s-value y-label :top (min (- (the fn y-body-y1) (+ 5 (the fn (g-value y-label :height))))
			       (the fn y-body-y2)))
    (s-value x-label :string (format nil "~a~A~a"
				     (if (g-value win :simple-axis-x-value-p)
					 (x-axis-number-string win (or *simple-axis-x* (g-value win :x-inc)))
					 "")
				     (if (and (g-value win :x-label) (g-value win :simple-axis-x-value-p))
					 " " "")
				     (g-value win :x-label)))
    (s-value x-label :visible (o-formula (gvl :parent :window :x-axis-p)))
    (s-value x-label :top (+ 5 (the fn x-body-y1)))

    nil))


(defun parse-plot-win-log-base (win)
  (if (and (numberp (g-value win :log-base))
	   (= (round (g-value win :log-base))
	      (g-value win :log-base)))
      (round (g-value win :log-base))
      (or (g-value win :log-base) "E")))

(defun get-plot-y-axis-label (win)
  (or (if (g-value win :y-log)
	  (format nil "~A[log~A]"
		  (g-value win :y-label)
		  (parse-plot-win-log-base win))
	  (g-value win :y-label))
      ""))


(defun get-plot-x-axis-label (win)
  (or (if (g-value win :x-log)
	  (format nil "~A[log~A]"
		  (g-value win :x-label)
		  (parse-plot-win-log-base win))
	  (g-value win :x-label))
      ""))


;; DRAW-FULL-CARTESIAN-AXES Draw X and Y axis onto plot window WIN so that data will be contained in scaled
;; rectangle centered in window. Also adds labeled tick marks. Also draws the grid.
(defun draw-full-cartesian-axes (win &key (draw-axes t) draw-grid)
;  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let* (				; (font-height (the fn (g-value (window-plot-axis-font win) :font-height)))
	 (draw-grid (or draw-grid (g-value win :draw-grid)))
	 (axis-line-style (or (g-value win :axis-line-style) *default-axis-line-style*)) 
	 (grid-line-style (or (g-value win :grid-line-style) *default-grid-line-style*)) 
	 (axis-agg (when draw-axes (get-plot-agg win 'data-axes t))) ; Get a data-axis agg and clear it.
	 (grid-agg (when draw-grid (get-plot-agg win 'data-grid t))) ; Get a data-axis agg and clear it.

	 (y-axis-max (s-flt (max (g-value win :y-origin) (g-value win :y-axis-max))))
	 (y-axis-min (s-flt (min (g-value win :y-origin) (g-value win :y-axis-min))))

	 (y-axis-root (s-flt (if (and (not (g-value win :reference-ticks-to-origin))
				      (g-value win :y-axis-root)
				      (< y-axis-min (g-value win :y-axis-root) y-axis-max))
			       (g-value win :y-axis-root)
			       (g-value win :y-origin))))

	 (x-axis-max (s-flt (max (g-value win :x-origin) (g-value win :x-axis-max))))
	 (x-axis-min (s-flt (min (g-value win :x-origin) (g-value win :x-axis-min))))

	 (x-axis-root  (s-flt (if (and (not (g-value win :reference-ticks-to-origin))
				       (g-value win :x-axis-root)
				       (< x-axis-min (g-value win :x-axis-root) x-axis-max))
				(g-value win :x-axis-root)
				(g-value win :x-origin))))
	 
	 (y-origin-w (y-plot-win-float-unbounded (s-flt (g-value win :y-origin)) win))
	 (x-origin-w (x-plot-win-float-unbounded (s-flt (g-value win :x-origin)) win))
	 (number-x-tics-below-zero (truncate (/ (- x-axis-root x-axis-min) (g-value win :x-inc))))
	 (number-x-tics-above-zero (truncate (/ (- x-axis-max x-axis-root) (g-value win :x-inc))))
	 (number-y-tics-below-zero (truncate (/ (- y-axis-root y-axis-min) (g-value win :y-inc))))
	 (number-y-tics-above-zero (truncate (/ (- y-axis-max y-axis-root) (g-value win :y-inc))))
	 (y-axis-visible-max (when (g-value win :consider-y-axis-visible-limit) (g-value win :y-axis-visible-max)))
				
	 (y-axis-visible-min (when (g-value win :consider-y-axis-visible-limit) (g-value win :y-axis-visible-min)))
	 (x-axis-visible-max (when (g-value win :consider-x-axis-visible-limit) (g-value win :x-axis-visible-max)))
	 (x-axis-visible-min (when (g-value win :consider-x-axis-visible-limit) (g-value win :x-axis-visible-min))))


    (when (or (and draw-axes (g-value win :y-axis-p)) draw-grid)
      (when (and draw-axes (g-value win :y-axis-p))
	(opal:add-component		;Y AXIS
	 axis-agg (create-instance nil opal:line (:constant t) (:line-style axis-line-style)
				   (:x1 x-origin-w) (:x2 x-origin-w)
				   (:y1 (y-plot-win-float-unbounded (or y-axis-visible-min y-axis-min) win))
				   (:y2 (y-plot-win-float-unbounded (or y-axis-visible-max y-axis-max) win))))
	(add-y-axis-label axis-agg win y-axis-max y-axis-min x-origin-w))	  
      (add-y-ticks win draw-axes axis-agg y-axis-root number-y-tics-above-zero
		   y-axis-visible-min y-axis-visible-max x-axis-visible-min x-axis-visible-max
		   x-origin-w draw-grid grid-agg grid-line-style NUMBER-y-TICS-BELOW-ZERO x-AXIS-MAX x-AXIS-MIN))      
    (when (or (and draw-axes (g-value win :x-axis-p)) draw-grid)
      (when (and draw-axes (g-value win :x-axis-p))
	(opal:add-component		;X AXIS
	 axis-agg (create-instance nil opal:line
				   (:constant t)
				   (:x1 (x-plot-win-float-unbounded (or x-axis-visible-min x-axis-min) win))
				   (:x2 (x-plot-win-float-unbounded (or x-axis-visible-max x-axis-max) win))
				   (:y1 y-origin-w) (:y2 y-origin-w)
				   (:line-style axis-line-style)))
	(add-x-axis-label axis-agg win x-axis-max x-axis-min y-origin-w))
      (add-x-ticks win draw-axes axis-agg x-axis-root number-x-tics-above-zero
		   x-axis-visible-min x-axis-visible-max y-axis-visible-min y-axis-visible-max
		   y-origin-w draw-grid grid-agg grid-line-style NUMBER-X-TICS-BELOW-ZERO Y-AXIS-MAX Y-AXIS-MIN))
    (when (and draw-axes (eq (g-value win :plot-type) :polar) (g-value win :polar-circles-p)) ; Polar circles
      (add-polar-circles win axis-agg y-axis-root y-axis-max)))
  (when (g-value win :bitmap) (opal:move-component (get-agg win) (g-value win :bitmap) :where :back)))


(defun add-y-axis-label (axis-agg win y-axis-max y-axis-min x-origin-w)
  (let* ((font-height (the fn (Y-AXIS-LABEL-HEIGHT win)))
	 (label (create-instance nil axis-text
				 (:label-position
				  (o-formula (case (gvl :window :y-label-vertical-position)
					       ((:upper-left :center-left :lower-left :two-thirds-up) :right)
					       ((:center-center :upper-center :lower-center) :center)
					       (t ; :center-right :upper-right :lower-right
						:left)))))))
    (s-value label :window win)
    (opal:add-component axis-agg label)
    (s-value label :string (get-plot-y-axis-label win))
    (s-value label :orientation (o-formula (gvl :parent :window :y-label-orientation)))
    (s-value label :left (case (g-value win :y-label-vertical-position)
			   (:two-thirds-up 5)
			   (:center-right (+ x-origin-w 5))
			   ((:upper-center :lower-center :center-center) x-origin-w)
			   ((:upper-right :lower-right) x-origin-w)
			   (t		; :upper-left :lower-left :center-left
			    (+ x-origin-w  (* -1 (the fn (g-value label :width))) -5))))
    (s-value label :top (case (g-value win :y-label-vertical-position)
			  ((:center-left :center-right)
			   (round (the sf (+ (y-plot-win-float-unbounded y-axis-min win)
					     (* 0.5 (- (y-plot-win-float-unbounded y-axis-max win)
						       (y-plot-win-float-unbounded y-axis-min win)))
					     (* -0.5 font-height)))))
			  (:two-thirds-up
			   (round (the sf (+ (y-plot-win-float-unbounded y-axis-min win)
					     (* 0.67 (- (y-plot-win-float-unbounded y-axis-max win)
							(y-plot-win-float-unbounded y-axis-min win)))
					     (* -0.5 font-height)))))
			  ((:lower-left :lower-right)
			   (round (- (y-plot-win-float-unbounded y-axis-min win)
				     (* -1.5 font-height))))
			  (t		; :upper-right :upper-left
			   (round (+ (y-plot-win-float-unbounded y-axis-max win) 
				     (* -1.5 font-height))))))
    label))


(defun add-x-axis-label (axis-agg win x-axis-max x-axis-min y-origin-w)
  (let* ((font-height (the fn (g-value (window-plot-axis-font win) :font-height)))
	 (x-axis-label-width (the fn (plot-window-string-width win (g-value win :x-label))))
	 (label (create-instance nil axis-text (:label-position :right ; :center
								))))
    (s-value label :window win)
					;    (s-value label :string (g-value win :x-label))
    (s-value label :string (get-plot-x-axis-label win))
    (opal:add-component axis-agg label)
    (s-value label :orientation (o-formula (gvl :parent :window :x-label-orientation)))
    (s-value label :left
	     (case (g-value axis-agg :window :x-label-horizontal-position)
	       (:left (x-plot-win-float-unbounded x-axis-min win))
	       (:center	(round
			 (+ (x-plot-win-float-unbounded x-axis-min win)
			    (* 0.5 (- (x-plot-win-distance-float (- x-axis-max x-axis-min) win) x-axis-label-width)))))
	       (t (- (x-plot-win-float-unbounded x-axis-max win) x-axis-label-width))))	; :right
    (s-value label :top (+ y-origin-w
			   (case (g-value axis-agg :window :x-label-vertical-position)
			     (:above (- (+ *GAP-BTWN-X-LABEL-AND-TICK-MARKS* (+ font-height font-height))))
			     (t ; :below
			      (+ *GAP-BTWN-X-LABEL-AND-TICK-MARKS* font-height))
			     )))
    label))


(defun add-y-ticks (win draw-axes axis-agg y-axis-root number-y-tics-above-zero
			y-axis-visible-min y-axis-visible-max x-axis-visible-min x-axis-visible-max
			x-origin-w
			draw-grid grid-agg grid-line-style NUMBER-y-TICS-BELOW-ZERO x-AXIS-MAX x-AXIS-MIN)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (loop for y single-float downfrom (- y-axis-root (the sf (g-value win :y-inc)))
	by (the sf (g-value win :y-inc))
	for tick-count fixnum from 1 to (max 0 number-y-tics-below-zero)
	when (and (or (not y-axis-visible-min) (<= (the sf y-axis-visible-min) y))
		  (or (not y-axis-visible-max) (<= y (the sf y-axis-visible-max))))
	do				; Y AXIS LABELED TICKS, BELOW 0.0
	(when (and draw-axes (g-value win :y-axis-p))
	  (y-axis-tick axis-agg win x-origin-w y tick-count))
	(when draw-grid
	  (opal:add-component grid-agg	; Y GRID
			      (create-instance nil opal:line (:constant t)
					       (:x1 (x-plot-win-float-unbounded (or x-axis-visible-min x-axis-min) win))
					       (:x2 (x-plot-win-float-unbounded (or x-axis-visible-max x-axis-max) win))
					       (:y1 (y-plot-win-float-unbounded y win))
					       (:y2 (y-plot-win-float-unbounded y win))
					       (:line-style grid-line-style)))))
  ;; Y AXIS LABELED TICKS, INCLUDING AND ABOVE 0.0
  (loop for y single-float from y-axis-root by (the sf (g-value win :y-inc))
	for tick-count fixnum from 0 to (max 0 number-y-tics-above-zero)
	when (and (or (not y-axis-visible-min) (<= (the sf y-axis-visible-min) y))
		  (or (not y-axis-visible-max) (<= y (the sf y-axis-visible-max))))
	do
	(when (and draw-axes (g-value win :y-axis-p))
	  (y-axis-tick axis-agg win x-origin-w y tick-count))
	(when draw-grid
	  (opal:add-component grid-agg	; Y GRID
			      (create-instance nil opal:line (:constant t)
					       (:x1 (x-plot-win-float-unbounded (or x-axis-visible-min x-axis-min) win))
					       (:x2 (x-plot-win-float-unbounded (or x-axis-visible-max x-axis-max) win))
					       (:y1 (y-plot-win-float-unbounded y win))
					       (:y2 (y-plot-win-float-unbounded y win))
					       (:line-style grid-line-style))))))


(defun add-x-ticks (win draw-axes axis-agg
			x-axis-root number-x-tics-above-zero
			x-axis-visible-min x-axis-visible-max
			y-axis-visible-min y-axis-visible-max
			y-origin-w
			draw-grid grid-agg grid-line-style
			NUMBER-X-TICS-BELOW-ZERO Y-AXIS-MAX Y-AXIS-MIN)
  (loop for x single-float downfrom (- x-axis-root (g-value win :x-inc))
	by (the sf (g-value win :x-inc)) ; X TICKS, BELOW 0.0
	for tick-count fixnum from 1 to (max 0 number-x-tics-below-zero)
	when (and (or (not x-axis-visible-min) (<= x-axis-visible-min x))
		  (or (not x-axis-visible-max) (<= x x-axis-visible-max)))
	do
	(when (and draw-axes (g-value win :x-axis-p))
	  (x-axis-tick axis-agg win y-origin-w x tick-count))
	(when draw-grid		
	  (opal:add-component grid-agg	; X GRID 
			      (create-instance nil opal:line (:constant t)
					       (:x1 (x-plot-win-float-unbounded x win))
					       (:x2 (x-plot-win-float-unbounded x win))
					       (:y1 (y-plot-win-float-unbounded (or y-axis-visible-min y-axis-min) win))
					       (:y2 (y-plot-win-float-unbounded (or y-axis-visible-max y-axis-max) win))
					       (:line-style grid-line-style)))))
  ;; X AXIS LABELED TICKS, ABOVE 0.0
  (loop for x single-float upfrom x-axis-root by (the sf (g-value win :x-inc))
	for tick-count fixnum from 0 to (max 0 number-x-tics-above-zero)
	when (and (or (not x-axis-visible-min) (<= x-axis-visible-min x))
		  (or (not x-axis-visible-max) (<= x x-axis-visible-max)))
	do
	(when (and draw-axes (g-value win :x-axis-p))
	  (x-axis-tick axis-agg win y-origin-w x tick-count))
	(when draw-grid		
	  (opal:add-component grid-agg	; X GRID
			      (create-instance nil opal:line (:constant t)
					       (:x1 (x-plot-win-float-unbounded x win))
					       (:x2 (x-plot-win-float-unbounded x win))
					       (:y1 (y-plot-win-float-unbounded (or y-axis-visible-min y-axis-min) win))
					       (:y2 (y-plot-win-float-unbounded (or y-axis-visible-max y-axis-max) win))
					       (:line-style grid-line-style))))))


(defun add-polar-circles (win axis-agg y-axis-root y-axis-max)
  (loop for y from (+ y-axis-root (g-value win :y-inc)) by (g-value win :y-inc) to y-axis-max do
	(opal:add-component	
	 axis-agg				
	 (create-instance nil opal:circle (:constant t) (:visible t)
					; These 1- and +3 are fudges to get better polar circles.
			  (:left (1- (x-plot-win-float-unbounded (- y) win)))
			  (:top (1- (y-plot-win-float-unbounded y win)))
			  (:height (+ 3 (y-plot-win-distance (+ y y) win)))
			  (:width (+ 3 (x-plot-win-distance (+ y y) win)))))))


(defun add-axis-text (string top left win agg &optional (text-position :right))
  (let ((label (create-instance nil axis-text (:label-position text-position))))
    (opal:add-component agg label)
    (s-value label :window win)
    (s-value label :top top)
    (s-value label :string string)
    (s-value label :left left)
    label))

(defun x-axis-number-string (win x)
  (let* ((adjusted-x (if (g-value win :x-axis-number-coefficient)
			 (* (g-value win :x-axis-number-coefficient) x)
			 x))
	 (x (cond ((g-value win :absolute-value-ticks) (abs adjusted-x))
		  (t adjusted-x))))
    (if (and (= x (g-value win :x-origin))
	     (not (and (or (g-value win :x-axis-p) (g-value win :x-origin-tick))
		       (or (not (and (g-value win :y-axis-min) (g-value win :y-origin) (g-value win :y-axis-max)))
			   (not (< (g-value win :y-axis-min) (g-value win :y-origin) (g-value win :y-axis-max)))))))
	""
	(let ((naked-number-string
	       (cond ((g-value win :x-are-fns) (format nil "~D" (round x)))
		     (*use-my-float-format-for-plots*
		      (plot-window-x-number-format (s-flt x) win
						   :decimals (g-value win :x-tick-decimal)
						   :range (if (< (g-value win :x-inc) 0.1)
									    (/ (g-value win :x-inc) 10)
									    (g-value win :x-inc))))
		     (t (format nil "~E" x)))))
	  (format nil "~a~A~a"
		  (or (g-value win :x-axis-value-prefix) "")
		  naked-number-string
		  (or (g-value win :x-axis-value-suffix) ""))))))
		
(defun y-axis-number-string (win y)
  (let* ((adjusted-y (if (g-value win :y-axis-number-coefficient)
			 (* (g-value win :y-axis-number-coefficient) y)
			 y))
	 (y (cond ((g-value win :absolute-value-ticks) (abs adjusted-y))
		  ((g-value win :invert-y-axis-label) (- adjusted-y))
		  (t adjusted-y))))
    (if (and (= y (g-value win :y-origin))
	     (not (and (g-value win :y-origin-tick)
		       (or (not (and (g-value win :x-axis-min) (g-value win :x-origin) (g-value win :x-axis-max)))
			   (not (< (g-value win :x-axis-min) (g-value win :x-origin) (g-value win :x-axis-max)))))))
	""
	(let ((naked-number-string
	       (cond ((g-value win :y-are-fns) (format nil "~D" (round y)))
		     (*use-my-float-format-for-plots*
		      (plot-window-y-number-format (s-flt y) win
						   :decimals (g-value win :y-tick-decimal)
						   :range (if (< (g-value win :y-inc) 0.1)
									    (/ (g-value win :y-inc) 10)
									    (g-value win :y-inc))))
		     (t (format nil "~E" y)))))
	  (format nil "~a~A~a"
		  (or (g-value win :y-axis-value-prefix) "")
		  naked-number-string
		  (or (g-value win :y-axis-value-suffix) ""))))))


;;; X-AXIS-TICK On the X-axis at y position Y-ORIGIN-W (window coordinates) and at x position X (data coordinates),
;;; adds a tick mark and number label derived from X to the AXIS-AGG of window WIN.
(defun x-axis-tick (axis-agg win y-origin-w x tick-count)
  (let ((include-label (and (= 0 (mod tick-count (1+ (the fn (g-value win :x-axis-tick-skip)))))
			    (or (not (= x 0)) (g-value win :include-x-tick-at-0))))
	(include-mark (and (= 0 (mod tick-count (1+ (the fn (g-value win :x-axis-tick-mark-skip)))))
			   (or (not (= x 0)) (g-value win :include-x-tick-at-0)))))
    (when include-label (x-axis-tick-label axis-agg y-origin-w x))
    (unless (or (g-value win :polar-circles-p) (not include-mark))
      (x-axis-tick-mark axis-agg y-origin-w x include-label))))


(defun x-axis-tick-label (axis-agg y-origin-w x &optional actual-x-position-w)
  (let* ((text (opal:add-component axis-agg (create-instance nil axis-text (:label-position :center))))
	 (win (g-value axis-agg :window))
	 (left (round (+ (or actual-x-position-w (x-plot-win-float-unbounded x win))
					; (* 0.5 (the fn (g-value text :width)))
			 ))))
    (s-value text :window win)
    (s-value text :top (+ y-origin-w
			  (case (g-value axis-agg :window :x-label-vertical-position)
			    (:below 10)
			    (:above (- (+ 10 (the fn (g-value (window-plot-axis-font win) :font-height)))))
			    (t 0))))
    (s-value text :string (x-axis-number-string win x))
    (s-value text :left left)
    text))

(defun x-axis-tick-mark (axis-agg y-origin-w x include-label)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (fixnum y-origin-w)
	   (single-float x))
  (let ((win (g-value axis-agg :window)))
    (opal:add-component
     axis-agg
     (create-instance nil opal:line
		      (:constant t)
		      (:y1 (+ y-origin-w
			      (round (the sf (* (if include-label 1.0 0.6)
						(case (g-value axis-agg :window :x-label-vertical-position)
						  (:below (g-value win :x-axis-tick-mark-length))
						  (:above (- (g-value win :x-axis-tick-mark-length)))
						  (t 0)))))))
		      (:y2 (+ y-origin-w
			      (case (g-value axis-agg :window :plot-type)
				(:polar (case (g-value axis-agg :window :x-label-vertical-position)
					  (:below (- (g-value win :x-axis-tick-mark-length)))
					  (:above (g-value win :x-axis-tick-mark-length))
					  (t 0)))
				(t 0))))
		      (:x1 (x-plot-win-float-unbounded x win))
		      (:x2 (x-plot-win-float-unbounded x win))))))


(defun y-axis-tick (axis-agg win x-origin-w y tick-count)
  (let ((include-label (and (= 0 (mod tick-count (1+ (g-value win :y-axis-tick-skip))))
			    (or (not (= y 0)) (g-value win :include-y-tick-at-0))))
	(include-mark (and (= 0 (mod tick-count (1+ (g-value win :y-axis-tick-mark-skip))))
			   (or (not (= y 0)) (g-value win :include-y-tick-at-0)))))
    (when include-label (y-axis-tick-label axis-agg x-origin-w y))
    (unless (or (g-value win :polar-circles-p) (not include-mark))
      (y-axis-tick-mark axis-agg x-origin-w y include-label))))


(defun y-axis-tick-mark (axis-agg x-origin-w y include-label)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (fixnum x-origin-w)
	   (single-float y))
  (let ((win (g-value axis-agg :window)))
    (opal:add-component
     axis-agg
     (create-instance nil opal:line
		      (:constant t)
		      (:x1 (+ x-origin-w
			      (* (case (g-value axis-agg :window :y-label-horizontal-position)
				   (:left -1)
				   (:right 1)
				   (t -1))
				 (the fn (round (the sf (* (if include-label 1.0 0.65)
							   (g-value win :y-axis-tick-mark-length))))))))
		      (:x2 (+ x-origin-w
			      (case (g-value win :plot-type)
				(:polar (g-value win :y-axis-tick-mark-length))
				(t 0))))
		      (:y1 (y-plot-win-float-unbounded y win))
		      (:y2 (y-plot-win-float-unbounded y win))))))


(defun y-axis-tick-label (axis-agg x-origin-w y &optional actual-y-position-w)
  (let ((text (opal:add-component axis-agg (create-instance nil axis-text (:label-position (g-value axis-agg :window :y-label-horizontal-position)))))
	(win (g-value axis-agg :window)))
    (s-value text :window win)
    (s-value text :top (round (- (or actual-y-position-w (y-plot-win-float-unbounded y win))
				 (* 0.5 (the fn (g-value (window-plot-axis-font win) :font-height))))))
    (s-value text :string (y-axis-number-string win y))
    (s-value text :left (+ x-origin-w
			   (* (case (g-value axis-agg :window :y-label-horizontal-position)
				(:left -1)
				(:right 1)
				(t -1))
			      (+ 5	; for tick mark
				 (case (g-value axis-agg :window :y-label-horizontal-position) ; a little gap
				   (:left 10)
				   (:right 10)
				   (t 10))))))
    text))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Point list functions
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(proclaim '(inline log-or-not))
(defun log-or-not (data-value enable-log base-p base)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (single-float base data-value))
  (the sf (if enable-log
	      (if base-p
		  (/ (the sf (log data-value)) (the sf (log base)))
		  (the sf (log data-value)))
	      data-value)))


;; get-plot-point-list Note that X-SEQ and Y-SEQ can be either lists and/or simple-arrays of
;; single-floats. :X-TRACE-OFFSET and :Y-TRACE-OFFSET are applied to the data *after*, and
;; :X-DATA-OFFSET and :Y-DATA-OFFSET are applied to the data *before* log, if enabled.

(defun get-plot-point-list (x-seq y-seq win &key (x-trace-offset 0.0) (y-trace-offset 0.0) (use-timed-data-x-constraints t) only-visible)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let* (point-list
	 (x-start (s-flt (or (g-value win :delta-t-start) 0)))
	 (x-trace-offset (s-flt (or x-trace-offset 0.0)))
	 (y-trace-offset (s-flt (or y-trace-offset 0.0)))
	 (margin (the fn (round *GET-PLOT-POINT-LIST-margin*)))
	 (base (the sf (or (g-value win :log-base) e-single)))
	 (base-p (g-value win :log-base))

	 ;; These are in pixels
	 (y-max-pixels (min (1- (the fn (g-value win :y-bound-max))) (the fn (+ margin (the fn (g-value win :height))))))
	 (x-max-pixels (min (1- (the fn (g-value win :x-bound-max))) (the fn (+ margin (the fn (g-value win :width))))))
	 (y-min-pixels (max (1+ (the fn (g-value win :x-bound-min))) (- margin)))
	 (x-min-pixels (max (1+ (the fn (g-value win :x-bound-min))) (- margin)))

	 ;; These are in data units
	 (x-min (g-value win :x-min))
	 (x-max (g-value win :x-max))
	 (y-min (g-value win :y-min))
	 (y-max (g-value win :y-max))
	 (x-log (g-value win :x-log)) (y-log (g-value win :y-log))
	 (x-data-offset (s-flt (or (g-value win :x-data-offset) 0)))
	 (y-data-offset (s-flt (or (g-value win :y-data-offset) 0)))
	 (last-x 0) (last-y 0) (this-x 0) (this-y 0))
    (declare (single-float x-data-offset y-data-offset x-min x-max y-trace-offset x-trace-offset)
	     (fixnum last-x last-y this-x this-y y-max-pixels x-max-pixels y-min-pixels x-min-pixels))
    (cond ((and (listp x-seq) (listp y-seq)) ; For now almost all cases are with both x and y seq as lists
	   (setq point-list (get-plot-point-list-from-lists
			     x-seq x-log x-min x-max x-min-pixels x-max-pixels x-data-offset x-trace-offset use-timed-data-x-constraints
			     y-seq y-log y-min y-max y-min-pixels y-max-pixels y-data-offset y-trace-offset
			     base base-p win only-visible)))
	  ((and (numberp x-seq) (listp y-seq))
	   (let ((delta-x-float (float x-seq)))
	     (do ((x x-start (the sf (+ x delta-x-float)))
		  (y-seq y-seq (cdr y-seq)))
		 ((null y-seq) point-list)
	       (let ((offsetted-x (+ x-data-offset (s-flt x)))
		     (offsetted-y (+ y-data-offset (s-flt (car y-seq)))))
		 (when (or x-log (not use-timed-data-x-constraints)
			   (<= x-min offsetted-x x-max))
		   (setq this-x (x-plot-win-float (the sf (+ x-trace-offset (log-or-not offsetted-x x-log base-p base))) win t)
			 this-y (y-plot-win-float (the sf (+ y-trace-offset (log-or-not offsetted-y y-log base-p base))) win t))))
	       (when (and (not (and (= this-x last-x) (= this-y last-y)))
			  (<= x-min-pixels this-x x-max-pixels)
			  (<= y-min-pixels this-y y-max-pixels))
		 (push this-y point-list)
		 (push this-x point-list)
		 (setq last-x this-x last-y this-y)))))
	  ((and (listp x-seq) (arrayp y-seq))
	   (do* ((x-seq x-seq (cdr x-seq))
		 (index 0 (1+ index)))
	       ((or (= index (1- (length (the (simple-array sf (*)) y-seq)))) (null x-seq)) point-list)
	     (push (y-plot-win-float (the sf (+ y-trace-offset (the sf (log (aref (the (simple-array sf (*)) y-seq) index))))) win t) point-list)
	     (push (x-plot-win-float (+ x-trace-offset (s-flt (car x-seq))) win t) point-list)))
	  ((and (arrayp x-seq) (listp y-seq))
	   (do* ((y-seq y-seq (cdr y-seq))
		 (index 0 (1+ index)))
	       ((or (= index (1- (length (the (simple-array sf (*)) x-seq)))) (null y-seq)) point-list)
	     (push (y-plot-win-float (+ y-trace-offset (the sf (log (s-flt (car y-seq))))) win t) point-list)
	     (push (x-plot-win-float (+ x-trace-offset (aref (the (simple-array sf (*)) x-seq) index)) win t) point-list)))
	  (t				; Both sequences are arrays.
	   (dotimes (index (min (length (the (simple-array sf (*)) y-seq)) (length (the (simple-array sf (*)) x-seq))))
	     (push (y-plot-win-float (+ y-trace-offset (the sf (log (aref (the (simple-array sf (*)) y-seq) index)))) win t) point-list)
	     (push (x-plot-win-float (+ x-trace-offset (aref (the (simple-array sf (*)) x-seq) index)) win t) point-list))))
    point-list))

(defun get-xfrmed-point-list (x-seq y-seq win)
  (let* (point-list
	 (x-start (or (g-value win :delta-t-start) 0.0))
	 (x-trace-offset (s-flt (or (g-value win :x-trace-offset) 0.0)))
	 (y-trace-offset (s-flt (or (g-value win :y-trace-offset) 0.0)))
	 (base (the sf (or (g-value win :log-base) e-single)))
	 (base-p (g-value win :log-base))

	 (x-seq (typecase x-seq
		  (cons x-seq)
		  (number (let ((delta-x-float (s-flt x-seq))
				out)
			    (do ((x x-start (the sf (+ x delta-x-float)))
				 (y-seq y-seq (cdr y-seq)))
				((null y-seq) (reverse out))
			      (push x out))))))
		   
	 ;; These are in data units
	 (x-min (g-value win :x-min))
	 (x-max (g-value win :x-max))

	 (x-log (g-value win :x-log)) (y-log (g-value win :y-log))
	 (x-data-offset (s-flt (or (g-value win :x-data-offset) 0)))
	 (y-data-offset (s-flt (or (g-value win :y-data-offset) 0)))
	 x-out y-out)
    (do ((x-seq x-seq (cdr x-seq))
	 (y-seq y-seq (cdr y-seq)))
	((null y-seq) point-list)
      (let ((offsetted-x (+ x-data-offset (s-flt (car x-seq))))
	    (offsetted-y (+ y-data-offset (s-flt (car y-seq)))))
	(when  (<= x-min offsetted-x x-max)
	  (push (the sf (+ x-trace-offset (log-or-not offsetted-x x-log base-p base))) x-out)
	  (push (the sf (+ y-trace-offset (log-or-not offsetted-y y-log base-p base))) y-out))))
    (values (reverse x-out) (reverse y-out)))) 



(defvar *debug-plot-border-point* nil)


(defun get-plot-point-list-from-lists (x-seq x-log x-min x-max x-off-win-min-pixels x-off-win-max-pixels
					     x-data-offset x-trace-offset use-timed-data-x-constraints 
					     y-seq y-log y-min y-max y-off-win-min-pixels y-off-win-max-pixels
					     y-data-offset y-trace-offset 
					     base base-p win only-visible)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (ignore use-timed-data-x-constraints) 
	   (fixnum x-off-win-min-pixels x-off-win-max-pixels y-off-win-min-pixels y-off-win-max-pixels)
	   (single-float x-trace-offset x-data-offset y-trace-offset y-data-offset base x-min x-max y-min y-max))
  (let* ((x-min-limit (G-VALUE WIN :x-min-limit))
	 (x-max-limit (G-VALUE WIN :x-max-limit))
	 (x-bound-min (G-VALUE WIN :x-bound-min))
	 (x-bound-max (G-VALUE WIN :x-bound-max))
	 (width (G-VALUE WIN :width))
	 (x-plot-left-gap (G-VALUE WIN :x-plot-left-gap))
	 (x-min (G-VALUE WIN :x-min))
	 (x-mag (G-VALUE WIN :x-mag))
	 (plot-area-width (G-VALUE WIN :plot-area-width))
    
	 (y-min-limit (G-VALUE WIN :y-min-limit))
	 (y-max-limit (G-VALUE WIN :y-max-limit))
	 (y-bound-min (G-VALUE WIN :y-bound-min))
	 (y-bound-max (G-VALUE WIN :y-bound-max))
	 (height (G-VALUE WIN :height))
	 (y-plot-bottom-gap (G-VALUE WIN :y-plot-bottom-gap))
	 (y-min (G-VALUE WIN :y-min))
	 (y-mag (G-VALUE WIN :y-mag))
	 (plot-area-height (G-VALUE WIN :plot-area-height))

	 (last-x-pixels 0) (this-x-pixels 0) (last-y-pixels 0) (this-y-pixels 0)
	 (x-off-plot-min-pixels (x-plot-win-float x-min win))
	 (x-off-plot-max-pixels (x-plot-win-float x-max win))
	 (y-off-plot-min-pixels (y-plot-win-float y-max win))
	 (y-off-plot-max-pixels (y-plot-win-float y-min win))
	 (x-left-border-pixels (if (g-value win :apply-horizontal-borders) x-off-plot-min-pixels x-off-win-min-pixels))
	 (x-right-border-pixels (if (g-value win :apply-horizontal-borders) x-off-plot-max-pixels x-off-win-max-pixels))
	 (y-left-border-pixels (if (g-value win :apply-vertical-borders) y-off-plot-min-pixels y-off-win-min-pixels))
	 (y-right-border-pixels (if (g-value win :apply-vertical-borders) y-off-plot-max-pixels y-off-win-max-pixels))
	 (plot-point-skip-mod (the fn (or (when (numberp (g-value win :plot-point-skip)) (1+ (the fn (g-value win :plot-point-skip))))
					  1)))
	 (plot-point-skip-mod-one-p (= plot-point-skip-mod 1))
	 point-list this-point-visible last-point-valid)
    (declare (fixnum last-x-pixels last-y-pixels x-off-plot-min-pixels x-off-plot-max-pixels y-off-plot-min-pixels y-off-plot-max-pixels))
    (do ((x-seq x-seq (cdr x-seq))
	 (y-seq y-seq (cdr y-seq))
	 (count 0 (1+ count)))
	((or (null x-seq) (null y-seq)) point-list)
      (declare (fixnum count))
					;      (format t "count ~a, x-data-offset ~a, x ~a, y-data-offset ~a, y ~a ~%"  count x-data-offset (car x-seq) y-data-offset (car y-seq) )
      (when (or plot-point-skip-mod-one-p (zerop (mod (the (unsigned-byte 29) count) (the (unsigned-byte 29) plot-point-skip-mod))))
	(let* ((offst-x-data-sf (+ x-data-offset (the sf (car x-seq))))
	       (offset-log-x (+ x-trace-offset (log-or-not offst-x-data-sf x-log base-p base)))
	       (offst-y-data-sf (+ y-data-offset (the sf (car y-seq))))
	       (offset-log-y (+ y-trace-offset (log-or-not offst-y-data-sf y-log base-p base)))
	       (last-point-visible this-point-visible)
	       this-x-pixels 
	       this-y-pixels)

					; (declare (fixnum this-x-pixels this-y-pixels))
					;  (format t "ready...  ")
	  (setq this-x-pixels 
		(x-plot-win-float-bounded-w-win-args offset-log-x
						     x-min-limit x-max-limit
						     x-bound-min x-bound-max
						     width x-plot-left-gap x-min x-mag plot-area-width))
		       
					;       (format t "   x-pix ~a, y-pix ~a ~%"  this-x-pixels this-y-pixels)
	  (setq 	        this-y-pixels (y-plot-win-float-bounded-w-win-args offset-log-y y-min-limit y-max-limit
										   y-bound-min y-bound-max
										   height y-plot-bottom-gap y-min
										   y-mag plot-area-height))
					;			        (format t "   x-pix ~a, y-pix ~a ~%"  this-x-pixels this-y-pixels)
	  (unless (and (= this-x-pixels last-x-pixels) (= this-y-pixels last-y-pixels))
	    (if  (and (<= x-left-border-pixels this-x-pixels x-right-border-pixels)
		      (<= y-left-border-pixels this-y-pixels y-right-border-pixels))
	      (progn
		(setq this-point-visible t)
		(unless last-point-visible
		  (when (and (not only-visible) (g-value win :include-border-points) last-point-valid)
		    (multiple-value-bind (border-x-pixels border-y-pixels)
			(border-point last-x-pixels this-x-pixels last-y-pixels this-Y-pixels
				      x-left-border-pixels x-right-border-pixels
				      y-left-border-pixels y-right-border-pixels
				      t)
		      (push nil point-list)
		      (push border-y-pixels point-list)
		      (push border-x-pixels point-list))))
		(push this-y-pixels point-list)
		(push this-x-pixels point-list))
	      (progn
		(setq this-point-visible nil)
		(when (and last-point-visible (not only-visible) (g-value win :include-border-points))
		  (multiple-value-bind (border-x-pixels border-y-pixels)
		      (border-point last-x-pixels this-x-pixels last-y-pixels this-Y-pixels
				    x-left-border-pixels x-right-border-pixels
				    y-left-border-pixels y-right-border-pixels
				    nil)
		    (push border-y-pixels point-list)
		    (push border-x-pixels point-list)
		    (push nil point-list)))))
	    (setq last-x-pixels this-x-pixels last-y-pixels this-y-pixels last-point-valid t)))))
    point-list))

(defun border-point (last-x-pixels this-x-pixels last-y-pixels this-Y-pixels 
				   x-off-plot-min-pixels x-off-plot-max-pixels
				   y-off-plot-min-pixels y-off-plot-max-pixels
				   previous-point-hidden)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (fixnum last-x-pixels this-x-pixels last-y-pixels this-Y-pixels 
				   x-off-plot-min-pixels x-off-plot-max-pixels
				   y-off-plot-min-pixels y-off-plot-max-pixels))
  (let* ((hidden-x-pixels (if previous-point-hidden last-x-pixels this-x-pixels))
	 (shown-x-pixels (if previous-point-hidden this-x-pixels last-x-pixels))
	 (hidden-y-pixels (if previous-point-hidden last-y-pixels this-y-pixels))
	 (shown-y-pixels (if previous-point-hidden this-y-pixels last-y-pixels))
   
	 (diff-vector-x (s-flt (- hidden-x-pixels shown-x-pixels)))
	 (diff-vector-y (s-flt (- hidden-y-pixels shown-y-pixels)))
	 (diff-vector-angle (if (= diff-vector-x 0)
				(* (if (> diff-vector-y 0) 1 -1) (/ pi 2))
				(atan diff-vector-y diff-vector-x)))
	 final-diff-vector-x final-diff-vector-y)

    (when nil ; *debug-plot-border-point*
      (format t "hidden-x-pixels ~A, hidden-y-pixels ~A, shown-x-pixels ~A, shown-y-pixels ~A~%"
	      hidden-x-pixels hidden-y-pixels shown-x-pixels shown-y-pixels)
      (format t "x-off-plot-min-pixels ~A, x-off-plot-max-pixels ~A ~%"
	      x-off-plot-min-pixels x-off-plot-max-pixels)
      (format t "y-off-plot-min-pixels ~A, y-off-plot-max-pixels ~A ~%"
	      y-off-plot-min-pixels y-off-plot-max-pixels)
      (format t "diff-vector-angle ~A, diff-vector-x ~A, diff-vector-y ~A~%"
	      diff-vector-angle diff-vector-x diff-vector-y))

    (cond (;; Top border
	   (and (<= hidden-x-pixels (+ x-off-plot-max-pixels (- hidden-y-pixels) y-off-plot-min-pixels))
		(>= hidden-x-pixels (+ hidden-y-pixels (- y-off-plot-min-pixels) x-off-plot-min-pixels))
		(<= hidden-y-pixels y-off-plot-min-pixels))
	   (when *debug-plot-border-point* (format t "over top border  ~%"))
	   (setq final-diff-vector-y y-off-plot-min-pixels)
	   (setq final-diff-vector-x
		 (+ shown-x-pixels (round (/ (- final-diff-vector-y shown-y-pixels) (tan diff-vector-angle))))))
	  ;; Bottom border
	  ((and (>= hidden-x-pixels (+ y-off-plot-max-pixels (- hidden-y-pixels) x-off-plot-min-pixels))
		(<= hidden-x-pixels (+ hidden-y-pixels x-off-plot-max-pixels (- y-off-plot-max-pixels)))
		(>= hidden-y-pixels y-off-plot-max-pixels))
	   (when *debug-plot-border-point* (format t "over bottom border  ~%"))
	   (setq final-diff-vector-y y-off-plot-max-pixels)
	   (setq final-diff-vector-x
		 (+ shown-x-pixels (round (/ (- final-diff-vector-y shown-y-pixels) (tan diff-vector-angle))))))
	  ;; Left border
	  ((and (>= hidden-y-pixels (+ hidden-x-pixels (- x-off-plot-min-pixels) y-off-plot-min-pixels))
		(<= hidden-y-pixels (+ y-off-plot-max-pixels (- hidden-x-pixels) x-off-plot-min-pixels))
		(<= hidden-x-pixels x-off-plot-min-pixels))
	   (when *debug-plot-border-point* (format t "over left border  ~%"))
	   (setq final-diff-vector-x x-off-plot-min-pixels)
	   (setq final-diff-vector-y
		 (round (+ shown-y-pixels (* (- final-diff-vector-x shown-x-pixels) (tan diff-vector-angle))))))
	  ;; Right border
	  ((and (>= hidden-y-pixels (+ x-off-plot-max-pixels (- hidden-x-pixels) y-off-plot-min-pixels))
		(<= hidden-y-pixels (+ hidden-x-pixels (- x-off-plot-max-pixels) y-off-plot-max-pixels))
		(>= hidden-x-pixels x-off-plot-max-pixels))
	   (when *debug-plot-border-point* (format t "over right border  ~%"))
	   (setq final-diff-vector-x x-off-plot-max-pixels)
	   (setq final-diff-vector-y
		 (round (+ shown-y-pixels (* (- final-diff-vector-x shown-x-pixels) (tan diff-vector-angle)))))))
    (unless (and final-diff-vector-x final-diff-vector-y) (break))
    (when *debug-plot-border-point*
      (format t " final-diff-vector-x ~A, final-diff-vector-y ~A~%" final-diff-vector-x final-diff-vector-y))
    (values final-diff-vector-x final-diff-vector-y)))
	    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;
;;; Density Plotting
;;;
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-2d-density-array (win array array-width-x array-width-y left-border top-border rect-width rect-height z-min z-amp color invert)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (type (array single-float (* *)) array)
	   (fixnum left-border rect-width top-border rect-height array-width-x array-width-y)
	   (single-float z-min z-amp))
  (let ((v-agg (create-instance
		nil opal:virtual-aggregate
		(:item-prototype virtual-rectangle)
		(:item-array
		 (list-to-array-generic
		  (loop for left fixnum from left-border by rect-width
			for x fixnum from 0 to (the fn (1- array-width-x))
			nconc (loop for y fixnum from 0 to (the fn (1- array-width-y))
				    for top fixnum from top-border by rect-height
				    collect 
				    ;;x y w h c
				    (let ((val (the sf (/ (the sf (- (aref array x y) z-min)) z-amp))))
				      (list left top rect-width rect-height
					    (get-number-fill
					     (if invert (- 1 val) val)
					     color)))))))
		(:point-in-item nil))))
    (opal:add-component (g-value win :aggregate) v-agg :where :front)
    (resurrect-opal-win win)))	    

(defun add-density-plot-axes (win x-min x-max y-min y-max x-label-text y-label-text
				  array-width-x array-width-y left-border rect-width top-border rect-height)				  
  (let* ((axes-agg (get-plot-agg win 'data-axes t)) ; Get a data-axis agg and clear it.
	 (axis-line-style (or (g-value win :axis-line-style) *default-axis-line-style*))
	 (x-axis-y-position (round (+ top-border (* rect-height array-width-y) (/ rect-height 2))))
	 (y-axis-x-position (round (- left-border 10 ; (/ rect-width 2)
				      )))
	 (x-inc (/ (- x-max x-min) array-width-x))
	 (y-inc (/ (- y-max y-min) array-width-y))
	 
	 (tick-length 5))
    ;; X axis
    (when (g-value win :x-axis-p)
      (let ((x-label
	     (create-instance nil axis-text
			      (:label-position (o-formula (or (gvl :window :x-label-vertical-position) :center)))
			      (:left (round  (/ (g-value win :width) 2)))
			      (:top (o-formula
				     (+ x-axis-y-position
					(if (gvl :window)
					    (g-value (window-plot-axis-font (gvl :window)) :font-height) ; Tick Numbers
					    0)
					tick-length
					5))))))
	(s-value x-label :window win)
	(opal:add-component axes-agg x-label)
	(s-value x-label :string x-label-text)

	(loop for x-tick-position from left-border by rect-width
	      for x-value from x-min by x-inc
	      for count from 0 to array-width-x
	      do
	      (when (= 0 (mod count (1+ (g-value win :x-axis-tick-skip))))
		(x-axis-tick-label axes-agg x-axis-y-position x-value x-tick-position))
	      (when (= 0 (mod count (1+ (g-value win :x-axis-tick-mark-skip))))
	    
		(opal::add-component
		 axes-agg
		 (create-instance nil opal:line (:constant t) (:line-style axis-line-style)
				  (:x1 x-tick-position)
				  (:x2 x-tick-position)
				  (:y1 x-axis-y-position)
				  (:y2 (+ (if (= 0 (mod count (1+ (g-value win :x-axis-tick-skip))))
					      tick-length (- tick-length 2))
					  x-axis-y-position))))))
	(opal::add-component
	 axes-agg
	 (create-instance nil opal:line (:constant t) (:line-style axis-line-style)
			  (:x1 left-border)
			  (:x2 (+ left-border (* rect-width array-width-x)))
			  (:y1 x-axis-y-position)
			  (:y2 x-axis-y-position)))))

    ;; Y axis
    (when (g-value win :y-axis-p)
      (let ((y-label
	     (create-instance nil axis-text
			      (:label-position (o-formula (or (gvl :window :y-label-horizontal-position) :left))) 
			      (:left y-axis-x-position)
			      (:top (o-formula
				     (- top-border 
					(+ (if (gvl :window)
					       (g-value (window-plot-axis-font (gvl :window)) :font-height)
					       0)
					   8)))))))
	(s-value y-label :window win)
	(opal:add-component axes-agg y-label)
	(s-value y-label :string y-label-text)
	(loop for y-tick-position from top-border by rect-height
	      for y-value from y-max by (- y-inc)
	      for count from 0 to array-width-y
	      do
	      (when (= 0 (mod count (1+ (g-value win :y-axis-tick-skip))))
		(y-axis-tick-label axes-agg y-axis-x-position y-value y-tick-position))
	      (when (= 0 (mod count (1+ (g-value win :y-axis-tick-mark-skip))))
		(opal::add-component
		 axes-agg
		 (create-instance nil opal:line (:constant t) (:line-style axis-line-style)
				  (:y1 y-tick-position)
				  (:y2 y-tick-position)
				  (:x1 y-axis-x-position)
				  (:x2 (- y-axis-x-position
					  (if (= 0 (mod count (1+ (g-value win :y-axis-tick-skip))))
					      tick-length (- tick-length 2))))))))
	(opal::add-component
	 axes-agg
	 (create-instance nil opal:line (:constant t) (:line-style axis-line-style)
			  (:y1 top-border)
			  (:y2 (+ top-border (* rect-height array-width-y)))
			  (:x1 y-axis-x-position)
			  (:x2 y-axis-x-position)))))))

(defun density-plot-scale (label &key (min 0.0) (max 1.0)
				 (y-incs 10) y-min y-max
				 (z-min 0) (z-max 1.00)
				 (width 100) (height 400)
				 (left-border 60))
  (let ((scale-array (make-array '(1 100) :element-type 'single-float)))
    (loop for val from (s-flt min) to max by (/ (- max min) 100.0)
	  for i from 0 to 99
	  do (setf (aref scale-array 0 i) val))
    (density-plot scale-array :y-label label
		  :width width :height height
		  :element-aspect-ratio 1.0
		  :vertical-border 50
		  :right-border 15
		  :left-border left-border 
		  :y-min y-min :y-max y-max
		  :z-min z-min :z-max z-max
		  :x-axis-p  nil
		  :y-are-fns t
		  :y-axis-tick-skip (round (1- (/ 100 y-incs)))
		  :y-axis-tick-mark-skip (round (1- (/ 100 y-incs))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Baselines 
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mark-baseline-menu (win &optional edited-baseline)
  (let ((dummy1 (if edited-baseline (g-value edited-baseline :x-start) (g-value win :x-min)))
	(dummy2 (if edited-baseline (g-value edited-baseline :x-stop) (g-value win :x-max)))
	(dummy3 (if edited-baseline (g-value edited-baseline :y-start) (g-value win :y-min)))
	(dummy4 (if edited-baseline (g-value edited-baseline :y-stop) (g-value win :y-max)))
	dummy5
	dummy6
	(line-style (if edited-baseline (g-value edited-baseline :line-style) (or (g-value win :baseline-line-style) thick-black-line))))
    (choose-variable-values
     `((dummy1 ,(format nil "Start X ~A" (or (g-value win :x-label) ""))  :float)
       (dummy2 ,(format nil "Stop X ~A" (or (g-value win :x-label) "")) :float)
       (dummy3 ,(format nil "Start Y ~A" (or (g-value win :y-label) "")) :float)
       (dummy4 ,(format nil "Stop Y ~A" (or (g-value win :y-label) "")) :float)
       (dummy6 ,(format nil "Choose different line-style~%[Default is ~A]" (opal::name-for-schema line-style)) :boolean)
       (dummy5 "CANCEL" :boolean))
     :title (format nil "Add line to ~A" (g-value win :title)))
    (unless dummy5
      (when edited-baseline (remove-baselines win (list edited-baseline)))
      (mark-baseline win dummy1 dummy2 dummy3 dummy4
		     :line-style
		     (or (when dummy6
			   (line-style-menu
			    :default-style line-style
			    :label (format nil "Choose a line style for baseline in ~A" (g-value win :title)))) 
			 line-style)))))

(defun mark-plot-wins-at-time (time &key (win (standard-plot-windows)) (line-style thick-black-line))
  "Add a vertical LINE-STYLE line at TIME, from :y-min to :y-max, to each plot window referenced by WIN."
  (let ((time (float time)))
    (loop for win in (coerce-to-list win)
	  when (eq :standard-plot (g-value win :mode))
	  do (mark-baseline win time time (g-value win :y-min) (g-value win :y-max) :line-style line-style))))

(defun mark-baselines (win coordinates &key (update t) (line-style thick-black-line) clear-previous)
  (when clear-previous (remove-baselines win))
  (loop for values in coordinates do
	(mark-baseline win (first values) (second values) (third values) (fourth values) :update update :line-style line-style)))

(defun mark-baseline (win x-start x-stop y-start y-stop &key (update t) (line-style thick-black-line) clear-previous)
  (when clear-previous (remove-baselines win))
  (s-value win :baselines (no-nils (loop for baseline in (g-value win :baselines) when (opal-obj-exists baseline) collect baseline)))
  (let* ((x-start (s-flt x-start))
	 (x-stop (s-flt x-stop))
	 (y-start (s-flt y-start))
	 (y-stop (s-flt y-stop))
	 (previous-one (loop for baseline in (g-value win :baselines)
			     when (and (= x-start (g-value baseline :x-start)) (= x-stop (g-value baseline :x-stop))
				       (= y-start (g-value baseline :y-start)) (= y-stop (g-value baseline :y-stop))
				       (eq line-style (g-value baseline :line-style)))
			     do (return baseline)))
	 (x-start-pixels (x-plot-win-float x-start win))
	 (x-stop-pixels (x-plot-win-float x-stop win))
	 (y-start-pixels (y-plot-win-float y-start win))
	 (y-stop-pixels (y-plot-win-float y-stop win))
	 (all-ends-in-window-p (and (<= 0 x-start-pixels (g-value win :width))
				    (<= 0 x-stop-pixels (g-value win :width))
				    (<= 0 y-start-pixels (g-value win :height))
				    (<= 0 y-stop-pixels (g-value win ::height)))))
    (or previous-one
	(let ((aggad
	       (create-instance
		nil opal:aggregadget
		(:label t) (:x-start x-start) (:x-stop x-stop)
		(:y-start y-start) (:y-stop y-stop)
		(:line-style line-style)
		(:v-bar-width 10) (:h-bar-width 10)
		(:line-style line-style)
		(:parts
		 (when t ; all-ends-in-window-p
		   `((:line
		      ,opal:line
		      (:line-style ,(o-formula (gvl :parent :line-style)))
		      (:known-as :line)
		      (:x1 ,(o-formula (x-plot-win-float (gvl :parent :x-start) (gvl :parent :window))))
		      (:x2 ,(o-formula (x-plot-win-float (gvl :parent :x-stop) (gvl :parent :window))))
		      (:y1 ,(o-formula (y-plot-win-float (gvl :parent :y-start) (gvl :parent :window))))
		      (:y2 ,(o-formula (y-plot-win-float (gvl :parent :y-stop) (gvl :parent :window)))))))))))
	  (s-value win :baseline-line-style line-style)
	  (push (opal:add-component (g-value win :aggregate) aggad) (g-value win :baselines))
	  (when update (resurrect-opal-win win))
	  aggad))))

(defun frame-plot (&optional win (line-style thin-line))
  (loop for win in (coerce-to-list (or win (win-menu "Select Plot to Add Frame" (standard-plot-windows)))) do
	(let ((x-min (g-value win :x-min))
	      (x-max (g-value win :x-max))
	      (y-min (g-value win :y-min))
	      (y-max (g-value win :y-max)))
	  (loop for x-start in (list x-min x-min x-min x-max)
		for x-stop  in (list x-max x-max x-min x-max)
		for y-start in (list y-min y-max y-min y-min)
		for y-stop  in (list y-min y-max y-max y-max)
		do
		(mark-baseline win x-start x-stop y-start y-stop :line-style line-style)))))

(defun mark-plot-origin-axises (win &optional (line-style thin-line))
  (loop for win in (coerce-to-list win) do
	(mark-baseline win  0.0 0.0 (g-value win :y-min) (g-value win :y-max) :line-style line-style)
	(mark-baseline win (g-value win :x-min) (g-value win :x-max) 0.0 0.0 :line-style line-style)))


(defun mark-plot-odd-quadrant-diagonal (plot-win &key (line-style thin-dotted-line-2))
  (loop for plot-win in (coerce-to-list plot-win) do
	(mark-baseline plot-win
		       (g-value plot-win :x-min) (g-value plot-win :x-max)
		       (g-value plot-win :y-min) (g-value plot-win :y-max)
		       :line-style line-style)))

(defun mark-plot-even-quadrant-diagonal (plot-win &key (line-style thin-dotted-line-2))
  (loop for plot-win in (coerce-to-list plot-win) do
	(mark-baseline plot-win
		       (g-value plot-win :x-min) (g-value plot-win :x-max)
		       (g-value plot-win :y-max) (g-value plot-win :y-min)
		       :line-style line-style)))

(defun mark-plot-diagonal (plot-win &key (line-style thin-dotted-line-2))
  (mark-plot-odd-quadrant-diagonal plot-win :line-style line-style))


(defun update-baselines (win)
  (let ((original-points (no-nils (g-value win :baselines))))
    (s-value win :baselines nil)
    (loop for point in original-points
	  collect (mark-baseline win
				 (g-value point :x-start) (g-value point :x-stop)
				 (g-value point :y-start) (g-value point :y-stop)
				 :line-style (when (g-value point :line) (g-value point :line :line-style)))
	  into new-points
	  do (opal:remove-component (g-value win :aggregate) point) (opal:destroy point)
	  finally (s-value win :baselines new-points))))

(defun baseline-description-string (point win)
  (format nil "Baseline from [~,2f~d, ~,2f~d] to [~,2f~d, ~,2f~d]" 
	  (g-value point :x-start) (g-value win :x-label)
	  (g-value point :y-start) (g-value win :y-label)
	  (g-value point :x-stop) (g-value win :x-label)
	  (g-value point :y-stop) (g-value win :y-label)))

(defun remove-baselines (win &optional (removed-baselines :all))
  (loop for point in (no-nils (g-value win :baselines))
	when (or (eq removed-baselines :all)
		 (member point (if (listp removed-baselines) removed-baselines (list removed-baselines))))
	do (opal:remove-component (g-value win :aggregate) point)
	else collect point into points
	finally (s-value win :baselines points)))

(defun choose-baselines (win menu-label)
  (let ((baselines (g-value win :baselines)))
    (choose-list-values-from-keys 
     (loop for point in baselines collect (list (baseline-description-string point win) point)) nil :punt-if-only-one-entry nil :label menu-label)))

(defun edit-baselines (win)
  (remove-baselines win (choose-baselines win (format nil "Remove these baselines from ~A" (g-value win :title))))
  (loop for baseline in (choose-baselines win (format nil "Edit these baselines in ~A" (g-value win :title))) do (mark-baseline-menu win baseline))
  (resurrect-opal-win win))
	  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun remove-virtual-aggs (win)
  (loop for comp in (g-value win :aggregate :components)
	when (eq  opal:virtual-aggregate (car (g-value comp :is-a))) do
	(opal:remove-component (g-value win :aggregate) comp)
	(opal:destroy comp)))

(proclaim '(inline find-max-y-out-of-xy-list find-min-y))
;;; FIND-MAX-Y-OUT-OF-XY-LIST This takes a list of xy points (x0 y0 x1 y1 ... xn yn) and returns ymax.
(defun find-max-y-out-of-xy-list (point-list)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (loop for point in point-list
	for i from 0
	when (oddp (the fn i)) maximize (the fn point)))


;;; Find-Min-Y-Out-Of-Xy-List This takes a list of xy points (x0 y0 x1 y1 ... xn yn) and returns ymin.
(defun find-min-y-out-of-xy-list (point-list)
    (declare (optimize (safety 0) (speed 3) (space 0)))
  (loop for point in point-list
	for i from 0
	when (oddp (the fn i)) minimize (the fn point)))


;;; Make sure that all the labels are strings.
(defun clear-up-label-list (win label-list)
  (let ((label-list (coerce-to-list label-list)))
    (when (and label-list
	       (loop for label in label-list
		     unless (and (stringp label) (string= "" label))
		     do (return t)))
      (s-value win :label-list
	       (loop for count from 0 to (1- (num-curves-per-group win))
		     collect (let ((label (nth count label-list)))
			       (typecase label
				 (string label)
				 (number (princ-to-string label))
				 (t (if label (format nil "~A" label) "")))))))))


;; For getting a time sequence based on a DELTA-T and the length of the data lists
(defun make-time-sequence (window-y-lists delta-t &optional (start-time 0.0))
  (list-of-nums
   (1+ (loop for list in (if (consp (caar window-y-lists)) (car window-y-lists) window-y-lists)
	     maximize (length list)))
   start-time
   delta-t))

;; PARSED-XORY-LISTS (data-lists)
;; Also coerces to single float, and array to list conversion.
;;
;;  DATA-LISTS can be -
;;
;;  array
;;  (...)
;;  (array ... )
;;  (...)
;;  ((...) ... )
;;  (((...) ... ) ((...) ... ) ((...) ... ) ... )
;;
;; We want -
;;
;;  (((...) ... ) ((...) ... ) ((...) ... ) ((...) ... ) ((...) ... ) ....)
;;                                           \___________________________/
;;                                               old-lists when overlay

(defun parsed-xory-lists (data-lists)
  (cond
   ((arrayp data-lists)			; array
    (list (list (array-to-float-list data-lists)))) ; (((array-vals)))
   ((not (or (consp (car data-lists))	; (...)
	     (arrayp (car data-lists))))
    (list (list (float-list data-lists)))) ; (((data-list-vals)))
   ((or (arrayp (car data-lists))	; (array ...) - list of arrays
	(not (consp (caar data-lists)))) ;  ((...) ... ) - list of lists
    (list (loop for seq in data-lists collect (SEQUENCE-TO-float-LIST seq)))) ; (( (vals) (vals) (vals) ... ))
   (t (loop for list in data-lists collect (car (parsed-xory-lists list))))))


(defun load-y-lists (win data-lists)
  (loop for new-data in (reverse (parsed-xory-lists data-lists))
	do (s-value win :y-lists (cons new-data (when (g-value win :overlay) (g-value win :y-lists))))))


;;  TIME-SEQ can be -
;;
;;  array
;;  (...)
;;  (array ... )
;;  (...)
;;  ((...) ... )
;;  (((...) ... ) ((...) ... ) ((...) ... ) ... )
;;
;; We want -
;;
;;  (((...) ... ) ((...) ... ) ((...) ... ) ((...) ... ) ((...) ... ) ....)
;;                                           \___________________________/
;;                                               old-lists when overlay


(defun load-x-lists (win time-seq)
  (loop for new-data in (if (numberp time-seq) (list time-seq) (reverse (parsed-xory-lists time-seq)))
	do (s-value win :x-lists (cons new-data (when (g-value win :overlay) (g-value win :x-lists))))))

;;
;;  Format of x and y lists in window slots:
;;
;;  :y-lists -  '( ( (group-1, y-trace-1) (group-1, y-trace-2) ... (group-1, y-trace-n1) )
;;                 ( (group-2, y-trace-1) (group-2, y-trace-2) ... (group-2, y-trace-n2) )
;;                                             .
;;                                             .
;;                                             .
;;                 ( (group-m, y-trace-1) (group-m, y-trace-2) ... (group-m, y-trace-nm) ) )
;;
;;
;;  :x-lists -  '( ( (group-1, x-trace) ) or ( (group-1, x-trace-1) (group-1, x-trace-2) ... (group-1, x-trace-n1) )
;;                 ( (group-2, x-trace) ) or ( (group-2, x-trace-1) (group-2, x-trace-2) ... (group-2, x-trace-n1) )
;;                            .
;;                            .
;;                            .
;;                 ( (group-m, x-trace) ) or ( (group-m, x-trace-1) (group-m, x-trace-2) ... (group-m, x-trace-n1) ) )



(defun get-canonic-labels (num-curves-per-group canonic-label)
  (if (= num-curves-per-group 1)
      '(nil)
      (loop for i from 1 to num-curves-per-group collect (format nil "~a-~a" CANONIC-LABEL i))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Scatter plot functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun retrieve-scatter-v-aggs (win)
  (loop for comp in (g-value (get-plot-agg win 'data-plot) :components)
	when (eq (g-value comp :what-is-it) :data-points)
	collect comp))

(defun SET-VIRTUAL-THING-WIDTH-AND-HEIGHT (thing width height)
  (s-value thing :item-array
	   (list-to-array-generic
	    (loop for item-value across (g-value thing :item-array)
		  collect (list (nth 0 item-value) ; x-center
				(nth 1 item-value) ; y-center
				(- (nth 0 item-value) (floor (/ width 2))) ; x-left
				(+ (nth 0 item-value) (ceiling (/ width 2))) ; x-right
				(- (nth 1 item-value) (floor (/ height 2))) ; y-top
				(+ (nth 1 item-value) (ceiling (/ height 2))) ; y-bottom
				(nth 6 item-value) ; linestyle
				(nth 7 item-value) ; color-fill
				))))
  (opal::update (g-value thing :window) t))

(defun get-scatter-symbol (win curve-num)
  (if (consp (g-value win :scatter-symbol))
      (nth (mod curve-num (length (g-value win :scatter-symbol))) (g-value win :scatter-symbol))
      (case (g-value win :scatter-symbol)
	(:all (nth (mod curve-num (length *scatter-symbols*)) *scatter-symbols*))
	(t (g-value win :scatter-symbol)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Histo plot functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun add-histo-bins (plot-agg points y-base)
  (let* ((win (g-value plot-agg :window))
	 x
	 (y-base-pixels (when y-base (y-plot-win-float y-base win)))
	 (stipple-percent (or (g-value win :stipple-percent) (unless (g-value win :bar-border-p) 50)))
	 (line-style (when (g-value win :bar-border-p) thick-line-style))
	 (filling-style  (when stipple-percent (get-opal-color-to-fill 'black stipple-percent))))
    (loop for xy in points
	  for count from 0
	  when (evenp count) do (setq x xy)
	  when (oddp count) do (add-histo-bin plot-agg x xy t y-base-pixels line-style filling-style))))

(defun add-histo-bin (plot-agg x y points-are-in-plot-win-coords y-base-pixels line-style filling-style)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let* ((win (g-value plot-agg :window))
	 (bin-width (or (g-value win :bin-width) 1.0))
	 (x-pixels (the fn (if points-are-in-plot-win-coords (the fn x) (x-plot-win-float x win))))
	 (x-pixels+bin (the fn (if points-are-in-plot-win-coords
				   (+ (the fn x) (x-plot-win-distance bin-width win))
				   (x-plot-win-float (the sf (+ x bin-width)) win))))
	 (y-pixels (the fn (+ 0		; (or y-base-pixels 0)
			      (if points-are-in-plot-win-coords
				  (the fn y)
				  (y-plot-win-float (s-flt y) win))))))
    (when (> (- (the fn (or y-base-pixels (y-plot-win-float 0.0 win))) y-pixels) 0)
      (add-histo-bin-rectangle-to-agg
       plot-agg y-base-pixels line-style filling-style
       x-pixels x-pixels+bin
       y-pixels win))))

(defun add-histo-bin-rectangle-to-agg (plot-agg y-base-pixels line-style filling-style x-pixels x-pixels+bin y-pixels win)
  (opal:add-component 
   plot-agg
   (create-instance nil opal:rectangle
		    (:constant t)
		    (:left (- x-pixels 1))
		    (:width (+ 2 (- x-pixels+bin x-pixels)))
		    (:top y-pixels)
		    (:height (- (+ (the fn (or y-base-pixels (y-plot-win-float 0.0 win))) 1) y-pixels))
		    (:line-style line-style)
		    (:filling-style filling-style))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Waterfall plot functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; POINT-LIST-W-SKIRT This takes a list of xy points (x0 y0 x1 y1 ... xn yn) and returns (x0 ymax
;;; x0 y0 x1 y1 ... xn yn xn ymax), unless HEM-IS-MAX is nil, in which case the result is (x0 ymin
;;; x0 y0 x1 y1 ... xn yn xn ymin).
(defun point-list-w-skirt (point-list win)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let* ((last-x (the fn (car (last point-list 2))))
	 (hem-is-max (> (g-value win :y-trace-offset) 0.0))
	 (max-y (if (g-value win :skirt-to-window-border)
		  (if hem-is-max (g-value win :height) 0)
		  (if hem-is-max (find-max-y-out-of-xy-list point-list) (find-min-y-out-of-xy-list point-list)))))
    (cons (car point-list)		; x0
	  (cons max-y (append point-list (list last-x max-y))))))

;; POINTS must be fixnums.
;; MASSAGE-POINTS In case the POINTS list includes NILs, break up into separate lists.
(defun massage-points (points)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (if (loop for point in points unless point do (return t))
      (let (outs out)
	(loop for point in points
	      when point do (push point out)
	      else when out do (push (reverse out) outs) (setq out nil)
	      finally (return (if out (cons (reverse out) outs) outs))))
    (list points)))

(defun add-polyline-to-plot-point-list (points connect-ends)
  (if connect-ends
      (let ((first-x (first points))
	    (first-y (second points)))
	(nconc (nconc points (list first-x)) (list first-y)))
    points))

(defun new-add-polyline-to-plot (plot-agg points line-style connect-ends)
  (let ((massaged-points (massage-points points)))
    (loop for points in massaged-points do
	  (opal:add-component           
	   plot-agg (create-instance nil opal:polyline
				     (:constant t)
				     (:line-style line-style)
				     (:point-list (add-polyline-to-plot-point-list points connect-ends)))
	   :where :back))))


(defun add-wf-skirt (plot-agg win points)
  (opal:add-component
   plot-agg
   (create-instance nil opal:polyline
		    (:point-list (point-list-w-skirt points win))
		    (:line-style opal:no-line) (:constant t) (:filling-style opal:white-fill))
   :where :back))

(defun add-waterfall-label (plot-agg label x-start-offset y-start-offset)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (single-float x-start-offset y-start-offset)
	   (type kr::schema plot-agg))
  (let ((win (g-value plot-agg :window)))
    (unless (g-value win :waterfall-label-offset) (s-value win :waterfall-label-offset 0.0))
    (let ((text (opal:add-component plot-agg (create-instance nil axis-text (:label-position :right)))))
      (s-value text :left (the fn (+ (the fn (or (g-value win :gap-between-trace-and-waterfall-label) 20))
				     (x-plot-win-float (+ x-start-offset (the sf (g-value win :xfrmd-x-data-max))) win))))
      (s-value text :top (the fn (+ -30 (y-plot-win-float (+ (the sf (g-value win :y-min))
							     (the sf (g-value win :waterfall-label-offset))
							     y-start-offset)
							  win))))
      (s-value text :string label))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Key and label functions.
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Need to add these with o-formulas
(create-instance 'plot-key-label opal:aggregadget
                 (:top 0)
                 (:left 0)
		 (:parts
		  `((:background ,opal:rectangle
				 (:filling-style ,opal::white-fill)
				 (:left ,(o-formula (gvl :parent :left)))
				 (:top ,(o-formula (gvl :parent :top)))
				 (:width ,(o-formula (+ (the fn (gvl :parent :label :width))
							(the fn (* 2 *background-border-width*)))))
				 (:height ,(o-formula (+ (the fn (gvl :parent :label :height))
							 (the fn (* 2 *background-border-width*)))))
				 (:box '(0 0 0 0))
				 (:line-style nil))
		    (:label ,axis-text))))

(defun add-key-and-label (plot-agg curve-num line-style label &optional scatter-symbol max-key-width)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (type fixnum curve-num))
  (when (and label (> (length (the SIMPLE-BASE-STRING label)) 0))
    (let* ((win (g-value plot-agg :parent :window))
	   (include-key (> (length (g-value win :label-list)) 1))
	   (label-height (the fn (key-and-label-height win curve-num)))
	   (y (the fn (+ *trace-keys-top* (the fn (* curve-num label-height)))))
	   (y-key (the fn (round (+ y (* 0.5 label-height)))))
	   (text (opal:add-component plot-agg (create-instance nil axis-text (:what-is-it :key-label) (:label-position :right)))))
      (s-value text :string label)
      (s-value text :window (g-value plot-agg :window))
      (s-value text :left (if include-key
			    (+ *trace-keys-right*
			       (round (if max-key-width (/ max-key-width 2) 0))
			       10)
			    *trace-keys-left*))
      (s-value text :top (round (- y-key (/ (g-value text :height) 2))))
      ;; Background
      (opal:add-component plot-agg (create-instance nil opal:rectangle
						    (:what-is-it :key-label-background)
						    (:filling-style opal::white-fill)
						    (:left *trace-keys-left*)
						    (:top y)
						    (:width (the fn (+ (+ *trace-keys-right* 10) (the fn (g-value text :width)))))
						    (:height (g-value text :height))
						    (:box '(0 0 0 0))
						    (:line-style nil)))
      (opal:move-component plot-agg text :where :front)
      (when include-key
	(let ((key-left (+ (round (if max-key-width (/ max-key-width 2) 0)) *trace-keys-left*)))
	  (when (g-value win :scatter)
	    (add-scatter-points-to-plot plot-agg win
					(if (g-value win :connect-data-points)
					  (list key-left y-key *trace-keys-right* y-key)
					  (list *trace-keys-middle* y-key))
					line-style scatter-symbol :data-key
					curve-num))
	  (when (g-value win :connect-data-points)
	    (opal:add-component		; Data Key
	     plot-agg
	     (create-instance nil opal:line
			      (:constant t)
			      (:what-is-it :key-label)
			      (:x1 key-left) (:x2 *trace-keys-right*) 
			      (:y1 y-key) (:y2 y-key)
			      (:line-style line-style)))))))))


(defun non-null-plot-win-labels (win)
  (when (g-value win :label-list)
    (loop for label in (g-value win :label-list)
	  when (typecase label
		 (string (> (length label) 0))
		 (t label))
	  do (return t))))

(defun plot-window-curve-numbers (win number-of-curves)
  (or (g-value win :trace-order)
      (loop for count from 0 to (1- number-of-curves) collect count)))

(defun relevant-labels (win number-of-curves)
  (when (non-null-plot-win-labels win)
    (loop for curve-num in (plot-window-curve-numbers win number-of-curves)
	  collect (nth curve-num (g-value win :label-list)))))

(defun label-traces (plot-agg number-of-curves line-styles &optional labels)
  (let ((win (g-value plot-agg :window)))
    (when (non-null-plot-win-labels win)
      (let ((max-key-width (max-key-width win number-of-curves))
	    (curve-numbers (plot-window-curve-numbers win number-of-curves)))
	(loop for curve-num in curve-numbers
	      for count from 0
	      ;; when (member curve-num (g-value win :trace-order)) collect (nth curve-num (g-value win :label-list)) into labels
	      collect (get-scatter-symbol win count) into scatter-symbols
	      collect (if (g-value win :use-same-line-style)
			(car line-styles)
			(nth (mod count (length line-styles)) line-styles))
	      into plot-line-styles
	      finally
	      (loop for curve-num from 0
		    for label in (or labels (relevant-labels win number-of-curves))
		    for scatter-symbol in scatter-symbols
		    for line-style in plot-line-styles
		    do (add-key-and-label plot-agg curve-num line-style label scatter-symbol max-key-width)))))))

(defun max-key-width (win number-of-curves)
  (if (g-value win :scatter)
    (round (loop for curve-num from 1 to number-of-curves
		 maximize (multiple-value-bind (height width)
			      (scatter-symbol-height-width win (get-scatter-symbol win curve-num) curve-num)
			    width)))
    0))

(defun REFRESH-key-labels-POSITION (win)
  (let ((plot-agg (get-plot-agg win 'data-plot)))
    (loop for comp in (loop for comp in (g-value plot-agg :components)
			    when (eq (g-value comp :what-is-it) :key-label-background)
			    collect comp)
	  do (opal:move-component plot-agg comp :where :front))
    (loop for comp in (loop for comp in (g-value plot-agg :components)
			    when (case (g-value comp :what-is-it)
				   ((:key-label :data-key) t))
			    collect comp)
	  do (opal:move-component plot-agg comp :where :front))))


;; This needs to happen before and after adding the virtual agg.
(defun frob-plot-for-virtual-scatter (win)
  (when (and (g-value win :scatter) (not *raise-output-windows*))
    (s-value win :visible t)
    (s-value win :height (g-value win :height))
    (s-value win :width (g-value win :width))
    (opal::update win t)
    (opal::raise-window win)
    (when *hide-output-windows* (s-value win :visible nil))))


(defun plot-windows-finishing (win &optional (final-action :resurrect))
  (update-baselines win)
  (REFRESH-MARKERs-POSITION win)
  (REFRESH-key-labels-POSITION win)
  (frob-plot-for-virtual-scatter win)
  (case final-action
    (:resurrect (resurrect-opal-win win))
    (:update (resurrect-opal-win win :update t)))
  win)

(defun add-linear-regression (win slope intercept r line-style &key label)
					;  (declare (ignore r))
  (add-comment win (format nil "~ASlp ~,2f~A Itrcpt ~,2f R ~,2f"
			   (if label (format nil "~A: " label) "")
			   slope
			   (if (zerop slope) "" (format nil " 1/Slp ~,2f" (/ 1 slope)))
			   intercept r)
	       :append-to-old-comment t
					; :where :upper-right
	       )
  (when (and (numberp slope) (numberp intercept))
    (let* ((plot-agg (get-plot-agg win 'data-plot))
	   (y-as-function-of-xmin (+ (* slope (g-value win :x-min)) intercept))
	   (y-as-function-of-xmax (+ (* slope (g-value win :x-max)) intercept))
	   (x-as-function-of-ymin (unless (zerop slope) (/ (- (g-value win :y-min) intercept) slope)))
	   (x-as-function-of-ymax (unless (zerop slope) (/ (- (g-value win :y-max) intercept) slope)))
	   (sorted-xys (sort (no-nils (list (list (g-value win :x-min) y-as-function-of-xmin)
					    (list (g-value win :x-max) y-as-function-of-xmax)
					    (when x-as-function-of-ymin
					      (list x-as-function-of-ymin (g-value win :y-min)))
					    (when x-as-function-of-ymax
					      (list x-as-function-of-ymax (g-value win :y-max)))))
			     '> :key 'car))
	   (start-point (if x-as-function-of-ymin
			  (nth 1 sorted-xys)
			  (list (g-value win :x-min) y-as-function-of-xmin)))
	   (end-point (if x-as-function-of-ymin
			(nth 2 sorted-xys)
			(list (g-value win :x-max) y-as-function-of-xmax))))
      (opal:add-component plot-agg (create-instance nil opal:line
						    (:constant t)
						    (:x1 (x-plot-win-float (car start-point) win))
						    (:x2 (x-plot-win-float (car end-point) win))
						    (:y1 (y-plot-win (cadr start-point) win))
						    (:y2 (y-plot-win (cadr end-point) win))
						    (:line-style line-style))))))

(defun add-polyline-to-plot (plot-agg plot-win x-list y-list line-style &key connect-ends)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (type KR::SCHEMA plot-win plot-agg))
  (opal:add-component			;Data Points
   plot-agg		 
   (create-instance nil opal:polyline
		    (:constant t)
		    (:point-list (loop for x in (if connect-ends
						    (concatenate 'list x-list (list (first x-list)))
						    x-list)
						
				       for y in (if connect-ends
						    (concatenate 'list y-list (list (first y-list)))
						    y-list)
				       when (and (numberp x) (numberp y))
				       collect (x-plot-win-float (s-flt x) plot-win)
				       and
				       collect (y-plot-win-float (s-flt y) plot-win)))
		    (:line-style line-style))))

;; PLOT-COORDS-POINTER-RESULT This is used as a FINAL-FUNCTION for the plot window coords pointer, i.e.:

;; (add-window-coords-pointer win #'plot-coords-pointer-result))

;; When middle mouse is released the xy coordinates of the last location of the cross hair is
;; displayed in the units of the window data. If there was a previous point so delineated then the
;; slope between that point and the current one is also displayed, again in the units of the data in
;; the window.

(defun event-plotter-point-to-time (win x-point)
  (float (+ (g-value win :minimum-event-time)
	    (* (- (g-value win :maximum-event-time) (g-value win :minimum-event-time))
	       (/ (- x-point (g-value win :event-plot-left-border))
		  (g-value win :event-plot-width))))))

(defun XY-EVENT-PLOTTER-WIN-TO-POINTS (time dummy-y win)
  (list 
   (round (+ (g-value win :event-plot-left-border)
	     (* (g-value win :event-plot-width)
		(/ (- time (g-value win :minimum-event-time))
		   (- (g-value win :maximum-event-time) (g-value win :minimum-event-time))))))
   0))

;; Have to define this after XY-EVENT-PLOTTER-WIN-TO-POINTS and XY-PLOT-WIN-TO-POINTS.
(defun plot-data-to-points-function (win)
  (or (g-value win :data-to-points-function)
      (case (g-value win :mode)
	(:2plot #'XY-EVENT-PLOTTER-WIN-TO-POINTS)
	(:3dplot nil)
	(t #'XY-PLOT-WIN-TO-POINTS))))

(defun plot-coords-running-function (interactor points)
  (plot-coords-pointer-result-core interactor points nil))

(defun plot-coords-pointer-result (interactor points)
  (plot-coords-pointer-result-core interactor points t))

(defun plot-coords-pointer-result-core (interactor points final)
  (let ((win (g-value interactor :window)))
    (unless final (call-prototype-method interactor points)) ; Keep default running action.
    (case (g-value interactor :window :mode)
      (:2dplot (cond ((g-value win :event-plotter)
		      (let ((time (event-plotter-point-to-time win (nth 2 points))))
			(when (<= (g-value win :minimum-event-time) time (g-value win :maximum-event-time))
			  (update-running-comment win (concatenate 'string
								   (format nil "Event time: ~,1fms" time)
								   (when (g-value win :last-pointer-time)
								     (format nil "~%dt: ~,1fms" (-
												 time (g-value win :last-pointer-time)))))))
			(when final 
			  (s-value win :last-pointer-time time)
			  (add-marker win points :data-x time
				      :data-to-points-function #'XY-EVENT-PLOTTER-WIN-TO-POINTS))
						))))
      (:3dplot
       (let ((x (x-plot-win-inv (nth 2 points) win))
	     (y (y-plot-win-inv (nth 3 points) win))
	     *automatic-run*)

;	 (s-value interactor :running-comment :label :text
;                  (concatenate
;                   'string
;                   (plot-coords-pointer-current-value-string x y win)
;                   (when (g-value win :last-pointer-xy) (plot-coords-pointer-last-pointer-value-string x y win))))
;         (s-value interactor :running-comment :visible t)
	 
	 (when final
	   (s-value win :last-pointer-xy (list x y))
	   (add-marker win points :add-cross t :data-x x :data-y y :data-to-points-function #'XY-PLOT-WIN-TO-POINTS))))
      (t
       (let ((x (x-plot-win-inv (nth 2 points) win))
	     (y (y-plot-win-inv (nth 3 points) win))
	     *automatic-run*)
	 (update-running-comment win (concatenate
				      'string
				      (plot-coords-pointer-current-value-string x y win)
				      (when (g-value win :last-pointer-xy) (plot-coords-pointer-last-pointer-value-string x y win))))
	 (when final
	   (s-value win :last-pointer-xy (list x y))
	   (add-marker win points :data-x x :data-y y :data-to-points-function #'XY-PLOT-WIN-TO-POINTS))))))
  nil)


(defun plot-coords-pointer-last-pointer-value-string (x y win)
  (case (g-value win :plot-type)
    ((or :waterfall :xy)
     (let ((dy (- y (nth 1 (g-value win :last-pointer-xy))))
	   (dx  (- x (nth 0 (g-value win :last-pointer-xy)))))
       (format nil "~%dy/dx: ~a / ~a" (plot-window-y-number-format dy win) (plot-window-x-number-format dx win))))))
	       
(defun plot-coords-pointer-current-value-string (x y win)
  (let ((x-label (get-plot-x-axis-label win))
	(y-label (get-plot-y-axis-label win)))
    (case (g-value win :plot-type)
      (:polar (format nil "Mag: ~a ~a~%Angle: ~a degrees"
		      (my-float-format (cartesian-distance 0.0 0.0 x y) 4)
		      x-label
		      (my-float-format (rad-to-deg (atan y x)) 2)))
      (t (format nil "X: ~a ~a~%Y: ~a ~a"
		 (plot-window-x-number-format x win) x-label
		 (plot-window-y-number-format y win) y-label)))))

(defun mark-plot-coords-pointer-result (interactor points)
  (declare (ignore points))
  (mark-coords-pointer-menu (g-value interactor :window)))

;;; These three functions work on the plot windows produced by PLOT-TIMED-DATA.
(defun reset-plot (win point-list)
  (let* ((left (the fn (first point-list)))
	 (top (the fn (second point-list)))
	 (width (the fn (third point-list)))
	 (height (the fn (fourth point-list)))
	 (x-min (x-plot-win-inv left win))
	 (y-min (y-plot-win-inv (+ top height) win))
	 (x-max (x-plot-win-inv (+ left width) win))
	 (y-max (y-plot-win-inv top win)))
    (setup-plot win :y-min-spec y-min :y-max-spec y-max :x-min-spec x-min :x-max-spec x-max)))

(defun zoom (interactor point-list)
  (let ((win (g-value interactor :window))
	*create-new-plot-windows*
	*preserve-plot-layout*
	*automatic-run*)
    (unless (or (g-value win :data-erased)
		(eq :2dplot (g-value win :mode))
		(eq :scanner (g-value win :mode))
		(g-value win :waterfall))
      (add-temp-comment win "Zooming...")
      (plot-timed-data nil nil nil
		       :win win
		       :preserve-window-attributes t
		       :replot-win-point-list point-list
		       :resurrect nil
		       :upper-right-hand-comment "Zooming..")
      (add-temp-comment win ""))))

(defun find-comment (win))

(defun zoom-to-new-window (interactor point-list)
  (unless (or (g-value interactor :window :data-erased)
	      (eq :2dplot (g-value interactor :window :mode))
	      (eq :scanner (g-value interactor :window :mode))
	      (g-value interactor :window :waterfall))
    (let* ((parent-win (g-value interactor :window))
	   (win (get-child-plot-window parent-win))
	   (left (the fn (first point-list)))
	   (top (the fn (second point-list)))
	   (width (the fn (third point-list)))
	   (height (the fn (fourth point-list)))
	   ;; Need to convert the coordinates from the mouse to data coordinates.
	   (x-min (x-plot-win-inv left parent-win))
	   (y-min (y-plot-win-inv (+ top height) parent-win))
	   (x-max (x-plot-win-inv (+ left width) parent-win))
	   (y-max (y-plot-win-inv top parent-win))
	   *automatic-run* *create-new-plot-windows*)
      (when win
	(add-temp-comment parent-win "Parenting...")
	; (add-comment win (find-comment parent-win))
	(s-value parent-win :child-number (1+ (g-value parent-win :child-number)))
	(s-value win :mode (g-value parent-win :mode))
	(case (g-value interactor :window :mode)
	  (:standard-plot (plot-timed-data
			   nil		; (g-value parent-win :y-lists)
			   nil		; (g-value parent-win :label-list)
			   nil		; (g-value parent-win :x-lists)
			   :win win
			   :preserve-window-attributes t
			   :x-are-fns (g-value parent-win :x-are-fns)
			   :y-are-fns (g-value parent-win :y-are-fns)
			   :draw-grid (g-value parent-win :draw-grid)
			   :label-traces (g-value parent-win :label-traces)
			   :x-min x-min :x-max x-max
			   :y-min y-min :y-max y-max
			   :x-label (g-value parent-win :x-label)
			   :y-label (g-value parent-win :y-label)
			   ))
	  (:histology nil))		; add something to zoom histology
	(add-zoom-marker interactor)
	(add-temp-comment parent-win "")
	(add-temp-comment win "")))))

(defun unzoom (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (let ((win (g-value interactor :window))
	*preserve-plot-layout* *automatic-run* *create-new-plot-windows*)
    (unless (or (g-value win :data-erased)
		(eq :2dplot (g-value win :mode))
		(g-value win :waterfall)
		(not (g-value win :min-max-lists)))
      ;; (remove-all-markers win)
      (add-temp-comment win "UnZooming...")
      (plot-timed-data nil nil nil
		       :win win 
		       :unzoom t
		       :preserve-window-attributes t
		       :resurrect nil
		       :upper-right-hand-comment "UnZooming..")
      (add-temp-comment win ""))))


(defun restore-plot (interactor-or-win &optional final-obj-over)
  (declare (ignore final-obj-over))
  (unless (or (eq :2dplot (g-value interactor-or-win :window :mode))
	      (g-value interactor-or-win :window :data-erased))
    (let ((win (if (eq (car (g-value interactor-or-win :is-a)) ph::plot-window)
		   interactor-or-win
		   (g-value interactor-or-win :window)))
	  *automatic-run* *create-new-plot-windows*)
      (add-temp-comment win "Restoring...")
      (s-value win :has-been-setup nil)
      (plot-timed-data nil nil nil
		       :win win
		       :restore-plot t
		       :preserve-window-attributes t
		       :resurrect nil
		       :upper-right-hand-comment "Restoring..")
      (add-temp-comment win ""))))

(defun edit-individual-scatter-dimensions (&optional plot-win)
  (let ((plot-win (or plot-win (win-menu "Select Plot to Edit Scatter Dimensions" (standard-plot-windows) nil t)))
	temp-line-styles temp-borderps temp-scatter-symbols)
    (loop for v-agg in (retrieve-scatter-v-aggs plot-win)
	  for curve-num from 0
	  do (let* ((dummy5 (SCATTER-PROTOTYPE-TO-SYMBOL (g-value v-agg :item-prototype)))
		    (dummy1 (round (or (car (nth curve-num (g-value plot-win :scatter-width-heights)))
				       (g-value v-agg :symbol-width)
				       (g-value plot-win :x-symbol-width)
				       10)))
		    (dummy2 (round (or (cadr (nth curve-num (g-value plot-win :scatter-width-heights)))
				       (g-value v-agg :symbol-height)
				       (g-value plot-win :y-symbol-width)
				       10)))
		    (line-style (or (get-scatter-line-style plot-win curve-num) width-1-line))
		    (dummy3 (g-value line-style :line-thickness))
		    (dummy4 (get-scatter-symbol-borderp plot-win curve-num))
		    (menu-list `((dummy5 "Scatter symbol:" :choose ,*scatter-symbols*)
				 (dummy1 "Symbol width (ignored for dots):" :integer)
				 (dummy2 "Symbol height:" :integer)
				 (dummy4 "Include border" :boolean)
				 (dummy3 ,(format nil "Border thickness (1 to ~d)" (length varying-widths)) :integer))))
	       (choose-variable-values menu-list :label (format nil "Edit symbol for the ~:R data list" (1+ curve-num)))
	       (s-value v-agg :item-prototype (scatter-symbol-to-prototype dummy5))
	       (s-value v-agg :symbol-width dummy1)
	       (s-value v-agg :symbol-height dummy2)
	       (set-virtual-thing-width-and-height v-agg dummy1 dummy2)
	       (push dummy5 temp-scatter-symbols)
	       (push (read-from-string (opal::name-for-schema (nth (1- (max 1 (min (length varying-widths) dummy3))) varying-widths))) temp-line-styles)
	       (push dummy4 temp-borderps))
	       
	  collect (list (g-value v-agg :symbol-width) (g-value v-agg :symbol-height)) into scatter-width-heights
	  finally
	  (s-value plot-win :scatter-symbol (reverse temp-scatter-symbols))
	  (s-value plot-win :scatter-symbol-borderp (reverse temp-borderps))
	  (s-value plot-win :scatter-width-heights scatter-width-heights)
	  (s-value plot-win :scatter-symbol-line-style (reverse temp-line-styles)))))
	  


(defun plot-line-style-menu (&optional (win (win-menu "Choose plots to assign line styles")))
  (loop for win in (coerce-to-list win) do
	(loop for count from 1
	      for label in (g-value win :label-list)
	      do (s-value win :plot-line-style (line-style-menu :label (format nil "~:R line style for ~s" count (g-value win :title)))))))

(defun 2dplot-menu (win)
  (let* ((dummy1 nil) (dummy2 nil) dummy3 dummy4
	 dummy9
	 dummy12
	 dummy15
	 dummy17
	 dummy22
	 dummy30
	 (menu-list '((dummy22 "Change label font" :boolean))))
    (when (or t				; (g-value win :marked-points)
	      (g-value win :baselines))
      (push '(dummy4 "Edit marked points font" :boolean) menu-list))
    (choose-variable-values menu-list :title (format nil "Plot Parameters for ~A" (g-value win :title)))
    (cond-every
     (dummy4 (Edit-marked-points-font win))
     (dummy22 (s-value win :font
		       (s-value win :plot-axis-font (font-menu (g-value win :plot-axis-font) (g-value win :title)))))))
  (resurrect-opal-win win))

(defun 3dplot-menu (win))

(defun edit-tick-format (win &optional revised-win)
  (unless revised-win (setq revised-win win))
  (let ((dummy1 (or (g-value win :x-tick-decimal) 0))
	(dummy2 (or (g-value win :y-tick-decimal) 0))
	(dummy3 (or (g-value win :x-axis-tick-mark-length) *x-axis-tick-mark-length*))
	(dummy4 (or (g-value win :y-axis-tick-mark-length) *y-axis-tick-mark-length*)))
    (choose-variable-values
     `((:comment ,(format nil "~A~%~A"
			  "If a decimal places spec is 0, and axis values are NOT specified as integers,"
			  "then the decimal places used is automatically determined."))
       (dummy1 "X tick decimal places" :integer)
       (dummy2 "Y tick decimal places" :integer)
       (:comment ,(format nil "If negative, the following will cause axis tick marks~%to point away from tick labels."))
       (dummy3 "X tick mark length [pixels]" :integer)
       (dummy4 "Y tick mark length [pixels]" :integer))
     :title (g-value win :title))
    (s-value revised-win :x-axis-tick-mark-length dummy3) 
    (s-value revised-win :y-axis-tick-mark-length dummy4) 
    (s-value revised-win :x-tick-decimal (unless (zerop dummy1) dummy1))
    (s-value revised-win :y-tick-decimal (unless (zerop dummy2) dummy2))))
    
(defun histogram-menu (win &optional revised-win)
  (unless revised-win (setq revised-win win))
  (let* (dummy1
	 (dummy2 (g-value win :x-max))
	 dummy3 
	 (dummy4 (round (g-value win :y-max)))
	 (dummy5 (g-value win :bin-width))
	 (dummy6 (ceiling (g-value win :y-inc)))
	 (dummy7 (or (g-value win :stipple-percent) 0))
	 (dummy8 (g-value win :x-inc))
	 (dummy9 (g-value win :x-min))
	 (dummy12 (g-value win :title))
	 dummy15 
	 dummy16 
	 (dummy17 (g-value win :width))
	 (dummy19 (g-value win :height))
	 dummy20 dummy22 dummy26 )
    (choose-variable-values
     '((dummy6 "Y axis interval" :integer)
       (dummy4 "Y maximum" :integer)
       (dummy9 "X minimum" :number)
       (dummy2 "X maximum" :number)
       (dummy12 "title" :string)
       (dummy1 "Edit axes" :boolean)
       (dummy17 "Window width" :integer)
       (dummy19 "Window height" :integer)
       (dummy7 "Stipple percent" :integer)
       (dummy15 "Edit space around plot" :boolean)
       (dummy22 "Change label font" :boolean)
       (dummy20 "CANCEL" :boolean))
     :title (g-value win :title))
    (unless dummy20
      (cond-every
       (dummy1 (axes-menu win))
       (dummy15 (plotting-space-menu win revised-win))
       (dummy22
	(s-value win :comment-font (s-value win :font (s-value win :plot-axis-font (font-menu (g-value win :plot-axis-font) (g-value win :title)))))
	;; (s-value win :font (s-value win :plot-axis-font (font-menu (g-value win :plot-axis-font) (g-value win :title))))
	;; (change-comment-font win (g-value win :plot-axis-font))
	))
      (s-value win :title dummy12)
      (s-value win :x-label-horizontal-position dummy28)
      (plot-histogram (list (caar (g-value win :x-lists)) (caar (g-value win :y-lists)))
		      dummy5
		      :win win :title dummy12
		      :stipple-percent dummy7
		      :x-axis-tick-skip (g-value win :x-axis-tick-skip)
		      :x-are-fns (g-value win :x-are-fns)
		      :x-origin dummy9	; (g-value win :x-origin)
					; :bin-width (g-value win :bin-width)
		      :x-label dummy10 :y-label dummy11
		      :width dummy17 :height dummy19
		      :x-max dummy2 :x-min dummy9
		      :y-max dummy4 :y-inc dummy6))))

(defun waterfall-plot-menu (win)
  (let ((dummy2 (g-value win :use-waterfall-y-data-max))
	(dummy3 (or (g-value win :waterfall-y-data-max) 0))
	(dummy5 (or (g-value win :waterfall-trace-label-skip) 0))
	(dummy6 (or (g-value win :waterfall-label-offset) 0.0))
	(dummy14 (g-value win :x-trace-offset))
	(dummy15 (g-value win :y-trace-offset))
	(dummy16 (g-value win :label-waterfall))
	(dummy17 (or (g-value win :waterfall-base-x-offset) 0.0))
	(dummy18 (g-value win :wf-skirt))
	(dummy19 (or (g-value win :waterfall-base-y-offset) 0.0))
	(dummy20 (or (g-value win :gap-between-trace-and-waterfall-label) 20))
	(dummy21 (g-value win :skirt-to-window-border))
	dummy1)
    (choose-variable-values
     `((dummy1 "Adjust simple axes" :boolean)
       (dummy2 "Use WATERFALL-Y-DATA-MAX" :boolean)
       (dummy3 "WATERFALL-Y-DATA-MAX" :float)
       (dummy14 "X trace offset" :number)
       (dummy15 "Y trace offset" :number)
       (dummy17 "X base offset" :float)
       (dummy19 "Y base offset" :float) 
       (dummy18 "Add opaque skirt to traces" :boolean)
       (dummy21 "Extend skirt to window border" :boolean)
       (dummy16 "Label waterfall" :boolean)
       (dummy5 "Waterfall trace label skip (0=> no skip)" :integer)
       (dummy6 ,(format nil "If adding trace labels alongside traces, label offset [~A]" (g-value win :y-label)) :float)
       (dummy20 "Gap between traces and waterfall labels [pixels]" :integer))
     :title (format nil "Waterfall Plot Parameters for ~A" (g-value win :title)))
    (when dummy1 (simple-axes-menu win))
    (s-value win :use-waterfall-y-data-max dummy2)
    (s-value win :waterfall-y-data-max dummy3)
    (s-value win :waterfall-base-x-offset dummy17)
    (s-value win :waterfall-base-y-offset dummy19)
    (s-value win :waterfall-label-offset dummy6)
    (s-value win :x-trace-offset dummy14)
    (s-value win :y-trace-offset dummy15)
    (s-value win :waterfall-trace-label-skip dummy5)
    (s-value win :wf-skirt dummy18)
    (s-value win :skirt-to-window-border dummy21)
    (s-value win :gap-between-trace-and-waterfall-label dummy20)
    (s-value win :label-waterfall dummy16)))

(defun overlay-layout-menu (win)
  (let ((dummy1 (g-value win :preserve-plot-layout))
	(dummy2 (g-value win :overlay))
	(dummy3 (g-value win :accomodate-all-overlays))
	dummy4
	(dummy5 (or (when (numberp (g-value win :plot-point-skip)) (round (g-value win :plot-point-skip)))
		    0)))
    (choose-variable-values
     `((dummy4 "Edit space around plot" :boolean)
       (dummy5 ,(format nil "Plot point skip~%(this can recover dash patterns for close points)") :integer)
       (:comment "The following apply to any subsequent plots to this window:")
       (dummy2  "Overlay data" :boolean)
       (dummy3 "Adjust window dimensions to accomodate all overlays" :boolean)
       (dummy1 "Preserve layout of for new data" :boolean))
     :text (concatenate-string-list
	    (no-nils (list (when *overlay-all-plots* (format nil "Global overlay set=> above flag ignored"))
			   (when *accomodate-all-overlays* (format nil "Global accomodate overlay set=> above flag ignored"))
			   (when *preserve-plot-layout* (format nil "Global preserve layout set=> above flag ignored"))))
	    :string-spacer (format nil "~%"))
     :title (format nil "Overlay and Layout Parameters for ~A" (g-value win :title)))
    (s-value win :plot-point-skip (max 0 dummy5))
    (s-value win :preserve-plot-layout dummy1)
    (s-value win :overlay dummy2)
    (s-value win :accomodate-all-overlays dummy3)
    (when dummy4 (plotting-space-menu win))))

(defun simple-axes-menu (win)
  (let ((dummy1 (g-value win :x-scale-l%))
	(dummy3 (g-value win :y-scale-t%))
	(dummy4 (g-value win :x-scale-t%))
	(dummy5 (g-value win :x-inc))
	(dummy6 (g-value win :y-inc))
	(dummy7 (g-value win :simple-axis-x-value-p))
	(dummy8 (g-value win :simple-axis-y-value-p)))
    (choose-variable-values
     `((dummy1 "Left end position of X scale bar (% window width, from left)" :float)
       (dummy4 "Position of X scale bar (% window height, from top)" :float)
       (dummy3 "Bottom end position of Y scale bar (% window height, from top)" :float)
       (dummy5 ,(format nil "X scale bar length [~A]" (g-value win :x-label)) :number)
       (dummy6 ,(format nil "Y scale bar length [~A]" (g-value win :y-label)) :number)
       (dummy7 "Include bar length in X label" :boolean)
       (dummy8 "Include bar length in Y label" :boolean))
     :title (format nil "Simple Axes Parameters for ~A" (g-value win :title)))
    (s-value win :simple-axis-x-value-p dummy7)
    (s-value win :simple-axis-y-value-p dummy8)
    (s-value win :x-inc dummy5)
    (s-value win :y-inc dummy6)
    (s-value win :x-scale-l% dummy1)
    (s-value win :y-scale-t% dummy3)
    (s-value win :x-scale-t% dummy4)))

(defun data-and-trace-offset-plot-menu (win) 
  (let ((dummy14 (g-value win :x-trace-offset))
	(dummy15 (g-value win :y-trace-offset))
	(dummy21 (g-value win :x-data-offset))
	(dummy22 (g-value win :y-data-offset)))	 
    (choose-variable-values
     '((dummy21 "X data offset (applied before log, if log coordinates)" :float)
       (dummy22 "Y data offset (applied before log, if log coordinates)" :float)
       (dummy14 "X trace offset" :number)
       (dummy15 "Y trace offset" :number))
     :title (format nil "Plot Data and Trace Offsets for ~A" (g-value win :title)))
    (s-value win :x-data-offset dummy21)
    (s-value win :y-data-offset dummy22)
    (s-value win :x-trace-offset dummy14)
    (s-value win :y-trace-offset dummy15)))

(defun edit-labels (win &optional revised-win)
  (clear-up-label-list win (g-value win :label-list))
  (s-value (or revised-win win) :label-list (EDIT-string-LIST
					     (sequence-to-string-list (g-value win :label-list))
					     :label (format nil "Editing labels for ~a" (g-value win :title))
					     :entry-string "trace label")))

(defun scatter-menu (win &optional revised-win)
  (unless revised-win (setq revised-win win))
  (let* ((dummy1 (if (consp (g-value win :scatter-symbol)) :all (g-value win :scatter-symbol)))
	 (dummy2 (g-value win :x-symbol-width))
	 (dummy3 (g-value win :y-symbol-width))
	 (dummy4 (g-value win :scatter-symbol-units-in-pixels))
	 (dummy5 (g-value win :fill-scatter))
	 dummy6 dummy7 dummy8
	 dummy9)
    (unless dummy2 (setq dummy2 (if dummy4 *default-scatter-size* (* 0.02 (g-value win :x-mag)))))
    (unless dummy3 (setq dummy3 (if dummy4 *default-scatter-size* (* 0.02 (g-value win :y-mag)))))
    (choose-variable-values
     `(,(when (consp (g-value win :scatter-symbol))
	  `(dummy8 ,(format nil "Symbols are currently ~A: Clear this and use value below" (g-value win :scatter-symbol)) :boolean))
       ,(unless (< (length *scatter-symbols*) 2)
	  `(dummy1 "Scatter symbol: " :choose ,(cons :all *scatter-symbols*)))
       (dummy5 "Fill scatter symbols" :boolean)
       (dummy9 "Include border in all scatter symbols" :boolean)
       (dummy3 "Symbol height" :number)
       ,(unless (< (length *scatter-symbols*) 2)
	  `(dummy2 "Symbol width (ignored for dots)" :number))
       (dummy4 "Symbol dimensions in pixels (T) or plotted units (F)" :boolean)
       ,(when (g-value win :scatter-width-heights) `(dummy7 "Clear individual scatter dimensions" :boolean))
       (dummy6 "Edit individual scatter dimensions" :boolean))
     :title (format nil "Scatter Plot Parameters for ~A" (g-value win :title)))
    (when dummy9 (s-value win :scatter-symbol-borderp t))
    (when dummy7 (s-value win :scatter-width-heights nil))
    (when dummy4			; pixel widths and heights must be at least 2pixels.
      (setq dummy2 (max 2 dummy2)  dummy3 (max 2 dummy3)))
    (s-value revised-win :fill-scatter dummy5)
    (when (or (not (consp (g-value win :scatter-symbol))) dummy8) (s-value revised-win :scatter-symbol dummy1))
    (s-value revised-win :scatter-symbol-units-in-pixels dummy4)
    (s-value revised-win :x-symbol-width dummy2) (s-value revised-win :y-symbol-width dummy3)
    (when dummy6 (EDIT-INDIVIDUAL-SCATTER-DIMENSIONS revised-win))))

(defun edit-tick-skip (win &optional (revised-win win))
  (let ((dummy3 (or (g-value win :y-axis-tick-skip) 0))
	(dummy4 (or (g-value win :x-axis-tick-skip) 0))
	(dummy17 (or (g-value win :y-axis-tick-mark-skip) 0))
	(dummy18 (or (g-value win :x-axis-tick-mark-skip) 0)))
    (choose-variable-values
     `((dummy3 "Y tick skip" :integer)
       (dummy17 "Y tick mark skip" :integer)
       (dummy4 "X tick skip" :integer)
       (dummy18 "X tick mark skip" :integer))
     :text "For tick and tick mark skips, 0=> no skip" :title (g-value win :title))
    (s-value revised-win :y-axis-tick-mark-skip dummy17)
    (s-value revised-win :x-axis-tick-mark-skip dummy18)
    (s-value revised-win :y-axis-tick-skip dummy3)
    (s-value revised-win :x-axis-tick-skip dummy4)
    nil))

(defun border-extrapolation-menu (win)
  (let ((dummy1 (g-value win :include-border-points))
	(dummy2 (g-value win :apply-horizontal-borders))
	(dummy3 (g-value win :apply-vertical-borders)))
    (choose-variable-values
     `( ; (dummy1 "Extrapolate off plot lines to axes borders" :boolean)
       (dummy2 "Apply horizontal borders" :boolean)
       (dummy3 "Apply vertical borders" :boolean))
     :title (g-value win :title))
    (s-value win :include-border-points dummy1)
    (s-value win :apply-horizontal-borders dummy2)
    (s-value win :apply-vertical-borders dummy3)))

(defun axes-menu (win &optional revised-win)
  (unless revised-win (setq revised-win win))
  (let ((dummy1 (cond ((and (g-value win :x-are-fns) (g-value win :y-are-fns)) :x_&_y)
		      ((g-value win :x-are-fns) :x)
		      ((g-value win :y-are-fns) :y)
		      (t :neither)))
	dummy2
	(dummy7 (or (g-value win :x-axis-root) (g-value win :x-origin)))
	(dummy8 (or (g-value win :y-axis-root) (g-value win :y-origin)))
	(dummy10 (or (g-value win :x-label) "")) (dummy11 (or (g-value win :y-label) ""))
	dummy12
	(dummy13 (g-value win :y-label-vertical-position))
	dummy14
	dummy15
	(dummy16 (g-value win :x-label-vertical-position)) (dummy17 (g-value win :y-label-horizontal-position))
	dummy21
	dummy22
	(dummy28 (g-value win :x-label-horizontal-position))
	(dummy29 (g-value win :axes-type))
	dummy30)	
    (choose-variable-values
     `((dummy29 "Axes type:" :choose (:standard :simple :none) :label-left)
       (dummy21 "Border extrapolation menu" :boolean)
       (dummy22 "Axes label values menu:" :boolean)
       (dummy10 "X axis label" :string)
       (dummy11 "Y axis label" :string)       
       (dummy1 "Print ticks as integers:" :choose (:x_&_y :x :y :neither) :rank-margin 4 :label-left)
       (dummy14 "Edit tick format and lengths" :boolean)
       (dummy2 "Edit tick skips" :boolean)
       (dummy13 "Y axis label vertical position:"
		:choose (:two-thirds-up :upper-right :center-right :lower-right :lower-left :center-left :upper-left) :rank-margin 3) 
       (dummy28 "X label position relative to X axis:" :choose (:left :center :right) :label-left)
       (dummy16 "Tick label position relative to X axis:" :choose (:below :above) :label-left)
       (dummy17 "Tick label position relative to Y axis:" :choose (:left :right) :label-left)
       (dummy12 "Use roots below for tick marks (default is origin)" :boolean)
       (dummy7 "Root for X axis tick marks" :float)
       (dummy8 "Root for Y axis tick marks" :float)
       (dummy15 "Data grid menu" :boolean)
       (dummy30 "Axes visibility menu" :boolean))
     :title (g-value win :title))
    (s-value revised-win :y-label-horizontal-position dummy17)
    (s-value revised-win :x-label-vertical-position dummy16)
    (when dummy22 (axes-tick-values-menu win revised-win))
    (when dummy21 (border-extrapolation-menu revised-win))
    (when dummy14 (edit-tick-format win revised-win))
    (when dummy2 (EDIT-TICK-SKIP win revised-win))
    (when dummy15 (edit-data-grid win revised-win))
    (when dummy30 (axes-visibility-menu win revised-win))
    (case dummy1
      (:x_&_y (s-value revised-win :x-are-fns t) (s-value revised-win :y-are-fns t))
      (:x (s-value revised-win :x-are-fns t) (s-value revised-win :y-are-fns nil))
      (:y (s-value revised-win :y-are-fns t) (s-value revised-win :x-are-fns nil))
      (t (s-value revised-win :y-are-fns nil) (s-value revised-win :x-are-fns nil)))
    (s-value revised-win :axes-type dummy29)
    (s-value revised-win :x-label dummy10) (s-value revised-win :y-label dummy11)
    (s-value revised-win :y-label-vertical-position dummy13)
    (s-value revised-win :x-label-horizontal-position dummy28)
    (if dummy12
	(progn (s-value revised-win :x-axis-root dummy7)
	       (s-value revised-win :y-axis-root dummy8))
	(progn (s-value revised-win :x-axis-root (g-value win :x-origin))
	       (s-value revised-win :y-axis-root (g-value win :y-origin))))))

(defun axes-tick-values-menu (win &optional revised-win)
  (unless revised-win (setq revised-win win))
  (let ((dummy22 (or (g-value win :x-axis-value-prefix) ""))
	(dummy23 (or (g-value win :x-axis-value-suffix) ""))
	(dummy24 (or (g-value win :y-axis-value-prefix) ""))
	(dummy25 (or (g-value win :y-axis-value-suffix) ""))
	(dummy26 (or (g-value win :x-axis-number-coefficient) 1.0))
	(dummy27 (or (g-value win :y-axis-number-coefficient) 1.0)))	
    (choose-variable-values
     `((dummy22 "X axis value prefix" :string)
       (dummy23 "X axis value suffix" :string)
       (dummy26 "X axis number coefficient" :number)
       (dummy24 "Y axis value prefix" :string)
       (dummy25 "Y axis value suffix" :string)
       (dummy27 "Y axis number coefficient" :number))
     :title (format nil "Axes Label Values: ~A" (g-value win :title)))
    (s-value revised-win :x-axis-number-coefficient dummy26)
    (s-value revised-win :y-axis-number-coefficient dummy27)
    (s-value revised-win :x-axis-value-prefix dummy22)
    (s-value revised-win :x-axis-value-suffix dummy23)
    (s-value revised-win :y-axis-value-prefix dummy24)
    (s-value revised-win :y-axis-value-suffix dummy25)))

(defun axes-visibility-menu (win &optional revised-win)
  (unless revised-win (setq revised-win win))
  (let ((dummy10 (list (when (g-value win :consider-y-axis-visible-limit) :limit-y-axis)
		       (when (g-value win :consider-x-axis-visible-limit) :limit-x-axis)))
	(dummy1 (or (g-value win :y-axis-visible-max)  (g-value win :y-axis-max)))
	(dummy2 (or (g-value win :y-axis-visible-min)  (g-value win :y-axis-min)))
	(dummy3 (or (g-value win :x-axis-visible-max)  (g-value win :x-axis-max)))
	(dummy4 (or (g-value win :x-axis-visible-min)  (g-value win :x-axis-min)))
	(dummy5 (cond ((and (g-value win :x-axis-p) (g-value win :y-axis-p)) :x_&_Y)
		      ((g-value win :x-axis-p) :x)
		      ((g-value win :y-axis-p) :y)
		      (t :none))))
    (choose-variable-values
     `((dummy5 "Visible axes:" :choose (:x_&_Y :x :y :none) :rank-margin 4)
       (dummy10 "Limit visibility of X and/or Y axis:" :x-choose  (:limit-x-axis :limit-y-axis))
       (dummy1 "Y axis maximum" :float)
       (dummy2 "Y axis minimum" :float) 
       (dummy3 "X axis maximum" :float)
       (dummy4 "X axis minimum" :float))
     :title (g-value win :title))
    (case dummy5
      (:none (s-value revised-win :x-axis-p nil) (s-value revised-win :y-axis-p nil))
      (:x_&_Y (s-value revised-win :x-axis-p t) (s-value revised-win :y-axis-p t))
      (:x (s-value revised-win :x-axis-p t) (s-value revised-win :y-axis-p nil))
      (:y (s-value revised-win :x-axis-p nil) (s-value revised-win :y-axis-p t)))
    (s-value revised-win :consider-y-axis-visible-limit (true-p (member :limit-y-axis dummy10)))
    (s-value revised-win :consider-x-axis-visible-limit (true-p (member :limit-x-axis dummy10)))
    (s-value revised-win :y-axis-visible-max (s-flt dummy1))
    (s-value revised-win :y-axis-visible-min (s-flt dummy2))
    (s-value revised-win :x-axis-visible-max (s-flt dummy3))
    (s-value revised-win :x-axis-visible-min (s-flt dummy4))))

(defun plotting-space-menu (win &optional revised-win)
  (unless revised-win (setq revised-win win))
  (let ((dummy1 (g-value win :use-fixed-top-gap))
	(dummy2 (or (and (g-value win :use-fixed-top-gap) (numberp (g-value win :fixed-top-gap)) (g-value win :fixed-top-gap))
		    (g-value win :label-height)
		    0))
	(dummy3 (g-value win :use-fixed-bottom-gap))
	(dummy4 (or (and (g-value win :use-fixed-bottom-gap) (numberp (g-value win :fixed-bottom-gap)) (g-value win :fixed-bottom-gap))
		    (g-value win :y-plot-bottom-gap)
		    0))
	(dummy5 (g-value win :use-fixed-right-gap))
	(dummy6 (or (and (g-value win :use-fixed-right-gap) (numberp (g-value win :fixed-right-gap)) (g-value win :fixed-right-gap))
		    (g-value win :x-plot-right-gap)
		    0))
	(dummy7 (g-value win :use-fixed-left-gap))
	(dummy8 (or (and (g-value win :use-fixed-left-gap) (numberp (g-value win :fixed-left-gap)) (g-value win :fixed-left-gap))
		    (g-value win :x-plot-left-gap)
		    0))
	dummy9
	(dummy14 (g-value win :x-plot-right-gap-extra))
	(dummy16 (or (g-value win :y-plot-top-gap-extra) 0))
	(dummy18 (or (g-value win :x-plot-left-gap-extra) 0)))
	
    (choose-variable-values
     '((dummy1 "Use fixed top gap" :boolean)
       (dummy2 "Fixed top gap [pixels]" :integer)
       (dummy3 "Use fixed bottom gap" :boolean)
       (dummy4 "Fixed bottom gap [pixels]" :integer)
       (dummy5 "Use fixed right gap" :boolean)
       (dummy6 "Fixed right gap [pixels]" :integer)
       (dummy7 "Use fixed left gap" :boolean)
       (dummy8 "Fixed left gap [pixels]" :integer)
       (dummy9 "Assign via menu the same fixed gaps to other windows" :boolean) 
       ("Extra space parameters ignored if associated fixed gap is used" :comment)
       (dummy14 "Extra space on right side [pixels]" :integer)
       (dummy18 "Extra space on left side [pixels]" :integer)
       (dummy16 "Extra space between top of window and traces [pixels]" :integer))
     :title (format nil "Space Around Plot for ~A" (g-value revised-win :title)))
    
    (loop for win in (cons revised-win (when dummy9 (win-menu
						     (format nil "Choose Plot Windows to Set~%Same Fixed Gaps as ~A"
							     (g-value revised-win :title))
						     (loop for win in (windows-of-mode (g-value revised-win :mode))
							   unless (eq win revised-win) collect win))))
	  when win do 
	  (UPDATE-FIXED-GAP-PARAMETERS win dummy1 dummy2 dummy3 dummy4 dummy5 dummy6 dummy7 dummy8)
	  (s-value win :x-plot-right-gap-extra dummy14)
	  (s-value win :x-plot-left-gap-extra dummy18)
	  (s-value win :y-plot-top-gap-extra dummy16)
	  unless (eq win revised-win) do (plot-timed-data nil nil nil :win win :revise-plot t))))

(defun standard-plot-menu (win)
  (let* ((dummy1 (g-value win :x-min)) (dummy2 (g-value win :x-max))
	 (dummy3 (g-value win :y-min)) (dummy4 (g-value win :y-max))
	 (dummy5 (g-value win :x-inc)) (dummy6 (g-value win :y-inc))
	 (dummy7 (g-value win :x-origin)) (dummy8 (g-value win :y-origin))
	 (dummy9 :none)
	 dummy10 (dummy11 (g-value win :connect-data-points)) (dummy12 (g-value win :scatter))
	 dummy13
	 (dummy14 (cond ((and dummy11 dummy12) :Connect_&_show)
			(dummy11 :Connect)
			(dummy12 :Show)))
	 (dummy15 :none) dummy16 dummy17 (dummy18 :none)
	 (revised-win win)
	 menu-list *preserve-plot-layout* *create-new-plot-windows*
	 (only-win-of-mode-p (= (length (windows-of-mode (g-value win :mode))) 1))
	 (only-one-win-of-mode-p-or-is-scanner (or (eq (g-value win :mode) :scanner) only-win-of-mode-p)))
    (setq menu-list
	  `((:comment "For X or Y plot layout precedence is Match, Automatic, Explicit")
	    ,(unless only-one-win-of-mode-p-or-is-scanner
	       `(dummy9 "Match layout to another plot:" :choose (:X_&_Y :X :Y :none) :label-left :rank-margin 4))
	    ,(unless (eq (g-value win :mode) :scanner) '(dummy15 "Automatic layout:" :choose (:x_&_y :x :y :none) :label-left :rank-margin 4))
	    ,(unless (eq (g-value win :mode) :scanner) `(dummy1 "X minimum" :number))
	    ,(unless (eq (g-value win :mode) :scanner) `(dummy2 "X maximum" :number))
	    ,(unless (eq (g-value win :mode) :scanner) `(dummy3 "Y minimum" :number))
	    ,(unless (eq (g-value win :mode) :scanner) `(dummy4 "Y maximum" :number))
	    (dummy5 "X axis interval" :number) (dummy6 "Y axis interval" :number)
	    (dummy7 "X origin (Y intercept on X axis)" :number)
	    (dummy8 "Y origin (X intercept on Y axis)" :number)
	    ,(unless only-one-win-of-mode-p-or-is-scanner
	       `(dummy18 "Match window dimensions to another plot:" :choose (:Width_&_Height :Width :Height :none) :rank-margin 4))
	    (dummy14 "Plot technique (vis-a-vis points):" :choose (:Connect :Show :Connect_&_show) :horizontal)
	    ,(unless (eq (g-value win :mode) :scanner) `(dummy10 "Create new window for revisions" :boolean))
	    ,(unless only-win-of-mode-p `(dummy16 "Match other plots to this one (coordinates, grid, axis type)" :boolean))
	    (dummy13 "More edit and analysis options" :boolean)
	    (dummy17 "CANCEL" :boolean)))
    (choose-variable-values menu-list :title "Edit Plot" :text (format nil "Editing plot window ~A" (g-value win :title)))
    (unless dummy17
      (when dummy10 (setq revised-win (GET-CHILD-PLOT-WINDOW win)))
      (when revised-win
	(case dummy14
	  (:Connect_&_show (s-value revised-win :connect-data-points t) (s-value revised-win :scatter t))
	  (:Connect (s-value revised-win :connect-data-points t) (s-value revised-win :scatter nil))
	  (:Show (s-value revised-win :connect-data-points nil) (s-value revised-win :scatter t)))
	(case dummy15
	  (:x_&_y (s-value revised-win :auto-x-scaling t) (s-value revised-win :auto-y-scaling t))       
	  (:x (s-value revised-win :auto-x-scaling t) (s-value revised-win :auto-y-scaling nil))
	  (:y (s-value revised-win :auto-y-scaling t) (s-value revised-win :auto-x-scaling nil)))
	(unless (eq dummy18 :none) (match-win-dimensions-menu (or revised-win win) dummy18))
	(unless (eq dummy9 :none) (plot-match-win-scale-menu win revised-win dummy9))
	(s-value revised-win :x-min dummy1) (s-value revised-win :x-max dummy2)
	(s-value revised-win :y-min dummy3) (s-value revised-win :y-max dummy4)
	(s-value revised-win :x-inc dummy5) (s-value revised-win :y-inc dummy6)
	(s-value revised-win :x-origin dummy7) (s-value revised-win :y-origin dummy8)
	(when dummy13 (misc-plot-parameters-menu win revised-win))		    
	(when (eq (g-value revised-win :waterfall) t) (waterfall-plot-menu revised-win))
	(let ((label-traces (g-value revised-win :label-traces))
	      (scatter (g-value revised-win :scatter)) (SCATTER-SYMBOL (g-value revised-win :SCATTER-SYMBOL))
	      (fill-scatter (g-value revised-win :fill-scatter)) (scatter-symbol-borderp (g-value revised-win :scatter-symbol-borderp))
	      (x-symbol-width (g-value revised-win :x-symbol-width)) (y-symbol-width (g-value revised-win :y-symbol-width))
	      (connect-data-points (g-value revised-win :connect-data-points)) (draw-grid (g-value win :draw-grid))
	      (simple-axis-x-value-p (g-value revised-win :simple-axis-x-value-p))
	      (simple-axis-y-value-p (g-value revised-win :simple-axis-y-value-p))
	      (use-same-line-style (g-value revised-win :use-same-line-style))
	      (x-label-vertical-position (g-value revised-win :x-label-vertical-position))
	      (y-label-horizontal-position (g-value revised-win :y-label-horizontal-position))
	      (x-min (g-value revised-win :x-min)) (x-max (g-value revised-win :x-max))
	      (y-min (g-value revised-win :y-min)) (y-max (g-value revised-win :y-max))
	      (x-inc (g-value revised-win :x-inc)) (y-inc (g-value revised-win :y-inc))
	      (x-origin (g-value revised-win :x-origin)) (y-origin (g-value revised-win :y-origin))
	      (x-label (g-value win :x-label)) (y-label (g-value win :y-label))
	      (x-axis-p (g-value revised-win :x-axis-p)) (y-axis-p (g-value revised-win :y-axis-p))
	      (x-are-fns (g-value revised-win :x-are-fns)) (y-are-fns (g-value revised-win :y-are-fns)))
	  (if (g-value win :linear-regression)
	    (plot-xy-data (loop for x-list in (car (g-value win :x-lists))
				for y-list in (car (g-value win :y-lists))
				collect (list x-list y-list))
			  (g-value win :label-list)
			  :win revised-win
			  ;; :delta-t-start (g-value revised-win :delta-t-start)
			  :label-traces label-traces :connect-data-points connect-data-points 
			  :scatter scatter :SCATTER-SYMBOL SCATTER-SYMBOL :fill-scatter fill-scatter :scatter-symbol-borderp scatter-symbol-borderp
			  :x-symbol-width x-symbol-width :y-symbol-width y-symbol-width 
			  :linear-regression t :draw-grid draw-grid
			  :simple-axis-x-value-p simple-axis-x-value-p :simple-axis-y-value-p simple-axis-y-value-p 
			  :use-same-line-style use-same-line-style 
			  :x-label-vertical-position x-label-vertical-position :y-label-horizontal-position y-label-horizontal-position 
			  :x-min x-min :x-max x-max :y-min y-min :y-max y-max :x-inc x-inc :y-inc y-inc 
			  :x-origin x-origin :y-origin y-origin :x-label x-label :y-label y-label :x-are-fns x-are-fns :y-are-fns y-are-fns)
	    (plot-timed-data (when dummy10 (g-value win :y-lists)) (when dummy10 (g-value win :label-list)) nil
			     :win revised-win :delta-t-start (g-value revised-win :delta-t-start) :x-lists (when dummy10 (g-value win :x-lists))
			     :use-bins (g-value revised-win :use-bins) :bin-width (g-value revised-win :bin-width)
			     :stipple-percent (g-value win :stipple-percent) 
			     :label-traces label-traces 
			     :revise-plot (not dummy10)
			     :simple-axis-x-value-p simple-axis-x-value-p :simple-axis-y-value-p simple-axis-y-value-p
			     :consider-y-axis-visible-limit (g-value revised-win :consider-y-axis-visible-limit)
			     :y-axis-visible-max (g-value revised-win :y-axis-visible-max)
			     :y-axis-visible-min (g-value revised-win :y-axis-visible-min)
			     :consider-x-axis-visible-limit (g-value revised-win :consider-x-axis-visible-limit)
			     :x-axis-visible-max (g-value revised-win :x-axis-visible-max)
			     :x-axis-visible-min (g-value revised-win :x-axis-visible-min)
			     :x-log (g-value revised-win :x-log) :y-log (g-value revised-win :y-log) :log-base (g-value revised-win :log-base)
			     :x-data-offset (g-value revised-win :x-data-offset) :y-data-offset (g-value revised-win :y-data-offset)
			     :connect-data-points connect-data-points
			     :scatter scatter :SCATTER-SYMBOL SCATTER-SYMBOL :fill-scatter fill-scatter
			     :scatter-symbol-borderp scatter-symbol-borderp 
			     :x-symbol-width x-symbol-width :y-symbol-width y-symbol-width 
			     :draw-grid draw-grid 
			     :x-label-vertical-position x-label-vertical-position :y-label-horizontal-position y-label-horizontal-position
			     :x-min x-min :x-max x-max :y-min y-min :y-max y-max :x-inc x-inc :y-inc y-inc
			     :x-origin x-origin :y-origin y-origin :x-label x-label :y-label y-label :x-are-fns x-are-fns :y-are-fns y-are-fns
			     :waterfall (g-value revised-win :waterfall) :wf-skirt (g-value revised-win :wf-skirt)
			     :x-axis-p x-axis-p :y-axis-p y-axis-p
			     :resurrect (not (eq revised-win win))
			     :use-same-line-style use-same-line-style
			     :x-trace-offset (g-value revised-win :x-trace-offset) :y-trace-offset (g-value revised-win :y-trace-offset)
			     :waterfall-trace-label-skip (g-value revised-win :waterfall-trace-label-skip)
			     :waterfall-label-offset (g-value revised-win :waterfall-label-offset)
			     :label-waterfall (g-value revised-win :label-waterfall)))
	  (when dummy16 (MATCH-PLOTS-MENU revised-win))
	  (refresh-axes revised-win)))
      revised-win)))


;; REFRESH-ALL-PLOTS The optional GRID can be :DRAW, :ERASE or nil (don't change).
(defun refresh-all-plots (&optional grid)
  (loop for win in (standard-plot-windows) do (refresh-plot win grid)))

(defun refresh-plot (win &optional grid)
  (s-value win :draw-grid (case grid
			    (:draw t)
			    (:erase nil)
			    (t (g-value win :draw-grid))))
  (refresh-axes win)
  (let ((*automatic-run* t))
    (standard-plot-menu win)
;    (plot-timed-data nil nil nil :preserve-window-attributes t :win win :draw-grid (g-value win :draw-grid) :revise-plot t
;		     :restore-plot t :resurrect t)
    ))


(defun match-plots-menu (&optional match-win)
  (let* ((match-win (or match-win
			(choose-list-values-from-keys
			 (loop for plot-win in (standard-plot-windows)
			       collect (list (g-value plot-win :title) plot-win))
			 nil :punt-if-only-one-entry nil :only-one-choice t :label "Choose plot window for setting coordinates")))
	 (wins-to-be-revised (choose-list-values-from-keys
			      (loop for plot-win in (loop for output-win in *output-windows*
							  when (and (not (eq match-win output-win))
								    (eq (g-value output-win :mode) :standard-plot))
							  collect output-win)
				    collect (list (g-value plot-win :title) plot-win))
			      nil :punt-if-only-one-entry nil :only-one-choice nil
			      :label (format nil "Choose plot windows for setting~%coordinates to ~a" (g-value match-win :title)))))
    (loop for revise-win in wins-to-be-revised do
	  (s-value revise-win :scatter-symbol (g-value match-win :scatter-symbol))
	  (s-value revise-win :x-symbol-width (g-value match-win :x-symbol-width))
	  (s-value revise-win :y-symbol-width (g-value match-win :y-symbol-width))
	  (s-value revise-win :scatter-symbol-units-in-pixels (g-value match-win :scatter-symbol-units-in-pixels))
	  (s-value revise-win :fill-scatter (g-value match-win :fill-scatter))
	  (plot-timed-data nil nil nil :win revise-win :revise-plot t
			   :x-label (g-value revise-win :x-label)
			   :x-are-fns (g-value match-win :x-are-fns) :y-are-fns (g-value match-win :y-are-fns)
			   :draw-grid (g-value match-win :draw-grid)
			   :x-min (g-value match-win :x-min) :x-max (g-value match-win :x-max) 
			   :x-inc (g-value match-win :x-inc)
			   :x-origin (g-value match-win :x-origin)
			   :y-label (g-value revise-win :y-label)
			   :label-traces (g-value revise-win :label-traces)
			   :y-min (g-value match-win :y-min) :y-max (g-value match-win :y-max)
			   :y-inc (g-value match-win :y-inc)
			   :y-origin (g-value match-win :y-origin)))))

(defun plot-match-win-scale-menu (win revised-win scaled-axes)
  (let ((match-win (choose-list-values-from-keys
		    (loop for plot-win in (loop for output-win in *output-windows*
						when (and (not (or (eq revised-win output-win) (eq win output-win)))
							  (eq (g-value output-win :mode) :standard-plot))
						collect output-win)
			  collect (list (g-value plot-win :title) plot-win))
		    nil
		    :punt-if-only-one-entry nil :only-one-choice t
		    :text (format nil "Choose plot window for setting ~a axis(es) scale~%of window ~a"
				  (NICE-STRING-FROM-KEYWORD scaled-axes) (g-value win :title))
		    :label "Matching Plot Scales")))
    (when match-win
      (case scaled-axes
	(:X_&_Y (setq dummy1 (g-value match-win :x-min) dummy2 (g-value match-win :x-max)
		      dummy3 (g-value match-win :y-min) dummy4 (g-value match-win :y-max)
		      dummy5 (g-value match-win :x-inc) dummy6 (g-value match-win :y-inc)
		      dummy7 (g-value match-win :x-origin) dummy8 (g-value match-win :y-origin)))
	(:X (setq dummy1 (g-value match-win :x-min) dummy2 (g-value match-win :x-max)
		  dummy5 (g-value match-win :x-inc) dummy7 (g-value match-win :x-origin))) 
	(:Y (setq dummy3 (g-value match-win :y-min) dummy4 (g-value match-win :y-max)
		  dummy8 (g-value match-win :y-origin) dummy6 (g-value match-win :y-inc)))))))

(defun plot-window-default-font-menu (&optional reference-window)
  (let (dummy1 dummy2 (dummy3 :no_change) dummy4 dummy5)
    (choose-variable-values
     `((dummy1 ,(format nil "Set default plot axis font from menu~%(currently ~s ~s ~s)"
			(g-value *plot-axis-font* :family)
			(g-value *plot-axis-font* :face)
			(g-value *plot-axis-font* :size))
	       :boolean)
       (dummy2 "Update axis font of current output windows to default" :boolean)
       (dummy3 "Set default comment font:" :choose (:from_menu :default_plot_axis_font :no_change) :vertical)
       (dummy5 "Update comment font of current output windows to default" :boolean))
     :label "General Window Fonts")
    (when dummy1 (setq *plot-axis-font* (font-menu (or (and reference-window (g-value reference-window :plot-axis-font))
						       *plot-axis-font*)
						   "Choose default plot axis font")))
    (when dummy2 (loop for win in *output-windows* do (s-value win :plot-axis-font *plot-axis-font*)))
    
    (case dummy3
      (:default_plot_axis_font (setq *comment-font* *plot-axis-font*))
      (:from_menu (setq *comment-font* (font-menu (or (and reference-window (g-value reference-window :comment-font)) *comment-font*)
						  "Choose default comment font"))))
    (when dummy5 (loop for win in *output-windows* do (s-value win :comment-font *comment-font*)))
    (when (or dummy2 dummy5)
      (loop for win in (ALL-PLOT-WINDOWS) unless (eq win reference-window) do (refresh-plot win)))))

;; Backward comp
(defun default-window-font-menu (&optional reference-window)
  (plot-window-default-font-menu reference-window))

(defun misc-plot-parameters-menu (win revised-win)
  (let* ((dummy1 nil) (dummy2 nil) dummy3 dummy4
	 (dummy5 (cond ((eq :auto (g-value win :waterfall)) :waterfall_[auto_scale])
		       ((g-value win :waterfall) :waterfall)
		       (t :regular)))
	 dummy6 dummy7 dummy8 dummy9 dummy15 dummy16 dummy17
	 (dummy19 (g-value win :use-same-line-style))
	 dummy20
	 (dummy21 (g-value win :label-traces))
	 dummy22
	 (dummy24 (g-value win :plot-line-style))
	 dummy25 dummy26 dummy30
	 (menu-list
	  `((dummy9 "Ticks, Grid and Axes menu" :boolean)
	    (dummy15 "Reorder/suppress traces" :boolean)
	    ,(when (> (length (g-value win :label-list)) 5) '(dummy16 "Invert order of traces" :boolean))
	    (dummy21 "Include trace labels" :boolean)
	    (dummy26 "Edit scatter" :boolean)
	    (dummy2 "Edit labels" :boolean)
	    (dummy22 "Edit fonts:" :choose (:all_windows :this_window) :label-left)
	    (dummy24 "Plot line style:" :choose (:Thick-Colors :Thin-Colors :Thick-Dashed-lines :Dashed-lines :double-VARYING-WIDTHS :VARYING-WIDTHS)
		     :rank-margin 2)
	    (dummy19 ,(format nil "Use same line style for all traces~%(always true for waterfalls)") :boolean)
	    ,(unless (eq (g-value win :mode) :scanner)
	       `(dummy5 "Plot layout:" :choose (:regular :waterfall ; :waterfall_[auto_scale]
						)
		 :label-left :horizontal))
	    (dummy20 ,(format nil "Log plot menu~A" (or (log-parameters-string win) "")) :boolean)
	    (dummy6 "Overlay and layout specification and flag menu" :boolean)
	    (dummy1 "Data and trace offset menu" :boolean)
	    (dummy7 "Write traces to file" :boolean)
	    (dummy8 "Trace analysis" :boolean))))
    (when nil				; (or t ; (g-value win :marked-points) (g-value win :baselines))
      (push '(dummy4 "Edit marked points font" :boolean) menu-list))
    (choose-variable-values menu-list :title "Edit Plot" :text (g-value revised-win :title))
    (when dummy7 (grab-and-store-plot-data
		  :filename (STRIP-DISPLAYED-HOST-NAME-FROM-TITLE (g-value win :title))
		  :win win :force-menu t))
    (when dummy8 (ADD-TRACE-ANALYSIS-TO-PLOT-MENU win))
    (s-value revised-win :waterfall (case dummy5
				      (:waterfall t)
				      (:waterfall_[auto_scale] :auto)
				      (:regular nil)))
    (when dummy6 (overlay-layout-menu win))
    (s-value revised-win :use-same-line-style dummy19)
    (s-value revised-win :label-traces dummy21)
    (s-value revised-win :plot-line-style dummy24)
    (case dummy22
      (:this_window (s-value revised-win :comment-font
			     (s-value revised-win :font
				      (s-value revised-win :plot-axis-font (font-menu (g-value win :plot-axis-font) (g-value revised-win :title))))))
      (:all_windows (plot-window-default-font-menu revised-win)))
    (cond-every
     (dummy25 (edit-tick-format win revised-win))
     (dummy16 (reorder-plot-trace-menu win t))
     (dummy15 (reorder-plot-trace-menu revised-win))
     (dummy2 (edit-labels revised-win))
     (dummy9 (axes-menu win revised-win))
     (dummy4 (Edit-marked-points-font revised-win))
     (dummy1 (data-and-trace-offset-plot-menu revised-win))
     (dummy26 (scatter-menu win revised-win))
     ((and (eq (g-value revised-win :axes-type) :simple) (not (g-value revised-win :waterfall)))
      (SIMPLE-AXES-MENU revised-win))
     (dummy17 (plot-scale-to-trace-menu revised-win))		    
     (dummy20 (plot-log-parameters-menu win revised-win)))))

(defun plot-scale-to-trace-menu (revised-win)
  (let* ((label-list (g-value revised-win :label-list))
	 (dummy1 (car label-list)))
    (choose-variable-values `((dummy1 "Choose trace:" :choose ,(loop for label in label-list collecting label)))
			    :title (format nil "Choose trace for scaling ~A" (g-value revised-win :title)))
    (let ((scaling-data (loop for label in label-list
			      for data in (car (g-value revised-win :y-lists))
			      when (string= label dummy1) do (return data))))
      (s-value revised-win :y-max (a-bit-more (list scaling-data) 0.05))
      (s-value revised-win :y-min (a-bit-less (list scaling-data) 0.05)))))

(defun log-parameters-string (win)
  (when (or (g-value win :x-log) (g-value win :y-log))
    (concatenate-strings
     (format nil " (")
     (when (g-value win :x-log) (format nil "Log X"))
     (when (and (g-value win :x-log) (g-value win :y-log)) (format nil " "))
     (when (g-value win :y-log) (format nil "Log Y"))
     (format nil ", Base: ~A)" (or (g-value win :log-base) "Natural base")))))
  
(defun plot-log-parameters-menu (win revised-win)
  (let* ((dummy29 (g-value win :x-log)) (dummy30 (g-value win :y-log))
	 (dummy1 (g-value win :log-base)) (dummy2 (not dummy1)) menu-list)
    (choose-variable-values
     '((dummy29 "Log plot for X" :boolean)
       (dummy30 "Log plot for Y" :boolean)
       (dummy2 "Use natural base" :boolean))
     :title (format nil "Log Plot Parameters for ~A" (g-value revised-win :title)))
    (if dummy2
      (s-value revised-win :log-base nil)
      (when (or dummy29 dummy30)
	(unless (numberp dummy1) (setq dummy1 10.0))
	(push '(dummy1 "Log base (bad values default to 10)" :float) menu-list)
	(choose-variable-values menu-list :title (format nil "Log Plot Parameters for ~A" (g-value revised-win :title)))
	(when (or (<= dummy1 0.0) (< (- 1.0 0.01) dummy1 (+ 1.0 0.01)))
	  (setq dummy1 10.0))
	(s-value revised-win :log-base dummy1)))
    (s-value revised-win :x-log dummy29)
    (s-value revised-win :y-log dummy30)))

(defun reorder-plot-trace-menu (win &optional just-invert)
  (let* ((label-list (g-value win :label-list))
	 (all-values (sequence-to-string-list (list-of-nums (length label-list) 1 1)))
	 (trace-order (or (g-value win :trace-order) (list-of-nums (length label-list))))
	 (included-list (loop for order in trace-order collect (nth (round order) label-list)))
	 (excluded-list (loop for label in label-list unless (member label included-list :test 'equal) collect label))
	 (menu-list (loop for i from 1
			  for key in (concatenate 'list included-list excluded-list)
			  collect (list key (if (> i (length included-list)) "" (princ-to-string i)))))
	 (new-trace-order (if just-invert
			    (reverse trace-order)
			    (let* ((menu-trace-order-list
				    (XCLUSIVE-CHOOSE-BUTTON menu-list all-values t
							    :rank-margin 6 :label (format nil "Trace Order for ~a" (g-value win :title))
							    :text "Unassigned traces will not be displayed"))
				   (sorted-menu-trace-order-list (sort (loop for string-val in menu-trace-order-list
									     when (and (cadr string-val) (> (length (cadr string-val)) 0))
									     collect (list (car string-val) (read-from-string (cadr string-val))))
								       '< :key 'cadr)))
			      (loop for label-val in sorted-menu-trace-order-list
				    collect (search (list (car label-val)) label-list :test 'equal))))))
    (s-value win :trace-order new-trace-order)))

;; These keywords should correspond to useful slots of plot windows, that will be saved by WRITE-WINDOW-PLOT-FORM. Eventually these should be used as
;; key args to plot-timed-data and friends.
(defparameter GENERIC-PLOT-WINDOW-SLOTS
  '(
    :x-tick-decimal
    :y-tick-decimal
    :x-axis-tick-mark-length
    :y-axis-tick-mark-length
    :scatter-symbol-line-style
    :use-waterfall-y-data-max
    :waterfall-y-data-max
    :waterfall-base-x-offset :waterfall-base-y-offset
    :gap-between-trace-and-waterfall-label :skirt-to-window-border))


(defparameter generic-plot-keywords
  '(:TITLE
    :left :top :WIDTH :HEIGHT 
    :delta-t :delta-t-start :timed-data
    ; :scale
    :data-type :canonic-label
    :ACCOMODATE-all-overlays
    :X-LABEL :Y-LABEL
    :X-LABEL-VERTICAL-POSITION :x-label-horizontal-position
    :invert-y-axis-label :y-label-vertical-position :y-label-horizontal-position

    :X-ARE-FNS :Y-ARE-FNS :x-axis-number-coefficient :y-axis-number-coefficient
    :x-axis-value-prefix :x-axis-value-suffix :y-axis-value-prefix :y-axis-value-suffix
    
    :X-MIN :X-MAX :X-INC :X-ORIGIN :Y-MIN :Y-MAX :Y-INC :Y-ORIGIN :Y-LOG :X-LOG :LOG-BASE :x-axis-root :y-axis-root
    
    :AXES-TYPE
    :X-AXIS-P :Y-AXIS-P
    :X-SCALE-L% :X-SCALE-T% :Y-SCALE-T%
    :SIMPLE-AXIS-X-VALUE-P :SIMPLE-AXIS-Y-VALUE-P
    :consider-y-axis-visible-limit :y-axis-visible-max :y-axis-visible-min
    :consider-x-axis-visible-limit :x-axis-visible-max :x-axis-visible-min

    :linear-regression

    :x-origin-tick :y-origin-tick :reference-ticks-to-origin :include-x-tick-at-0 :include-y-tick-at-0
    
    :USE-BINS :BIN-WIDTH :STIPPLE-PERCENT

    :USE-SAME-LINE-STYLE
    :upper-right-hand-comment :comment :comment-position

    :USE-FIXED-TOP-GAP :FIXED-TOP-GAP :USE-FIXED-BOTTOM-GAP :FIXED-BOTTOM-GAP :USE-FIXED-RIGHT-GAP :FIXED-RIGHT-GAP
    :USE-FIXED-LEFT-GAP :FIXED-LEFT-GAP
    
    :DRAW-GRID ; :grid-line-style 
    :LABEL-TRACES
    :X-DATA-OFFSET :Y-DATA-OFFSET :X-TRACE-OFFSET :Y-TRACE-OFFSET
    :fix-to-unity-mag-if-so :session-name

    :CONNECT-DATA-POINTS :SCATTER :SCATTER-SYMBOL
    :SCATTER-WIDTH-HEIGHTS :scatter-symbol-borderp :fill-scatter 
    :x-symbol-width :y-symbol-width
    
    :polar :POLAR-CIRCLES-P
    
    :LABEL-WATERFALL :WATERFALL :WF-SKIRT :WATERFALL-TRACE-LABEL-SKIP :WATERFALL-LABEL-OFFSET 
    ))

(defun fixup-formatted-object (obj)
  (typecase obj
    (cons (format nil "`~s" obj))
    (t obj)))


(defun print-window-key-and-value (win key &optional (stream t))
  (let ((obj (g-value win key)))
    (typecase obj
      (cons (format stream "~% ~s `~s" key obj))
      (t (format stream "~% ~s ~s" key obj)))))

(defun print-s-value-form-window-key-and-value (win key &optional (stream t))
  (let ((obj (g-value win key)))
    (typecase obj
      (cons (format stream "  (s-value win ~s `~s)~%" key obj))
      (t    (format stream "  (s-value win ~s ~s)~%" key obj)))))

#|
(defun write-window-plot-form (win &optional (stream *standard-output*))
  (let ((*standard-output* stream))
    (format t ";;; -*- Package: ~a; Mode: LISP -*-~%~%~%~%~%" *default-lisp-data-package-name*)
    (format t "(let ((win ~%  ")
    (format t "(plot-timed-data~%  ")
    (formatted-list-dump (g-value win :y-lists))    (format t "~%  ")
    (formatted-list-dump (g-value win :label-list))
    (format t "~% NIL ~%  :x-lists~%  ")
    (formatted-list-dump (g-value win :x-lists))    (format t "~%")
    (format t "	:update-fixed-gap-parameters t ~%")
    (loop for key in GENERIC-PLOT-KEYWORDS do (print-window-key-and-value win key))
    (format t " )))~%~% ")

    (loop for key in GENERIC-PLOT-WINDOW-SLOTS do (print-s-value-form-window-key-and-value win key))
	      
    (WRITE-WINDOW-DRESSING win)
    (format t " (let ((*automatic-run* t)) (standard-plot-menu win))~%")
    (format t " nil)~% ")))
|#


(defvar *write-window-plot-form-w-explicit-x-and-y-lists* t)

(defun write-window-plot-form (win &optional (stream *standard-output*))
  (let ((*standard-output* stream))
    (format t ";;; -*- Package: ~a; Mode: LISP -*-~%~%~%~%~%" *default-lisp-data-package-name*)
    (format t "(let ((win ~%  ")
    (format t "(plot-timed-data~%  ")
    (if *write-window-plot-form-w-explicit-x-and-y-lists*
      (format t "  nil    ; Refer to :y-lists arg below")
      (formatted-list-dump (g-value win :y-lists)))
    (format t "~%  ")
    
    (formatted-list-dump (g-value win :label-list))
    (format t "~% NIL ~%")

    (when *write-window-plot-form-w-explicit-x-and-y-lists*
      (format t "  :y-lists~%  ")
      (formatted-list-dump (g-value win :y-lists))    (format t "~%"))
    
    (format t "  :x-lists~%  ")
    (formatted-list-dump (g-value win :x-lists))
    (format t "~%")
    
    (format t "	:update-fixed-gap-parameters t ~%")
    (loop for key in GENERIC-PLOT-KEYWORDS do (print-window-key-and-value win key))
    (format t " )))~%~% ")
    (WRITE-WINDOW-DRESSING win)
    (format t " (refresh-plot win)~%")
    (format t " nil)~% ")))

(defun write-window-dressing (win)
  (loop for key in GENERIC-PLOT-WINDOW-SLOTS do (print-s-value-form-window-key-and-value win key))
  (loop for font-slot in '(:window-font :comment-font :plot-axis-font :marked-points-font) do (format-window-font-slot-description win font-slot))
  (write-window-markers win)
  (when (g-value win :grid-line-style)
    (format t "  (s-value win :grid-line-style ~A)~%" (read-from-string (opal::name-for-schema (g-value win :grid-line-style)))))
  (loop for baseline in (g-value win :baselines) do
	(format t "  (mark-baseline win ~S ~S ~S ~S :line-style ~a)~%"
		(g-value baseline :x-start) (g-value baseline :x-stop) (g-value baseline :y-start) (g-value baseline :y-stop)
		(OUTPUT-PARSE-LINE-STYLE (g-value baseline :line :line-style) nil))))

(defun format-window-font-slot-description (win slot)
  (when (g-value win slot)
    (format t "  (s-value win ~s (opal:get-standard-font ~S ~S ~S))~%"
	    slot
	    (g-value win slot :family)
	    (g-value win slot :face)
	    (g-value win slot :size))))

(defun write-window-markers (win)
  (when (g-value win :markers) (format t ";; Markers ~%"))
  (loop for marker in (g-value win :markers) do
	(let ((marker-label (marker-label marker)))
	  (format t "  (add-marker win `~S :add-point ~S :add-cross ~S ~%"
		  (marker-points marker) (true-p (marker-point marker)) (true-p (marker-cross marker)))
	  (when (true-p (marker-point marker))
	    (format t "   :point-type ~a :point-width ~a :point-height ~a~%"
		    (marker-point-type marker)
		    (marker-point-width marker)
		    (marker-point-height marker)))
	  (format t "    :data-x ~S :data-y ~S :label ~S :label-position ~S)~%"
		  (marker-data-x marker) (marker-data-y marker)
		  (when marker-label  (g-value marker-label :label :text))
		  (when marker-label (g-value marker-label :label-position))))))

(defun output-parse-color (color &optional stream)
  (when color
    (format stream
	    " (create-instance nil opal::color (:red ~a) (:blue ~a) (:green ~a))"
	    (g-value color :red)
	    (g-value color :blue)
	    (g-value color :green))))


(defun output-parse-line-style (line-style &optional stream)
  (when line-style
    (format stream "~a"
	    (if (member line-style *all-line-styles*)
		(read-from-string (opal::name-for-schema line-style))
		(concatenate 'string
			     "(create-instance nil opal:line-style "
			     (format nil "(:join-style ~s) (:line-thickness ~A) "
				     (g-value line-style :join-style)
				     (g-value line-style :line-thickness))
			     (format nil "(:shading ~S) (:line-style ~S)"
				     (g-value line-style :shading)
				     (g-value line-style :line-style))
			     (format nil " (:dash-pattern ~S) (:color ~a) (:foreground-color ~a))"
				     (fixup-formatted-object (g-value line-style :dash-pattern))
				     (g-value line-style :color)
				     (output-parse-color (g-value line-style :foreground-color))))))))

		      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		      
;; Plot interactors
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun refresh-plot-inter-function (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (window-interactor-wrapper
   interactor
   (unless (g-value window :data-erased)
     (case (g-value window :mode)
					;       (:scanner (scanned-image-menu window))
					;       (:histogram (histogram-menu window))
					;       (:2dplot (2dplot-menu window))
					;       (:3dplot (3dplot-menu window))
       (:standard-plot (refresh-plot window))))))


(defun plot-window-menu-inter-function (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (window-interactor-wrapper
   interactor
   (unless (g-value window :data-erased)
     (case (g-value window :mode)
       (:scanner (scanned-image-menu window))
       (:histogram (histogram-menu window))
       (:2dplot (2dplot-menu window))
       (:3dplot (3dplot-menu window))
       (:standard-plot (standard-plot-menu window))))))


(create-instance 'menu-for-grid-window-Interactor inter:button-Interactor
		 (:continuous nil) (:start-where t)
		 (:start-event :control-\G)
		 (:final-function #'(lambda (interactor final-obj-over)
				      (declare (ignore final-obj-over))
				      (window-interactor-wrapper interactor
								 (progn (edit-data-grid window)
									(refresh-axes window)
									(opal:update window))))))

(create-instance 'mark-baseline-interactor inter:button-Interactor
		 (:continuous nil) (:start-where t)
		 (:start-event :control-\b)
		 (:final-function #'(lambda (interactor final-obj-over)
				      (declare (ignore final-obj-over))
				      (window-interactor-wrapper interactor (mark-baseline-menu window)))))

(create-instance 'edit-baselines-interactor inter:button-Interactor
		 (:continuous nil) (:start-where t)
		 (:start-event :control-B)
		 (:final-function #'(lambda (interactor final-obj-over)
				      (declare (ignore final-obj-over))
				      (window-interactor-wrapper interactor (edit-baselines window)))))

(defun erase-plot-data-inter-function (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (window-interactor-wrapper interactor (erase-plot-data window)))

(create-instance 'erase-plot-data-Interactor inter:button-Interactor
		 (:continuous nil) (:start-where t)
		 (:start-event :control-\E)
		 (:final-function #'erase-plot-data-inter-function))

(defun restore-just-recent-traces (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (if (and nil (not (eq :2dplot (g-value interactor :window :mode)))
	   (> (length (g-value interactor :window :y-lists)) 1))
      (unless (g-value interactor :window :data-erased)
	(let ((win (g-value interactor :window)) *automatic-run* *create-new-plot-windows*)
	  ; (remove-all-markers win)
	  (add-temp-comment win "Restoring...")
	  (s-value win :y-lists (list (car (g-value win :y-lists))))
	  (s-value win :x-lists (list (car (g-value win :x-lists))))
	  (plot-timed-data
	   nil nil nil
	   :win (g-value interactor :window)
	   :draw-grid (g-value (g-value interactor :window) :draw-grid)
	   :overlay t
	   :upper-right-hand-comment "Restoring..")
	  (add-temp-comment win "")))))


(create-instance 'restore-just-recent-traces-window-Interactor inter:button-Interactor
	 (:continuous nil)
	 (:start-where t)
	 (:start-event :control-R)
	 (:final-function #'restore-just-recent-traces))


(defun grab-and-store-plot-data (&key filename win force-menu (output-format :lisp) suppress-comments)
  "Writes plot data from WIN into FILENAME. Prompts for non-specfied args. OUTPUT-FORMAT specifies
how the data are arranged in the output file, as follows:

    :OUTPUT-FORMAT key    File format
 -------------------------------------------
      :LISP [default]   A list of lists - ((x1 x2 ... xn)(y1 y2 ... yn))
      :COLUMNS          Two columns -
                           x1   y1   
                           x2   y2   
                              .
                              .
                              .
                           xn   yn
For :LISP format, if more than one trace from the plot is selected, the additional trace data are
appended to the output file. For :COLUMNS format, multiple traces are written to separate files,
where each filename is appended with the trace label. Comments about the plot window and the trace
labels are written to the output file, preceded by a ;, unless SUPPRESS-COMMENTS is T [default NIL]." 
  (let ((win (or win (win-menu "Select Plot Window for Data" (standard-plot-windows) nil t))))
    (when win
      
      (let* ((number-of-overlays (number-of-overlays win))
	     (overlay-index (if (= number-of-overlays 1)
			      0
			      (let ((dummy1 0))
				(choose-variable-values
				 '((dummy1 "Extract overlay index:" :integer))
				 :text (format nil "Choose overlay index from 0 to ~d" (1- number-of-overlays))
				 :title (format nil "Choose overlay for ~a" (g-value win :title)))
				dummy1)))
				
	     (filename (make-nice-filename (or filename (strip-displayed-host-name-from-title (g-value win :title)))))
	     (directory-namestring-no-colons (directory-namestring-no-colons filename))
	     (dummy1 (format nil "~A~A"
			     (if (> (length directory-namestring-no-colons) 0)
			       directory-namestring-no-colons
			       *data-directory*)
			     (remove-dirs-from-path filename)))
	     dummy2 (dummy3 (or output-format :lisp))
	     (dummy4 suppress-comments))
	(multiple-value-bind (data labels)
	    (extract-plot-window-data win :menu overlay-index)
	  (when (and data labels)
	    (when (or force-menu (not (full-pathname-p dummy1)))
	      (choose-variable-values
	       `((dummy1
		  ,(format nil "Edit filename (for COLUMNS format, this is base name.~%For LISP format, add .lisp extension if desired)") :string)
		 (dummy3 "Output format" :choose (:lisp :columns))
		 (dummy4 "Suppress file comments" :boolean)
		 (dummy2 "CANCEL" :boolean))
	       :title (format nil "Write Data From ~A" (g-value win :title))))
	    (unless dummy2
	      (let* ((output-format dummy3)
		     (filenames
 		      (case output-format
			(:lisp (list dummy1))
			(:columns (loop for label in labels collect
					(format nil "~A_~A" dummy1 (if (> (length label) 0) (make-nice-filename label) "data"))))))
		     (successful-write
		      (loop for filename in filenames
			    unless
			    (case output-format
			      (:lisp (dump-result-to-lisp-file
				      (unless dummy4 (format nil "~%;; Data from plot window ~A" (g-value win :title)))
				      :filename filename)) 
			      (:columns (write-lists-multi-column
					 nil
					 :comment (unless dummy4 (format nil "~%;; Data from plot window ~A" (g-value win :title)))
					 :filename filename))) 
			    do (return nil) finally (return t))))
		(when successful-write
		  (case output-format
		    (:lisp (loop for xy in data
				 for label in labels do
				 (unless dummy4 
				   (dump-result-to-lisp-file (format nil "~%;; ~A trace ((x x x ...)(y y y ....))" label) :filename (car filenames)))
				 (dump-result-to-lisp-file xy :filename (car filenames))))
		    (:columns (loop for xy in data
				    for label in labels
				    for filename in filenames do
				    (write-lists-multi-column
				     xy :comment (unless dummy4 (format nil "~%;; ~A trace, x y format" label)) :filename filename)))) 
		  (format t ";; Data from ~A written to~% ~{;;      ~a~%~}"
			  (g-value win :title) filenames))))))))))

(defun grab-and-store-plot-data-interactor-function (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (unless (or (not (eq :standard-plot (g-value interactor :window :mode)))
	      (g-value interactor :window :data-erased))
    (let ((win (g-value interactor :window)) *automatic-run*)
      (grab-and-store-plot-data :win win :force-menu t))))

(create-instance 'grab-and-store-plot-data-Interactor inter:button-Interactor
		 (:continuous nil) (:start-where t)
		 (:start-event :control-\T)
		 (:final-function #'grab-and-store-plot-data-interactor-function))

(defun save-graphics-to-lisp-file (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (unless (or (not (eq :standard-plot (g-value interactor :window :mode)))
	      (g-value interactor :window :data-erased))
    (let ((win (g-value interactor :window)) *automatic-run*)
      (dump-plot-to-lisp-file win))))

(create-instance 'save-graphics-to-lisp-file-Interactor inter:button-Interactor
		 (:continuous nil) (:start-where t)
		 (:start-event :control-\s)
		 (:final-function #'save-graphics-to-lisp-file))

(create-instance 'raise-all-menus-interactor inter:button-Interactor
		 (:continuous nil) (:start-where t)
		 (:start-event :control-\q)
		 (:final-function #'(lambda (interactor final-obj-over)
				      (declare (ignore final-obj-over interactor))
				      (loop for win in (garnet-debug:windows)
					    when (and (g-value win :icon-title)
						      (or (equal *menu-win* (car (g-value win :is-a) ))
							  (equal *ss-win-w-bars* (car (g-value win :is-a)))))
					    do (resurrect-opal-win win :raise t :visible t :deiconify t :update t)))))

(create-instance 'toggle-grid-window-Interactor inter:button-Interactor
		 (:continuous nil) (:start-where t)
		 (:start-event :control-\g)
		 (:final-function #'(lambda (interactor final-obj-over)
				      (declare (ignore final-obj-over))
				      (let ((win (g-value interactor :window)))
					(s-value win :draw-grid (not (g-value win :draw-grid)))
					(refresh-axes win)
					(opal:update win)))))


(defun edit-data-grid (win &optional revised-win)
  (let ((dummy1 (g-value win :draw-grid))
	dummy2
	(target-win (or revised-win win)))
    (choose-variable-values
     '((dummy1 "Draw grid" :boolean)
       (dummy2 "Edit grid line style:" :choose (:dashed-and-solid-greys :solid-colors :dashed-colors :dashed-and-solid-blacks :from_components)))
     :title (format nil "Edit Data Grid For ~A" (g-value target-win :title)))
    (s-value target-win :draw-grid dummy1)
    (s-value target-win :grid-line-style
	     (or (and dummy2
		      (line-style-menu :default-style (or (g-value win :grid-line-style) *default-grid-line-style*)
				       :text (format nil "Line Style For Data Grid in ~A" (g-value target-win :title))
				       :choose-from-components (true-p (member dummy2 '(:from_components)))
				       :choose-from-classes (true-p (member dummy2 '(:dashed-and-solid-greys
										     :solid-colors
										     :dashed-colors
										     :dashed-and-solid-blacks)))
				       :style-options (case dummy2
							(:from_components nil)
							(:dashed-and-solid-blacks 'dashed-lines)
							(:dashed-and-solid-greys 'grey-dashed-lines)
							(:solid-colors 'thin-colors)
							(:dashed-colors 'thin-dashed-1-colors))))
		 (g-value target-win :grid-line-style)))))


;;; This adds a bunch of interactors for windows used for data plotting.
(defun add-plotting-interactors (win)
  (case (g-value win :mode)
    (:standard-plot
     (add-window-zoom win #'zoom :control-leftdown)
     (add-window-zoom win #'zoom-to-new-window :shift-control-leftdown)
     (add-window-zoom win #'unzoom :rightdown nil)
     (create-instance nil save-graphics-to-lisp-file-Interactor (:Window win))
     (create-instance nil grab-and-store-plot-data-Interactor (:Window win))
     (create-instance nil restore-just-recent-traces-window-Interactor (:Window win))
     (add-window-zoom win #'restore-plot :control-rightdown nil)))
  (case (g-value win :mode)
    ((or :standard-plot :scanner :histogram)
     (create-instance nil toggle-grid-window-Interactor (:Window win))
     (create-instance nil menu-for-grid-window-Interactor (:Window win))))
  (case (g-value win :mode)
    (:scanner (add-scanner-interactors win)))
  (create-instance nil erase-plot-data-Interactor (:window win))
  (create-instance nil raise-all-menus-interactor (:window win))
  (create-instance nil mark-baseline-interactor (:window win))
  (create-instance nil edit-baselines-interactor (:window win))
  (create-instance nil window-menu-Interactor (:Window win) (:final-function #'plot-window-menu-inter-function))
  (create-instance nil refresh-window-Interactor (:Window win) (:final-function #'refresh-plot-inter-function))
  (add-window-coords-pointer win
			     #'plot-coords-pointer-result
			     #'mark-plot-coords-pointer-result
			     #'plot-coords-running-function))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clear-plot-windows ()
  (clear-windows-of-mode :plot))


;;; This should work on multipoint objects as well.
(defun transform-polyline (polyline xfrm-point-list zoom-p)
  ;; When zoom-p then xfrm-point-list is new coords, else xfrm-point-list is last coords.
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (type (unsigned-byte 29)))
  (let* ((x-p t)
	 (new-left (the fn (first xfrm-point-list)))
	 (new-top (the fn (second xfrm-point-list)))
	 (new-width (the fn (third xfrm-point-list)))
	 (new-height (the fn (fourth xfrm-point-list)))
	 (x-coeff (float (/ (the fn (g-value polyline :parent :window :width)) new-width)))
	 (y-coeff (float (/ (the fn (g-value polyline :parent :window :height)) new-height))))
    (s-value polyline :point-list
	     (if zoom-p
		 (loop for point in (g-value polyline :point-list) 
		       if x-p collect (truncate (the sf (* (- (the fn point) new-left) x-coeff)))
		       else collect (truncate (the sf (* (- (the fn point) new-top) y-coeff)))
		       do (setq x-p (not x-p)))
		 (loop for point in (g-value polyline :point-list) 
		       if x-p  collect (truncate (the sf (+  new-left (* (the fn point) x-coeff))))
		       else collect (truncate (the sf (+ new-top (* (the fn point) y-coeff))))
		       do (setq x-p (not x-p)))))))


;; POINTS are in pixel units - (x y x y ...) - not rounded!
(defun scatter-symbol-height-width (plot-win scatter-symbol curve-num)
  (let* ((scatter-symbol (or scatter-symbol (g-value plot-win :scatter-symbol)))
	 (scatter-width-heights (g-value plot-win :scatter-width-heights))
	 (y-symbol-width (or (cadr (nth curve-num scatter-width-heights))
			     (g-value plot-win :y-symbol-width)
			     *default-scatter-size*
			     (* 0.02 (the sf (g-value plot-win :y-mag)))))
	 (x-symbol-width (case scatter-symbol
			   (:dot y-symbol-width)
			   (t (or (car (nth curve-num scatter-width-heights))
				  (g-value plot-win :x-symbol-width)
				  *default-scatter-size*
				  (* 0.02 (the sf (g-value plot-win :x-mag)))))))
	 ;; Following params are in pixels
	 (width (if (g-value plot-win :scatter-symbol-units-in-pixels)
		    x-symbol-width
		    (x-plot-win-distance x-symbol-width plot-win)))
	 (height (if (g-value plot-win :scatter-symbol-units-in-pixels)
		     y-symbol-width
		     (y-plot-win-distance y-symbol-width plot-win))))
    (values height width)))

;; CURVE-NUM ranges from 0 to N-1 for N curves
(defun get-scatter-line-style (plot-win curve-num)
  (let ((temp-value (typecase (g-value plot-win :scatter-symbol-line-style)
		      (cons (nth curve-num (g-value plot-win :scatter-symbol-line-style)))
		      (t (or (g-value plot-win :scatter-symbol-line-style) thin-black-line)))))
    (typecase temp-value
      (string (symbol-value (read-from-string temp-value)))
      (symbol (symbol-value temp-value))
      (t temp-value))))

(defun get-scatter-symbol-borderp (plot-win curve-num)
  (typecase (g-value plot-win :scatter-symbol-borderp)
    (cons (nth curve-num (g-value plot-win :scatter-symbol-borderp)))
    (t (g-value plot-win :scatter-symbol-borderp))))


(defun add-scatter-points-to-plot (plot-agg plot-win points line-style &optional scatter-symbol what-is-it curve-num)
  ;; LINE-STYLE applies to the line (whether shown or not) that connects the data points, not the line style for the scatter symbol.
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (type KR::SCHEMA plot-win plot-agg)
	   (cons points))
  (let* ((color-fill (when (or (nth curve-num (g-value plot-win :scatter-symbol-fill))
			       (g-value plot-win :fill-scatter))
		       (color-to-fill (g-value line-style :foreground-color))))
	 (scatter-symbol (or scatter-symbol (g-value plot-win :scatter-symbol)))
	 (scatter-line-style (when (or (member scatter-symbol *SCATTER-SYMBOLS-OPEN-CURVES*) (get-scatter-symbol-borderp plot-win curve-num))
			       (get-scatter-line-style plot-win curve-num))))
    (multiple-value-bind (height width)
	(scatter-symbol-height-width plot-win scatter-symbol curve-num)
      (let ((floor-half-width (the fn (floor (/ width 2)))) ; This and the following params are in pixels
	    (ceiling-half-width (the fn (ceiling (/ width 2))))
	    (floor-half-height (the fn (floor (/ height 2))))
	    (ceiling-half-height (the fn (ceiling (/ height 2))))
	    (x-off-plot-min-pixels (x-plot-win-float (g-value plot-win :x-min) plot-win))
	    (x-off-plot-max-pixels (x-plot-win-float (g-value plot-win :x-max) plot-win))
	    (y-off-plot-min-pixels (y-plot-win-float (g-value plot-win :y-max) plot-win))
	    (y-off-plot-max-pixels (y-plot-win-float (g-value plot-win :y-min) plot-win))
	    output-list)
	(do* ((points points (cddr points))
	      (x-win (car points) (car points))
	      (y-win (cadr points) (cadr points)))
	    ((null points))
	  (declare (fixnum floor-half-height floor-half-width ceiling-half-height ceiling-half-width x-win y-win))
	  (push				; (x-center y-center x-left x-right y-top y-bottom linestyle color-fill)
	   (list x-win y-win
		 (the fn (- x-win ceiling-half-width))	(the fn (+ x-win  floor-half-width))
		 (the fn (- y-win ceiling-half-height))	(the fn (+ y-win floor-half-height))
		 scatter-line-style color-fill)
	   output-list))
	(FROB-PLOT-FOR-VIRTUAL-SCATTER plot-win)
	(let ((v-agg (make-virtual-scatter-aggregate plot-win (sequence-to-gen-array output-list) scatter-symbol what-is-it)))
	  (g-value v-agg :width) (g-value v-agg :height) ; To make the width/height calculation simpler - not clear that this helps
	  (opal:add-component plot-agg v-agg :where :front))))))

(defun make-virtual-scatter-aggregate (plot-win item-array scatter-symbol what-is-it)
  (create-instance nil opal:virtual-aggregate
		   (:what-is-it what-is-it)
		   ; (:borderp (g-value plot-win :scatter-symbol-borderp))
		   (:item-array item-array) (:point-in-item nil)
		   (:item-prototype (scatter-symbol-to-prototype scatter-symbol))))





;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(proclaim '(type fixnum *x-plot-win-minimum-margin* *x-plot-win-maximum-margin*
	    *x-axis-tick-mark-length*
	    *y-axis-tick-mark-length*
	    *y-plot-win-minimum-margin*
	    *y-plot-win-maximum-margin*))

(export '(get-plot-window find-plot-window
			  plot-line-style-menu
			  generic-plot-keywords
			  GENERIC-PLOT-WINDOW-SLOTS

	;;  *default-plot-line-style*
	  *label-plot-traces*
	  *x-axis-tick-mark-length*
	  *y-axis-tick-mark-length*
	  mark-all-plot-wins-at-time
	  plot-window-top-x-lists plot-window-top-y-lists
	  
	  ALL-PLOT-WINDOWS standard-plot-menu *GRID-STYLE*
	  2d-array-max 2d-array-min

	  clear-plot-windows
	  plot 2dplot
	  
	  *default-scatter-size*
	  *default-x-label* *default-y-label*
	  *create-new-plot-windows* *create-new-plot-window-types*
	  *connect-data-points *plot-line-style* *plot-axis-font*
	  *accomodate-all-overlays* *overlay-all-plots* *preserve-plot-layout*
	  3dplot-menu 

	  output-parse-line-style
	  output-parse-color
	  raise-all-menus-interactor 
	  mark-baseline remove-baselines
	  MARK-BASELINES
	  MARK-PLOT-DIAGONAL
	  MARK-PLOT-EVEN-QUADRANT-DIAGONAL
	  MARK-PLOT-odd-QUADRANT-DIAGONAL
	  frame-plot
	  mark-plot-origin-axises

	  DEFAULT-WINDOW-FONT-MENU
	  plot-window-default-font-menu

	  *x-trace-offset* *x-plot-right-gap* *X-PLOT-LEFT-GAP-WATERFALL*
	  *auto-waterfall-y-trace-overlap* *default-y-plot-top-gap-extra-waterfall*
	  *simple-axis-x* *simple-axis-y*
	  *WATERFALL-FIXED-Y-MIN* *WATERFALL-FIXED-Y-Max*
	  restore-plot *USE-MY-FLOAT-FORMAT-FOR-PLOTS*
	  
	  remove-virtual-aggs

	  *x-plot-win-minimum-margin* *x-plot-win-maximum-margin*
	  *y-plot-win-minimum-margin* *y-plot-win-maximum-margin*
	  REFRESH-PLOT REFRESH-ALL-PLOTS
	  
	  UPDATE-FIXED-GAP-PARAMETERS
	  plot-window-font
	  window-plot-axis-font
	  window-comment-font
	  DENSITY-PLOT-SCALE RETRIEVE-SCATTER-V-AGGS SET-VIRTUAL-THING-WIDTH-AND-HEIGHT EDIT-INDIVIDUAL-SCATTER-DIMENSIONS
	  ))
	  

