;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: #-PARALLEL SURF #+PARALLEL *SURF; Base: 10; -*-
;;;  (c) Copyright 1990, Lyle Borg-Graham, MIT Center for Biological Information Processing

;;; The graphics file for drawing cells. Requires Symbolics Window environment.

;;; 7/11/92 LBG Now runs under Garnet.
#+(and garnet (not parallel))
(in-package "SURF-HIPPO"
	    :use '("COMMON-LISP-USER" "COMMON-LISP" "GARNET-GADGETS"  "LISP" "KR")
	    :nicknames '("SURF"))


#+(not (or garnet parallel))
(in-package 'surf)

#+parallel
(in-package '*surf)


#+garnet
(defparameter histology-window
  (create-instance 'histology-window
		   opal:window
		   ;;These values are in anatomical coordinates
		   ;;(microns). The origin is put at the center of the
		   ;;window.
		   (:y-shift 0.0)	;Shift graphic output wrt origin (microns)
		   (:x-shift 0.0)
		   (:theta 0.0)		;Rotate graphic output wrt origin (radians).
		   (:cos-theta 1.0)		
		   (:sin-theta 0.0)		
		   (:scale 1.0)		;microns per pixel
		   (:current-xfrm (fill-3-by-3-identity (make-array '(3 3))))
		   (:dummy-xfrm1 (make-array '(3 3)))
		   (:dummy-xfrm2 (make-array '(3 3)))
		   (:title
		    (concatenate 'string
				 "Surf-Hippo Histology - " (string *circuit-name*)))
		   (:icon-title
		    (concatenate 'string
				 "Surf-Hippo Histology - " (string *circuit-name*)))))

#+garnet
(defparameter *histology-agg* nil)

#+garnet
(defparameter *histology-graphics-pane* 
  (create-instance nil histology-window))

#+garnet
(setq *STANDARD-GRAPHICS-OUTPUT* *histology-graphics-pane* )

#+garnet
(s-value *histology-graphics-pane* :aggregate (create-instance nil opal:aggregate))


#+garnet
(defparameter *histology-note-pane t)



(defvar *light-graphics-object)
(defvar *stimuli-graphics-object)
(defvar *plotted-nodes-object)
(defvar *update-plotted-nodes nil)
(defvar *update-stimuli nil)

(defvar *pixels-per-micron 2)
(defvar *scale 3.0)
(defvar *label-synapses nil)
(defvar *draw-synapse-stimulus t)
(defvar *translation-x 500.0)
(defvar *translation-y 00.0)
(defvar *label-nodes nil)
(defvar *label-sources nil)
(defvar *label-plotted-nodes t)
(defvar *current-graphics-pane "Main" )
(defvar *update-light-stimulus nil)
(defvar *viewing-phi 0.0)
(defvar *viewing-theta (* (/ 3.0 2.0) pi-single))	;These values map x'=x and y'=y.


(defun test-light ()
;;  (menu-for-synapse-parameters)
  (draw-cells t))
(proclaim '(single-float *viewing-theta *viewing-phi))
(proclaim '(function  GET-VIEW-PLANE-X (cons) single-float))
;;; GET-VIEW-PLANE-X
(defun get-view-plane-x (location-in-3-space)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (+ (* -1.0 (the single-float (first location-in-3-space)) ; X
	(sin *viewing-theta))
     (* (the single-float (second location-in-3-space)) ; Y
	(cos *viewing-theta))))

(proclaim '(function GET-VIEW-PLANE-y (cons) single-float))
;;; GET-VIEW-PLANE-Y
(defun get-view-plane-y (location-in-3-space)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (+ (* -1.0 (the single-float (first location-in-3-space)) ; X
	(cos *viewing-theta) (cos *viewing-phi))
     (* -1.0 (the single-float (second location-in-3-space)) ; Y
	(sin *viewing-theta) (cos *viewing-phi))
     (* (the single-float (third location-in-3-space)) ; Z
	(sin *viewing-phi))))

;;; WRITE-HISTOLOGY-NOTES
(defun write-histology-notes ()
#+symbolics  (send *histology-note-pane  :clear-window)
  (maphash 'write-histology-notes-for-type cell-type-hash-table))

;;; WRITE-HISTOLOGY-NOTES-FOR-TYPE
(defun write-histology-notes-for-type (name type)
  (declare (ignore name))
  (format *histology-note-pane (cell-type-notes type)))

;;; DRAWING-MENU
#+symbolics
(defun drawing-menu ()
  (setq *viewing-phi (* *viewing-phi (/ 1.0 pi-single) 0.5 360)
	*viewing-theta (* *viewing-theta (/ 1.0 pi-single) 0.5 360))
  (tv:choose-variable-values
    '((*draw-cells "Draw cells?" :boolean)
      (*update-light-stimulus "Update light stimulus only?" :boolean) 
      (*label-nodes "Label nodes on cells?" :boolean)
      (*label-synapses "Label synapses on cells?" :boolean)
      (*label-sources "Label sources on cells?" :boolean)
      (*label-plotted-nodes "Label plotted nodes on cells?" :boolean)
      (*draw-synapse-stimulus "Draw synapse stimulus?" :boolean)
      (*translation-x "Translation in X direction:" :number)
      (*translation-y "Translation in Y direction:" :number)
      (*viewing-phi "Viewing angle phi (degrees; 0 is flat mount; 90 is radial)" :number)
      (*viewing-theta "Viewing angle theta (degrees; 270 is flat mount and radial)" :number)
;      (*moving-bar-snapshot-times "Moving Bar snapshot times." :list)
      (*scale "Scale:" :number)
      (*current-graphics-pane "Draw cells on Main window or Histology window?"
			      :choose ("Main" "Histology")))
    ':label "SETTING UP CELL DRAWING")
  (setq *viewing-phi (* *viewing-phi 2.0 pi-single (/ 1.0 360))
	*viewing-theta (* *viewing-theta 2.0 pi-single (/ 1.0 360)))
  )	;convert to radians.

;;; DRAW-CELLS
(defun draw-cells (&optional drawing-menu (redraw-stimulus t))
  #+symbolics (if drawing-menu (drawing-menu))
  #+symbolics
  (let ((*standard-output*))
    (if (equal *current-graphics-pane "Main")
	(setq *standard-output* graphics-pane)
	(progn (write-histology-notes) (setq *standard-output* *histology-graphics-pane)))
    (if (not *update-light-stimulus) (send *standard-output*  :clear-window))
    (graphics:with-room-for-graphics (t 200 :fresh-line nil :move-cursor nil)
      (graphics:with-graphics-translation (t  *translation-x *translation-y)
	(graphics:with-graphics-scale (t  *scale)
	  (if *update-light-stimulus (update-light-stimulus-graphics redraw-stimulus)
	      (progn
		(draw-scale-bar)
		(if *draw-cells (maphash 'draw-cell cell-hash-table))
		(if (and *include-light-synapses *draw-synapse-stimulus) (draw-synapse-stimulus))
		))))))
  #+garnet
  ;;  (write-histology-notes) (if (not *update-light-stimulus)
  ;;  (clear-histology-window)) *translation-x *translation-y *scale
  (declare (ignore drawing-menu redraw-stimulus))
  (if *update-light-stimulus
      (update-light-stimulus-graphics t)
      (let ((win *histology-graphics-pane*))
	(if (g-value win :aggregate) (opal:destroy (g-value win :aggregate)))
	(s-value win :aggregate (create-instance nil opal:aggregate))
	;; Initialize coordinate transform.
	(s-value win :current-xfrm (fill-3-by-3-identity (g-value win :current-xfrm)))
	(s-value win :title (concatenate 'string
				 "Surf-Hippo Histology - " (string *circuit-name*)))
	(s-value win :icon-title (concatenate 'string
				      "Surf-Hippo Histology - " (string *circuit-name*)))
	(s-value win :scale *scale)
	(draw-scale-bar)
	(if *draw-cells (maphash 'draw-cell cell-hash-table))
	(if (and *include-light-synapses *draw-synapse-stimulus) (draw-synapse-stimulus))
	(opal:raise-window win)
	(opal:deiconify-window win)
	(opal:update win t)
	)))


;; need these garnet functins.
(defun clear-histology-window ())

;;;; DRAW-SYNAPSE-STIMULUS
#+symbolics
(defun draw-synapse-stimulus ()
  (setq *light-graphics-object
	(graphics:with-output-as-graphics-presentation () 
	  (cond ((eq *light-stimulus 'moving-bar)
		 (draw-moving-bar))
		((eq *light-stimulus 'reversing-bar)
		 (draw-moving-bar))
		((eq *light-stimulus 'moving-bar-grating)
		 (draw-grating))	
		((eq *light-stimulus 'apparent-motion)
		 (draw-apparent-motion))
		((eq *light-stimulus 'spot)
		 (draw-spot))
		((eq *light-stimulus 'annulus)
		 (graphics:draw-circle *light-start-position-x  *light-start-position-y
				       (/  *spot-outside-diameter 2.0)
				       :inner-radius (/ *spot-inside-diameter 2.0)
				       :gray-level 0.2))))))

#+garnet
(defun draw-synapse-stimulus ()
  (cond ((eq *light-stimulus 'moving-bar)
	 (draw-moving-bar))
	((eq *light-stimulus 'reversing-bar)
	 (draw-moving-bar))
	((eq *light-stimulus 'moving-bar-grating)
	 (draw-grating))	
	((eq *light-stimulus 'apparent-motion)
	 (draw-apparent-motion))
	((eq *light-stimulus 'spot)
	 (draw-spot))
	;; Need to make this into a real annulus.
	((eq *light-stimulus 'annulus)
	 (new-garnet-circle *light-start-position-x  *light-start-position-y
			    (/ *spot-inside-diameter 2.0))
	 (new-garnet-circle *light-start-position-x  *light-start-position-y
			    (/  *spot-outside-diameter 2.0)))))

(defun update-light-stimulus-graphics (&optional (redraw-stimulus t))
  #+symbolics
  (graphics:erase-graphics-presentation *light-graphics-object)
  (if redraw-stimulus (draw-synapse-stimulus)))

#+garnet
(defun draw-spot ()
  ())

#+symbolics
(defun draw-spot ()
  (graphics:draw-circle *light-start-position-x  *light-start-position-y
			(/  *spot-outside-diameter 2.0)
			:gray-level 0.2 )
  (graphics:with-graphics-translation (t 260 80)
    (graphics:draw-line 0 0.0 100 0.0
			:thickness 3)
    (graphics:draw-line (* 100 (/ *light-stimulus-start-time user-stop-time)) 0.0
			(* 100 (/ (min *light-stimulus-stop-time user-stop-time) user-stop-time)) 0.0
			:thickness 6)

    (graphics:draw-string (format nil "Spot")
			  (* 0.5 (+ (* 100 (/ *light-stimulus-start-time user-stop-time))
				    (* 100 (/ (min *light-stimulus-stop-time user-stop-time)
					      user-stop-time)))) 10
			  :character-style '(:fix :roman nil) ))
  (graphics:with-graphics-translation (t 260 60)
    (graphics:draw-line 0 0.0 100 0.0 :thickness 1)
    (graphics:draw-triangle 0 0.5 5 3 5 -3 )
    (graphics:draw-triangle 100 0.5 95 3 95 -3 )
    (graphics:draw-string (format nil "~a msec" user-stop-time) 40 5 :character-style '(:fix :italic :small) )
    ))

;;; DRAW-APPARENT-MOTION
#+garnet
(defun draw-apparent-motion ()
  ())
#+symbolics
(defun draw-apparent-motion ()
  (graphics:with-graphics-translation (t *bar-a-position-x *bar-a-position-y)
    (graphics:with-graphics-rotation (t *light-theta)
      (graphics:draw-line (/ *bar-a-length -2) 0.0 (/ *bar-a-length 2) 0.0
			  :thickness   *bar-a-width :gray-level (* *bar-a-intensity .4) :opaque nil)
      (graphics:with-graphics-translation (t (/ *bar-a-length -2) 0)
	(graphics:draw-string (format nil "A")  -70 0 :character-style '(:dutch :bold :large) ))))
  (graphics:with-graphics-translation (t *bar-b-position-x *bar-b-position-y)
    (graphics:with-graphics-rotation (t *light-theta)
      (graphics:draw-line (/ *bar-b-length -2) 0.0 (/ *bar-b-length 2) 0.0
			  :thickness   *bar-b-width :gray-level (* *bar-b-intensity .4) :opaque nil)
      (graphics:with-graphics-translation (t (/ *bar-b-length -2) 0)
	(graphics:draw-string (format nil "B")  -70 0 :character-style '(:dutch :bold :large) ))))
  (graphics:with-graphics-translation (t 330 180)
    (graphics:draw-line 0 0.0 200 0.0 :thickness 5)
    (graphics:draw-line (* 200 (/ *bar-a-start-time user-stop-time)) 0.0
			(* 200 (/ *bar-a-stop-time user-stop-time)) 0.0
			:thickness 10)
    (graphics:draw-string (format nil "A")
			  (* 0.5 (+ (* 200 (/ *bar-a-start-time user-stop-time))
				    (* 200 (/ *bar-a-stop-time user-stop-time)))) 10
			  :character-style '(:fix :roman nil) ))
  (graphics:with-graphics-translation (t 330 150)
    (graphics:draw-line 0 0.0 200 0.0 :thickness 5)
    (graphics:draw-line (* 200 (/ *bar-b-start-time user-stop-time)) 0.0
			(* 200 (/ *bar-b-stop-time user-stop-time)) 0.0
			:thickness 10)
    (graphics:draw-string (format nil "B")
			  (* 0.5 (+ (* 200 (/ *bar-b-start-time user-stop-time))
				    (* 200 (/ *bar-b-stop-time user-stop-time)))) 10
			  :character-style '(:fix :roman nil) ))
  (graphics:with-graphics-translation (t 330 120)
    (graphics:draw-line 0 0.0 200 0.0 :thickness 1)
    (graphics:draw-triangle 0 0.5 5 3 5 -3 )
    (graphics:draw-triangle 200 0.5 195 3 195 -3 )
    (graphics:draw-string (format nil "~a msec" user-stop-time) 80 5
			  :character-style '(:fix :italic :normal) )
    ))

;;; DRAW-MOVING-BAR
(defvar  *label-stimulus-times nil)
#+garnet
(defun draw-moving-bar ()
  (let ((moving-bar-snapshot-times 
	 (loop for i from 0 to 5 collect
	       (truncate (* i (/ (min *grating-temporal-period user-stop-time) 5))))))
    (dolist (snapshot-time moving-bar-snapshot-times)
      (let ((y-translation
	     (cond
	       ((> snapshot-time *motion-stop-time)
		(* (if *light-direction *light-speed (- *light-speed))
		   (- *motion-stop-time *motion-start-time)))
	       ((and (> snapshot-time *motion-start-time)(< snapshot-time *motion-stop-time))
		(* (if *light-direction *light-speed (- *light-speed))
		   (- snapshot-time *motion-start-time)))
	       (t  0.0))))
	(with-graphics-translation (t 0.0 y-translation)
	  (with-graphics-rotation (t *light-theta)
	    (with-graphics-translation (t *light-start-position-x *light-start-position-y)
	      (new-garnet-line (/ *bar-length -2) 0.0 (/ *bar-length 2) 0.0
			       :thickness *bar-width :style `grey )
	      (if *label-stimulus-times
		  (with-graphics-translation (t 0.0 (/ *bar-length -1.8))
		    (new-garnet-string (format nil "~amS" snapshot-time) 0.0 0.0)))))))))
  (draw-motion-arrow))

#+symbolics
(defun draw-moving-bar ()
  (let ((moving-bar-snapshot-times '()))
    (dotimes (i 6)
      (setq moving-bar-snapshot-times
	    (nconc moving-bar-snapshot-times
		   (list (truncate
			  (* i (/ (min *grating-temporal-period user-stop-time) 5)))))))
    (with-graphics-translation (t *light-start-position-x *light-start-position-y)
      (with-graphics-rotation (t *light-theta)
	(dolist (snapshot-time moving-bar-snapshot-times)
	  (let ((y-translation))
	    (cond
	      ((> snapshot-time *motion-stop-time)
	       (setq  y-translation
		      (* (if *light-direction *light-speed (- *light-speed))
			 (- *motion-stop-time *motion-start-time))))
	      ((and (> snapshot-time *motion-start-time)(< snapshot-time *motion-stop-time))
	       (setq  y-translation
		      (* (if *light-direction *light-speed (- *light-speed))
			 (- snapshot-time *motion-start-time))))
	      (t  (setq y-translation  0.0)))
	    (graphics:with-graphics-translation (t 0 y-translation)
	      (graphics:draw-line (/ *bar-length -2) 0.0 (/ *bar-length 2) 0.0
				  :thickness   *bar-width :gray-level 0.15 :opaque nil)
	      (if *label-stimulus-times
		  (graphics:with-graphics-translation (t (/ *bar-length -2) 0)
		    (graphics:draw-string (format nil "T = ~a" snapshot-time)
					  0 0 ;(/ *bar-length -2) (/ *bar-width 2) 
					  :character-style '(:fix :roman :small) )))))))))
  (draw-motion-arrow))


#+garnet
(defun draw-motion-arrow ()
  (with-graphics-rotation (t (+ (if *light-direction 0.0 pi-single) *light-theta))
    (with-graphics-translation  (t 200.0 200.0)
      (new-garnet-string (format nil "Motion") 40.0 0.0)
      (new-garnet-arrow 0.0 50.0 100))))



#+symbolics
(defun draw-motion-arrow ()
  (graphics:with-graphics-translation  (t 400 60)
    (graphics:draw-string (format nil "Motion")
			  30 5 :character-style '(:fix :italic :normal) )
    (graphics:with-graphics-rotation (t   (+  *light-theta (* (if *light-direction 1 -1) 0.5 pi-single)))
      (graphics:draw-arrow -25 0.0 25 0.0 :thickness 2))))


;;; DRAW-grating
#+garnet
(defun draw-grating ()
  ())
#+symbolics
(defun draw-grating ()
  (graphics:with-graphics-translation (t *light-start-position-x *light-start-position-y)
    (graphics:with-graphics-rotation (t *light-theta)
      (let ((y-translation-up 0)(y-translation-down 0))
	(dotimes (i (floor (/ (/ 1000 *grating-spatial-period) 2.0)))
	  (ignore i)
	  (setq  y-translation-up (+ y-translation-up *grating-spatial-period))
	  (setq  y-translation-down (- y-translation-down *grating-spatial-period))
	  (graphics:with-graphics-translation (t 0 y-translation-up)
	    (graphics:draw-line (/ *bar-length -2) 0.0 (/ *bar-length 2) 0.0
				:thickness   *bar-width :gray-level 0.15 :opaque nil))
	  (graphics:with-graphics-translation (t 0 y-translation-down)
	    (graphics:draw-line (/ *bar-length -2) 0.0 (/ *bar-length 2) 0.0
				:thickness   *bar-width :gray-level 0.15 :opaque nil)
	    )))
      (graphics:draw-line (/ *bar-length -2) 0.0 (/ *bar-length 2) 0.0
			  :thickness   *bar-width :gray-level 0.15 :opaque nil)
      ))
    (draw-motion-arrow))




;;; DRAW-CELL
#+symbolics
(defun draw-cell (name cell)
  (ignore name)
  (setf (cell-graphics-object cell) '())
;    (print segment-list)
  (setf (cell-graphics-object cell)
	(cons
	  (graphics:with-output-as-graphics-presentation () (draw-cell-segments cell))
	  (cell-graphics-object cell)))
  (setf (cell-graphics-object cell)
	(cons
	  (graphics:with-output-as-graphics-presentation () (draw-soma cell))
	  (cell-graphics-object cell)))
  (setf (cell-graphics-object cell)
	(cons
	  (graphics:with-output-as-graphics-presentation () (draw-cell-isources cell))
  (cell-graphics-object cell))))
#+garnet
(defun draw-cell (name cell)
  (declare (ignore name))
  (draw-soma cell)
  (draw-cell-segments cell)
  (draw-cell-isources cell))


;; LABEL-CELL-NODE
(defun label-cell-node (node  &key (x-offset 0.0) (y-offset 0.0) (extra-label nil))
  (format nil "~a~a" extra-label (node-name node))
  (let* ((node-location (mapcar '+ (cell-origin (node-cell node)) (node-relative-location  node)))
	 (view-plane-x (get-view-plane-x node-location))
	 (view-plane-y (get-view-plane-y node-location)))
#+symbolics
    (graphics:draw-string (format nil "~a" (node-name node))
			  (+ x-offset view-plane-x)
			  (+  y-offset  view-plane-y)
			  :character-style '(:fix :roman :very-small))
#+garnet
    (new-garnet-string (format nil "~a" (node-name node))
			  (+ x-offset view-plane-x)
			  (+  y-offset  view-plane-y)
			  :character-style '(:fix :roman :very-small))))


;;; DRAW-CELL-ISOURCES
#+garnet
(defun draw-cell-isources (cell)
  (let ((cell-origin (cell-origin cell)))
    (dolist (node (cell-nodes cell))
      (dolist (element (node-elements node))
	(if (eq (named-structure-symbol element) 'isource)
	    ;; Offset a bit for soma source
	    (let ((y-offset (if (equalp (node-relative-location node) '(0.0 0.0 0.0))
				(- (* 0.5 (soma-diameter (cell-soma cell)))) 0.0)))
	      (if *label-sources (label-cell-node node :x-offset 13 :y-offset (+ -16  y-offset)))
	      (let* ((node-location (mapcar '+ cell-origin (node-relative-location (isource-node-2 element))))
		     (view-plane-x (get-view-plane-x node-location))
		     (view-plane-y (get-view-plane-y node-location)))
		(new-garnet-arrow
		 view-plane-x (+ view-plane-y -10.0 y-offset) 60.0 :thickness 3))))))))

#+symbolics
(defun draw-cell-isources (cell)
  (let ((cell-origin (cell-origin cell)))
    (dolist (node (cell-nodes cell))
      (dolist (element (node-elements node))
;	(format *surf-interaction-pane "node ~a: ~a~%" (node-name node)  (named-structure-symbol element))
	(if (eq (named-structure-symbol element) 'isource)
	    (let ((y-offset (if (equalp (node-relative-location node) '(0 0 0))
				(- (* 0.5 (soma-diameter (cell-soma cell)))) 0)))	;offset a bit
						;for soma source
	      (if *label-sources (label-cell-node node :x-offset 3 :y-offset (+ -10  y-offset)))
	      (let* ((node-location (mapcar '+ cell-origin (node-relative-location (isource-node-2 element))))
		     (view-plane-x (get-view-plane-x node-location))
		     (view-plane-y (get-view-plane-y node-location)))
		(graphics:draw-arrow
		  view-plane-x
		  (+ view-plane-y -10 y-offset)
		  view-plane-x
		  (+ view-plane-y   y-offset)
		  :arrow-head-length 10 :arrow-base-width 4 ))))))))

;;; DRAW-SCALE-BAR
(defun draw-scale-bar () 
  #+symbolics
  (graphics:with-graphics-translation (t -400 180)
    (graphics:draw-line 0 0 100 0 :thickness 1)
    (graphics:draw-line 0 -5 0 5 :thickness 1)
    (graphics:draw-line 100 -5 100 5 :thickness 1)
    (graphics:draw-string "100 uM" 30 -10  :character-style '(:fix :italic :normal)))
;  #+garnet
;  (new-garnet-line -2000.0 0.0 2000.0 0.0)
;  #+garnet
;  (new-garnet-line 0.0 -2000.0 0.0 2000.0 :thickness 1)
  #+garnet
  (with-graphics-translation (t -200.0 -200.0)
    (new-garnet-line 0.0 0.0 100.0 0.0 :thickness 1)
    (new-garnet-line 0.0 -5.0 0.0 5.0 :thickness 1)
    (new-garnet-line 100.0 -5.0 100.0 5.0 :thickness 1)
    (new-garnet-string "100 uM" 30.0 -10.0  :character-style '(:fix :italic :normal))))


  
;;; DRAW-CELL-SEGMENTS 
(defvar *exaggerate-plotted-node nil)
(defun draw-cell-segments (cell)
  ;;  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((cell-origin (cell-origin cell)))
    (dolist (segment (cell-segments cell))
      (let* ((start-location (mapcar '+ cell-origin (node-relative-location (segment-node-1 segment))))
	     (end-location (mapcar '+ cell-origin (node-relative-location (segment-node-2 segment))))
	     (start-x (get-view-plane-x start-location))
	     (start-y (get-view-plane-y start-location))
	     (end-x  (get-view-plane-x end-location))
	     (end-y (get-view-plane-y end-location)))
	(declare (single-float start-x start-y end-x end-y))
	#+symbolics
	(graphics:draw-line
	 start-x  start-y end-x end-y
	 :thickness (minimum-plotted-segment-diameter (segment-diameter segment)))
	#+garnet
	(new-garnet-line start-x start-y end-x end-y :thickness (segment-diameter segment))
	#+symbolics		
	(graphics:draw-circle
	 end-x end-y
	 (if (or (member (node-name (segment-node-2 segment)) *plot-nodes* :test #'equal)
		 (member (node-name (segment-node-2 segment)) *analysis-nodes* :test #'equal))
	     (if *exaggerate-plotted-node  5 2) 1)) ;Draw segment end.
	#+garnet
	(new-garnet-circle end-x end-y
			   (if (or (member (node-name (segment-node-2 segment)) *plot-nodes* :test #'equal)
				   (member (node-name (segment-node-2 segment)) *analysis-nodes* :test #'equal))
			       (if *exaggerate-plotted-node 5 2) 1)
			   :filled t)
	(if (and *label-synapses (node-has-synapse (segment-node-2 segment) ))
	    (label-cell-node (segment-node-2 segment) :x-offset -10 :y-offset -10 :extra-label "SYN ")
	    (if  (or *label-nodes
		     (and *label-plotted-nodes
			  (member (node-name (segment-node-2 segment)) *plot-nodes* :test #'equal)))
		 (label-cell-node (segment-node-2 segment) :x-offset -10 :y-offset -10 ))))
      )))

;;; DRAW-SOMA
(defun draw-soma (cell)
  (let* ((cell-origin (cell-origin cell))
	 (start-location (mapcar '+ cell-origin (node-relative-location (soma-node (cell-soma cell)))))
	 (x (get-view-plane-x start-location))
	 (y (get-view-plane-y start-location)))	
    #+symbolics
    (graphics:draw-circle x y (* 0.5 (soma-diameter (cell-soma cell))))
    #+garnet
    (new-garnet-circle x y (* 0.5 (soma-diameter (cell-soma cell)))
		       :filled t)))


;;; ERASE-CELL  Does not work......
#+symbolics
(defun erase-cell (cell-graphics-object-list)
  (dolist (cell-graphics-object cell-graphics-object-list)
    (graphics:erase-graphics-presentation cell-graphics-object)))


#+symbolics
(defun histograms (&optional (stream *standard-output*))
  (dotimes (ignore 10)
    (let ((n (random 100)))
      (format stream "~&~3D~10T" n)
      (let ((height (- (send stream :line-height)(send stream :vsp)))
	    (width (* 10 (send stream :char-width))))
	(graphics:with-room-for-graphics
	  (stream height :fresh-line nil :move-cursor nil)
	  (graphics:draw-rectangle 0 0 width height :gray-level .15 :stream stream)
	  (graphics:draw-rectangle 0 0 (* width (/ n 100)) height :gray-level .05 :stream stream)
;	  (graphics:draw-rectangle 0 0 width height :filled nil :stream stream)
	  (send stream :increment-cursorpos width 0)
	  (format stream " %~%"))))))


(defun minimum-plotted-segment-diameter (segment-diameter)
  (let ((actual-pixel-diameter (* segment-diameter *pixels-per-micron)))
    (if (<  actual-pixel-diameter 1.50) 1.50 actual-pixel-diameter)))


#|
;;;; "The Dynamic Neuron Builder"
(defvar *segment-list* nil)

#+symbolics
(defun find-and-query-nearest-node (x y segment-list)
  (let ((min-distance)(distance)(nearest-segment)(*standard-output* *histology-note-pane))
    (dolist (segment segment-list)
      (setq distance
 	    (sqrt (+ (square (- x (nth 2 segment)))(square (- y (nth 3 segment))))))
      (if (not min-distance)
	  (setq min-distance distance nearest-segment segment)
	  (if (< distance min-distance)
	      (setq min-distance distance nearest-segment segment))))
    (clear-and-select-window)
    (if (y-or-n-p (format t "Do you want node ~a?" (nth 1 nearest-segment)))
	nearest-segment)))




(defvar *neuron-builder-scale 1)

;SCALE FACTOR SHOULD BE IN MICRONS PER PIXEL
#+symbolics
(defun segment-exists (segment-candidate-nodes segment-list)
  (let ((node1 (nth 0 segment-candidate-nodes))
	(node2 (nth 10 segment-candidate-nodes)))
    (dolist (segment segment-list)
      (if (or (and (eql (nth 0 segment) node1)(eql (nth 1 segment) node2))
	      (and (eql (nth 1 segment) node1)(eql (nth 0 segment) node2)))
	  (return segment)))))



(defvar *delete nil)
(defvar *synapse nil)
(defvar *diameter nil)


(defun look-for-element-in-extras-list (extras-list element)
  (dolist (element-list extras-list)
    (if (equal (car element-list) element)
	(return t))))


#+symbolics
(defun edit-segment (edited-node segment-list cell-name)
  (setq *synapse (look-for-element-in-extras-list (nth 6 edited-node) 'synapse))
  (setq *diameter (nth 5 edited-node))
  (tv:choose-variable-values
    '((*delete "Delete this node" :boolean)
      (*synapse "Add synapse to this node" :boolean)
      (*diameter "Diameter of this segment" :number))
    ':label (format nil "Edit Node ~a" (nth 1 edited-node)))
  (draw-cell-from-segment-list segment-list edited-node)
  (setq segment-list (remove edited-node segment-list :test #'equal))
  (if (not (and *delete
		(y-or-n-p (format nil "Are you sure you want to delete node ~a?" (nth 1 edited-node)))))
      (progn
	(if (= 7 (length edited-node))
	    (setq edited-node (butlast edited-node)))
	(setq edited-node
	      (nconc (butlast edited-node) (if *synapse (list *diameter (list (list 'synapse cell-name)))
					       (list *diameter))))
	(setq segment-list (nconc segment-list (list edited-node)))
	(draw-cell-from-segment-list segment-list edited-node)))
  segment-list)


#+symbolics
(defun edit-synapse-annulus (segment-list cell-name)
  (let ((inner-radius
	  (prompt-and-accept (list :type 'number :default 0) "Enter inner radius"))
	(outer-radius 
	  (prompt-and-accept (list :type 'number :default 0) "Enter outer radius"))
	(soma-location-x)(soma-location-y))
    (dolist (segment segment-list)
      (if (equal (nth 0 segment)(nth 1 segment))
	  (setq soma-location-x (nth 2 segment)
		soma-location-y (nth 3 segment))))
    (dolist (edited-node segment-list)
      (let ((distance
	      (/ (cartesian-distance soma-location-x soma-location-y
				     (nth 2 edited-node) (nth 3 edited-node))
		 *pixels-per-micron)))
	(draw-cell-from-segment-list segment-list edited-node)
	(setq segment-list (remove edited-node segment-list :test #'equal))
	(if (and (> distance inner-radius) (< distance outer-radius))
	    (if (= 6 (length edited-node))
		(setq edited-node (nconc edited-node (list (list (list 'synapse cell-name))))))
	    (if (= 7 (length edited-node))
		(setq edited-node (butlast edited-node))))
	(setq segment-list (nconc (list edited-node)  segment-list))
	(draw-cell-from-segment-list segment-list edited-node))))
  segment-list)

#+symbolics
(defun edit-distal-synapses (segment-list cell-name)
  (let ((distal-synapses (y-or-n-p "Do you want distal tip synapses?")))
    (dolist (edited-node segment-list)
      (let ((is-distal t))
	(dolist (test-node segment-list)
	  (if (equal (nth 1 edited-node) (nth 0 test-node))
	      (return (setq is-distal nil))))
	(draw-cell-from-segment-list segment-list edited-node)
	(setq segment-list (remove edited-node segment-list :test #'equal))
	(cond ((and distal-synapses is-distal)
	       (if (= 6 (length edited-node))
		   (setq edited-node (nconc edited-node (list (list (list 'synapse cell-name)))))))
	      (t
	       (if (= 7 (length edited-node))
		   (setq edited-node (butlast edited-node)))))
	(setq segment-list (nconc (list edited-node)  segment-list))
	(draw-cell-from-segment-list segment-list edited-node))))
  segment-list)

(defun cartesian-distance (x1 y1 x2 y2)
  (sqrt (+ (square (- x1 x2))
	   (square (- y1 y2)))))


#+symbolics
(defun mouse-find-node (segment-list)
  (let ((scale-left)(scale-right) 
	(segment-diameter-in-microns (if segment-list (nth 5 (car (last segment-list))) 1)))
    (multiple-value-bind (start-x start-y edited-node extras-flag)
	(dw:tracking-mouse ()
	  (:who-line-documentation-string
	    ()
	    (cond
 	      ((tv:key-state :super) "s-Mouse-L to refresh display; s-Mouse-R for syn annulus.")
	      ((tv:key-state :meta) "m-Mouse-L for scale 1;m-Mouse-R for scale 2;m-Mouse-M for scale distance.")
	      ((tv:key-state :shift)
	       "sh-Mouse-L diameter, sh-Mouse-M to edit segment.")
	      (t (if segment-list
		     "Mouse-L: Find nearest node; Mouse-R to QUIT."
		     "Mouse-L: Put soma here; Mouse-R to QUIT."))))
	  (:mouse-click (click x y)
	   (graphics:untransform-window-points *standard-output* x y)
	   (cond ((eql click #\mouse-l)		;Put down starting point
		  (if segment-list 
		      (let ((segment (find-and-query-nearest-node x y segment-list)))
			(if segment (return (values (nth 2 segment)(nth 3 segment)(nth 1 segment)))))
		      (return (values x y nil))))
		 ((eql click #\m-mouse-l) (setq scale-left (cons x y)))
		 ((eql click #\m-mouse-r) (setq scale-right (cons x y)))
		 ((eql click #\m-mouse-m)
		  (if (and scale-left scale-right)
		      (progn
			(setq *pixels-per-micron
			      (/ (cartesian-distance
				   (car scale-left)(cdr scale-left)(car scale-right)(cdr scale-right))
				 (prompt-and-accept (list :type 'number)
						    "Enter scale bar distance in microns")))
			(return (values nil nil nil 'rescale)))))
		 ((eql click #\s-mouse-l)	;Rescale and redraw
		  (setq  *neuron-builder-scale
			 (prompt-and-accept
			   (list :type 'number :default *neuron-builder-scale) "Enter scale - "))
		  (return (values nil nil nil 'rescale)))
		 ((eql click #\sh-mouse-m)	;Edit segment
		  (let ((segment (find-and-query-nearest-node x y segment-list)))
		    (if segment (return (values nil nil segment)))))
		 ((eql click #\s-mouse-r)	;Add synapse annulus
		  (cond ((y-or-n-p "Edit synapse annulus?")
			 (return (values nil nil nil 'edit-synapse-annulus)))
			((y-or-n-p "Edit distal synapses?")
			 (return (values nil nil nil 'edit-distal-synapses)))))
		 ((eql click #\sh-mouse-l)
		  (setq segment-diameter-in-microns
			(prompt-and-accept
			  (list :type 'number :default segment-diameter-in-microns
				:stream *histology-note-pane) "Enter segment diameter in microns")))
		 ((eql click #\mouse-r)  (return)))))
      (let ((old-x nil)(old-y nil))
	(dw:with-output-recording-disabled ()
	  (if start-x
	      (dw:tracking-mouse ()
		(:who-line-documentation-string () "Put other end of line here.")
		(:mouse-motion (x y)
		 (graphics:untransform-window-points *standard-output* x y)
		 (when (and old-x old-y)
		   (graphics:draw-line start-x start-y old-x old-y :alu :flip
				       :thickness (minimum-plotted-segment-diameter segment-diameter-in-microns)))
		 (graphics:draw-line start-x start-y x y :alu :flip
				     :thickness (minimum-plotted-segment-diameter segment-diameter-in-microns))
		 (setq old-x x old-y y))
		(:mouse-click (click x y)
		 (graphics:untransform-window-points *standard-output* x y)
		 (ignore click)
;		 (unless (eql click #\mouse-l)
;		   (signal 'sys:abort))
		 (when (and old-x old-y)
		   (graphics:draw-line start-x start-y old-x old-y :alu :flip
				       :thickness (minimum-plotted-segment-diameter segment-diameter-in-microns)))
		 (return (values start-x start-y edited-node x y segment-diameter-in-microns nil))))
	      (if edited-node
		  (values nil nil edited-node nil nil nil NIL)
		  (values nil nil nil nil nil nil extras-flag))))))))

#+symbolics
(defun clear-and-select-window (&optional (window *standard-output*))
  (send window :clear-window)
  (send window :select))

#+symbolics
(defun draw-cell-from-segment-list (segment-list &optional only-this-segment)
  (let ((proximal-segment)(proximal-node))
    (dolist (segment segment-list)
      (setq proximal-node (nth 0 segment))
      (if (or (not only-this-segment) (eql only-this-segment segment))
	  (if (equal proximal-node (nth 1 segment))	;we have soma
	      (graphics:draw-circle
		(nth 2 segment) (nth 3 segment)
		(* *pixels-per-micron (* 0.5 (if (nth 5 segment)(nth 5 segment) 10)))
		:alu :flip)
	      (if (setq proximal-segment
			(dolist (segment segment-list)
			  (if (eql proximal-node (nth 1 segment)) (return segment))))
		  (progn
		    (graphics:draw-line
		      (nth 2 proximal-segment) (nth 3 proximal-segment) (nth 2 segment) (nth 3 segment)
		      :thickness (minimum-plotted-segment-diameter (if (nth 5 segment)(nth 5 segment) 1))
		      :alu :flip)
		    (if (look-for-element-in-extras-list (nth 6 segment) 'synapse)
			(graphics:draw-circle  (nth 2 segment) (nth 3 segment) 6 :alu :flip))
		    (graphics:draw-string
		      (format nil "~a"  (nth 1 segment)) (nth 2 segment) (nth 3 segment) 
		      :character-style '(:fix :roman :very-small)  :alu :flip)))))))
  (graphics:with-graphics-translation (t 700 500)
    (graphics:with-graphics-scale (t *pixels-per-micron)
      (graphics:draw-line 0 0  100 0 :thickness 1)
      (graphics:draw-line 0 -5 0 5 :thickness 1)
      (graphics:draw-line 100 -5 100 5 :thickness 1)
      (graphics:draw-string "100 microns"  30 -10  :character-style '(:fix :italic :small)))))

(defun name-already-used (name segment-list)
  (dolist (segment segment-list)
    (if (eql name (nth 1 segment)) (return t))))





#+symbolics
(defun draw-some-lines (&optional (segment-list '()) cell-name &key (origin-x 0) (origin-y 0))
  (setq *neuron-builder-scale 1)
  (let ((*standard-output*)(*query-io* *histology-note-pane))
    (clear-and-select-window  *histology-note-pane)
    (if segment-list
	(setq *pixels-per-micron
	      (prompt-and-accept (list :type 'number :default *pixels-per-micron) "Enter pixels-per-micron - ")))
    (setq *standard-output* *histology-graphics-pane)
    (send *histology-graphics-pane :set-label "The Dynamic Neuron Builder") 
    (clear-and-select-window)
   (send *standard-output* :clear-history)
    (send *standard-output* :set-mouse-blinker-character #\mouse:maltese-cross)
    (loop
 while 
   (graphics:with-graphics-translation (t origin-x origin-y)
     (graphics:with-graphics-scale (t *neuron-builder-scale)
       (send  *standard-output* :clear-window)
       (if segment-list (draw-cell-from-segment-list segment-list))
       (let ((new-name (top-node-number segment-list))(last-name (if (not segment-list) 'soma)))
	 (loop
	   (multiple-value-bind (x1 y1 edited-node x2 y2 diameter extras-flag)
	       (mouse-find-node segment-list)

	     (cond  ((and extras-flag (not (equal extras-flag 'rescale)))
		     (setq segment-list (funcall extras-flag segment-list cell-name)))
		    ((and segment-list (not edited-node))
		     (return (equal extras-flag 'rescale)))
		    ((not x1)
		     (setq segment-list
			   (edit-segment edited-node segment-list cell-name)))
		    (t
		     (graphics:draw-line x1 y1 x2 y2 :thickness (minimum-plotted-segment-diameter diameter))
		     (find-and-draw-soma segment-list)
		     (clear-and-select-window  *histology-note-pane)
		     (if edited-node (setq last-name edited-node))
		     (loop do (setq new-name (get-new-name new-name last-name))
			   while (if (not new-name) t (name-already-used new-name segment-list)))
		     (graphics:draw-string (format nil "~a" new-name) x2 y2 :alu :flip :character-style
					   '(:fix :roman :very-small) )
		     (setq segment-list
			   (if segment-list
			       (nconc segment-list (list  (list last-name new-name x2 y2 0 diameter)))
			       (list (list last-name last-name x1 y1 0 10)
				     (list last-name new-name x2 y2 0 diameter))))))
	     (setq *segment-list* segment-list)))))))))


(defun top-node-number (segment-list)
  (if segment-list
      (let ((top-node 0))
  	(dolist (segment segment-list)
	  (if (numberp (nth 1 segment))
	      (if (>  (nth 1 segment) top-node)
		  (setq  top-node (nth 1 segment)))))
	(if (> top-node 0)
	    top-node))))

#+symbolics
(defun find-and-draw-soma (segment-list)
  (if (= 2 (length segment-list))
      (graphics:draw-circle
	(nth 2 (car segment-list)) (nth 3 (car segment-list))
	(* *neuron-builder-scale *pixels-per-micron (* 0.5 (nth 5  (car segment-list)))))))
#+symbolics
(defun get-new-name (new-name last-name)
  (if (not
	(if (numberp new-name)
	    (if (y-or-n-p
		  (format *histology-note-pane
			  "Connect to node ~a - increment node ~a for new node?" last-name new-name))
		(setq new-name (1+ new-name)))))
      (setq new-name
	    (prompt-and-accept '(or number symbol) "Connect to node ~a - new node:" last-name)))  
  new-name)

|#



