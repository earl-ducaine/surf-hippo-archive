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


;;; GUI Source file: tracer.lisp


(in-package "SON-OF-PLOT-HACK")


(defun show-scanned-image (&optional filename)
  (set-scanned-image-window-params
   (let ((path (if filename (pathname filename) (file-browser "Find Image File" "/home" '("xbm")))))
     (show-image filename
		 :win (get-plot-window :xy nil nil :name (string (file-namestring path)) :mode :scanner)))))


(defun set-scanned-image-window-params (win)
  (let (dummy1 dummy2 dummy3)
    (remove-all-markers win t)
    (loop until (or dummy3 (= (length (g-value win :markers)) 2))
	  do (remove-all-markers win t)
	  (setq dummy1 (g-value win :y-mag) dummy2 (or (g-value win :y-label) ""))
	  (choose-variable-values
	   '((:comment "Click 2 cross hairs for Y-magnitude")
	     (dummy1 "Y magnitude between the 2 cross hairs:" :float)
	     (dummy2 "Y axis label:" :string)
	     (dummy3 "CANCEL" :boolean))
	   :label (format nil "Setting Y mag for ~A" (g-value win :title))))
    (unless dummy3
      (s-value win :y-label dummy2)
      (s-value win :y-mag (* dummy1
			     (/ (g-value win :height)
				(abs (- (marker-y (nth 0 (g-value win :markers)))
					(marker-y (nth 1 (g-value win :markers)))))))))
    (remove-all-markers win t)
    (loop until (or dummy3 (= (length (g-value win :markers)) 2))
	  do (remove-all-markers win t)
	  (setq dummy1 (g-value win :x-mag)  dummy2 (or (g-value win :x-label) ""))
	  (choose-variable-values
	   '((:comment "Click 2 cross hairs for X-magnitude")
	     (dummy1 "X magnitude between the 2 cross hairs:" :float)
	     (dummy2 "X axis label:" :string)
	     (dummy3 "CANCEL" :boolean))
	   :label (format nil "Setting X mag for ~A" (g-value win :title))))
    (unless dummy3
      (s-value win :x-label dummy2)
      (s-value win :X-mag (* dummy1
			     (/ (g-value win :width)
				(abs (- (marker-x (nth 0 (g-value win :markers)))
					(marker-x (nth 1 (g-value win :markers)))))))))
    (remove-all-markers win t)
    (loop until (or dummy3 (= (length (g-value win :markers)) 1))
	  do (remove-all-markers win t)
	  (setq dummy1 (g-value win :x-origin)
		dummy2 (g-value win :y-origin))
	  (choose-variable-values
	   '((:comment "Click 1 cross hair for XY origin")
	     (dummy1 "X origin:" :float)
	     (dummy2 "y origin:" :float)
	     (dummy3 "CANCEL" :boolean))
	   :label (format nil "Setting XY origin for ~A" (g-value win :title))))
    (unless dummy3
      (s-value win :X-origin dummy1)
      (s-value win :y-origin dummy2)
      (s-value win :x-min (- dummy1 (* (g-value win :x-mag)
				       (/ (marker-x (car (g-value win :markers)))
					  (g-value win :width)))))
      (s-value win :x-max (+ (g-value win :x-mag) (g-value win :x-min)))
    
      (s-value win :y-min (- dummy2 (* (g-value win :y-mag)
				       (- 1.0 (/ (marker-y (car (g-value win :markers)))
						 (g-value win :height))))))
      (s-value win :y-max (+ (g-value win :y-mag) (g-value win :y-min)))

      (s-value win :x-axis-min (g-value win :x-min))
      (s-value win :x-axis-max (g-value win :x-max))
      (s-value win :y-axis-min (g-value win :y-min))
      (s-value win :y-axis-max (g-value win :y-max))

      (unless (g-value win :x-inc)
	(s-value win :x-inc (/ (msd-round (g-value win :x-mag)) 5.0)))

      (unless (g-value win :y-inc)
	(s-value win :y-inc (/ (msd-round (g-value win :y-mag)) 5.0)))
      (remove-all-markers win t)
      (opal:move-component (wh::get-agg win) (g-value win :bitmap) :where :back)
      (s-value win :x-plot-right-gap 0)
      (s-value win :x-plot-left-gap 0)
      (s-value win :label-height 0)
      (s-value win :y-plot-bottom-gap 0)
      (set-plot-area-hw win)
      (draw-all-axes win)))
  (opal:update win))

(defun edit-point-function (interactor points-list)
  (edit-point interactor points-list))

(defun edit-point (interactor points-list)
  (let* ((win (g-value interactor :window))
	 (x-label (g-value win :x-label)) (y-label (g-value win :y-label))
	 distance closest-scanned-point *automatic-run*)
    (loop for marker in (g-value win :markers)
	  when (member (list (marker-data-x marker) (marker-data-y marker))
		       (g-value win :scanned-points) :test 'equal)
	  do (let ((current-distance (cartesian-distance ; (x-1 y-1 x-2 y-2)
				      (nth 2 points-list)
				      (nth 3 points-list)
				      (marker-x marker)
				      (marker-y marker))))
	       (when (or (not distance) (< current-distance distance))
		 (setq distance current-distance
		       closest-scanned-point marker))))
    (when closest-scanned-point
      (let ((highlight (create-instance nil opal:circle
					(:left (- (marker-x closest-scanned-point) 5))
					(:top (- (marker-y closest-scanned-point) 5))
					(:width 10) (:height 10)
					(:filling-style (color-to-fill 'red))))
	    (x (x-plot-win-inv (marker-x closest-scanned-point) win))
	    (y (y-plot-win-inv (marker-y closest-scanned-point) win)))
	(opal:add-component (g-value win :aggregate) highlight :where :front)
	(opal:update win)
	(when (go-ahead-menu (format nil "Remove highlighted point @ ~,2f~a, ~,2f~a?" x x-label  y y-label) )
	  (remove-marker closest-scanned-point win)
	  (s-value win :scanned-points (remove (list x y) (g-value win :scanned-points) :test 'equal)))
	(opal:remove-component (g-value win :aggregate) highlight)
	(opal:update win)))))
			   

(defun collect-trace-points-function (interactor points-list)
  (collect-trace-points interactor points-list))

(defun collect-trace-points (interactor points-list)
  (let* ((win (g-value interactor :window))
	 (x-y (list (x-plot-win-inv (nth 2 points-list) win) (y-plot-win-inv (nth 3 points-list) win)))
	 (x-label (or (g-value win :x-label) "")) (y-label (or (g-value win :y-label) ""))
	 *automatic-run*)
    (add-temp-comment
     win
     (concatenate
      'string
       (format nil "X: ~a ~a~%Y: ~a ~a"
		   (car x-y)
		   x-label (cadr x-y) y-label)
      (when (g-value win :last-pointer-xy)
	(let ((dy (- (cadr x-y) (nth 1 (g-value win :last-pointer-xy))))
		 (dx  (- (car x-y) (nth 0 (g-value win :last-pointer-xy)))))
	     (format nil "~%dy/dx: ~a / ~a"
		     dy  dx))))
     :update t)
    (s-value win :last-pointer-xy x-y)
    (add-marker win points-list :add-cross :w/o-hairs :data-x (car x-y) :data-y (cadr x-y))
    (push x-y (g-value win :scanned-points))))
    
(defun add-scanner-interactors (win)
  (s-value win :scanner-interactor
	   (create-instance nil wh::window-coords-pointer
			    (:Window win)
			    (:feedback-obj wh::cross-hair-w/o-hairs)
			    (:start-event :leftdown)
			    (:final-function #'collect-trace-points-function)))
    (s-value win :scanner-edit-point-interactor
	   (create-instance nil wh::window-coords-pointer
			    (:Window win)
			    (:feedback-obj wh::cross-hair-w/o-hairs)
			    (:start-event :control-leftdown)
			    (:final-function #'edit-point-function))))

(defun trace-image (win &optional clear-points)
  (when clear-points (remove-all-markers win t) (s-value win :scanned-points '()))
  (when
      (go-ahead-menu
       (format nil
	       "Enter points in left-to-right order with the left mouse,~%and hit Ok to plot entered points."))
    (multiple-value-bind (x-list y-list)
	(loop for xy in (sort (copy-list (g-value win :scanned-points)) '< :key 'car)
	      collect (car xy) into x-list
	      collect (cadr xy) into y-list
	      finally (return (values x-list y-list )))
      (plot-timed-data y-list '("") x-list))))

(defun scanned-image-menu (win)
  (let (dummy1 dummy2 dummy3
	(dummy5 (g-value win :x-inc))
	(dummy6 (g-value win :y-inc))
	(dummy7  (g-value win :x-origin))
	(dummy8  (g-value win :y-origin))
	dummy9
	dummy11
	(dummy13 (g-value win :connect-data-points))
	dummy14
	dummy15
	(dummy25 (g-value win :scatter)) dummy27 
	dummy33)
    (setq dummy14 (cond ((and dummy13 dummy25) :Connect_&_show_points)
			(dummy13 :Connect_points)
			(dummy25 :Show_points)))
    (choose-variable-values
     '((dummy1 "Recalibrate image" :boolean)
       (dummy2 "Trace image" :boolean)
       (dummy3 "Clear any points in tracer window" :boolean)
       (dummy5 "X axis interval" :number) (dummy6 "Y axis interval" :number)
       (dummy7 "X origin" :number) (dummy8 "Y origin" :number)
       (dummy14 "Plot technique:" :choose (:Connect_&_show_points :Connect_points :Show_points) :horizontal)
       (dummy27 "More" :boolean))
     :label (format nil "Plot Menu for ~A" (g-value win :title)))
    (case dummy14
      (:Connect_&_show_points (s-value win :connect-data-points t) (s-value win :scatter t))
      (:Connect_points (s-value win :connect-data-points t) (s-value win :scatter nil))
      (:Show_points (s-value win :connect-data-points nil) (s-value win :scatter t)))
    (s-value win :x-inc dummy5) (s-value win :y-inc dummy6)
    (s-value win :x-origin dummy7) (s-value win :y-origin dummy8)
    (when dummy1 (SET-SCANNED-IMAGE-WINDOW-PARAMS win))
    (when dummy2 (trace-image win dummy3))
    (when dummy27
      (let* ((dummy1 nil) (dummy2 nil) dummy3 dummy4
	     dummy6
	     dummy9
	     dummy15
	     dummy16
	     dummy17
	     dummy20
	     dummy22
	     dummy25
	     dummy26 
	     dummy30
	     (menu-list
	      (list
	       '(dummy9 "Ticks, Grid and Axes menu" :boolean)
	       '(dummy6 "Overlay and layout specification and flag menu" :boolean)
	       '(dummy26 "Edit scatter?" :boolean)
	       '(dummy22 "Change axes and label font" :boolean)
	       '(dummy1 "Data and trace offset menu" :boolean)
	       '(dummy2 "Edit labels" :boolean))))
;	(when (g-value win :marked-points) (push '(dummy3 "Edit marked points" :boolean) menu-list))
	(when (or (g-value win :marked-points) (g-value win :baselines))
	  (push '(dummy4 "Edit marked points/baselines font" :boolean) menu-list))
	(choose-variable-values menu-list ':label (format nil "More Plot Parameters for ~A" (g-value win :title)))

	(when dummy6 (overlay-layout-menu win))
	(wh::cond-every
	 (dummy25 (edit-tick-format win win))
	 (dummy9 (axes-menu win win))
	; (dummy3 (Edit-marked-points win))
	 (dummy4 (Edit-marked-points-font win))
	 (dummy22 (s-value win :font
			   (s-value win :plot-axis-font (font-menu (g-value win :plot-axis-font)
								   (g-value win :title)))))
	 (dummy26 (scatter-menu win win))
	 ((eq (g-value win :axes-type) :simple)
	  (SIMPLE-AXES-MENU win)))))
    (draw-all-axes win)
    (opal:update win)))

(export '(show-scanned-image
	  scanned-image-menu
	  ;; ADD-SCANNER-INTERACTORS
	  set-scanned-image-window-params
	  trace-image))
  



