;;; -*- Package: SURF; Mode: LISP -*-


;; Also interesting stuff in misc.lisp



(defun fit-exp (data-list start stop)
  (let ((num 0)(den 0)min max temp (out-list '()))
    (setq temp data-list)
    (dotimes (time (length temp))
      (setq min  (if min (min min (car temp))
		     (car temp))
	    max  (if max (max max (car temp))
		     (car temp))
	    temp (cdr temp)))
    (setq temp (loop for x in data-list		;normalize and shift, log
		     collect (log (/ (+ (- x min) 1d-10)
				     (- max min)))))
    (dotimes (time (length temp))
      (if (and (>= time start)(<= time stop))
	  (setq num (+ num  (*  (- time start)(- time start)))
		den (+ den (* (- time start) (car temp)))))
      (setq temp (cdr temp)))
    (dotimes (time (length data-list ))
      (setq out-list (nconc out-list
			    (list
			      (cond
				((< time start)
				 max)
				((< time stop)
				 (+ min (* (- max min) (exp (/ (- time start) (/  num den))))))
				(t min))))))
    (print (/  num den))
    out-list))


(defun rad-to-deg (angle-in-radians)
  (let ((angle  (* (/  360  ( * 2 pi)) angle-in-radians)))
    (if (< angle 0) (+ 360 angle) angle)))

(defvar *analysis-nodes* '())

(defun tip-tuning ()
  (let ((results-and-angles
	  (sort 
	    (loop for node-name in *analysis-nodes*
		  if (neq (soma-node (cell-soma (node-cell (gethash node-name node-hash-table) )))	;;avoid soma
			  (gethash node-name node-hash-table))
		    collect
		      (let ((amplitude 0)(nl-integral 0)(integral 0))
			(do ((lt  *ordered-time-list* (cdr lt))
			     (lv (reverse (node-voltage-data (gethash node-name node-hash-table))) (cdr lv)))
			    ((not (cdr lt)))
			  (setq nl-integral (+ nl-integral 
					       (* (sigmoid (car lv)) (- (cadr lt) (car lt))))
				integral (+ integral (* (- (car lv) -70) (- (cadr lt) (car lt))))
				amplitude (if (> (- (car lv) -70) amplitude) (- (car lv) -70) amplitude)))
			(cons (list (/ nl-integral user-stop-time) (/ integral user-stop-time) amplitude)
			      (rad-to-deg (atan (nth 1 (node-relative-location (gethash node-name node-hash-table)))
						(nth 0 (node-relative-location (gethash node-name node-hash-table))))))))
	    #'< :key #'cdr)))
    (loop for plot-pane in (list 'ds-pane-0 'ds-pane-1 'ds-pane-2)
	  for plot-label in (list "Tip Tuning - NL-Average" "Tip Tuning - Average-Value" "Tip Tuning - Amplitude")
	  for data-point from 0 to 2
	  do
      (send (send *ds-frame* :get-pane plot-pane)
	    :plot "Node Values "
	    (let ((plot-data '())(plot-angle '()))
	      (dotimes (angle-factor 2)
		(dotimes (i (length results-and-angles))
		  (setq plot-data (nconc (list (nth data-point (car (nth i results-and-angles)))) plot-data)
			plot-angle (nconc (list (+ (cdr (nth i results-and-angles)) (* angle-factor 360))) plot-angle))))
	      (list (list plot-data plot-angle)))
	    nil
	    :x-label "Angle" :y-label plot-label
	    :x-min 0 :x-max 720 :symbols-p t :symbol-size 5))
    (send (send *ds-frame* :get-pane 'ds-interaction-pane) :clear-window)
    (print-circuit (send *ds-frame* :get-pane 'ds-interaction-pane))
    (if *hard-copy-screen
	(let ((temp-window tv:selected-window))
	  (send (send *ds-frame* :get-pane 'ds-interaction-pane) :select)
	  (HCI::HARDCOPY-SCREEN)
	  (send temp-window :select)))
    results-and-angles))


(defvar *sat-nl-low-thresh -60)
(defvar *sat-nl-high-thresh -40)

(defun saturating-non-linearity (voltage &key (low-thresh *sat-nl-low-thresh)(high-thresh *sat-nl-high-thresh))
  (cond ((<= voltage low-thresh) 0)
	((< voltage high-thresh) (/ (-  voltage low-thresh) (- high-thresh low-thresh)))
	(t 1)))

;;; SIGMOID Normal polarity is monotonically increasing.
(defvar *sigmoid-v-half -50)
(defvar *sigmoid-slope .1)
(defun sigmoid (voltage &key (midpoint  *sigmoid-v-half)(slope *sigmoid-slope)(polarity t))
  (let ((arg (* (if polarity 1 -1) (-  midpoint voltage) slope)))
    (cond ((> arg 10) 0)			;Avoid overflowing the exponential
	  ((< arg -10) 1.0)
	  (t (/ 1.0 (+ (exp arg) 1.0))))))


(defvar *do-analysis* t)
;; polar-plot-ds
(defun polar-plot-ds (results-and-angles plot-pane &key (function-index 1))
      (send plot-pane :set-label-font '(:dutch :roman :normal))
      (send plot-pane :plot "Radius = Tip Integral (mV-seconds) Scaled by Stimulus Speed (uM/millisecond)"
	(loop for speed-list in results-and-angles
	      for speed in '(8 2 0.5)
	      collect
		(let ((x-list)(y-list) lastx lasty)
		  (loop for xy in (loop for result-angle-list in speed-list 
					collect
					  (let* ((result (* 1.400 speed (nth function-index (car result-angle-list))))
						 (angle (* 2 pi-single (/ (cdr result-angle-list) 360)))
						 (x (* result (cos angle)))
						 (y (* result (sin angle))))
					    (cons x y))) do
		    (setq x-list (nconc (list (cdr xy)) x-list)
			  y-list (nconc (list (car xy)) y-list)))
		  (setq lastx (car (last x-list)) lasty (car (last y-list)))
		  (list (nconc  (list lastx) x-list) (nconc (list lasty)  y-list))))
	'( "8 uM/mS"  "2 uM/mS" "0.5 uM/mS") 
	:x-min -3 :x-max 3 :y-min -3 :y-max 3 :equal-scale? t :y-interval 1 :x-interval 1
	:lines-p t :symbols-p t :symbol-size 4)
  (send plot-pane :plot nil

	(loop for radius in '(1.0 2.0 )
	      collect
	  (let ((x-list)(y-list))
	    (loop for xy in	    (loop for angle from 0 to 360 by 10
	      collect (let ((x (* radius (cos (* 2 pi-single (/ angle 360)))))
			(y (* radius (sin (* 2 pi-single (/ angle 360))))))
					    (cons x y))) do
		    (setq x-list (nconc (list (car xy)) x-list)
			  y-list (nconc (list (cdr xy)) y-list)))
		  (list x-list y-list)))
	nil :leave-window t :overlay t :all-solid-lines t))



;;; LOW-PASS-THIS-LIST Returns a list of the same length as 'list, convolved by a unit area triangle with base =
;;; 'support. 
(defun low-pass-this-list (list support)
  (do ((list list (cdr list))
       (this-point (car list)(car list))
       (filtered nil (nconc 
		      (list (apply '+
			      (loop for pointer from 0 to support
				    collect (* (/ 2 support)(/ 2 support)
					       (if (<= pointer (/ support 2))
						   pointer (- support pointer))
					       (if (nth pointer list)(nth pointer list) this-point)))))
		       filtered)))
      ((null list) (reverse filtered))))



(defun g-in-t (voltage-control voltage-hyper hyper-current tau r-in)
;; delta t is assumed 1 ms, current is A, voltage waveforms in mV, tau in ms. Ouput list is in siemens.
  (let ((cap (/ (* tau 1.0e-3)(* r-in 1.0e9)))	;tau in ms, r-in in Gohms, cap in F
	v-diff-n+1 v-diff-n g-list)
    (dolist (v-diff (mapcar '- voltage-control voltage-hyper))
      (if v-diff-n
	  (progn
	    (setq v-diff-n+1 v-diff-n
		  v-diff-n (* 1.0e-3 v-diff))
	    (setq g-list (nconc (list (/ (- hyper-current (*  1.0e3 cap (- v-diff-n+1 v-diff-n)))
					 (* 0.5 (+ v-diff-n+1 v-diff-n)))) g-list)))
	  (setq v-diff-n (* 1.0e-3 v-diff))))
    (reverse    g-list)))



(defun plot3d-lineruns ()
)
(defvar line-3d-array (make-raster-array 6 6))
(defun fill-3d-array (di-index cap-index to-array from-array)
  (loop for velocity-index from 0 to 5 do
    (loop for g-in-index from 0 to 5 do
      (setf (raster-aref to-array velocity-index g-in-index)
	    (if (= g-in-index 5) 0
		(aref from-array velocity-index cap-index 0 0 g-in-index (+ 6 di-index))))))
  to-array)

(defvar sun-mathematica-path "ab:/home/vi/lyle/surf/m-data")

(defun write-mathematica-line-runs-output (array name &optional comment)
  (let* ((filename (string-downcase (concatenate 'string sun-mathematica-path
						 (string '/) (string name) ".m")))
		   (stream (open filename :direction :output :if-exists :supersede)))
	 (if comment (format stream (concatenate 'string "(* " (string comment) " *)")))

	 (format stream "~%(* dimensions from outer to inner are capm, di, velocity, and gin-dens *)")

	 (format stream "~%~%(* ~a  *)~% ~%~a = ~%" name  name)
	 (loop for cap-index from 0 to (- (array-dimension array 1) 1) do
	   (format stream "{")
	   (loop for di-index from 0 to 2 do
	     (format stream "~%{")
	     (loop for velocity-index from 0 to (- (array-dimension array 0) 1) do
	       (format stream "~%{")
	       (loop for g-in-index from 0 to (- (array-dimension array 4) 1) do
		 (format stream " ~6f"
			 (aref array velocity-index cap-index 0 0 g-in-index (+ 6 di-index)))
		 (if (/= g-in-index (- (array-dimension array 4) 1)) (format stream ",")))
	       (format stream "}"))
	     (format stream "}"))
	   (format stream "}~%"))
	 (close stream)
	 (format t "File ~a written~%" filename))))


