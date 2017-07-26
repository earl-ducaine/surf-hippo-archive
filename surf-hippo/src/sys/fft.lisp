;;; -*- mode: lisp; Syntax: Common-lisp; package: surf ; base: 10 ; -*-
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


;;; SYS Source file: fft.lisp

(in-package "SURF-HIPPO")

#|
user::dft-init
user::dft-array
user::dft-forward
user::dft-reverse
|#

(in-package "USER")
(export '(dft-init
	  dft-array
	  dft-forward
	  dft-reverse))

(in-package "SURF-HIPPO")

;; Pads WAVE with PAD until its length reaches 2^N, where N is an integer. 
(defun dft-wave (wave &key (pad 0.0))
  (let* ((wave (sequence-to-list wave))
	 (log-size (ceiling (log (length wave) 2)))
	 (n        (expt 2 log-size))
	 (tables   (dft-init log-size))
	 (x-mag   (make-array n :element-type 'dft-float))
	 (x-real   (make-array n :element-type 'dft-float))
	 (x-imag   (make-array n :element-type 'dft-float)))
    (declare (fixnum log-size n)
	     (type (dft-array (*)) x-real x-imag))
    (loop for i from 0 
	  for val in wave do
	  (setf (aref x-real i) val)
	  (setf (aref x-imag i) 0.0))
    (loop for i from (length wave) to (1- n) do
	  (setf (aref x-real i) pad)
	  (setf (aref x-imag i) 0.0))
    (plot-timed-data x-real nil nil :title "Time")
    (dft-forward x-real x-imag tables)
    (loop for i from 0 to (1- n) do
	  (setf (aref x-mag i) (sqrt (+ (square (aref x-real i))
					(square (aref x-imag i))))))
    (plot-timed-data x-real nil nil :title "Frequency Real")
    (plot-timed-data x-mag nil nil :title "Frequency Mag" :y-min 0.0)
    (plot-timed-data x-imag nil nil :title "Frequency Imag")
    (dft-reverse x-real x-imag tables)))
		    


#|


(setq wave  g t)
(dft-stretch-wave wave)
(dft-stretch-wave (list-head
		   (mapcar '+
			   (sequence-to-list (sinewave 1 4300 0.4 :step 0.1))
			   (sequence-to-list (sinewave 1 4300 4 :step 0.1)))
					; 2048
		   )
		  :plot-wave t
		  :delta-t 0.1)


;; Random process from uniform distribution, filtered.
(defun foo ()
  (let* ((delta-t 0.125)
	 (impulse (exponential-array-unit-area 10 delta-t))
	 (wave (loop for time from 0 by delta-t to (1- (* (expt 2 15) delta-t))
		     collect (- (random 1.0) 0.5))))
    (dft-stretch-wave (sequence-head (convolve impulse wave) (- (length wave) (length impulse)))
		      :delta-t delta-t
		      :remove-dc t
		      :plot-wave t)))

(defun foo ()
  (let* ((delta-t 0.125)		; ms
	 (frequency 500)		; hz
	 (impulse (exponential-array-unit-area 10 delta-t))
	 (wave (loop for time from 0 by delta-t to (1- (* (expt 2 11) delta-t))
		     collect (+ (* 0.5
					; (sin (* 2 pi frequency time 0.001))
				   (signum (sin (* 2 pi frequency time 0.001)))
				   )
					; (- (random 1.0) 0.5)
				))))
    (dft-stretch-wave wave		; (sequence-head (convolve impulse wave) (- (length wave) (length impulse)))
		      :delta-t delta-t
		      :remove-dc t
		      :plot-wave t)))
  

|#

#|
  "Plot the frequency magnitude (when PLOT-MAG [default T], normalized if NORMALIZE-MAG [default
NIL], up to MAX-FREQ [hz, default NIL] if non-NIL) and phase (when PLOT-PHASE [default T]) of the
time sequence[s] WAVE (plotted if PLOT-WAVE [default NIL]), which has a time step DELTA-T
[milliseconds, default 1.0]. If REMOVE-DC [default NIL] then the DC component is set to zero;
otherwise a DC-OFFSET [default 0.0] is applied to WAVE before the dft. Plot titles include the
string TITLE-PREFIX [default NIL]. This function processes any length of WAVE by resampling the time
sequence to fit a 2^N array that may be DFTed. A list of strings in LABELS is used to label plot
traces. Note that if a time sequence must be resampled then the effective delta-t will be smaller
than the given DELTA-T. Also, this resampling is done by simple linear interpolation between known
points, and thus may introduce spurious high frequencies into the spectrum. Maximum length of a time
sequence in WAVE is 2^16. 

Given a WAVE input, if RETURN-WAVES is T [default NIL] then returns as values lists for the input and outputs in the form:

 (WAVE MAG PHASE REAL-PART IMAG-PART)

For multiple sequences in the WAVE argument, of the form:

 (WAVE-1 WAVE-2 ...)

Then if RETURN-WAVES is T, the returned values are of the form:

 ((WAVE-1 MAG-1 PHASE-1 REAL-PART-1 IMAG-PART-1) (WAVE-2 MAG-2 PHASE-2 REAL-PART-2 IMAG-PART-2) ... )

The LOG-PLOT-MAG and LOG-PLOT-FREQ arguments control plotting of the magnitude and phase transform."
|#

(defun dft-stretch-wave (wave &key (delta-t 1.0) (dc-offset 0.0) remove-dc normalize-mag max-freq
			      log-plot-mag log-plot-freq
			      labels title-prefix plot-wave (plot-mag t) (plot-phase t)
			      plot-real-wave plot-imag-wave
			      plot-imaginary-wave-in
			      (forward t)
			      (time-shift 0)
			      unfold-phase
			      return-waves imaginary-wave)

  (let* ((title-prefix (string (or title-prefix "")))
	 (data-x-label (if forward "ms" "Hz"))
	 (txfmed-x-label (if forward "Hz" "ms"))
	 (true-max-freq (/ 1 (* 2 0.001 delta-t)))
	 (waves (if (and (consp wave) (consp (car wave))) wave (list wave)))
	 (wave-length (loop for wave in waves maximize (length wave)))
	 (log-size (ceiling (log wave-length 2)))
	 (tables   (dft-init log-size))	; Need this below in DFT-FORWARD.
	 (n        (expt 2 log-size))
	 (x-real   (make-array n :element-type 'dft-float)) ; Default is 0.0
	 (x-imag   (make-array n :element-type 'dft-float))
	 (freq-step (/ 1.0 (* wave-length 0.001 delta-t))) ; hz
	 (labels (or labels (loop for wave in waves collect nil)))
	 (imaginary-waves '())
	 (true-mags '()) (true-reals '()) (true-imags '())
	 (true-phases '())
	 (true-waves '()))
    (declare (fixnum log-size n)
	     (type (dft-array (*)) x-real x-imag))
    (when (> log-size 16) (sim-error (format nil "DFT can only handle log2(N) <= 16 (here exponent is ~D)" log-size)))
    (loop for wave in waves do
	  (let* ((dc-offset (if remove-dc 0.0 (s-flt dc-offset)))
		 (wave (sequence-to-float-list-w-offset wave dc-offset))
		 ;; Stretch time data to fit 2^N length array
		 (stretched-wave (CONVERT-DATA-dt-LISTS wave 1.0 n t nil t))
		 (imaginary-wave (when imaginary-wave (sequence-to-float-list imaginary-wave)))
		 (stretched-imaginary-wave (when imaginary-wave (CONVERT-DATA-dt-LISTS imaginary-wave 1.0 n t nil t))))
					     
	    ;; (format t "DFT length ~A, freq step ~A, stretched-wave length ~A ~%" n freq-step (length stretched-wave))
	    ;; Transfer stretched time values to real array, and clear imag array for DFT.
	    ;; Note that the length of stretched-wave is not always exactly equal to n.
					; (format t "length stretched-imaginary-wave: ~A stretched-wave: ~A~%"
					; (length stretched-imaginary-wave) (length stretched-wave))
	    (when stretched-imaginary-wave
	      (loop for i from 0 to (1- n)
		    for val in stretched-imaginary-wave do
		    (setf (aref x-imag i) val)))
	    (loop for i from 0 to (1- n)
		  for val in stretched-wave do
		  (setf (aref x-real i) val)
		  (unless stretched-imaginary-wave (setf (aref x-imag i) 0.0)))
			     
	    (if stretched-imaginary-wave (dft-reverse x-real x-imag tables) (dft-forward x-real x-imag tables))
	    (let* ((true-real (sequence-to-list x-real))
		   (true-imag (sequence-to-list x-imag))
		   (true-mag (loop for real in true-real
				   for imag in true-imag
				   for freq-count from 0
				   unless (and (= freq-count 0) remove-dc)
				   collect 
				   (* (sqrt (+ (square real) (square imag))) (* 0.001 delta-t))))
		   (true-phase
		    (let ((phase-shift 0) phase last-imag last-real)
		      ;; (plot-timed-data (list true-real true-imag))
		      (loop for real in true-real
			    for imag in true-imag
			    for mag in true-mag
			    for freq-count from 0 by freq-step
			    unless (and (= freq-count 0) remove-dc)
			    do (setq phase
				     (if (or (= real 0) (= mag 0))
					 0.0
					 (+ (if nil ; unfold-phase
						(progn
						  (when last-real
						    (cond ((> (signum (* last-imag last-real))
							      (signum (* imag real)))
					; (format t "Phase shifting up...(freq-count ~A)~%" freq-count)
							   (setq phase-shift (+ phase-shift
										pi-single)))
							  ((< (signum (* last-imag last-real))
							      (signum (* imag real)))
					; (format t "Phase shifting down...(freq-count ~A)~%" freq-count)
							   (setq phase-shift (- phase-shift
										pi-single)))))
					; (format t "Phase shift ~A~%" phase-shift)
						  phase-shift
						  )
						0.0)
					    (if unfold-phase
						(- (mod (+ (- (atan imag real)  (* freq-count time-shift)) pi)
							(* 2 pi))
						   pi)
						(atan (/ imag real))))))
			    (setq last-real real last-imag imag)
			    and collect phase))))

	      (when normalize-mag (setq true-mag (normalize-sequence true-mag)))
	      (push true-real true-reals)
	      (push true-imag true-imags)
	      (push wave true-waves)
	      (push imaginary-wave imaginary-waves)
	      (push true-phase true-phases)
	      (push true-mag true-mags))))
    
    (let* ((max-freq (min (or max-freq (* freq-step (/ (apply 'max (mapcar 'length true-mags)) 2)))
			  true-max-freq))
	   (plot-max-freq (if log-plot-freq (log max-freq 10) max-freq))
	   (plot-min-freq (if log-plot-freq (log freq-step 10) 0.0))
	   (plot-freq-inc (/ plot-max-freq 5))
	   (delta-freq-start (if (or log-plot-freq remove-dc) freq-step 0.0))
	   (fixed-title-prefix (concatenate 'string title-prefix (if (> (length title-prefix) 0) ": " "")))
	   (true-length (- (round (/ max-freq freq-step)) (if (or log-plot-freq remove-dc) 1 0))))

      ;; Get rid of symmetric top half
      (setq true-phases (mapcar #'(lambda (list) (list-head list true-length)) true-phases)
	    true-mags (mapcar #'(lambda (list) (list-head list true-length)) true-mags))

      (cond-every
       (plot-wave
	(plot-timed-data true-waves labels nil
			 :title (concatenate 'string fixed-title-prefix "Input Real Part")
			 :delta-t delta-t :x-label data-x-label))
       (plot-imaginary-wave-in
	(plot-timed-data imaginary-waves labels nil
			 :title (concatenate 'string fixed-title-prefix "Input Imaginary Part")
			 :delta-t delta-t :x-label data-x-label))
       (plot-real-wave
	(plot-timed-data true-reals labels nil
			 :title (concatenate 'string fixed-title-prefix "Output Real Part")
			 :delta-t freq-step :delta-t-start delta-freq-start
			 ;; :y-min (unless log-plot-mag 0.0) :y-log log-plot-mag :log-base 10
			 :x-origin plot-min-freq :x-min plot-min-freq :x-log log-plot-freq
			 :x-are-fns (not log-plot-freq) :x-label txfmed-x-label :x-inc plot-freq-inc :x-max plot-max-freq))
       (plot-imag-wave
	(plot-timed-data true-imags labels nil
			 :title (concatenate 'string fixed-title-prefix "Output Imaginary Part")
			 :delta-t freq-step :delta-t-start delta-freq-start
			 ;; :y-min (unless log-plot-mag 0.0) :y-log log-plot-mag :log-base 10
			 :x-origin plot-min-freq :x-min plot-min-freq :x-log log-plot-freq
			 :x-are-fns (not log-plot-freq) :x-label txfmed-x-label :x-inc plot-freq-inc :x-max plot-max-freq))
       (plot-phase
	(plot-timed-data true-phases labels nil
			 :title (concatenate 'string fixed-title-prefix "Output Phase")
			 :log-base 10
			 :delta-t freq-step :delta-t-start delta-freq-start			 
			 :y-min (unless unfold-phase (- (/ pi-single 2))) :y-max (unless unfold-phase (/ pi-single 2))
			 :y-inc (/ pi-single 4) :y-label "Radians"
			 :x-origin plot-min-freq :x-min plot-min-freq :x-log log-plot-freq 
			 :x-are-fns (not log-plot-freq) :x-label txfmed-x-label :x-inc plot-freq-inc :x-max plot-max-freq))
       (plot-mag
	(plot-timed-data true-mags labels nil
			 :title (concatenate 'string fixed-title-prefix "Output Mag")
			 :delta-t freq-step :delta-t-start delta-freq-start
			 :log-base 10
			 :y-min (unless log-plot-mag 0.0) :y-log log-plot-mag 
			 :x-origin plot-min-freq :x-min plot-min-freq :x-log log-plot-freq
			 :x-are-fns (not log-plot-freq) :x-label txfmed-x-label :x-inc plot-freq-inc :x-max plot-max-freq))))
    (when return-waves
      (values (loop for true-wave in true-waves
		    for true-mag in true-mags
		    for true-phase in true-phases
		    for true-real in true-reals
		    for true-imag in true-imags
		    collect (list true-wave true-mag true-phase true-real true-imag))))))
				  
  



(defun element-data-dft (element &key data-type type state-index
				 (delta-t 1.0) (reference-time-list (CURRENT-SIM-PLOT-TIME-LIST))
				 (dc-offset 0.0))
  "Plot magnitude and phase of the DFT of ELEMENT data, resampled on a regular grid given by
DELTA-T [milliseconds, default 1.0]. DC-OFFSET [default 0.0] is subtracted from the data before
the DFT. Remaining arguments are as for ELEMENT-DATA-DTED. DFT processing done by DFT-STRETCH-WAVE,
which may also resample the data."
  (dft-stretch-wave (element-data-dted element delta-t data-type type reference-time-list state-index)
		    :title-prefix (format nil "~A ~A ~A" *simulation-name* (element-name element)
					  (or data-type (default-data-type element)))
		    :delta-t delta-t :dc-offset dc-offset))


(defun dft-forward-complete (data delta-t &key plot return-values title-prefix)
  (let* ((wave-length (length data))
	 (title-prefix (if (= 0 (length (when title-prefix (string title-prefix)))) "" (format nil "~A: " title-prefix)))
	 (log-size (ceiling (log wave-length 2)))
	 (tables   (dft-init log-size))	; Need this below in DFT-FORWARD.
	 (n        (expt 2 log-size))
	 (delta-t (s-flt delta-t))	; ms
	 (delta-freq (/ 1 (* wave-length 0.001 delta-t))) ; Hz
	 (max-time (* wave-length delta-t))
	 (max-freq (/ 1 (* 0.001 delta-t)))
	 (x-real   (make-array n :element-type 'dft-float))
	 (x-imag   (make-array n :element-type 'dft-float))
	 (data (sequence-to-float-list data)))
    (loop for i from 0 to (1- n)
	  for val in data do
	  (setf (aref x-real i) val)
	  (setf (aref x-imag i) 0.0))
    (dft-forward x-real x-imag tables)
    (setq x-real (sequence-to-float-list x-real)
	  x-imag (sequence-to-float-list x-imag))
    (let (mag)
      (loop for real in x-real
	    for imag in x-imag
	    do (setq mag (* 1		; (* 0.001 delta-t) ; convert to Hz, assuming that delta-t is in ms.
			    (sqrt (+ (square real) (square imag)))))
	    collect mag into mags
	    collect (if (or (= real 0) (= mag 0)) 0.0 (atan imag real)) into phases
	    finally
	    (when plot
	      (plot-timed-data (list data) nil nil :delta-t delta-t :x-label "ms"
			       :title (format nil "~ADFT Input Data" title-prefix))
	      (plot-timed-data (list (fold-wave x-real) (fold-wave x-imag))
			       '(dft-real dft-imag) nil :delta-t delta-freq
			       :delta-t-start (round (/ max-freq -2))
			       :x-axis-root 0.0 :reference-ticks-to-origin t
			       :x-label "Hz" :x-origin (round (/ max-freq -2))
			       :title (format nil "~ADFT Real and Imag Output" title-prefix))
	      (plot-timed-data (fold-wave mags) nil nil :delta-t delta-freq :delta-t-start (round (/ max-freq -2))
			       :x-label "Hz" :x-origin (round (/ max-freq -2))
			       :x-axis-root 0.0 :reference-ticks-to-origin t
			       :title (format nil "~ADFT Magnitude" title-prefix))
	      (plot-timed-data (fold-wave phases) nil nil :delta-t delta-freq :delta-t-start (round (/ max-freq -2))
			       :x-label "Hz" :x-origin (round (/ max-freq -2))
			       :y-origin (- pi-single) :y-inc (/ pi-single 2)
			       :x-axis-root 0.0 :reference-ticks-to-origin t
			       :y-min (- pi-single) :y-max pi-single
			       :title (format nil "~ADFT Phase" title-prefix)))
	    (when return-values (return (values x-real x-imag mags phases)))))))

(defun dft-reverse-complete (data-real data-imag delta-hz &key plot return-values title-prefix)
  (let* ((wave-length (length data-real))
	 (title-prefix (if (= 0 (length (when title-prefix (string title-prefix)))) "" (format nil "~A: " title-prefix)))
	 (log-size  (ceiling (log wave-length 2)))
	 (tables    (dft-init log-size)) ; Need this below in DFT-REVERSE.
	 (n         (expt 2 log-size))
	 (delta-hz  (s-flt delta-hz))	; Hz
	 (delta-t   (/ 1 (*  0.001 wave-length delta-hz))) ; ms
	 (max-time (* wave-length delta-t))
	 (max-freq (/ 1 (* 0.001 delta-t)))
	 (x-real    (make-array n :element-type 'dft-float))
	 (x-imag    (make-array n :element-type 'dft-float))
	 (data-real (sequence-to-float-list data-real))
	 (data-imag (sequence-to-float-list data-imag)))
    (loop for i from 0 to (1- n)
	  for real in data-real
	  for imag in data-imag
	  do (setf (aref x-real i) real
		   (aref x-imag i) imag))
    (dft-reverse x-real x-imag tables)
    (setq x-real (sequence-to-float-list x-real)
	  x-imag (sequence-to-float-list x-imag))
    (let (mag)
      (loop for real in x-real
	    for imag in x-imag
	    do (setq mag (* 2 pi-single
			    (sqrt (+ (square real) (square imag)))))
	    collect mag into mags
	    collect (if (or (= real 0) (= mag 0)) 0.0 (atan imag real)) into phases
	    finally
	    (when plot
	      (plot-timed-data (list (fold-wave data-real) (fold-wave data-imag)) '(data-real data-imag) nil
			       :delta-t delta-hz :delta-t-start (round (/ max-freq -2))
			       :x-label "Hz" :title (format nil "~AReverse DFT Input Data" title-prefix))
	      (plot-timed-data (list (fold-wave x-real) (fold-wave x-imag)) '(dft-real dft-imag) nil
			       :delta-t delta-t :delta-t-start (round (/ max-time -2))
			       :x-label "ms"
			       :title (format nil "~AReverse DFT Real and Imag Output" title-prefix))
;	      (plot-timed-data mags nil nil :delta-t delta-t
;                               :title (format nil "~AReverse DFT Magnitude" title-prefix)
;                               :x-label "ms")
;              (plot-timed-data phases nil nil :delta-t delta-t :title (format nil "~AReverse DFT Phase" title-prefix)
;                               :x-label "ms")
	      )
	    (when return-values (return (values x-real x-imag mags phases)))))))

(defun bode-plot-mag (magnitude delta-f start-freq title)
  (loop for freq from start-freq by delta-f
	for mag in magnitude
	when (>= freq 0)
	collect (if (= freq 0)
		    (* 0.1 delta-f)
		    freq)
	into out-frequencies
	collect mag into out-magnitudes
	finally
	(plot-timed-data out-magnitudes nil out-frequencies
			 :y-log t :log-base 10
			 :x-log t
			 :title (format nil "Bode Plot: ~A" title))))
		    
  
(defun deconvolve (data-x data-y delta-t &key cutoff-freq return-values plot-x plot-y plot-h)
  (let* ((title-prefix (if cutoff-freq (format nil "DATA-H (Cutoff ~AHz)" cutoff-freq) 'data-h))
	 (wave-length (length data-y))
	 (delta-t (s-flt delta-t))	; ms
	 (delta-freq (/ 1 (* wave-length 0.001 delta-t))) ; Hz
	 (max-freq (* delta-freq wave-length))
	 (max-time (* wave-length delta-t))
	 (data-y (sequence-to-float-list data-y))
	 (data-x (sequence-to-float-list data-x)))
    (multiple-value-bind (dft-x-real dft-x-imag dft-x-mag dft-x-phase)
	(dft-forward-complete data-x delta-t :return-values t :plot plot-x :title-prefix 'data-x)
      (multiple-value-bind (dft-y-real dft-y-imag dft-y-mag dft-y-phase)
	  (dft-forward-complete data-y delta-t :return-values t :plot plot-y :title-prefix 'data-y)
	(let (h-mag h-phase)
	  (loop for y-mag in dft-y-mag
		for x-mag in dft-x-mag
		for y-phase in dft-y-phase
		for x-phase in dft-x-phase
		for freq from 0.0 by delta-freq
		do (setq h-mag (if (or (not cutoff-freq)
				       (< freq cutoff-freq)
				       (> freq (- max-freq cutoff-freq)))
				   (/ y-mag x-mag) 0.0)
			 h-phase (- y-phase x-phase))
		collect (* h-mag (cos h-phase)) into h-reals
		collect (* h-mag (sin h-phase)) into h-imags
		collect h-mag into h-mags
		collect h-phase into h-phases
		finally
		(when plot-h
		  (plot-timed-data (fold-wave h-mags) nil nil :delta-t delta-freq
				   :delta-t-start (round (/ max-freq -2))
				   :x-label "Hz"
				   :title (format nil "~A Magnitude" title-prefix))
		  (plot-timed-data (fold-wave h-phases) nil nil :delta-t delta-freq :delta-t-start (round (/ max-freq -2))
				   :x-label "Hz"
				   :title (format nil "~A Phase" title-prefix)))
		(multiple-value-bind (dft-h-real dft-h-imag dft-h-mag dft-h-phase)
		    (dft-reverse-complete h-reals h-imags delta-freq :return-values t :plot plot-h :title-prefix title-prefix)
		  (when return-values (return (values dft-h-real dft-h-imag dft-h-mag dft-h-phase))))))))))

;; works properly for even length sequences. ie
;; * (FOLD-WAVE '(1 2 a b))
;; (A B 1 2)
;; * (FOLD-WAVE '(1 2 a b c))
;; (A B 1 2 C)
(defun fold-wave (wave)
  (let* ((wave-is-arrayp (arrayp wave))
	 (array (sequence-to-gen-array wave))
	 (wave-length (length wave))
	 (lhl (floor (/ wave-length 2))))
    (loop for i from 0 to (1- lhl)
	  do (let ((left-element (aref array i))
		   (right-element (aref array (+ lhl i))))
	       (setf (aref array i) right-element
		     (aref array (+ lhl i)) left-element)))
    (if wave-is-arrayp array (sequence-to-list array))))

