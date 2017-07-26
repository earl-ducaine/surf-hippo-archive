;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: #-PARALLEL SURF #+PARALLEL *surf; Base: 10; -*-


(defvar ds-amacrine-branch-length 40)

;(defun draw-ds-layout ()
;  (dolist (amacrine-cell-origin amacrine-cell-location-list)


(defun ds-line-test ()
  (line-cell "ds-test"))

(defun taper-line ()
  (taper-line-cell "ds-test"))

(defun taper-line-offset ()
  (setq *r-a 1.0)
  (setq *light-input-offset-distance 200 *light-input-offset-angle 0)
  (taper-line-cell "marchiafava-test" :extras-list '((synapse EXCITATORY-1)(synapse INHIBITORY-1-offset))))


(defvar *seg-diameter 0.5)

(defun taper-line-cell (cell-name &key (extras-list '((synapse EXCITATORY-1)(synapse INHIBITORY-1)) ))
  (create-cell-type "line" :soma-resistivity *r-mem-soma)
  (create-cell cell-name :cell-type-name "line")
  (let ((soma (create-soma (format nil "~a-soma" cell-name) cell-name 10)))
    (if *include-sources (create-source *clamp-type soma))
D,#TD1PsT[Begin using 006 escapes](1 0 (NIL 0) (NIL :ITALIC NIL) "CPTFONTI");; The segment-list format is as follows: (mother-segment-name segment-name x y0 z1 diameter extras-list)
0    (create-tree soma
		 `(
		   (soma 1  25 0 0  1.0    ,extras-list)
		   (1 2     50 0 0  1.0    ,extras-list)
		   (2 3     75 0 0  .8    ,extras-list)
		   (3 4     100 0 0  .8    ,extras-list)
		   (4 5     125 0 0  .6    ,extras-list)
		   (5 6     150 0 0  .6    ,extras-list)
		   (6 7     175 0 0  .4    ,extras-list)
		   (7 8     200 0 0  .4    ,extras-list)
		   (8 9     225 0 0  .2    ,extras-list)
		   (9 10    250 0 0  .2    ,extras-list)
		   (10 11   275 0 0  .2    ,extras-list)
		   (11 12   300 0 0  .2    ,extras-list)
		   (soma 1a -25 0 0  1.0    ,extras-list)
		   (1a 2a     -50 0 0  1.0    ,extras-list)
		   (2a 3a     -75 0 0  .8    ,extras-list)
		   (3a 4a     -100 0 0  .8    ,extras-list)
		   (4a 5a     -125 0 0  .6    ,extras-list)
		   (5a 6a     -150 0 0  .6    ,extras-list)
		   (6a 7a     -175 0 0  .4    ,extras-list)
		   (7a 8a     -200 0 0  .4    ,extras-list)
		   (8a 9a     -225 0 0  .2    ,extras-list)
		   (9a 10a    -250 0 0  .2    ,extras-list)
		   (10a 11a   -275 0 0  .2    ,extras-list)
		   (11a 12a   -300 0 0  .2    ,extras-list)
		   ) 
		 :synapse t)

;    (dolist (segment-name '("12" "12A"))
;      (create-a-channel (gethash segment-name segment-hash-table) cell-name :gbar-density 500 :save-particle t))
    ))

(defun line-cell (cell-name &key (extras-list  '((synapse EXCITATORY-1)(synapse INHIBITORY-1))))
  (create-cell-type "line" :soma-resistivity *r-mem-soma)
  (create-cell cell-name :cell-type-name "line")
  (let ((soma (create-soma (format nil "~a-soma" cell-name) cell-name 10)))
    (if *include-sources (create-source *clamp-type soma))
1;; The segment-list format is as follows: (mother-segment-name segment-name x y0 z1 diameter extras-list)
0    (create-tree soma
		 `(		   (soma 1a -25 0 0  nil    ,extras-list)
		   (soma 1  25 0 0  nil    ,extras-list)
		   (1 2     50 0 0  nil    ,extras-list)
		   (2 3     75 0 0  nil    ,extras-list)
		   (3 4     100 0 0  nil    ,extras-list)
		   (4 5     125 0 0  nil    ,extras-list)
		   (5 6     150 0 0  nil    ,extras-list)
		   (6 7     175 0 0  nil    ,extras-list)
		   (7 8     200 0 0  nil    ,extras-list)
		   (8 9     225 0 0  nil    ,extras-list)
		   (9 10    250 0 0  nil    ,extras-list)
		   (10 11   275 0 0  nil    ,extras-list)
		   (11 12   300 0 0  nil    ,extras-list)
		   (1a 2a     -50 0 0  nil    ,extras-list)
		   (2a 3a     -75 0 0  nil    ,extras-list)
		   (3a 4a     -100 0 0  nil    ,extras-list)
		   (4a 5a     -125 0 0  nil    ,extras-list)
		   (5a 6a     -150 0 0  nil    ,extras-list)
		   (6a 7a     -175 0 0  nil    ,extras-list)
		   (7a 8a     -200 0 0  nil    ,extras-list)
		   (8a 9a     -225 0 0  nil    ,extras-list)
		   (9a 10a    -250 0 0  nil    ,extras-list)
		   (10a 11a   -275 0 0  nil    ,extras-list)
		   (11a 12a   -300 0 0  nil    ,extras-list)
		   ) :default-diameter *seg-diameter
		 :synapse t)))

(defun extract-tuning (cap-index g-in-index functional-index data-array)
  (let ((array (make-array 6)))
    (loop for i from 0 to 5 do
      (setf (aref array i) (aref data-array i cap-index 0 0 g-in-index functional-index)))
    array))
  
(defun line-runs ()
  (setq	*r-mem 2.0e5 *r-a 200 *r-mem-soma 2.0e5
	*g-excitatory-1-dens  10 *g-inhibitory-1-dens 10)
  (line-run)
  (setq	*r-mem 2.0e5 *r-a 200 *r-mem-soma 2.0e5
	*g-excitatory-1-dens  10 *g-inhibitory-1-dens 1)
  (line-run)
  (setq	*r-mem 2.0e5 *r-a 200 *r-mem-soma 2.0e5
	*g-excitatory-1-dens  10 *g-inhibitory-1-dens .1)
  (line-run)
  (setq *r-mem 5.0e5 *r-a 200 *r-mem-soma 5.0e5
	*g-excitatory-1-dens  10 *g-inhibitory-1-dens 100)
  (line-run)
  (setq *r-mem 5.0e5 *r-a 200 *r-mem-soma 5.0e5
	*g-excitatory-1-dens  10 *g-inhibitory-1-dens 10)
  (line-run)
  (setq *r-mem 5.0e5 *r-a 200 *r-mem-soma 5.0e5
	*g-excitatory-1-dens  10 *g-inhibitory-1-dens 1)
  (line-run)
  (setq *r-mem 5.0e5 *r-a 200 *r-mem-soma 5.0e5
	*g-excitatory-1-dens  10 *g-inhibitory-1-dens .1)
  (line-run)
  )

;;Format is  speed-index cap-index r-mem-index g-ex-index g-in-index functionals
(defvar  *line-runs-output*)			;line-cell with g-ex = 10, e-ex = 0
(defvar  *line-runs-output2*)			;line-cell with g-ex = 1, e-ex = 50
(defvar  *line-runs-output3* nil)			;taper-line-cell with g-ex = 10, e-ex = 0
(defvar  *line-runs-output4* nil)			;taper-line-cell with g-ex = 1, e-ex = 50
(defvar  *line-runs-output5* nil)			;taper-line-cell with g-ex = 1, e-ex = 50, inhibition
						;offset, r-a = 1.0


;(defvar *line-runs-frame*
;	(tv:make-window
;	  'plot-frame
;	  ':panes '((di11 plot3d::plot3d-window :tmat (identity-matrix) :scale-x 1.0 :scale-y 1.0 :scale-z 1.0
;			  :eye (list 100.0 100.0 100.0)
;			  :zoom 1.0 :plot-inc 1 :autoscale-switch t :hidden-line-switch t :perspective-switch t
;			  :three-point-switch t
;			  :ask-for-variables-switch nil :leave-window-displayed-switch nil :x-size 5 :y-size 6
;			  :vector (make-array '(1 4)) :result-vector (make-array '(1 4)) :save-bits t :activate-p t
;			  :process (unless no-process-p '(plot3d-window-main-process)) :expose-p nil
;			  )
;		    (di12 plot3d::plot3d-window :tmat (identity-matrix) :scale-x 1.0 :scale-y 1.0 :scale-z 1.0
;			  :eye (list 100.0 100.0 100.0)
;			  :zoom 1.0 :plot-inc 1 :autoscale-switch t :hidden-line-switch t :perspective-switch t
;			  :three-point-switch t
;			  :ask-for-variables-switch nil :leave-window-displayed-switch nil :x-size 5 :y-size 6
;			  :vector (make-array '(1 4)) :result-vector (make-array '(1 4)) :save-bits t :activate-p t
;			  :process (unless no-process-p '(plot3d-window-main-process)) :expose-p nil
;			  )
;		    (di13 plot3d::plot3d-window :tmat (identity-matrix) :scale-x 1.0 :scale-y 1.0 :scale-z 1.0
;			  :eye (list 100.0 100.0 100.0)
;			  :zoom 1.0 :plot-inc 1 :autoscale-switch t :hidden-line-switch t :perspective-switch t
;			  :three-point-switch t
;			  :ask-for-variables-switch nil :leave-window-displayed-switch nil :x-size 5 :y-size 6
;			  :vector (make-array '(1 4)) :result-vector (make-array '(1 4)) :save-bits t :activate-p t
;			  :process (unless no-process-p '(plot3d-window-main-process)) :expose-p nil
;			  )
;		    (di21 plot3d::plot3d-window :tmat (identity-matrix) :scale-x 1.0 :scale-y 1.0 :scale-z 1.0
;			  :eye (list 100.0 100.0 100.0)
;			  :zoom 1.0 :plot-inc 1 :autoscale-switch t :hidden-line-switch t :perspective-switch t
;			  :three-point-switch t
;			  :ask-for-variables-switch nil :leave-window-displayed-switch nil :x-size 5 :y-size 6
;			  :vector (make-array '(1 4)) :result-vector (make-array '(1 4)) :save-bits t :activate-p t
;			  :process (unless no-process-p '(plot3d-window-main-process)) :expose-p nil
;			  )
;		    (di22 plot3d::plot3d-window :tmat (identity-matrix) :scale-x 1.0 :scale-y 1.0 :scale-z 1.0
;			  :eye (list 100.0 100.0 100.0)
;			  :zoom 1.0 :plot-inc 1 :autoscale-switch t :hidden-line-switch t :perspective-switch t
;			  :three-point-switch t
;			  :ask-for-variables-switch nil :leave-window-displayed-switch nil :x-size 5 :y-size 6
;			  :vector (make-array '(1 4)) :result-vector (make-array '(1 4)) :save-bits t :activate-p t
;			  :process (unless no-process-p '(plot3d-window-main-process)) :expose-p nil
;			  )
;		    (di23 plot3d::plot3d-window :tmat (identity-matrix) :scale-x 1.0 :scale-y 1.0 :scale-z 1.0
;			  :eye (list 100.0 100.0 100.0)
;			  :zoom 1.0 :plot-inc 1 :autoscale-switch t :hidden-line-switch t :perspective-switch t
;			  :three-point-switch t
;			  :ask-for-variables-switch nil :leave-window-displayed-switch nil :x-size 5 :y-size 6
;			  :vector (make-array '(1 4)) :result-vector (make-array '(1 4)) :save-bits t :activate-p t
;			  :process (unless no-process-p '(plot3d-window-main-process)) :expose-p nil
;			  ))
;	  ':configurations '((c1
;			       (:layout
;				 (c1 :column r1 r2)
;				 (r1 :row di11 di12 di13)
;				 (r2 :row di21 di22 di23))
;			       (:sizes		
;				 (c1  (r1 0.5)(r2 0.5))
;				 (r1 (di11 0.333333)(di12 0.333333)(di13 0.333333))
;				 (r2 (di21 0.333333)(di22 0.333333)(di23 0.333333)))))))



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
  (let* ((filename (string-downcase
		     (concatenate 'string sun-mathematica-path "/"
				  (string name)  ".m")))
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
    (format t "File ~a written~%" filename)))

(defvar *line-runs-output6* nil)

(defvar array-runs-parameters '((0.5 1.0 2.0 4.0 8.0 16)	;Velocity
				(1.0 0.00001)	;Capacitance
				(100e3 )	;R-mem
				(1)		;g-ex-dens
				(100 10 1 0.1 0)	;g-in-dens
				))

  (setq array-runs-parameters '((0.5 1.0 2.0 4.0 8.0 16)	;Velocity
				(1.0 0.0001)	;Capacitance
				(300e3 )	;R-mem
				(10)		;g-ex-dens
				(100 10 1 0.1 0)	;g-in-dens
				)
	*e-rev-excitatory-1 0)


(defun taper-array-runs ()
  (setq array-runs-parameters '((4.0)	;Velocity
				(1.0 0.00001)	;Capacitance
				(100e3 )	;R-mem
				(1)		;g-ex-dens
				(100 0)	;g-in-dens
;				(100 10 1 0.1 0)	;g-in-dens
				))
  (array-runs array-runs-parameters  *line-runs-output3* 'taper-line )	;taper-line-cell with g-ex = 10, e-ex = 0
;  (write-mathematica-line-runs-output *line-runs-output3* 'taper-1 "taper-line-cell with g-ex = 10, e-ex = 0")
;  (setq array-runs-parameters '((0.5 1.0 2.0 4.0 8.0 16)	;Velocity
;				(1.0 0.0001)	;Capacitance
;				(300e3 )	;R-mem
;				(1)		;g-ex-dens
;				(100 10 1 0.1 0)	;g-in-dens
;				)
;	*e-rev-excitatory-1 50)
;  (array-runs array-runs-parameters  *line-runs-output4* 'taper-line)	;;taper-line-cell with g-ex = 1, e-ex = 50
;  (write-mathematica-line-runs-output *line-runs-output4* 'taper-2 "taper-line-cell with g-ex = 1, e-ex = 50")
)

(dolist (plot-pane '(plot:|Plot Hack 20| plot:|Plot Hack 21| plot:|Plot Hack 22| plot:|Plot Hack 23|
					 plot:|Plot Hack 24| plot:|Plot Hack 25| plot:|Plot Hack 26|
					 plot:|Plot Hack 27|))
    (send  (eval plot-pane) :set-label-font '(:dutch :roman :normal)))


(defun auto-urn ()
  (setq *old-plot-nodes* '("12A" "12" "ds-test-soma") *scale 1 *draw-cells t *print-analysis nil
	*automatic-run t *modify-cell-type nil *include-light-synapses t *include-synapses t *fast-rf-bar t
	*overlay-simulations nil *hard-copy-screen nil *save-simulation nil user-stop-time 1400
	*soma-shunt 1.0e30 *light-theta (/ pi 2.0) *bar-length 500 *bar-width 50  *light-stimulus 'moving-bar
	*light-direction nil *synapse-names-to-do-first '("Syn-2A-INHIBITORY-1" "Syn-2A-EXCITATORY-1"))
  (setq *light-speed 4.0)
  (setq *light-start-position-x (* *light-speed  -700) *use-old-synapse-waveforms nil)
  (setq *r-mem 100e3 *g-excitatory-1-dens 1)
	  (setq *r-mem-soma *r-mem)
  (setq *cap-mem 1.0)
  (setq *g-inhibitory-1-dens 100)
  (surf 'taper-line)

  (setq *use-old-synapse-waveforms t *overlay-simulations t)
  (setq *g-inhibitory-1-dens 00)
  (surf 'taper-line)

  (setq *cap-mem .0001)
  (setq *g-inhibitory-1-dens 100)
  (surf 'taper-line)
  (setq *g-inhibitory-1-dens 0)
  (surf 'taper-line))




  
(defun array-runs (parameters-list circuit)
  (let* ((velocity-list (nth 0 parameters-list))	(c-mem-list  (nth 1 parameters-list))
	(r-mem-list  (nth 2 parameters-list))	(g-ex-list  (nth 3 parameters-list))
	(g-in-list  (nth 4 parameters-list)) pane-list
	(data-array (make-array (list (length velocity-list) (length c-mem-list) (length r-mem-list)
				     (length g-ex-list) (length g-in-list) (+ 3 3 3)))))	;Last one is
    ;;P/N (12/12A) values and DIs for av, nl-av. The order in the array is as follows:  P-av (0), N-av (1),
    ;;P-nl-av (2), N-nl-av (3), P-amp (4), N-amp (5), DI-av (6), DI-nl-av (7), DI-amp (8).
    (loop for *light-speed in velocity-list 	;Microns per millisecond
	  for speed-index from 0 to (- (length velocity-list) 1) do
      (setq *light-start-position-x (* *light-speed  -700) *use-old-synapse-waveforms nil)
      (loop for *cap-mem in c-mem-list
	    for cap-index from 0 to (- (length c-mem-list) 1) do
	(loop for *r-mem in r-mem-list
	      for r-mem-index from 0 to (- (length r-mem-list) 1) do
	  (setq *r-mem-soma *r-mem)
	  (loop for *g-excitatory-1-dens in g-ex-list 
		for g-ex-index from 0 to (- (length g-ex-list) 1) do
	    (setq *g-excitatory-1-dens *g-excitatory-1-dens)
	    (loop for *g-inhibitory-1-dens in g-in-list 
		  for g-in-index from 0 to (- (length g-in-list) 1) do
	      (setq pane-list
		    (if (cdr pane-list) (cdr pane-list)
			(list 'plot::|Plot Hack 1| 'plot::|Plot Hack 3| 'plot::|Plot Hack 5|
			      'plot::|Plot Hack 7| 'plot::|Plot Hack 9|
			      'plot::|Plot Hack 2| 'plot::|Plot Hack 4| 'plot::|Plot Hack 6|
			      'plot::|Plot Hack 8| 'plot::|Plot Hack 10|)))
	      (setq data-array
		    (line-run
		      (car pane-list) speed-index cap-index r-mem-index g-ex-index g-in-index circuit data-array))
	      (if (not (cdr pane-list)) (surf-screen (eval 'plot::|Plot Hack 1|)))
	      (setq *use-old-synapse-waveforms t))))))
;    (loop for cap-index from 0 to (- (length c-mem-list) 1) do
;      (loop for di-index from 0 to 2 do
;	(let ((3d-window (plot3d::plot3d (fill-3d-array di-index cap-index line-3d-array data-array) nil nil)))
;	  (send 3d-window :set-label
;		(format nil "Circuit ~a DI of ~a, Cm = ~a, Gex = ~a, Eex = ~a mV " circuit
;			(case di-index (0 'Average) (1 'Sigmoid-Average)(2 'Amplitude))
;		(case cap-index (0 '1uF/cm) (1 '0.00001uF/cm))
;			*g-excitatory-1-dens *e-rev-excitatory-1))
;	  (surf-screen 3d-window))))
    (loop for g-in-index from 0 to (- (length g-in-list) 1) do
      (plot-line-runs g-in-index data-array *g-excitatory-1-dens *e-rev-excitatory-1 circuit)
      (surf-screen (eval 'plot::|Plot Hack 11|)))
    data-array))

 
(defun line-run (plot-pane speed-index cap-index r-mem-index g-ex-index g-in-index circuit data-array)
  (setq *old-plot-nodes* '("12A" "12" "ds-test-soma") *scale 1 *draw-cells t *print-analysis nil
	*automatic-run t *modify-cell-type nil *include-light-synapses t *include-synapses t *fast-rf-bar t
	*overlay-simulations nil *hard-copy-screen nil *save-simulation nil user-stop-time 1400
	*soma-shunt 1.0e30 *light-theta (/ pi 2.0) *bar-length 500 *bar-width 50  *light-stimulus 'moving-bar
	*light-direction nil *synapse-names-to-do-first '("Syn-2A-INHIBITORY-1" "Syn-2A-EXCITATORY-1"))
  (surf circuit) 
  (let ((amplitude1 0)(nl-integral1 0)(integral1 0)(amplitude2 0)(nl-integral2 0)(integral2 0)
	di-av di-nl-av di-amp)
    (do ((lt  *ordered-time-list* (cdr lt))
	 (lv1 (reverse (node-voltage-data (gethash "12A" node-hash-table))) (cdr lv1))	;Null
	 (lv2 (reverse (node-voltage-data (gethash "12" node-hash-table))) (cdr lv2)))	;Pref
	((not (cdr lt)))
      (setq nl-integral1 (+ nl-integral1 (* (sigmoid (car lv1)) (- (cadr lt) (car lt))))	;Null
	    integral1 (+ integral1 (* (- (car lv1) -70) (- (cadr lt) (car lt))))
	    amplitude1 (if (> (- (car lv1) -70) amplitude1) (- (car lv1) -70) amplitude1)
	    nl-integral2 (+ nl-integral2 (* (sigmoid (car lv2)) (- (cadr lt) (car lt))))	;Pref
	    integral2 (+ integral2 (* (- (car lv2) -70) (- (cadr lt) (car lt))))
	    amplitude2 (if (> (- (car lv2) -70) amplitude2) (- (car lv2) -70) amplitude2)))
    (setq di-av (/ (- integral2 integral1)(+ integral2 integral1))
	  di-nl-av (/ (- nl-integral2 nl-integral1)(+ nl-integral2 nl-integral1))
	  di-amp (/ (- amplitude2 amplitude1)(+ amplitude2 amplitude1)))
    (send (eval plot-pane) :expose) (send (eval plot-pane) :select)
    (send  (eval plot-pane) :set-label-font '(:dutch :roman :normal))
    (send (eval plot-pane)
	  :plot
;	  (format nil
;			  (concatenate 'string
;				       "Spd ~a uM/ms, Cap ~a uF/cm2, Rm ~,1e ohm-cm2, Gex ~a, Gin ~a~%"
;				       "DI(av)=~,2e, DI(nlav)=~,2e, DI(amp)=~,2e ~a")
;			  *light-speed *cap-mem *r-mem *g-excitatory-1-dens *g-inhibitory-1-dens di-av di-nl-av di-amp
;			  circuit)
	  (format nil "Inhibitory Synaptic Conductance Density ~a pS/uM2~%" *g-inhibitory-1-dens)
	  *pane2-data-list
nil						;	  (list "Leftward Motion" "Rightward Motion")
;	  (list  (format nil "12A:Avg=~,1e,NL-Avg=~,1e,Amp=~,1f"
;			 (/ integral1 user-stop-time) (/ nl-integral1 user-stop-time) amplitude1)
;		 (format nil "12:Avg=~,1e,NL-Avg=~,1e,Amp=~,1f"
;			 (/ integral2 user-stop-time) (/ nl-integral2 user-stop-time) amplitude2))
	  :y-label "mV" :x-label "mS" :overlay nil :leave-window nil  :y-min -75 )
 (loop for value in (list integral2 integral1  nl-integral2 nl-integral1 amplitude2 amplitude1 di-av di-nl-av
			     di-amp)
	  for i from 0 to 8 do
      (setf (aref data-array
		  speed-index cap-index r-mem-index g-ex-index g-in-index i) value)))
  data-array)
	;P/N (12/12A) values for av, nl-av, and amp plus
						;the DI for av, nl-av, and amp. The order in the array is as
						;follows:  P-av (0), N-av (1), P-nl-av (2), N-nl-av (3), P-amp
						;(4), N-amp (5), DI-av (6), DI-nl-av (7), DI-amp (8).

(defun plot-line-runs (g-in-index data-array gex eex circuit)
  (send (eval 'plot::|Plot Hack 11|) :select)
  (send (eval 'plot::|Plot Hack 11|)
	:plot (format nil "~a Rm ~,1e ohm-cm2, Syn dens: Gex ~a, E-ex ~a, Gin ~a~%"
		      circuit 3.0e5 gex eex  (case g-in-index (0 100)(1 10)(2 1)(3 0.1)(4 0)))
	(loop for functional-index from 6 to 8
	      nconc
		(loop for cap-index from 0 to 1
		      collect
			(list (extract-tuning cap-index g-in-index functional-index data-array)
			      '(0.5 1 2 4 8 16))))
	(loop for functional-index from 0 to 2
	      nconc
		(loop for cap-index from 0 to 1
		      collect
			(format nil "~a, ~a"
				(case functional-index (0 'Average) (1 'Sigmoid-Average)(2 'Amplitude))
				(case cap-index (0 '1uF/cm) (1 '0.0001uF/cm)))))
	:y-label "DI Index" :x-label "Velocity (uM/mS)" :y-min -1 :y-max 1))



(defun test-cell-tree ()       
  (create-cell-type "test-tree" :soma-resistivity 30000 :membrane-resistivity 60000)
  (create-cell "test" :cell-type-name  "test-tree")
  (let ((soma (create-soma "soma" "test" 5)))
    (create-source *clamp-type soma)

    (create-synapse-type soma 'excitatory-facilitation)
    (create-synapse-type soma 'inhibitory-1-offset)
    (create-segment "1a" "soma" "test"
		    '((length . 100) (diameter . 2)(theta . 0.0)))


    (create-segment "2a" "1a" "test"
		    '((length . 100) (diameter . 2)))
    (create-synapse-type 
      (create-segment "3a" "2a" "test"
		      '((length . 100) (diameter . 2)))
      'excitatory-facilitation)

    (create-segment "1b" "1a" "test"
		    '((length . 100) (diameter . 2)(theta . 1.0)))
    (create-segment "2b" "1b" "test"
		    '((length . 100) (diameter . 2)))
    (create-segment "3b" "2b" "test"
		    '((length . 100) (diameter . 2)))))


(defun test-cell ()
  (let* ((cell-name 'test-cell))
    (create-cell-type 'test :soma-resistivity 10e3)
    (create-cell cell-name :cell-type-name 'test)
    (let ((soma (create-soma 'soma cell-name 40)))
      (create-source *clamp-type soma)
      (create-segment 'seg-1 'soma 'test-cell
		      '((length . 100) (diameter . 10))))))


  Particle soma-na3-h at soma-na3-h, cell marchiafava-cell : z -30.0 gamma 0.83 alpha-0 0.0023333 v-half -20 tau-0 3.0 Q10 6.17e-1 Initial value 1.0
  Particle soma-na3-m at soma-na3-m, cell marchiafava-cell : z 18 gamma 0.5 alpha-0 0.6667 v-half -20 tau-0 0.4 Q10 6.17e-1 Initial value 7.7010933e-16
  Particle soma-na1-h at soma-na1-h, cell marchiafava-cell : z -10 gamma 0.5 alpha-0 5.0e-4 v-half -50 tau-0 0.5 Q10 6.17e-1 Initial value 0.9995622
  Particle soma-na1-m at soma-na1-m, cell marchiafava-cell : z 15 gamma 0.5 alpha-0 0.3 v-half -53 tau-0 0.5 Q10 6.17e-1 Initial value 5.221964e-5




 (progn (send pref-pane :select)  
 (send pref-pane
   :plot "Preferred Direction Responses" 
  (list (list pre-dep pre-dep-time) (list pre-cont pre-cont-time) (list pre-hyp pre-hyp-time)) 
	(list "+20pA" "Control" "-20pA")
 :label-character-style '(:dutch :bold :normal)
 :title-character-style '(:dutch :bold :normal)
 :x-label "milliseconds" :y-label "mV")
 (send null-pane
   :plot "Null Direction Responses" 
  (list (list nulldep nulldep-time) (list nullcont nullcont-time) (list nullhyp nullhyp-time)) 
	(list "+20pA" "Control" "-20pA")
 :label-character-style '(:dutch :bold :normal)
 :title-character-style '(:dutch :bold :normal)
 :x-label "milliseconds" :y-label "mV"))


(defun auto-mar ()
  (setq *ds t)
  (setq *include-sources t)
  (setq *old-pulse-lists* '((("soma-istim" . ISOURCE) (100 1100 -0.2))))
  (surf 'marchiafava-cell t)
  (setq pre-hyp (nth 0 (car *pane1-data-list )) pre-hyp-time (nth 1 (car *pane1-data-list )) )
  (setq *old-pulse-lists* '((("soma-istim" . ISOURCE) (100 1100 0.2))))
  (surf 'marchiafava-cell t)
  (setq pre-dep (nth 0 (car *pane1-data-list )) pre-dep-time (nth 1 (car *pane1-data-list )) )
  (setq *include-sources nil)
  (surf 'marchiafava-cell t)
  (setq pre-cont (nth 0 (car *pane1-data-list )) pre-cont-time (nth 1 (car *pane1-data-list )))

  (setq *ds nil)
  (setq *include-sources t)
  (setq *old-pulse-lists* '((("soma-istim" . ISOURCE) (100 1100 -0.2))))
  (surf 'marchiafava-cell t)
  (setq null-hyp (nth 0 (car *pane1-data-list )) null-hyp-time (nth 1 (car *pane1-data-list )) )
  (setq *old-pulse-lists* '((("soma-istim" . ISOURCE) (100 1100 0.2))))
  (surf 'marchiafava-cell t)
  (setq null-dep (nth 0 (car *pane1-data-list )) null-dep-time (nth 1 (car *pane1-data-list )) )
  (setq *include-sources nil)
  (surf 'marchiafava-cell t)
  (setq null-cont (nth 0 (car *pane1-data-list )) null-cont-time (nth 1 (car *pane1-data-list )))
)

(defvars null-cont null-cont-time  null-dep null-dep-time  null-hyp null-hyp-time
 pre-cont pre-cont-time  pre-dep pre-dep-time  pre-hyp pre-hyp-time)
(defvar *ds t)

(defun marchiafava-cell ()
  (setq *valence-drx 12      *gamma-drx 0.6      *base-rate-drx 0.01      *v-half-drx -28.0      *base-txdr 0.5
	*qten-factor-at-30 1.3903892)
  (setq *valence-dry 9      *gamma-dry .2 *base-rate-dry 4.0e-4 *v-half-dry -45.0 *base-tydr 6.0 *qten-factor-at-30 1.3903892
	*g-dr-dens  2  *e-dr  -73.5)

  (setq *valence-m1 20 *gamma-m1 0.5 *base-rate-m1 0.3 *v-half-m1 -57 *base-tm1 0.005 *qten-factor-at-24 0.61703384
	*valence-h1 30 *gamma-h1 0.2 *base-rate-h1 3.0e-3 *v-half-h1 -64 *base-th1 0.005 *qten-factor-at-24 0.61703384
	*g-na1-dens 50  *e-na 50)
  (setq *valence-m3 25 *gamma-m3 0.5 *base-rate-m3 0.6667 *v-half-m3 -24 *base-tm3 0.004 *qten-factor-at-24 0.61703384
	*valence-h3 30.0 *gamma-h3 0.17 *base-rate-h3 0.0023333 *v-half-h3 -28 *base-th3 0.003 *qten-factor-at-24 0.61703384
	*g-na3-dens 40 *e-na 50)
;  (setq *valence-m1 15 *gamma-m1 0.5 *base-rate-m1 0.3 *v-half-m1 -40 *base-tm1 0.5 *qten-factor-at-24 0.61703384
;	*valence-h1 10 *gamma-h1 0.5 *base-rate-h1 5.0e-4 *v-half-h1 -53 *base-th1 0.5 *qten-factor-at-24 0.61703384
;	*g-na1-dens 20  *e-na 50)
;  (setq *valence-m3 18 *gamma-m3 0.5 *base-rate-m3 0.6667 *v-half-m3 -20 *base-tm3 0.4 *qten-factor-at-24 0.61703384
;	*valence-h3 30.0 *gamma-h3 (- 1 0.83) *base-rate-h3 0.0023333 *v-half-h3 -24 *base-th3 3.0 *qten-factor-at-24 0.61703384
;	*g-na3-dens 40 *e-na 50)
  (let* ((cell-name "marchiafava"))
    (create-cell-type "lumped-ganglion" :soma-resistivity 10e3)
    (create-cell cell-name :cell-type-name "lumped-ganglion")
    (let ((soma (create-soma "soma" cell-name 40)))
      (setq *include-synapses t)
      (if *ds
	  (create-synapse
	    (format nil "~a-CONT-EX-DS" (soma-name soma))
	    cell-name soma 'CONT-EX-DS  :parameters (list *ex-ds-tau *ex-ds-delay)))
      (create-synapse (format nil "~a-CONT-IN" (soma-name soma)) cell-name soma 'CONT-IN
		      :parameters (list *in-tau *in-delay))
      (create-channels '(na1 na3 dr a) soma :plot-pane 3)
      (if *include-sources (create-source *clamp-type soma)))))


; Marchiafava Cell channel parameters:

; dr
; 3 x particles (list "x" *valence-drx *gamma-drx *base-rate-drx *v-half-drx *base-txdr *qten-factor-at-30)
;("x" 12 0.6 0.01 -28.0 0.5 1.3903892)

; 1 y particle (list "y" *valence-dry *gamma-dry *base-rate-dry *v-half-dry *base-tydr *qten-factor-at-30)
;("y" 9 .2 4.0e-4 -45.0 6.0 1.3903892)
 
; *g-dr-dens = 2
;  *e-dr = -73.5

;na1
; 1 (list "m" *valence-m1 *gamma-m1 *base-rate-m1 *v-half-m1 *base-tm1 *qten-factor-at-24)
;("m" 15 0.5 0.3 -53 0.5 0.61703384)
; 2 (list "h" *valence-h1 *gamma-h1 *base-rate-h1 *v-half-h1 *base-th1 *qten-factor-at-24)
; ("h" 10 0.5 5.0e-4 -40 0.5 0.61703384)
;  *g-na1-dens = 20, *e-na = 50


;na3
; 2 (list "m" *valence-m3 *gamma-m3 *base-rate-m3 *v-half-m3 *base-tm3 *qten-factor-at-24)
;("m" 18 0.5 0.6667 -24 0.4 0.61703384)
; 3 (list "h" (- *valence-h3) (- 1.0 *gamma-h3) *base-rate-h3 *v-half-h3 *base-th3 *qten-factor-at-24)
;("h" -30.0 0.83 0.0023333 -20 3.0 0.61703384)
;  *g-na3-dens = 40, *e-na = 50





(defun ds-ganglion (cell-name &key (cell-origin '(0 0 0)))
  (let* ((cell (create-cell cell-name :cell-type-name "ganglion" :cell-origin cell-origin))
	 (soma (create-soma cell-name 20 :g-shunt 0)))
    (create-channels '(na1 na2 na3 dr a) soma :plot-pane 3)
    (create-isource (format nil "~a-soma-stim" cell-name)  "Ground" (node-name (soma-node soma))
		    "ipwl" cell-name (get-source-values "soma-stim" `isource))
    (create-segment (format nil "~a-prox-~a" cell-name 0)	
		    (soma-name (cell-soma (gethash cell-name cell-hash-table)))
		    cell-name (list '(length . 20) '(diameter . 3.0) (cons 'phi (* -0.5 pi))))
    (create-segment (format nil "~a-prox-~a" cell-name 1) (format nil "~a-prox-~a" cell-name 0) 
		    cell-name (list '(length . 20) '(diameter . 2.0) '(phi . 0)) )
    (dotimes (i 5)
      (create-synapse-type
	(create-segment (format nil "~a-~a-0" cell-name i) (format nil "~a-prox-~a" cell-name 1) cell-name 
			(list '(length . 20) '(diameter . 1.0) (cons 'theta (* i 0.4 pi)) (cons 'phi (* 0.25 pi))))
	'V-1)
      (create-synapse-type
	(create-segment (format nil "~a-~a-1" cell-name i) (format nil "~a-~a-0" cell-name i) cell-name 
			(list '(length . 20) '(diameter . 1.0)))
	'V-1)
      (dotimes (j 4)
        (create-synapse-type
	  (create-segment (format nil "~a-~a-~a-0" cell-name i j) (format nil "~a-~a-1" cell-name i)
			  cell-name (list '(length . 20) '(diameter . 1.0) (cons 'theta (* i 0.4 pi))))
	  'V-1)					
        (create-synapse-type
	  (create-segment (format nil "~a-~a-~a-1" cell-name i j) (format nil "~a-~a-~a-0" cell-name i j)	
			  cell-name (list '(length . 20) '(diameter . 1.0)))
	  'V-1)))
    cell))






(defun ds-amacrine (cell-name &key (cell-origin '(0 0 0)) (cell-theta 0) (include-isource nil))	
  (let* ((cell (create-cell cell-name :cell-type-name "ds-amacrine" :cell-origin cell-origin))
	 (soma (create-soma cell-name 20 :g-shunt 0)))
    (if include-isource
	(create-isource (format nil "~a-soma-stim" cell-name) "Ground" (node-name (soma-node soma))
			"ipwl" cell-name  (get-source-values (format nil "~a-soma-stim" cell-name)  `isource)))
    (dotimes (i 6)
      (create-synapse-type
	(create-segment (format nil "~a-~a-0" cell-name i) (soma-name (cell-soma (gethash cell-name cell-hash-table)))
			cell-name 
			(list '(length . ds-amacrine-branch-length)
			      '(diameter . 1.5) (cons 'theta (* i 0.6667 pi)) (cons 'phi (* 0.25 pi))))
	'EX-3)
      (create-synapse-type
	(create-segment (format nil "~a-~a-1" cell-name i)	
			(format nil "~a-~a-0" cell-name i) 
			cell-name 
			(list '(length . ds-amacrine-branch-length) '(diameter . 1.0)(cons 'phi (* 0.25 pi))))
	'EX-3)
      (let ((2nd-angle (* -0.15 pi)))
	(dotimes (j 3)
	  (setq 2nd-angle (+ 2nd-angle (* 0.15 pi)))
	  (create-synapse-type
	    (create-segment (format nil "~a-~a-1-~a-0" cell-name i j)	
			    (format nil "~a-~a-1" cell-name i) cell-name 
			    (list '(length . ds-amacrine-branch-length) '(diameter . 0.75) (cons 'theta 2nd-angle)))
	    'EX-3)
	  (create-synapse-type
	    (create-segment (format nil "~a-~a-1-~a-1" cell-name i j)
			    (format nil "~a-~a-1-~a-0" cell-name i j) cell-name
			    (list '(length . ds-amacrine-branch-length) '(diameter . 0.50)))
	    'EX-3)
	  (create-synapse-type
	    (create-segment (format nil "~a-~a-1-~a-2" cell-name i j)
			    (format nil "~a-~a-1-~a-1" cell-name i j)
			    (format nil "~a-~a-1-~a-2" cell-name i j)
			    (list '(length . ds-amacrine-branch-length) '(diameter . 0.50)))
	    'IN-3))))
    cell))




(defun 32211-amacrine-test ()       
  (let ((cell-name "ac-cable"))
    (create-cell-type "star-amacrine")
    (create-cell cell-name :cell-type-name "star-amacrine")
    (let ((soma (create-soma (format nil "~a-soma" cell-name) cell-name 16)))
      (if *include-sources (create-source *clamp-type soma))
      (create-tree soma
1;; The segment-list format is as follows: (mother-segment-name segment-name x y0 z1 diameter extras-list)
0		   `(
		     (soma		3		0		2  -5       1.2)
		     (3		32		6		3  -10         0.9)
;				((synapse EX-3)))
		     (32		322a		12		2  -10   0.6)
;					((synapse EX-3)))
		     (322a	322b		18		2  -10   0.6)
;				((synapse EX-3)))
		     (322b	3221		        23		3  -10 nil)
;				((synapse EX-3)))
		     (3221		32211a		27		4  -10 nil)
;					((synapse EX-3)))
		     (32211a		32211b		31		5  -10 nil )
;					((synapse EX-3)	; (synapse IN-3)

		     (32211b		32211c		35		5  -10 nil
;					((synapse EX-3)(synapse IN-3))
					)
		     (3221		32212		33		0  -10 nil
;					((synapse EX-3)(synapse IN-3))
					)
		     (322b		3222		26		-1  -10 nil
;					((synapse EX-3)(synapse IN-3))
					)
		     (soma		L3		0		2  -5       1.2)
		     (L3		L32		-6		3  -10         0.9
;					((synapse EX-3))
					)
		     (L32		L322a		-12		2  -10   0.6
;					((synapse EX-3))
					)
		     (L322a	L322b		-18		2  -10   0.6
;				((synapse EX-3))
				)
		     (L322b	L3221		        -23		3  -10 nil
;				((synapse EX-3))
				)
		     (L3221		L32211a		-27		4  -10 nil
;					((synapse EX-3))
					)
		     (L32211a		L32211b		-31		5  -10 nil 
;					((synapse EX-3)	;(synapse IN-3)
					)
		     (L32211b		L32211c		-35		5  -10 nil
;					((synapse EX-3)(synapse IN-3))
					)
		     (L3221		L32212		-33		0  -10 nil
;					((synapse EX-3)(synapse IN-3))
					)
		     (L322b		L3222		-26		-1  -10 nil
;(synapse IN-3)
					))
		   :default-diameter 0.2 :synapse t :xy-factor 6.06) )))

(defun amacrine-arb-ex-in (excitatory-location-name-list inhibitory-location-name-list )       
  (let ((cell-name "ac-cable"))
    (create-cell-type "star-amacrine")
    (create-cell cell-name :cell-type-name "star-amacrine")
    (let ((soma (create-soma (format nil "~a-soma" cell-name) cell-name 16)))
      (create-tree soma
		   `((soma		3		0		2  -5       1.2)
		     (3		32		6		3  -10         0.9)
		     (32		322a		12		2  -10   0.6)
		     (322a	322b		18		2  -10   0.6)
		     (322b	3221		        23		3  -10 nil)
		     (3221		32211a		27		4  -10 nil)
		     (32211a		32211b		31		5  -10 nil )
		     (32211b		32211c		35		5  -10 nil)
		     (3221		32212		33		0  -10 nil)
		     (322b		3222		26		-1  -10 nil)
		     (soma		L3		0		2  -5       1.2)
		     (L3		L32		-6		3  -10         0.9)
		     (L32		L322a		-12		2  -10   0.6)
		     (L322a	L322b		-18		2  -10   0.6)
		     (L322b	L3221		        -23		3  -10 nil)
		     (L3221		L32211a		-27		4  -10 nil)
		     (L32211a		L32211b		-31		5  -10 nil )
		     (L32211b		L32211c		-35		5  -10 nil)
		     (L3221		L32212		-33		0  -10 nil)
		     (L322b		L3222		-26		-1  -10 nil))
		   :default-diameter 0.2 :synapse t :xy-factor 6.06)

      (create-synapses-from-name-list inhibitory-location-name-list 'CONT-IN)
      (create-synapses-from-name-list excitatory-location-name-list 'CONT-EX))))


;for arvo, change lettering on histology, add labels/title to plots.
(defun auto-run ()
  (setq *old-plot-nodes* '("star-1-soma" "32211"))
  (setq user-max-step 1 *overlay-simulations nil   *automatic-run  nil *modify-cell-type nil)
  (setq *hines-max-error 0.01)			;these are reasonable 
; (nth 0)) ;p 
; (nth 1)) ;b
; (nth 2)) ;a
; (nth 3)) ;n
  (setq *bar-a-start-time 50	*bar-a-stop-time 250
	*bar-b-start-time 200	*bar-b-stop-time 400)
    (surf 'star-amacrine-1)
  (setq *overlay-simulations t *automatic-run t)
  (setq *bar-a-start-time 50	*bar-a-stop-time 51
	*bar-b-start-time 200	*bar-b-stop-time 400)
    (surf 'star-amacrine-1)
  (setq *bar-a-start-time 50	*bar-a-stop-time 250
	*bar-b-start-time 200	*bar-b-stop-time 201)
    (surf 'star-amacrine-1)
  (setq *bar-a-start-time 200	*bar-a-stop-time  400
	*bar-b-start-time 50	*bar-b-stop-time 250)
    (surf 'star-amacrine-1)
)


(dolist (plot-pane '(plot:|Plot Hack 20| plot:|Plot Hack 21| plot:|Plot Hack 22| plot:|Plot Hack 23|
					 plot:|Plot Hack 24| plot:|Plot Hack 25| plot:|Plot Hack 26|
					 plot:|Plot Hack 27|))
    (send  (eval plot-pane) :set-label-font '(:dutch :roman :normal)))


(setq list (let ((list '())) (dotimes (v 100)
			       (setq list (nconc list (list (sigmoid (- v 100) -65 1)))))))




 (list (mapcar 'sigmoid (car (cdr (nth 5 (nth 0 (cdar (symbol-value-in-instance voltage-pane2 'plot:zoom-list))))))) ;p
      (cadr (cdr (nth 5 (nth 0 (cdar (symbol-value-in-instance voltage-pane2 'plot:zoom-list)))))))
 (list (mapcar 'sigmoid (car (cdr (nth 4 (nth 0 (cdar (symbol-value-in-instance voltage-pane2 'plot:zoom-list))))))) ;n
      (cadr (cdr (nth 4 (nth 0 (cdar (symbol-value-in-instance voltage-pane2 'plot:zoom-list)))))))

(cdr (nth 4 (nth 0 (cdar (symbol-value-in-instance voltage-pane2 'plot:zoom-list))))) ;nn cm 0.0001, gin 100 

(cdr (nth 7 (nth 0 (cdar (symbol-value-in-instance voltage-pane2 'plot:zoom-list))))) ;p
(cdr (nth 6 (nth 0 (cdar (symbol-value-in-instance voltage-pane2 'plot:zoom-list))))) ;nn cm 0.0001, gin 0


(progn  (send plot:|Plot Hack 20| :expose) (send plot:|Plot Hack 20| :select)
	(send plot:|Plot Hack 20|
	      :plot "C-m = 1uF/cm2, G-in = 100pS/um2"
	      (list
		(cdr (nth 1 (nth 0 (cdar (symbol-value-in-instance voltage-pane2 'plot:zoom-list)))))	;p
		(cdr (nth 0 (nth 0 (cdar (symbol-value-in-instance voltage-pane2 'plot:zoom-list)))))	;n cm 1, gin 100 
		)				;a
	      (list  "Rightward Motion"  "Leftward Motion") :y-min -72 ; :y-max -45
	      :x-interval 200	      :y-interval 5
	      :title-character-style '(:dutch :roman :normal)  :x-label "mS" :y-label "mV")
	(send plot:|Plot Hack 22|
	      :plot  "C-m = 1uF/cm2, G-in = 0pS/um2"
	      (list
		(cdr (nth 3 (nth 0 (cdar (symbol-value-in-instance voltage-pane2 'plot:zoom-list)))))	;p
		(cdr (nth 2 (nth 0 (cdar (symbol-value-in-instance voltage-pane2 'plot:zoom-list)))))	;nn cm 1, gin 0 
		)				;a
	      (list  "Rightward Motion"  "Leftward Motion") 	; :y-min -80 :y-max -45
	      :x-interval 200
	      :title-character-style '(:dutch :roman :normal)  :x-label "mS" :y-label "mV")

	(send plot:|Plot Hack 24|
	      :plot "C-m = .0001uF/cm2, G-in = 100pS/um2"
	      (list
		(cdr (nth 5 (nth 0 (cdar (symbol-value-in-instance voltage-pane2 'plot:zoom-list)))))	;p
		(cdr (nth 4 (nth 0 (cdar (symbol-value-in-instance voltage-pane2 'plot:zoom-list)))))	;n cm 1, gin 100 
		)				;a
	      (list  "Rightward Motion"  "Leftward Motion") 	; :y-min -80 :y-max -45
	      :x-interval 200
	      :title-character-style '(:dutch :roman :normal)  :x-label "mS" :y-label "mV")
	(send plot:|Plot Hack 26|
	      :plot  "C-m = .0001uF/cm2, G-in = 0pS/um2"
	      (list
		(cdr (nth 7 (nth 0 (cdar (symbol-value-in-instance voltage-pane2 'plot:zoom-list)))))	;p
		(cdr (nth 6 (nth 0 (cdar (symbol-value-in-instance voltage-pane2 'plot:zoom-list)))))	;nn cm 1, gin 0 
		)				;a
	      (list  "Rightward Motion"  "Leftward Motion") 	; :y-min -80 :y-max -45
	      :x-interval 200
	      :title-character-style '(:dutch :roman :normal)  :x-label "mS" :y-label "mV")

	)

(progn
  (send plot:|Plot Hack 25| :expose)	(send plot:|Plot Hack 25| :select)
  (send plot:|Plot Hack 21|
	:plot "Sigmoid Output with C-m = 1uF/cm2, G-in = 100pS/um2"
	(list
	  (list
	    (mapcar 'sigmoid
		    (car (cdr (nth 1 (nth 0 (cdar (symbol-value-in-instance voltage-pane2 'plot:zoom-list)))))));p
	    (cadr (cdr (nth 1 (nth 0 (cdar (symbol-value-in-instance voltage-pane2 'plot:zoom-list)))))))
	  (list
	    (mapcar 'sigmoid
		    (car (cdr (nth 0 (nth 0 (cdar (symbol-value-in-instance voltage-pane2 'plot:zoom-list)))))))	;n
		(cadr (cdr (nth 0 (nth 0 (cdar (symbol-value-in-instance voltage-pane2 'plot:zoom-list)))))))
						;n cm 1, gin 100 
	  )					;a
	(list  "Rightward Motion"  "Leftward Motion") 	:y-min 0	; :y-max -45
	:x-interval 200
	:title-character-style '(:dutch :roman :normal)  :x-label "mS" :y-label "")
  (send plot:|Plot Hack 23|
	:plot "Sigmoid Output with C-m = 1uF/cm2, G-in = 0pS/um2"
	(list
	  (list
	    (mapcar 'sigmoid
		    (car (cdr (nth 3 (nth 0 (cdar (symbol-value-in-instance voltage-pane2 'plot:zoom-list)))))))	;p
		(cadr (cdr (nth 3 (nth 0 (cdar (symbol-value-in-instance voltage-pane2 'plot:zoom-list)))))))
	  (list
	    (mapcar 'sigmoid
		    (car (cdr (nth 2 (nth 0 (cdar (symbol-value-in-instance voltage-pane2 'plot:zoom-list)))))))	;n
		(cadr (cdr (nth 2 (nth 0 (cdar (symbol-value-in-instance voltage-pane2 'plot:zoom-list)))))))
						;n cm 1, gin 100 
	  )					;a
	(list  "Rightward Motion"  "Leftward Motion") 	:y-min 0	; :y-max -45
	:x-interval 200
	:title-character-style '(:dutch :roman :normal)  :x-label "mS" :y-label "")
  (send plot:|Plot Hack 25|
	:plot "Sigmoid Output with C-m = .0001uF/cm2, G-in = 100pS/um2"
	(list
	  (list
	    (mapcar 'sigmoid
		    (car (cdr (nth 5 (nth 0 (cdar (symbol-value-in-instance voltage-pane2 'plot:zoom-list)))))))	;p
		(cadr (cdr (nth 5 (nth 0 (cdar (symbol-value-in-instance voltage-pane2 'plot:zoom-list)))))))
	  (list
	    (mapcar 'sigmoid
		    (car (cdr (nth 4 (nth 0 (cdar (symbol-value-in-instance voltage-pane2 'plot:zoom-list)))))))	;n
		(cadr (cdr (nth 4 (nth 0 (cdar (symbol-value-in-instance voltage-pane2 'plot:zoom-list)))))))
						;n cm 1, gin 100 
	  )					;a
	(list  "Rightward Motion"  "Leftward Motion") 	:y-min 0	; :y-max -45
	:x-interval 200
	:title-character-style '(:dutch :roman :normal)  :x-label "mS" :y-label "")
 (send plot:|Plot Hack 27|
	:plot "Sigmoid Output with C-m = .0001uF/cm2, G-in = 0pS/um2"
	(list
	  (list
	    (mapcar 'sigmoid
		    (car (cdr (nth 7 (nth 0 (cdar (symbol-value-in-instance voltage-pane2 'plot:zoom-list)))))))	;p
		(cadr (cdr (nth 7 (nth 0 (cdar (symbol-value-in-instance voltage-pane2 'plot:zoom-list)))))))
	  (list
	    (mapcar 'sigmoid
		    (car (cdr (nth 6 (nth 0 (cdar (symbol-value-in-instance voltage-pane2 'plot:zoom-list)))))))	;n
		(cadr (cdr (nth 6 (nth 0 (cdar (symbol-value-in-instance voltage-pane2 'plot:zoom-list)))))))
						;n cm 1, gin 100 
	  )					;a
	(list  "Rightward Motion"  "Leftward Motion") 	:y-min 0	; :y-max -45
	:x-interval 200
	:title-character-style '(:dutch :roman :normal)  :x-label "mS" :y-label "")
  )


(defun ds-ac-test ()
  (ds-ac 'test))

(defun ds-ac (name)       
  (create-cell-type "ds-ac" :soma-resistivity 30000 :membrane-resistivity 60000)
  (create-cell name :cell-type-name  "ds-ac")
  (let ((soma (create-soma (format nil "~a-soma" name) name 1)))
    (if *include-sources (create-source *clamp-type soma))
    (create-tree
      soma *ds-ac-segment-list*
	:default-diameter 0.2 :synapse t :xy-factor .5)))






(defvar *ds-ac-segment-list*
	'((SOMA SOMA 300 200 0 20)
	  (SOMA 1 375 200 0 2 ((SYNAPSE Excitatory-trans)))
	  (1 2 450 200 0 1 ((SYNAPSE Excitatory-trans)))
	  (2 3 525 200 0 1 ((SYNAPSE Excitatory-trans)))
	  (3 4 600 200 0 1 ((SYNAPSE Excitatory-trans)))
	  (4 5 675 200 0 1 ((SYNAPSE Excitatory-trans)))
	  (5 6 750 200 0 0.75 ((SYNAPSE Excitatory-trans)))
	  (6 7 825 200 0 0.75 ((SYNAPSE Excitatory-trans)))
	  (7 8 900 200 0 0.75 ((SYNAPSE Excitatory-trans)))
	  (8 9 975 200 0 0.5 ((SYNAPSE Excitatory-trans)))
	  (9 10 1050 200 0 0.5 ((SYNAPSE Excitatory-trans)))
	  (SOMA 13 354 287 0 3)
	  (13 15 381 373 0 1)
	  (13 14 463 301 0 2)))


(defvar *rabbit-ac-segment-list*


 `(
		   (soma		1a		7		-1  -5          1.2    )
		   (1a		1b		12		-3  -7          0.6    )
		   (1b		11		18		-5  -10     .2      )
		   (11		111		20		-3  -10 .2 )
		   (111		1111		30		-3  -10 .2 )
		   (111		1112a		30		-5  -10 .2 )
		   (1112a		1112b		33		-5  -10 .2 )
		   (11		112		30		-6  -10 .2 )
		   (11		113a		17		-7  -10 .2 )
		   (113a		113b		24		-9  -10 .2 )
		   (113b		1131		28		-9  -10 .2 )
		   (113b		1132		24		-11  -10 .2 )
		   (1b		12a		11		-6  -10          .2 )
		   (12a		12b		24		-14  -10         .2 )
		   (12b		121		28		-14  -10 .2 )
		   (12b		122		34		-17  -10 .2 )
		   (soma		2		-2		0  -5          1.2  )
		   (2		21		-3		-2  -7         1.0  )
		   (21		211		5		-6  -10       0.6  )
		   (211		2111		16		-14  -10 .2 )
		   (2111		21111a		17		-13  -10 .2 )
		   (21111a		21111b		21		-15  -10 .2 )
		   (21111b		211111a		23		-16  -10 .2 )
		   (211111a		211111b		27		-16  -10 .2 )
		   (21111b		211112a		22		-17  -10 .2 )
		   (211112a		211112b		25		-20  -10 .2 )
		   (2111		21112		19		-18  -10 .2 )
		   (2111		21113		21		-23  -10 .2 )
		   (211		2112		9		-13  -10 .2 )
		   (2112		21121a		13		-14  -10 .2 )
		   (21121a		21121b		13		-17  -10 .2 )
		   (21121b		211211		18		-22  -10 .2 )
		   (21121b		211212		17		-24  -10 .2 )
		   (2112		21122		11		-16  -10 .2 )
		   (2112		21123a		8		-16  -10 .2 )
		   (21123a		21123b		12		-23  -10 .2 )
		   (21123b		211231		14		-24  -10 .2 )
		   (21123b		211232		12		-26  -10 .2 )
		   (21		212a		-5		-3  -10     0.8  )
		   (212a		212b		-5		-5  -10     0.6  )
		   (212b		2121		-4		-7  -10 .2 )
		   (2121		21211		3		-11  -10 .2 )
		   (21211		212111		10		-22  -10 .2 )
		   (21211		212112		3		-18  -10 .2 )
		   (212112		2121121		9		-25  -10 .2 )
		   (212112		2121122a		1		-19  -10 .2 )
		   (2121122a		2121122b		5		-23  -10 .2 )
		   (21211		212113a		-1		-11  -10 .2 )
		   (212113a		212113b		0		-16  -10 .2 )
		   (2121		21212a		-7		-10  -10 .2 )
		   (21212a		21212b		-7		-13  -10 .2 )
		   (21212b		212121a		5		-24  -10 .2 )
		   (212121a		212121b		3		-25  -10 .2 )
		   (21212b		212122		-7		-17  -10 .2 )
		   (212122		2121221a		-2		-19  -10 .2 )
		   (2121221a		2121221b		0		-27  -10 .2 )
		   (212122		2121222		-7		-20  -10 .2 )
		   (2121222		21212221		-5		-26  -10 .2 )
		   (2121222		21212222		-5		-29  -10 .2 )
		   (212b		2122a		-8		-6  -10 .2 )
		   (2122a		2122b		-12		-13  -10 .2 )
		   (2122b		21221a		-10		-14  -10 .2 )
		   (21221a		21221b		-14		-26  -10 .2 )
		   (2122b		21222		-13		-15  -10 .2 )
		   (21222		212221		-14		-20  -10 .2 )
		   (212221		2122211a		-14		-23  -10 .2 )
		   (2122211a		2122211b		-16		-23  -10 .2 )
		   (212221		2122212		-17		-23  -10 .2 )
		   (2122212		21222121		-19		-26  -10 .2 )
		   (2122212		21222122		-19		-20  -10 .2 )
		   (21222		212222		-16		-16  -10 .2 )
		   (2		22		-4		0  -10        1.0 )
		   (22		221		-8		-2  -10       0.8 )
		   (221		2211		-10		-6  -10       0.6 )
		   (2211		22111a		-10		-8  -10 .2 )
		   (22111a		22111b		-19		-16  -10 .2 )
		   (22111b		221111a		-25		-23  -10 .2 )
		   (221111a		221111b		-27		-22  -10 .2 )
		   (22111b		221112a		-20		-15  -10 .2 )
		   (221112a		221112b		-26		-18  -10 .2 )
		   (2211		22112		-13		-8  -10 .2 )
		   (22112		221121		-18		-13  -10 .2 )
		   (221121		2211211		-20		-14  -10 .2 )
		   (221121		2211212		-24		-14  -10 .2 )
		   (2211212		22112121		-24		-16  -10 .2 )
		   (2211212		22112122		-28		-16  -10 .2 )
		   (22112		221122		-19 		-10  -10 .2 )
		   (221122		2211221a		-21		-12  -10 .2 )
		   (2211221a		2211221b		-22		-13  -10 .2 )
		   (221122		2211222		-30		-16  -10 .2 )
		   (221		2212		-14		-2  -10      0.6  )
		   (2212		22121		-29		-8  -10 .2 )
		   (22121		221211a		-29		-12  -10 .2 )
		   (221211a		221211b		-32		-12  -10 .2 )
		   (22121		221212		-33		-9  -10 .2 )
		   (2212		22122		-17		-1  -10 .2 )
		   (22122		221221		-19		-2  -10 .2 )
		   (221221		2212211		-21		-3  -10 .2 )
		   (2212211		22122111		-21		-4  -10 .2 )
		   (2212211		22122112		-23		-4  -10 .2 )
		   (22122112		221221121		-29		-5  -10 .2 )
		   (22122112		221221122		-27		-4  -10 .2 )
		   (221221		2212212		-20		-2  -10 .2 )
		   (22122		221222a		-19		0  -10 .2 )
		   (221222a		221222b		-20		-1  -10 .2 )
		   (221222b		2212221		-30		-3  -10 .2 )
		   (221222b		2212222a		-28		-1  -10 .2 )
		   (2212222a		2212222b		-33		-3  -10 .2 )
		   (22		222		-15		4  -10    0.6 )
		   (222		2221		-18		4  -10 .2 )
		   (2221		22211		-27		3  -10 .2 )
		   (22211		222111a		-32		3  -10 .2 )
		   (222111a		222111b		-39		0  -10 .2 )
		   (22211		222112		-35		5  -10 .2 )
		   (2221		22212		-28		8  -10 .2 )
		   (22212		222121a		-30		6  -10 .2 )
		   (222121a		222121b		-35		6  -10 .2 )
		   (22212		222122		-34		11  -10 .2 )
		   (222		2222		-25		10  -10 .2 )
		   (22		223		-4		4  -10       0.8 )
		   (223		2231		-11		8  -10    0.6  )
		   (2231		22311		-15		9  -10 .2 )
		   (22311		223111		-21		10  -10 .2 )
		   (223111		2231111		-29		12  -10 .2 )
		   (2231111		22311111		-30		11  -10 .2 )
		   (2231111		22311112a		-32		13  -10 .2 )
		   (22311112a		22311112b		-34		12  -10 .2 )
		   (223111		2231112		-25		14  -10 .2 )
		   (2231112		22311121a		-29		18  -10 .2 )
		   (22311121a		22311121b		-31		17  -10 .2 )
		   (2231112		22311122a		-25		16  -10 .2 )
		   (22311122a		22311122b		-26		16  -10 .2 )
		   (22311		223112		-18		12  -10 .2 )
		   (223112		2231121		-22		14  -10 .2 )
		   (223112		2231122		-28		20  -10 .2 )
		   (2231		22312		-19		18  -10 .2 )
		   (22312		223121		-22		21  -10 .2 )
		   (223121		2231211a		-24		21  -10 .2 )
		   (2231211a		2231211b		-25		23  -10 .2 )
		   (223121		2231212a		-21		22  -10 .2 )
		   (2231212a		2231212b		-26		26  -10 .2 )
		   (22312		223122		-18		20  -10 .2 )
		   (223		2232		-8		9  -10   0.6  )
		   (2232		22321		-18		21  -10 .2 )
		   (22321		223211a		-20		21  -10 .2 )
		   (223211a		223211b		-21		24  -10 .2 )
		   (22321		223212a		-19		24  -10 .2 )
		   (223212a		223212b		-23		25  -10 .2 )
		   (2232		22322		-9		16  -10 .2 )
		   (22322		223221		-11		18  -10 .2 )
		   (223221		2232211		-17		21  -10 .2 )
		   (223221		2232212		-18		29  -10 .2 )
		   (22322		223222a		-8		17  -10 .2 )
		   (223222a		223222b		-14		29  -10 .2 )
		   (223		2233a		-1		7  -10      0.6 )
		   (2233a		2233b		-4		10  -10 .2 )
		   (2233b		22331		-12		28  -10 .2 )
		   (2233b		22332a		-3		18  -10 .2 )
		   (22332a		22332b		-12		31  -10 .2 )
		   (soma		3		0		2  -5       1.2  )
		   (3		31		4		9  -7       0.6  )
		   (31		311		2		11  -10 .2 )
		   (311		3111a		-1		11  -10 .2 )
		   (3111a		3111b		-2		18  -10 .2 )
		   (311		3112		3		17  -10 .2 )
		   (3112		31121		-4		30  -10 .2 )
		   (3112		31122		5		25  -10 .2 )
		   (31		312		8		12  -10 .2 )
		   (312		3121		7		19  -10 .2 )
		   (312		3122		10		13  -10 .2 )
		   (3122		31221a		10		14  -10 .2 )
		   (31221a		31221b		15		20  -10 .2 )
		   (3122		31222a		12		14  -10 .2 )
		   (31222a		31222b		22		20  -10 .2 )
		   (31221b		312211		14		22  -10 .2 )
		   (31221b		312212		18		22  -10 .2 )
		   (31		313a		8		8  -10 .2 )
		   (313a		313b		18		16  -10 .2 )
		   (3		32		6		3  -10         0.9 )
		   (32		321		12		5  -10 0.6  )
		   (321		3211		18		9  -10     .2 )
		   (3211		32111a		17		13  -10 .2 )
		   (32111a		32111b		22		19  -10 .2 )
		   (3211		32112		32		17  -10 .2 )
		   (321		3212a		14		4  -10 .2 )
		   (3212a		3212b		19		6  -10 .2 )
		   (3212b		32121		33		13  -10 .2 )
		   (3212b		32122		30		9  -10 .2 )
		   (32		322		18		2  -10   0.6 )
		   (322		3221		23		3  -10 .2 )
		   (3221		32211		35		5  -10 .2 )
		   (3221		32212		33		0  -10 .2 )
		   (322		3222		26		-1  -10 .2 )
		   ))