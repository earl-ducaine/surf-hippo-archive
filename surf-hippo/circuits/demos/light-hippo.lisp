;; A sample neuron file, with a moving bar light input. This file should be TOPLOADed to setup the
;; circuit. 

(setq *light-stimulus* :moving-bar
      *light-speed* 4
      *bar-length* 500 *bar-width* 50
      *light-theta* (/ pi-single 2)
      ;; T (nil) => movement is 90 degrees ahead (behind) of *light-theta
      *light-direction* nil
      *light-stimulus-start-time* 0	;Time to start bar moving, milliseconds
      *light-start-position-x* -300	; Point of center of stimulus at *motion-start-time in microns
      *light-start-position-y* 700

      *enable-light* t *enable-synapses* t
      *user-stop-time* 300)

;; See definition of the HIPPO function in surf-hippo/src/hippocampus/hippo.lisp
(hippo "hippo" :cell-origin '(0.0 0.0 0.0) :total-segs 10
       :synapse-segs '("hippo-6")
       :synapse-types (list 'l-ex-fac))

(enable-element-plot '("hippo-2" "hippo-4" "hippo-6" "hippo-8" "hippo-10" "hippo-soma")) 




