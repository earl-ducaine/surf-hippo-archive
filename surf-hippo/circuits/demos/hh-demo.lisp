;; A sample neuron file. This file should be TOPLOADed to setup the circuit.

(cell-type-def
 '(cortical
   (membrane-resistivity . 40000)	; ohms-cm2
   (cytoplasmic-resistivity  . 200)	; ohms-cm
   (specific-capacitance . 0.7)		; uF/cm2
   (v-leak . -65)			; mV
   ))

(let ((soma (create-soma :cell (create-cell "Demo-Cell"
					    :cell-origin '(100.0 -300.0 50.0) ; XYZ in ums
					    :cell-type 'cortical)
			 :diameter 35))) ; Soma diameter in ums.

  ;; Add a simple chain of dendritic segments.
  (make-segment-chain
   soma					; Attach chain to soma.
   "apical"				; Label to add to segments
   5					; Number of segments in chain
   (/ 1200.0 5)				; Segment length in ums
   12.0					; Segment diameter in ums
   :proximal-theta (* 0.5 pi-single))	; Relative orientation of chain in
					; radians. Default for proximal-phi = 0

  ;; Now a basal cable.
  (make-segment-chain
   soma					; Attach chain to soma.
   "basal"				; Label to add to segments
   3					; Number of segments in chain
   (/ 500.0 3)				; Segment length in ums
   6.0					; Segment diameter in ums
   :proximal-theta (* -0.5 pi-single))	; Relative orientation of chain in
					; radians. Default for proximal-phi = 0

  ;; Add some channels to the soma.
  (create-element
   soma
   ;; These fits to the canonical HH channels are in the channel library.
   'na-hh-fit 'dr-hh-fit)		

  ;; Setup some plot flags.
  (enable-element-plot soma)		; This will flag voltage by default
    
  ;; The function (channels) returns a list of all the channels
  (enable-element-plot (channels) 'current)
  (enable-element-plot (channels) 'conductance)
    
  (enable-element-plot (segments))	; Plot all the segment voltages too - voltage by default.

  ;; Install and setup a current source at the soma.
  (pulse-list
   (add-isource soma)			; This function returns the current source.
   '(10 20 .5))				; A 0.5 nA pulse from 10 to 20 ms

  
  ;; A 50ms simulation.
  (setq *user-stop-time* 50)		

  ;; The function will return a pointer to the created cell. This might be useful if this function
  ;; is included in a more complicated circuit definition file. Note that here you could do the same
  ;; thing with the variable *CELL*.

  (element-cell soma))
