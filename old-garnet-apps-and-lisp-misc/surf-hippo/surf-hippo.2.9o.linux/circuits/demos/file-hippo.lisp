;; A sample neuron file. This file should be TOPLOADed to setup the circuit.


(in-package "SURF-HIPPO")

;; Soma/short-cable geometry, HH channels. 

(let ((cell-name "file-cell")
      (seg-length (/ 1200.0 5))
      (seg-diam 12.0))
  (create-soma :cell (create-cell cell-name :cell-type 'HPC)
	       :diameter 35.0)
  (create-element *soma* 'na-hh 'dr-hh)	; Squid axon in hippos?
  (segment-chain *soma* nil
		 5
		 seg-length
		 seg-diam
		 :proximal-theta (* 0.5 pi-single)))

(pulse-list (add-isource *soma*) '(10 200 1.4))

(enable-element-plot (list *soma* (distal-tips)))

(setq *user-stop-time* 300)


