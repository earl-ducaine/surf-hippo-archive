;; Colorization Demonstration
;;
;; Cell model is full ~3000 compartment reconstruction of a hippocampal pyramidal cell (see
;; Borg-Graham, 1998), supplied by Dennis Turner (Southampton University). Somatic channels are
;; taken from the Working Model. Input includes a current pulse at the soma, and random activity of
;; an excitatory synapse in the dendritic tree.
;;
;;


(topload "anatomy/turner/ca1/n120.fix")
(setq *user-stop-time* 20 *enable-colorize-time* t
      *enable-colorize-scale* t
      *colorize-simulation* t
      *enable-sparse-data* t)
(enable-element-plot *soma*)
(enable-element-plot 711)
(events (create-element 711 'AUTO-FAST-EX-DOUBLE-EXP-ABS)
	;; (poisson-events 1.0 3.0 20.0)
	'(3.6254377 4.2996325 5.8688464 5.9309845 6.3608437 6.4220166 6.686985 7.152315
	  7.2406297 7.442387 7.975373 9.300047 15.894774 17.591448 17.971172 18.254477))

(set-type-graphics 'AUTO-FAST-EX-DOUBLE-EXP-ABS 'purple 'color)

(just-draw :scale 1.0)
(mark-elements :win *standard-graphics-output* :draw-synapses t)
(label-element (element 711 'synapse))

(add-working-hpc-channels *soma* t)
(turn-off 'KAHP-HPC-ABSOLUTE)
(pulse-list (add-isource *soma*) '(5 100 0.5))

(goferit)

(show-sparse-data 6.5)			; Show the middle of the spike.
