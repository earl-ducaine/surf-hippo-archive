
(in-package "SURF-HIPPO")

#|

This file reproduces traces in Figure 5 of:

Borg-Graham, L., "Interpretations of Data and Mechanisms for Hippocampal
Pyramidal Cell Models". Chapter in "Cerebral Cortex, Volume 13, Cortical
Models", edited by P.S. Ulinski, E.G. Jones and A. Peters, Plenum Press, 1998.

This file can be loaded directly into Surf-Hippo for demonstration.

|#


(topload 'working-hpc)
(setq *user-stop-time* 250)

(std-setup)				; Plot soma voltage and add soma current source.

(let ((*overlay-all-plots* t)
      (*accomodate-all-overlays* t))
  (setq *active* t) 
  (pulse-list *isource* '(10 150 0.10)) (goferit)
  (pulse-list *isource* '(10 150 -0.10)) (goferit)
  (pulse-list *isource* '(10 150 0.05)) (goferit)
  (pulse-list *isource* '(10 150 -0.05)) (goferit)
  (setq *active* nil) 
  (pulse-list *isource* '(10 150 0.10)) (goferit)
  (pulse-list *isource* '(10 150 -0.10)) (goferit)
  (pulse-list *isource* '(10 150 0.05)) (goferit)
  (pulse-list *isource* '(10 150 -0.05)) (goferit))