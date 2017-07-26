;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: #-PARALLEL SURF #+PARALLEL *SURF; Base: 10; -*-
;;; Lyle Borg-Graham, MIT Center for Biological Information Processing

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              The Surf-Hippo Neuron Simulator                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Surf-Hippo project at the  ;;;
;;; Center for Biological Information Processing, Department of     ;;;
;;; Brain and Cognitive Sciences, Massachusetts Institute of        ;;;
;;; Technology, and has been placed in the public domain. If you    ;;;
;;; are using this code or any part of Surf-Hippo, please contact   ;;;
;;; surf-hippo@ai.mit.edu to be put on the mailing list.            ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package "SURF-HIPPO")

#|

Soma - short cable reverse engineered geometry from: 

@ARTICLE{Bro-Fri-Per-81,
	Author  = {Brown, T. H. and Fricke, R. A. and Perkel, D. H.},
  Title   = {Passive electrical constants in three classes of hippocampal neurons},
  Journal = {Journal of Neurophysiology},
  Year    = {1981},
  Volume  = {46},
  Number  = {4},
  Pages   = {812-827},
  Month   = {October}
  }
|#

(cell-type-def
 '(Brown-81
   (membrane-resistivity . 19000)
   (cytoplasmic-resistivity  . 75)
   (specific-capacitance . 1)))

(defun brown-81 ()
  (hippo "Brown-81"
	 :soma-diameter (* 2 42)
	 :dendrite-length 1800
	 :dendrite-diameter (* 2 3)
	 :total-segs 20
	 :cell-type (create-celltype 'Brown-81)
	 :channels nil
	 :active nil
	 :soma-electrode t))
#|
some results from this structure:

* (electrotonic-length 1800.0 6.0 75.0 1.0 19000.0)
0.9233805
* 
* (rho)
1.1856153
* (print-element *cell*)
Cell Brown-81 (soma @ [0.0 0.0 0.0])  -  Created from BROWN-81
      20 Segments, 1 Trunk, 1 Terminal, Total Membrane Area 5.61e+4um^2
      Passive R-in from soma (actual / cable model) = 39.22 / 38.86 Mohms 
       Max G-in / Min R-in = 0.03 uS / 33.87 Mohms 
       Soma passive R-in = 85.71 Mohms
       Dendritic tree passive R-in (actual / cable model) = 72.29 / 71.08 Mohms
       Coupling R's from individual compartments [single-leg]
NIL
*

tau-0 from simulation:

* (/ 77.834  4.1)
18.983902
* 

|#