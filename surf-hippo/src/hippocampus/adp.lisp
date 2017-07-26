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

;; adp models

(in-package "SURF-HIPPO")

(defun adp-c12861-cable ()
  (let (*active*)
    (HIPPO "Hippo" :dendrite-diameter 5.0 ; 6.0 ;8.0 ; 3.6055512 ; 9.196152
	   :soma-diameter 18.2
	   :r-cyto 550.0
	   :r-mem 20000.0
	   :r-mem-soma 10000.0
	   :cap-mem 1.0
	   :cap-mem-soma 0.75
	   :dendrite-length 1000.0
	   :total-segs 50)
    (setq user-stop-time 12.0)
    (setq *relative-voltage-error 0.001)
    (enable-element-plot *soma*)
    (setq *pwl-isource-di-dt 10.0)	;maybe this should be 5
    (pulse-list (add-isource *soma*) '((1.0 2.9 1.0) (2.9 3.30 -1.2)))
    (enable-element-plot *isource*)
    *cell*))

(defun adp-c12861 ()
  (read-in-circuit "/home/lyle/surf-hippo/anatomy/misc/c12861.ca1")
  (enable-element-plot *soma*)
  (setq user-stop-time 12.0)
  (setq *relative-voltage-error 0.001)
  (cell-type-param "CA1" 'soma-resistivity 10000.0 t)
  (cell-type-param "CA1" 'membrane-resistivity 20000.0 t)
  (set-cell-type-param "CA1" 'cytoplasmic-resistivity 550.0 t)
  (setq *pwl-isource-di-dt 10.0)	;maybe this should be 5
  (pulse-list (add-isource *soma*) '((1.0 2.9 1.0) (2.9 3.30 -1.2)))
  (enable-element-plot *isource*)
  *cell*)


(defun adp-HIPPO-f5 ()
  (let (*active*)
    (HIPPO "Hippo" :dendrite-diameter 6.0 :dendrite-length 10000.0 :total-segs 50)
    (enable-element-plot *soma*)
    (setq user-stop-time 12.0)
    (setq *relative-voltage-error 0.001)
    (cell-type-param "CA1" 'soma-resistivity 10000.0 t)
    (cell-type-param "CA1" 'membrane-resistivity 20000.0 t)
    (set-cell-type-param "CA1" 'cytoplasmic-resistivity 550.0 t)
    (setq *pwl-isource-di-dt 10.0)	;maybe this should be 5
    (pulse-list (add-isource *soma*) '((1.0 2.9 1.0) (2.9 3.30 -1.2)))
    (enable-element-plot *isource*)
    *cell*))




