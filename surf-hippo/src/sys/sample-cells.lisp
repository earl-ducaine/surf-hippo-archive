;; -*- mode: lisp; Syntax: Common-lisp; package: surf ; base: 10;  -*-
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


;;; SYS Source file: sample-cells.lisp
(in-package "SURF-HIPPO")


;; some simple cells....


;; Rm ~40K cm2, Rin ~50M => soma diameter ~160um
(defun basic-soma-cell ()
  (create-cell "basic-soma" :soma-diameter 160))



(defun basic-is-cell ()
  (create-cell "basic-tree"
	       :soma-diameter 100.0
	       :tree-list '((soma 1 5  0 0)
			    (1    2 10 0 0)
			    (2    3 15 0 0)
			    (3    4 20 0 0)
			    (4    5 25 0 0))
	       :segment-diameter 1.0))

(defun basic-tree-cell ()
  (create-cell "basic-tree"
	       :soma-diameter 25
	       :tree-list '((soma 1 20  -10 0)
			    (1    2 40 10 0)
			    (2    3 60 -10 0)
			    (3    4 80 10 0)
			    (4    5 100 -10 0))
	       :segment-diameter 5.0))

(defun basic-tree-cell-2 ()
  (create-cell "basic-tree"
	       :soma-diameter 25
	       :tree-list '((soma 1 20  -10 0)
			    (1    2a 40 10 0)
			    (2a    3a 60 -10 0)
			    (3a    4a 80 10 0)
			    (4a    5a 100 -10 0)
			    (1    2b 40 -40 0)
			    (2b    3b 60 -80 0)
			    (3b    4b 80 -90 0)
			    (4b    5b 100 -100 0))
	       :segment-diameter 5.0))

(defun basic-single-seg-cell ()
  (create-cell "basic-tree"
	       :soma-diameter 25
	       :tree-list '((soma 1 20 -10 0))
	       :segment-diameter 5.0))

(defun basic-single-seg-cell-vs ()
  (basic-single-seg-cell)
  (add-vsource *soma*)
  (enable-element-plot *soma*)
  (setq *user-stop-time* 10))


(defun basic-synapse-cell ()
  (create-cell "Basic-auto-synapse" :soma-diameter 25)
  (add-isource *soma* nil '(5 10 -.10))
  (add-events (CREATE-SYNAPSE *soma* 'auto-fast-ex) 10.0)
  (setq *user-start-time* 0.0 *user-stop-time* 50.0)
  (enable-element-plot *soma* :all)
  *cell*)


(defun basic-light-synapse-cell ()
  (create-cell "Basic-light-synapse" :soma-diameter 25)
  (CREATE-SYNAPSE *soma* 'l-ex-1)
  (add-isource *soma* nil '(5 10 -.10))
  (setq *user-start-time* 0.0 *user-stop-time* 25.0
	*enable-light* t
	*user-stop-time* 30.0
	*light-stimulus* :spot
	*spot-outside-diameter* 300.0 
	*light-stimulus-start-time* 10.0	
	*light-stimulus-stop-time* 20.0
	*light-start-position-x* 0.0
	*light-start-position-y* 0.0
	*plot-node-elements* *soma*)
  *cell*)

