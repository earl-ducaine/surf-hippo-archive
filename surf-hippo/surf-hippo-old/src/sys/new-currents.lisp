;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: #-PARALLEL SURF #+PARALLEL *SURF; Base: 10; -*-
;;; (c) Copyright 1990, Lyle Borg-Graham, MIT Center for Biological Information Processing
;;;
;;;                        **** Surf CURRENTS FILE ******
;;;
;;;   This file contains make-believe membrane current parameters.             


(in-package #+parallel '*surf #-parallel 'surf)

;;; 	A-RET-CURRENT *************

;;;; This is inspired by the hippocampal A-current.

;;; May want to have long de-inactivation to allow for the eventual killing of the facilitory depolarization by
;;; inhibition (approx 200 ms) WITHOUT the immediate de-inactivation of the A-RET current, in particular we would like
;;; the A-RET current to stay inactivated for another 100-200 ms. Note that this is consistent with the
;;; requirement for delayed inactivation on the depolarizing phase (that is the need for sustained priming input
;;; for facilitation).

(defvars-w-value
  (*base-tx-a-ret 1.0)  (*base-ty-a-ret 24.0)
  (*v-half-a-ret-x -52.0) (*base-rate-a-ret-x 0.2) (*valence-a-ret-x 3.5) (*gamma-a-ret-x 0.8)
  (*v-half-a-ret-y -72.0) (*base-rate-a-ret-y 0.0015) (*valence-a-ret-y 7) (*gamma-a-ret-y 0.4))

;;; A-current conductance (microsiemans)
(defvar *gbar-a-ret  .50 )	


;;; X-A-RET-INF
;;; 	x-inf is activation variable for A-current (- not confirmed sigmoid)
(defun x-a-ret-inf (voltage)
;Segal and Barker; Segal, Rogawski, and Barker -  z=3.67,vhalf=-30
;Zbicz and Weight - z=8.5,vhalf=-45
    (let ((a (alpha voltage *v-half-a-ret-x *base-rate-a-ret-x *valence-a-ret-x *gamma-a-ret-x))	;
	  (b (beta voltage *v-half-a-ret-x *base-rate-a-ret-x *valence-a-ret-x *gamma-a-ret-x)))
      (/ a (+ a b))))

;;; Y-A-RET-INF
;;; 	y-inf is inactivation variable for A-current
(defun y-a-ret-inf (voltage)
;Segal and Barker; Segal, Rogawski, and Barker - z=4.28,vhalf=-70
;Z & W = z=8,vhalf=-55
  (let ((b (alpha voltage *v-half-a-ret-y *base-rate-a-ret-y *valence-a-ret-y *gamma-a-ret-y))
	(a (beta voltage *v-half-a-ret-y *base-rate-a-ret-y *valence-a-ret-y *gamma-a-ret-y)))
    (/ a (+ a b))))

;;; T-X-A-RET
;;; 	tau-A-RET-current(activation) - msec	(estimate)
(defvars-w-value (*t-x-a-ret-1 3.0)(*t-x-a-ret-2 5.0)(*t-y-a-ret-1 5.0))



(defun t-x-a-ret (&optional voltage)
;Segal and Barker; Segal, Rogawski, and Barker. Measured from V-holding = -70mv to steps up to -20mv
;Z  & W  Probably more of an estimate, i.e. < 10ms.
    (let* ((a (alpha voltage *v-half-a-ret-x *base-rate-a-ret-x *valence-a-ret-x *gamma-a-ret-x))
	  (b (beta voltage *v-half-a-ret-x *base-rate-a-ret-x *valence-a-ret-x *gamma-a-ret-x))
	  (tx (/ *qten-factor-at-30 (+ a b))))
      (if (< tx (* *qten-factor-at-30 *base-tx-a-ret)) (* *qten-factor-at-30 *base-tx-a-ret) tx)))

;;; T-Y-A-RET-
;;; 	tau-A-RET-current(inactivation) - msec
(defun t-y-a-ret (&optional voltage)
;Segal and Barker; Segal, Rogawski, and Barker
;Z & W Supposedly very rapid below -54mv(5ms) -~380 ms otherwise.
  (let* ((b (alpha voltage *v-half-a-ret-y *base-rate-a-ret-y *valence-a-ret-y *gamma-a-ret-y))
	(a (beta voltage *v-half-a-ret-y *base-rate-a-ret-y *valence-a-ret-y *gamma-a-ret-y))
	(ty (/ *qten-factor-at-30 (+ a b))))
      (if (< ty (* *qten-factor-at-30 *base-ty-a-ret)) (* *qten-factor-at-30  *base-ty-a-ret) ty)))

;;; A-PLOT
(defvars *x-a-ret-inf* *y-a-ret-inf* *x-a-ret-eff* *y-a-ret-eff*
  *t-x-a-ret* *t-y-a-ret* *g-a-ret-inf*)

;;; G-A-RET 
(defun g-a-ret ( gbar-a-ret x-a-ret y-a-ret)
  (* gbar-a-ret x-a-ret  x-a-ret x-a-ret y-a-ret  ))

;;;X-A-RET-EFF, Y-A-RET-EFF
(defun x-a-ret-eff (x-a-ret)
  (expt x-a-ret 3.0))
(defun y-a-ret-eff (y-a-ret)
  (expt y-a-ret  2.0))
