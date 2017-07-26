;;;; CarGo 3.0
;;;
;;; CarGo is copyright (c) 1994 by Peter Dudey Drake
;;;
;;; CARGO package definition
;;;
;;; This package includes the files:
;;;
;;; board
;;;


;;; DEFINE THE PACKAGE

(defpackage "CARGO"
  (:use "LISP" "LUCID-COMMON-LISP" "UTILS")
  (:export "IN-ATARI-P"
	   "SUICIDAL-P"
	   "LEGAL-P"
	   "PLAY-AT"
	   "PASS"
	   "REMOVE-DOOMED-CHAIN"
	   "HANDLE-EVENT"
	   "COUNT-SCORE"
	   "PLAY-GO"))

