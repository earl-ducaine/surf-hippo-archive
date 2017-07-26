;;;; CarGo 3.0
;;;
;;; CarGo is copyright (c) 1994 by Peter Dudey Drake
;;;
;;; UTILS package definition
;;;
;;; This package includes the files:
;;;
;;; utils
;;;


;;; DEFINE THE PACKAGE

(defpackage "UTILS"
  (:use "LISP" "LUCID-COMMON-LISP")
  (:export "WITH-POINT"
	   "COORDINATES"
	   "OPPOSITE"
	   "NOR"
	   "SINGLE"
	   "AIF"
	   "AWHEN"
	   "IT"
	   "FOR"
	   "PULL"))
