;;;; CarGo 3.0
;;;
;;; CarGo is copyright (c) 1994 by Peter Dudey Drake
;;;
;;; GUI package definition
;;;
;;; This package includes any one of the following files:
;;;
;;; gui
;;; gui.no-graphics
;;;


;;; MAKE SURE GARNET IS LOADED

(unless (find-package "KR")
  (error "You need to load Garnet first."))


;;; DEFINE THE PACKAGE

(defpackage "GUI"
  (:use "LISP" "LUCID-COMMON-LISP" "KR" "OPAL" "UTILS")
  (:export "CLEAR-BOARD"
	   "KILL-GRAPHICS"
	   "SELECT-WINDOW"
	   "SET-STONES"
	   "MESSAGE"
	   "BEEP"))


;;; LOAD THE DEFINITION OF THE CARGO PACKAGE IF NECESSARY

(unless (find-package "CARGO")
  (load "cargo-package"))
