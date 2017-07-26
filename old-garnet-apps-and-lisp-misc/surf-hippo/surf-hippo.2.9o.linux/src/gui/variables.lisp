;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: WINDOWS-HACK; Base: 10 -*-
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


;; GUI Source file: variables.lisp


(IN-PACKAGE "WINDOWS-HACK")



;;; This file contains some variables used by the GUI files. Many variables are defined in other
;;; files, however.  

(deftype fn () 'fixnum)
(deftype df () 'double-float)
(deftype sf () 'single-float)


(defconstant e 2.71828182845904523536028747135266249775724709369996L0)
(defconstant e-single (coerce e 'single-float))

(defconstant pi-single (COERCE user::PI 'SINGLE-FLOAT))




;; *****************************************
;;
;; Dummys for menus.
;;
;; *****************************************

;; Creates dummy1 - dummy50 - I am sure this is not the "correct" way....
;(eval (cons 'defvars-w-value
;            (loop for i from 1 to 50
;                  collect (list (read-from-string (format nil "dummy~d" i))
;                                nil))))
;;
;(defvar menu-dummys
;  (loop for i from 1 to 100
;        collect (list (read-from-string (format nil "dummy~d" i))
;                      nil)))
;  
;(defvars-w-value (eval menu-dummys))
;
;(loop for dummy-list in menu-dummys do
;      (export (car dummy-list)))
;

(defvar dummy1 nil)
(defvar dummy2 nil)
(defvar dummy3 nil)
(defvar dummy4 nil)
(defvar dummy5 nil)
(defvar dummy6 nil)
(defvar dummy7 nil)
(defvar dummy8 nil)
(defvar dummy9 nil)
(defvar dummy10 nil)
(defvar dummy11 nil)
(defvar dummy12 nil)
(defvar dummy13 nil)
(defvar dummy14 nil)
(defvar dummy15 nil)
(defvar dummy16 nil)
(defvar dummy17 nil)
(defvar dummy18 nil)
(defvar dummy19 nil)
(defvar dummy20 nil)
(defvar dummy21 nil)
(defvar dummy22 nil)
(defvar dummy23 nil)
(defvar dummy24 nil)
(defvar dummy25 nil)
(defvar dummy26 nil)
(defvar dummy27 nil)
(defvar dummy28 nil)
(defvar dummy29 nil)
(defvar dummy30 nil)

(defvar menu-dummys '(
		      dummy1 
		      dummy2 
		      dummy3 
		      dummy4 
		      dummy5 
		      dummy6 
		      dummy7 
		      dummy8 
		      dummy9 
		      dummy10 
		      dummy11 
		      dummy12 
		      dummy13 
		      dummy14 
		      dummy15 
		      dummy16 
		      dummy17 
		      dummy18 
		      dummy19 
		      dummy20 
		      dummy21 
		      dummy22 
		      dummy23 
		      dummy24 
		      dummy25 
		      dummy26 
		      dummy27 
		      dummy28 
		      dummy29 
		      dummy30


		      fmenu-dummy1 
		      fmenu-dummy2 
		      fmenu-dummy3 
		      fmenu-dummy4 
		      fmenu-dummy5 
		      fmenu-dummy6 
		      fmenu-dummy7 
		      fmenu-dummy8 
		      fmenu-dummy9 
		      fmenu-dummy10 
		      fmenu-dummy11 
		      fmenu-dummy12 
		      fmenu-dummy13 
		      fmenu-dummy14 
		      fmenu-dummy15 
		      fmenu-dummy16 
		      fmenu-dummy17 
		      fmenu-dummy18 
		      fmenu-dummy19 
		      fmenu-dummy20 
		      fmenu-dummy21 
		      fmenu-dummy22 
		      fmenu-dummy23 
		      fmenu-dummy24 
		      fmenu-dummy25 
		      fmenu-dummy26 
		      fmenu-dummy27 
		      fmenu-dummy28 
		      fmenu-dummy29 
		      fmenu-dummy30 ))



(export '(menu-dummys e sf df fn E-SINGLE))

(loop for dummy in menu-dummys do
      (export dummy))