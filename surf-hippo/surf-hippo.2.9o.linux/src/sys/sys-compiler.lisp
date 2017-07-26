;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: USER; Base: 10 -*-
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


;;; SYS Source file: sys-compiler.lisp

;; Loader file for the Surf-Hippo System files.



;;; This loader file was adapted from kr-compiler.lisp, part of the
;;; Garnet project at CMU.

#|
==================================================================
Change log:
==================================================================
|#

(in-package "USER")


;;(proclaim '(optimize (speed 3)(safety 0)(space 0)))
(with-compilation-unit (:optimize `(optimize (speed 3)(safety 0)(space 0)
				    (extensions:inhibit-warnings 0)))
  (dolist (file Surf-Hippo-Sys-Files)
    (compile-file (merge-pathnames file Surf-Hippo-Sys-Src))
    #+(or allegro explorer lispworks lucid cmu)
    (load (merge-pathnames file Surf-Hippo-Sys-Src))))


#+(or allegro explorer lispworks lucid cmu)
(setf (get :surf-hippo-modules :sys) T)



