;;; -*- Mode: lisp; package: user; base: 10;  Syntax: Common-lisp; -*-
;;; Lyle Borg-Graham, Equipe Cognisciences, Institut Alfred Fessard, CNRS
;;
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
;;;
;; This compiles and loads the Garnet and Surf-Hippo systems, according to the Unix environment
;; variables GARNETHOME and SURFHOME. The image is stored in the file given by
;;
;;                (format nil "~Aimage" *surf-home*) 

;; This file is loaded into a fresh CMUCL Ilisp environment, or one with GARNET added.

;; This file is assumed to be in the same directory as main-compiler.lisp (normally, surf-hippo/misc/loaders).

;; The development files are not loaded into this image.

(defvar load-development-p nil)

(load (concatenate 'string (directory-namestring *load-truename*) "main-compiler") :if-source-newer :compile)


;; This doesn't work for setting the package when the image is restarted.
;; (in-package "SURF")
;;

(defvar surf-hippo-image-name "image")
(opal:make-image (format nil "~Alib/~A" *surf-home* surf-hippo-image-name))

;; (unix::run-program (concatenate 'string *surf-home* "loaders/rm_bins") nil))   

