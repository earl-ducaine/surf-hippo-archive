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


;; GUI Source file: annotation-file.lisp


(IN-PACKAGE "WINDOWS-HACK")


(defvar *annotation-filename* nil)


(defun dated-filename (directory-name extension &optional (time-stamp *actual-time-stamp*))
  (let ((directory-name (if (stringp directory-name)
			    directory-name
			    (namestring directory-name))))
    (REPLACE-REPEATED-CHARACTER-W-SINGLE
     (multiple-value-bind (second minute hour date month year day daylight zone)
	 (decode-universal-time (+ (truncate (* 10 time-stamp)) *universal-time-conversion-factor*))
       (concatenate 'string "/" directory-name "/" (format nil "~D_~D_~D.~A" 
							   month date year extension)))
     "/")))


#|
(defun update-annotation-file (info-function path extension &optional info-function-args)
  (let ((filename (DATED-FILEname (create-path path) extension)))
    (dribble filename)
    (apply info-function info-function-args)
    (dribble)))
|#

(defun update-annotation-file (info-function path extension &optional (info-function-args '()))
  (let ((filename (DATED-FILEname (create-path path) extension)))
    (with-open-stream (stream (open filename :if-exists :append :if-does-not-exist :create :direction :output))
      (let ((*standard-output* stream))
	(format t "~%")
	(if info-function-args
	    (apply info-function info-function-args)
	    (funcall info-function))))
    (format t ";; ~A updated..~%" filename
	    )))


(export '(update-annotation-file dated-filenam))



  
