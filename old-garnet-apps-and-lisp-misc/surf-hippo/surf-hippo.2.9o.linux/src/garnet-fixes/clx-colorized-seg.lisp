;;; -*- Mode: LISP; Syntax: Common-lisp; Package: XLIB; Base: 10; Lowercase: Yes -*-
;;; Lyle Borg-Graham, Equipe Cogniscience, Institut Alfred Fessard, CNRS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              The Surf-Hippo Neuron Simulator                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Surf-Hippo project at the  ;;;
;;; Center for Biological Information Processing, Department of     ;;;
;;; Brain and Cognitive Sciences, Massachusetts Institute of        ;;;
;;; Technology, and currently at the Equipe Cogniscience, Institut  ;;;
;;; Alfred Fessard, CNRS and has been placed in the public domain.  ;;;
;;; If you are using this code or any part of Surf-Hippo, please    ;;;
;;; contact surf-hippo@ai.mit.edu to be put on the mailing list.    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; This file is modified from Garnet source code.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;         The Garnet User Interface Development Environment.      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Garnet project at          ;;;
;;; Carnegie Mellon University, and has been placed in the public   ;;;
;;; domain.  If you are using this code or any part of Garnet,      ;;;
;;; please contact garnet@cs.cmu.edu to be put on the mailing list. ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(in-package :xlib)

;; adapted from draw-line
(proclaim '(inline draw-colorized-segment))
(defun draw-colorized-segment (drawable gcontext x1 y1 x2 y2 display)
  ;; Should be clever about appending to existing buffered protocol request.
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (type drawable drawable)
	   (type gcontext gcontext)
	   (type int16 x1 y1 x2 y2))
  (declare (type display display))
  (with-display (display)
    (force-gcontext-changes-internal gcontext)
    (with-buffer-output (display :length *requestsize*)
      (let* ((last-request-byte (display-last-request display))
	     (current-boffset buffer-boffset))
	;; To append or not append, that is the question
	(if (and (not *inhibit-appending*)
		 last-request-byte
		 ;; Same request?
		 (= (aref-card8 (the (simple-array card8 (*)) buffer-bbuf) last-request-byte) *x-polysegment*)
		 (progn;; Set buffer pointers to last request
		   (set-buffer-offset last-request-byte)
		   ;; same drawable and gcontext?
		   (or (compare-request (4)
					(drawable drawable)
					(gcontext gcontext))
		       (progn;; If failed, reset buffer pointers
			 (set-buffer-offset current-boffset)
			 nil))))
	    ;; Append request
	    (progn
	      ;; Set new request length
	      (card16-put 2 (index+ 2 (index-ash (index- current-boffset last-request-byte) -2)))
	      (set-buffer-offset current-boffset)
	      (put-items (0)		; Insert new point
			 (int16 x1 y1 x2 y2))
	      (setf (display-boffset display) (index+ buffer-boffset 8)))
	    ;; New Request
	    (progn
              (put-items (4)
			 (code *x-polysegment*)
			 (length 5)
			 (drawable drawable)
			 (gcontext gcontext)
			 (int16 x1 y1 x2 y2))
	      (buffer-new-request-number display)
	      (setf (buffer-last-request display) buffer-boffset)
	      (setf (display-boffset display) (index+ buffer-boffset 20)))))))
;  (display-invoke-after-function display)
  nil)
  