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

(defmacro read-card8 (byte-index)
  `(aref-card8 (the (simple-array card8 (*)) buffer-bbuf) (index+ buffer-boffset ,byte-index)))

(defmacro read-int8 (byte-index)
  `(aref-int8 (the (simple-array int8 (*)) buffer-bbuf) (index+ buffer-boffset ,byte-index)))

(defmacro read-card16 (byte-index)
  #+clx-overlapping-arrays
  `(aref-card16 buffer-wbuf (index+ buffer-woffset (index-ash ,byte-index -1)))
  #-clx-overlapping-arrays
  `(aref-card16 (the (simple-array card16 (*)) buffer-bbuf) (index+ buffer-boffset ,byte-index)))

(defmacro read-int16 (byte-index)
  #+clx-overlapping-arrays
  `(aref-int16 buffer-wbuf (index+ buffer-woffset (index-ash ,byte-index -1)))
  #-clx-overlapping-arrays
  `(aref-int16 (the (simple-array int16 (*)) buffer-bbuf) (index+ buffer-boffset ,byte-index)))

(defmacro read-card32 (byte-index)
  #+clx-overlapping-arrays
  `(aref-card32 buffer-lbuf (index+ buffer-loffset (index-ash ,byte-index -2)))
  #-clx-overlapping-arrays
  `(aref-card32 (the (simple-array card32 (*)) buffer-bbuf) (index+ buffer-boffset ,byte-index)))

(defmacro read-int32 (byte-index)
  #+clx-overlapping-arrays
  `(aref-int32 buffer-lbuf (index+ buffer-loffset (index-ash ,byte-index -2)))
  #-clx-overlapping-arrays
  `(aref-int32 (the (simple-array int32 (*)) buffer-bbuf) (index+ buffer-boffset ,byte-index)))

(defmacro read-card29 (byte-index)
  #+clx-overlapping-arrays
  `(aref-card29 buffer-lbuf (index+ buffer-loffset (index-ash ,byte-index -2)))
  #-clx-overlapping-arrays
  `(aref-card29 (the (simple-array card29 (*)) buffer-bbuf) (index+ buffer-boffset ,byte-index)))

(defmacro write-card8 (byte-index item)
  `(aset-card8 (the card8 ,item) (the (simple-array card8 (*)) buffer-bbuf) (index+ buffer-boffset ,byte-index)))

(defmacro write-int8 (byte-index item)
  `(aset-int8 (the int8 ,item) (the (simple-array int8 (*)) buffer-bbuf) (index+ buffer-boffset ,byte-index)))

(defmacro write-card16 (byte-index item)
  #+clx-overlapping-arrays
  `(aset-card16 (the card16 ,item) buffer-wbuf
		(index+ buffer-woffset (index-ash ,byte-index -1)))
  #-clx-overlapping-arrays
  `(aset-card16 (the card16 ,item) (the (simple-array card16 (*)) buffer-bbuf)
		(index+ buffer-boffset ,byte-index)))

(defmacro write-int16 (byte-index item)
  #+clx-overlapping-arrays
  `(aset-int16 (the int16 ,item) buffer-wbuf
	       (index+ buffer-woffset (index-ash ,byte-index -1)))
  #-clx-overlapping-arrays
  `(aset-int16 (the int16 ,item) (the (simple-array int16 (*)) buffer-bbuf)
	       (index+ buffer-boffset ,byte-index)))

(defmacro write-card32 (byte-index item)
  #+clx-overlapping-arrays
  `(aset-card32 (the card32 ,item) buffer-lbuf
		(index+ buffer-loffset (index-ash ,byte-index -2)))
  #-clx-overlapping-arrays
  `(aset-card32 (the card32 ,item) (the (simple-array card32 (*)) buffer-bbuf)
		(index+ buffer-boffset ,byte-index)))

(defmacro write-int32 (byte-index item)
  #+clx-overlapping-arrays
  `(aset-int32 (the int32 ,item) buffer-lbuf
	       (index+ buffer-loffset (index-ash ,byte-index -2)))
  #-clx-overlapping-arrays
  `(aset-int32 (the int32 ,item) (the (simple-array int32 (*)) buffer-bbuf)
	       (index+ buffer-boffset ,byte-index)))

(defmacro write-card29 (byte-index item)
  #+clx-overlapping-arrays
  `(aset-card29 (the card29 ,item) buffer-lbuf
		(index+ buffer-loffset (index-ash ,byte-index -2)))
  #-clx-overlapping-arrays
  `(aset-card29 (the card29 ,item) (the (simple-array card29 (*)) buffer-bbuf)
		(index+ buffer-boffset ,byte-index)))

#|
(defmacro compare-request ((index) &body body)
  `(macrolet ((write-card32 (index item) `(= ,item (read-card32 ,index)))
	      (write-int32 (index item) `(= ,item (read-int32 ,index)))
	      (write-card29 (index item) `(= (the card29 ,item) (the card29 (read-card29 ,index))))
	      (write-int29 (index item) `(= ,item (read-int29 ,index)))
	      (write-card16 (index item) `(= ,item (read-card16 ,index)))
	      (write-int16 (index item) `(= ,item (read-int16 ,index)))
	      (write-card8 (index item) `(= ,item (read-card8 ,index)))
	      (write-int8 (index item) `(= ,item (read-int8 ,index))))
     (macrolet ((type-check (value type) value type nil))
       (and ,@(get-put-items index body t)))))
|#

(defmacro compare-request ((index) &body body)
  `(macrolet ((write-card32 (index item) `(= (the fixnum ,item) (the fixnum (read-card32 ,index))))
	      (write-int32 (index item) `(= (the fixnum ,item) (the fixnum (read-int32 ,index))))
	      (write-card29 (index item) `(= (the fixnum ,item) (the fixnum (read-card29 ,index))))
	      (write-int29 (index item) `(= (the fixnum ,item) (the fixnum (read-int29 ,index))))
	      (write-card16 (index item) `(= (the fixnum ,item) (the fixnum (read-card16 ,index))))
	      (write-int16 (index item) `(= (the fixnum ,item) (the fixnum (read-int16 ,index))))
	      (write-card8 (index item) `(= (the fixnum ,item) (the fixnum (read-card8 ,index))))
	      (write-int8 (index item) `(= (the fixnum ,item) (the fixnum (read-int8 ,index)))))
     (macrolet ((type-check (value type) value type nil))
       (and ,@(get-put-items index body t)))))


(defun get-put-items (index type-args putp &optional body-function)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (type (or null function) body-function)
	   #+clx-ansi-common-lisp
	   (dynamic-extent body-function)
	   #+(and lispm (not clx-ansi-common-lisp))
	   (sys:downward-funarg body-function))
  ;; Given a lists of the form (type item item ... item)
  ;; Calls body-function with four arguments, a function name,
  ;; index, item name, and optional arguments.
  ;; The results are appended together and retured.
  (unless body-function
    (setq body-function
	  #'(lambda (type index item args)
	      `((check-put ,index ,item ,type ,@args)))))
  (do* ((items type-args (cdr items))
	(type (caar items) (caar items))
	(args nil nil)
	(result nil)
	(sizes nil))
       ((endp items) (values result index sizes))
    (when (consp type)
      (setq args (cdr type)
	    type (car type)))
    (cond ((member type '(return buffer)))
	  ((eq type 'mask);; Hack to enable mask-get/put to return multiple values
	   (setq result
		 (append result (if putp
				    (mask-put index (cdar items) body-function)
				    (mask-get index (cdar items) body-function)))
		 index nil))
	  (t (do* ((item (cdar items) (cdr item))
		   (increment (index-increment type)))
		  ((endp item))
	       (when (constantp index)
		 (case increment	;Round up index when needed
		   (2 (setq index (wround index)))
		   (4 (setq index (lround index)))))
	       (setq result
		     (append result (funcall body-function type index (car item) args)))
	       (when (constantp index)
		 ;; Variable length requests have null length increment.
		 ;; Variable length requests set the request size 
		 ;; & maintain buffer pointers
		 (if (null increment) 
		     (setq index nil)
		     (progn
		       (incf (the fixnum index) (the fixnum increment))
		       (when (and increment (zerop (the fixnum increment))) (setq increment 1))
		       (pushnew (the fixnum (* (the fixnum increment) 8)) sizes)))))))))

(defmacro put-items ((index) &body body)
  `(progn ,@(get-put-items index body t)))

;; LBG 8/23/99
;; Added branch to DRAW-LINE-RELATIVE, depending on RELATIVE-P, since this avoids consing with the
;; compiled DRAW-LINE when the incf's of x2 and y2 are removed out. ???
(defun draw-line-fast (drawable gcontext x1 y1 x2 y2 &optional relative-p (drawable-display (drawable-display drawable)))
  ;; Should be clever about appending to existing buffered protocol request.
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (type drawable drawable)
	   (type gcontext gcontext)
	   (type int16 x1 y1 x2 y2)
	   (type boolean relative-p))
  (if relative-p
      (draw-line-relative drawable gcontext x1 y1 x2 y2)
      (let ((display (or drawable-display (drawable-display drawable))))
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
		    (progn ;; Set new request length
		      (card16-put 2 (index+ 2 (index-ash (index- current-boffset last-request-byte)
							 -2)))
		      (set-buffer-offset current-boffset)
		      (put-items (0)	; Insert new point
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
	(display-invoke-after-function display)))
  nil)

(defun draw-line-relative (drawable gcontext x1 y1 x2 y2)
  ;; Should be clever about appending to existing buffered protocol request.
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (type drawable drawable)
	   (type gcontext gcontext)
	   (type int16 x1 y1 x2 y2)
	   ; (type boolean relative-p)
	   )
  (let ((display (drawable-display drawable))
	(relative-p t))
    (declare (type display display))
    (when relative-p
      (incf x2 x1)
      (incf y2 y1))
      
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
		(card16-put 2 (index+ 2 (index-ash (index- current-boffset last-request-byte)
						   -2)))
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
    (display-invoke-after-function display))
  nil)


#|
(defun draw-lines (drawable gcontext points &key relative-p fill-p (shape :complex))
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   (type sequence points) ;(repeat-seq (integer x) (integer y))
	   (type boolean relative-p fill-p)
	   (type (member :complex :non-convex :convex) shape))
  (if fill-p
      (fill-polygon drawable gcontext points relative-p shape)
    (with-buffer-request ((drawable-display drawable)  *x-polyline* :gc-force gcontext)
      ((data boolean) relative-p)
      (drawable drawable)
      (gcontext gcontext)
      ((sequence :format int16) points))))

;; Internal function called from DRAW-LINES
(defun fill-polygon (drawable gcontext points relative-p shape)
  ;; This is clever about appending to previous requests.  Should it be?
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   (type sequence points)		;(repeat-seq (integer x) (integer y))
	   (type boolean relative-p)
	   (type (member :complex :non-convex :convex) shape))
  (with-buffer-request ((drawable-display drawable)  *x-fillpoly* :gc-force gcontext)
    (drawable drawable)
    (gcontext gcontext)
    ((member8 :complex :non-convex :convex) shape)
    (boolean relative-p)
    ((sequence :format int16) points)))

|#

