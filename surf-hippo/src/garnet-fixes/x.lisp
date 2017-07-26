;;; -*- Mode: LISP; Syntax: Common-lisp; Package: XLIB; Base: 10; Lowercase: Yes -*-

;;; LBG changes CLX drawing requests

(in-package "XLIB")


(defmacro compare-request ((index) &body body)
  `(macrolet ((write-card32 (index item) `(= (the fixnum ,item) (read-card32 ,index)))
	      (write-int32 (index item) `(= (the fixnum ,item) (read-int32 ,index)))
	      (write-card29 (index item) `(= (the fixnum ,item)) (read-card29 ,index))
	      (write-int29 (index item) `(= (the fixnum ,item) (read-int29 ,index)))
	      (write-card16 (index item) `(= (the fixnum ,item) (read-card16 ,index)))
	      (write-int16 (index item) `(= (the fixnum ,item) (read-int16 ,index)))
	      (write-card8 (index item) `(= (the fixnum ,item) (read-card8 ,index)))
	      (write-int8 (index item) `(= (the fixnum ,item) (read-int8 ,index))))
     (macrolet ((type-check (value type) value type nil))
       (and ,@(get-put-items index body t)))))

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

(defun draw-line (drawable gcontext x1 y1 x2 y2 &optional relative-p)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  ;; Should be clever about appending to existing buffered protocol request.
  (declare (type drawable drawable)
	   (type gcontext gcontext)
	   (type int16 x1 y1 x2 y2)
	   (type boolean relative-p))
  (let ((display (drawable-display drawable)))
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
		   (= (aref-card8 buffer-bbuf last-request-byte) *x-polysegment*)
		   (progn ;; Set buffer pointers to last request
		     (set-buffer-offset last-request-byte)
		     ;; same drawable and gcontext?
		     (or (compare-request (4)
			   (drawable drawable)
			   (gcontext gcontext))
			 (progn ;; If failed, reset buffer pointers
			   (set-buffer-offset current-boffset)
			   nil))))
	      ;; Append request
	      (progn
		;; Set new request length
		(card16-put 2 (index+ 2 (index-ash (index- current-boffset last-request-byte)
						   -2)))
		(set-buffer-offset current-boffset)
		(put-items (0)			; Insert new point
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