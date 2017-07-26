
;;; Copyright (C) 2004-2005 Yannick Gingras <ygingras@ygingras.net>

;;; Yannick Gingras grants you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.

(in-package #:poly-pen)


(defclass gradient-segment ()
  ((left :initarg :left)
   (mid :initarg :mid)
   (right :initarg :right)
   (l-col :initarg :l-col)
   (r-col :initarg :r-col)))

(defclass gradient ()
  ((segments :initarg :segments)))

(defun get-gradient-seg-at (gradient pos)
  (when (not (<= 0.0 pos 1.0))
    (error "position not in [0..1]"))
  (loop for seg in (slot-value gradient 'segments)
     when (<= (slot-value seg 'left)
	      pos
	      (slot-value seg 'right))
     return seg))

(defun get-gradient-col-at (gradient pos)
  (with-slots (left mid right l-col r-col) (get-gradient-seg-at gradient pos)
    (let* ((len (- right left))
	   (mid (/ (- mid left) len))
	   (pos (/ (- pos left) len))
	   (fact (if (<= pos mid)
		     (/ (* .5 pos) mid)
		     (+ .5 (/ (* .5 (- pos mid)) (- 1.0 mid))))))
      (with-slots (r g b) (rgb l-col)
	(rgb (list (+ r (* fact (- (slot-value r-col 'r)
					    r)))
			    (+ g (* fact (- (slot-value r-col 'g)
					    g)))
			    (+ b (* fact (- (slot-value r-col 'b)
					    b)))))))))

(defun load-gimp-gradient (path)
  ;; TODO: handle alpha, handle non-linear segment
  (let ((lines (cdddr (with-open-file (f path)
			(loop for l = (read-line f nil nil)
			   while l
			   collect l)))))
    (make-instance
     'gradient
     :segments (mapcar
		#'(lambda (l)
		    (destructuring-bind (left mid right
					      r1 g1 b1 a1
					      r2 g2 b2 a2
					      seg-type col-type)
			(mapcar
			 #'(lambda (f)
			     (multiple-value-bind (match groups)
				 (cl-ppcre:scan-to-strings
				  "(\\d+(\\.\\d+)?)" f)
			       (if match
				   (read-from-string (elt groups 0)))))
			 (cl-ppcre:split " " l))
		      (if (zerop col-type)
			  (make-instance 'gradient-segment
					 :left left :mid mid :right right
					 :l-col (rgb (list r1 g1 b1))
					 :r-col (rgb (list r2 g2 b2)))
			  (error "Unsuported color type"))))
		lines))))

(defun make-gradient-map (nb-col gradient &optional
			  (reverse nil) (fact 1.6))
  (coerce
   (funcall (if reverse
		#'nreverse
		#'identity)
	    (loop for i in (make-log-pos nb-col fact) collect
		 (get-gradient-col-at gradient i)))
   'vector))

;; arch-tag: bfd02687-d6a2-4aaa-8b64-9172344b1f6d
