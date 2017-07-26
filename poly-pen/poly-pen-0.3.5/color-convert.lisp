
;;; Copyright (C) 2004-2005 Yannick Gingras <ygingras@ygingras.net>

;;; Yannick Gingras grants you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.

#| re-implementation in Common Lisp of the color conversion functions
   found in colorsys.py of Python 2.3 |#

(in-package #:poly-pen)

(defun rgb->hsv (r g b)
  "channels are in range [0..1]"
  (let* ((maxc (max r g b))
	 (minc (min r g b))
	 (v maxc))
    (if (eql minc maxc)
	(list 0 0 v)
	(let* ((s (/ (- maxc minc) maxc))
	       (rc (/ (- maxc r) (- maxc minc)))
	       (gc (/ (- maxc g) (- maxc minc)))
	       (bc (/ (- maxc b) (- maxc minc)))
	       (h (mod (/ (cond ((eql r maxc)
				 (- bc gc))
				((eql g maxc)
				 (+ 2 (- rc bc)))
				(t
				 (+ 4 (- gc rc)))) 6) 1)))
	  (list h s v)))))

(defun hsv->rgb (h s v)
  "channels are in range [0..1]"
  (if (eql 0 s)
      (list v v v)
      (let* ((i (floor (* h 6)))
	     (f (- (* h 6) i))
	     (p (* v (- 1 s)))
	     (q (* v (- 1 (* s f))))
	     (t_ (* v (- 1 (* s (- 1 f)))))
	     (hint (mod i 6)))
	(case hint
	  (0 (list v t_ p))
	  (1 (list q v p))
	  (2 (list p v t_))
	  (3 (list p q v))
	  (4 (list t_ p v))
	  (5 (list v p q))))))



;; arch-tag: 7155c64e-9d1b-489e-a170-92d4011c080a
