;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-

(in-package 'opal)

(defstruct bbox
  (x1 0 :type fixnum)
  (y1 0 :type fixnum)
  (x2 0 :type fixnum)
  (y2 0 :type fixnum)
  valid-p)


(defmacro update-bbox (object bbox)
    `(let ((left (the fixnum (g-value ,object :left)))
	   (top  (the fixnum (g-value ,object :top ))))
	(setf (bbox-x1 ,bbox) left)
	(setf (bbox-y1 ,bbox) top)
	(setf (bbox-x2 ,bbox) (the fixnum (+ left (the fixnum (g-value ,object :width )))))
	(setf (bbox-y2 ,bbox) (the fixnum (+ top (the fixnum (g-value ,object :height)))))
	(setf (bbox-valid-p ,bbox) T)))

(defmacro merge-bbox (dest-bbox source-bbox)
  `(when (bbox-valid-p ,source-bbox)
    (if (bbox-valid-p ,dest-bbox)
	(progn
	  (setf (bbox-x1 ,dest-bbox)
		(the fixnum (MIN (the fixnum (bbox-x1 ,dest-bbox)) (the fixnum (bbox-x1 ,source-bbox)))))
	  (setf (bbox-y1 ,dest-bbox)
		(the fixnum (MIN (the fixnum (bbox-y1 ,dest-bbox)) (the fixnum (bbox-y1 ,source-bbox)))))
	  (setf (bbox-x2 ,dest-bbox)
		(the fixnum (MAX (the fixnum (bbox-x2 ,dest-bbox)) (the fixnum (bbox-x2 ,source-bbox)))))
	  (setf (bbox-y2 ,dest-bbox)
		(the fixnum (MAX (the fixnum (bbox-y2 ,dest-bbox)) (the fixnum (bbox-y2 ,source-bbox))))))
	(progn
	  (setf (bbox-x1 ,dest-bbox) (the fixnum (bbox-x1 ,source-bbox)))
	  (setf (bbox-y1 ,dest-bbox) (the fixnum (bbox-y1 ,source-bbox)))
	  (setf (bbox-x2 ,dest-bbox) (the fixnum (bbox-x2 ,source-bbox)))
	  (setf (bbox-y2 ,dest-bbox) (the fixnum (bbox-y2 ,source-bbox)))
	  (setf (bbox-valid-p ,dest-bbox) T)))))

(defun set-things-size (thing thing-bbox)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (s-value thing :left (bbox-x1 thing-bbox))
  (s-value thing :top (bbox-y1 thing-bbox))
  (s-value thing :width (the fixnum (- (the fixnum (bbox-x2 thing-bbox))
				       (the fixnum (bbox-x1 thing-bbox)))))
  (s-value thing :height (the fixnum (- (the fixnum (bbox-y2 thing-bbox))
					(the fixnum (bbox-y1 thing-bbox))))))


(defun initialize-item-bbox (thing thing-bbox bbox-array dummy item-array rank
								&optional rank2)
    (declare (optimize (safety 0) (speed 3) (space 1)))
;    (break)
  (let (src-bbox)
    (if rank2
	(progn
	  (s-value dummy :rank1 rank)
	  (s-value dummy :rank2 rank2))
        (s-value dummy :rank rank))
    (s-value dummy :item-values 
	(if rank2 (aref item-array rank rank2) (aref item-array rank)))
    (if rank2
	(setf (aref bbox-array rank rank2) (make-bbox))
	(setf (aref bbox-array rank) (make-bbox)))
    (setq src-bbox
	  (if rank2 (aref bbox-array rank rank2) (aref bbox-array rank)))
    (update-bbox dummy src-bbox)
    (merge-bbox thing-bbox src-bbox)
    (set-things-size thing thing-bbox)))
