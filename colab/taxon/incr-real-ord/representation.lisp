#|
**************************************************
	File : representation

**************************************************
|#

(proclaim '(inline make-function
		   ))

(defstruct (real-ord-obj (:print-function print-RO))
  id eq lb ub ne *> *< *>= *<= = <>)

(defvar *index -1)
(defvar max-length 1024)

(defvar *index-table (make-hash-table))
(defvar *obj-table (make-array '(1024) :element-type 'bit-vector
				     :adjustable t
				     :fill-pointer t))
(defun get-index (id)
  (gethash id *index-table))

(defun clear-index ()
  (clrhash *index-table))

(defun clear-objs ()
  (setf *index -1))

(defun clear-real-ord ()
  (clear-index)
  (clear-objs)
  (clear-filter))

(defun get-obj (index)
  (aref *obj-table index))

(defun get-RO-object (obj)
  (let ((index (get-index obj))
	)
       (if index
	   (get-obj index))))

(defun make-new-vector () (make-array '(1024) :element-type 'bit))
(defvar *null-vector (make-new-vector))

(defun make-filter () (make-array '(1024) :element-type 'bit :initial-element 1))
(defvar *filter (make-filter))
(defun filter (vector)
  (bit-and *filter vector))
(defun clear-filter ()
  (setq *filter (make-filter)))

(defun create-new-obj (id)
  (incf *index)
  (setf (gethash id *index-table) *index)
  (trail! (make-function 'destroy-object id))
  (setf (aref *obj-table *index) 
	(make-real-ord-obj :id id
			   :*>  (make-new-vector)
			   :*<  (make-new-vector)
			   :*>= (make-new-vector)
			   :*<= (make-new-vector)
			   :=   (set-bit! *index (make-new-vector))
			   :<>  (make-new-vector)))
  )
(defun destroy-object (id)
  (setf (gethash id *index-table) nil)
  (decf *index))

(defun set-eq (number obj)
  (trail! (make-function 'reset-eq obj))
  (setf (real-ord-obj-eq obj) number))
(defun reset-eq (obj)
  (setf (real-ord-obj-eq obj) nil))

(defun set-ne (list obj)
  (trail! (make-function 'reset-ne obj))
  (setf (real-ord-obj-ne obj) list))
(defun reset-ne (obj)
  (setf (real-ord-obj-ne obj) nil))

(defun set-lb (lb obj)
  (trail! (make-function 'restore-lb 
			(real-ord-obj-lb obj)
			obj))
  (setf (real-ord-obj-lb obj) lb))
(defun restore-lb (lb obj)
  (setf (real-ord-obj-lb obj) lb))
(defun restore-ub (ub obj)
  (setf (real-ord-obj-ub obj) ub))

(defun set-ub (ub obj)
  (trail! (make-function 'restore-ub
			(real-ord-obj-ub obj)
			obj))
  (setf (real-ord-obj-ub obj) ub))
(defun reset-lb (ub obj)
  (setf (real-ord-obj-ub obj) ub))

(defun set-*> (*> obj)
  (trail! (make-function 'restore-*>
			(real-ord-obj-*> obj)
			obj))
  (setf (real-ord-obj-*> obj) *>))
(defun restore-*> (*> obj)
  (setf (real-ord-obj-*> obj) *>))

(defun set-*< (*< obj)
  (trail! (make-function 'restore-*<
			(real-ord-obj-*< obj)
			obj))
  (setf (real-ord-obj-*< obj) *<))
(defun restore-*< (*< obj)
  (setf (real-ord-obj-*< obj) *<))

(defun set-*>= (*>= obj)
  (trail! (make-function 'restore-*>=
			(real-ord-obj-*>= obj)
			obj))
  (setf (real-ord-obj-*>= obj) *>=))
(defun restore-*>= (*>= obj)
  (setf (real-ord-obj-*>= obj) *>=))

(defun set-*<= (*<= obj)
  (trail! (make-function 'restore-*<=
			(real-ord-obj-*<= obj)
			obj))
  (setf (real-ord-obj-*<= obj) *<=))
(defun restore-*<= (*<= obj)
  (setf (real-ord-obj-*<= obj) *<=))

(defun set-= (eq obj)
  (trail! (make-function 'restore-=
			(real-ord-obj-= obj)
			obj))
  (setf (real-ord-obj-= obj) eq))
(defun restore-= (eq obj)
  (setf (real-ord-obj-= obj) eq))

(defun set-<> (ne obj)
  (trail! (make-function 'restore-<>
			(real-ord-obj-<> obj)
			obj))
  (setf (real-ord-obj-<> obj) ne))
(defun restore-<> (ne obj)
  (setf (real-ord-obj-<> obj) ne))

(defun set-bit! (number vector)
  (setf (apply #'sbit vector (list number)) 1)
  vector)
(defun set-bit-weakly (number vector)
  (bit-ior vector
	   (set-bit! number (make-new-vector))))

(defun reset-bit! (nr vector)
  (setf (apply #'sbit vector (list nr)) 0))

(defun set-bit-*> (index obj)
  (let ((*>obj (real-ord-obj-*> obj)))
       (trail! (make-function 'reset-bit! 
			      index
			      *>obj))
       (setf (real-ord-obj-*> obj) (set-bit-weakly index *>obj))))
(defun set-bit-*>= (index obj)
  (let ((*>=obj (real-ord-obj-*>= obj)))
       (trail! (make-function 'reset-bit!
			      index
			      *>=obj))
       (setf (real-ord-obj-*>= obj) (set-bit-weakly index *>=obj))))
(defun set-bit-*< (index obj)
  (let ((*<obj (real-ord-obj-*< obj)))
       (trail! (make-function 'reset-bit!
			      index
			      *<obj))
       (setf (real-ord-obj-*< obj) (set-bit-weakly index *<obj))))
(defun set-bit-*<= (index obj)
  (let ((*<=obj (real-ord-obj-*<= obj)))
       (trail! (make-function 'reset-bit!
			      index
			      *<=obj))
       (setf (real-ord-obj-*<= obj) (set-bit-weakly index *<=obj))))

(defun set-bit-<> (index obj)
  (let ((<>obj (real-ord-obj-<> obj)))
       (trail! (make-function 'reset-bit!
			      index
			      <>obj))
       (setf (real-ord-obj-<> obj) (set-bit-weakly index <>obj))))
(defun set-same-index (index vector)
  (do ((counter 0 (1+ counter))
       (end (1+ *index)))
      ((= end counter))
      (cond ((set? counter vector)
	     (let ((id (real-ord-obj-id (get-obj counter))))
		  (trail! (make-function 'set-id->index
					 id
					 counter))    
		  (set-id->index id index)
		  (trail! (make-function 'set-bit! counter *filter))
		  (reset-bit! counter *filter))
	    ))))

(defun set-id->index (id index)
  (setf (gethash id *index-table) index))
(defun reset-index (id)
  (setf (gethash id *index-table) nil)
  )

(defun set? (number vector)
  (= 1 (sbit vector number)))

(defun make-> (ub)
  (list '> ub))
(defun make-< (lb)
  (list '< lb))
(defun >? (b)
  (eq '> (car b)))
(defun <? (b)
  (eq '< (car b)))

(defun nr-of-b (b)
  (cadr b))
(defun rel-of-b (b)
  (car b))

(defun eq-lb-clash? (eq lb)
  (and lb (or (< eq (cadr lb))
	      (equal (list '> eq) lb))))
(defun eq-ub-clash? (eq ub)
  (and ub (or (> eq (cadr ub))
	      (equal (list '> eq) ub))))
(defun eq-ne-clash? (eq ne-list)
  (dolist (x ne-list nil)
	  (if (= x eq)
	      (return t))))
(defun lb-ub-clash-=? (lb ub)
  (and lb 
       ub 
       (or (> (cadr lb) (cadr ub)) 
	   (and (= (cadr ub) (cadr lb)) 
		(eq '> (car lb)) 
		(eq '< (car ub))))))

(defun new-eq-=? (lb ub)
  (and lb ub (= (cadr lb) (cadr ub)) (eq '>= (car lb)) (eq '<= (car ub))))
(defun get-new-eq-= (lb)
  (cadr lb))

(defun get-new-lb-= (lb1 lb2)
  (cond ((not lb1) lb2)
	((not lb2) lb1)
	((= (cadr lb1) (cadr lb2))
	 (if (eq '> (car lb1))
	     lb1
	     lb2))
	((> (cadr lb1) (cadr lb2))
	 lb1)
	(t lb2)))

(defun get-new-ub-= (ub1 ub2)
  (cond ((not ub1) ub2)
	((not ub2) ub1)
	((= (cadr ub1) (cadr ub2))
	 (if (eq '< (car ub1))
	     ub1
	     ub2))
	((< (cadr ub1) (cadr ub2))
	 ub1)
	(t ub2)))

(defun get-new-lb-> (lb1 lb2)
  (cond ((not lb1) lb2)
	((not lb2) lb1)
	((and lb1 lb2 (>= (cadr lb2) (cadr lb1)))
	 (list '> (cadr lb2)))
       (t lb1)))

(defun get-new-ub-> (ub2 ub1)
  (cond ((not ub2) ub1)
	((not ub1) ub2)
	((and (<= (cadr ub1) (cadr ub2)))
	 (list '< (cadr ub1)))	
	(t ub2)))

(defun get-new-lb->= (lb1 lb2)
  (cond ((not lb1) lb2)
	((not lb2) lb1)
	((or (> (cadr lb2) (cadr lb1))
	     (and (= (cadr lb1) (cadr lb2))
		  (eq (car lb2) '>)))
	 lb2)
	(t lb1)))

(defun get-new-ub->= (ub2 ub1)
  (if (and  ub1 ub2 (or (< (cadr ub1) (cadr ub2))
			(and (= (cadr ub1) (cadr ub2))
			     (eq (car ub1) '<))))
      ub1
      ub2))

(defun get-all ()
  (dotimes (x (1+ *index))
	   (if (set? x *filter)
	       (print (get-obj x)))))

(defun get-all-i ()
  (dotimes (x (1+ *index))
	   (princ (real-ord-obj-id (get-obj x)))
	   (princ (get-index (real-ord-obj-id (get-obj x))))
	   (terpri)))

(defun make-function (function &rest args)
  (let ((arg (copy-list args))
	)
       #'(lambda () (apply function arg))))
