;*******************************************************************************
;*
;*	File : Print Real Ord
;*
;*******************************************************************************

(defvar *print-RO-call nil)

;;; macro

(defun set-print-ro-call ()
  (setq *print-RO-call t))
(defun reset-print-ro-call ()
  (setq *print-RO-call nil))

(defmacro print-RO-call (a op b)
  `(if *print-RO-call 
       (pp-RO-call ,a ,op ,b)))


(defun print-RO (p s k)
  (if (real-ord-obj-eq p)
      (format s "ID = ~A   VALUE = ~A   ~A" (get-id (real-ord-obj-id p)) (real-ord-obj-eq p) (print-= (real-ord-obj-= p)))
      (format s "~A~A~A~A~A~A~A~A~A~A"
	      (print-id (real-ord-obj-id p))
	      (print-lb (real-ord-obj-lb p))
	      (print-ub (real-ord-obj-ub p))
	      (print-ne (real-ord-obj-ne p))
	      (print-*> (real-ord-obj-*> p))
	      (print-*>= (real-ord-obj-*>= p))
	      (print-*< (real-ord-obj-*< p))
	      (print-*<= (real-ord-obj-*<= p))
	      (print-= (real-ord-obj-= p))
	      (print-<> (real-ord-obj-<> p))
	      ))
  )
(defun print-id (x)
  (format nil "ID = ~A   " (get-id x)))
(defun print-lb (x)
  (if x
      (format nil "LB = ~A   " x)
      ""))
(defun print-ub (x)
  (if x
      (format nil "UB = ~A   " x)
      ""))
(defun print-ne (x)
  (if x
      (format nil "NE-List = ~A   " x)
      ""))
(defun print-*> (vector)
  (let ((list (make-id-list (get-obj-list vector))))
       (if list
	   (format nil "*> = ~A   " list)
	   "")))
(defun print-*< (vector)
  (let ((list (make-id-list (get-obj-list vector))))
       (if list
	   (format nil "*< = ~A   " list)
	   "")))
(defun print-*>= (vector)
  (let ((list (make-id-list (get-obj-list vector))))
       (if list
	   (format nil "*>= = ~A   " list)
	   "")))
(defun print-*<= (vector)
  (let ((list (make-id-list (get-obj-list vector))))
       (if list
	   (format nil "*<= = ~A   " list)
	   "")))
(defun print-<> (vector)
  (let ((list (make-id-list (get-obj-list vector))))
       (if list
	   (format nil "<> = ~A   " list)
	   "")))
(defun print-= (vector)
  (let ((list (make-id-list (get-obj-list-= vector))))
       (if list
	   (format nil "EQ = ~A   " list)
	   "")))

(defun get-obj-list (vector)
  (declare (special *index)
	   (special *filter))
  (do ((counter 0 (1+ counter))
       (list nil)
       (end (1+ *index)))
      ((= end counter) list)
      (if (and (set? counter vector) (set? counter *filter)
	       )
	  (setf list (cons (real-ord-obj-id (get-obj counter))
			   list)))))

(defun get-obj-list-= (vector)
  (declare (special *index))
  (do ((counter 0 (1+ counter))
       (list nil)
       (end (1+ *index)))
      ((= end counter) list)
      (if (set? counter vector)
	  (setf list (cons (real-ord-obj-id (get-obj counter))
			   list)))))


(defun pp-RO-call (a op b)
  (let ((a2 (if (numberp a)
		a
		(get-id a)))
	(b2 (if (numberp b)
		b
		(get-id b)))
	)
       (print (format nil "  RO-Call : ~A  ~A  ~A" a2 op b2))))

(defun make-id-list (l)
  (mapcar #'get-id l))
