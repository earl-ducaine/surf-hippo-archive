(defvar *trace-assert-ind? NIL)
(defvar *trace-concept-closure NIL)
(defvar *trace-subsumption NIL)
(defvar *trace-classify NIL)
(defvar *trace-realize NIL)


(setf (get 'trace 'assert-ind?) 'trace-assert-ind?)
(setf (get 'trace 'concept-closure) 'trace-concept-closure)
(setf (get 'trace 'subsumption) 'trace-subsumption)
(setf (get 'trace 'user::classify) 'trace-classify)
(setf (get 'trace 'user::realize) 'trace-realize)

(defun my-trace (function)
  (let ((func (get 'trace function)))
    (if func
        (funcall func t)
	(print "illegal trace option"))))

(defun my-untrace (function)
  (let ((func (get 'trace function)))
    (if func
        (funcall func nil)
	(print "illegal trace option"))))

(defun trace-assert-ind? (value)
  (setf *trace-assert-ind? value)
  (if *trace-assert-ind?
      (trace assert-ind?)
      (untrace assert-ind?))
T)
(defun trace-concept-closure (value)
  (setf *trace-concept-closure value)
  (if *trace-concept-closure
      (trace concept-closure)
      (untrace concept-closure))
 T)
(defun trace-subsumption (value)
  (setf *trace-subsumption value))
(defun trace-classify (value)
  (setf *trace-classify value))
(defun trace-realize (value)
  (setf *trace-realize value))
