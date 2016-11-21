;(in-package "TAXON")

(defvar *reduced-abox? NIL )
(defvar *filtered-graph? NIL)
(defvar *draw-all? NIL )
(defvar *draw-bottom? T)

(setf (get 'switch 'user::filtered-graph) 'switch-filtered-graph)
(setf (get 'switch 'user::reduced-abox) 'switch-reduced-abox)
(setf (get 'switch 'user::draw-all) 'switch-draw-all)
(setf (get 'switch 'user::draw-bottom) 'switch-draw-bottom)
(setf (get 'switch 'user::show) 'switch-show-options)




(defun switch (&optional (function nil)  (value nil))
  (if (null function)
      (switch-show-options nil)
      (let ((func (get 'switch function)))
	(if func
	    (funcall func value)
	    (progn (print "illegal switch option")
		   (throw :error nil))))))



(defun switch-reduced-abox (value)
  (setf *reduced-abox? value))

(defun switch-filtered-graph (value)
   (setf *filtered-graph? value))

(defun switch-draw-all (value)
   (setf *draw-all? value))

(defun switch-draw-bottom (value)
   (setf *draw-bottom? value))
     

(defun switch-show-options (dummy)
   (princ (format nil "~%~%Print TAXON Switch Options ~%"))
   (princ (format nil "========================== ~%"))
   (if *filtered-graph?
       (princ (format nil "Filtered Subsumption Hierarchy?   YES ~%"))
       (princ (format nil "Filtered Subsumption Hierarchy?    NO ~%")))
   (if *reduced-abox?
       (princ (format nil "Abox Reduced before Realization?  YES ~%"))
       (princ (format nil "Abox Reduced before Realization?   NO ~%")))
   (if *draw-all?
       (princ (format nil "Show only user defined concepts?   NO ~%"))
       (princ (format nil "Show only user defined concepts?  YES ~%")))
   (if *draw-bottom?
       (princ (format nil "Show The Bottom Element?          YES ~%"))
       (princ (format nil "Show The Bottom Element?           NO ~%")))
   T    
)
           


 

