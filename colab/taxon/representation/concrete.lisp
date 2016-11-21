;*******************************************************************************
;*
;*	Representation of Concrete Objects
;*
;*******************************************************************************

(defstruct (concrete (:predicate concrete?)
		     (:conc-name object-)
		     (:print-function print-concrete)
		     (:include object)
		     )
  (domain    nil :type symbol)
  )

(defun print-concrete (p s k)
  (print (format nil "Concrete Object ~A" (get-id p))))

(defun create-concrete-ind! (id domain)
  (let ((old (get-object id))
        )
       (if old
           old
           (let ((new (make-concrete :domain domain))
                 )
                (setf (object-id new) id)
                (setf (gethash id *individual-table*) new)
                new))))

(defun get-domain (obj)
  (object-domain obj))

