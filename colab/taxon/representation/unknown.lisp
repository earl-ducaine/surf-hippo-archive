;*******************************************************************************
;*
;*	Representation of Objects of Unknown Type
;*
;*******************************************************************************

;;; compiler instructions

(proclaim '(inline get-abstract-functions get-concrete-functions 
		   ))


(defstruct (unknown (:predicate unknown?)
		    (:print-function print-unknown)
		    (:conc-name object-)
		    (:include object)
		    )
  (abstract-functions 	nil :type list)
  (concrete-functions 	nil :type list)
  (print-fcts		nil :type list)
  )

(defun print-unknown (p s k)
  (terpri)
  (if (eq (deref p) p)
      (format t "Unknown Object: ~A" (get-id p))
      (format t "Unknown Object: ~A --- Definition: ~A" (get-id (deref p)) (get-id p)))
  (print-functions (object-print-fcts p))
  )

;;; constructors

(defun make-new-obj ()
  (let ((id (make-new-id))
	)
       (setf (gethash id *individual-table*) (make-unknown :id id))))

;;; selectors

(defun get-abstract-functions (obj)
  (object-abstract-functions obj))
(defun get-concrete-functions (obj)
  (object-concrete-functions obj))

;;; modifiers

(defun add-abstract-function! (fct obj)
  (push fct (object-abstract-functions obj))
  (trail! #'(lambda () (pop (object-abstract-functions obj))))
  (copy-trail! #'(lambda () (pop (object-abstract-functions (map-to-new-obj obj)))))
  )
(defun add-concrete-function! (fct obj)
  (push fct (object-concrete-functions obj))
  (trail! #'(lambda () (pop (object-concrete-functions obj))))
  (copy-trail! #'(lambda () (pop (object-concrete-functions (map-to-new-obj obj)))))
  )

(defun add-printable-function! (obj lambda-exp &rest args)
  (let ((rest-args (copy-list args))
	)
       (push (cons lambda-exp rest-args) (object-print-fcts obj))
       (trail! #'(lambda () (pop (object-print-fcts obj))))
       (copy-trail! #'(lambda () (pop (object-print-fcts (map-to-new-obj obj)))))
       ))


;;; print functions

(defun print-functions (slot)
  (dolist (fct slot)
	  (apply (car fct) (cdr fct))))

(defun print-fct-of-NCR (fct)
  (let ((user-defined-entry (get-definition-from-function fct))
        )
       (let ((restrictor  (get-restrictor-of-definition-entry user-defined-entry))
             )
	    (format t "~%~10TNCR: Restrictor: ~A" restrictor)
	    )))

(defun print-fct-of-N_PR (fct first pos pred)
  (let ((user-defined-entry (get-definition-from-function fct))
        )
       (let ((restrictor  (get-restrictor-of-definition-entry user-defined-entry))
	     (rest-pathes (rest-pathes (get-pathes-of-definition-entry user-defined-entry)))
             )
	    (format t "~%~10TNAPR: First: ~A Rest-Pathes: ~A Predicate: ~A Position: ~A"
		    first rest-pathes restrictor pos)
	    )))


;*******************************************************************************
;*
;*	Help Functions
;*
;*******************************************************************************

(defvar *symbol-counter 0)
(defvar *id-list nil)

(defun clear-new-objects ()
  (setf *symbol-counter 0 *id-list nil))

(defun make-new-id ()
  (let ((new (format nil "New~A" (incf *symbol-counter)))
	)
       (push new *id-list)
       new))

(defun new? (obj)
  (let ((id (get-id obj)))
       (and (stringp id)
	    (eq (aref id 0) #\N)
	    (eq (aref id 1) #\e)
	    (eq (aref id 2) #\w))))

(defun get-new-dereferenced-obj-of-number  (number)
  (let ((eq (find (format nil "New~A" number) *id-list :test #'equal))
	)
       (if eq (deref (get-object  eq)))))

(defun get-new-obj-of-number  (number)
  (let ((eq (find (format nil "New~A" number) *id-list :test #'equal))
	)
       (if eq (get-object eq))))

(defun get-n (number)
  (find (format nil "New~A" number)
	*id-list
	:test #'equal))
