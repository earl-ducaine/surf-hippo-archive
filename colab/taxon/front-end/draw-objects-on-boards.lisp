;;(load"/home/aabecker/RCS_Colab/taxon3/front-end2/draw-objects-on-boards")
#+:symbolics (progn 

;(setq *dag* '((top a b c) (a e f) (c g) (g bot) (e bot) (f bot) (b bot)))

;(defun inferior-producer (x) (cdr (assoc x *dag*)))
;(defun obj-printer (obj stream) (format stream "~a" obj))  

;(use-package 'CLIM)
;(use-package 'scl)


(defstruct role-node
	node
)

(defvar *daw-test* 'numberp)

(defun get-X-pairs-or-object (x) 
  (if (role-node-p X)
      (cdr (role-node-node X))
      (if (and (listp X) (eq 'user::tup (car X)))
          (cdr X)
      (mapcar #'(lambda (x) (make-role-node :node x))
              (delete-if #'(lambda (pair) (funcall *daw-test* (cadr pair)))
                           (fx-value-pairs x))))
      )
  )



;(in-package 'tx)
(defun print-X-pair-or-object (X stream)
  (if (role-node-p X)
      (format stream "~A" (caar (role-node-node X)))
      (if (numberp X)
	  (format stream "~A" x)
	  (format stream "~A : ~A" X (frealize-only-if-necessary X))
	  )
      )
  )

;(in-package 'user)

(defun init-object-window ()
   (setq *object-window*
	(tv:make-window 
		'dw:dynamic-window
		;:activate-p t
		;:expose-p t
		:edges-from :mouse)
	 )
  (scl:send *object-window*
	    :set-deexposed-typeout-action :expose)
  *object-window*
  )

(defun ai-draw-object-window (concepts-objects)
	(draw-object-window
		(mapcan #'(lambda (x)
				(cond ((visible-item? x)
				        (finstances x))
				      (t (list x))))
			concepts-objects)
	)
  )
   

(defun draw-object-window (objects)
  (progn
   (scl:send *object-window* :expose-near '(:mouse))
   (scl:formatting-table
    (*object-window*)
    (scl:formatting-row
     (*object-window*)
     (dolist (object objects)
	     (scl:formatting-cell 
	      (*object-window*)
	      (scl:format-graph-from-root 
	       object
	       'print-X-pair-or-object
	       'get-X-pairs-or-object ; inferior-producer
	       :dont-draw-duplicates t
	       :test `equal
	       :key #'(lambda (x) (if (role-node-p x) (role-node-node x) x))
	       #|
	       geht leider nicht
	       :test #'(lambda (x y) 
			       (and (equal x y)
				    (not (numberp x))))
	       |#
	       ;:root-is-sequence t
	       :orientation :vertical
	       :stream *object-window*
	       )
	      )
	     )
     )
    )
   )
  )


(defun clear-object-window ()
  (progn
   ;(scl:send *object-window* :set-status :selected)
   (scl:send *object-window* :clear-window)
   ;(scl:send *object-window* :set-status :selected)
   )
  )

(defun select-object-window ()
 ; (scl:send *object-window* :expose-near '(:mouse))
  (scl:send *object-window* :set-status :selected)
  )

)
#-symbolics (progn
	     (defun select-object-window ()
		    (princ (format nil "Only available under Symbolics CL~%"))
		    (terpri)
		    )
	     
	     (defun clear-object-window ()
		    (princ (format nil "Only available under Symbolics CL~%"))
		    (terpri)
		    )
	     
	     (defun draw-object-window ()
		    (princ (format nil "Only available under Symbolics CL~%"))
		    (terpri)
		    )
	     
	     (defun init-object-window ()
		    (princ (format nil "Only available under Symbolics CL~%"))
		    (terpri)
		    )
	     
	     )
