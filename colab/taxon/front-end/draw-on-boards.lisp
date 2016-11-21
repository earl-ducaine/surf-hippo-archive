#+:symbolics (progn 

;(setq *dag* '((top a b c) (a e f) (c g) (g bot) (e bot) (f bot) (b bot)))

;(defun inferior-producer (x) (cdr (assoc x *dag*)))
;(defun obj-printer (obj stream) (format stream "~a" obj))  

;(use-package 'CLIM)
;(use-package 'scl)

(defvar *hierarchy-window*)

(defun inferior-producer (x) #'(lambda (x) (remove-if-not *draw-test* (node-immediate-lowers x))))
(defun obj-printer (obj stream) (format stream "~A" (remove-if-not *draw-test* (node-value obj))))

(defun init-hierarchy-window ()
   (setq *hierarchy-window*
	(tv:make-window 
		'dw:dynamic-window
		:activate-p t
		:expose-p t
		:edges-from :mouse)))

(defun draw-hierarchy-window ()
   (progn
    ; (scl:send *hierarchy-window* :expose-near '(:mouse))
     (scl:send *hierarchy-window* :set-status :selected)

     (scl:format-graph-from-root (if *filtered-graph?
                                     (graph-top *filtered-graph*)
                                     (graph-top *subsumption-graph*))
                                     
	'obj-printer
        'im-lowers-passing-draw-test; inferior-producer
	:dont-draw-duplicates t
	:orientation :horizontal
	:stream *hierarchy-window*
     )
    )
)


(defun clear-hierarchy-window ()
  (progn
    (scl:send *hierarchy-window* :set-status :selected)
    (scl:send *hierarchy-window* :clear-window)
    (scl:send *hierarchy-window* :set-status :selected)
   )
)

(defun select-hierarchy-window ()
   (scl:send *hierarchy-window* :set-status :selected)
)

)
#-symbolics (progn
(defun select-hierarchy-window ()
   (princ (format nil "Only available under Symbolics CL~%"))
   (terpri)
)

(defun clear-hierarchy-window ()
   (princ (format nil "Only available under Symbolics CL~%"))
   (terpri)
)

(defun draw-hierarchy-window ()
   (princ (format nil "Only available under Symbolics CL~%"))
   (terpri)
)

(defun init-hierarchy-window ()
   (princ (format nil "Only available under Symbolics CL~%"))
   (terpri)
)

)