#|
(defun fw (tag &rest args)
  (cond ((or (eq tag 'bf)
             (eq tag 'df))
         (let* ((goal (read-from-string (format nil
                                                "((~A (inst ~A ) id))"
                                                (if (eq tag 'bf)
                                                    'bf-all
                                                    'df-all)
                                                (cons 'tup args)))))
                (cond (*fw-interpreter-mode*
                            ;;;
                            ;;; use interpreter to answer query
                            ;;;
                        (let ((res (and-process (deanon-request 
                                   (if *rfi-static* 
                                       (flatten-request goal) 
                                       goal)) 
                                  '((bottom)) 
                                  (list *factbase* 
                                        *forward-rules* 
                                        *fc-strategies* 
                                        (if (null *hn-rulebase*) 
                                            (rl2hn nil *rule-database*) 
                                            *hn-rulebase*)) 
                                  1 
                                  'once))) 
                             (and res 
                                  (cons (ultimate-instant (un-inst (car res)) 
                                                          (cadr res)) 
                                        (reverse (answer-bindings 
                                                  (cadr res) 
                                                  (cadr res)))))))
                      (t
;;;;;;;;;;;;;;;; (init-retain-system)

                       (emulate (transform-query-for-emulator goal))))))
        ((eq tag 'goal) 
         (colab-magiceval-forward args))
        (t 
         (print "Invalid request for funktion fw")
         (values))))


|#


;;; to handle the tupof in the WAM
(setf *lisp-extras* (union '(collect-tupof-A1) *lisp-extras*))
(defvar *tupof-stack* '() "stack, to store the collections of the tupof")

(defun init-tupof ()
  "Enter a new stack in the *tupof-stack*"
  (setf *tupof-stack* (list nil *tupof-stack*)))

(defun leave-tupof()
  "Leave a stack in the *tupof-stack*. Returns
the last stack"
  (let ((result (car *tupof-stack*)))
    (setf *tupof-stack* (cdr *tupof-stack*))
    result))

(definstr collect-tupof (Ai)(NAT):standard (gwam.collect-tupof
  "Push the value in argument-reg Ai (--> the result of the last
ft clause) onto the top of the *tupof-stack*"
  (let ((entry (emu-lisp-wandel (argument-reg Ai))))
    (setf (car *tupof-stack*) (cons entry (car *tupof-stack*))))))


(defun fw (command &rest userline)
  (let* ((fkt  (col-function (assoc command *col-list-of-forward*))))
    (cond ((null fkt)
	     (forward-else (cons command userline)))
	  ((eq fkt 'colab-rfquery-forward)
	   (if *fw-interpreter-mode*
	       (progn
		 (when *automatic-mode*
		       (progn (or *forward-rules*
				  (colab-fwtransform-forward nil))
			      (or *hn-rulebase*
				  (colab-splitrules-forward nil))))
		 (let ((*rfi-database*
			(append *rfi-database*
				*fw-prelude*
				*factbase* 
				*forward-rules* 
				*fc-strategies* 
				*hn-rulebase*
				)))
		   (apply #'rf (cons 'tupof userline))))
	     (let* ((*readtable* *rfi-readtable*)
		      (goals (read-from-string (format nil "~A" userline)))
		      (rev-goals (reverse goals))
		      (pre-goals (reverse (cdr rev-goals)))
		      (last-goal (car rev-goals))
		      (goal (append pre-goals `((collect-tupof ,last-goal) unknown)))
		      (*gwam-silent* T))
	       (init-tupof)
	     
	       (emulate (transform-query-for-emulator goal))
	       (reverse (leave-tupof)))))
	  
	  (t (funcall fkt userline)))))

 
