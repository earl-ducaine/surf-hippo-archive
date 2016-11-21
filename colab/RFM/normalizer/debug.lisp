(defvar *tracer-level* 0 "default tracer-level is: no trace")
(defvar *error-level* 0 "default error-level is: ignore errors")
(defvar *error-stream* nil "post error-messsage to this stream if *error-level* = 4.
                            Otherwise the current clause stays here")

(defun set-trace (level) (declare (specials *tracer-level*)) (setq *tracer-level* level))

;;;
;;;     ******** trace-in-handler ********   
;;;

(defun set-err (level) (declare (specials *error-level*)) 
       (setq *error-level* level))

(defun trace-in-handler (func-name args environment &optional (level *tracer-level*))
  (cond ((or (not (numberp level))
             (<= level 0)
             (> level 2))
         level)
        (t 
         (progn (terpri) (princ "Observe Function:")
                (princ func-name) (terpri)
                (if (eql level 2)
                  (progn (princ (documentation func-name 'function)) (terpri)))
                (trace-args args)
                (trace-env environment)))))

;;;
;;;     ******** trace-out-handler ********   
;;;

(defun trace-out-handler (func-name value &optional (level *tracer-level*))
  (cond ((or (not (numberp level))
             (<= level 0)
             (> level 2))
         value)
        (t (progn (terpri) (princ "*** ") (princ func-name) (princ " *** returns:")
                  (rf-pprint value)
                  value))))


(defun trace-args (argvalue-list)
  (cond ((null argvalue-list) nil)
        (t
         (progn (princ "   *** Arguments ***") (terpri)
                (mapcar #'trace-name-value argvalue-list)
                nil))))


(defun trace-env (argvalue-list)
  (cond ((null argvalue-list) nil)
        (t
         (progn (princ "    *** Evironment ***")(terpri)
                (mapcar #'trace-name-value argvalue-list)
                nil))))

(defun trace-name-value (name-value)
  (rf-pprint (first name-value)) (princ " :")
  (rf-pprint (second name-value))
  (terpri))



;;;
;;;       ********   error-handler  *********
;;;

(defun error-handler (err-no error-args) ; hanldes warnings also
  (declare (specials *error-level* *error-stream*))
  (cond ((or (<= *error-level* 0) (> *error-level* 4))
         t)
        (t 
         (let* ((proc-name**clause (first *error-stream*))
                (proc-name (first proc-name**clause))
                (clause (second proc-name**clause)))
           (case err-no ; 
             (20 ; occur-check
              (case *error-level*
                ((1 2) ; print on screen
                 (print-error "Occur-check has failed"
                              proc-name clause error-args t)
                 (if (eql *error-level* 2) (break)))
                (4 ; print on *error-stream*
                 (print-error "Occur-check has failed"
                              proc-name clause error-args nil))))
                 
             (21 ; unknown-detection
              (case *error-level*
                ((1 2) ; print on screen
                 (print-error "Unknown-detection"
                              proc-name clause error-args t)
                 (if (eql *error-level* 2) (break)))
                (4 ; print on *error-stream*
                 (print-error "Unknown-detection"
                              proc-name clause error-args nil))))
             (22 ; unknown-generation
              (case *error-level*
                ((1 2) ; print on screen
                 (print-error "Unknown-generation"
                              proc-name clause error-args t)
                 (if (eql *error-level* 2) (break)))
                (4 ; print on *error-stream*
                 (print-error "Unknown-generation"
                              proc-name clause error-args nil)))))))))

(defun print-error (err-mess proc-name clause err-args on-screen)
  (declare (specials *error-level* *error-stream*))
  (cond (on-screen
         (terpri) (princ "***** WARNING ****")(terpri)
         (princ err-mess) (terpri)
         (princ "In procedure: " ) (princ proc-name) (terpri)
         (princ "In clause: ")(terpri)(rf-pprint clause)
         (princ "While normalizing: ") (rf-pprint err-args))
         (t
          (setq *error-stream* 
                (cons 
                 (list err-mess 
                       "In procedure:"  proc-name
                       "In clause: " clause 
                       "While normalizing: " err-args)
                 (s-rest *error-stream*))))))