;;; -*- Mode: LISP -*-


(setq *lisp-predicates* (cons 'break-demo *lisp-predicates*))


(defun break-demo nil
  (format t "~%~%------------------------------BREAK--------------------------------~%~%")
  (continue-demo))

;(defun continue-demo nil
;  (format t "~%    Continue? (Please type Y)  ")
;  (let ((answer (read-char t)))
;    (cond ((or (equal answer '#\y) (equal answer '#\Y))
;	   (format t "~%~%") t)
;	  (t (continue-demo)))))

(defun continue-demo nil
  (format t "~%    Continue? (Please type Y)  ")
  (let ((answer (read)))
    (cond ((equal answer 'y)
	   (format t "~%~%") t)
	  ((equal answer 'stop)
	   (format t "~%~%       EXECUTION STOPPED !!!!~%~%")
	   (throw :error nil))
	  (t (continue-demo)))))
