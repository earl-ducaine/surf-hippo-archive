(defun p/a2p_a (pred/arity)
"Transforms eg  cylinder/3  to  (cylinder 2) "
(let* ((pra_str (string pred/arity))
       (slash (position #\/ pra_str :from-end t)))
  (if (null slash) ;; there is no backslash
      (list pred/arity -1)
    (let* ((pred_str (subseq pra_str 0 slash))
	   (arity_str (subseq  pra_str (1+ slash)))
	   (pred (intern pred_str))
	   (arity (read-from-string arity_str)))
      (list pred arity)))))

(defun p_a2p/a (pred arity)
 "Transforms eg. cylinder 3  to cylinder/3"
 (read-from-string (format nil "~a/~a" pred arity)))

   
(defun make-one-symbol (first second)
  "Generates one Symbol out of two "
  (if (and (or (symbolP first)(numberP first))
	   (or (symbolP second)(numberP second)))
      (read-from-string (concatenate 'string (princ-to-string first)
				             (princ-to-string second)))
      (error "In make-one-symbol: symbols or numbers expected: ~A ~A" first second)))
