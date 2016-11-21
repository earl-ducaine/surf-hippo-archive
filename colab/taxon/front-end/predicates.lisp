;; 01-07-92 aa

(defun make-internal-rep-of-predicate (domain args body)
  (make-complex-predicate-rule args body))

(defun make-internal-rep-of-neg-predicate (domain args body)
  (make-complex-predicate-rule args (make-negation body)))
