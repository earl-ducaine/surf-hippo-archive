;;; abstract syntax of terms in concrete domain
;;; ro -- real-ord
;;; 01-07-92 aa


(proclaim '(inline get-predicate get-arguments))

(defun get-predicate (term)
   (first term)
)

(defun get-arguments (term)
   (rest term)
)

