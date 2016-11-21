;; first-char(atom char) delivers as result true if the first char of atom
;; it is used in order to find the geometry of a cutting plate.

(lispeval
(defun first-char (atom1 res)
       (cond
            ((null atom1) nil)
            ( t (eq res (intern (substring (string atom1) 0 1))))))
)
;; le180(W1 W2 W3) is true if the sum of W1, W2 and W3 is less or equal 180

(lispeval
(defun le180 (W1 W2 W3)
       (<= (+ (+ W1 W2) W3) 180)) 
)


;; ge-plus-fa(W1 W2) is true if W1 >= W2 + 5

(lispeval
(defun ge-plus-5 (W1 W2)
       (>= W1 (+ W2 5)))
)

(lispeval
(defun ge-or-zero (W1 W2)
       (or
           (>= W1 W2)
           (equal W1 0)
       ))
)
