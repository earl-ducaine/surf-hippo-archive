;;; Lable-Test unter Symbolics [mh].


(defun gen-adder (summand)
  (labels ((lab-adder (arg)
		      (+ (add-one (sub-one arg))
			 summand) )
	   (add-one (x)
		    (+ x 1) )
	   (sub-one (x)
		    (- x 1) )
	   )
	  (function lab-adder) ))



(setq adder-4 (gen-adder 4))


(funcall adder-4 10)


;;; Letzte Zeile.