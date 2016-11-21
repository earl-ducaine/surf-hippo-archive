;*******************************************************************************
;*
;*	File : Display Print Functions
;*
;*******************************************************************************

(defun DPF ()
  (print-line)
  (cond (*print-rule
	 (format t "~%Mode		: Print-Rules")
	 (format t "~2%Alternative	: (reset-print-rules)"))
	(t
	 (format t "~%Mode		: No Print-Rules")
	 (format t "~2%Alternative	: (set-print-rules)"))
	)
  (print-line)
  (cond (*print-ro-call
	 (format t "~%Mode 		: Print-Real-Ord-Call")
	 (format t "~2%Alternative 	: (reset-print-ro-call)"))
	(t
	 (format t "~%Mode		: No Print-Real-Ord-Call")
	 (format t "~2%Alternative	: (set-print-ro-call)"))
	)
  (print-line)
  (cond (*print-clash
	 (format t "~%Mode		: Print-Clash")
	 (format t "~2%Alternative	: (reset-print-clash)")
	 )
	(t
	 (format t "~%Mode		: No Print-Clash")
	 (format t "~2%Alternative	: (set-print-clash)"))
	 )
  (print-line)
  (cond ((eq *print-cnf 'pp)
	 (format t "~%Mode		: Pretty-Print-CNF")
	 (format t "~2%Alternative	: (set-print-name-of-cnf)")
	 (format t "~%Alternative	: (set-print-slot-of-cnf)")
	 )
	((eq *print-cnf 'name)
	 (format t "~%Mode              : Print-Name-of-CNF")
	 (format t "~2%Alternative	: (set-pretty-print-of-cnf)")
	 (format t "~%Alternative	: (set-print-slot-of-cnf)")
	 )
	((eq *print-cnf 'all)
	 (format t "~%Mode              : Print-Slot")
	 (format t "~2%Alternative	: (set-pretty-print-of-cnf)")
	 (format t "~%Alternative	: (set-print-slot-of-cnf)")
	 ))
  (print-line)
  (cond ((eq *print-abstract 'id)
	 (format t "~%Mode              : Print-ID-of-Abstract")
	 (format t "~2%Alternative      : (set-print-type+id-of-abstract)")
	 (format t "~%Alternative       : (set-pretty-print-of-abstract)")
	 )
	((eq *print-abstract 'type+id)
	 (format t "~%Mode              : Print-Type+Id-of-Abstract")
	 (format t "~2%Alternative      : (set-pretty-print-of-abstract)")
	 (format t "~%Alternative       : (set-print-id-of-abstract)")
	 )
	((eq *print-abstract 'pp)
	 (format t "~%Mode		: Pretty-Print-of-Abstract")
	 (format t "~2%Alternative	: (set-print-id-of-abstract)")
	 (format t "~%Alternative	: (set-print-type+id-of-abstract)")
	 ))
  (print-line)
  )

(defun print-line ()
  (format t "~%------------------------------------------------------------------"))



