; File get-wp.lsp
;
; Author: Knut Hinkelmann
; Stand:  16.9.1991
;
; These Lisp Funktions select all the workpiece facts from the
; factbase and arrange them in a tup-list.
; The Funktion GET-WP is defined as a RELFUN Extra (in the *relfun-extras*)
; It is used to start the forward-reasoner with all the facts of the
; concerning the workpiece.

(if *relfun-extras* (if (not (member 'get-wp *relfun-extras*))
                        (setq *relfun-extras* (cons 'get-wp *relfun-extras*))))

(defun get-wp ()
  (cons 'tup (get-wp-facts *factbase*)))

(defun get-wp-facts (db)
  (cond ((null db) nil)
	((wp-fact (car db)) (cons (second (first db))
				  (get-wp-facts (rest db))))
	((get-wp-facts (rest db)))))

(defun wp-fact (clause)
  (and (eq (length clause) 2)
       (eq (car clause) 'hn)
       (member (first (second clause))
	       '(truncone cylinder ring circ cone))))

