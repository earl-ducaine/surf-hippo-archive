
(defvar  *usrlit-arity* nil)
(defvar  *usrlit-arity* nil)
(defvar  *VAR-BINDINGS* nil)
(defvar  *VAR-BINDINGS* nil)
(defvar  *lisp-functions* nil)
(defvar  *next-chunkreg* nil)
(defvar  *usrlit-arity* nil)
(defvar  *usrlit-arity* nil)
(defvar  in*struc nil)
(defvar  *next-chunkreg* nil)
(defvar  *var-bindings*  nil)

; (defmacro putniv (name indicator value)
;  `(setf (get ,name ,indicator) ,value))

(defun putniv (name indicator value)
 (setf (get name indicator) value))

(defvar *predicates* nil)

(defun sel-head-n-a(x)
 (car x))

(defun sel-predicate-n-a (x)
 (car x))

(defun wandel (liste)
 (cond	((null liste) nil)
	((numberp liste) liste)
	((atom liste) (cond ((equal (subseq (symbol-name liste) 0 1 ) "_")
				`(vari 
                                  ,(intern (subseq (symbol-name liste) 1 ))
                                 )
                            ) 
			    (t liste)
                       )
        )
        ( t (cons (wandel (car liste))(wandel (cdr liste)))  )
 )
)

(defun read-db (database)
 (do* ( (lc*restclauses database (cdr lc*restclauses))
        (lc*clause (if (member (caar lc*restclauses) '(hn ft)) 
	                     (cdar lc*restclauses)
                             (car  lc*restclauses)
                   )
		    (if (member (caar lc*restclauses) '(hn ft)) 
			     (cdar lc*restclauses)
                             (car  lc*restclauses)
		    )
        )
        ; HGH some work must be done , dirty hack !
	(lc*head (sel-head-n-a lc*clause)(sel-head-n-a lc*clause))
	(lc*pred (sel-predicate-n-a lc*head)(sel-predicate-n-a lc*head))
	(lc*length (1- (length lc*head)) (1- (length lc*head)))
	(lc*intern (intern (format nil "~a/~a" lc*pred lc*length))
                   (intern (format nil "~a/~a" lc*pred lc*length)))
      )
      ( (null lc*restclauses) *predicates* )

    (if (member lc*intern *predicates*)
	(putniv  lc*intern 'clause (append (get lc*intern 'clause) 
                                   (list (wandel lc*clause))
                                   )
	)
	(progn (putniv  lc*intern 'clause (list (wandel lc*clause)) )
	       (setq *predicates* (cons lc*intern *predicates*))
	)
    ) ; if
   ) ; do*
) ; defun


(defun list-db()
 (map nil #'(lambda(x) (progn (print x) (terpri) 
                              (pprint (get x 'clause))
                              (terpri)
                       )
            ) 
      *predicates*
 )
)

(defun list-code(what)
 (if (null what)
 (map nil #'(lambda(x) (progn (rf-pprint x) (terpri) 
                              (rf-pprint (get x 'procedure))
                              (rf-terpri)
                       )
            ) 
      *predicates*
 )
 (rf-pprint (get (intern (format nil "~a" what)) 'procedure))
 )
 (rf-terpri)
)


(defun list-class (what)
 (if (null what)
 (map nil #'(lambda(x) (progn (rf-pprint x) (terpri) 
                              (rf-pprint (classify-db (list x)))
                              (rf-terpri)
                       )
            ) 
      *predicates*
 )
 (rf-pprint (classify-db (list(intern (format nil "~a" what)))))
 ) ; if
 (rf-terpri)
)


(defun del-db ()
  (dolist (x *predicates*)
    (del-proc x)))

(defun del-proc (procedure)
  (remprop procedure 'clause ) ; Source loeschen
  (del-proc*code procedure)    ; Code loeschen
  (setq *predicates* (remove procedure *predicates*)))

(defun del-proc*code (procedure)
  (remprop procedure 'procedure ))

#|
(defun loco (db)
 (setq *predicates* nil)
 (let (a)
;;;  (rf-pprint db)
;;;  (cr-return)
  (read-db db)
  (setq a (classify-db *predicates*))
;;;  (rf-pprint (setq a (classify-db *predicates*)))
;;;  (cr-return)
;;; (rf-pprint (mapcar #'code-gen-proc a))
  (mapcar #'code-gen-proc a)
 )
)
|#

(defun loco (db)
 ; INDEXING -- added by real-fun
 (setq *predicates* nil)
 (let (a)
;;;  (rf-pprint db)
;;;  (cr-return)
  (read-db db)
  (setq a (classify-db *predicates*))
;;;  (rf-pprint (setq a (classify-db *predicates*)))
;;;  (cr-return)
;;; (rf-pprint (mapcar #'code-gen-proc a))
; <------- indexing ------>
  (mapcar #'(lambda (x) (append
                         (mapcan2
                          #'(lambda (y)
                                    (icg.mk-header y (s-cg-arity-of-proc x)))
                          (iif.mk-tree x))
                         (code-gen-proc x)))
          a)
 )
)


