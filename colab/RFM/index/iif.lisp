
(defun mapcon2 (&rest fx)
  (apply #'append (apply #'maplist fx)))

(defun mapcan2 (&rest fx)
  (apply #'append (apply #'mapcar fx)))



; (*module* iif "IIF: Indexing / Interface")


(defun iif.number-or-nil-p (item)
  (or (numberp item) (null item)))
;=========================================================================
(defun iif.tag-of-idxtree (idxtree)
  (cond	((= 3 (length idxtree)) 'label+idxtree)
        ((iif.number-or-nil-p (first (second idxtree))) 'clauses)
	(t (first (second idxtree)))))

(defun iif.s-label-f-idxtree (idxtree)
  (second (second idxtree)))

(defun iif.s-lab-f-idxtree (idxtree)
  (list (first idxtree) (second idxtree)))

(defun iif.s-clauses-f-idxtree (idxtree)
  (first idxtree))

(defun iif.s-idxtrees-f-try-trust (try-trust)
  (rest (second try-trust)))

(defun iif.s-arg-f-indextree (indextree)
  (second (second indextree)))

(defun iif.s-sequindparts-f-indextree (indextree)
  (rest (rest (second indextree))))

(defun iif.if-s.o.?.-sequindpart (sequindpart)
  (not (listp (second sequindpart))))

(defun iif.s-s.o.?.-f-sequindpart (sequindpart)
  (second sequindpart))

(defun iif.s-idxtree-f-sequindpart (sequindpart)
  (second sequindpart))

(defun iif.s-switchparts-f-sequindpart (sequindpart)
  (third sequindpart))

(defun iif.s-atom-f-switchpart (switchpart)
  (first switchpart))

(defun iif.s-idxtree-f-switchpart (switchpart)
  (second switchpart))

(defun iif.s-clauses-f-clauses (clauses)
  (first clauses))

;=========================================================================

(defun iif.mk-tree (class-proc)
  (string (gentemp "label"))
  (if idx.*dbg* (rf-print (cadr class-proc)))
  (let* ((idxtree (progn
		   (if idx.*dbg* (rf-format "~%mk-indextree ... ~%"))
		   (iif.mk-indextree (icl.s-iblock-from-class-proc
				    class-proc))))
	 (optimize1 (progn
		     (if idx.*dbg* (rf-format "flatten ... ~%"))
		     (lin.unique (flatten-idx idxtree))))
	 (optimize1.1 (progn
		       (if idx.*dbg* (rf-format "linearize ... ~%"))
		       (lin.unique
			(linearize (car optimize1)
				   (cdr optimize1)))))
	 (optimize2 (progn
		     (if idx.*dbg* (rf-format "cut-down ... ~%"))
		     (lin.unique
		      (lin.cut-down optimize1.1
				    (lin.s-label-f-idxtree (first optimize1.1))
				    idx.*numberofargs*
				    idx.*maxdepth*)))))
	(progn (if idx.*dbg* (rf-format "insert-try-trust ... ~%"))
	       (lin.unique
		(lin.insert-t-t optimize2)))))

(defun iif.mapindex (blocks)
  (mapcar #'(lambda (x) 
		    (if (numberp x)
			(list (list x) (list x))
			(iif.mk-indextree x)))
	  blocks))

(defun iif.mk-indextree (block)
  (let* ((type  (icl.s-iblock-type block)) )
        (cond ((eq type 'pblock)
	       (list
		(icl.s-clauses-from-rblock 
		 (icl.s-rblock-from-pblock block))
	       (cons 'try-trust
		     (iif.mapindex (icl.s-iblock-list-from-pblock block)))))
	      ((eq type '1block)
	       (first
	       (iif.mapindex (list (icl.s-clause-from-1block block)))))
	      ((eq type 'sblock)
	       (let* ((rblock (icl.s-rblock-from-sblock block))
		      (iblock (icl.s-iblock-from-sblock block))
		      (rest   (if (null iblock) rblock iblock))
                      (clauses (icl.s-clauses-from-rblock 
				(icl.s-rblock-from-sblock block)))
		      (seqblock (append 
				 (icl.s-seqind-arg-list-from-sblock block)
				 (list rest))))
		     (iif.seqind-list-car-cdr (car seqblock) 
					      (cdr seqblock)
					      clauses)))
	      ((eq type 'rblock)
	       (list
		(icl.s-clauses-from-rblock block)
	       (if (= (length (icl.s-clauses-from-rblock block)) 1)
		   (list (car (icl.s-clauses-from-rblock block)))
		   (cons 'try-trust
			 (iif.mapindex (icl.s-clauses-from-rblock block))))))
	      ((null type) nil)
	      (t (error "Falscher Type in Indexbaum:iif.mk-indextree")))))

(defun iif.seqind-list-car-cdr (block rest clauses)
  (list clauses
  (let* ((others (icl.s-other-from-seqind-arg block))
	 (other-block (iif.mk_block_from_element others))
        (const (mapcar #'(lambda (x)
				 (iif.element-from-seqind-elementlist x))
		       (icl.s-constant-list-from-seqind-arg block)))
	(struct (mapcar #'(lambda (x)
				  (iif.element-from-seqind-elementlist x))
		        (icl.s-structure-list-from-seqind-arg block)))
	;<----- list & nil holen ----->
	(list1 (icl.s-list-from-seqind-arg block))
	(list (if (null (cdr list1)) others list1))
	(empty-list1 (icl.s-nil-from-seqind-arg block))
	(empty-list (if (null (cdr empty-list1)) others empty-list1))
	)
  (list
   'indextree 
   (list (icl.s-arg-no-from-seqind-arg block))
    (cond ((null const)
	   (if (null others) '(const nil)
	       (list 'const other-block)))
; generate switch-on-constant if only one constant is possible ????
;	  ((and (= 1 (length const))
;	        (null (cdr others)))
;	   (list 'const (cadar const)))
	  (t (list 'const 's.o.c. (cons (list 'other other-block) const))))
    (cond ((null struct)
	   (if (null others) '(struct nil)
	       (list 'struct other-block)))
; generate switch-on-structure if only one structure is possible ????
;	  ((= 1 (length struct))
;	   (list 'struct (cadar struct)))
	  (t (list 'struct 's.o.s. (cons (list 'other other-block) struct))))
    ;<-------- list & nil bearbeiten ------->
    (if (null (cdr list)) '(list nil)
		    (list 'list (iif.mk_block_from_element list)))
    (if (null (cdr empty-list)) '(empty-list nil)
		   (list 'empty-list (iif.mk_block_from_element empty-list)))
    (list
     'var
     (if (= (length rest) 1)
	 (iif.mk-indextree (car rest))
	 (iif.seqind-list-car-cdr (car rest) (cdr rest) clauses)
	     ))))))

(defun iif.element-from-seqind-elementlist (element)
  (list
   (icl.s-element-name-from-element element)
   (iif.mk_block_from_element element)))

(defun iif.mk_block_from_element (element)
  (if 
   (null element)
   (list nil)
   (if (null (icl.s-iblock-from-element element))
       (iif.mk-indextree (list 'rblock
			       (cons 'clauses
				     (icl.s-clauses-from-element element))))
       (iif.mk-indextree (icl.s-iblock-from-element element)))))



#|
trees:
;=========================================================================

indextree: /\
--------- /  \
     clauses  clauses
     -------  -------
	 
	  /\
	 /  \
   clauses  /\
   ------- /..\
	  /. . \
  try-trust ....indextree
		---------

	  /\
	 /  \
   clauses  /\---\.......
   ------- /  \   \.......
	  /    \   \........
  indextree     arg const struct list nil var
		--- ----- ------ ---- --- ---

	  /\
	 /  \
   clauses  /\
   ------- /  \
	  /    \
     label      labelname
		+++++++++

;=========================================================================
const(struct)
-----	  /\
	 /  \
   const    indextree
	    ---------

	  /\------\
	 /  \      \
   const    s.o.c  /\
                  /..\
		 /. . \
	  constidx . . constidx
	  --------     --------

;=========================================================================
constidx
--------  /\
	 /  \
     atom    indextree
     ++++    ---------

;=========================================================================
list(nil)
----      /\
	 /  \
     list    indextree
             ---------

;=========================================================================
clauses
-------   /\
	 /..\
   number .. number
   ++++++    ++++++ 

;=========================================================================
|#
