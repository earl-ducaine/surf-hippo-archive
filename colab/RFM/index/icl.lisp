#|

(*module* icl "ICL: Indexing / Classifier")

(*export*
  mk-index-struct ; used by CLA : absyn.macro : mk-procedure
  )

|#



#|

----------------------------------------------------------------
EBNF for classified clauses - indexing part - (c) Michael Sintek
----------------------------------------------------------------

indexing ::= (indexing [ <iblock> ] )

iblock ::= <pblock> | <sblock>

pblock ::= (pblock <rblock> { <sblock> | <1block> }+ )

rblock ::= (rblock <clauses> { arg-col }+ )

clauses ::= (clauses { <clause-number> }+ )

arg-col ::= (arg <arg-number> { <base-type> }+ )

base-type ::= <const> | <struct> | <var>

const ::= (const <symbol>)

struct ::= (struct <symbol> <arity>)

var ::= (var <symbol>)

1block ::= (1block <clauses> { arg-col }+ )

sblock ::= (sblock <rblock> <seqind> [ <pblock> ] )

seqind ::= (seqind { <seqind-arg> }+ )

seqind-arg ::= (arg <arg-number>
		    (info <inhomogenity>)
		    <constants>
		    <structures>
		    <lists>
		    <empty-lists>
		    [ <others> ])

constants ::= (const { <element> }* )

structures ::= (struct { <element> }* )

element ::= ( <element-name> <clauses> [ <iblock> ] )

element-name ::= <symbol> | ( <symbol> <arity> )

lists ::= (list <clauses> [ <iblock> ] )

empty-lists ::= (nil <clauses> [ <iblock> ] )

others ::= (other <clauses> [ <iblock> ] )


Explanations:
-------------

iblock = indexed block
pblock = partitioned block
sblock = standard index block
1block = block consisting of only one clause
rblock = raw block containing the initial data

seqind = sequential indexing

arg-col = argument column

others = (possibly indexed) clauses for elements not occurring in any
	 hash table


|#



; -------
; patches
; -------

#|
; CLAS: absyn.macro:
; ------------------

(defmacro mk-procedure (procedure*name clause*count list*of*clause)
  `(cons 'proc
         (cons ,procedure*name
               (cons ,clause*count
                     (cons (mk-index-struct ,procedure*name  ;; real-fun
                                            ,clause*count
                                            ,list*of*clause)
                           ,list*of*clause)))))



; CG: absynt.lsp
; --------------

;;;;;; to get the list of clause_classifications
(defmacro s-cg-clause_classifications (procedure)
 `(cddddr ,procedure)) 
|#



; ----------
; selectors:
; ----------

; CG:
; ---

;(*import* CG : absynt.lsp : ...

(defmacro s-var-name (term-classification)
  `(cadar ,term-classification))


; ICL:
; ----

; classified procedure:

(defun icl.s-iblock-from-class-proc (classified-procedure)
  (cadr (cadddr classified-procedure)))

; iblock:

(defun icl.s-iblock-type (iblock)
  ; nil, pblock, sblock, 1block
  (car iblock))


; pblock:

(defun icl.s-rblock-from-pblock (pblock)
  (cadr pblock))

(defun icl.s-iblock-list-from-pblock (pblock)
  (cddr pblock)) ; cannot be another pblock or rblock!


; sblock:

(defun icl.s-rblock-from-sblock (sblock)
  (cadr sblock))

(defun icl.s-seqind-arg-list-from-sblock (sblock)
  (cdaddr sblock))

(defun icl.s-iblock-from-sblock (sblock)
  (cadddr sblock))


; 1block: 

(defun icl.s-clause-from-1block (1block)
  (cadadr 1block))

(defun icl.s-arg-col-list-from-1block (1block)
  (cddr 1block))


; rblock:

(defun icl.s-clauses-from-rblock (rblock)
  (cdadr rblock))

(defun icl.s-arg-col-list-from-rblock (rblock)
  (cddr rblock))


; arg-col:

(defun icl.s-arg-no-from-arg-col (arg-col)
  (cadr arg-col))

(defun icl.s-it-list-from-arg-col (arg-col)
  (cddr arg-col))


; seqind-arg:

(defun icl.s-arg-no-from-seqind-arg (seqind-arg)
  (cadr seqind-arg))

(defun icl.s-info-from-seqind-arg (seqind-arg)
  (caddr seqind-arg))

(defun icl.s-constant-list-from-seqind-arg (seqind-arg) ; -> element list
  (cdr (cadddr seqind-arg)))

(defun icl.s-structure-list-from-seqind-arg (seqind-arg) ; -> element list
  (cdar (cddddr seqind-arg)))

(defun icl.s-list-from-seqind-arg (seqind-arg) ; -> 1 element
  (cadr (cddddr seqind-arg)))

(defun icl.s-nil-from-seqind-arg (seqind-arg) ; -> 1 element
  (caddr (cddddr seqind-arg)))

(defun icl.s-other-from-seqind-arg (seqind-arg) ; -> 1 element
  (cadddr (cddddr seqind-arg)))

(defun icl.s-var-from-raw-seqind-arg (seqind-arg) ; -> 1 element
  (cadr (cddddr seqind-arg)))


; element (in constant list, structure list, or list, nil):

(defun icl.s-element-name-from-element (element) ; doesn't make sense
  (car element))                                 ; on list, nil, other

(defun icl.s-clauses-from-element (element)
  (cdadr element))

(defun icl.s-iblock-from-element (element)
  (caddr element))



; ---------------
; mk-index-struct
; ---------------


(defun mk-index-struct (procedure-name clause-count list-of-clauses)
  (cons 'indexing
	(when (>=  clause-count idx.*min-no-of-proc-clauses*)
	      (let ((it-heads (mapcar #'icl.mk-it-head list-of-clauses)))
		   (when (car it-heads) ; args exist
			 (let* ((rblock (icl.gen-rblock it-heads))
				(iblock (icl.gen-iblock rblock)))
			       (icl.nil-or-list iblock)))))))
	      



; --------------------
; make index type head
; --------------------

(defun icl.mk-it-head (clause)
  (let ((head-chunk (car (s-cg-chunks clause))))
       (icl.mk-it-head2
	(s-cg-arglist_classification 
	 (s-cg-fac_list (s-cg-chunk_head_literal head-chunk)))
        (icl.get-it-bindings (s-cg-chunk_hd_cgfpl head-chunk)))))

(defun icl.mk-it-head2 (old-head it-bindings)
  (unless (null old-head)
	  (cons
	   (let ((index-type (icl.g-index-type (car old-head))))
		(cond ((eq (car index-type) 'var)
		       (cond ((cdr (assoc (cadr index-type) it-bindings)))
			     (T index-type)))
		      (T index-type)))
	   (icl.mk-it-head2 (cdr old-head) it-bindings))))



; get index type bindings

(defun icl.get-it-bindings (guards*fpl) ; fpl = first premise literal
  (mapcan #'icl.get-it-binding guards*fpl))

(defun icl.get-it-binding (guard)
  ; returns (<it>) or nil
  (when (consp guard) ; ignore constant "first_premise_literals"
  	(when (eq (s-cg-functor guard) 'is)
	      (let ((arglist (s-cg-arglist_classification guard)))
		   (when (arg-var-p (car arglist))
			 (cons (cons (s-var-name (car arglist))
				     (icl.g-index-type (cadr arglist)))
			       nil))))))


; generate index types (only basic types: var, const, struct)

(defun icl.g-it-const (term)
  (when (atom term)
	(list 'const term)))

(defun icl.g-it-var (term)
  (when (arg-var-p term)
	(list 'var (s-var-name term))))

(defun icl.g-it-struct (term)
  (when (cg-inst-p term)
	(list 'struct
	      (cg-s-inst-functor term)
	      (length (cg-s-inst-funargs term)))))

(defun icl.g-index-type (term)
  (cond ((icl.g-it-const term))
	((icl.g-it-var term))
	((icl.g-it-struct term))
	(T (error "icl.g-index-type: unknown type ~A" term))))


; index types type tests ...

(defun icl.it-const-p (it)
  (eq (car it) 'const))

(defun icl.it-var-p (it)
  (eq (car it) 'var))

(defun icl.it-struct-p (it)
  (eq (car it) 'struct))

(defun icl.it-p (it) T) ; needed in 'icl.arg-col-statistics'

(defun icl.it-not-index-p (it) ; change this if additional var-like
  (icl.it-var-p it))	       ; types are added !

(defun icl.it-index-p (it)
  (not (icl.it-not-index-p it)))


(defun icl.it-element (it)
  (if (null (cddr it))
      (cadr it)   ; element is an atom
      (cdr it)))  ; element is a list


; type transformations

(defun icl.id (it)
  it)

(defun icl.var-anonym (it) ; anonymize variables: (var x) -> (var _)
  (if (icl.it-var-p it)
      `(var _)
      it))



; ---------------------------
; generate rblock (raw block)
; ---------------------------


(defun icl.gen-rblock (it-heads)
  (cons 'rblock (cons (cons 'clauses (icl.numbers 1 (length it-heads)))
		      (icl.gen-arg-col-tags
		       (icl.swap-rows-and-cols it-heads)))))

(defun icl.gen-arg-col-tags (arg-cols &optional (no 1))
  (unless (null arg-cols)
	  (cons (cons 'arg (cons no (car arg-cols)))
		(icl.gen-arg-col-tags (cdr arg-cols) (1+ no)))))



; ----------------------
; generate rblock*rblock
; ----------------------

(defun icl.gen-rblock*rblock (rblock len)
  (let* ((clauses (icl.s-clauses-from-rblock rblock))
	 (clauses*clauses (get-first-n-elements-and-rest len clauses))
	 (arg-cols (icl.s-arg-col-list-from-rblock rblock))
	 (arg-nos (mapcar #'icl.s-arg-no-from-arg-col arg-cols))
	 (splitted-arg-cols (multiple-splitting
			     len
			     (mapcar #'icl.s-it-list-from-arg-col arg-cols)))
	 (arg-cols1 (mapcar #'car splitted-arg-cols))
	 (arg-cols2 (mapcar #'cdr splitted-arg-cols))
	 (rblock1 (cons 'rblock
			(cons (cons 'clauses (car clauses*clauses))
			      (icl.add-arg-tags arg-nos arg-cols1))))
	 (rblock2 (cons 'rblock
			(cons (cons 'clauses (cdr clauses*clauses))
			      (icl.add-arg-tags arg-nos arg-cols2)))))
	(cons rblock1 rblock2)))


(defun icl.add-arg-tags (arg-nos arg-cols)
  (mapcar #'(lambda (arg-no arg-col)
		    (cons 'arg (cons arg-no arg-col)))
	  arg-nos arg-cols))



; ----------------------------------------
; block analysis: icl.analyze-all-arg-cols
; ----------------------------------------


(defun icl.analyze-arg-col (it-list len max-no-of-vars max-portion-of-vars)
  (let ((pos 1)
	(itl it-list)
	(l nil)
	(max-pos 0)
	(max-list nil)
	(no-of-vars 0))
       (loop
	(when (null itl) (return (cons max-pos max-list)))
	(when (icl.it-not-index-p (car itl))
	      (set-inc no-of-vars)
	      (when (or (> no-of-vars max-no-of-vars)
			(> (/ (float no-of-vars) len)
			   max-portion-of-vars))
		    (return (cons max-pos max-list))))
	(let ((var-portion (/ (float no-of-vars) pos)))
	     (set-cons var-portion l)
	     (when (<= var-portion max-portion-of-vars)
		   (setq max-pos pos
			 max-list l)))
	(set-inc pos)
	(set-cdr+ itl))))



(defun icl.analyze-all-arg-cols (arg-col-list
				 no-of-clauses
				 max-no-of-vars
				 max-portion-of-vars
				 min-block-portion)

  ; returns: - (1) for 1blocks
  ;          - (len . nil/t-list) for sblocks
  ;            where a t in the nil/t-list stands for a useful argument

  (let ((analyzed-arg-cols
	 (mapcar #'(lambda (arg-col)
			   (icl.analyze-arg-col (cddr arg-col)
						no-of-clauses
						max-no-of-vars
						max-portion-of-vars))
		 arg-col-list)))
       (let ((max-len (apply #'max (mapcar #'car analyzed-arg-cols))))
	    (cond
	     ((< max-len 2) '(1))
	     (T (let ((min-len (truncate (* max-len min-block-portion))))
		     (icl.find-last-optimum
		      analyzed-arg-cols
		      (length analyzed-arg-cols)
		      (if (< min-len 2) 2 min-len)
		      max-len
		      max-portion-of-vars)))))))


(defun icl.find-last-optimum (analyzed-arg-cols no-of-arg-cols min-len max-len
						max-portion-of-vars)
  (do ((pos max-len (1- pos))
       (arg-cols analyzed-arg-cols)
       (opt-pos max-len)
       (opt-useful-arg-cols nil)
       (optimum 0))

      ((or (< pos min-len)
	   (= optimum no-of-arg-cols))
       (cons opt-pos opt-useful-arg-cols))

      (let* ((cars*cdrs (mapcar #'(lambda (arg-col)
					  (icl.pl-car*cdr arg-col pos 1))
				arg-cols))
	     (useful-arg-cols (mapcar #'(lambda (p)
						(<= p max-portion-of-vars))
				      (mapcar #'car cars*cdrs)))
	     (no-of-useful-arg-cols (count-if #'(lambda (x) x)
					      useful-arg-cols)))
	    (setq arg-cols (mapcar #'cdr cars*cdrs))
	    (when (> no-of-useful-arg-cols optimum)
		  (setq optimum no-of-useful-arg-cols
			opt-useful-arg-cols useful-arg-cols
			opt-pos pos)))))


(defun icl.pl-car*cdr (plist pos &optional default)
  ; car/cdr of partial list (len . list)
  (cond ((> pos (car plist)) (cons default plist))
	((<= pos 0) (cons nil plist))
	(T (cons (cadr plist)
		 (cons (1- (car plist)) (cddr plist))))))



; --------------------------------------
; generate iblock (indexed block) or nil
; --------------------------------------


(defun icl.gen-iblock (rblock)
  (let ((no-of-clauses (length (icl.s-clauses-from-rblock rblock))))
       (when (> no-of-clauses 1)
	     (let ((pblock (icl.gen-pblock rblock no-of-clauses)))
		  (if (null (cdddr pblock))
		      (caddr pblock)   ; simplify pblocks with only 1 partition
		      pblock)))))


; -------------------------------------------
; heuristics for generating pblock partitions
; -------------------------------------------


(defun icl.max-no-of-vars (no-of-clauses)
  (if (<= no-of-clauses idx.*max-no-of-vars*)
      (1- no-of-clauses)
      idx.*max-no-of-vars*))


(defun icl.max-portion-of-vars (no-of-clauses)
  (if (<= no-of-clauses idx.*max-no-of-vars*)
      0.99
      0.75))

(defun icl.min-block-portion (no-of-clauses)
  0.7)



; -----------------------------------
; generate pblock (partitioned block)
; -----------------------------------


(defun icl.gen-pblock (rblock no-of-clauses) ; -> pblock
  (cons 'pblock (cons rblock
		      (icl.gen-pblock-partitions rblock no-of-clauses))))

(defun icl.gen-pblock-partitions (rblock no-of-clauses)
  (when (> no-of-clauses 0)
	(let ((len*nil/t-list (icl.analyze-all-arg-cols
			       (icl.s-arg-col-list-from-rblock rblock)
			       no-of-clauses
			       (icl.max-no-of-vars no-of-clauses)
			       (icl.max-portion-of-vars no-of-clauses)
			       (icl.min-block-portion no-of-clauses))))
	     (let ((rblock*rblock (icl.gen-rblock*rblock
				   rblock (car len*nil/t-list))))
		  (cons (icl.gen-sblock (car rblock*rblock)
					(car len*nil/t-list)
					(cdr len*nil/t-list))
			(icl.gen-pblock-partitions
			 (cdr rblock*rblock)
			 (- no-of-clauses (car len*nil/t-list))))))))

; ---------------
; generate sblock
; ---------------

(defun icl.gen-sblock (rblock len nil/t-list) ; -> sblock

  ; 1a. return 1block

  (cond
   ((= len 1)
    (cons '1block
	  (cdr rblock)))


  ; 1b. create and return normal sblock

   (T (let* ((clauses (icl.s-clauses-from-rblock rblock))
	     (arg-col-list (icl.s-arg-col-list-from-rblock rblock)))

  ; 2. select 'constant'/'variable' argument columns

	    (let ((constant-arg-cols
		   (mapcan #'(lambda (useful arg-col)
				     (when useful (list arg-col)))
			   nil/t-list arg-col-list)))

		 (let ((variable-arg-cols
			(mapcan #'(lambda (useful arg-col)
					  (unless useful (list arg-col)))
				nil/t-list arg-col-list)))

  ; 3. create seqind structure

		      (let ((seqind-structure
			     (icl.gen-seqind constant-arg-cols
					     variable-arg-cols
					     clauses)))

  ; 4. create indexed rest block (from variable-arg-cols)

			   (let ((indexed-rest-block
				  (when (and variable-arg-cols
					     (> (length clauses) 1))
					(cons (icl.gen-iblock
					       (cons 'rblock
						     (cons
						      (cons 'clauses clauses)
						      variable-arg-cols)))
					      nil))))
  ; 5. build sblock

				(cons 'sblock
				      (cons rblock
					    (cons
					     seqind-structure
					     indexed-rest-block)))))))))))



(defun icl.arg-col-statistics (arg-col
			       clauses
			       &optional (predicate #'icl.it-p)
					 (it-transform #'icl.id))

  ; create an assoc list for an argument column of the form
  ; ((<it> . <clauses>) ...) where <it> is of the form
  ; (const <c>) ...
  ; predicate should be #'icl.it-[not-]index-p ...
  ; it-transform should be #'icl.id or #'icl.var-anonym

  (cond ((null arg-col) nil)
        ((not (funcall predicate (car arg-col)))
         (icl.arg-col-statistics (cdr arg-col) (cdr clauses)
				 predicate it-transform))
        (T (let* ((rest-args
                   (icl.arg-col-statistics (cdr arg-col) (cdr clauses)
					   predicate it-transform))
		  (clause (car clauses))
		  (index-arg (funcall it-transform (car arg-col)))
		  (index-arg*clauses (assoc index-arg rest-args
					    :test #'equal)))
		 (acons index-arg (cons clause (cdr index-arg*clauses))
			(delete index-arg*clauses
				rest-args))))))


(defun icl.gen-seqind (tagged-arg-cols additional-arg-cols clauses)

  ; sequential indexing

  (let* ((seqind-args
	  (sort (mapcar #'(lambda (t-a-c)
				  (icl.gen-seqind-arg t-a-c clauses))
			tagged-arg-cols)
		#'(lambda (a b)
			  ; change this for better heuristics!!
			  (> (car (cdaddr a)) (car (cdaddr b))))))
	 (sorted-tagged-arg-cols
	  (icl.sort-tagged-arg-cols
	   tagged-arg-cols
	   (mapcar #'cadr seqind-args))))
	(cons 'seqind
	      (maplist #'(lambda (rest-seqinds rest-t-a-c)
				 (icl.extend-seqind clauses
						    (car rest-seqinds)
						    (append
						     (cdr rest-t-a-c)
						     additional-arg-cols)))
		       seqind-args
		       sorted-tagged-arg-cols))))


(defun icl.sort-tagged-arg-cols (tagged-arg-cols numbers)
  ; sort tagged-arg-cols the same way the numbers are sorted
  (mapcar #'(lambda (n)
		    (find-if #'(lambda (t-a-c)
				       (= (cadr t-a-c) n))
			     tagged-arg-cols))
	  numbers))


(defun icl.gen-seqind-arg (tagged-arg-col clauses)
  (let ((type-table (icl.type-collect
		     (icl.arg-col-statistics
		      (cddr tagged-arg-col)
		      clauses
		      #'icl.it-p
		      #'icl.var-anonym))))
  (cons 'arg
	(cons (cadr tagged-arg-col)
	      (cons (list 'info (icl.compute-weight-of-const-arg-col
				 type-table))
		    type-table)))))


(defun icl.compute-weight-of-const-arg-col (type-table)
  ; simply count number of different constants/structures
  (+ (length (cdar type-table))
     (length (cdadr type-table))))


(defun icl.type-collect (stat-table)
  ; only for constants, structures and vars;
  ; returns const*struct*var
  ; subtypes handled by icl.extend-seqind
  (let ((constants nil)
	(structures nil)
	(vars nil))
       (dolist (it*clauses stat-table)
	       (let* ((it (car it*clauses))
		      (element (icl.it-element it))
		      (clauses (cdr it*clauses))
		      (tagged-clauses (cons 'clauses clauses))
		      (element**tagged-clauses (list element tagged-clauses)))
		     (cond ((icl.it-const-p it)
			    (set-cons element**tagged-clauses constants))
			   ((icl.it-struct-p it)
			    (set-cons element**tagged-clauses structures))
			   ((icl.it-var-p it)
			    (setq vars (cons tagged-clauses nil)))
			   (T (error "icl.type-collect: unknown type: ~A"
				     it)))))
       (list (cons 'const (nreverse constants))
	     (cons 'struct (nreverse structures))
	     (cons 'var vars))))



(defun icl.gen-constants*nil (constants)
  (let ((empty-list
	 (find-if #'(lambda (constant)
			    (null (icl.s-element-name-from-element constant)))
		  constants)))
       (cons (delete empty-list constants :test #'equal)
	     (cdr empty-list))))


(defun icl.gen-structures*list (structures)
  (let ((list
	 (find-if #'(lambda (structure)
			    (equal (icl.s-element-name-from-element structure)
				   '(cns 2)))
		  structures)))
       (cons (delete list structures :test #'equal)
	     (cdr list))))


(defun icl.extend-seqind (org-clauses seqind rest-tagged-arg-cols)
  ; add new iblocks for multiply orruring elements
  ; and split constants and structures for subtypes (nil, list)
  (let* ((arg-no (icl.s-arg-no-from-seqind-arg seqind))
	 (info (icl.s-info-from-seqind-arg seqind))
	 (constants (icl.s-constant-list-from-seqind-arg seqind))
	 (structures (icl.s-structure-list-from-seqind-arg seqind))
	 (vars (icl.s-var-from-raw-seqind-arg seqind))
	 (var-clauses (icl.s-clauses-from-element vars))
	 (ext-constants (icl.extend-seqind-elements
			 constants
			 rest-tagged-arg-cols
			 org-clauses
			 var-clauses))
	 (ext-structures (icl.extend-seqind-elements
			  structures
			  rest-tagged-arg-cols
			  org-clauses
			  var-clauses))
	 (constants*nil (icl.gen-constants*nil ext-constants))
	 (structures*list (icl.gen-structures*list ext-structures)))
	(cons
	 'arg
	 (cons
	  arg-no
	  (cons
	   info
	   (cons
	    (cons 'const (car constants*nil))
	    (cons
	     (cons 'struct (car structures*list))
	     (cons (cons 'list (cdr structures*list))
		   (cons (cons 'nil (cdr constants*nil))
			 (when var-clauses
			       (cons
				(cons 'other 
				      (cdr (icl.extend-seqind-element
					    vars
					    rest-tagged-arg-cols
					    org-clauses
					    nil)))
				nil)))))))))))



(defun icl.extend-seqind-elements (elements rest-t-a-c org-clauses var-clauses)
  (mapcar #'(lambda (element)
		    (icl.extend-seqind-element
		     element rest-t-a-c org-clauses var-clauses))
	  elements))


(defun icl.extend-seqind-element (element rest-t-a-c org-clauses var-clauses)
  (let ((clauses (sort (append (icl.s-clauses-from-element element)
			       (copy-list var-clauses)) ; sort is destructive!
		       #'<)))
       (cons (icl.s-element-name-from-element element)
	     (cons (cons 'clauses clauses)
		   (when rest-t-a-c
			 (icl.nil-or-list
			  (icl.gen-iblock
			   (icl.gen-rblock-for-seqind
			    org-clauses clauses rest-t-a-c))))))))


(defun icl.gen-rblock-for-seqind (org-clauses clauses tagged-arg-cols)
  (cons 'rblock
	(cons
	 (cons 'clauses clauses)
	 (mapcar #'(lambda (tagged-arg-col)
			   (cons
			    'arg
			    (cons
			     (cadr tagged-arg-col)
			     (mapcan #'(lambda (it clause)
					       (when (member clause clauses)
						     (cons it nil)))
				     (cddr tagged-arg-col)
				     org-clauses))))
		 tagged-arg-cols))))





; -------------------
; auxiliary functions
; -------------------


(defun icl.swap-rows-and-cols (lists)
  (apply #'mapcar (cons #'list lists)))



(defun icl.numbers (start end)
  (unless (> start end)
	  (cons start (icl.numbers (1+ start) end))))


(defun icl.nil-or-list (l)
  (when l (cons l nil)))
