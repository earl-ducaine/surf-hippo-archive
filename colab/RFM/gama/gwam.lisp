;;;
;;; GWAM   --   NyWAM for GAMA
;;;
;;; (c) Michael Sintek  1/1991
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; The following is the original copyright notice:
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; A WAM emulator  Common Lisp
;
; Author: Sven-Olof Nystroem
; Summer 1985 and May 1989
;
; Copyright (c) May 1989 by Sven-Olof Nystroem and Uppsala University.
; Permission to copy all or part of this material is granted, provided that
; the copies are not made or redistributed for resale, and that the copyright
; notice and reference to the source file appear.

; Note: There is no prolog compiler; you must supply your own WAM code.

; Some documentation is in Swedish, but I think you'll understand the code
; anyway.

; The emulator only handles the 'pure' prolog instructions given in Warren's
; paper.


(princ "NyWam --- Version 0.23g --- 14 Feb 92")


(defvar *user-variables* nil)
(defvar *registers* nil)
(defvar *read-mode* nil)

(defvar *emu-debug* nil)

;;; Part 1. Primitive operations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmacro word-tag (word)
  `(car ,word))

(defmacro word-value (word)
  `(cadr ,word))


(defmacro definstr (name param-list types &key standard dynamic static)
  ; defines an instruction
  `(progn
     ,(when standard (gwam.defprint (car standard) :standard types))
     ,(when standard (gwam.defun standard param-list))
     ,(when dynamic (gwam.defprint (car dynamic) :dynamic types))
     ,(when dynamic (gwam.defun dynamic param-list))
     ,(when static (gwam.defprint (car static) :static types))
     ,(when static (gwam.defun static param-list))
     (gasm.definstr ',name
       2 #'gasm.std-instr2
       3 #'gasm.std-instr3
       'types ',types
       'emul-fct    ',(car standard)
       'emul-fct/dy ',(car dynamic )
       'emul-fct/st ',(car static  ))))

(defun gwam.defun (name*body para-list)
  (cons 'defun 
	(cons (car name*body) 
	      (cons para-list (cdr name*body)))))

(defvar gwam.*print-info*)
(setq gwam.*print-info* nil)

#+Genera (defun gwam.defprint (&rest name*mode*types)
  ; returns lisp statement that adds print info (used in macro definstr!)
  ; copy-list is needed for Symbolics &rest incompatibility
  (list
   'setq 'gwam.*print-info* 
   (list 'cons (list 'quote (copy-list name*mode*types)) 'gwam.*print-info*)))

#-Genera (defun gwam.defprint (&rest name*mode*types)
  ; returns lisp statement that adds print info (used in macro definstr!)
  (list
   'setq 'gwam.*print-info* 
   (list 'cons (list 'quote name*mode*types) 'gwam.*print-info*)))


(defun gwam.print-mem-cell (mem-cell &optional 
				     (print-fct #'rf-princ-like-lisp))
  ; prints mem-cell if contents known, else returns nil
  ; (used in gmem as a print hook function)
  (when (consp mem-cell)
	(let* ((name (car mem-cell))
	       (print-info (assoc name gwam.*print-info*)))
	      (when print-info ; name known
		(let ((mode (cadr print-info))
		      (types (caddr print-info))
		      (args (cdr mem-cell)))
		     (funcall print-fct
		       (cons 
			 name (mapcar 
			       #'(lambda (x type) 
					 (gwam.get-print-arg mode x type))
			       args types)))
		     T)))))
							   
(defun gwam.get-print-arg (mode x type)
  (let ((real-x (eval x)))
       (cond ((and (eq type 'LABEL)
		   (eq mode :dynamic))
	      (format nil "~a@[~a]" (gmem.get (1- real-x)) real-x))
	     ; additional conversions ...
	     (T real-x))))

(gmem.add-print-fct #'gwam.print-mem-cell)



(defmacro define-offset (name value)
  `(progn
    (if (not (numberp ,value))
        (gerror "gwam" "define-offset" "value of offset not a number"))
    (setf (get ',name 'offset-value) ,value)))

(defmacro offset (name)
  `(get ',name 'offset-value))


(defmacro define-register (name)
  (declare (special *registers*))
  `(progn
    (setq *registers* (delete-duplicates (cons ',name *registers*)))
    (setf (get ',name 'reg-value) nil)))


(defmacro reg (name)
  `(get ',name 'reg-value))

(defmacro set-reg (name value)
  `(setf (reg ,name) ,value))


(defparameter heap-size 20000)

(defparameter start-of-heap (gmem.alloc/clr heap-size :unused-heap-cell))

(defparameter stack-size 20000)

(defparameter start-of-stack (gmem.alloc/clr stack-size :unused-stack-cell))

(defmacro mem (adr)
  `(gmem.get+ (address ,adr)))

(defmacro setmem (adr val)
  `(gmem.put+ (address ,adr) ,val))


(defparameter number-of-argument-regs 1000)

(defvar argument-regs (make-array number-of-argument-regs))

(defmacro argument-reg (n)
  `(aref argument-regs (1- ,n)))

(defmacro set-argument-reg (n value)
  `(setf (aref argument-regs (1- ,n)) ,value))


(defun perm-variable (n)
  (ref-plus (reg E) (offset Y) (1- n)))

(defmacro constant (c)
  `(list :const ,c))

(defmacro constant-nil ()
  `(constant nil))

(defmacro functor (f)
  `(list :fun ,f))

(defmacro noteq (x y)
  `(not (eq ,x ,y)))


#|
(defun address (val)
  (if (member (word-tag val) '(:ref :struct :list))
      (word-value val)
      (gerror "gwam" "address" "word ~a does not contain an address" val)))
|#

(defmacro address (val) ; no error checking needed !!
  `(word-value ,val))

(defun ref-plus (ref &rest numbers)
  (list :ref (apply #'+ (cons (address ref) numbers))))

(defun ref-lessp (ref1 ref2)
  (< (address ref1) (address ref2)))

(defun next-term-S ()
  (mem (set-reg S (ref-plus (reg S) 1))))

(defun new-value (val)
  (set-reg H (ref-plus (reg H) 1))
  (setmem (reg H) val))

(defun new-variable ()
  (set-reg H (ref-plus (reg H) 1))
  (setmem (reg H) (reg H)))

(defun new-struc (S)
  (set-reg H (ref-plus (reg H) 1))
  (setmem (reg H) S)
  (list :struct (word-value (reg H))))

(defun new-list-cell ()
  (list :list (word-value (ref-plus (reg H) 1))))

(defun deref (term)
  (if (and (eq (word-tag term) :ref)
	   (noteq (mem term) term))
      (deref (mem term))
      term))

(defun ny-unify (ref1 ref2)
  (cond  ((equal ref1 ref2))
         ((and (varp ref1)(varp ref2))
	 (if (ref-lessp ref1 ref2)
	     (bind ref2 ref1)
	     (bind ref1 ref2)))
	((varp ref1)
	 (bind ref1 ref2))
	((varp ref2)
	 (bind ref2 ref1))
	((not (eq (word-tag ref1) (word-tag ref2)))
	 (fail))
	(t (case (word-tag ref1)
	     ((:const)
	      (if (equal (word-value ref1)
			 (word-value ref2))
		  nil
		  (fail)))
	     ((:struct)
	      (ny-unify-structures (mem ref1) (mem ref2) ref1 ref2))
	     ((:list)
	      (ny-unify (deref (mem ref1))
		     (deref (mem ref2)))
	      ;(ny-unify (mem (ref-plus ref1 1)) 
	      ;	     (mem (ref-plus ref2 1))))))))
	      (ny-unify (deref (mem (ref-plus ref1 1)))    ; derefence cdr
		     (deref (mem (ref-plus ref2 1))))))))) ; M.S. 6/92 


(defun ny-unify-structures (fun1 fun2 ref1 ref2)
  (if (not (equal (word-tag fun1) (word-tag fun2)))
      (fail))
  (ecase (word-tag fun1)
    ((:fun)
     (if (equal (word-value fun1) (word-value fun2))
	 (let ((arity (second (word-value fun1))))
	   (do ((i arity (1- i)))
	       ((zerop i))
	     (ny-unify (deref (ref-plus ref1 i))
		    (deref (ref-plus ref2 i)))))
         (fail)))))

(defun varp (term)
  (and (eq (word-tag term) :ref)
       ; (equal term (mem term))))
       (eq term (mem term))))

(defun bind (ref val)
  (setmem ref val)
  (if (or (not (ref-lessp (reg HB) ref))
	  (and
	   (< start-of-stack (address ref))
	   (not (ref-lessp (reg B) ref))))
      (trail ref)))

(defun trail (ref)
  (set-reg TR
   (list :trail (cons ref (word-value (reg TR))))))


(defun fail ()
  (let ((temp (reg TR)))
    (set-reg TR (mem (ref-plus (reg B) (offset TR1))))
    (set-reg H  (mem (ref-plus (reg B) (offset H1))))
    (set-reg E  (mem (ref-plus (reg B) (offset BCE))))
    (set-reg CP (mem (ref-plus (reg B) (offset BCP))))
    (set-reg P  (mem (ref-plus (reg B) (offset BP))))
    (set-reg A  (reg B))
    (unwind-trail temp (reg TR)))
  (throw 'fail nil))

(defun unwind-trail (r1 r2)
  (uwtr (word-value r1) (word-value r2)))

(defun uwtr (r1 r2)
  (cond
   ((noteq r1 r2)
    (setmem (first r1) (first r1))
    (uwtr (rest r1) r2))))

(defun save-argument-regs (n)
  (dotimes (i n)
    (setmem (ref-plus (reg A) (offset A) (- i))
	    (argument-reg (1+ i)))))


(defun restore-argument-regs (n)
  (dotimes (i n)
    (set-argument-reg (1+ i) (mem (ref-plus (reg A) (offset A) (- i))))))

(defmacro set-read-mode ()
  `(setq *read-mode* t))

(defmacro set-write-mode ()
  `(setq *read-mode* nil))

(defmacro read-mode ()
  `*read-mode*)


;;; Part 2: Some definitions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;def av register

(define-register P  )
(define-register CP )
(define-register E  )
(define-register B  )
(define-register A  )
(define-register TR )
(define-register H  )
(define-register HB )
(define-register S  )
(define-register IX ) ; index register
(define-register CUTP )

;def av offset i enviroment

(define-offset CE +1)
(define-offset CP +2)
(define-offset OCUTP +3)
(define-offset Y +4)

;def av offset i choice point

(define-offset BCE -5)
(define-offset BCP -4)
(define-offset B1  -3)
(define-offset BP  -2)
(define-offset TR1 -1)
(define-offset H1   0)
(define-offset A   -6)

(defconstant regs-in-choicepoint 6)

(defconstant regs-in-environment 3)


;;; Part 3. The instructions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(definstr fail () () :standard (gwam.fail
 (fail)))


; *** put instructions ***
; ************************


(definstr put_variable_perm (Yn Ai) (NAT NAT) :standard
  (gwam.put_variable_perm
    (setmem (perm-variable Yn) (perm-variable Yn))
    (set-argument-reg Ai (perm-variable Yn))))

(definstr put_variable_temp (Xn Ai) (NAT NAT) :standard
  (gwam.put_variable_temp
    (set-argument-reg Ai (set-argument-reg Xn (new-variable)))))

(definstr put_value_perm (Yn Ai) (NAT NAT) :standard
  (gwam.put_value_perm
    (set-argument-reg Ai (mem (perm-variable Yn)))))

(definstr put_value_temp (Xn Ai) (NAT NAT) :standard
  (gwam.put_value_temp
    (set-argument-reg Ai (argument-reg Xn))))

(definstr put_unsafe_value_perm (Yn Ai) (NAT NAT) :standard
  (gwam.put_unsafe_value_perm
    (let ((temp (deref (perm-variable Yn))))
      (if (and (varp temp)
	       (ref-lessp (reg E) temp))
	  (bind temp (set-argument-reg Ai (new-variable)))
	  (set-argument-reg Ai temp)))))

(definstr put_constant (C Ai) (CONST NAT) :standard
  (gwam.put_constant
    (set-argument-reg Ai (constant C))))

(definstr put_nil (Ai) (NAT) :standard
  (gwam.put_nil
    (set-argument-reg Ai (constant-nil))))

(definstr put_structure (F Ai) (FUNCTOR NAT) :standard
  (gwam.put_structure
    (set-argument-reg Ai (new-struc (functor F)))
    (set-write-mode)))

(definstr put_list (Ai) (NAT) :standard
  (gwam.put_list
    (set-argument-reg Ai (new-list-cell))
    (set-write-mode)))


; ***  get instructions ***
; *************************


(definstr get_variable_temp (Xn Ai) (NAT NAT) :standard
  (gwam.get_variable_temp
    (set-argument-reg Xn (argument-reg Ai))))

(definstr get_variable_perm (Yn Ai) (NAT NAT) :standard
  (gwam.get_variable_perm
    (setmem (perm-variable Yn) (argument-reg Ai))))

(definstr get_value_temp (Xn Ai) (NAT NAT) :standard
  (gwam.get_value_temp
    (let ((temp1 (deref (argument-reg Xn)))
	  (temp2 (deref (argument-reg Ai))))
         (ny-unify temp1 temp2)
         (set-argument-reg Xn temp1))))

(definstr get_value_perm (Yn Ai) (NAT NAT) :standard
  (gwam.get_value_perm
    (let ((temp1 (deref (perm-variable Yn)))
	  (temp2 (deref (argument-reg Ai))))
         (ny-unify temp1 temp2))))

(definstr get_nil (Ai) (NAT) :standard
  (gwam.get_nil
    (let ((temp (deref (argument-reg Ai))))
         (cond
           ((varp temp) (bind temp (constant-nil)))
           ((equal temp (constant-nil)))
           (t (fail))))))

(definstr get_constant (C Ai) (CONST NAT) :standard
  (gwam.get_constant
    (let ((temp (deref (argument-reg Ai))))
      (cond
       ((varp temp) (bind temp (constant C)))
       ((equal temp (constant C)))
       (t (fail))))))

(definstr get_structure (F Ai) (FUNCTOR NAT) :standard
  (gwam.get_structure
    (let ((temp (deref (argument-reg Ai))))
      (cond
       ((varp temp)
        (bind temp (new-struc (functor F)))
        (set-write-mode))
       ((and (eq (word-tag temp) :struct)
	     (equal (mem temp) (functor F)))
        (set-reg S temp)
        (set-read-mode))
       (t (fail))))))

(definstr get_list (Ai) (NAT) :standard
  (gwam.get_list
    (let ((temp (deref (argument-reg Ai))))
      (cond
       ((varp temp)
        (bind temp (new-list-cell))
        (set-write-mode))
       ((eq (word-tag temp) :list)
        (set-reg S (ref-plus temp -1))
        (set-read-mode))
       (t (fail))))))


; *** unify instructions ***
; **************************


(definstr unify_variable_temp (Xn) (NAT) :standard
  (gwam.unify_variable_temp
    (if (read-mode)
        (set-argument-reg Xn (next-term-S))
        (set-argument-reg Xn (new-variable)))))

(definstr unify_variable_perm (Yn) (NAT) :standard
  (gwam.unify_variable_perm
    (if (read-mode)
        (setmem (perm-variable Yn) (next-term-S))
        (setmem (perm-variable Yn) (new-variable)))))

(definstr unify_void (n) (NAT) :standard
  (gwam.unify_void
    (if (read-mode)
        (dotimes (? n) (next-term-S))
        (dotimes (? n) (new-variable)))))

(definstr unify_value_temp (Xn) (NAT) :standard
  (gwam.unify_value_temp
    (if (read-mode)
        (let ((temp1 (deref (argument-reg Xn)))
	      (temp2 (deref (next-term-S))))
	  (ny-unify temp1 temp2)
	  (set-argument-reg Xn temp1))
        (new-value (argument-reg Xn)))))

(definstr unify_value_perm (Yn) (NAT) :standard
  (gwam.unify_value_perm
    (if (read-mode)
        (let ((temp1 (deref (perm-variable Yn)))
	      (temp2 (deref (next-term-S))))
	  (ny-unify temp1 temp2))
        (new-value (mem (perm-variable Yn))))))

(definstr unify_local_value_temp (Xn) (NAT) :standard
  (gwam.unify_local_value_temp
    (if (read-mode)
        (let ((temp1 (deref (argument-reg Xn)))
	      (temp2 (deref (next-term-S))))
	  (ny-unify temp1 temp2)
	  (set-argument-reg Xn temp1))
        (let ((temp (deref (argument-reg Xn))))
	  (if (and (varp temp)
		   (< start-of-stack (address temp)))
	      (set-argument-reg Xn
			        (bind temp (new-variable)))
	      (new-value temp))))))

(definstr unify_local_value_perm (Yn) (NAT) :standard
  (gwam.unify_local_value_perm
    (if (read-mode)
        (let ((temp1 (deref (perm-variable Yn)))
	      (temp2 (deref (next-term-S))))
	  (ny-unify temp1 temp2))
        (let ((temp (deref (perm-variable Yn))))
	  (if (and (varp temp)
		   (< start-of-stack (address temp)))
	      (bind temp (new-variable))
	      (new-value temp))))))

(definstr unify_nil () () :standard
  (gwam.unify_nil
    (if (read-mode)
        (let ((temp (deref (next-term-S))))
	  (cond
	   ((varp temp) (bind temp (constant-nil)))
	   ((equal temp (constant-nil)))
	   (t (fail))))
        (new-value (constant-nil)))))

(definstr unify_constant (C) (CONST) :standard
  (gwam.unify_constant
    (if (read-mode)
        (let ((temp (deref (next-term-S))))
	  (cond
	   ((varp temp) (bind temp (constant C)))
	   ((equal temp (constant C)))
	   (t (fail))))
        (new-value (constant C)))))



; ***  indexing instructions ****
; *******************************


(definstr try (L n) (LABEL NAT) :static (gwam.try
  (set-reg A
	   (ref-plus
	    (reg A) n regs-in-choicepoint))
  (save-argument-regs n)
  (setmem (ref-plus (reg A) (offset BCE)) (reg E))
  (setmem (ref-plus (reg A) (offset BCP)) (reg CP))
  (setmem (ref-plus (reg A) (offset B1)) (reg B))
  (setmem (ref-plus (reg A) (offset BP)) (reg P))
  (setmem (ref-plus (reg A) (offset TR1)) (reg TR))
  (setmem (ref-plus (reg A) (offset H1)) (reg H))

  (set-reg HB (reg H))
  (set-reg B  (reg A))
  (set-reg P L)))


(definstr retry (L n) (LABEL NAT) :static (gwam.retry
  (restore-argument-regs n)
  (setmem (ref-plus (reg B) (offset BP)) (reg P))
  (set-reg P L)))


(definstr trust (L n) (LABEL NAT) :static (gwam.trust
  (restore-argument-regs n)
  (set-reg P L)
  (set-reg A
	   (ref-plus
	    (reg A)
	    (- n)
	    (- regs-in-choicepoint)))
  (set-reg B  (mem (ref-plus (reg B) (offset B1))))))


; *** added functions ***  --  (c) HGH & real-fun

(definstr try_me_else (L n) (LABEL NAT) :static (gwam.try_me_else
  (set-reg A
	   (ref-plus
             (reg A) n  regs-in-choicepoint))
  (save-argument-regs n)
  (setmem (ref-plus (reg A) (offset BCE)) (reg E))
  (setmem (ref-plus (reg A) (offset BCP)) (reg CP))
  (setmem (ref-plus (reg A) (offset B1)) (reg B))
  (setmem (ref-plus (reg A) (offset BP)) L)
  (setmem (ref-plus (reg A) (offset TR1)) (reg TR))
  (setmem (ref-plus (reg A) (offset H1)) (reg H))
  (set-reg HB (reg H))
  (set-reg B  (reg A))))


(definstr retry_me_else (L n) (LABEL NAT) :static (gwam.retry_me_else
  (restore-argument-regs n)
  (setmem (ref-plus (reg B) (offset BP)) L)))


(definstr trust_me_else_fail (n) (NAT) :standard (gwam.trust_me_else_fail
  (restore-argument-regs n)
  (set-reg A
           (ref-plus
             (reg A)
             (- n)
             (- regs-in-choicepoint)))
   (set-reg B  (mem (ref-plus (reg B) (offset B1))))))


(definstr switch_on_type 
  (Va In Sy Ls St Ni Ot) (LABEL LABEL LABEL LABEL LABEL LABEL LABEL)
  :static (gwam.switch_on_type
  (let ((temp (deref (argument-reg 1))))
    (case (word-tag temp)
      ((:ref) 	(set-reg P Va))
      ((:const)	(cond
                 ((null (word-value temp)) (set-reg P Ni))
                 ((integerp (word-value temp)) (set-reg P In))
                 (t (set-reg P Sy))))
      ((:list) 	(set-reg P Ls))
      ((:struct) (set-reg P St))
      (T 	(set-reg P Ot))))))


(definstr set_index_number (No) (NAT) ; real-fun
  :standard (gwam.set_index_number
  (set-reg IX No)))


(definstr switch_on_term  ; real-fun
  (Lc Ls Ll Ln Lv) (LABEL LABEL LABEL LABEL LABEL)
  :static (gwam.switch_on_term
  (let ((temp (deref (argument-reg (reg IX)))))
    (case (word-tag temp)
      ((:ref)    (set-reg P Lv))
      ((:const)  (if (null (word-value temp))
                     (set-reg P Ln)   ; nil
                     (set-reg P Lc))) ; ordinary constant
      ((:list)   (set-reg P Ll))
      ((:struct) (set-reg P Ls))
      (T         (set-reg P Lv))))))  ; ??? should not happen! ???


(definstr switch_on_constant 
  (Len Table Ot) (NAT HASHTABLE LABEL)
  :static (gwam.switch_on_constant
  (let* ((temp (deref (argument-reg (reg IX))))
	 (dest (second (assoc (word-value temp) Table :test #'equal))))
    (if dest (set-reg P dest)
        (set-reg P Ot)))))


(definstr switch_on_structure 
  (Len Table Ot) (NAT HASHTABLE LABEL)
  :static (gwam.switch_on_structure
  (let* ((temp (deref (argument-reg (reg IX))))
	 (dest (second (assoc (word-value (mem temp)) Table :test #'equal))))
    (if dest (set-reg P dest)
        (set-reg P Ot)))))


; *** control instructions ***
; ****************************


(definstr allocate (enviroment-size) (NAT) :standard
  (gwam.allocate
    (let ((temp (reg E)))
      (set-reg E (reg A))
      (setmem (ref-plus (reg E) (offset CP)) (reg CP))
      (setmem (ref-plus (reg E) (offset CE)) temp)
      (set-reg A (ref-plus (reg A) enviroment-size regs-in-environment)))))

(definstr deallocate () () :standard
  (gwam.deallocate
    (if (<= (address (reg B)) (address (reg E)))
        (set-reg A (reg E)))
    (set-reg CP (mem (ref-plus (reg E) (offset CP))))
    (set-reg E  (mem (ref-plus (reg E) (offset CE))))))

(definstr proceed () () :standard
  (gwam.proceed
    (set-reg P (reg CP))))

#| set CUTP is missing ???  M.S. 7/92
(definstr execute (proc) (LABEL)
  :static (gwam.execute/st (set-reg P proc))
  :dynamic (gwam.execute/dy (set-reg P (gmem.get proc))))
|#

(definstr execute (proc) (LABEL)
  :static (gwam.execute/st (set-reg CUTP (reg B)) (set-reg P proc))
  :dynamic (gwam.execute/dy (set-reg CUTP (reg B)) (set-reg P (gmem.get proc))))

(definstr call (proc k) (LABEL NAT)
  :static (gwam.call/st
    (set-reg CP (reg P))
    (set-reg CUTP (reg B))
    (if (ref-lessp (reg B) (reg E)) (set-reg A (ref-plus (reg E) (offset Y) k)))
    (set-reg P proc))
  :dynamic (gwam.call/dy
    (set-reg CP (reg P))
    (set-reg CUTP (reg B))
    (if (ref-lessp (reg B) (reg E)) (set-reg A (ref-plus (reg E) (offset Y) k)))
    (set-reg P (gmem.get proc))))



; *** utility instructions ***
; ****************************


(definstr choice_temp (Xn) (NAT) :standard
  (gwam.choice_temp
    (set-argument-reg Xn (reg B))))

(definstr commit_temp (i Xn) (NAT NAT) :standard
  (gwam.commit_temp
    (set-reg B (argument-reg Xn))
    (set-reg TR
	     (list
	       :reference
	       (compact-trail
		(word-value (reg TR))
		(word-value (mem (ref-plus (reg B) (offset TR1))))
		(word-value (reg B)))))))

(definstr commit-perm (i Yn) (NAT NAT) :standard
  (gwam.commit-perm
    (set-reg B (permanent-variable Yn))
    (set-reg TR
	      (list
	        :reference
	        (compact-trail
		 (word-value (reg TR))
		 (word-value (mem (ref-plus (reg B) (offset TR1))))
		 (word-value (reg B)))))))


(defun compact-trail (TR TR1 B)
  (cond
   ((eq TR TR1) TR)
   ((< B (word-value (car TR)))
    (compact-trail (cdr TR) TR1 B))
   (t (cons (car TR) (compact-trail (cdr TR) TR1 B)))))



; *** special instructions ***
; ****************************


(definstr has-succeeded () () :standard (gwam.has-succeeded
  (declare (special *lureg*))
  ; (format *standard-output* "~% VALUE = ~a" (show-term (argument-reg 1) t) )
  ; (rf-pprint (show-term (argument-reg 1) t))  (rf-terpri)
  (gpprint (show-term (argument-reg 1) t))
  (do ((var *user-variables* (rest var)))
      ((null var))
      (gpprint (list (caar var) '=
                     (show-term (cdr (first var)) t))))
  (if (g-y-or-n-p "~%More solutions? ")
      (progn (gterpri) (fail))
      (throw 'halt nil))))

(definstr has-failed () () :standard (gwam.has-failed
  (gformat "unknown~%") ; "No (more) solutions"
  (throw 'halt nil)))

(definstr escape-to-lisp (fun) (X) :standard  ; !!!!!!
  (gwam.escape-to-lisp
    (funcall  ; one eval too much in GAMA !!!!!
     (eval fun))))       ;ser konstigt ut men det ska visst va s}...


; lisp --> emu

(defun lisp-emu-wandel (x)
 (cond ((atom x)  (list :const x))
       ((consp x)
        (if (and (eq (car x) 'vari) (atom (cadr x)) (null (caddr x)))
	 (gerror "gwam" "lisp-emu-wandel"
		 "unclear semantics: lisp is giving back a variable")
 	 (lisp-emu-wandel-list x)
	)
       )
       (t (gerror "gwam" "lisp-emu-wandel"
		  "lisp-emu-wandel error"))))

(defun lisp-emu-wandel-list (x)
  (let ((head (lisp-emu-wandel (car x)))
        (tail (lisp-emu-wandel (cdr x)))
        (ref (new-list-cell))
       )
    (new-value head)
    (new-value tail)
    ref))


; emu --> lisp

(defun emu-lisp-wandel (x)
 (cond ((eq (word-tag x) :const) (word-value x))
       ((varp x) (gerror "gwam" "emu-lisp-wandel"
			 "lisp called with variable not instantiated"))
       ((eq (word-tag x) :ref) (emu-lisp-wandel (mem x)) )
       ((eq (word-tag x) :list) (cons (emu-lisp-wandel (mem x))
                                      (emu-lisp-wandel (mem (ref-plus x 1)))))
       ((eq (word-tag x) :struct)
;;;        (gerror "gwam" "emu-lisp-wandel" "Cannot handle structs in LISP")
          (let ((name (first (word-value (mem x))))
	        (arity (second (word-value (mem x))))
		(ref   (ref-plus x 1)))
	       (if (tupstruct-p name) ; added for tupified structs  M.S. 6/92
		   (cons name (car (emu-lisp-wandel-terms ref arity)))
		   (cons name (emu-lisp-wandel-terms ref arity)))))
       ( t (gerror "gwam" "emu-lisp-wandel" "unknown tag"))))

(defun emu-lisp-wandel-terms (ref arity)
  (if (zerop arity) nil
      (cons (emu-lisp-wandel (mem ref))
	      (emu-lisp-wandel-terms (ref-plus ref 1) (1- arity)))))


; builtins

(definstr cl-func (fun arity) (FUNCTION NAT) :standard
  (gwam.cl-func
            (do ( (i arity (1- i))
                  argliste
                  temp)
                ( (= i 0) (set-argument-reg 1 (lisp-emu-wandel (apply fun argliste))))
                (if (eq (word-tag (setq temp (deref (argument-reg i)))) :ref)
                 (gerror "gwam" "cl-func" "lispcall with variable IMPOSSIBLE")
                 (setq argliste (cons (emu-lisp-wandel temp) argliste))
                ) ; if
            ) ; do
))

(definstr cl-pred (fun arity) (FUNCTION NAT) :standard
  (gwam.cl-pred
            (do ( (i arity (1- i))
                  argliste
                  temp
                )
                ( (= i 0)
		 (let ( (tmp (apply fun argliste)) )
		    (if tmp
			; (set-argument-reg 1 (lisp-emu-wandel (apply fun argliste)))
			(set-argument-reg 1 (lisp-emu-wandel tmp))
			(fail)
		    ) ; if
		 ) ; let
               ) ; (term result)-do
                (if (eq (word-tag (setq temp (deref (argument-reg i)))) :ref)
                 (gerror "gwam" "cl-pred" "lispcall with variable IMPOSSIBLE")
                 (setq argliste (cons (emu-lisp-wandel temp) argliste))
                ) ; if
            ) ; do
))

(definstr cl-relf (fun arity) (FUNCTION NAT) :standard
  (gwam.cl-relf
            (do ( (i arity (1- i))
                  argliste
                  temp
                )
                ( (= i 0)
		 (let ( (tmp (apply fun argliste)) )
		    (if tmp
		     ; (set-argument-reg 1 (lisp-emu-wandel (apply fun argliste)))
		     (set-argument-reg 1 (lisp-emu-wandel tmp))
		     (fail)
		    ) ; if
		 ) ; let
               ) ; (term result)-do
              (setq temp (deref (argument-reg i)))
              (setq argliste (cons (emu-lisp-wandel temp) argliste))
            ) ; do
))

#|

(definstr cl-extra (fun arity) (FUNCTION NAT) :standard
  (gwam.cl-extra
            (do ( (i arity (1- i))
                  argliste
                  temp
                )
                ( (= i 0)
		  (apply fun argliste))
                (if (eq (word-tag (setq temp (deref (argument-reg i)))) :ref)
                 (gerror "gwam" "cl-extra" "lispcall with variable IMPOSSIBLE")
                 (setq argliste (cons (emu-lisp-wandel temp) argliste))
                ) ; if
            ) ; do
   (when (zerop arity) ; if arity is 0 return true
	 (set-argument-reg 1 '(:const true)))
))

|#


; new cl-extra + emu-lisp-wandel-extra for correct handling of lists
; and free variables                        Michael Sintek  11.02.92


(definstr cl-extra (fun arity) (FUNCTION NAT) :standard
  (gwam.cl-extra
            (do ( (i arity (1- i))
                  argliste
                  temp
                )
                ( (= i 0) 
		  (let ((*emulator-init-allowed* nil))
		       (apply fun argliste)))
		(setq temp (deref (argument-reg i)))
		(setq argliste (cons (emu-lisp-wandel-extra temp) argliste))
            ) ; do
   (when (zerop arity) ; if arity is 0 return true
	 (set-argument-reg 1 '(:const true)))))


(defun emu-lisp-wandel-extra (x)
 (cond ((eq (word-tag x) :const) (const-or-empty-list (word-value x)))
       ((varp x) (list 'vari '|Ny| (word-value x)))
       ((eq (word-tag x) :ref) (emu-lisp-wandel-extra (mem x)) )
       ((eq (word-tag x) :list)
	(cons 'tup (cons (emu-lisp-wandel-extra (mem x))
			 (emu-lisp-wandel-extra2 (mem (ref-plus x 1))))))
       ((eq (word-tag x) :struct)
          (let ((name (first (word-value (mem x))))
                (arity (second (word-value (mem x))))
                (ref   (ref-plus x 1)))
	       (if (tupstruct-p name) ; added for tupified structs  M.S. 6/92
		   (cons name 
			 (let ((term 
				(car (emu-lisp-wandel-extra-terms ref arity))))
			      (if (fun-eq term 'tup)
				  (cdr term)
				  (dotted-pair term))))
		   (cons name (emu-lisp-wandel-extra-terms ref arity)))))
       ( t (gerror "gwam" "emul-lisp-wandel-extra" "unknown tag"))))


(defun emu-lisp-wandel-extra2 (x) ; handling lists (dotted pairs allowed!)
  (cond 
   ((eq (word-tag x) :list)
    (cons (emu-lisp-wandel-extra (mem x))
	  (emu-lisp-wandel-extra2 (mem (ref-plus x 1)))))
   ((and (eq (word-tag x) :const) ; nil
	 (null (word-value x)))
    nil)
   ((varp x) (dotted-pair (list 'vari '|Ny| (word-value x))))
   ((eq (word-tag x) :ref) (emu-lisp-wandel-extra2 (mem x)))
   (T (dotted-pair (emu-lisp-wandel-extra x)))))

(defun dotted-pair (x)
  (list '\| x))


(defun const-or-empty-list (x)
  (if x x '(tup)))


(defun emu-lisp-wandel-extra-terms (ref arity)
  (if (zerop arity) nil
      (cons (emu-lisp-wandel-extra (mem ref))
              (emu-lisp-wandel-extra-terms (ref-plus ref 1) (1- arity)))))


; replace show-term and show-terms by emu-lisp-wandel-extra
; (the only difference is the correct handling of
;  empty lists)

(defun show-term (x flag) ; flag ignored
  (mk-inst (emu-lisp-wandel-extra x))) ; mk-inst needed for ProSyn !!

(defun show-terms (x arity)
  (emu-lisp-wandel-extra-terms x arity))




;(has-failed)
;
;
;(escape-to-lisp #`(lambda ()(.. )))
;


;;; Part 3a. CUT Extensions. (HGH, Jan91)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(definstr save_cut_pointer () () :standard (gwam.save_cut_pointer
 (setmem (ref-plus (reg E) (offset OCUTP)) (reg CUTP))))

#|
(definstr cut (n) (NAT) :standard (gwam.cut
 (when (ref-lessp (reg E) (reg B)) 
       (set-reg A (ref-plus regs-in-environment (reg E) n))) ; <<< !!!
 (set-reg B (mem (ref-plus (reg E) (offset OCUTP))))
 (set-reg CP (mem (ref-plus (reg B) (offset BCP))))))
|#

(definstr cut (n) (NAT) :standard (gwam.cut  ; M.S. 7/92
 (when (ref-lessp (reg E) (reg B)) 
       (set-reg A (ref-plus (reg E) regs-in-environment n)))
 (set-reg CP (mem (ref-plus (reg B) (offset BCP))))
 (set-reg B (mem (ref-plus (reg E) (offset OCUTP))))))

#|
(definstr last_cut()
 (set-reg B (mem (ref-plus (reg E) (offset OCUTP))))
 (set-reg CP (mem (ref-plus (reg B) (offset BCP))))
 (set-reg A (reg E))
 (set-reg P (mem (ref-plus (reg E) (offset CP)))))
|#

;;; changed on 18 Sept. 1991:
(definstr last_cut () () :standard (gwam.last_cut
  (set-reg CP (mem (ref-plus (reg B) (offset BCP))))
  (set-reg B (mem (ref-plus (reg E) (offset OCUTP))))
  (set-reg A (reg E))
  (set-reg P (mem (ref-plus (reg E) (offset CP))))
  (set-reg E (mem (ref-plus (reg E) (offset CE))))))

#|
(definstr lonely_cut () () :standard (gwam.lonely_cut
 (set-reg B (reg CUTP))
 (set-reg CP (mem (ref-plus (reg B) (offset BCP))))
 (set-reg P (reg CP))))
|#

(definstr lonely_cut () () :standard (gwam.lonely_cut ; M.S. 7/92
 (set-reg CP (mem (ref-plus (reg B) (offset BCP))))
 (set-reg B (reg CUTP))
 (set-reg P (reg CP))))

#|
(definstr first_cut () () :standard (gwam.first_cut
 (set-reg B (reg CUTP))
 (set-reg CP (mem (ref-plus (reg B) (offset BCP))))))
|#

(definstr first_cut () () :standard (gwam.first_cut ; M.S. 7/92
 (set-reg CP (mem (ref-plus (reg B) (offset BCP))))
 (set-reg B (reg CUTP))))


;;; Part 3b. META-CALL, APPLY ... Extensions.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; auxiliary functions
; -------------------

(defun gwam.set-arg-regs-from-structure (struc lureg) ; -> (name . lureg)
  (let* ((func (mem struc))
	 (val (word-value func))
	 (n (cadr val))
	 (proc (car val)))
	(dotimes (j  n) (set-argument-reg (+ j 1 lureg) 
					  (mem (ref-plus struc j 1))))
	(cons proc (+ n lureg))))


(defun gwam.set-arg-regs-from-list (term lureg) ; -> lureg
  (let ((tag (word-tag term)))
       (cond ((and (eq tag :const) (null (word-value term))) ; end of list
	      lureg)
	     ((eq tag :list)
	      (set-argument-reg (1+ lureg) (mem term)) ; "car"
	      (gwam.set-arg-regs-from-list
	       (deref (mem (ref-plus term 1))) (1+ lureg)))
	     (T (gerror "gwam" "gwam.set-arg-regs-from-list"
			"complete list expected:~%~a" term)))))


(defun gwam.create-struct-from-arg-regs (arity+1) ; in X1
  (let ((name (deref (argument-reg 1))))
       (if (eq (word-tag name) :const)
	   (let ((adr (ref-plus (reg H) 1)))
		(setmem adr (functor (list (word-value name) (1- arity+1))))
		(dotimes (j (1- arity+1))
			 (setmem (ref-plus adr j 1) (argument-reg (+ j 2))))
		(set-reg H (ref-plus (reg H) arity+1))
		(set-argument-reg 1 (list :struct (address adr))))
	   (gerror "gwam" "gwam.create-struct-from-arg-regs"
		   "functor must be a constant."))))


(defun gwam.create-list-from-arg-regs (arity &optional head) ; in X1
  (let ((previous-list (constant-nil)))
       (do ((i arity (1- i)))
	   ((zerop i) 
	    (if head ; add head as "car"
		(progn (set-argument-reg 1 (new-list-cell))
		       (new-value head)
		       (new-value previous-list))
		(set-argument-reg 1 previous-list)))
	   (let ((new-list-cell (new-list-cell)))
		(new-value (argument-reg i))
		(new-value previous-list)
		(setq previous-list new-list-cell)))))





; 1. meta-call
; ------------

(defun gwam.meta-call (xn k imports)
  (set-reg CP (reg P))
  (set-reg CUTP (reg B))
  (if (ref-lessp (reg B) (reg E)) (set-reg A (ref-plus (reg E) (offset Y) k)))
  (let ((temp (deref (argument-reg xn))))
    (if (eq (word-tag temp) :struct)
	(let ((name*arity (gwam.set-arg-regs-from-structure temp 0)))
	     (set-reg P (gasm.get-global-adr* 
			 (intern (format nil "~a/~d" 
					 (car name*arity)
					 (cdr name*arity)))
			 imports)))
	(gerror "gwam" "gwam.meta-call"
		"illegal meta-call: ~a" temp))))

(defun gwam.meta-call/mod (xn k module)
  (let ((mod (gcla.get gasm.*modules* module)))
       (if mod
	   (gwam.meta-call xn k (cons module (gcla.get mod 'imports)))
	   (gerror "gwam" "gwam.call/mod"
		   "module '~a' does not exist." module))))

(definstr mcall/dy (xn k) (NAT NAT) :standard (gwam.mcall/dy
  ; dynamic meta-call
  (gwam.meta-call/mod xn k gasm.*default-module*)))

(definstr mcall/mod (xn k xm) (NAT NAT NAT) :standard (gwam.mcall/mod
  ; meta-call in a given module
  (let ((module (deref (argument-reg xm))))
       (if (eq (word-tag module) :const)
	   (gwam.meta-call/mod xn k (word-value module))
	   (gerror "gwam" "gwam.mcall/mod"
		   "module name expected, not ~a" module)))))


; 2. apply
; --------


(defun gwam.iapply (Xn Xm k imports) ; Xn = operator, Xm = arglist
  (set-reg CP (reg P))
  (set-reg CUTP (reg B))
  (if (ref-lessp (reg B) (reg E)) (set-reg A (ref-plus (reg E) (offset Y) k)))
  (let* ((operator (deref (argument-reg Xn)))
	 (tag (word-tag operator)))
	(cond ((eq tag :const)
	       (let ((arity (gwam.set-arg-regs-from-list 
			     (deref (argument-reg Xm)) 0)))
		    (set-reg P (gasm.get-global-adr*
				(intern (format nil "~a/~d" 
						(word-value operator) arity))
				imports))))
	      ((eq tag :struct)
	       (let* ((li (deref (argument-reg Xm))) ; save Xm!
		      (name*lureg (gwam.set-arg-regs-from-structure
				   operator 0))
		      (arity (gwam.set-arg-regs-from-list
			      li (cdr name*lureg))))
		     (set-reg P (gasm.get-global-adr*
				 (intern (format 
					  nil "~a/~d" 
					  (car name*lureg) arity))
				 imports))))
	      (T (gerror "gwam" "gwam.iapply"
		   "operator expected, not ~a" operator)))))

(defun gwam.iapply/mod (Xn Xm k module)
  (let ((mod (gcla.get gasm.*modules* module)))
       (if mod
	   (gwam.iapply Xn Xm k (cons module (gcla.get mod 'imports)))
	   (gerror "gwam" "gwam.iapply/mod"
		   "module '~a' does not exist." module))))

(definstr apply/mod (Xn Xm k Xo) (NAT NAT NAT NAT) :standard (gwam.apply/mod
  ; apply in a given module
  (let ((module (deref (argument-reg Xo))))
       (if (eq (word-tag module) :const)
	   (gwam.iapply/mod Xn Xm k (word-value module))
	   (gerror "gwam" "gwam.apply/mod"
		   "module name expected, not ~a" module)))))


; 3. tup2struct & struct2tup
; --------------------------

(definstr tup2struct (Xn) (NAT) :standard (gwam.tup2struct
  (gwam.create-struct-from-arg-regs (gwam.set-arg-regs-from-list 
				     (deref (argument-reg Xn)) 0))))

(definstr struct2tup (Xn) (NAT) :standard (gwam.struct2tup
  (let ((struct*lureg (gwam.set-arg-regs-from-structure
		       (deref (argument-reg Xn)) 0)))
       (gwam.create-list-from-arg-regs 
	(cdr struct*lureg)
	(constant (car struct*lureg))))))




; transformations:
; ----------------

; auxiliaries:

(defun fun-eq (term x) ;  replace by functions in rfi !!!
  (and (consp term)
       (eq (car term) x)))

(defun funfun-eq (term x)
  (and (consp term)
       (consp (car term))
       (eq (caar term) x)))

(defun second-eq (term x)
  (and (consp term)
       (consp (cdr term))
       (eq (cadr term) x)))


(defun subst-expr-p (expr) ; expressions for substitutions in lambda's etc.
  (or (fun-eq expr 'vari) 
      (atom expr)
      (and (fun-eq expr 'uninst) (subst-expr-p (cadr expr)))
      (and (fun-eq expr 'inst) (subst-expr-p (cadr expr)))))


; untype (type -> lambda)
; -----------------------


(defun untype/active-type (type) ; type = (expr : type1 ...)
  (type2lambda (untype/literal (car type)) (cddr type)))
  
(defun untype/passive-type (type) ; type = (expr : type1 ...)
  (list 'uninst (type2lambda (mk-inst (untype/passive (car type)))
                             (cddr type))))

(defun type2lambda (expr types)
  (let ((x '(vari x)))
       (list
         (cons 'lambda
               (cons (list x)
                     (append (mapcar #'(lambda (type)
					       (list (untype/literal type) x))
					       ; (types may be typed !)
				     types)
                             (list x))))
         expr)))


(defun untype (db)
  (execute-declare-facts db) ; move this to a better place ??
  (mapcar #'untype/clause (remove-declare-facts db)))

(defun untype/clause (clause)
  (cons (car clause) ; ft or hn
        (cons (untype/passive (cadr clause))
              (untype/body (cddr clause)))))

(defun untype/body (body)
  (mapcar #'untype/literal body))

(defun untype/literal (literal)
  (cond ((second-eq literal '\:)
	 (untype/active-type literal))
        ((fun-eq literal 'is)
         (list 'is
               (untype/passive (cadr literal))
               (untype/literal (caddr literal))))
        ((fun-eq literal 'inst)
         (mk-inst (untype/passive (cadr literal))))
	; lambda !!!  (wird erst noetig falls lambda vars komplexer !!)
	; getypte lambda vars noch nicht erlaubt !!
	((consp literal)
	 (untype/body literal))
        (T literal)))

(defun untype/passive (struct)
  (cond ((fun-eq struct 'inst)
	 (mk-inst (untype/passive (cadr struct))))
        ((fun-eq struct 'uninst)
         (list 'uninst (untype/literal (cadr struct))))
        ((second-eq struct '\:)
	 (untype/passive-type struct))
        ((fun-eq struct 'vari)
         struct)
        ((consp struct) ; inner structure
         (mapcar #'untype/passive struct))
        (T struct)))



; unmacro
; -------

(defun unmacro (db)
  (mapcar #'unmacro/clause db))

(defun unmacro/clause (clause)
  (cons (car clause) ; ft or hn
        (cons (unmacro/passive (cadr clause))
              (unmacro/body (cddr clause)))))

(defun unmacro/body (body)
  (mapcar #'unmacro/literal body))

(defun unmacro/literal (literal)
  (cond ((and (consp literal) ; user declared macro
	      (assoc (car literal) *horizon-macros*))
	 (unmacro/literal
	  (apply (cdr (assoc (car literal) *horizon-macros*)) literal)))
	((fun-eq literal 'is)
         (list 'is
               (unmacro/passive (cadr literal))
               (unmacro/literal (caddr literal))))
        ((fun-eq literal 'inst)
         (list 'inst (unmacro/passive (cadr literal))))
        ((fun-eq literal 'uninst) ; remove
	 (unmacro/literal (cadr literal)))
	; lambda ??? (not yet necessary)

	; predefined macros:
	((fun-eq literal 'progn) ; (progn f1 ... fn) ->
				 ; ((lambda () f1' ... fn'))
	 (list (cons 'lambda
		     (cons ()
			   (unmacro/body (cdr literal))))))

	((fun-eq literal 'let) ; (let ( v1 ... vn ) . body) ->
			       ; ((lambda (...) . body) ...)
         (let* ((var-bindings (cadr literal))
		(aux-vars (mapcan 
			   #'(lambda (v)
				     (when (fun-eq v 'vari)
					   (cons v nil)))
			   var-bindings))
		(vars (mapcan
		       #'(lambda (v)
				 (when (funfun-eq v 'vari)
				       (cons (car v) nil)))
		       var-bindings))
		(bindings (mapcan
			   #'(lambda (v)
				     (when (funfun-eq v 'vari)
					   (cons (cadr v) nil)))
			   var-bindings)))

	       (cons (cons 'lambda
			   (cons
			    (append vars (cons '&aux aux-vars))
			    (unmacro/body (cddr literal))))
		     (unmacro/body bindings))))

	((fun-eq literal 'let*)
	 (let* ((var-bindings (cadr literal))
		(aux-vars (mapcar #'(lambda (v)
					    (if (funfun-eq v 'vari)
						(car v)
						v))
				  var-bindings))
		(is-bindings (mapcan #'(lambda (v)
					       (when (funfun-eq v 'vari)
						;(cons (cons 'is v) nil)
						; does not work
						; (occur check)
						(gen-rec-binding v)))
				     var-bindings)))
	       (cons (cons 'lambda 
			   (cons (cons '&aux aux-vars)
				 (append
				  (unmacro/body is-bindings)
				  (unmacro/body (cddr literal)))))
		     nil))) ; no arguments!

	; ((eq literal '!) ; ! -> (!)  (is a relfun-extra)
	; (list '!))

	; fall through cases:
	((consp literal)
	 (unmacro/body literal))
	(T literal)))

(defun gen-rec-binding (v) ; generate recursive binding
  ; necessary because of a compiler error
  ; ( (is _x `(s _x)) is not compiled correctly )
  (let ((rb (list 'vari (gentemp "RB"))))
       (list (list 'is rb (cadr v))
	     (list 'is (car v) rb))))


(defun unmacro/passive (struct)
  (cond ((fun-eq struct 'inst)
	 (unmacro/passive (cadr struct)))
	((fun-eq struct 'uninst)
	 (list 'uninst (unmacro/literal (cadr struct))))
	((fun-eq struct 'vari)
	 struct)
	((consp struct) ; inner structure
	 (mapcar #'unmacro/passive struct))
	(T struct)))



; unor (or -> lambda)
; -------------------

(defun unor (db)
  (mapcar #'unor/clause db))

(defun unor/clause (clause)
  (cons (car clause) ; ft or hn
	(cons (unor/passive (cadr clause))
	      (unor/body (cddr clause)))))

(defun unor/body (body)
  (mapcar #'unor/literal body))

(defun unor/or (literal) ; literal = (or <arg1> ... <argn>)
  (let ((no-of-args (length (cdr literal))))
       (cond ((zerop no-of-args) 'unknown) ; empty disjunction = fail
	     ((= no-of-args 1) ; unary disjunction
	      (unor/literal (unor/literal (cadr literal))))
	     (T ; non-trivial disjunction -> lambda-or
		(list (cons 'lambda 
			    (list nil
				  (cons 'or (unor/body (cdr literal))))))))))

(defun unor/normalize-or (literal)
  (let ((no-of-args (length (cdr literal))))
       (cond ((zerop no-of-args) 'unknown) ; empty disjunction = fail
	     ((= no-of-args 1) ; unary disjunction
	      (unor/literal (unor/literal (cadr literal))))
	     (T (cons 'or (unor/body (cdr literal)))))))

(defun unor/lambda-or (literal) ; literal = (lambda (..) (or ...))
  (cons 'lambda
	(list (cadr literal) ; vars
	      (unor/normalize-or (caddr literal)))))

(defun unor/literal (literal)
  (cond ((fun-eq literal 'or) ; or -> lambda
	 (unor/or literal))
	((and (fun-eq literal 'lambda) ; lambda-or = (lambda (..) (or ...))
	      (= (length literal) 3)
	      (fun-eq (caddr literal) 'or))
	 (unor/lambda-or literal))
	((fun-eq literal 'lambda) ; (lambda (vars) . body)
	 (cons 'lambda
	       (cons (cadr literal) ; vars
		     (unor/body (cddr literal))))) ; body
	((fun-eq literal 'is)
	 (list 'is
	       (unor/passive (cadr literal))
	       (unor/literal (caddr literal))))
	((fun-eq literal 'inst)
	 (list 'inst (unor/passive (cadr literal))))
	((fun-eq literal 'uninst) ; remove
	 (unor/literal (cadr literal)))
	((consp literal)
	 (unor/body literal))
	(T literal)))

(defun unor/passive (struct)
  (cond ((fun-eq struct 'inst)
	 (unor/passive (cadr struct)))
	((fun-eq struct 'uninst)
	 (list 'uninst (unor/literal (cadr struct))))
	((fun-eq struct 'vari)
	 struct)
	((consp struct) ; inner structure
	 (mapcar #'unor/passive struct))
	(T struct)))


; unlambda
; --------


(defun unlambda (db)
  (mapcar #'unlambda/clause db))

(defun unlambda/clause (clause)
  (cons (car clause) ; ft or hn
	(cons (unlambda/passive (cadr clause))
	      (unlambda/body (cddr clause)))))

(defun unlambda/body (body)
  (mapcar #'unlambda/literal body))

(defun unlambda/lambda-or0 (literal) ; literal = ((lambda (..) (or ...)) ...)
  (let ((lo-vars (get-lambda-vars (cadar literal)))
	(clauses (mapcar #'list (cdar (cddar literal)))))
       (cons (unlambda/azft (fvars (car literal)) lo-vars clauses)
	     (unlambda/body (cdr literal)))))


(defun get-lambda-clauses (body)
  (if (funfun-eq body 'or) (mapcar #'list (cdar body)) (cons body nil)))

(defun unlambda/literal (literal)
  (cond ((and (funfun-eq literal 'lambda) ; lambda-or at first position
	      ; = ((lambda (..) (or ...)) ...)
	      (= (length (car literal)) 3)
	      (fun-eq (caddar literal) 'or))
	 (unlambda/lambda-or0 literal))
	((funfun-eq literal 'lambda) ; normal lambda at first position (no or)
	 ; will be flattened in uncomma
	 (cons
	  (cons 'lambda
		(cons (cadar literal) ; vars
		      (unlambda/body (cddar literal)))) ; body
	  (unlambda/body (cdr literal)))) ; args
	((fun-eq literal 'lambda) ; (lambda (vars) . body) as argument! 
	 (unlambda/azft (fvars literal) (get-lambda-vars (cadr literal))
			(get-lambda-clauses (cddr literal))))
        ((fun-eq literal 'is)
         (list 'is
               (unlambda/passive (cadr literal))
               (unlambda/literal (caddr literal))))
        ((fun-eq literal 'inst)
         (list 'inst (unlambda/passive (cadr literal))))
        ((fun-eq literal 'uninst) ; remove
         (unlambda/literal (cadr literal)))
        ((consp literal)
         (unlambda/body literal))
        (T literal)))

(defun unlambda/passive (struct)
  (cond ((fun-eq struct 'inst)
         (unlambda/passive (cadr struct)))
        ((fun-eq struct 'uninst)
         (list 'uninst (unlambda/literal (cadr struct))))
        ((fun-eq struct 'vari)
         struct)
        ((consp struct) ; inner structure
         (mapcar #'unlambda/passive struct))
        (T struct)))


(defun unlambda/azft (ho-vars lo-vars clauses) 
  ; returns lambdaX or `(lambdaX . ho-vars) where X is a number
  (let* ((new-lambda (gentemp "LAMBDA"))
	 (clause-name (if ho-vars
			  (cons new-lambda ho-vars)
			  new-lambda))
	 (horizoned-db (horizon-database 
			(mapcar #'(lambda (clause)
				    (cons 'ft
					  (cons 
					   (cons clause-name lo-vars) clause)))
				clauses))))
	(loco horizoned-db)
	(gwam.assemble-db horizoned-db '(.static))
	; store horizoned-db ??? .......
	(if ho-vars (list 'inst clause-name) clause-name)))


(defun fvars (expr) ; free variables
  (cond ((fun-eq expr 'lambda)
	 (set-difference (fvars (cddr expr)) (lambda-vars expr) :test #'equal))
	((fun-eq expr 'vari) (cons expr nil))
	((consp expr)
	 (remove-duplicates (mapcan #'fvars expr) :test #'equal))))

(defun lambda-vars (lambda-expr)
  (remove-if #'atom (cadr lambda-expr))) ; remove &aux

(defun get-lambda-vars (vars) ; all vars before &aux
  (unless (atom (car vars)) (cons (car vars) (get-lambda-vars (cdr vars)))))

(defun get-lambda-aux-vars (vars) ; all vars behind &aux
  (cdr (member '&aux vars)))


; hotrans: transformation of db for higher order operators (= hitrans)
;          and "higher order" structures
; --------------------------------------------------------------------

; 1. handling of declare facts:

(defvar *tupstructs*) ; tupified structures (with varying arity)
(setq *tupstructs* '(tupstruct))

(defvar *horizon-macros*)
(setq *horizon-macros* nil)


(defun execute-declare-facts (db)
  ; this function must be called *before* all other horirzontals!!
  ; (momentarily in 'untype')
  (mapcar #'(lambda (rule) ; search for (hn (declare ...))
	      (when (and (fun-eq rule 'hn)
			 (fun-eq (cadr rule) 'declare))
		    (mapcar #'exec-declare-stmt
			    (cdadr rule))))
	  db))

(defun exec-declare-stmt (stmt)
  (cond ((fun-eq stmt 'vari)) ; ignore (already compiled)
			      ; should not happen!
	((fun-eq stmt 'info)
	 (mapcar #'(lambda (info) (rf-format ";;; ~a~%" info))
		 (cdr stmt)))
	((fun-eq stmt 'tupstruct) ; tupified structures
	 (setq *tupstructs*
	       (remove-duplicates (append (cdr stmt) *tupstructs*))))
	((fun-eq stmt 'defun)
	 (eval stmt))
	((fun-eq stmt 'macro) ; (macro <name> <functional-obj>)
	 (setq *horizon-macros*
	       (acons (cadr stmt) (eval (caddr stmt)) *horizon-macros*)))
	; ... additional declarations ...
	(t (gformat "Warning: declaration ignored: ~a~%" stmt))))


(defun remove-declare-facts (db)
  ; pipe db through this before any other horizontals
  (remove-if #'(lambda (rule) (and (fun-eq rule 'hn)
				   (fun-eq (cadr rule) 'declare)))
	     db))


(defun tupstruct-p (atom) ; internal builtin (not directly accessable)
  (if (member atom *tupstructs*) t nil))

(defun not-tupstruct-p (atom) ; internal builtin
  (not (tupstruct-p atom)))


; 2. hotrans:

(defvar *head-transform*)

(defun hotrans (db)
  (mapcar #'hotrans/clause db))

(defun hotrans/clause (clause)
  (declare (special *head-tranform*))
  (setq *head-transform* nil)
  (cons (car clause) ; ft or hn
	(cons (let ((*head-transform* t)) ; hotrans/passive behaves
		                          ; slightly differently for heads!
		   (hotrans/passive (hotrans/head (cadr clause))))
	      (hotrans/body (cddr clause)))))

(defun hotrans/head (head) ; flatten structures in functor position
  (if (consp (car head))
      (if (member (caar head) '(vari inst uninst)) ; ...
	  (gerror "gwam" "hotrans/head"
		  "illegal head:~%~a" head)
	  (cons (caar head)
		(append (cdar head) (cdr head))))
      head))

(defun hotrans/body (body)
  (mapcar #'hotrans/literal body))

(defun hotrans/literal (literal)
  (cond ((funfun-eq literal 'lambda) ; ((lambda (vars) body) args)
	 (cons (cons 'lambda 
		      (cons (cadar literal) ; vars
			    (hotrans/body (cddar literal)))) ; body
	       (hotrans/body (cdr literal)))) ; args
	; ... Konstante wird von lambda berechnet ! ...         ??????
	; ... Struktur mit konstantem Funktor -> flatten ...    ??????
	((and (consp literal) ; apply
	      (consp (car literal)))
	 (list 'apply 
	       (car literal) 
	       (if (eq (cadr literal) '|\||)
		   (caddr literal)
		   (cons 'tup (hotrans/body (cdr literal))))
	       gasm.*default-module*)) ; change this !!! ???

	((fun-eq literal 'is)
	 (let ((passive-literal (cadr literal))
	       (active-literal (caddr literal)))
	      (cond ((and (real-struct-p/passive passive-literal)
			  (real-struct-p active-literal)
			  (or (ho-struct-p passive-literal)
			      (ho-struct-p active-literal)))
		     (hotrans/literal
		      (list 'tup2struct* ; not necessary in relational-is!!
			    (list 'is
				  (cons 'tup (get-struct passive-literal))
				  (list 'inst 
					(cons 'tup 
					    (get-struct active-literal)))))))
		    ((and (real-struct-p/passive passive-literal)
			  (ho-struct-p passive-literal))
		     (hotrans/literal
		      (list 'hi-struct-unify 
			    active-literal
			    (list 'inst 
				  (cons 'tup (get-struct passive-literal))))))
		    ((and (real-struct-p active-literal)
			  (ho-struct-p active-literal))
		     (hotrans/literal
		      (list 'hi-struct-unify 
			    (mk-inst passive-literal)
			    (list 'inst 
				  (cons 'tup (get-struct active-literal))))))
		    (t (list 'is 
			     (hotrans/passive (cadr literal))
			     (hotrans/literal (caddr literal)))))))

	((fun-eq literal 'inst)
	 (list 'inst (hotrans/passive (cadr literal))))
	((fun-eq literal 'uninst) ; remove
	 (hotrans/literal (cadr literal)))
	((and (consp literal)
	      (tupstruct-p (car literal)))
	 (list (car literal) 
	       (if (eq (cadr literal) '\|)
		   (hotrans/literal (caddr literal))
		   (cons 'tup (mapcar #'hotrans/literal (cdr literal))))))
	((consp literal)
	 (hotrans/body literal))
	(T literal)))

(defun hotrans/passive (struct)
  (declare (special *head-tranform*))
  (cond ((fun-eq struct 'inst)
	 (hotrans/passive (cadr struct)))
	((fun-eq struct 'uninst)
	 (list 'uninst (hotrans/literal (cadr struct))))
	((and (fun-eq struct 'tup) ; (tup | _x) ->
	      (eq (cadr struct) '\|)) ; ,((lambda () (check-tup _x) _x))
	 (hotrans/passive 
	  (list 'uninst 
		(list (list 'lambda ()
			    (list 'check-tup (mk-inst (caddr struct)))
			    (mk-inst (caddr struct)))))))
	((and (consp struct)
	      (consp (car struct)))
	 (if *head-transform*
	     (let ((new-var (list 'vari (gentemp "HS"))))
		  (hotrans/passive
		   (list 'uninst
			 (list
			  (list 'lambda ()
				(list 'hi-struct-unify new-var
				      (cons 'tup (get-struct struct)))
				new-var)))))
	     (list 'uninst
		   (list 'tup2struct*
			 (list 'inst
			       (cons 'tup (cons 
					   (car struct) 
					   (mapcar #'hotrans/passive
						   (cdr struct)))))))))
	((fun-eq struct 'vari)
	 struct)
	((consp struct) ; inner structure
	 (if (tupstruct-p (car struct))
	     (list (car struct) 
		   (if (eq (cadr struct) '\|)
		       (hotrans/passive (caddr struct))
		       (cons 'tup (mapcar #'hotrans/passive (cdr struct)))))
	     (cons (car struct) (mapcar #'hotrans/passive (cdr struct)))))
	(T struct)))


(defun real-struct-p (literal)
  (and (fun-eq literal 'inst)
       (real-struct-p/passive literal)))

(defun real-struct-p/passive (literal)
  (if (fun-eq literal 'inst)
      (real-struct-p/passive (cadr literal))
      (and (consp literal)
	   (not (member (car literal) '(vari tup uninst))))))

(defun ho-struct-p (struct) ; struct must have been tested with real-struct-p
  (if (fun-eq struct 'inst)
      (ho-struct-p (cadr struct))
      (funfun-eq struct 'vari)))

(defun get-struct (struct)
  (if (fun-eq struct 'inst) 
      (get-struct (cadr struct))
      struct))



; uncomma: removing comma expressions (and some trivial lambda exprs)
; -------------------------------------------------------------------

(defun uncomma (db)
  (mapcar #'uncomma/clause db))

(defun uncomma/clause (clause)
  (let ((head-pair (uncomma/passive (cadr clause))))
       (cons (car clause) ; ft or hn
	     (cons (cdr head-pair) ; new head
		   (append (car head-pair) (uncomma/body (cddr clause)))))))

(defun uncomma/body (body) ; (L1 L2 L3 ...) -> (L1" L1"" ... L1' L2" L2"" ...)
  (mapcan #'(lambda (pair) (append (car pair) (cons (cdr pair) nil)))
	  (mapcar #'uncomma/literal body)))

(defun uncomma/literal (literal) ; L -> ((L" L"" ...) . L')
  (cond ((and (consp literal) ; remove `,  as in (inst (uninst (f a b)))
	      (eq (car literal) 'inst)
	      (eq (caadr literal) 'uninst))
	 (uncomma/literal (cadadr literal)))
	((fun-eq literal 'uninst)
	 (uncomma/literal (cadr literal)))
	((fun-eq literal 'inst) ; structure
	 (let ((pair (uncomma/passive (cadr literal))))
	       (cons (car pair) (list 'inst (cdr pair)))))
	((fun-eq literal 'is)  ; is
	 (let ((pair1 (uncomma/passive (cadr literal)))
	       (pair2 (uncomma/literal (caddr literal))))
	      (cons (append (car pair1) (car pair2))
		    (list 'is (cdr pair1) (cdr pair2)))))
	((funfun-eq literal 'lambda)
	 (uncomma/lambda literal))
	((fun-eq literal 'vari) ; variable
	 (cons nil literal))
	((consp literal) ; active call (<> body !!)
	 (let ((pairs (mapcar #'uncomma/literal literal)))
	      (cons (mapcan #'(lambda (pair) (copy-list (car pair))) pairs)
		    (mapcar #'cdr pairs))))
	(T (cons nil literal))))

(defun uncomma/passive (parg) ; P -> ((P" P"" ...) . P')
  (cond ((and (consp parg) ; remove ,`
	      (eq (car parg) 'uninst)
	      (eq (caadr parg) 'inst))
	 (uncomma/passive (cadadr parg)))
	((fun-eq parg 'inst)
	 (uncomma/passive (cadr parg)))
	((fun-eq parg 'uninst) ; comma
	 (let ((pair (uncomma/literal (cadr parg))))
	      (if (subst-expr-p (cdr pair))
		  pair
		  (let ((newvar (cons 'vari (cons (gentemp "S") nil))))
		       (cons
			(append (car pair)
				(cons
				 (cons 'is 
				       (cons newvar 
					     (cons (cdr pair) nil))) nil))
			newvar)))))
	((fun-eq parg 'vari) ; variable
	 (cons nil parg))
	((consp parg) ; inner structure
	 (let ((pairs (mapcar #'uncomma/passive parg)))
	      (cons
	       (mapcan #'(lambda (pair) (copy-list (car pair))) pairs)
	       (mapcar #'cdr pairs))))
	(T (cons nil parg))))

(defun uncomma/lambda (lambda) ; ((lambda (vars) body) args)
  (let* ((body*expr (uncomma/flatten-lambda lambda))
         (body (uncomma/body (car body*expr)))
         (b1*ex1 (uncomma/literal (cdr body*expr))))
        (cons (append body (car b1*ex1))
              (cdr b1*ex1))))

(defun uncomma/flatten-lambda (lambda)
  (let* ((var-list (cadar lambda))
	 (vars (get-lambda-vars var-list))
	 (aux-vars (get-lambda-aux-vars var-list))
         (fcts (cddar lambda))
         (exprs (cdr lambda)))

	; 0. error checking
        (when (or (/= (length vars) (length exprs))
		  (/= (length vars) (length (remove-duplicates vars))))
	      ; ... check aux vars ...
          (gerror "gwam" "uncomma/flatten-lambda"
		  "wrong arguments for lambda expression"))


	; 1. rename aux vars
	(mapcar #'(lambda (aux-var) (let ((newvar (list 'vari 
							(gentemp "AUX"))))
					 (setq fcts
					       (lambda-var-subst 
						newvar aux-var fcts))
					 newvar))
		aux-vars)

	; 2. rename vars
        (setq vars
         (mapcar #'(lambda (var) (let ((newvar (list 'vari (gentemp "LV"))))
                                      (setq fcts
                                            (lambda-var-subst newvar var fcts))
                                      newvar))
                 vars))

	; 3. flatten
        (cons
         (append
          (mapcan #'(lambda (expr var)
                      (cond ((subst-expr-p expr)
                             (setq fcts (subst expr var fcts :test #'equal))
                             nil)
                            (T (cons (list 'is var expr) nil))))
                  exprs vars)
	  (butlast fcts))
	 (car (last fcts)))))


(defun lambda-var-subst (new old expr)
  (cond ((equal expr old) new)
	((fun-eq expr 'lambda) ; subst only if old not in lambda vars
	 (if (member old (cadr expr))
	     expr
	     (cons 'lambda
		   (cons (cadr expr) ; vars
			 (lambda-var-subst new old (cddr expr)))))) ; body
	((consp expr)
	 (mapcar #'(lambda (x) (lambda-var-subst new old x)) expr))
	(T expr)))






;;; Part 3c. Assert and Retract.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; ...



;;; Part 4. User Interface.
;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun interpret () ; execute one instruction
  (catch 'fail
	 (let ((instr (gmem.get (reg P))))
              (set-reg P (1+ (reg P))) ; should perhaps be done *in* instr.
	      (eval instr)))) ; !!


; assemble db
; -----------

(defun gwam.assem-cmd (user-line)
  (cond ((null user-line) (gwam.assem))
	((and (eq (car user-line) 'put) (stringp (cadr user-line)))
	 (gwam.put-db (cadr user-line)))
	((and (eq (car user-line) 'get) (stringp (cadr user-line)))
	 (gassem (cadr user-line)))
	((member (car user-line) '(mod module modules))
	 (gwam.mod-cmd (cdr user-line)))
	((member (car user-line) '(l list listing mem memory))
	 (gwam.mem-cmd (cdr user-line)))
	(T (gformat "incorrect command: ~a" user-line))))


(defun gwam.assem ()
  (gwam.assemble-db *rfi-database*))

(defun gwam.assemble-db (db &optional head-code) 
  ; call with *rfi-database* to assemble all
  ; head-code may contain module specification etc.
  (gassem (append head-code (gwam.db2asm-code db))))
  ; (gassem (uncut (append head-code (gwam.db2asm-code db))))) ; uncut!!

(defun gwam.db2asm-code (db)
  (mapcan #'gwam.get-asm-code
	  (remove-duplicates (mapcar #'gwam.name/arity db))))

(defun gwam.name/arity (clause)
  (let* ((head (cadr clause))
	 (name (car head))
	 (arity (length (cdr head))))
	(intern (format nil "~a/~d" name arity))))
	
(defun gwam.get-asm-code (name/arity)
  (append
   (list '.proc name/arity)
   (get name/arity 'procedure)
   (cons '.dend nil))) ; must be a *new* cons cell!! (mapcan!)

(defun gwam.put-db (name)
  (let ((*print-length* nil)
	(*print-pretty* T))
       (with-open-file (stream name :direction :output)
		       (print (list 'gasm
				    'author "from internal database"
				    'code (gwam.db2asm-code *rfi-database*))
			      stream)
		       (terpri stream))))


(defvar gwam.*show-address*)
(setq gwam.*show-address* 0)

(defun gwam.mem-cmd (args)
  (cond ((null args) ; show next 20 cells
	 (gmem.list gwam.*show-address* (+ gwam.*show-address* 19))
	 (setq gwam.*show-address* (+ gwam.*show-address* 20)))
	((and (= (length args) 1) ; start address specified
	      (integerp (car args))
	      (>= (car args) 0))
	 (gmem.list (car args) (+ (car args) 19))
	 (setq gwam.*show-address* (+ (car args) 20)))
	((and (= (length args) 2) ; start and end address specified
	      (integerp (car args))
	      (>= (car args) 0)
	      (integerp (cadr args))
	      (>= (cadr args) 0))
	 (gmem.list (car args) (cadr args))
	 (setq gwam.*show-address* (1+ (cadr args))))
	(T (gformat "incorrect mem args: ~a" args))))

(defun gwam.mod-cmd (args)
  (cond ((null args) ; show all module names
	 (let ((*style* 'lisp))
              (gpprint gasm.*modules*) 
              (gterpri)))
	(T (mapcar #'gwam.show-module args))))

(defun gwam.show-module (module-name)
  (let ((mod (gcla.get gasm.*modules* module-name)))
       (cond (mod
	      (gformat "Module ~a:~%~%" module-name)
	      (gmht.show (gcla.get mod 'name-space)))
	     (T (gformat "Module ~a does not exist.~%" module-name)))))


; transformations on assembler code 
; ---------------------------------

#|
; uncut: (preliminarily built in)

(defun uncut (asm) 
  (let ((scp-ps*fc-ps (uncut-collect-positions asm 1 -1 T nil nil)))
       (swap-dealloc-cut 
	(transform-cuts asm 1 (car scp-ps*fc-ps) (cdr scp-ps*fc-ps) -1))))
  
(defun transform-cuts (asm no scp-ps fc-ps env-size)
  (unless (null asm)
    (let ((stmt (gasm.transform-instr (car asm))))
	 (cond ((is-cut-instruction stmt)
		(cons (if (member no fc-ps :test #'=)
			  '(first_cut)
			  (list 'cut env-size))
		      (transform-cuts (cdr asm) (1+ no) scp-ps fc-ps env-size)))
	       ((eq (car stmt) 'allocate)
		(if (member no scp-ps :test #'=) 
		    (cons (car asm) 
			  (cons '(save_cut_pointer) 
				(transform-cuts 
				 (cdr asm) (1+ no) scp-ps fc-ps (cadr stmt))))
		    (cons (car asm)
			  (transform-cuts (cdr asm) (1+ no) scp-ps fc-ps
					  (cadr stmt)))))
	       ((eq (car stmt) 'call) ; env trimmed ?
		(cons (car asm)
		      (transform-cuts (cdr asm) (1+ no) scp-ps fc-ps
				      (caddr stmt)))) ; (new) env size
	       (t (cons (car asm) 
			(transform-cuts (cdr asm) (1+ no) scp-ps fc-ps
					env-size)))))))
	  
(defun uncut-collect-positions (asm no active-pos first-cut scp-ps fc-ps)
 ; collect all positions where save_cut_pointer stmts must be inserted
 ; and which cuts are first cuts (no allocate!)
 (if (null asm)
  (cons scp-ps fc-ps)
  (let ((stmt (gasm.transform-instr (car asm))))
       (cond ((eq (car stmt) 'allocate)
              (uncut-collect-positions (cdr asm) (1+ no) no nil scp-ps fc-ps))
             ((or (eq (car stmt) '.global)
		  (eq (car stmt) '.local))
              (uncut-collect-positions (cdr asm) (1+ no) -1 T scp-ps fc-ps))
             ((is-cut-instruction stmt)
              (uncut-collect-positions 
                (cdr asm) (1+ no) active-pos first-cut
                (if first-cut
                    scp-ps
                    (cons active-pos scp-ps))
                (if first-cut (cons no fc-ps) fc-ps)))
             (t (uncut-collect-positions 
                  (cdr asm) (1+ no) active-pos first-cut scp-ps fc-ps))))))
        
(defun is-cut-instruction (stmt)
  (and (eq (car stmt) 'cl-relf)
       (eq (cadr stmt) '!)))
     

(defun swap-dealloc-cut (asm)
  ; swap the sequence (deallocate <n>) (cut <n>)
  (cond ((null asm) nil)
	((and (consp (cdr asm)) ; at least 2 instructions left
	      (eq (car (gasm.transform-instr (car asm))) 'deallocate)
	      (eq (car (gasm.transform-instr (cadr asm))) 'cut))
	 (cons (cadr asm)
	       (cons (car asm)
		     (swap-dealloc-cut (cddr asm)))))
	(t (cons (car asm) (swap-dealloc-cut (cdr asm))))))
|#

; emulate
; -------

(defun emu (anruf) (emulate anruf))

(defun emulate (mainrest)
  (declare (special *lureg*))
  (when (init)
	(setq *user-variables* nil)
	(let* ((main      (first mainrest))
	       (mainlit   (car main))
	       (vars      (rest main))
	       (len       (length vars))
	       (name-len
		(intern (format nil "~a/~a" mainlit len)))
	       singlevar
	       classmain
	       codemain)
	      (if (noteq mainlit 'main) (gerror "gwam" "emulate"
						"emulate called in a weired way"))
	      (putniv name-len 'clause (list mainrest))
	      (setq classmain (classify-db (list name-len)))
	      (when *emu-debug* (gterpri) (gformat "~a~%" classmain))
	      (setq codemain (code-gen-proc (car classmain)))
	      (when *emu-debug* (gterpri) (gprint codemain) (gterpri))
	      (gassem (append '( (.module user) .dynamic query) codemain))
	      
	      (dotimes (i len)
		(setq singlevar (nth i vars))
		(set-argument-reg (1+ i)
		  (cdar
		   (if (assoc singlevar *user-variables*)
		       (new-value (cdr (assoc singlevar *user-variables*)))
		       (let ((temp (new-variable)))
			    (setq  *user-variables* 
				   (cons (cons singlevar temp) *user-variables*)))))))
	      (setq  *user-variables* (reverse *user-variables*))
	      (set-reg P (gasm.get-global-adr 'query* '(system)))
	      (stepp))))


(defun run (&rest vars &aux singlevar) ; execute procedure 'query'
  
  (when (init)
	(setq *user-variables* nil)
	
	(set-reg P (gasm.get-global-adr 'query* '(system)))
	
	(dotimes (i (length vars))
		 (setq singlevar (nth i vars))
		 (set-argument-reg (1+ i)
		   (cdar
		    (if (assoc singlevar *user-variables*)
			(new-value (cdr (assoc singlevar *user-variables*)))
			(let ((temp (new-variable)))
			     (setq *user-variables*
				   (cons (cons singlevar temp) *user-variables*)))))))
	(setq  *user-variables* (reverse *user-variables*))
	(stepp)))



;P-registret inneh}ller i value-f{ltet en cons av procedurnamn och
;resterande kod.


(defvar *emulator-init-allowed*)
(setq *emulator-init-allowed* t)

(defun init () ; returns t if init succeeded
  (cond (*emulator-init-allowed*
	 (set-reg E (list :ref start-of-stack))
	 (set-reg B (list :ref start-of-stack))
	 (set-reg A (list :ref start-of-stack))
	 (set-reg H (list :ref start-of-heap))
	 (set-reg HB (list :ref start-of-heap))
	 (set-reg TR (list :trail nil))
	 (set-reg S (list :ref start-of-heap))
	 t)
	(t (gformat "Emulator already in use -- please switch to interpreter or type `bye'")
	   nil)))



(defun printmseg (loc1 loc2)
  (gterpri)
  (do ((i loc1 (1+ i)))
      ((> i loc2))
    (printmloc i)
    (if (eql i (address (reg E))) (gformat " <== E"))
    (if (eql i (address (reg B))) (gformat " <== B"))
    (if (eql i (address (reg H))) (gformat " <== H"))
    (if (eql i (address (reg HB)))(gformat " <== HB"))
    (if (eql i (address (reg S))) (gformat " <== S")))
  (gterpri))

(defun printmloc (i)
  (gformat "~% [~a] = " i)
  (gmem.print-mem-cell (gmem.get i)))
  ; (gformat "~% [~a] = ~a " i (gmem.get i)))

(defun printreg (name)
  (gformat "~% Register ~a = ~a " name (get name 'reg-value)))


; Warning : THIS ROUTINE MAY BE MACHINE DEPENDENT

(defun step-get-char ()
 (do ((char (read-char) (read-char)))
     ((noteq char #\Newline) (make-char char))
     nil))


(defun stepp ()
   (catch
    'halt
    (if *emu-debug*
      (loop (let ((adr (reg P)))
               (gformat "~% [~a] = " adr)
               (gmem.print-mem-cell (gmem.get adr))
	       (gformat " : "))
	  (case (step-get-char)
	    ((#\? #\H #\h)
	     (print-stepp-help))
	    ((#\Newline #\Space #\S #\s) (interpret))
	    ((#\R #\r)
	     (loop (interpret)))
	    ((#\F #\f)
	     (catch 'fail (fail)))
	    ((#\E #\e)
	     (throw 'halt nil))
	    ((#\V #\v)
	     (show-value-of-something))
	    (t (gformat "~% Unknown input, type ? for help "))
          ) ; case
    ) ; loop
    (loop (interpret))
   ) ; if
   ) ; catch
) ; defun
           ; ^ this is the otherwise case

(defun print-stepp-help ()
  (gformat
"~%    All commands consist of one character.

     E,e                Terminate and go to LISP.
     F,f                Generate a fail. (Sometimes this command may
                          cause trouble.)
     H,h,?              Output this Help-Menu.
     R,r                Execute until program succeeds.
     S,s                Single step execution.
     V,v                Output values before single step.
"))

(defun show-value-of-something ()
  (declare (special *registers*))
  (gformat "~% Value of? ")
  (case (step-get-char)
    ((#\?)(value-help)
	  (show-value-of-something))
    ((#\A #\a)
    (gformat "~% Number of argument registers: ")
     (dotimes (i (read))
       (gformat "~% A(~a) = ~a" (1+ i) (argument-reg (1+ i))))
     (gterpri))
    ((#\H #\h)
     (printmseg start-of-heap (address (reg H))))
    ((#\S #\s)
     (printmseg start-of-stack (address (reg A))))
    ((#\R #\r)
     (gterpri)
     (dolist (x *registers*)
       (gformat "   Reg ~a = ~a ~%" x (get x 'reg-value)))
     (gterpri))
    (t (gformat "~% Unknown input, type ? for help "))))


(defun value-help ()
  (gformat
"~%    All commands consist of one character.

     ?                  Output this Help-Menu.
     A,a                Output n (to be read) argument registers
                          A(0)..A(n-1).
     H,h                Output Heap.
     R,r                Output all registers exept argumentregisters.
     S,s                Output stack.

"))


(defun construct-term (term)
  (cond
   ((null term)
    (constant-nil))
   ((atom term)
    (constant term))
   ((and (consp term) (eq (car term) 'cons))
    (construct-conslist (cdr term)) )
   ((and (consp term) (eq (first term) 'vari))
    (construct-variable (second term)))
   ((consp term)
    (construct-list term))
   (t
    (let ((temp (map 'list #'construct-term (rest term)))
	  (ref
	   (new-struc (functor (list (first term) (length (rest term)))))))
     (mapc #'new-value temp)
     ref))))

(defun construct-variable (var)
  (declare (special *lureg*))
  (cond ((numberp var)
	 (new-value (list :ref var)))
	((assoc var *user-variables*)
	 (new-value (cdr (assoc var *user-variables*))))
	(t
	 (let ((temp (new-variable)))
	   (setq  *user-variables* (cons (cons var temp) *user-variables*))
	   temp))))

(defun construct-list (term)
  (let ((head (construct-term (cons-car term)))
	(tail (construct-term (cons-cdr term)))
	(ref (new-list-cell)))
    (new-value head)
    (new-value tail)
    ref))

(defun construct-conslist (term)
  (let ((head (construct-term (car term)))
	(tail (construct-term (cdr term)))
	(ref (new-list-cell)))
    (new-value head)
    (new-value tail)
    ref))


#| show term(s) replaced, see some pages above (near cl-extra)

(defun show-term (term tupflg)
  (ecase (word-tag term)
   ((const) (word-value term))
   ((ref) (if (varp term) (list 'vari '|Ny| (word-value term))
		    (show-term (mem term) t )))
   ((list) (if tupflg
            (cons 'tup
	     (cons (show-term (mem term) t)
		   (show-term (mem (ref-plus term 1)) nil)
             )
	    )
	    (cons (show-term (mem term) t)
		  (show-term (mem (ref-plus term 1)) nil)
            )
           ) ; if
   )
   ((struct)
    (ecase (word-tag (mem term))
     ((fun)
      (let ((name (first (word-value (mem term))))
	    (arity (second (word-value (mem term))))
	    (ref   (ref-plus term 1)))
	(cons name (show-terms ref arity))
      )
     )
    ) ; ecase2
   ) ; struct
 ) ; ecase1
)

(defun show-terms (ref arity)
  (if (zerop arity) nil
      (cons (show-term (mem ref) t)
	      (show-terms (ref-plus ref 1) (1- arity)))))

; This function is called, when a goal is satisfied,
; additionally X1 must be written to the screen.


|#


; builtins
; --------

(gassem (namestring (logdir-file :emulator "gwaminit.asm"))) 

(setq gasm.*default-unknown-label* 
  (gasm.get-global-adr 'fail '(system)))

(if (probe-file (namestring (logdir-file :RFM "prelude.asm"))) 
  (gassem (namestring (logdir-file :RFM "prelude.asm"))))

(terpri)

