
;;; A horn clause FORWARD emulator
;;; Common Lisp, Author : Christian Falter

;;; Version 1.2 (Matching und top-down-proofs mit GAMA)



;;; heap word

(defstruct *word (*tag 'empty) (*value nil))


;;; heap

(defparameter *heap-size 20000)
(defun fa-make-heap () (make-array *heap-size :initial-element (make-*word)))      ; <--- fuer Xlisp rausschmeissen
(defvar *heap (fa-make-heap))


;;; value registers X1...Xn (heap-pointers)

(defparameter *x-register-size 10)
(defun fa-make-x-registers () (make-array *x-register-size))
(defvar *x-registers (fa-make-x-registers))


;;; alpha/beta memories MEM1...MEMn (array of a-lists of a-lists)

(defparameter *a-b-memory-size 50)
(defun fa-make-a-b-memories ()
   (let ((array (make-array *a-b-memory-size)))   ; initial element equal but not! eq
        (dotimes (i *a-b-memory-size)
            (setf (aref array i) (list (cons 'count-max 0))))
         array))
(defvar *a-b-memories (fa-make-a-b-memories))


;;; control-stack

(defvar *cstack nil)


;;; special registers

(defvar *F nil)             ;fact pointer                  (pointer to heap)
(defvar *NP nil)            ;instruction pointer           (pointer to code area)
(defvar *TOP nil)           ;top of heap                   (pointer to heap)
(defvar *REM nil)           ;top of fact-reference-chain   (pointer to heap)
(defvar *P nil)             ;aux. pointer to actual fact   (pointer to heap)
(defvar *I nil)             ;pointer to first infered fact (pointer to heap)


;;; code area (array)

(defparameter *code-area-size 200)
(defun fa-make-code-area () (make-array *code-area-size))
(defvar *code (fa-make-code-area))


;;; instruction definitions (property-list)

(defvar *instructions nil)


;;; auxiliary global variables

(defvar *fa-trace* nil)            ; stepper mode
(defvar *fa-dump* nil)             ; trace emulator without escaping
(defvar *fa-direction* 'left)      ; determines behaviour of interelement instructions
(defvar *fa-fact-base* nil)        ; contains all initial facts
(defvar *fa-infered-facts* nil)    ; contains all infered facts
(defvar *fa-WAM-bindings* nil)  ; enthaelt nach WAM-Aufruf alle zurueckgegebenen Bindungen


;;; defines an instruction in property-list "*instructions"

(defmacro fa-definstr (name params &rest body)
   (list 'setf
         (list 'get (list 'quote name) '(quote *instructions))
         (list 'function
               (cons 'lambda
                      (cons params body)))))


;;; defines labeled forward code in array "*code"

(defmacro deflabel (label &rest body)
   `(setf (aref *code ,label) (quote ,body)))


;;; stack handling

(defun empty (stack)
   (null stack))

(defun top (stack)
   (if (empty stack)
        nil
       (first stack)))


;;; make heap-cells

(defun fa-con-cell (c)
   (make-*word :*tag 'con
              :*value c  ))

(defun fa-con-nil-cell ()
   (make-*word :*tag 'con
              :*value nil ))

(defun fa-ref-cell (reference)
   (make-*word :*tag 'ref
              :*value reference))

(defun fa-fun-cell (funct arity)
   (make-*word :*tag 'fun
              :*value (list funct arity )))

(defun fa-pred-cell  (pred arity)
   (make-*word :*tag 'pred
              :*value (list pred arity  )))

(defun fa-fact-cell ()
   (make-*word :*tag 'fact
              :*value nil ))


;;; access heap-elements

(defun fa-cell (ref)
   (aref *heap ref))

(defun fa-set-cell (ref value)
   (setf (aref *heap ref) value))

(defun fa-tag (ref)
   (*word-*tag (fa-cell ref)))

(defun fa-value (ref)
   (*word-*value (fa-cell ref)))

(defun fa-functor (ref)
   (when (equal (fa-tag ref) 'fun)
         (first (fa-value ref))))

(defun fa-predicate (ref)
   (when (equal (fa-tag ref) 'pred)
         (first (fa-value ref))))

(defun fa-arity (ref)
   (when (or (equal (fa-tag ref) 'pred)
             (equal (fa-tag ref) 'fun ))
         (second (fa-value ref))))


;;; access x-registers

(defun fa-x-reg (n)
   (aref *x-registers n))

(defun fa-set-x-reg (n value)
   (setf (aref *x-registers n) value))


;;; access alpha/beta memories

   ; get a-b-memory

(defun fa-memory (mem)
   (aref *a-b-memories mem))

(defun set-fa-memory (mem value)
  (setf (aref *a-b-memories mem) value))

   ; get entry count in a-b-memory

(defun fa-count-max (mem)
   (cdr (assoc 'count-max (fa-memory mem))))

   ; increment entry count in a-b-memory

(defun fa-inc-count-max (mem)
   (let ((memory (fa-memory mem))
         (count-max (fa-count-max mem)) )
        (rplacd (assoc 'count-max memory) (1+ count-max))))

   ; decrement entry count in a-b-memory

(defun fa-dec-count-max (mem)
   (let ((memory (fa-memory mem))
         (count-max (fa-count-max mem)) )
        (rplacd (assoc 'count-max memory) (1- count-max))))

   ; get nth binding of the variable

(defun fa-nth-binding (mem variablename n)
   (cdr (assoc variablename (fa-nth-partial-inst mem n))))

   ; get new binding of the variable

(defun fa-newest-binding (mem variablename)
   (cdr (assoc variablename
           (cdr (assoc (fa-count-max mem) (fa-memory mem))))))

  ; get nth partial instantiation

(defun fa-nth-partial-inst (mem n)
   (cdr (assoc n (fa-memory mem))))

  ; get newest partial instantiation

(defun fa-newest-partial-inst (mem)
   (cdr (assoc (fa-count-max mem) (fa-memory mem))))

  ; store a partial instantiation

(defun fa-set-new-inst (mem inst)
   (fa-inc-count-max mem)
   (let ((count-max (fa-count-max mem)) )
     (set-fa-memory mem (append (fa-memory mem) `((,count-max . ,inst))))))

        ;;;(rplacd (last (fa-memory mem)) (list (cons count-max inst)))))

  ; store a new binding for a variable in a partial inst.
  ; if there is yet a binding in the last inst. than this binding is 
  ; changed !!

(defun fa-set-new-binding (mem varname value)
   (let* ((memory (fa-memory mem))
          (count-max (fa-count-max mem))
          (counth-inst (assoc count-max memory))
	  (var-binding (assoc varname (cdr counth-inst))) )
         (if (null counth-inst)
	     (set-fa-memory mem
		 (append memory `((,count-max (,varname . ,value)))))
         ;;;;(rplacd (last memory) (list (cons count-max (list (cons varname value))))) ;create new
	    (if var-binding
		(setf (cdr var-binding) value)
	      (set-fa-memory mem
		   (append (reverse (cdr (reverse memory))) ;; all but the last
			   (list  (append counth-inst `((,varname . ,value))))))
	      ))))
         ;;;;(rplacd (last (cdr counth-inst)) (list (cons varname value))))))           ;update new

  ; remove a duplicate from a-memory

(defun fa-remove-newest-inst (mem)
   (let* ((memory (fa-memory mem))
          (n (1- (1- (length memory)))))
         (dotimes (i n) (setq memory (cdr memory)))
         (rplacd memory nil)
         (fa-dec-count-max mem)))
          


;;; some auxiliary functions for instruction definition

(defun fa-fail ()                                  ; exit the execution of instructions
   (when *fa-trace* (print 'fail))
   (setq *ref-stack nil)
   (throw 'fail T))

(defun fa-next-top-position ()                     ; increment *TOP
   (setq *TOP (1+ *TOP)))


(defvar *ref-stack nil)                          ; store bound-REF-cell return addresses

(defun bound-ref-cell (ref)                     ; is the cell denoted by ref a bound REF cell ?
   (and (equal (fa-tag ref) 'ref)
        (not (= (fa-value ref) ref))))

(defun unbound-ref-cell (ref)                   ; is the heap-cell denoted by ref an unbound variable?
  (and (equal (fa-tag ref) 'ref)
       (= (fa-value ref) ref)))

(defun decrement-arity-on-top ()                ; decrement arity on top of ref-stack
   (let* ((top (top *ref-stack))
          (top-arity (first top))
          (top-return (second top)))
         (pop *ref-stack)
         (push (list (1- top-arity) top-return) *ref-stack)))

(defun add-to-arity-on-top (arity)              ; add arity of new structure to arity
   (let* ((top (top *ref-stack))                  ; on top of ref-stack
          (top-arity (first top))
          (top-return (second top)))
         (pop *ref-stack)
         (push (list (+ arity top-arity) top-return) *ref-stack)))


(defun next-p-position (&optional (arity 0))    ; compute next position for P, taking into account 
   (if (empty *ref-stack)                        ; that fact terms may reside outside of the actual 
       (if (bound-ref-cell (1+ *P))              ; fact (bound REF cells, use ref-stack for return-
           (progn                               ; addresses)
            (push (list arity (1+ (1+ *P))) *ref-stack)
            (setq *P (fa-deref (1+ *P))))
           (setq *P (1+ *P)))
       (if (equal (first (top *ref-stack)) 0)
           (progn
            (setq *P (second (top *ref-stack)))
            (pop *ref-stack)
            (when (bound-ref-cell *P)
                  (push (list arity (1+ *P)) *ref-stack)
                  (setq *P (fa-deref *P))))
           (progn
            (decrement-arity-on-top)
            (if (bound-ref-cell (1+ *P))
                 (progn
                  (if (equal (first (top *ref-stack)) 0) 
                      (let ((return (second (top *ref-stack))))
                           (pop *ref-stack)
                           (push (list arity return) *ref-stack))
                      (push (list arity (1+ (1+ *P))) *ref-stack))
                 (setq *P (fa-deref (1+ *P))))
                (progn
                 (setq *P (1+ *P))
                 (when (> arity 0)
                       (add-to-arity-on-top arity))))))))


(defun fa-deref (pointer)                          ; dereference a REF cell
   (if (and (equal (fa-tag pointer) 'ref)
            (not (equal (fa-value pointer) pointer)))
       (fa-deref (fa-value pointer))
       pointer))


(defun jump-term (pointer)                      ; compute heap address of first cell following
   (case (fa-tag pointer)                          ; the term denoted by pointer
      ('ref  (1+ pointer))
      ('con (1+ pointer))
      ('fun  (jump-arguments (fa-arity pointer) (1+ pointer)))))

(defun jump-arguments (arity pointer)
   (if (zerop arity) pointer
       (jump-arguments (1- arity) (jump-term pointer))))


(defun fa-term-equal (p1 p2)                           ; test whether terms denoted by heap
   (catch 'equal (fa-term-equal1 p1 p2)))              ; pointers p1 and p2 are equal

(defun fa-term-equal1 (p1 p2)
  (if (bound-ref-cell p1)
      (if (bound-ref-cell p2)
	  (progn
	    (fa-term-equal1 (fa-deref p1) (fa-deref p2))                  ; p1,p2 bound ref cells
	    (list (1+ p1) (1+ p2)))
	(list (1+ p1) (second (fa-term-equal1 (fa-deref p1) p2))))   ; p1 bound ref cell
    (if (bound-ref-cell p2)
	(list (first (fa-term-equal1 p1 (fa-deref p2))) (1+ p2))     ; p2 bound ref cell
      (case (fa-tag p1)
	    ('ref  (if (not (equal (fa-tag p2) 'ref))
		       (throw 'equal nil)
		     (list (1+ p1) (1+ p2))))
	    ('con (if (or (not (equal (fa-tag p2) 'con))         ; p1,p2 : - unbound ref cell
			  (not (equal (fa-value p1) (fa-value p2)))) ;         - con cell
		      (throw 'equal nil)                       ;         - fun cell
		    (list (1+ p1) (1+ p2))))
	    ('fun  (if (or (not (equal (fa-tag p2) 'fun))
			   (not (equal (fa-value p1) (fa-value p2))))
		       (throw 'equal nil)
		     (fa-arguments-equal (1+ p1) (1+ p2) (fa-arity p1))))))))

(defun fa-arguments-equal (p1 p2 arity)
   (if (zerop arity)
       (list p1 p2)
       (let* ((term-equal1-value (fa-term-equal1 p1 p2))         ; test first argument
              (next-p1 (first term-equal1-value))
              (next-p2 (second term-equal1-value)))
             (fa-arguments-equal next-p1 next-p2 (1- arity)))))  ; test rest arguments recursively


(defun consistent-union (inst1 inst2)              ; liefert die konsistente Vereinigungs-
   (catch 'inconsistent                            ; menge zweier partieller Instanziierungen
      (mapcar #'(lambda (binding)                  ; falls es sie gibt, ansonsten nil
                  (let* ((name2  (car binding))
                         (value2 (cdr binding))
                         (value1 (cdr (assoc name2 inst1))) )
                     (if (null value1)
                         (setq inst1 (cons binding inst1))
                         (unless (equal value1 value2) ;consistent?
                                 (throw 'inconsistent nil)))))
              inst2)
      inst1))

(defun search-consistent-union (direction mem1 mem2 next)
   (let*
      ((active-mem   (if (equal direction 'left) mem1 mem2))
       (opposite-mem (if (equal direction 'left) mem2 mem1))
       (inst-in      (fa-newest-partial-inst active-mem))
       (inst-out                                    ; search for a consistent union of
           (catch 'found                            ; instantiations in memorys 1 and 2
             (let (oppos-inst union (i next)) 
               (loop
                 (setq oppos-inst (fa-nth-partial-inst opposite-mem i))
                 (if (null oppos-inst)
                     (throw 'found nil)             ; no more inst in opp. mem to try
                     (progn
                      (setq union (consistent-union inst-in oppos-inst))
                      (when union                   ; remember next opp. inst to test
                            (setq next (1+ i))
                            (throw 'found union))))
                 (setq i (1+ i)))))))
      (list inst-out next)))



;;; the INTERPRETER :

   ;;; execute instruction list labeled with *NP

(defun fa-execute ()
   (catch 'fail                             ; "throw 'fail" from an instruction exits the loop
      (loop
         (let ((save-np *NP))                ; apply instructions to code labeled with *NP
            (mapcar #'(lambda (instr)
                        (apply (get (first instr) '*instructions) (rest instr)))
                    (let ((code-list (aref *code *NP)))
                       (if (null code-list)
                          (error "label doesn't exist")
                          code-list)))
            (when (equal save-np *NP)         ; increment instruction pointer *NP, unless it was
                  (setq *NP (1+ *NP)))))))    ; modified by "merge-path" or "switch-on-predicate"


   ;;; the network interpreter

(defun fa-interpret ()                              ; (*F -> fact-cell )
   (setq *P (1+ *F))                                ; (*P -> pred-cell ) !
   (fa-execute)                                     ; instructions handle *P, not *F
   (when (and *fa-trace* (not *fa-dump*)) (fa-escape)) ; in stepper mode escape here
   (if (not (empty *cstack))
       (progn (setq *NP (top *cstack))              ; DFS through network
              (pop *cstack)
              (fa-interpret))
       (if (fa-value *F)                            ; if there are more facts in fact-
           (progn                                   ; chain, start program with new fact
              (when *fa-trace*
                 (terpri)
                 (princ "; input fact : ")
                 (princ (get-fact (fa-value *F)))
                 (terpri))
              (setq *F (fa-value *F))
              (setq *NP 1)
              (fa-interpret))
           (progn
              (when *fa-dump* (dump-emulator-state))
              (if (fa-value *I)
                  (setq *fa-infered-facts* (get-all-facts (fa-value *I)))
                  (princ "*** no inferences have been made !"))
              (terpri)(terpri)
              (princ "*** READY")   ; else ready
              (terpri)
              (terpri)))))


;;;    INSTRUCTIONS :

   ;;; intraelement test instructions

(fa-definstr switch-on-predicate (pred arity label)
   (when *fa-trace* (print (list 'switch-on-pred pred arity label)))
   (if (not (equal (fa-tag *P) 'pred))
       (error "pred-cell expected")
       (when (and (equal (fa-predicate *P) pred)
                  (equal (fa-arity *P) arity))
             (setq *NP label))))

(fa-definstr get-structure (func arity)
   (when *fa-trace* (print (list 'get-structure func arity)))
   (next-p-position arity)
   (unless (and (equal (fa-functor *P) func)
                (equal (fa-arity *P) arity))
           (fa-fail)))

(fa-definstr get-constant (const)
   (when *fa-trace* (print (list 'get-constant const)))
   (next-p-position)
   (unless (and (equal (fa-tag *P) 'con)
                (equal (fa-value *P) const))
           (fa-fail)))

(fa-definstr get-variable (xreg)
   (when *fa-trace* (print (list 'get-variable xreg)))
   (next-p-position)
   (fa-set-x-reg xreg *P)
   (setq *P (1- (jump-term *P))))

(fa-definstr get-value (xreg)
   (when *fa-trace* (print (list 'get-value xreg)))
   (next-p-position)
   (if (fa-term-equal (fa-x-reg xreg) *P)
       (setq *P (1- (jump-term *P)))
       (fa-fail)))

(fa-definstr new-instantiation (mem)
   (fa-inc-count-max mem))

(fa-definstr store-binding (xreg varname mem &optional a-memory-list)
   (let ((new-binding    (fa-x-reg xreg))
         (all-a-memories (if a-memory-list 
                             (cons mem a-memory-list)
                             (list mem))))
      (when *fa-trace* (print (list 'store-binding varname '= new-binding mem)))
      (catch 'unique
         (dolist (i all-a-memories)
            (let ((max (if (equal i mem)                              ; inconsistent count-max in mem
                           (1- (fa-count-max i))                         ; caused by new-instantiation !
                           (fa-count-max i))))
               (dotimes (j max)
                  (let ((old-binding (fa-nth-binding i varname (1+ j)))) ; 1..max
                     (if (equal new-binding old-binding)
                        (progn
                         (fa-set-new-binding mem varname new-binding)    ; new = old already unique
                         (throw 'unique T))
                        (when (fa-term-equal new-binding old-binding)
                           (fa-set-new-binding mem varname old-binding)  ; old already unique
                           (throw 'unique T)))))))
         ;uniqueness of new-binding tested in all a-memories thus:
         (fa-set-new-binding mem varname new-binding))))

(fa-definstr check-duplicates (mem)
   (when *fa-trace* (print (list 'check-duplicates mem)))
   (let ((max (fa-count-max mem)))
      (when (> max 1)
         (let ((new-inst (fa-newest-partial-inst mem)))
            (dotimes (i (1- max))
               (unless (catch 'ok 
                          (dolist (pair new-inst)
                             (let ((var (car pair))
                                   (val (cdr pair)))
                                  (when (not (equal val (fa-nth-binding mem var (1+ i))))
                                        (throw 'ok T))))
                           nil)
                       (fa-remove-newest-inst mem)
                       (fa-fail)))))))


   ;;; network linearisation instructions

(fa-definstr fork-path (label)
   (when *fa-trace* (print (list 'fork-path label)))
   (push label *cstack))

(fa-definstr merge-path (label)
   (when *fa-trace* (print (list 'merge-path label)))
   (setq *NP label)
   (setq *fa-direction* 'right))


   ;;; interelement test instructions

(fa-definstr propagate (mem1 mem2 mem3)
   (when *fa-trace*
      (print (list 'propagate mem1 mem2 mem3)))
   (let (direction
         opposite-mem
         opposite-max
         search-result
         inst-out
         next)
      (if (equal (top *cstack) *NP)
         (progn                            ; retrieve local data from cstack :
          (pop *cstack)
          (setq direction (top *cstack))   ; previous direction
          (pop *cstack)
          (setq next (top *cstack))        ; next instantiation to try
          (pop *cstack))
         (progn                            ; else new propagation
          (setq direction *fa-direction*)
          (setq next 1)))
      (setq *fa-direction* 'left)                 ; reset global direction
      (setq opposite-mem (if (equal direction 'left) mem2 mem1))
      (setq opposite-max (fa-count-max opposite-mem))
      (when (zerop opposite-max) (fa-fail))       ; fail if opposite memory is empty
      (setq search-result
            (search-consistent-union
                   direction mem1 mem2 next))
      (setq inst-out (first search-result))
      (setq next (second search-result))
      (when (null inst-out) (fa-fail))            ; fail if there is no consistent union
      (fa-set-new-inst mem3 inst-out)             ; store consistent union in memory 3
      (when *fa-trace* (print inst-out))
      (when (<= next opposite-max)                ; if there are still untested instantia-
            (push next *cstack)                   ; tions in opposite memory, make sure that
            (push direction *cstack)              ; control will return to this instruction
            (push *NP *cstack)                    ; <- push addr for propagate (local data)
            (push *NP *cstack))))                 ; <- push addr for control




   ;;; make conclusion instructions

(fa-definstr make-predicate (pred arity) ; TOP points to next free cell !
   (when *fa-trace* (print (list 'make-pred pred arity)))
   (fa-set-cell *TOP (fa-fact-cell))
   (fa-set-cell *REM (make-*word :*tag 'fact :*value *TOP)) ; chain previous fact with this one
   (setq *REM *TOP)
   (fa-next-top-position)
   (fa-set-cell *TOP (fa-pred-cell pred arity))
   (fa-next-top-position))

(fa-definstr make-structure (func arity)
   (when *fa-trace* (print (list 'make-str func arity)))
   (fa-set-cell *TOP (fa-fun-cell func arity))
   (fa-next-top-position))

(fa-definstr make-constant (const)
   (when *fa-trace* (print (list 'make-con const)))
   (fa-set-cell *TOP (fa-con-cell const))
   (fa-next-top-position))

(fa-definstr make-constant-nil ()
   (when *fa-trace* (print (list 'make-con 'nil)))
   (fa-set-cell *TOP (fa-con-nil-cell))
   (fa-next-top-position))

(fa-definstr make-bound-variable (var mem)
   (when *fa-trace* (print (list 'make-var (fa-newest-binding mem var))))
   (fa-set-cell *TOP (make-*word :*tag 'ref :*value (fa-newest-binding mem var)))
   (fa-next-top-position))

(fa-definstr action ()
   (when *fa-trace* (print (list 'action)))
   (terpri)
   (terpri)
   (princ "; --> infered fact : ")
   (princ (get-fact *REM))
   (terpri))

(fa-definstr proceed ()
   (when *fa-trace* (print 'proceed))
   (fa-fail))



;;; INPUT to heap / OUTPUT from heap :


  ; example:

     ; literal  :                p(a, h(x, f(b)), x)

     ; list-representation :     (p a (h (vari x) (f b)) (vari x))

     ;                               6    7    8    9    10   11   12   13   14   address
     ; heap-representation :    ________________________________________________
     ;                             |FACT|PRED|CON |FUN |REF |FUN |CON |REF |      tag
     ;                          ------------------------------------------------
     ;                             | 14 |p/3 | a  |h/2 | 10 |f/1 | b  | 10 |      value
     ;                          ------------------------------------------------


;;; converts external fact representation (list) to heap representation
;;; returns position of next free cell in heap
;;; increments *TOP and *REM (*TOP always points to first free cell)

(defun put-fact (fact)
   (let ((pointer *TOP)
         (predicate (first fact))
         (arguments (rest fact)))
        (fa-set-cell pointer (fa-fact-cell))
        (unless (equal *TOP 0)   ; unless first fact, chain to previous fact
           (fa-set-cell *REM (make-*word :*tag 'fact :*value pointer))) 
        (setq *REM pointer)
        (fa-set-cell (1+ pointer) (fa-pred-cell predicate (length arguments)))
        (setq *TOP (put-arguments arguments (1+ (1+ pointer))))))


;;; converts external term representation to heap representation,
;;; returns pointer to term and increments *TOP

(defun put-term-on-TOP (term)
   (let ((return *TOP))
      (setq *TOP (put-term term *TOP))
      return))


;;; converts arguments of a predicate or function to heap representation
;;; returns position of next free cell in heap

(defun put-arguments (arguments pointer)
   (if (null arguments) pointer  ; next top position as result
       (put-arguments (rest arguments) (put-term (first arguments) pointer))))


;;; converts argument-terms to heap representation
;;; returns position of next free cell in heap

(defun put-term (term pointer)
   (if (not (listp term))
       (progn
        (fa-set-cell pointer (fa-con-cell term))             ; constant
        (1+ pointer)) ; fct-result
       (if (equal (first term) 'vari)                   ; free variable
           (progn
            (fa-set-cell pointer (fa-ref-cell pointer))
            (1+ pointer))
           (progn                                       ; function
            (fa-set-cell pointer (fa-fun-cell (first term) (length (rest term))))
            (put-arguments (rest term) (1+ pointer))))))


;;; converts heap representation of a fact to list representation
;;; returns the fact starting with pointer in heap

(defun get-fact (pointer)
   (if (or (not (equal (fa-tag pointer) 'fact))
           (not (equal (fa-tag (1+ pointer)) 'pred)))
       nil  ;fct-result
       (let ((get-arguments-value
                (get-arguments (fa-arity (1+ pointer)) (1+ (1+ pointer)))))
            (cons (fa-predicate (1+ pointer)) (second get-arguments-value)))))


;;; converts the heap representation of the arguments of a
;;; predicate or function to external list representation
;;; returns in a list 1. position of first cell following the arguments
;;;                   2. argument list

(defun get-arguments (arity pointer)
   (if (zerop arity)
       (list pointer nil)  ;fct-result
       (let* ((get-term-value (get-term pointer))
              (get-arguments-value
                 (get-arguments (1- arity) (first get-term-value))))
             (list (first get-arguments-value)   ; first cell after arguments
                   (cons (second get-term-value) ; term
                         (second get-arguments-value))))))  ; fct-result


;;; converts a heap argument-term to external list representation
;;; returns in a list 1. position of first cell following the term
;;;                   2. term

(defun get-term (pointer)
  (case (fa-tag pointer)
	('ref (list (1+ pointer)               ; <- always next cell, bound or not bound
		    (if (equal (fa-value pointer) pointer)    ; not bound -> generate
			(list 'vari (gensym "_var"))          ;              new variable
                      (second (get-term (fa-value pointer)))))) ; bound
	('con (list (1+ pointer)
		    (fa-value pointer)))
	('fun  (let ((get-arguments-value
		      (get-arguments (fa-arity pointer) (1+ pointer))))
		 (list (first get-arguments-value)
		       (cons (fa-functor pointer)
			     (second get-arguments-value)))))))


;;; converts all facts from heap representation to list representation

(defun get-all-facts (pointer)
  (if (null pointer) nil
    (cons (get-fact pointer)
	  (get-all-facts (fa-value pointer)))))




;;; TRACE MODE auxiliary functions :


; escape from execution (in trace mode)

(defun fa-escape ()
   (terpri)
   (princ "*** escape ? (y,n) : ")
   (when (equal (read) 'y)
         (terpri)
         (princ "; for help type ? ")(terpri)
         (terpri)
         (catch 'quit
           (loop
             (princ "*")
             (case (read)
                ('? (show-trace-info))
                ('h (let ((from (read))
                          (to   (read)))
                         (when (and (numberp from)
                                    (numberp to)
                                    (<= from to))
                               (trace-heap from to))))
                ('r (trace-registers))
                ('x (trace-x-registers))
                ('m (let ((mem (read)))
                         (when (and (numberp mem)
                                    (>= mem 0)
                                    (<= mem *a-b-memory-size))
                               (trace-memory mem))))
                ('st (trace-stacks))
                ('f (let ((adr (read)))
                         (when (and (numberp adr)
                                    (>= adr 0)
                                    (<= adr *heap-size))
                               (pprint (get-fact adr))
                               (terpri))))
                ('s (let ((adr (read))
                          (tag (read))
                          (value (read)))
                         (when (and (numberp adr)
                                    (>= adr 0)
                                    (<= adr *heap-size))
                               (trace-set adr tag value))))
                ('d (dump-emulator-state))
                ('stop (throw 'stop T))
                ('cn (progn
                      (terpri)
                      (princ "*** continue execution, stop tracing")
                      (setq *fa-trace* nil)
                      (terpri)(terpri)
                      (throw 'quit T)))
                ('c (progn
                      (terpri)
                      (princ "*** continue execution")
                      (terpri)(terpri)
                      (throw 'quit T))))))))


; describe trace mode functions

(defun show-trace-info ()
   (princ "   trace options :")(terpri)(terpri)
   (princ "      ?                    -   show this menu")(terpri)
   (princ "      h  <from> <to>       -   show heap contents")(terpri)
   (princ "      r                    -   show registers ")(terpri)
   (princ "      x                    -   show x-registers")(terpri)
   (princ "      m  <memory>          -   show contents of a b-memory")(terpri)
   (princ "      st                   -   show stacks")(terpri)
   (princ "      f  <adr>             -   pprint a heap fact")(terpri)
   (princ "      s  <adr><tag><value> -   set a heap cell")(terpri)
   (princ "      d                    -   dump emulator state")(terpri)
   (princ "      stop                 -   stop execution of instructions")(terpri)
   (princ "      cn                   -   continue execution but stop tracing")(terpri)
   (princ "      c                    -   continue execution in trace mode")
   (terpri)(terpri))


; define trace functions

(defun trace-registers ()
   (terpri)
   (princ "  NP  : ")(prin1 *NP)(princ "  (next)")(terpri)
   (princ "  F   : ")(prin1 *F)(terpri)
   (princ "  P   : ")(prin1 *P)(terpri)
   (princ "  TOP : ")(prin1 *TOP)(terpri)
   (princ "  REM : ")(prin1 *REM)
   (terpri)
   (terpri))

(defun trace-x-registers ()
   (terpri)
   (dotimes (i *x-register-size)
      (princ "x-register ")
      (prin1 i)
      (princ " --> ")
      (prin1 (fa-x-reg i))
      (terpri))
   (terpri))

(defun trace-heap (from to)
   (terpri)
   (do ((i from (1+ i)))
       ((equal i to) T)
      (prin1 i)
      (prin1 (list (fa-tag i)(fa-value i)))
      (terpri))
   (terpri))

(defun trace-memory (mem)
   (terpri)
   (pprint (fa-memory mem))
   (terpri))

(defun trace-stacks ()
   (terpri)
   (princ "control-stack : ")
   (pprint *cstack)
   (terpri)
   (terpri)
   (princ "ref-stack : ")
   (pprint *ref-stack)
   (terpri))

(defun trace-set (adr tag value)
   (fa-set-cell adr (make-*word :*tag tag :*value value)))

(defun dump-emulator-state ()
   (terpri)
   (princ "***  DUMP emulator data areas :")(terpri)(terpri)
   (princ "    - registers")(terpri)
   (princ "    - xregisters")(terpri)
   (princ "    - heap")(terpri)
   (princ "    - b-memories")(terpri)
   (princ "    - stacks")(terpri)(terpri)
   (trace-registers)
   (trace-x-registers)
   (trace-heap 0 *TOP)
   (terpri)            ; b-memories
   (dotimes (i *a-b-memory-size)
            (prin1 i)
            (prin1 (fa-memory i))
            (terpri))
   (terpri)
   (trace-stacks)
   (terpri))


;;; the USER INTERFACE

(defun fa-forward (fact-base &key (trace nil) (dump nil) (updates nil) (prove nil))
   (catch 'stop
      (unless (null fact-base)
         (if dump
             (setq *fa-dump* T)
             (setq *fa-dump* nil))
         (if (or trace dump)
             (progn (setq *fa-trace* T)
                    (when prove (setq *emu-debug* T)))
             (progn (setq *fa-trace* nil)
                    (when prove (setq *emu-debug* nil))))
         ; ..
         ; transformiere facts in literale
         ; ..
         (setq *NP    1)                                   ; set instruction pointer to first instr
         (setq *cstack nil)                                ; init control stack
         (setq *ref-stack nil)                             ; init ref stack
         (if updates
            (progn
               (setq *F *TOP)                              ; set F to first update
               (mapcar 'put-fact updates)                  ; put updates on the heap 
               (fa-interpret))                               ; call interpreter
            (progn
               (setq *heap         (fa-make-heap))           ; init heap
               (setq *a-b-memories (fa-make-a-b-memories))   ; init a-b-memories
               (setq *x-registers  (fa-make-x-registers))    ; init X registers
               (setq *TOP 0)                               ; init top of heap
               (mapcar 'put-fact fact-base)                ; put initial facts on the heap
               (setq *F   0)                               ; set F to first initial fact
               (setq *I *REM)                              ; set I to last initial fact
               (when *fa-trace*
                  (terpri)
                  (terpri)
                  (princ "; ************************ stepper protocol ************************** ")
                  (terpri)
                  (terpri)
                  (princ "; input fact : ")
                  (princ (get-fact *F))
                  (terpri))
               (fa-interpret)))                             ; call interpreter
          *fa-infered-facts*)))                             ; function result

(terpri)
(princ "FAM  Ver 1.1 , 12. Juni 92")
(terpri)
