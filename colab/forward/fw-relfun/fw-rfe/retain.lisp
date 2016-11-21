;;; the access primitives of the retain-stack


(definstr actual-node(Ai)(NAT)        ; setzt Ai auf Fact(AN)
  :standard
  (gwam.actual-node
  (if (or (empty_retain_stack?)       ; fail falls kein AN mehr vorhanden.
          (nil-cell? (reg AN)))
      (fail)
       (set-argument-reg Ai (car_Retain_fact (reg AN))))))


 (definstr get-open-node(Ai)(NAT)     ; setzt Ai auf Fact(ON)(ON pts at open node)
   :standard
   (gwam.get-open-node
  (if (or (empty_retain_stack?)       ; fail falls kein ON mehr vorhanden.
          (nil-cell? (reg ON)))
      (fail)
       (set-argument-reg Ai (car_Retain_fact (reg ON))))))

#|
(definstr get-open-node(Ai)(NAT)      ; setzt Ai auf Fact(ON)(ON pts at prev open node)
  :standard
  (gwam.get-open-node
  (if (or (empty_retain_stack?)       ; fail falls kein ON mehr vorhanden.
          (nil-cell? (reg ON))
	  (nil-cell? (cdr_retain_fact (reg ON))))
      (fail)
       (set-argument-reg Ai (car_Retain_fact (cdr_retain_fact (reg ON)))))))
|#


(definstr next-open-node()()          ; setzt ON auf next Fakt
  :standard
  (gwam.next-open-node
 (if (or (empty_retain_stack?)        ; fail falls keiner mehr da, dh Ret leer
         (nil-cell? (reg ON))
         (nil-cell? (cdr_retain_fact (reg ON))))
            (fail)
     (set-reg ON (cdr_retain_fact (reg ON))))))


(definstr empty-retain-stack ()()
  :standard
  (gwam.empty-retain-stack
  (if (empty_retain_stack?)
      (fail)
    nil)))

(definstr some-more-open-nodes()()
  :standard
  (gwam.some-more-open-nodes
  (if (or (empty_retain_stack?)
          (nil-cell? (reg ON))
          (nil-cell? (cdr_retain_fact (reg ON))))
      (fail)
      nil)))

        
#|
(definstr bind (A1 A2)
  (bind (argument-reg A1) (argument-reg A2)))
|#

;(definstr open-nodes-left()
;   (if (or (empty_retain_stack?)
;	   (nil-cell? (reg ON)))
;       (fail)))



;;;; the implementation of the retain stack

                                     
(define-register Rtop)                ;;; Hilfregister zum Aufbau des RetainStacks 
                                      ;;; Rtop>=AN .(Au§nahme: Stack ist leer)
(define-register AN)                  ;;; Actual Node. zeigt immer auf den zuletzt hergeleiteten Fakt


(define-register ON)                  ;;; zeigt auf 'open-node' wird von
                                      ;;; bf-enum benutzt

(defparameter retain-size 10000)

(defparameter begin-of-retain-stack (gmem.alloc/clr retain-size :unused-retain-cell))

(defparameter start-of-retain-stack (+ begin-of-retain-stack 200))

(defvar *Precomp_Stack* (list 0))       ;;; Stack = (<sp> ....)
(defvar *Push_Register*)         ;;; werden fŸr push_main,push_param benštigt



;; the init_routines 



(defun init_push_retain()
  (setf *Precomp_Stack* (list 0))      ;;; *Precomp_Stack* : ( <sp> ....)
  (setf *Push_Register* (make-list 24)))





;; precomp ( word what)
;; liefert Referenz von what, *Precomp_Stack*(Referenz)=what     
;; baut Referenzen auf *Precomp_Stack* auf   (nur der ist interresant)
;; what wird nur als Adresse interpretiert!!
;;   
;; kennt nur :Ref,:Struct,:Const
;;
;; benutzt Globale Variable : *Precomp_Stack* 

;; 



(defun precomp (what)
  (let ((term (deref what)))
    (case 
     (word-tag term)
     (:const (nconc *Precomp_Stack* (list term))
	    (setf (car *Precomp_Stack*) (1+ (car *Precomp_Stack*))))               ;;; Position wo Term steht
     (:ref   (if (bekannt? term)              ;;; nur neue var auf stack
		(nr_of_reg term)
	      (progn (nconc *Precomp_Stack* (list term))
		     (setf (car *Precomp_Stack*) (1+ (car *Precomp_Stack*))))))
     (:struct (nconc *Precomp_Stack*
		    (list
		     (cons (mem term)
			   (do  ((i 1 (1+ i))
				 (result nil (cons (precomp (mem (ref-plus term i)))   ;; einzelnes Arg
						   result)))                           ;; alle args von fkt aufsammeln
				((> i (second (word-value (mem term))))                ;; Stelligkeit
				 (reverse result))))))                                ;; wegen Recursion von do
	     (setf (car *Precomp_Stack*) (1+ (car *Precomp_Stack*))))
     (:list   (nconc *Precomp_Stack*
		    (list
		     (cons term
			  (cons (precomp (mem (ref-plus term 0)))
				(cons (precomp (mem (ref-plus term 1))) nil)))))
	     (setf (car *Precomp_Stack*) (1+ (car *Precomp_Stack*))))
     (otherwise (error "unknown request to precomp ~a" what)))))





  
;;;; ================================= Main_Instruction ==========================
;;;; push-fact-retain(fact)
;;;; =============================================================================
;;;;  
;;;; Benutzt Golbale Variablen: *Precomp_Stack*, Rtop, AN
;;;; ruft precomp, push_main auf
;;;;  
;;;; Seiteneffect:!!! - auf RetainStack wird structur kopiert
;;;;                  - AN      = auf (Str:Fact)   --> car
;;;;                  - (AN +1) = auf (const:nil)  --> cdr
;;;;                  - Rtop = letzte Speicherzelle von Fact
;;;;                  
;;;; Vorbedingungen : - RetainStack ist leer
;;;;      --> Rtop = start-of-retain-stack -1
;;;;          AN = start-of-retain-stack
;;;;         
;;;;                  - oder es befinden sich bereits elemente auf Retain Stack
;;;;      --> Rtop = letzte Zelle von Retain Stack
;;;;          AN = last Fact
;;;;          
;;;;                




(definstr push-fact-retain (Ai)(NAT) :standard (gwam.push-fact-retain 
  (init_push_retain)    ;;;; loesche *Precomp_Stack*,*Push_Register*
  (precomp (argument-reg Ai))
  (let ((Akt-Fact 0)
        (Dummy-Reg -1)
	(stack_bottom_flag (bottom_retain_stack?)))
;;;    (push_fktor_cell '(cons 2) Dummy-Reg)   ;;; neue ConsZelle , Dummy-Reg= (str: ) 
    (push_variable Akt-Fact)               ;;; car-zelle, selbst referenz 
    (push_constant (constant-nil))    ;;; cdr ist nil
    (if (not stack_bottom_flag)
      (setmem (ref-plus (reg AN) 1)                 ;;; cdr-zelle der Liste 
              (make-word :tag :list
			 :value (word-value (push_reg_value Akt-Fact)))))          ;;; Verweis auf neuen Fact 
    (set-reg AN (push_reg_value Akt-Fact))        ;;; AN auf neues Element setzen 
    (do ((i 1 (1+ i)))      ;;; for i=1 to sp-1 do push_main(i)
        ((= i  (car *Precomp_Stack*))  nil)
      (push_main i))
    (setmem (push_reg_value Akt-Fact)
	    (push_main (car *Precomp_Stack*)))))
)

 

;;; push_main ( index on *Precomp_Stack*)
;;; Ergebnis: adresse von dem Stackelement auf dem Retain
;;; bekommt nacheinander die einzelnen Stackelemente 
;;; benutzt Rtop,*Precomp_Stack*
;;; ruft fŸr die parameterlisten push_param 


(defun push_main (index)     
  (let ((what (nth index *Precomp_Stack*))) 
    (cond ((not(word-p what))
           (cond ((eq (word-tag (car what)) :fun)
		  (let ((result (push_struct  (car what)  index)))
		    (map nil #'push_param (cdr what))
		    result))
		 ((eq (word-tag (car what)) :list)
		  (let ((result (push_list index)))
		    (map nil #'push_param (cdr what))
		    result))
		 (t (error "Unknow type in push_main ~a" what))))
          ((member (word-tag what) '(:ref :const)) 'ok)
          (t  (error "Unknown type in push_main ~a" what)))))
       
    
;;; push_paramn (index on  *Precomp_Stack*)
;;; nur fŸr Parameterlisten_indices
;;; erzeugt neue Vars,Constanten auf Retain Stack
;;; Strukturen mŸssen bereits bekannt sein!!
;;; Ergebnis: undef.

(defun push_param (index)   
  (let ((what (nth index *Precomp_Stack*)))
    (if (not(word-p what))
      (if (member (word-tag (car what)) '(:fun :list))
        (if (push_reg_bound? index)
          (push_struct_cell index)
          (error "Push_Register must be bound:" index))
        (error "Unknown list type in push_param ~a" what))
      (case (word-tag what)
        (:ref   (if (push_reg_bound? index)
                 (push_value index)
                 (push_variable index)))
        (:const (push_constant what))
        ( otherwise (error "Unknown type in push_param: ~a" what))))))



      
;; auxiliarry functions 
;; register handling


(defun set_push_reg (nr value)
  (setf nr (+ 2 nr))
  (let ((len (length *Push_Register*)))
    (if (>  nr len)
      (progn
        (nconc *Push_Register* (make-list (- nr len) :initial-element nil))
        (setf (nth (1- nr) *Push_Register*) value))
      (setf (nth (1- nr) *Push_Register*) value))))



(defun push_reg_value (nr)
  (nth (1+ nr) *Push_Register* ))

(defun push_reg_bound? (nr)
  (not (null  (push_reg_value nr))))



;; Retain Stack

(defun push_retain ( value)      ;; liefert value zurŸck!!
  (set-reg  Rtop (ref-plus  (reg Rtop) 1))  
  (setmem (reg Rtop) value))
 

(defun cdr_Retain_Fact (ptr2cons)
   ;;; liefert ptr2cons von cdr Fact
   (mem (ref-plus ptr2cons 1)))

(defun car_Retain_fact (ptr2cons)
  (mem (ref-plus ptr2cons 0)))


;;; Precompile Stack
 
(defun bekannt? (ref_term)              ;;; liefert <> nil falls ref_term in *Precomp_Stack* eingetragen
  (member ref_term (cdr *Precomp_Stack*) :test #'(lambda(xx yy) (and (word-p xx)
                                                              (word-p yy)
                                                              (word-equal xx yy)))))

(defun nr_of_reg (var)
  (1+ (position var (cdr *Precomp_Stack*) :test #'(lambda(xx yy) (and (word-p xx)
                                                               (word-p yy)
                                                               (word-equal xx yy))))))
 
       

;;; wam-instruktions, liefern den jeweiligen wert, der auf den Retain_Stack gebracht wurde
;;;                   zurŸck, und setzen das Register auf diesen Wert


(defun push_constant ( const)
  (push_retain  const))

  

(defun push_variable ( reg_nr)        ;; unbound cell
  (set_push_reg reg_nr 
        (push_retain (make-word :tag :ref 
                                :value (1+ (address (reg Rtop)))))))
   
(defun push_value ( reg_nr)          ;; nur den Registerwert
  (push_retain (push_reg_value reg_nr)))

(defun push_struct (fktor_n_cell reg_nr)
  (set_push_reg reg_nr (make-word :tag :struct :value (+ 1 (address (reg Rtop)))))
  (push_retain fktor_n_cell)
  (push_reg_value reg_nr))

(defun push_list (reg_nr)
  (set_push_reg reg_nr (make-word :tag :list :value (+ 1 (address (reg Rtop)))))
  
  
 )

(defun push_fktor_cell (fktor reg_nr)
  (push_retain (functor fktor))
  (set_push_reg reg_nr 
                (make-word :tag :struct :value (address (reg Rtop)))))

(defun push_struct_cell ( reg_nr)
  (push_retain (push_reg_value reg_nr)))


;; the environment

 (define-register RENV)                 ;;; Register, dass auf den Top des Environment-Stacks
                                       ;;; fuer den RS zeigt.



(define-offset Rtop 0)                 ;;; Offsetaddr. der Register zu RENV
(define-offset  AN -1)
(define-offset  ON -2)
(define-offset ON_Start -3)
(defconstant regs-in-rs-env 4)

(defun save-RS-Regs ()
   "Speichert die Retain-Stack Register ab"
   (set-reg RENV (ref-plus (reg RENV) (- regs-in-rs-env)))
   (setmem (ref-plus (reg RENV) (offset Rtop)) (reg Rtop))
   (setmem (ref-plus (reg RENV) (offset AN)) (reg AN))
   (setmem (ref-plus (reg RENV) (offset ON)) (reg ON))
   (reg RENV)
   )

(defun restore-RS-Regs ()
  "Restauriert die Retain-Stack Register"
   (set-reg Rtop (RSenv Rtop))
   (set-reg AN   (RSenv AN)) 
   (set-reg ON   (RSenv ON)) 
   (set-reg RENV (ref-plus (reg RENV)  regs-in-rs-env))
   (reg RENV)
   )



(defun init-retain-system ()
 ; (set-reg AN (make-word :tag :ref :value start-of-retain-stack))
  (set-reg Rtop (make-word :tag :ref :value (1- start-of-retain-stack)))
 ; (setmem (reg Rtop) (make-word :tag :struct :value start-of-retain-stack))
 ; (set-reg ON (ref-plus (reg Rtop) -2))
 ; (setmem (reg AN) (constant-nil))
  (set-reg RENV (make-word :tag :ref :value (1- start-of-retain-stack))))

(init-retain-system)


(defmacro RSenv(RSreg)
  `(mem (ref-plus (reg RENV) (offset ,RSreg))))



(definstr enter-retain-stack ()():standard (gwam.enter-retain-stack 
  (save-RS-Regs)
  (set-reg Rtop (ref-plus (reg Rtop) 1))
  (setmem (reg Rtop)
	  (make-word :tag :list :value (1+ (word-value (reg Rtop)))))
  (set-reg AN   (ref-plus (reg Rtop) 1))
  (set-reg ON   (ref-plus (reg Rtop) -1)) ;; the cdr of ON is well defined!!
  (setmem (ref-plus (reg RENV) (offset ON_Start))
	  (reg ON))  ;; to save the beginning state of ON 
                                     
  (setmem (reg AN) (constant-nil))

))

(definstr leave-retain-stack ()() :standard (gwam.leave-retain-stack
  (restore-RS-Regs)))

;;;;  just empty in THIS environment, but there are yet facts on it.
(defun empty_retain_stack? ()

  (or (bottom_retain_stack?)
      (word-equal (reg Rtop)
		  (RSenv Rtop))))


;;;  RS is really empty. There are no facts on it.
(defun bottom_retain_stack? ()
  (ref-lessp (reg Rtop) (reg AN)))


#|
(definstr reset-ON()() :standard (gwam.reset-ON
  (set-reg ON (cdr_retain_fact (RSenv ON_Start)))))
|#


(definstr begin-of-retain-stack(Ai)(NAT) :standard (gwam.begin-of-retain-stack
  (set-argument-reg Ai (cdr_retain_fact (RSenv ON_Start)))))



(definstr subsumes (Ai Aj)(NAT NAT) :standard (gwam.subs
  (if (not (subsume (argument-reg Ai)(argument-reg Aj)))
      (fail))))

(definstr collect-retain-facts/2 () () :standard
  (gwam.collect-retain-facts/2
 ;;; mit A1 = Restriktion/Result
 ;;;     A2 = Next_One
 ;;; mit ON = (list:first_fact_on_RS)
   (if  (and (not  (empty_retain_stack?))
	     (varp (mem (argument-reg 1)))) ;;; bei einer ungebundenen Variable 
       (bind (argument-reg 1) (reg ON))       ;;; einfach RetainStack ausgeben 
     (let ((result nil))
       (do ()
	   ((or (empty_retain_stack?)
		(nil-cell? (reg ON)))
	    nil
	    )
	   (when (subsume (argument-reg 1)
			  (car_retain_fact (reg ON)))
		 (setf result t)
		 (set-argument-reg 4 (car_retain_fact (reg ON)))  ;; get Akt Fakt
		 (gwam.put_list 3)            ;;; neue List erzeugen -> A3
		 (gwam.unify_value_temp 4)    ;;; ON Fakt in Liste eintragen
		 (gwam.unify_value_temp 2)    ;;; Rest der Liste 
		 (gwam.put_value_temp 3 2))   ;;; A2 := A3
	   
	   (when (not (nil-cell? (reg ON)))
		 (set-reg ON (cdr_retain_fact (reg ON)))))
       (if (not result) 
	     (gwam.put_nil 1)                ;;; kein passendes Element gefunden
	 (gwam.put_value_temp 2 1)           ;;; Ergebnis ausgeben
	 ) ; if
       ); let
    ) ; if
  )) ; definstr



;; the subsumption done by TO
;(definstr not-r-subsumed(Ai)
;  (not-subsumed-TO (argument-reg Ai)))

;;; ======================= Main_Instruction ==========================
;;; not-r-subsumed(Ai)
;;; ===================================================================
;;; 
;;; Tests if the structure referenced by Ai is subsumed by any
;;; structure on the retain stack.
;;; If it is subsumed ---> generates a fail
;;; else do nothing.
;;;
;;;


(definstr not-r-subsumed (Ai)(NAT) :standard (gwam.not-r-subsumed
  (let ((d (deref (argument-reg Ai)))
        (subsumed nil))
    (if (empty_retain_stack?)
      t
      (progn 
        (do ((ret_fact (mem (ref-plus (RSenv Rtop) 1)) ;(make-word :tag :struct :value start-of-retain-stack)
                       (cdr_Retain_Fact ret_fact))
             (done nil))
            (done nil)
	    
	    (setf subsumed (subsume (car_retain_fact ret_fact) d))
	    (setf done (or subsumed (nil-cell? (cdr_retain_fact ret_fact )))))
        (if subsumed
	    (fail)))))))

 
;; 
(defun nil-cell? ( xxx)
  (word-equal xxx (constant-nil)))


           



;;; Subsume (word a1, word a2)   liefert TRUE 
;;; <=> (Struct (a1)  logisch>= Struct(a2))
;;; normaler Aufruf durch (not_r_subsumed) 
;;;
;;; Erkennt Fakts mit Ref,Struct,Const,List
;;; Beschreibung: Strukturvergleich,
;;;               falls a1 ungeb. Var besitzt werden diese an a2 gebunden! 
;;;               falls a1 und a2 ungeb. Var sind, wird a2 Instantiiert
;;;                   und a1 an a2 gebunden
;;;              alle Bindungen/Instantiierungen werden local wieder r ruecgkaengig gemacht.
;;;      -> subsume ist Nebenwirkungsfrei  



(defun subsume(ref1 ref2)
  (let ((result nil))
    (setf *subsume_trail_list* (list))
    (setf instantiate_counter 0)
    (setf result (subsume-main ref1 ref2))
    (subsume_unwind)
    result))



(defun subsume-main(ref1 ref2)
  (cond  ((word-equal ref1 ref2) t)
         ((and (varp ref1)(varp ref2))
          (instantiate ref2 (new_inst_const))
          (subsume-bind ref1 ref2)
          t)
         ((varp ref1)
          (subsume-bind ref1 ref2) t)
         ((varp ref2) nil)
         ((not (eq (word-tag ref1)(word-tag ref2)))
          nil)
         (t (case (word-tag ref1)
              ((:const)
               (equal (word-value ref1)
                      (word-value ref2)))
              ((:struct)
               (subsume-structures (mem ref1) (mem ref2) ref1 ref2))
              ((:list)
               (subsume-main (deref (mem ref1))
                             (deref (mem ref2)))
               (subsume-main (mem (ref-plus ref1 1))
                             (mem (ref-plus ref2 1))))))))

(defun subsume-structures (fun1 fun2 ref1 ref2)
  (if (and (eq (word-tag fun1) :fun)
           (word-equal fun1 fun2))
    (let ((arity (second (word-value fun1))))
      (do ((i arity (1- i))                  
           (result t))
          ((or (zerop i) (not result))       ;;; alle Fkt_Arg. subsumiert oder einer nicht 
           result)
           (setf result (subsume-main (deref (ref-plus ref1 i))
                                      (deref (ref-plus ref2 i))))))
    nil))


;;; instantiierung...

(defparameter instantiate_counter 0)

(defun new_inst_const ()
  (setf instantiate_counter (1+ instantiate_counter))
  (constant (intern (concatenate 'string "instantiate_" (princ-to-string instantiate_counter)))))

(defun akt_inst_const ()
  (constant (intern (concatenate 'string "instantiate_" (princ-to-string instantiate_counter)))))


(defun instantiate (ref what)
  (setmem ref what)
  (setf *subsume_trail_list* (cons ref *subsume_trail_list*)))

;;; binding...

(defun subsume-bind (ref1 ref2)
  (setmem ref1 ref2)
  (setf *subsume_trail_list* (cons ref1 *subsume_trail_list*)))


;;; trailing...

(defparameter *subsume_trail_list* nil)

(defun subsume_unwind()
  (dolist  (akt_ref  *subsume_trail_list*) 
        (setmem akt_ref akt_ref)))





