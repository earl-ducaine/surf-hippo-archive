
;;; Compiler for FORWARD Abstract Emulator
;;; Christian Falter , Common Lisp
;;; Version 1.0 (mit Matching)

;;; Modified by Martin Harm
;;; Backward rules, COLAB integration
;;; Version 1.1 

;;;;
;;;; Transformation of the colab representation of rules into the FAM representation
;;;;
;;;; Colab: 
;;;;  ( {up|rl} conc_1 ... conc_N <- premisse_1 ... premisse_N)
;;;;  
;;;; Fam:
;;;;  ( ( premisse_1 premise_2 ... premise_N) (conc_1 conc_2 ... conc_N)  back_prem  )
;;;;
;;;; 1.) --> with back_prem is one! goal, generated out of all! the backward  premises
;;;;         and allways of the kind (*fam-back-pred*<i>  <var_1> .. <var_n>)
;;;;  
;;;; 2.) --> the rule is flattened (in the meaning of flatten all the calls to footed clauses)
;;;;         no static structures are flattened
;;;; 
;;;; 3.) --> all static structures are de-instanted ( f `(g ...)) --> (f (g ...)) 

(defvar *standard-backward-preds* `(< > = is + - * / <= >= 1+ rfprint)
  "All the preds and functions, which are allways prooved in the GWAM")

(defvar *fam-backward-index* 0 "Counter to get different back_prem")

(defvar *fam-backward-rules* nil
  "Database, contains all the backward-preds which are used in the FAM")

(defvar *fam-back-pred* 'fam-back- "Default name of the backward premissess")

(defun colab2fam_rule (rule backward_preds)
  "Transforms the rule (--> *standard-backward-preds*).
Genrates backward predicate (--> *fam-backward-rules*, *fam-backward-index*)."

  (let* (
	 (prems (de-instant (flatten-simple (premisses rule))))
	 (concs (conclusions rule))
	 (fprems nil)  ;; forward
	 (fvars nil)   ;; vars in all fprems
	 (bprems nil)  ;; backward
	 (bvars nil)   ;; vars in all bprems
	 (cvars nil)   ;; vars in all conclusions
	 (b_prem_name nil)  ;; name of backward predicate
	 (b_prem nil)       ;; whole backward call
	 (b_rule nil)       ;; the backward rule (a hn-clause !!)
	)
    (break)
    (dolist (pr prems)    ;;; divide prems in forward / backward prems
	    (if (or (member (first pr) backward_preds)
		    (member (first pr) *standard-backward-preds*))
		(setf bprems (cons pr bprems))
	      (setf fprems (cons pr fprems))))
    (when bprems
      (setf bvars (collect-all-vars bprems))
      (setf fvars (collect-all-vars fprems))
      (setf cvars (collect-all-vars concs))
      (setf bvars (intersection bvars (union fvars cvars :test #'equal) :test #'equal))
      (setf b_prem_name
	    (make-one-symbol *fam-back-pred*
			     (setf *fam-backward-index* (1+ *fam-backward-index*))))
      (setf b_prem (cons b_prem_name bvars))
      (setf b_rule (append (list 'hn b_prem) bprems))
      (setf *fam-backward-rules* (cons b_rule *fam-backward-rules*))
      )
    (list fprems concs b_prem)))
    

(defun collect-all-vars (prems)
  (cond ((null prems) nil)
	((atom prems) nil)
	((and (listp prems)
	      (= (length prems) 2)
	      (eq (first prems) 'vari))
	 (list prems))
	( t (union (collect-all-vars (car prems))
		   (collect-all-vars (cdr prems))
		   :test #'equal))))
		    

(defun flatten-simple (prems)
  (let ((vars (collect-all-vars prems))
	)
    (and-flattener nil
		   prems
		   vars
		   (nextnew '(0) vars))))

(defun de-instant (prems)
  (mapcar #'de-instant-prem prems))

(defun de-instant-prem (prem)
  (cond ((or (null prem)
	     (atom prem)
	     )
	 prem)
	((and (listp prem)
	      (= (length prem) 2)
	      (eq (first prem) 'inst))
	 (second prem))
	(t (cons (de-instant-prem (car prem))
		 (de-instant-prem (cdr prem))))))

;;;;
;;;; Transformation of the colab representation of facts into the FAM representation
;;;;
;;;; Colab: 
;;;;  ( {hn|fact} <fact> )
;;;;  
;;;; Fam:
;;;;  ( <fact> )
;;;; 
;;;;  
(defun colab2fam_fact (fact)
  (cadr fact))



;;; takes rules of the form:


;                  premises                conclusions

;    (    (   ((p1) (p2) ... (pn))    ((c1) (c2) ... (cl))  back_prem  )
;         (   ((p1) (p2) ... (pm))    ((c1) (c2) ... (ck))  back_prem  )
;                                   .
;                                   .
;         (   ((p1) (p2) ... (pr))    ((c1) (c2) ... (cz))  back_prem  )     )



(defvar *lc* 0)                          ; next available label 
(defvar *mc* 0)                          ; next available memory 
(defvar *prem-predicates* nil)                ; all premise predicates
(defvar *pred-adr*   nil)                ; property-list: a predicate's code addresses
(defvar *pred-arity* nil)                ; property-list: a predicate's arity
(defvar *store-memories* nil)            ; relations var-memories for store-binding (per rule)
(defvar *back-store-memories* nil)       ; var-memories for store-binding of backward rules (per rule)
(defvar *rules* nil)                     ; the rules to compile
(defvar *code*  nil)                     ; resulting code


; auxiliary functions


(defun pprint-label (label &optional stream)  ;; pprints the compiled code
   (let ((rest (rest (rest (rest label)))))
      (princ "(DEFLABEL " stream)
      (princ (second label) stream)
      (princ " " stream)
      (princ (third label) stream)
      (if (not rest) 
        (princ ")" stream)
        (progn
         (dolist (i rest)
           (terpri stream)
           (princ "             " stream)
           (princ i stream))
         (princ ")" stream)))
      (terpri stream)))

(defun err (string item)                    ;; outputs an error and stops compilation
   (terpri)
   (princ "*** Compilation error : ")
   (princ string)
   (pprint item)
   (terpri)
   (terpri)
   (throw 'error nil))

(defun delete-last (list)             ;; destructively deletes last element of a list
   (let ((rest list)
         (n (length list)))
        (dotimes (i (1- (1- n)))
           (setq rest (cdr rest)))
        (rplacd rest nil)
        list))

(defmacro cons-last (elem list)       ;; destructively conses at the end of a list
   `(setq ,list (append ,list (list ,elem))))

(defmacro remove-properties (symbol)           ;; removes symbol properties
   `(mapcar #'(lambda (key)
                 (remprop (quote ,symbol) key))
            (remove 'nil 
               (mapcar #'(lambda (elem)
                            (if (listp elem) nil elem))
                       (symbol-plist (quote ,symbol))))))

(defun store-pred-arity (pred arity)
   (setf (get '*pred-arity* pred) arity))

(defun pred-arity (pred)
   (get '*pred-arity* pred))

(defun add-to-pred-adr (pred adr)
   (let ((adrs (get '*pred-adr* pred)))
   (setf (get '*pred-adr* pred)
         (if adrs
             (cons-last adr adrs)
             (list adr)))))

(defun switching-adr (pred)
   (first (get '*pred-adr* pred)))

(defun fork-addresses (pred)
   (rest (get '*pred-adr* pred)))

(defun delete-predicate-addresses (pred)
   (setf (get '*pred-adr* pred) nil))

(defun next-label ()
   (setq *lc* (1+ *lc*))
   *lc*)

(defun next-memory ()
   (setq *mc* (1+ *mc*))
   *mc*)

(defun flatten (structure)                     ;; flattens the structure of a predicate/function
   (let* ((arguments (rest structure))
          (flattened (list                             ; ex: (f (vari x) a (h (vari y) b))  -->
                        (list (first structure)      
                              (length arguments)))))   ;     ((F 3) (VARI X) A (H 2) (VARI Y) B)
      (dolist (i arguments)
         (if (listp i)
            (if (equal (first i) 'vari)
               (cons-last i flattened)
               (setq flattened (append flattened (flatten i))))
            (cons-last i flattened)))
      flattened))


; compiler functions


(defun generate-indexing-instructions ()

   (dolist (i *prem-predicates*)
      (cons-last
         (list 'deflabel (next-label)
            (list 'switch-on-predicate i (pred-arity i) (switching-adr i)))
         *code*))
   (cons-last (list 'deflabel (next-label) '(proceed)) *code*))


(defun compile-premise (premise label memory store-memories &optional (merge-address nil))

   (let ((flattened (rest (flatten premise)))
         (code  (list 'deflabel label)) 
         (variables nil)                ; list of variables for store-binding
         (var-xreg  nil)                ; varname-xreg relations 
         (xreg 0)               
         (predicate (first premise))) 

        (let ((addresses (fork-addresses predicate)))       ; generate all forkpaths for first
           (unless (null addresses)                         ; premise of a predicate
              (dolist (i addresses)
                 (cons-last (list 'fork-path i) code))
              (delete-predicate-addresses predicate)))

        (dolist (i flattened)
           (if (not (listp i))
              (cons-last (list 'get-constant i) code)             ; get-constant
              (if (equal (first i) 'vari)                                    
                 (if (member (second i) variables)                ; is this the first occurrence of
                    (cons-last                                    ; the variable in this premise ?
                       (list 'get-value
                          (second (assoc (second i) var-xreg))) code)            ; get-value
                    (progn
                     (setq var-xreg (cons (list (second i) (1+ xreg)) var-xreg))
                     (setq xreg (1+ xreg))
                     (setq variables (cons (second i) variables))                ; get-variable
                     (cons-last (list 'get-variable xreg) code)))
                 (cons-last (list 'get-structure (first i) (second i)) code))))  ; get-structure

        (cons-last (list 'new-instantiation memory) code)               ; new-instantiation

        (dolist (j (reverse variables))
           (cons-last
              (let* ((all-mems (second (assoc j store-memories)))
                     (other-mems (remove memory all-mems)))
                 (if other-mems                                         ; store-bindings depending
                    (list 'store-binding                                ; on whether the variable
                          (second (assoc j var-xreg))                   ; occurs only in this premise
                             j                                          ; or in other premises of this
                           memory                                       ; rule too.
                           other-mems) 
                    (list 'store-binding
                          (second (assoc j var-xreg))
                             j 
                           memory)))
              code)) 

        (cons-last (list 'check-duplicates memory) code)                ; check-duplicates

        (when merge-address                                             ; merge-path
           (cons-last (list 'merge-path merge-address) code))

        (cons-last code *code*)))                                 ; output new label to global code

(defun compile-conclusions (conclusions label result-memory)

   (let ((code (list 'deflabel label)))
      (dolist (j conclusions)                                           ; make-instructions
         (let* ((flattened (flatten j))
                (predicate (first (first flattened)))
                (arity (second (first flattened))))
            (cons-last (list 'make-predicate predicate arity) code)
            (dolist (i (rest flattened)) ; predicate auslassen
               (if (not (listp i))
                  (cons-last (list 'make-constant i) code)
                  (if (equal (first i) 'vari)
                     (cons-last (list 'make-bound-variable (second i) result-memory) code)
                     (cons-last (list 'make-structure (first i) (second i)) code)))))
         (cons-last (list 'action ) code))                              ; action
      (cons-last (list 'proceed) code)                                  ; proceed
      (cons-last code *code*)))


(defun compile-rule (rule store-memories back-store)
  
  (let* ((premises (first rule))
	 (first-premises (reverse (rest (reverse premises))))
	 (last-premise (car (last premises)))
	 (conclusions (second rule))
	 (nr-of-premises (length premises))
	 (memory-base *mc*)
	 
	 (back-memories 0)
	 (back-prem (third rule)))
    
    (dolist (i first-premises)                           ; compile all premises but last
	    (let ((label (next-label))                        ; with merges
		  (memory (next-memory)))
	      (compile-premise i                             ; premise to compile
			       label                         ; label
			       memory                        ; memory in which to store
			       store-memories                ; other memories that contain this variable
			       (+ label nr-of-premises))))    ; address of corresponding propagate
    
    (compile-premise last-premise                                 ; compile last premise without
		     (next-label)                                 ; merge
		     (next-memory)
		     store-memories)
    
    (dotimes (i (1- nr-of-premises))         
	     (cons-last (list 'deflabel (next-label)                    ; generate propagates
			      (list 'propagate
				    (+ memory-base nr-of-premises i)            ; left memory
				    (+ memory-base (1+ i))                      ; right memory
				    (next-memory)))                             ; result memory
			*code*))
    (when back-prem
	  (cons-last `(deflabel ,(next-label)
			(prove ,*mc* ,(next-memory) ,back-prem)
			)
		     *code*)
	  (setf back-memories 1))
					; compile all conclusions and
    (compile-conclusions conclusions                              ; actions into one label
			 (next-label)                 
			 (+ memory-base back-memories (1- (* 2 nr-of-premises))))))    ; result memory


(defun pass-1 (rules)     ;; checks format of rules
   (unless (and (listp rules) (>= (length rules) 1))
      (err "the format of these rules is not correct: " rules))
   (dolist (r rules)
      (unless (and (listp r) (= (length r) 3))
         (err "this is no correct rule format: " r))
      (dolist (p (cdr (reverse r)))  ;; not the backward prem
         (unless (and (listp p) (>= (length p) 1))
            (err "the format of these premises/conclusions is not correct: " p))
         (dolist (i  p) 
            (unless (listp i) 
               (err "this is no correct premise/conclusion format: " i)))))
   T)

(defun pass-2 (rules)    ;; gather all predicates in *prem-predicates*

   (setq *prem-predicates* 
      (remove-duplicates
         (mapcar 'first
            (apply 'append
               (mapcar 'first rules))))))


(defun pass-3 (rules)    ;; for each predicate compute labels for switching and forking

   (let ((counter  (1+ (length *prem-predicates*)))                  ; nr. of labels for 
         (premises (mapcar 'first rules))                      ; switching + proceed
	 (backs    (mapcar 'third rules)))   ;; back preds 
      (dolist (i premises)
         (dolist (j i)
            (add-to-pred-adr (first j) (1+ counter))            ; store a predicate address
            (unless (pred-arity (first j))
               (store-pred-arity (first j) (length (rest j))))  ; store predicate arity
            (setq counter (1+ counter)))
         (if (equal (length i) 1)                      ;; special case only one premise :
            (setq counter (1+ counter))                 ; inc counter only for make-instr label
            (setq counter (+ counter (length i))))	; inc counter for propagate and make labels
	 (when (car backs)
	       (setf counter (1+ counter)))   ; inc to get a label for the back-prems
	 (setf backs (cdr backs))
	 )))
	 
	 
(defun pass-4 (rules)    ;; for each variable in a rule compute all a-memories
  (let ((counter 1))    ;; where its values will be stored
    (dolist (i rules)
	    (let* ((premises (first i))
		   (back-rule (third i))
		   (displacement (1- (length premises)))
		   (a-list '() )
		   
		   (back-rule (third i))
		   (ab-list nil))
	      (dolist (j premises)
		      (mapcar #'(lambda (var)                             ; for each extracted variable store
				  (let* ((exist (assoc var a-list))       ; counter (= memory) in a-list
					 (memories (if exist (second exist) nil)))
				    (if exist
					(rplacd exist (list (cons counter memories)))
				      (setq a-list (cons (list var (list counter)) a-list)))))
			      (remove 'nil
				      (mapcar #'(lambda (entry)
						  (when (listp entry)                     ; extract variables from
							(when (equal (first entry) 'vari)    ; flattened premise
							      (second entry))))
					      (flatten j))))
		      (setq counter (1+ counter))
		      )
	      (setq counter (+ counter displacement))             ; consider memories for propagation
	      (cons-last a-list *store-memories*)
	      
	      ;;; counter-1 points to the last propagated mem
	      (when back-rule      
		    (setf ab-list (list (1- counter) counter))
		    (setf counter (1+ counter)))
	      
	      (cons-last ab-list *back-store-memories* )
	      
	      
	      ))))


(defun pass-5 (rules)   ;; compile the rules and generate the code
  
  (generate-indexing-instructions)
  (dotimes (i (length rules))
	   (compile-rule (nth i rules)
			 (nth i *store-memories*)
			 (nth i *back-store-memories*))))



(defun compile-forward-fam (rules)  ;; the user interface
  "Returns the labels which are used "
  (setq *mc* 0)
  (setq *lc* 0)
  (setq *code* nil)
  (setq *prem-predicates* nil)
  (setq *store-memories* nil)
  (setf *back-store-memories* nil)
  (remove-properties *pred-arity*)
  (remove-properties *pred-adr*)
  
  (catch 'error
    
    ;;      (pass-1 rules)               ; check correct format of rules
    
    (pass-2 rules)               ; gather all predicates from premises in *prem-predicates*
    
    (pass-3 rules)               ; compute code addresses for each premise and store them
					; with corresp. predicate in prop-list *pred-adr*
    
    (pass-4 rules)               ; compute store-binding-addresses for variables in rules
    
    (pass-5 rules)               ; compile all rules and store resulting code in *code*
    
    
      ;;;; load *code* directly.
    (mapcar #'(lambda(form)(eval form)(cadr form)) *code*)
    )
  )



