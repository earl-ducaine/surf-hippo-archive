;;;;;
;;;;;   A mode interpreter for automatically generating procedure modes 
;;;;;   in a RELFUN program
;;;;;
;;;;;  Thomas Krause, DFKI Kaiserslautern
;;;;;  20. March 1991


(defun mode-interpret (query db) 
"QUERY x DATABASE -> ExtTable
Computes an approximation of the mode for all predicates in the database db
relative to the abstract query. The database has to be normalized by using
the command HORIZON on the RELFUN toplevel loop. The function mode-interpret
gives back the extension table, which describes the runtime dataflow."

  (let ((ExtTable (create-ExtTable))
        (fun2 nil))
    (setf ExtTable (index-db db ExtTable)) 
    (setf ExtTable (compute-aliasesBit db ExtTable))
    (setf fun2 (md-interp-goal (mk-litname query)
                               (rest query)
                               nil ;No recursion in the initial query
                               nil ;No call-aliasing in the initial query
                               ExtTable
                               db))
     (second fun2)))
    

(defun md-interp-clauseList (pname callp index-list c-ali ExtTable db)
"PNAME x CALLP x INDEX-LIST x C-ALI x ExtTable x DB
        -> ExtTable"
  (cond ((null index-list)
         (let ((procEntry (s-procEntry pname ExtTable))) 
           (cond ((null procEntry)
                  (progn
                    (princ "*** Warning ***")
                    (princ "there is no clause definition for >>") 
                    (princ (first pname)) (princ "/") (princ (second pname))
                    (princ "<<") (terpri))
                  (setf procEntry (mk-procEntry pname nil nil nil))
                  (setf ExtTable (add-procEntry procEntry  ExtTable))))

                 ;;; repeated call (with CALLP) to a procedure with no definition in DB
           (cond ((exist-succpatEntry-p callp procEntry)
                  ExtTable)

                 ;;; first call (with CALLP) to a procedure with no definition in DB
                 (t
                  (modify-procEntry 
                     pname 
                     (modify-succpEntry callp 
                                        (mk-noDef-succpat callp); compute safe approximation
                                        'close procEntry)
                     ExtTable))
                 )))
         
         ;;; call to a procedure which is defined in DB          
        (t 
         (md-interp-clauseList2 callp index-list c-ali ExtTable db))
        ))

(defun md-interp-clauseList2 (callp index-list c-ali ExtTable db)
  "CALLP x INDEX-LIST x C-ALI x ExtTable x DB
        -> ExtTable"
  (cond ((null index-list)
         ExtTable)
        (t 
         (let ((rest-list (rest index-list)))
           (md-interp-clauseList2 callp rest-list c-ali
                               (md-interp-clause callp 
                                                 (nth (first index-list) db)
                                                 (null rest-list)
                                                 c-ali
                                                 ExtTable db)
                               db)))
        ))

(defun md-interp-clause (callp clause lastcl c-ali ExtTable db) 
  "CALLP x CLAUSE x LASTCLx C-ALI x ExtTable x DB
        -> ExtTable"
  (let ((i-state nil) (aliasing nil)
        (f-state**val**ET nil) ;final state, return value, extension table 
        (succp nil)
        (cl-kind (s-kind clause))
        (cl-head (s-conclusion clause))
        (cl-premises (s-premises clause)))

    
    (setf aliasing (or c-ali                           ; call-aliasing
                       (repeated-variable-p cl-head))) ; return-aliasing
    ;;; computing the initial instantion state
    (setf i-state (safe-istate cl-head callp nil aliasing))

    ;;; computing the final instantiation state and the return value state
    (setf f-state**val**ET (md-interp-goallist (mk-pname clause)
                                                   cl-premises 
                                                   i-state 'free 
                                                   lastcl ExtTable db))

    ;;; computing the success pattern for CLAUSE with respect to CALLP
    (setf succp (get-succp (rest cl-head) (first f-state**val**ET)
                               (cond ((eq cl-kind 'ft)
                                      (second f-state**val**ET))
                                     (t 'true))))

    ;;; computing the new extension table and give back as the result
    (add-succp-to-table (mk-pname clause) callp succp
                        (third f-state**val**ET))))

    
    
    


(defun md-interp-goallist (pname premises i-state val lastcl ExtTable db)
"PNAME x PREMISES x I-STATE x VAL x LASTCL x ExtTable x DATABASE
                              -> I-STATE x VAL x ExtTable"
  (cond ((null premises)
         (list i-state val ExtTable))
         (t
          (let ((goal (first premises)))
           
            ;;; the goal is a denotative literal and by definition in foot position
            (cond ((final-p goal) 
                   (list i-state 
                         (term-state (project-state goal i-state))
                         ExtTable))

                  ;;; the goal is a kind of LISP builtin
                  ((lispcall-p goal)
                   (md-interp-goallist pname 
                                       (rest premises)
                                       (safe-istate goal
                                                    (mk-lsp-succpat (s-termargs 
                                                                     (project-state goal i-state)))
                                                    i-state 
                                                    nil) ; calling LISP cause no aliasing effects  
                                       'closed  ;;; LISP builtin default return value
                                       lastcl
                                       ExtTable db))

                  ;;; the goal is a kind of an is primitive
                  ((is-t goal)
                   (md-interp-is pname (rest premises)
                                 goal i-state lastcl ExtTable db))

                  ;;; an evaluative (normal) goal
                  (t
                   (let ((kof-rec ; kind of allowed recursion
                          (cond ((and lastcl                     ; Only the last clause is recursiv?
                                      (equal pname (mk-litname goal)) ; It is not a indiect recursion?
                                              (null (rest premises))) ; It is a tail recursion?
                                 'safe-rec)                           ; then it is a safe recursion!
                                (t 'unsafe-rec)))
                         (succp**newET nil) ;;; succp, new extension table
                         (lit-activation (rest (project-state goal i-state))))

                     (setf succp**newET (md-interp-goal (mk-litname goal)
                                                        lit-activation
                                                        kof-rec 
                                                        (call-aliasing-p goal)
                                                        ExtTable
                                                        db)) 
                     (md-interp-goallist pname  (rest premises)
                                         (safe-istate goal (first succp**newET)
                                                      i-state
                                                      nil);No use for c-aliasing information here
                                         (first (last (first succp**newET)))
                                         lastcl
                                         (second succp**newET)
                                         db)))
                  )))
         ))
                     
                

(defun md-interp-is (pname rest-premises is-term i-state lastcl ExtTable db)
"PNAME x REST-PREMISES x IS-TERM x I-STATE x LASTCL x ExtTable x DATABASE
                              -> I-STATE x VAL x ExtTable"
  (let ((new-istate nil) 
        (val nil)
        (istate**retset**ExtTable nil)
        (lhs-term (s-patt-is is-term))
        (rhs-term (s-expr-is is-term))) 

           ;;; Denotative is-Literal
    (cond ((final-p rhs-term)
           (cond ((inst-t rhs-term)
                  (setf new-istate (struc-unify lhs-term rhs-term i-state))
                  (setf val (s-var-state lhs-term new-istate)))
                 (t
                  (setf val (s-unify (project-state lhs-term i-state)
                                         (project-state rhs-term i-state)))
                  (setf new-istate (add-istate lhs-term val i-state)))
                 ))

         ;;; evaluative is-Literal
          (t  
           (setf istate**retset**ExtTable
                 (md-interp-goallist pname (list rhs-term)
                                     i-state
                                     'empty    ;; No information about the return value available
                                     (cond (lastcl 'lcl-is); last clause and is primitive
                                           (t lastcl))
                                     ExtTable db))

           ;;; handling the VALue of the abstract goal given by RHS-TERM 
           (cond ((vari-t lhs-term)
                  (setf val (s-unify (second istate**retset**ExtTable)
                                     (s-var-state lhs-term (first istate**retset**ExtTable))))
                  (setf new-istate (add-istate lhs-term 
                                               val i-state)))

                 (t; lhs-term is a constant
                  (setf val (s-unify (second istate**retset**ExtTable)
                                         (project-state lhs-term i-state))
                        new-istate (first istate**retset**ExtTable)))
                 ))
          )

    (md-interp-goallist pname  rest-premises new-istate
                        val lastcl
                        (cond ((final-p rhs-term) ExtTable)
                              (t (third istate**retset**ExtTable)))
                        db))) 
    


(defun md-interp-goal (pname callp kof-rec c-ali ExtTable db)
  "PNAME  x  CALLP x KOF-REC x C-ALI x ExtTable x DATABASE 
                           -> succp x ExtTable"
  (let ((newExtTable nil)
        (procEntry (s-procEntry pname ExtTable)))
    (cond ((succpat-close-p callp procEntry) ; succes pattern is allready computed
           (list (s-succpat callp procEntry) ExtTable))

          ((succpat-open-p callp procEntry) ;a recursive call with the same call pattern 
           (cond ((eq kof-rec 'safe-rec)
                  (let ((new-callp nil)
                        (current-succp (s-succpat callp procEntry)))
                    (setf new-callp (reverse (rest (reverse current-succp))))

                    (cond ((or (not new-callp)           ; there exist no succp
                               (equal new-callp callp))  ; a recursive call gives no more information
                           (mk-succpat-close pname callp kof-rec ExtTable))
                          
                          (t
                           (md-interp-goal pname new-callp nil c-ali ExtTable db))
                          )))
                  
                 (t
                  (mk-succpat-close pname callp kof-rec ExtTable))))

          (t; the first call to procedure PNAME with call pattern CALLP
           
           ;;; try all possible clauses with these call pattern
           (setf newExtTable 
                 (md-interp-clauseList pname callp 
                                       (s-indexList procEntry) c-ali
                                       (cond ((null procEntry); call to extern procedure
                                              ExtTable)
                                             (t
                                              (mk-succpat-open pname callp ExtTable)))
                                       db))
           
           ;;; No further need to compute these goal with the same call pattern!
           (mk-succpat-close pname 
                             callp 
                             'goal-finished 
                             newExtTable))
          )))


(defun index-db (db ExtTable) 
"DATABASE ExtTable -> ExtTable"
  (index-db2 (reverse db)
             (1- (length db)) ; the first clause has the index 0!
             ExtTable))

  
(defun index-db2 (db clause-index ExtTable)
"DATABASE x CLAUSE-INDEX x ExtTable  ->  ExtTable"
  (cond ((null db) ExtTable)
        (t 
         (let ((clause (first db))
               (db-rest (rest db))
               (pname nil)(procEntry nil))
           (setf pname (mk-pname clause)
                 procEntry (s-procEntry pname ExtTable)) 
           (setf ExtTable
                 (cond ((null procEntry) ; there is no entry for the procedure yet
                                  (add-procEntry (mk-procEntry pname nil nil 
                                                               (list clause-index))
                                                 ExtTable))
                       
                       ((recursive-call-p clause)
                                  (modify-procEntry pname
                                                    (mk-procEntry pname nil nil
                                                                  (append (s-indexList procEntry)
                                                                         (list clause-index)))
                                                    ExtTable))
                       
                       (t 
                        (modify-procEntry pname
                                          (mk-procEntry pname nil nil
                                                        (cons  clause-index
                                                               (s-indexList procEntry)))
                                          ExtTable))
                       ))
           (index-db2 db-rest (1- clause-index) ExtTable)))
        ))
                             
           
           

(defun compute-aliasesBit (db ExtTable)
"DATABASE x ExtTable  ->  ExtTable"
  (let ((aliases-list nil)
        (new-ExtTable (return-aliasesBit db ExtTable)))
    (setf aliases-list (collect-aliasesBit new-ExtTable))
    (call-aliasesBit db aliases-list (null aliases-list) new-ExtTable)))


(defun return-aliasesBit (db ExtTable)
  (cond ((null ExtTable) nil)
        (t
         (let ((pname nil) (i-list nil)
               (procEntry (first ExtTable)))
           (setf pname (first procEntry)
                 i-list (s-indexList procEntry))
           (cond ((return-aliases-p i-list db)
                  (cons (mk-procEntry pname
                                      t            ; ALI-BIT!
                                      nil          ; SUCCPAT-TABLE
                                      i-list)
                        (return-aliasesBit db (rest ExtTable))))
                 (t
                  (cons procEntry
                        (return-aliasesBit db (rest ExtTable))))
                 )))
        ))
           
  
(defun call-aliasesBit (db ali-list endloop ExtTable)
;;; ENDLOOP indicates no chnage of the ALI-BIT while the last iteration
  (cond (endloop ExtTable)
        (t (let ((more-ali-list nil) 
                 (copy-ExtTable (copy-list ExtTable)))
             (dolist (procEntry ExtTable) ; for all predicates do
               (cond ((or (aliases-p procEntry)
                          (member (first procEntry) more-ali-list :test #'equal))
                      t); nothing to do becuse the predicates is allready tagged
                   
                     ((call-aliases-p (s-indexList procEntry)
                                        (append more-ali-list ali-list)
                                        db)
                      (setf more-ali-list (cons (first procEntry) more-ali-list)
                            copy-ExtTable (modify-procEntry (first procEntry)
                                                            (set-aliasesBit procEntry)
                                                            copy-ExtTable)))
                     ))
             (call-aliasesBit db (append more-ali-list ali-list) 
                                    (null more-ali-list) ; Was there a change in the ALI-BIT?
                                    copy-ExtTable)
             ))
        ))
        

(defun term-state (lit-activation) 
"LIT-ACTIVATION ->  TERM-STATE"
  (cond ((atom lit-activation) lit-activation)
        ((inclusion-p 'closed (lub (s-termargs lit-activation)))
         'closed)
        (t 'dontknow)
        ))

(defun safe-istate (literal succp i-state aliasing)
"LITERAL x SUCCP x I-STATE x ALIASING 	
				 ->  I-STATE"
  (safe-istate2 (rest literal) succp 
                      aliasing 
                      i-state))

(defun safe-istate2 (arglist succp aliBit i-state)
  (cond ((null arglist)  i-state) 
         (t
         (let ((var-state nil)
               (arg (first arglist)))
           (cond ((vari-t arg)
                  (setf var-state 
                        (s-unify (s-var-state arg i-state)
                                 (first succp)))
                  (cond ((and 
                          aliBit ; indicates return-aliasing    
                          (eq var-state 'free))
                         (setf var-state 'dontknow)))    
                  (safe-istate2 (rest arglist)
                                (rest succp) aliBit
                                (add-istate arg var-state i-state)))
                 (t
                  (safe-istate2 (rest arglist) (rest succp) aliBit i-state))
                 )))
        ))

           
(defun lub (term-state-list)
"TERM-STATE-LIST  ->  TERM-STATE"
  (lub2 term-state-list 'empty))

(defun lub2 (tsl ts)
  (cond ((null tsl) ts)
        (t (lub2 (rest tsl)
                 (get-lub (first tsl) ts)))
        ))

(defun pattern-lub (pat1 pat2)
"PATTERN x PATTERN  ->  PATTERN"
  (cond ((and (null pat1) 
              (null pat2))
         nil)
        (t (cons (get-lub (first pat1) (first pat2))
                 (pattern-lub (rest pat1) (rest pat2))))
        ))


(defun get-lub (ts1 ts2)
  (cond ((eq ts1 ts2) ts1)
        
        ((or (eq ts1 'dontknow)
             (eq ts2 'dontknow))
         'dontknow)
        
        ((or (and (eq ts1 'closed) (eq ts2 'true))
             (and (eq ts1 'true) (eq ts2 'closed)))
         'closed)
        
        ((or (eq ts1 'empty)
             (null ts1))
         ts2)
        
        ((or (eq ts2 'empty)
             (null ts2))
         ts1)
        
        ((or (eq ts1 'free)
            (eq ts2 'free))
            'dontknow)
        
        (t (error "calling get-lub with wrong arguments"))
        ))
  

(defun conser-approx (callp)
  " CALLP -> SUCCP"
  ;;; computes a conservative approximation for the callp, for instance because
  ;;; of a recursive call
  (cond ((null callp) 
         '(dontknow)) ;;; Approximation of the return term state VAL
        
        ((eq 'free (first callp))
         (cons 'dontknow (conser-approx (rest callp))))
        
        (t
         (cons (first callp) (conser-approx (rest callp))))
        ))

(defun consvalue-approx (callp succp)
  (cond ((eq (length callp) (length succp))
         (reverse (cons 'dontknow (reverse succp))))
        (t
         (reverse (cons 'dontknow (rest (reverse succp)))))))


(defun s-unify (ts1 ts2)
"TERM-STATE x TERM-STATE  ->  TERM-STATE "
  (cond ((or (eq ts1 'empty)
             (eq ts2 'empty))
         'empty)
        ((or (eq ts1 'true)
             (eq ts2 'true))
         'true)
        ((or (eq ts1 'closed) 
             (eq ts2 'closed))
         'closed)
        ((or (eq ts1 'dontknow) 
             (eq ts2 'dontknow))
         'dontknow)
        (t 'free)
        ))

(defun struc-unify (var struc i-state)
"VARIABLE x STRUCTURE x i-state  ->  i-state"
  (let ((new-istate i-state)
        (var-state (s-var-state var i-state))
        (term-state (term-state (project-state struc i-state))))
    
    (cond ((eq 'closed term-state)
           (add-istate var 'closed i-state))
          
          ((or (eq 'closed var-state)
               (eq 'true var-state)  ;;; maybe, an error could generated!!!
               )
           (dolist (v (collect-new-vars struc nil))
             (setf new-istate (add-istate v 'closed new-istate)))
           new-istate)
          
          ((eq var-state 'free)
           (add-istate var 'dontknow i-state))
          
          (t
           (dolist (v (collect-new-vars struc (list var)))
             (setf new-istate (add-istate v 'dontknow new-istate)))
           new-istate)
          )))
           
           

(defun project-state (literal i-state)
"LITERAL x I-STATE -> LIT-ACTIVATION
   Computes the activation of the literal (instantiation pattern) by 
   projecting each i-state of an argument into the literal"

  (cond ((eq literal 'true) 'true)
        ((atom literal) 'closed)
        ((vari-t literal) (s-var-state literal i-state))
        (t 
         (cons (s-functor literal)
               (project-state2 (s-termargs literal) i-state)))
        ))

(defun project-state2 (arglist i-state)
;;; Remember, only flat clauses belong to the relfun kernel. This means that
;;; the arglist is always flat
  (cond ((null arglist) nil)
        (t
         (let ((arg (first arglist)))
           (cons
            (cond ((eq arg 'true) 'true)
                  ((atom arg) 'closed)
                  ((vari-t arg) 
                   (let ((var-state (s-var-state arg i-state)))
                     (cond ((and nil (eq var-state 'free)
                                (member arg
                                      (rest arglist) 
                                      :test #'equal)); testing call-aliasing
                            (setf i-state (add-istate arg 'dontknow i-state))
                           'dontknow)
                           (t var-state))))
                  (t (error "No flat arglist!!!")))
            (project-state2 (rest arglist) i-state))))
        ))



;;;;;
;;;;;
;;;;;            SELECTORS and COLLECTORS
;;;;;
;;;;;

(defun s-procEntry (pname ExtTable)
  (assoc pname ExtTable :test #'equal))

(defun s-succpat-table (procEntry)
  (third procEntry))

(defun s-succpat-entry (callp procEntry)
  (assoc callp (s-succpat-table procEntry) :test #'equal))

(defun s-succpat (callp procEntry)
  (second (s-succpat-entry callp procEntry)))

(defun s-succpat-state (callp procEntry)
  (third (s-succpat-entry callp procEntry)))

(defun s-indexList (procEntry)
  (fourth procEntry))

(defun s-aliasesBit (procEntry)
  (second procEntry))

(defun collect-aliasesBit (ExtTable) 
  (cond ((null ExtTable) nil)
        ((aliases-p (first ExtTable))
         (cons (first (first ExtTable))
               (collect-aliasesBit (rest ExtTable))))
        (t (collect-aliasesBit (rest ExtTable)))))

(defun collect-new-vars (term variables) 
  "TERM x VARS -> VARS
  collects all variables of term and returns only the new variables"
   
   (cond ((null term) nil)
         ((atom term) nil)
         ((and (vari-t term) (not (member term variables :test #'equal)))
          (cons term nil))
         (t 
          (let ((new-vars (collect-new-vars (first term) variables)))
            (append new-vars
                    (collect-new-vars (rest term) (append new-vars variables)))))
         ))


(defun s-var-state (variable i-state)
"VARIABLE  x I-STATE  ->  VAR-STATE"
  (let ((var-state (assoc variable i-state :test #'equal))) 
    (cond (var-state
           (second var-state))
           (t 
            'free))))

(defun s-functor (term)
  (cond ((final-p term)
         (first (second term)))
        (t (first term))))


(defun s-termargs (term)
  (cond ((final-p term)
         (rest (second term)))
        (t (rest term))))




;;;;;
;;;;;
;;;;;            CONSTRUCTORS and MODIFIERS 
;;;;;
;;;;;


(defun create-ExtTable ()
" -> ExtTable
   creates an empty extension table"
   nil)

(defun add-procEntry (procEntry ExtTable)
"procEntry x ExtTable -> ExtTable"
  (cons procEntry ExtTable))

(defun rm-procEntry (pname ExtTable)
"PNAME x ExtTable -> ExtTable"
  (cond ((null ExtTable) nil)
        (t (let ((p-entry (first ExtTable)))
             (cond ((equal pname (first p-entry)) ; pname matched the procEntry
                    (rest ExtTable))
                   (t (cons p-entry (rm-procEntry pname (rest ExtTable)))))))))


(defun modify-procEntry (pname new-entry ExtTable)
"PNAME x newEntry x ExtTable -> ExtTable"
  (add-procEntry new-entry
                 (rm-procEntry pname ExtTable)))

(defun addnew-procEntry (pname ExtTable)
"PNAME x ExtTable -> ExtTable
   creates a new procedure entry for pname and adds the entry to the
   extension table"

   (add-procEntry (mk-procEntry pname nil nil nil) ; the procEntry skeleton
                  ExtTable))
 

(defun mk-succpatEntry (callp succp oc-bit)
  (list callp succp oc-bit))

(defun modify-succpEntry (callp succp oc-bit procEntry)
"function modify-succpEntry: callp x succp x oc-bit procEntry
                             -> procEntry"
  (mk-procEntry (first procEntry)
                (s-aliasesBit procEntry)
                (modify-succpEntry2 callp succp oc-bit
                                    (s-succpat-table procEntry) nil)
                (s-indexList procEntry)))

(defun modify-succpEntry2 (callp succp oc-bit succpat-table new-succpat-table)
  (cond ((null succpat-table) 
         (cons (mk-succpatEntry callp succp oc-bit) new-succpat-table))
        (t
         (let ((succpat-entry (first succpat-table)))
           (cond ((equal callp (first succpat-entry))
                  (cons (mk-succpatEntry callp succp oc-bit)
                        (append new-succpat-table
                                (rest succpat-table))))
                 (t
                  (modify-succpEntry2 callp succp oc-bit
                                      (rest succpat-table)
                                      (cons succpat-entry new-succpat-table)))
                 )))
        ))

(defun add-succp-to-table (pname callp succp ExtTable)
"PNAME x CALLP x SUCCP x ExtTable
                              -> ExtTable
   computes the least upper bound from the actual success pattern and succp
   and puts the result in the correspondending slot in the returning ExtTable"

   (let ((procEntry (s-procEntry pname ExtTable)))

            ;;; the succpat entry is allready closed
     (cond ((succpat-close-p callp procEntry)
            ExtTable)

           ;;; the succpat entry is open
           ((succpat-open-p callp procEntry)
            (let ((old-succp (s-succpat callp procEntry)))
              (modify-procEntry pname
                                (modify-succpEntry callp 
                                                   (pattern-lub succp old-succp)
                                                   'open procEntry)
                                ExtTable)))
            
           (t " error because the entry is neither close nor open"
             (error "close/open error in the succpat table"))
           )))

           

(defun get-succp (arglist i-state ret-value)
  (cond ((null arglist) (list ret-value))
        (t
         (cons (term-state (project-state (first arglist) i-state))
               (get-succp (rest arglist) i-state ret-value)))
        ))

  
(defun mk-succpat-open (pname callp ExtTable)
"PNAME x CALLP x ExtTable -> ExtTable "
  (let ((procEntry (s-procEntry pname ExtTable)))
    (modify-procEntry pname (modify-succpEntry callp 
                                                 (s-succpat  callp procEntry)
                                                 'open
                                                 procEntry)
                      ExtTable)))


(defun mk-succpat-close (pname callp why ExtTable)
"PNAME x CALLP x WHY x ExtTable
                           -> succpatEntry x ExtTable"
  (let ((procEntry (s-procEntry pname ExtTable))
        (new-procEntry nil))
    (case why
      ((goal-finished  safe-rec)  ;;; correct succes patter is always computed!
       (setf new-procEntry (modify-succpEntry callp
                                               (s-succpat callp procEntry)
                                               'close procEntry)))
      (unsafe-rec   ;;; compute a conservative (but safe) approximation
       (setf new-procEntry (modify-succpEntry callp
                                               (conser-approx callp)
                                               'close procEntry)))
      (otherwise
       (error "Unknown justification for closing the succpat entry")))
    
    (list (s-succpat callp new-procEntry)
          (modify-procEntry (first procEntry) new-procEntry
                            ExtTable))))
                             
           

(defun mk-procEntry (pname ali-bit succpat-table index-table)
"PNAME x ALI-BIT x  SUCCPAT-TABLE x INDEX-TABLE 
                        -> procEntry
   builts a procedure entry"
   (list pname ali-bit succpat-table index-table))


(defun set-aliasesBit (procEntry)
  (setf (second procEntry) t)
  procEntry)


(defun mk-pname (clause)
"CALUSE -> PNAME
  computes the procedure name, to which the clause belongs to"
  (let ((head (s-conclusion clause)))
    (list (first head)
          (length (rest head)))))


(defun mk-litname (literal)
  (list (first literal)
          (length (rest literal))))


(defun mk-lsp-succpat (callp)
  ;;; Note, there are only closed arguments allowed for a lispcall
  (cond ((null callp) '(closed))
        (t (cons (s-unify (first callp) 'closed)
                 (mk-lsp-succpat (rest callp))))
        ))
  
(defun mk-noDef-succpat (callp)
  ;;; Note, you have to safe in your approximation a
  (cond ((null callp) '(dontknow))
        (t (cons (s-unify (first callp) 'dontknow)
                 (mk-noDef-succpat (rest callp))))
        ))
  
(defun add-istate (var var-state state)
  (cond ((null state)
         (cons (list var var-state) state))
        (t (let ((state-entry (first state)))
             (cond ((equal var (first state-entry))
                    (cons (list var var-state)
                          (rest state)))
                   (t
                    (cons state-entry
                          (add-istate var var-state (rest state))))
                   )))
        ))




;;;;;
;;;;;
;;;;;            PREDICATES
;;;;;
;;;;;



(defun exist-procEntry-p (pname ExtTable)
  (s-procEntry pname ExtTable))

(defun exist-succpatEntry-p (callp procEntry)
  (s-succpat-entry callp procEntry))

(defun aliases-p (procEntry)
  (s-aliasesBit procEntry))

(defun return-aliases-p (i-list db)
"CL-INDEX-LIST x DATABASE -> TRUTH-VALUE"
  (cond ((null i-list) nil)
        (t 
         (let ((clause (nth (first i-list) db))) ;Select the next clause
           (cond ((repeated-variable-p (s-conclusion clause)) ; Does a variable occur more than once  
                  t)                                           ; in the head?
                 ; does a head variable occur in the body
                 ((intersection (collect-variables (s-conclusion clause))
                                (collect-variables (s-premises clause))
                                :test #'equal)
                  t)
                  
                 (t ; no, than testing the next clause
                  (return-aliases-p (rest i-list) db))
                 )))
        ))


(defun call-aliases-p (i-list ali-list db)
  (cond ((null i-list) nil)
        ((call-to-aliases-p (s-premises (nth (first i-list) db))
                            ali-list)
         t);
        (t 
         (call-aliases-p (rest i-list) ali-list db))
        ))


(defun call-to-aliases-p (literals ali-list)
"LITERAL x ALI-LIST -> TRUTH-VALUE"
  (cond ((null literals) nil)
        (t 
         (let ((lit (first literals)))
           (cond ((final-p lit)
                  (call-to-aliases-p (rest literals) ali-list))
                 ((is-t lit)
                  (call-to-aliases-p (cons (third lit) (rest literals))
                                     ali-list))
                 ((member (list (first lit) (length (rest lit)))
                          ali-list :test #'equal)
                  t)
                 (t
                  (call-to-aliases-p (rest literals) ali-list))
                 )))
        ))
           
(defun call-aliasing-p (literal)
  (repeated-variable-p literal))

(defun repeated-variable-p (literal) 
"LITERAL -> TRUTH-VALUE"
  (rep-var-p literal nil))

(defun rep-var-p (lit vars)
  (cond ((null lit) nil)
        (t
         (let ((arg (first lit)))
           (cond ((vari-t arg)
                  (cond ((member (second arg) vars)
                         t); repeated variable detected
                        (t (rep-var-p (rest lit) (cons (second arg) vars)))
                        ))
                 (t (rep-var-p (rest lit) vars)))))
        ))

  
(defun succpat-close-p (callp procEntry)
  (eq 'close (s-succpat-state callp procEntry)))

(defun succpat-open-p (callp procEntry)
  (eq 'open (s-succpat-state callp procEntry)))


(defun inclusion-p (ts1 ts2)
"TERM-STATE x TERM-STATE -> TRUTH-VALUE"
  (case ts1
    (empty (null ts2))
    (true (cond ((eq ts2 'true) t)
                (t (null ts2))))
    (closed (cond ((or (eq ts2 'closed) 
                       (eq ts2 'true))
                   t)
                  (t (null ts2))))
    (free (cond ((eq ts2 'free))
                (t (null ts2))))
    (dontknow t)))

        
(defun recursive-call-p (clause)
  (let ((head (s-conclusion clause)))
    (recursive-call2-p (first head) 
                       (length (rest head))
                       (s-premises clause))))

(defun recursive-call2-p (cl-name arity premises)
  (cond ((null premises) nil)
        (t
         (let ((prem (first premises)))
           (cond ((is-t prem)
                  (recursive-call2-p cl-name arity 
                                     (cons (s-expr-is prem)
                                           (rest premises))))
                 ((final-p prem)
                  (recursive-call2-p cl-name arity (rest premises)))
                  ((and (eq cl-name (s-functor prem))
                        (eql arity (length (s-termargs prem))))
                   t)
                  (t
                   (recursive-call2-p cl-name arity (rest premises)))
                  )))
        ))

               
