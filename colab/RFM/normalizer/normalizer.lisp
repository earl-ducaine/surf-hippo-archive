;;;;;
;;;;;   The normalizer computes the RELFUN kernel for a  
;;;;;   RELFUN program
;;;;;
;;;;;  Thomas Krause, DFKI Kaiserslautern
;;;;;  22. March 1991


(defun normalize-database (db &optional (count nil)) "DATABASE -> DATABASE"
;;;;
;;;; REMARK: COUNT is for debugging only
;;;;
   (declare (special *error-level* *error-stream*)); for debugging only
   (setq *error-stream* nil)
   (cond ((null db) nil)
         (t
          (let ((clause (first db))
                (proc-name (mk-proc-name (first db))))
            (case  *error-level* 
              ((1 2 3) (setf *error-stream* (list (list proc-name clause))))
              (4 (setf *error-stream* (cons (list proc-name clause) *error-stream*)))
              (otherwise (setq *error-stream* nil)))

            (if count (progn (princ count) (princ " ")))

            (cons (first (normalize-clause clause nil))
                  (normalize-database (rest db) (if count (1+ count) nil)))))))

(defun normalize-clause (clause proc-info) 
  "CLAUSE x PROC-INFO -> CLAUSE x PROC-INFO"
  (normalize-flat-clause clause proc-info)) ; clause must be allready flatten
                                            ; by using flatten-struc-clause

(defun normalize-flat-clause (cl proc-info)
  (let ((funM nil) ;holds values from multiple values functions
        (head-chunk nil) (cl-info nil) (guards nil)(prem-rest nil)
        (kind (s-kind cl))
        (head (s-conclusion cl))
        (premises (s-premises cl)))

    ;;; collecting the guards of the first chunk (namely the head chunk)
    (setf funM (collect-guards premises)) ; funM = <guard-list x <call-literal x premises>>
    (setf guards (first funM)            
          prem-rest (second funM))

    
    ;;; computing the normalized head chunk
    (setf funM                               ;;;  status x head chunk x clause info
          (norm-head-chunk kind head guards prem-rest))
    (setf head-chunk (second funM)
          cl-info (third funM))
    
    (case (first funM) ; whats happened by normalizing?
      (unknown  ; detecting unknown 
       (list (mk-clause kind head '(unknown))
             proc-info))

      (lisp-evaluate  ;  LISP expression was evaluated, as a result there are new guards!
       (normalize-flat-clause   ; with the computed lisp result
        (mk-clause kind (first head-chunk) 
                   (append (rest head-chunk)
                           (rewrite (rest prem-rest)
                                    (s-simple-bindings cl-info))))
        proc-info))
      
      (lisp-error
       (error "Lisp error: NOT YET IMPLEMENTED") (terpri))
      
      (ok
       (norm-premises (cons kind head-chunk) (rest prem-rest) cl-info proc-info))

      (otherwise 
       (error "normalize-clause")))))


(defun norm-premises (cl-new cl-rest cl-info proc-info) 
"CL-NEW x CL-REST x CL-INFO x PROC-INFO -> clause x proc-info"
   (cond ((null cl-rest) 
          (list cl-new proc-info))
         (t
          (let ((funM nil)
                (guards nil) (prem-rest nil)
                (chunk nil) (cl-info-new nil))
            
            ;;; collecting the guards of the next chunk 
            (setf funM (collect-guards cl-rest))
            (setf guards (first funM)            
                  prem-rest (second funM))

            (setf funM                      ;;; status x head chunk x clause info
                  (norm-chunk guards prem-rest cl-info))
            (setf chunk (second funM)
                  cl-info-new (third funM))


            (case (first funM) ; whats happened by normalizing?
              (unknown  ; detecting unknown 
               (list (mk-clause (s-kind cl-new) 
                          (s-conclusion cl-new)
                          '(unknown))
                     proc-info))

              (lisp-evaluate  ; LISP expression was evaluated, as a result there are new guards!
                (norm-premises cl-new
                               (append chunk
                                       (rewrite (rest prem-rest)
                                                (s-simple-bindings cl-info-new)))
                               cl-info proc-info))
      
              (lisp-error
               (error "Lisp error: NOT YET IMPLEMENTED") (terpri))
      
              (ok
               (norm-premises (append cl-new chunk)
                              (rest prem-rest)
                              cl-info-new proc-info))

              (otherwise 
               (error "normalize-clause")))))
         ))


(defun norm-head-chunk (kind head guards prem-rest) 
"KIND x HEAD x GUARDS x PREM-REST -> STATUS x HEAD-CHUNK x CL-INFO"
  (let ((funM nil)
        (new-guards nil) (chunk-vars nil)
        (bindings-s nil) (bindings-c nil) (new-head nil)
        (chunk-foot (s-chunk-foot guards)))

    (setf funM
          (norm-guards guards nil bindings-s bindings-c))

    (setf new-guards (first funM)
          bindings-s (second funM)
          bindings-c (third funM)
          chunk-vars (fourth funM)) 
  
    (cond ((eq new-guards 'unknown)
           (list 'unknown))
          
          ((null prem-rest)
           (setf new-head (rewrite head bindings-s))
           (list 'ok
                 (append (cons new-head
                               (sort-head-guards new-guards
                                                 (collect-new-vars new-head nil)))
                         (cond ((eq kind 'ft)
                                (list
                                 (norm-foot (rewrite chunk-foot bindings-s)
                                            bindings-c)))
                               (t ; hn-clause
                                nil)
                               ))
                 (mk-cl-info kind (collect-vars head chunk-vars)
                             bindings-s bindings-c)))

          (t
           (let ((prem (rewrite (first prem-rest) bindings-s)))
             (cond ((lispcall-inst-p prem bindings-c) ; full instantiate call to lisp
                    (list 'lisp-evaluate
                          (append (cons (rewrite head bindings-s)
                                  new-guards)
                                  (list (cond ((is-t prem)
                                               (mk-is (s-patt-is prem)
                                                      (un-inst (lisp-exec
                                                                (rewrite (s-expr-is prem)
                                                                         bindings-c) nil))))
                                              (t 
                                               (lisp-exec (rewrite prem bindings-c) nil)))))
                          (mk-cl-info kind (collect-vars head chunk-vars)
                                      bindings-s bindings-c)))
                          
                   (t  
                    (setf chunk-vars (collect-vars prem chunk-vars)
                          new-head (rewrite head bindings-s))
                    (list 'ok
                          (append (cons new-head
                               (sort-head-guards new-guards
                                                 (collect-new-vars new-head nil)))
                                  (list (rewrite (first prem-rest)
                                                 bindings-s)))
                          (mk-cl-info kind (collect-vars head chunk-vars)
                                      bindings-s bindings-c)))
                   )))
          )))

        

(defun norm-chunk (guards prem-rest cl-info) 
"GUARDS x PREM-REST x CL-INFO -> STATUS x BODY-CHUNKN x CL-INFO"
  (let ((funM nil)
        (new-guards nil) (chunk-vars nil)   
        (bindings-s (s-simple-bindings cl-info))
        (bindings-c (s-complex-bindings cl-info))
        (chunk-foot (s-chunk-foot guards)))

    (setf funM
          (norm-guards guards (s-cl-info-variables cl-info)
                       bindings-s bindings-c))
    (setf new-guards (first funM)
          bindings-s (second funM)
          bindings-c (third funM)
          chunk-vars (fourth funM)) 
    
    (cond ((eq new-guards 'unknown)
           (list 'unknown))
          
          ((null prem-rest)
           (list 'ok 
                 (append  new-guards
                          (cond ((eq (s-cl-info-kind cl-info)'ft)
                                 (list
                                  (norm-foot (rewrite chunk-foot bindings-s)
                                             bindings-c)))
                                (t ; hn-clause
                                 nil)
                                ))
                 (mk-cl-info (s-cl-info-kind cl-info)
                             chunk-vars
                             bindings-s bindings-c)))
          (t
           (let ((prem (rewrite (first prem-rest) bindings-s)))
             (cond ((lispcall-inst-p prem bindings-c) ; full instantiate call to lisp
                    (list 'lisp-evaluate
                          (append guards
                                  (list (cond ((is-t prem)
                                               (mk-is (s-patt-is prem)
                                                      (un-inst (lisp-exec
                                                                (rewrite (s-expr-is prem)
                                                                         bindings-c) nil))))
                                              (t 
                                               (lisp-exec (rewrite prem bindings-c) nil)))))
                          (mk-cl-info (s-cl-info-kind cl-info)
                                      chunk-vars
                                     (s-simple-bindings cl-info)
                                     (s-complex-bindings cl-info) )))
                   (t
                    (setf chunk-vars (collect-vars prem chunk-vars))
                    (list 'ok
                          (append new-guards (list prem))
                          (mk-cl-info (s-cl-info-kind cl-info)
                                      chunk-vars
                                      bindings-s bindings-c))
                   ))))
          )))
       

(defun norm-guards (guards var-before bindings-s bindings-c) 
  "GUARDS x VAR-BEFORE x BINDINGS-S x BINDINGS-C 
          -> NEW-GUARDS BINDINGS-S x BINDINGS-C x VAR-AFTER"
  (norm-guards2 guards nil nil var-before nil bindings-s bindings-c nil))

(defun norm-guards2 (guard-rest new-guards conflict-guards 
                            var-before var-add 
                            bind-s 
                            bind-c nbind-c) ; complex- and new-complex-bindings

  "computes the normalized guards called new-guards. The computation looks like a fixpoint
   computation. This means that a guard is added to the list CONFLICCT-GUARDS if it 
   causes a meaningfull change to the complex bindings. As a result, maybe two rhs of
   BINDINGS-C become identity and structure sharing is possible. This is indicated by the
   function COLLECT-IS-SET-FROM-NEW-BINDINGS and a new iteration is started with the new 
   guards, gaining by COLLECT-IS-SET-FROM-NEW-BINDINGS and  the guards of conflict-guards"
 
  (cond ((null guard-rest) 
         (let ((more-is-set (collect-is-set-from-new-bindings bind-c nbind-c)))
           (cond (more-is-set   ;;; one more iteration
                  (norm-guards2 (append more-is-set conflict-guards)
                                new-guards nil
                                var-before nil 
                                bind-s 
                                bind-c nil))
                 (t
                  (list (append new-guards conflict-guards) 
                        bind-s 
                        (append bind-c nbind-c)
                        (append var-before var-add))))))
        (t
         (let ((term**tag nil)
               (term (rewrite (first guard-rest) bind-s))
               (all-bind-c (append nbind-c bind-c)))
           (setf term**tag (get-guardClas term (append var-before var-add) all-bind-c))
           (setf term (first term**tag))
                

           (case (second term**tag)
             (unknown ;;; apply UNP
              (list 'unknown bind-s bind-c var-before))
             
             (deno-prem   ;;; can be deleted because a foot is handled in function norm-chunk
                          ;;; apply DBE
              (norm-guards2 (rest guard-rest) new-guards conflict-guards
                            var-before var-add
                            bind-s bind-c nbind-c))
             
             (new-simple ;;; e.g. (is _x a) or (is _x _v) _x occurs first 
                         ;;; apply DIP
              (norm-guards2 (rest guard-rest) new-guards conflict-guards
                            var-before 
                            (cond ((vari-t (s-expr-is term))
                                   (append var-add
                                           (collect-new-vars (s-expr-is term)
                                                             (append var-before var-add))))
                                  (t var-add))
                            (mk-bindings (s-patt-is term)  
                                     (s-expr-is term)
                                     bind-s)
                            bind-c nbind-c))
             
             (new-complex ;;; e.g. (is _x `(f a)) _x occurs first (_y `(f a)) is in complex-bindings
                          ;;;  apply DIP or CSE
              (let ((rhs-inst nil) (common-expr nil)
                    (lhs-is (s-patt-is term))
                    (rhs-is (un-inst (s-expr-is term))))
                (setf rhs-inst (rewrite rhs-is all-bind-c))
                (setf common-expr 
                      (rhs-match-p rhs-inst all-bind-c)) ;nil indicates no match, otherwise 
                                                         ;the variable (bind to rhs-inst) is returned
                
                (cond ((occur-check-p lhs-is rhs-inst) ; occur-check (-> apply SIU)
                       (progn 
                         (error-handler 20 ; occur-check
                                        term)
                         (list 'unknown bind-s bind-c var-before)))
                      
                      (common-expr    ;;; applx CSE
                       (norm-guards2 (rest guard-rest) new-guards conflict-guards
                                     var-before 
                                     var-add
                                     (mk-bindings lhs-is 
                                                  (first common-expr) 
                                                  bind-s)
                                     bind-c nbind-c))
                      
                      (t ;;; a new complex structure  (apply DIP)
                       (norm-guards2 (rest guard-rest) new-guards
                                     (cons term conflict-guards)
                                     var-before 
                                     (append var-add 
                                             (collect-new-vars term
                                                               (append var-add var-before)))
                                     bind-s bind-c
                                     (mk-bindings lhs-is 
                                                  rhs-inst 
                                                  nbind-c))))))
             
             (unbound-simple ; e.g. (is _v _w); _v have no complex bindings and both do not occur first
                             ;;; apply DIP 
              (let ((rhs-inst nil)
                    (lhs-is (s-patt-is term))
                    (rhs-is (s-expr-is term))
                    (reduction (list (rest term)))) ; builts the reduction lhs -> rhs
                (setf rhs-inst (rewrite rhs-is all-bind-c))
                (cond ((occur-check-p lhs-is (rewrite rhs-is all-bind-c)) ; occur-check
                       (progn 
                       (error-handler 20 ; occur-check
                                      term)
                       (list 'unknown bind-s bind-c var-before)))
                      (t
                       (norm-guards2 (rest guard-rest) 
                                     (cond ((member lhs-is var-before :test #'equal)
                                            (cons term (rewrite new-guards reduction)))
                                           (t (rewrite new-guards reduction)))
                                     (rewrite conflict-guards reduction)
                                     var-before var-add
                                     (mk-bindings (s-patt-is term) (s-expr-is term)
                                                  (rhs-rewrite lhs-is rhs-is bind-s))
                                     (rhs-rewrite lhs-is rhs-inst bind-c)
                                     (rhs-rewrite lhs-is rhs-inst nbind-c))))))
             
             (unbound-complex ; e.g.  (is _v `(f a)) (apply DIP, CSE or SIU)
              (let ((rhs-inst nil)(common-expr nil)
                    (lhs-is (s-patt-is term))
                    (rhs-is (un-inst (s-expr-is term))))
                (setf rhs-inst (rewrite rhs-is all-bind-c))
                (cond ((occur-check-p lhs-is rhs-inst) ; occur-check (apply SIU and UNP)
                       (progn 
                         (error-handler 20 ; occur-check
                                        term)
                         (list 'unknown bind-s bind-c var-before)))
                      (t
                       (setf common-expr (rhs-match-p rhs-inst all-bind-c))
                       (cond (common-expr ;(-> CSE)
                              (norm-guards2 (cons (mk-is lhs-is (first common-expr))
                                                  (rest guard-rest))
                                            new-guards conflict-guards
                                            var-before var-add
                                            bind-s bind-c  nbind-c))
                             (t  ;;; add a new structure to nbind-c 
                                 ;;; -> DIP
                              (norm-guards2 (rest guard-rest) 
                                            new-guards
                                            (cons term conflict-guards)
                                            var-before 
                                            (append var-add 
                                                    (collect-new-vars rhs-is
                                                                      (append var-add var-before)))
                                            bind-s
                                            (rhs-rewrite lhs-is rhs-inst bind-c)
                                            (mk-bindings lhs-is rhs-inst
                                                         (rhs-rewrite lhs-is rhs-inst nbind-c))))
                             ))
                      )))
             
             (complex-complex ;;; (rewrite term all-bind-c ==> (is `(..) `(..))
                              ;;; -> SIU
              (let ((error**is-set nil)  ;;; error=unkonwn, if unification fail; mgu defined by IS-SET
                    (lhs-inst (rewrite (s-patt-is term) all-bind-c))
                    (rhs-inst (rewrite (un-inst (s-expr-is term)) all-bind-c)))
                (setf error**is-set (mk-is-set lhs-inst rhs-inst))
                (norm-guards2 (cons (first error**is-set)
                                    (append (second error**is-set) (rest guard-rest)))
                              new-guards conflict-guards
                              var-before var-add
                              bind-s bind-c  nbind-c)))
             (otherwise
              (error "norm-guards"))
             )))
        ))
                    
  


(defun norm-foot (term bindings-c)
  (let ((common-expr (rhs-match-p term bindings-c)))
    (cond (common-expr
           (first common-expr))
          (t term))
    ))

  
(defun get-guardClas (guard variables bindings-c) 
"guard x VARIABLES x BINDINGS-C -> GUARD x TAG
       Returns a guard in a well manner with a description-tag
       A trivial guard likes (is a a) returns (a 'deno-prem) "

  (trace-in-handler 'get-guardClas (list (list 'guard guard) (list 'variables variables)
                                      (list 'bindings-c bindings-c)) nil)
  (cond ((eq 'unknown guard) ;;; -> UNP
         (error-handler 21 ; unknown-detection
                        guard)
         (trace-out-handler 'get-guardClas '(unknown unknown)))

        ((final-p guard) 
        (trace-out-handler 'guardClass (list guard 'deno-prem)))
        
        (t ; the rhs-is is a denotative term!
         (let* ((lhs-is (s-patt-is guard))
                (rhs-is (un-inst (s-expr-is guard)))
                (lhs-var (vari-t lhs-is))
                (rhs-var (vari-t rhs-is))
                (lhs-inst (if lhs-var (rewrite lhs-is bindings-c)
                             lhs-is))
                (rhs-inst (if rhs-var (rewrite rhs-is bindings-c)
                             rhs-is)))

           (cond ((equal lhs-inst rhs-inst)  ; Trivial Is-Term (-> SIU)
                  (trace-out-handler 'get-guardClas (list (s-expr-is guard)
                                                       ;(if (eq 'unknown lhs-is)
                                                        ; 'unknown
                                                         'deno-prem;)
                                                         )))

                 ((and (or (con-t lhs-is)(con-t rhs-is))  ; Trivial unknown dedection
                       (and (not (eq lhs-is rhs-is))
                            (not (vari-t lhs-inst))
                            (not (vari-t rhs-inst))))
                  (error-handler 22 ; unknown-generation
                                 guard)
                  (trace-out-handler 'get-isClas '(unknown unknown)))
           
                 ; Example (is _x a) + _x not in variables  -> (is _x a) + new-simple
                 ((and lhs-var (not (member lhs-is variables :test #'equal))) ; the variable occurs first
                  (trace-out-handler 'get-guardClass (list guard (if (convar-p rhs-is)
                                                               'new-simple ; then
                                                               'new-complex)))) ; else

                 ; Example (is (f a) _x) + _x not in variables -> (is _x `(f a)) + new-complex
                 ((and rhs-var (not (member rhs-is variables :test #'equal)))
                  (trace-out-handler 'get-guardClass (list (mk-is rhs-is (mk-inst lhs-is))
                                                       (if (convar-p lhs-is)
                                                         'new-simple ; then
                                                         'new-complex)))) ; else

                 ; Is primitive without new variables and structures
                 ((and (convar-p lhs-inst)   ; _v, _w in variables (or constants)
                       (convar-p rhs-is))    ; + no structure 
                  (trace-out-handler 'get-guardClass (list (if lhs-var
                                                             guard
                                                             (mk-is rhs-is lhs-is))
                                                           'unbound-simple)))

                 ((and (convar-p lhs-is)   ; _v, _w in variables (or constants)
                       (convar-p rhs-inst))    ; + no structure 
                  (trace-out-handler 'get-guardClass (list (if rhs-var
                                                             (mk-is rhs-is lhs-is)
                                                             guard)
                                                           'unbound-simple)))
                 
                 ((convar-p lhs-inst)
                  (trace-out-handler 'get-guardClass (list guard 'unbound-complex)))
                 
                 ((convar-p rhs-inst)
                  (trace-out-handler 'get-guardClass (list (mk-is rhs-is (mk-inst lhs-is))
                                                           'unbound-complex)))

                 
                 (t 
                  (trace-out-handler 'get-guardClass (list guard 'complex-complex)))
                 )))
        ))




(defun rewrite (term bindings) "TERM x BINDINGS -> TERM"
  (cond ((null term ) nil)
        ((con-t term)
         term)
        ((vari-t term )
         (s-binding term bindings))
        (t
         (cons (rewrite (first term) bindings) 
               (rewrite (rest term) bindings)))))


;;;      rhs-rewrite

(defun rhs-rewrite (lhs rhs bindings) "VAR x TERM x BINDINGS -> BINDINGS
      Replacing all occurrence of 'lhs' on the right-side in 'bindings'
      to the term denoted by 'rhs'"
   (trace-in-handler 'rhs-rewrite (list (list 'lhs lhs) (list 'rhs rhs) (list 'bindings bindings)) nil)                 
  (cond ((null bindings)
         nil)
        (t
         (let ((binding-first (first bindings))
               (binding-rest (rest bindings)))
           (cons (list (first binding-first)
                       (rewrite (second binding-first) (list (list lhs rhs))))
                 (rhs-rewrite lhs rhs binding-rest))))))

(defun rewrite-with-isset (term is-set)
  (rewrite term (remove-is-from-isset is-set)))

(defun remove-is-from-isset (is-set)
  (mapcar #'remove-is is-set))
    
(defun remove-is (is-term) (remove 'is is-term))

(defun sort-head-guards (guards head-vars)       
   (sort-guards guards  nil head-vars))

(defun sort-guards (guards guards-new variables)
  (cond ((null guards) guards-new)
        (t (let ((gl1 nil)
                 (gl2 nil)
                 (gl1**gl2 (split-guards guards variables)))
             (setf gl1 (first gl1**gl2)
                   gl2 (second gl1**gl2))
           (cond ((null gl1)
                  (sort-guards (rest gl2) (append guards-new
                                                  (list (first gl2)))
                               (collect-new-vars (first gl2) variables)))
                 (t
                  (sort-guards gl2 (append guards-new gl1)
                               (collect-new-vars gl1 variables))))
           ))))

(defun split-guards (guards variables)
  (split-guards2 guards nil nil variables))

(defun split-guards2 (guards gl1 gl2 variables)
  (cond ((null guards) (list gl1 gl2))
        (t (let ((guard (first guards)))
             (cond ((and (is-t guard) 
                         (member (s-patt-is guard) variables :test #'equal))
                    (split-guards2 (rest guards) 
                                   (cons guard gl1) 
                                   gl2 variables))
                   (t
                    (split-guards2 (rest guards) 
                                   gl1
                                   (cons guard gl2) 
                                   variables)))))
        ))


;;;;
;;;;
;;;;     SELECTORS and COLLECTORS
;;;;
;;;;

(defun collect-guards (premises)
  "function collect-guards: premises -> guards x premises
   collects the guards for the next chunk. 
   The guards are returned in reverse order."
  (collect-guards2 nil premises))

(defun collect-guards2 (guard-list premises)
  (cond ((null premises)
         (list guard-list premises))
        (t
         (let ((prem (first premises)))
           (cond ((final-p prem)
                  (collect-guards2 (cons prem guard-list)
                                   (rest premises)))
                 ((and (is-t prem) 
                       (final-p (s-expr-is prem)))
                  (collect-guards2 (cons prem guard-list)
                                   (rest premises)))
                 (t
                  (list guard-list premises)))))
        ))
        
(defun collect-deno-is (premises) 
"PREMISES -> IS-SET x PREMISES
      The returned IS-SET is in the reverse order
      First terms in IS-SET are simple-is-primitives as (is _x a), where both sides
      are Variables or constants"

  (let ((isList**isList**premises (collect-deno-is2 nil nil premises)))
   (list (append (first isList**isList**premises)
                 (second isList**isList**premises))
         (third isList**isList**premises))))

(defun collect-deno-is2 (cv-is ot-is prem)
;;;
;;; collects the next denotative is-literals. 'cv-is' means that only constants or variables
;;; occur in the is-literal
;;;
  (let ((term (first prem))
        (term-rest (rest prem)))

    (cond ((null term-rest) 
           (list cv-is ot-is prem))

          (t
                  ;;; denotative literal
           (cond ((final-p term)
                  (collect-deno-is2 cv-is ot-is term-rest))
                 
                 ;;; denotative is-literal
                 ((and (is-t term)
                       (final-p (s-expr-is term)))
                  (let* ((lhs-is (s-patt-is term))
                         (rhs-is (s-expr-is term))
                         (lhs-cv (convar-p lhs-is))
                         (rhs-cv (convar-p rhs-is)))
                         
                    (cond ((and lhs-cv rhs-cv)
                           (collect-deno-is2 (cons term cv-is) ot-is term-rest))

                          (t (collect-deno-is2 cv-is (cons term ot-is) term-rest)))))
                     
                 ;;; evaluative literal
                 (t 
                  (list cv-is ot-is prem)))))))

             

(defun collect-vars (term variables) 
  "TERM x VARS -> VARS
  collects all variables of term and returns the union of the new variables and VARIABLES"
   
   (cond ((null term) variables)
         ((atom term) variables)
         ((and (vari-t term) (not (member term variables :test #'equal)))
          (cons term variables))
         (t (collect-vars (rest term) (collect-vars (first term) variables)))))


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


(defun s-body-foot (term-list) " LIST -> LIST x TERM"
  (let ((rev-list (reverse term-list)))
    (list (reverse (rest rev-list))
          (first rev-list))))


(defun s-clause-kind (cl-info) "CL-INFO -> CLAUSE-TAG"
   (first cl-info))

(defun s-cl-info-kind (cl-info)
    (first cl-info))
 
(defun s-clause-variables (cl-info) "CL-INFO -> VAR-LIST"
   (second cl-info))

(defun s-cl-info-variables (cl-info)
  (second cl-info))

(defun s-simple-bindings (cl-info) "CL-INFO -> BINDINGS-S"
   (third cl-info))


(defun s-complex-bindings (cl-info) "CL-INFO -> BINDINGS-C"
   (fourth cl-info))


(defun s-binding (variable bindings) "VAR x BINDINGS -> TERM"
  (let ((var-binding (assoc variable bindings :test #'equal))) 
    (cond (var-binding
           (second var-binding))
           (t 
            variable))))

(defun s-chunk-foot (guards)
  (let ((ft-guard (first guards)))
    (cond ((is-t ft-guard)
           (s-patt-is ft-guard))
          (t ft-guard))))

;;;;
;;;;
;;;;    CONSTRUCTORS and MODIFIERS
;;;;
;;;;

(defun mk-variables-cl-info (var-list cl-info) "VARS x CL-INFO -> CL-INFO"
   (mk-cl-info (s-clause-kind cl-info)
               var-list 
               (s-simple-bindings cl-info)
               (s-complex-bindings cl-info)))

(defun mk-proc-name (clause) "CLAUSE -> PROCEDURE-NAME"
   (list (first (s-conclusion clause))
         (length (rest (s-conclusion clause)))))

(defun mk-clause (kind head body-list &optional (foot 'no-foot))
  (cons kind 
        (append (cons head body-list)
                (cond ((eq foot 'no-foot) nil)
                      (t (list foot))))))

(defun mk-bindings (lhs rhs bindings)
             (cons (list lhs rhs) bindings))

(defun mk-is-set (arg-list1 arg-list2) "ARG-LIST x ARG-LIST -> IS-SET
      IS-SET = ( (is ...) ... (is ...)) "
  (cond ((null arg-list1) '(true nil))
        ((eql (length arg-list1) (length arg-list2)) 
         (list 'true (mk-is-set2 arg-list1  arg-list2)))
        (t '(unknown nil))))

(defun mk-is-set2 (l1 l2)
  (cond ((null l1) nil)
        (t (cons (mk-is (first l1) (mk-inst (first l2)))
              (mk-is-set2 (rest l1) (rest l2))))))

(defun mk-clause-info (kind variables simple-bindings complex-bindings)
  (list kind variables simple-bindings complex-bindings))

(defun mk-inst (term)
  (cond ((convar-p term) term)
        (t (cons 'inst (cons term nil)))))

(defun mk-cl-info (kind vars bindings-s binding-c)
  (list kind vars bindings-s binding-c))

(defun mk-isset-from-complex-bindings (bindings)
;;; Die komplexe Bindungsumgebung wird durch die simple Bindung lhs -> rhs aktualisiert
;;; Hierbei kšnnen rechte Seiten gleich werden, dies fŸhrt zu neuen is-Primitiven und zur 
;;; Lšschung der entsprechenden komplexen Regel aus der Datenbasis

(mk-isset-from-complex-bindings2 bindings nil nil))

(defun collect-is-set-from-new-bindings (bind-c nbind-c)
  (second (mk-isset-from-complex-bindings2 nbind-c bind-c nil)))


(defun mk-isset-from-complex-bindings2 (old-bindings new-bindings is-set)
  (cond ((null old-bindings) 
         (list new-bindings is-set))
        (t
         (let* ((rest-bindings (rest old-bindings))
                (next-rule (first old-bindings))
                (lhs-rule (first next-rule))
                (rhs-rule (second next-rule))
                (common-expr (rhs-match-p rhs-rule new-bindings)))
           (cond (common-expr ;; there is the same rigth size
                  (mk-isset-from-complex-bindings2 rest-bindings
                                                   new-bindings
                                                   (cons (mk-is lhs-rule
                                                                (first common-expr))
                                                         is-set)))
                 (t
                  (mk-isset-from-complex-bindings2 rest-bindings
                                                   (cons next-rule new-bindings)
                                                   is-set)))))))


(defun add-conflict-guards  (guard guard-list)
  (reverse (add-conflict-guards2 guard (reverse guard-list))))

(defun add-conflict-guards2  (guard guard-list)
  (cond ((null guard-list)
         (list guard))
        ((is-t guard)
         (let ((lhs (s-patt-is guard))
               (first-guard (first guard-list)))
           (cond ((member lhs (collect-variables first-guard) :test #'equal)
                  (cons guard guard-list))
                 (t (cons first-guard
                     (add-conflict-guards2  guard (rest guard-list))))
                 )))    
        (t (cons guard guard-list))
        ))


;;;;
;;;;
;;;;    PREDICATES
;;;;
;;;;

(defun con-t (term) (atom term))

(defun rhs-match-p (rhs-term bindings) "TERM x BINDINGS -> BINDING"
   (cond ((null bindings) nil)
         (t 
          (let ((bind-rule (first bindings)))
            (cond ((equal (second bind-rule) rhs-term)
                   bind-rule)
                  (t 
                   (rhs-match-p rhs-term (rest bindings))))))))
         

(defun occur-check-p (x term)
  (cond ((null term) nil)
        ((equal x term) t)
        ((atom term) nil)
        ((occur-check-p x (first term))
         t)
        (t
         (occur-check-p x (rest term)))))

(defun lispcall-inst-p (guard bindings-c)
  (cond ((is-t guard)
         (let ((functor (first (s-expr-is guard))))
           (and (or (lisp-function-p functor)
                    (lisp-predicate-p functor))
                (null (collect-variables (rewrite (s-expr-is guard) bindings-c))))))
        (t 
         (let ((functor (first guard)))
           (and (or (lisp-function-p functor)
                    (lisp-predicate-p functor))
                (null (collect-variables (rewrite guard bindings-c))))))
        ))
