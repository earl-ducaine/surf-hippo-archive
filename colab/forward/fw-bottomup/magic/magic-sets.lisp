;-------------------------------------------------------------------------
;
; Magic sets transformation and evaluation
; August 1991
; by Thomas Labisch
;
; Integration in COLAB
; September 1991
; by Thomas Labisch
;
; Extended for dynamic constants and multiple seeds as arguments
; January 1992
; by Thomas Labisch
;
; Handling multiple conclusions by hornifying the rules
; by Thomas Labisch
; Juni 1992
;
;-------------------------------------------------------------------------
(defun forward-cmd-magiceval (userline)
  "Command to perform both, transformation and evaluation"
  (let ((adseeds (forward-cmd-magictrans userline)))
       (forward-cmd-magicsemi1 adseeds t)))
 
(defun forward-cmd-magictrans (userline)
  "Command to perform transformation"
  (if *up-rulebase*
      (hornify-rulebase *up-rulebase* nil)
      (if *automatic-mode*
	  (progn (colab-splitrules-forward nil)
		 (hornify-rulebase *up-rulebase* nil))
	  (progn (princ "No rules for evaluation")
		 (terpri))))
  (let ((seeds userline)
	(idb *up-rulebase*))
       (and idb
            (magic-sets seeds idb))))
 
(defun forward-cmd-magicsemi (userline)
  "Command to perform evaluation without having done transformation
  right before"
  (forward-cmd-magicsemi1 userline nil))
 
(defun forward-cmd-magicsemi1 (seeds flag)
  "Doing the bottom-up evaluation wrt to the given seeds and the flag. 
  If we do not have any given seeds the evaluation is done with all known 
  magic seeds. If the flag is true, we take the given seeds for evaluation 
  otherwise we extract the appliable seeds for evaluation."
  (let ((*print-case* :downcase))
       (put-facts (cond ((null seeds)
                         (magic-semi *magic-seeds*)
                         (get 'adorn 'seeds))
                        (t
                         (cond (flag 
                                (magic-semi (mod-db (magic-seeds1 
                                                     (rem-seeds seeds))))
                                seeds)
                               (t
                                (let* ((adseeds (ret-ad-seeds seeds nil))
                                       (adseeds1 (proove-ad adseeds))
                                       (adseeds2 (rem-seeds adseeds1))
                                       (mseeds (mod-db 
                                                (magic-seeds1 adseeds2))))
                                      (magic-semi mseeds)
                                      adseeds1))))))))
 
(defun ret-ad-seeds (seeds bounds)
  "Returns for given seeds the adorned seeds wrt bound variables in earlier
  seeds."
  (and seeds
       (let* ((seed (car seeds))
              (args (cdr seed))
              (newseed (cons (list (car seed)
                                   (ret-ad args bounds))
                             args))
              (newbounds (union (search-variables args)
                                bounds
                                :test #'equal)))
             (cons newseed
                   (ret-ad-seeds (cdr seeds)
                                 newbounds)))))
 
(defun proove-ad (seeds)
  "Extracts the seeds for evaluation. If a seed contains a dynamic argument
  or the transformation has not already been done, it is thrown away with
  a remark to the user"
  (and seeds
       (let ((seed (car seeds))
             (adseeds (proove-ad (cdr seeds))))
            (cond ((member seed
                           (get 'adorn 'seeds) 
                           :test #'munify)
                   (cons seed adseeds))
                  ((dynamic-seed-p (cdr seed))
                   (princ "ERROR: The seed ")
                   (forward-cmd-l nil 
                                  (list (list 'hn
                                              (cons (caar seed)
                                                    (cdr seed)))))
                   (princ "       has a dynamic constant as an argument.")
                   adseeds)
                  ((member seed
                           (get 'adorn 'dynamics)
                           :test #'munify)
                   (putpropp 'adorn
                             (cons seed
                                   (get 'adorn 'seeds))
                             'seeds)
                   (setq *magic-seeds*
                         (cons (car (mod-db (magic-seeds1 (list seed))))
                               *magic-seeds*))
                   (cons seed adseeds))
                  (t 
                   (princ "The seed ")
                   (terpri)
                   (forward-cmd-l nil 
                                  (list (list 'hn 
                                               (cons (caar seed)
                                                     (cdr seed)))))
                   (princ "did not cause a transformation")
                   (terpri)
                   adseeds)))))
 
(defun munify (x y)
  (unify-seed (rename-variables x '(1)) 
              y 
              '((bottom))))
 
(defun unify-seed (x y environment)
  (let ((x (ultimate-assoc x environment))
        (y (ultimate-assoc y environment)))
       (cond ((equal x y) environment)
             ((or (anonymous-p x)
                  (anonymous-p y))
              environment)
             ((vari-t x) (cons (list x y) environment))
             ((vari-t y) (cons (list y x) environment))
             ((dynamic-arg-p y) (if (ground-p x)
                                    (cons (list y x) environment)))
             ((dynamic-arg-p x) (if (ground-p y)
                                          (cons (list x y) environment)))
             ((or (atom x) (atom y)) nil)
             (t
              (let ((new-environment (unify-seed (car x)
                                                 (car y)
                                                 environment)))
                   (and new-environment
                        (unify-seed-args (cdr x) (cdr y) new-environment)))))))
 
(defun unify-seed-args (x y environment)
  (let ((x (ultimate-assoc x environment))
        (y (ultimate-assoc y environment)))
       (cond ((and (null x) (null y)) environment)
             ((or (anonymous-p x)
                  (anonymous-p y))
              environment)
             ((vari-t x) (cons (list x y) environment))
             ((vari-t y) (cons (list y x) environment))
             ((dynamic-arg-p y) (if (ground-p x)
                                    (cons (list y x) environment)))
             ((dynamic-arg-p x) (if (ground-p y)
                                    (cons (list x y) environment)))
             ((and (bar-t x) (bar-t y))              
              (unify (cadr x) (cadr y) environment))
             ((bar-t x)                             
              (unify-seed (cadr x) (cons 'tup y) environment))
             ((bar-t y)
              (unify-seed (cons 'tup x) (cadr y) environment))
             ((or (null x) (null y)) nil)
             ((or (atom x) (atom y)) nil)
             (t
              (let ((new-environment (unify-seed (car x)
                                                 (car y)
                                                 environment)))
                   (and new-environment
                        (unify-seed-args (cdr x) (cdr y) new-environment)))))))
 
(defun rename-seed-variables (term listified-level)
  (cond ((vari-t term) (append term listified-level))
        ((dynamic-arg-p term) (cons term listified-level))
        ((atom term) term)
        (t
         (cons (rename-variables (car term) listified-level)
               (rename-variables (cdr term) listified-level)))))
 
 
;-------------------------------------------------------------------------
;
; main function for magic sets evaluation
;
;-------------------------------------------------------------------------
(defun magic-semi (mseeds)
  "Starts the bottom-up evaluation with the semi-naive strategy for the 
  magic rules, given seeds or magic seeds and all facts."
  (cond ((or (null mseeds)
             (null *magic-rules*)
             (null *factbase*)) 
         (terpri)
         (princ "ERROR: Either no seeds, magic rules or facts")
         (terpri)
         (princ "       are given for bottom-up evaluation."))
        (t
         (let ((edb (mapcar #'cadr (union mseeds
                                          (append *factbase*
                                                  *derived-factbase*)
                                          :test #'equal)))
               (idb (mapcar #'cdr *magic-rules*)))
              (forward-cmd-semi edb idb)))))
 
;-------------------------------------------------------------------------
;
; main function for magic sets transformation
;
;-------------------------------------------------------------------------
(defun magic-sets (seeds idb)
  "First this function computes the adorned seeds, then tests whether some
  seeds had already caused a transformation. These seeds are not needed
  for another transformation. Wrt the remaining seeds the fact-database,
  the magic-seeds and the magic-rules are updated. In trace mode the
  adorned and magic database are shown to the user. The value of this
  function is the list of adorned seeds computed in the first step."
  (let* ((adseeds (ret-ad-seeds seeds nil))
         (remseeds (proove-ad-trans adseeds))
         (mdb (if (or remseeds
                      (and (null remseeds)
                           (> (length adseeds) 1)))
                  (let ((add (do-ad remseeds idb)))
                       (if *fw-spy*
                           (progn
                            (terpri)
                            (princ "Adorned Database :")
                            (terpri)
                            (forward-cmd-l nil (mod-db add))
                            (terpri)))
                       (setq *derived-factbase* 
                             (union *derived-factbase*
                                    (mod-db (do-ad-edb *factbase*))
                                    :test #'equal))
                       (if (> (length adseeds) 1)
                           (let* ((s-r (gen-query adseeds))
                                  (ms (first s-r))
                                  (mr (second s-r)))
                                 (append (setq *magic-seeds* 
                                               (union (mod-db ms)
                                                      *magic-seeds*
                                                      :test #'equal))
                                         (setq *magic-rules* 
                                               (union (mod-db 
                                                       (append 
                                                        (magic-rules add)
                                                        mr
                                                        (mod-adorned add)))
                                                      *magic-rules*
                                                      :test #'equal))))
                           (append (setq *magic-seeds* 
                                         (union (mod-db (magic-seeds))
                                                *magic-seeds*
                                                :test #'equal))
                                   (setq *magic-rules* 
                                         (union (mod-db (append 
                                                         (magic-rules add)
                                                         (mod-adorned add)))
                                                *magic-rules*
                                                :test #'equal)))))
                  (append *magic-seeds* *magic-rules*))))
        (if *fw-spy*
            (progn (terpri)
                   (princ "Magic Database :")
                   (terpri)
                   (forward-cmd-l nil mdb)
                   (terpri)))
        adseeds))
 
(defun proove-ad-trans (seeds)
  "If an adorned seed occurs in the list of the adorned seeds or
  adorned dynamics, it is thrown away, i.e. the transformation for it has
  already been done."
  (and seeds
       (let ((seed (car seeds))
             (adseeds (proove-ad-trans (cdr seeds))))
            (cond ((member seed
                           (append (get 'adorn 'seeds)
                                   (get 'adorn 'dynamics))
                           :test #'munify)
                   adseeds)
                  (t
                   (cons seed adseeds))))))
 
(defun gen-query (seeds)
  "For more than one seed given in the transformation a pseudo-query is 
  generated to guarantee the derivation of all possible magic seeds wrt.
  the former given seeds in the transformation."
  (let* ((args (collect-fw-vars seeds))
         (head (cons  (list 'query 
                            (gen-ad args)) 
                      args))
         (newrule (new-rule head 
                            seeds))
         (mrules (magic-rule1 (cdr newrule)
                              'rl
                              newrule))
         (new-rules (del-mag-query mrules))
         (m-edb (mapcar-not-nil #'(lambda (x)
                                          (if (cddr x)
                                              nil
                                              (cons 'hn 
                                                    (cdr x))))
                                new-rules))
         (m-idb (mapcar-not-nil #'(lambda (x)
                                          (if (cddr x)
                                              x
                                              nil))
                                new-rules)))
        (list m-edb
              m-idb)))
 
(defun collect-fw-vars (seeds)
  "All the variables of the seeds are collected. They are intended to be the
  arguments of the head of the pseudo-query."
  (and seeds
       (let* ((seed (car seeds))
              (actvars (search-variables (cdr seed)))
              (vars (collect-fw-vars (cdr seeds))))
             (union actvars 
                    vars
                    :test #'equal))))
 
(defun gen-ad (args)
  "An adornment in form of a list (f ... f) is generated for the head of the 
  pseudo-query with the length of the list of args."
  (mapcar #'(lambda (x) 'f) args))
 
(defun del-mag-query (rules)
  "The premise magic.query-f...f is removed from all the generated rules
  which are generated from the seeds."
  (and rules
       (cons (del-one-mag-query (car rules))
             (del-mag-query (cdr rules)))))
 
(defun del-one-mag-query (rule)
  "Same as above restricted to one rule."
  (let ((tag (first rule))
        (head (second rule))
        (body (cdddr rule)))
       (cons tag
             (cons head
                   body))))
 
;-------------------------------------------------------------------------
;
; Building the adorned database w.r.t the given seeds
;
;-------------------------------------------------------------------------
(defun do-ad (adseeds idb)
  "Gives the ordinary and dynamic seeds to property lists and starts for 
  all seeds the process of the adornment."
  (let* ((a-d (check-seeds adseeds))
         (aseeds (first a-d))
         (dseeds (second a-d)))
        (if aseeds
            (putpropp 'adorn
                      (append aseeds
                              (get 'adorn 'seeds))
                      'seeds))
        (if dseeds
            (putpropp 'adorn
                      (append dseeds
                              (get 'adorn 'dynamics))
                      'dynamics))
        (do-ad1 (append aseeds dseeds) 
                idb 
                (ret-rp idb)
                nil)))
 
(defun check-seeds (seeds)
  "Divides seeds with dynamic constants from ordinary seeds."
  (and seeds
       (let ((seed (car seeds))
             (a-d (check-seeds (cdr seeds))))
            (cond ((dynamic-seed-p (cdr seed))
                   (list (first a-d)
                         (cons seed (second a-d))))
                  (t
                   (list (cons seed (first a-d))
                         (second a-d)))))))
 
(defun dynamic-seed-p (list)
  "Tests whether an argument list contains a dynamic constant."
  (and list
       (if (atom list)
           (dynamic-arg-p list)
           (or (dynamic-arg-p (car list))
               (dynamic-arg-p (cdr list))))))
 
(defun dynamic-arg-p (x)
  "Tests whether an argument is a dynamic constant."
  (eq (char (princ-to-string x) 0)
      #\$))
 
(defun ret-rp (idb)
  "Returns the recursive predicates of the intensional database."
  (remove-duplicates (mapcar #'caadr idb) :test #'equal))
  
(defun do-ad1 (adseeds db rp bounds)
  "Performs for all adorned seeds the process of adornment. The first seed
  is chosen for the adornment step."
  (and adseeds
       (let* ((adseed (car adseeds))
              (seed (cons (caar adseed)
                          (cdr adseed)))
              (ad (ret-ad (cdr adseed) bounds))
              (newbounds (union (search-variables (cdr adseed))
                                bounds
                                :test #'equal))
              (new-rules (change-rules seed
                                      ad
                                      db
                                      rp)))
             (append new-rules 
                     (do-ad1 (cdr adseeds) 
                             db 
                             rp
                             newbounds)))))
    
(defun change-rules (seed ad db rp)
  "For one seed all corresponding rules in the idb are changed, if this seed
  has not already been changed."
  (let ((alr (get 'adorn 'alr)))
    (cond ((member (list (car seed) ad) alr :test #'equal)
           (look-yet db rp))
          (t 
           (append (change-rules1 seed ad db rp)
                   (look-yet db rp))))))
 
(defun look-yet (db rp)
  "If there are adornments, which do not have changed the idb, they are now 
  treated. The first one is chosen and removed from the list of to be
  treated adornments." 
  (let ((yet (get 'adorn 'yet))) 
    (cond ((null yet) nil)
          (t
           (putpropp 'adorn (cdr yet) 'yet)
           (append (change-rules1 (cons (first (car yet))
                                        (gen-args (second (car yet))))
                                  (second (car yet))
                                  db
                                  rp)
                   (look-yet db rp))))))
 
(defun gen-args (ad)
  "Arguments in form of variables are generated for the adornment of a
  predicate."
  (and ad
       (cons (list 'vari (gensym))
             (gen-args (cdr ad)))))
             
 
(defun change-rules1 (seed ad db rp)
  "If the whole idb was changed, the seed is added to the already treated
  seeds. Otherwise another clause of the idb is changed, if its head unifies
  with the given seed. The result of this changing is append to the changes
  of the remaining idb."
  (cond ((null db)
         (putpropp 'adorn 
                   (union (list (list (car seed) ad)) 
                          (get 'adorn 'alr) :test #'equal)
                   'alr)
         nil)
        (t       
         (let ((newenv (unify seed (s-conclusion (car db)) '((bottom)))))
           (append (and newenv
                        (list (change-rule seed ad (car db) rp)))
                   (change-rules1 seed ad (cdr db) rp))))))
 
(defun change-rule (seed ad rule rp)
  "A complete rule is changed. The kind of the rule, the changed head and the
  changed body build the new adorned rule."
  (let ((pred-bounds (change-head (s-conclusion rule) ad))) 
    (append (cons (s-kind rule) 
                  (first pred-bounds))
            (change-body (list (car seed) ad)
                         (s-premises rule) 
                         (second pred-bounds) 
                         rp))))
          
(defun change-head (head ad)
  "The adornment of the seed is added to the head of the rule"
  (list (list (cons (list (car head) ad) (cdr head)))
        (search-bound-variables ad (cdr head))))
        
(defun search-bound-variables (ad args)
  "All bound variables wrt the adornment are serched."
  (and ad
       (cond ((and (vari-t (car args))
                   (eq (car ad) 'b))
              (cons (car args)
                    (search-bound-variables (cdr ad) (cdr args))))
             ((and (consp (car args))
                   (eq (car ad) 'b))
              (append (search-variables (car args))
                      (search-bound-variables (cdr ad) (cdr args))))
             (t
              (search-bound-variables (cdr ad) (cdr args))))))
 
(defun change-body (act premises bounds rp)
  "Changes the body of a rule. If the current premise is a recursive predicate
  it is changed, the new bound variables are collected and if it is not in 
  the set of the already adorned preds, it is added to the preds not yet
  adorned. If it is not a recursive pred, only the new bounds are collected."
  (and premises
       (let ((premise (car premises)))
         (cond ((member (car premise) rp :test #'equal)
                (let ((pred-bounds (change-premise premise bounds)))
                  (cond ((member (car (first pred-bounds))
                                 (cons act (get 'adorn 'alr))
                                 :test #'equal))
                        (t
                         (putpropp 'adorn
                                   (cons (car (first pred-bounds))
                                         (get 'adorn 'yet))
                                   'yet)))
                  (cons (first pred-bounds)
                        (change-body act
                                     (cdr premises)
                                     (second pred-bounds)
                                     rp)))) 
               (t 
                (let ((newbounds (search-new-bounds (cdr premise) bounds)))
                  (cons premise
                        (change-body act 
                                     (cdr premises) 
                                     newbounds 
                                     rp))))))))
 
 
(defun search-new-bounds (args bounds)
  "All the new bound variables are collected."
  (let* ((vars (search-variables args))
         (sec (search-bounds vars bounds)))
    (cond ((null sec) bounds)
          (t 
           (union vars bounds :test #'equal)))))
           
(defun search-bounds (vars bounds)
  "The already  bound variables are collected."
  (intersection vars bounds :test #'equal))
 
(defun change-premise (premise bounds)
  "To a recursive premise the adornment of it is added and is returned 
  with the new bound variables, which are the entire one if there is 
  no bound argument in the adornment of the premise, otherwise the 
  union of the entire one and teh variables of the premise."
  (let* ((vars (search-variables (cdr premise)))
         (ad (ret-ad (cdr premise) bounds))
         (newbounds (if (member 'b ad)
                        (union vars bounds :test #'equal)
                        bounds)))
        (list (cons (list (car premise)
                          ad)
                    (cdr premise))            
              newbounds)))
 
(defun search-variables (args)
  "All variables of the arguments are collected."
  (and args
       (cond ((vari-t (car args))
              (cons (car args) 
                    (search-variables (cdr args))))
             ((consp (car args))
              (append (search-variables (car args))
                      (search-variables (cdr args))))
             (t 
              (search-variables (cdr args))))))
 
(defun ret-ad (args bounds)
  "An adornment for given arguments wrt the bound variables is created."
  (and args
       (cons (cond ((or (atom (car args))
                        (member (car args) bounds :test #'equal))
                    'b)
                   ((vari-t (car args))
                    'f)
                   (t
                    (ret-structure-ad (car args) bounds)))
             (ret-ad (cdr args) bounds))))
             
(defun ret-structure-ad (struc bounds)
  "If a structure contains no unbound variable, b is returned, 
  otherwise f."
  (if (ret-structure-ad1 struc bounds) 'b 'f))
 
(defun ret-structure-ad1 (struc bounds)
  "If a structure contains no unbound variable, t is returned, 
  otherwise nil."
  (or (null struc) 
      (cond ((or (atom (car struc))
                 (member (car struc) bounds :test #'equal))
             (ret-structure-ad1 (cdr struc) bounds))
            ((vari-t (car struc)) nil)
            (t
             (and (ret-structure-ad1 (car struc) bounds)
                  (ret-structure-ad1 (cdr struc) bounds))))))
 
(defun do-ad-edb (edb)
  "If there are already facts of recursive predicates used in the 
  transfomation, the edb has to be modified in the way that an 
  adornment is added to those predicates."
  (and edb
       (let* ((fact (car edb))
              (alr (get 'adorn 'alr)))
              (append (do-ad-edb1 fact alr)
                      (do-ad-edb (cdr edb))))))
 
(defun do-ad-edb1 (fact alr)
  (and alr
       (let ((adorned (member (caadr fact) alr :test #'assoc-equal)))
            (and adorned
                 (let ((ad (car adorned)))
                      (if (eql (length (cdadr fact))
                               (length (second ad)))
                          (cons (list 'hn (cons (list (caadr fact)
                                                      (second ad))
                                                (cdadr fact)))
                                (do-ad-edb1 fact (cdr adorned)))
                          (do-ad-edb1 fact (cdr adorned))))))))
 
(defun assoc-equal (x y)
  (equal x (car y)))
 
;-------------------------------------------------------------------------
;
; Building the magic rules from the adorned database
;
;-------------------------------------------------------------------------
(defun magic-rules (db)
  "Starts the generation of magic rules for the adorned database."
  (and db
       (append (magic-rule (car db))
               (magic-rules (cdr db)))))
               
(defun magic-rule (rule)
  "Performs the generation of magic rules from one adorned rule."
  (let* ((tag (s-kind rule))
         (head (s-conclusion rule))
         (new-rule (new-rule head (s-premises rule)))
         (body (cdr new-rule)))
    (magic-rule1 body
                 tag
                 new-rule)))
 
(defun new-rule (head body)
  "Returns a list of the form (head boundvars freevars) ... 
  (premise-n bound-n free-n)."
  (let ((bv (search-bound-variables (cadar head) (cdr head))))
       (cons (list head
                   bv
                   nil)
             (elim body bv))))
 
(defun elim (premises bv)
  "Gives a list as above described  for the body of a rule wrt 
  the bound variables of the head."
  (and premises
       (let* ((premise (car premises))
              (vars (search-variables (cdr premise))))
             (cond ;((member (car premise) arp :test #'equal)
                   ; (cons premise
                ;         (elim1 (cdr premises)
                ;                (cond ((check-args vars bv)
                ;                       (union vars bv :test #'equal))
                ;                      (t
                ;                       bv))
                ;                arp)))
                   (t
                    (cond ((or (check-args vars bv)
                               (const-arg premise))
                           (let* ((bounds (search-bounds vars bv))
                                  (tbounds (set-difference vars
                                                          bounds
                                                          :test #'equal)))
                                 (cons (list premise
                                             bounds
                                             tbounds)
                                       (elim (cdr premises)
                                             (append tbounds bv)))))
                          (t
                           (elim (cdr premises) bv))))))))
 
(defun check-args (vars bounds)
  "True if one of the variables is already bound."
  (and vars
       (or (member (car vars) bounds :test #'equal)
           (check-args (cdr vars) bounds))))
 
(defun const-arg (premise)
  "Looks for a constant in the arguments of a nonrecursive premise.
  Looks for a bound argument of a recursive premise."
  (if (and (consp (car premise))
           (consp (cdar premise)))
      (member 'b (cadar premise))
      (const-arg1 (cdr premise))))
 
(defun const-arg1 (args)
  "Searches a constant in the arguments."
  (and args
       (or (atom (car args))
           (const-arg1 (cdr args)))))
 
(defun ad2mg (literal)
  "Adds a 'magic' to the adorned predicate before the name of it."
  (let ((predname (car literal))
        (ad (cadar literal))
        (args (cdr literal)))
    (cons (cons 'magic 
                predname)
          (bound-args ad args))))
 
(defun bound-args (ad args)
  "Returns the bound arguments wrt the adornment."
  (and ad
       (cond ((eq (car ad) 'b)
              (cons (car args)
                    (bound-args (cdr ad) (cdr args))))
             (t
              (bound-args (cdr ad) (cdr args))))))
 
(defun magic-rule1 (premises tag rule)
  "Generates one after another all magic rules from one rule."
  (and premises
       (let ((rem (search-next premises)))
         (and rem
              (let* ((newprems (cdr rem))
                     (adpred (car rem))
                     (magic-h (make-magic adpred))
                     (bound-vars (second adpred)))
                (append (let* ((rule1 (cdr (member adpred (reverse rule)
                                                   :test #'equal)))
                               (m-b (bottom-union rule1 bound-vars)))
                              (if m-b
                                  (let ((magic-b (make-magic (car m-b)))
                                        (body (cdr m-b)))
                                       (if (or (and (equal magic-b
                                                           magic-h)
                                                    (null body))
                                               (and (null (cdr magic-b))
                                                    (null (cdr magic-h))))
                                           nil
                                           (list (build-magic-rule
                                                  tag
                                                  magic-h
                                                  magic-b
                                                  (mapcar #'car body)))))))
                        (magic-rule1 newprems
                                     tag
                                     rule)))))))
 
(defun search-next (premises)
  "Returns the next recursive premise"
  (and premises
       (cond ((rec-p (car premises))
              premises)
             (t
              (search-next (cdr premises))))))
                      
(defun make-magic (pred)
  "Adds a 'magic' to the name of the predicate."
  (ad2mg (car pred)))
 
(defun bottom-union (rule bounds)
  "Returns the rough structure of one generated body of a rule 
  with the magic premise at the front."
  (let ((res (bottom-union1 rule bounds nil)))
       (if res
           (append (last res)
                   (reverse (cdr (reverse res)))))))
 
(defun bottom-union1 (rule bounds bv)
  "Collects all involved (absolutely needed) premises for generating 
  a magic rule body. This function simulates the search in the sip
  graph."
  (cond ((null rule)
         (print "rule not safe")
         nil)
        (t
         (let* ((premise (car rule))
                (b (second premise))
                (tb (third premise))
                (rec-p (rec-p premise))
                ;detect the common variables in the actually treated 
                ;premise and the given bound variables
                (combv (intersection bounds
                                     (if rec-p
                                         (append b tb)
                                         tb)
                                     :test #'equal)))
               (if combv
                   (let* ((new-bv (union bv combv :test #'equal))
                          (res (set-difference bounds
                                               new-bv
                                               :test #'equal)))
                         (cond ((null res) ;all variables in the 
                                ;later magic head will be bound
                                ;applying evaluation 
                                (if (and rec-p
                                         (null (set-difference 
                                                bounds
                                                (append b bv)
                                                :test #'equal)))
                                    (list premise)
                                    (cons premise
                                          (bottom-union1 (cdr rule)
                                                         b
                                                         nil))))
                               (t
                                (append (if (intersection bounds 
                                                          tb 
                                                          :test #'equal)
                                            (list premise)
                                            nil)
                                        (bottom-union1 (cdr rule)
                                                       bounds
                                                       (union bv
                                                              tb
                                                              :test 
                                                              #'equal))))))
                   (if (consp (cdr rule))
                       (bottom-union1 (cdr rule)
                                      bounds
                                      bv)
                       (list premise)))))))
 
(defun rec-p (premise)
  "True if premise is recursive, here has an adornment."
  (and (consp (caar premise))
       (consp (cdaar premise))))
 
(defun build-magic-rule (tag magic-h magic-b premises)
  "Build a new magic rule where structures in the head are un-insted
  and structures in the body has to be insted."
  (append (list tag
                (de-inst1 magic-h)
                (inst-body-literal magic-b))
          premises))
 
(defun inst-body-literal (magic)
  "Performs the addition of the magic premise with an eventually needed
  'inst."
  (cons (car magic)
        (inst-body-args (cdr magic))))
 
(defun inst-body-args (args)
  "Adds a 'inst to a structure."
  (and args
       (mapcar #'(lambda (arg)
                         (cond ((or (atom arg) 
                                    (inst-t arg)
                                    (vari-t arg)) arg)
                               (t
                                (list 'inst arg))))
               args)))
 
;-------------------------------------------------------------------------
;
; Building of the magic-seeds from the adorned seeds
;
;-------------------------------------------------------------------------
(defun magic-seeds ()
  "Starts the generation of magic seeds."
  (let ((seeds (rem-seeds (get 'adorn 'seeds))))
    (magic-seeds1 seeds)))
    
(defun rem-seeds (seeds)
  "Only the seeds with constant arguments as bound arguments are returned"
  (and seeds
       (let* ((rs (rem-seeds (cdr seeds)))
              (seed (car seeds))
              (res (rem-seed-p (cadar seed) (cdr seed))))
             (if res 
                 (cons seed rs)
                 rs))))
 
(defun rem-seed-p (ad args)
  "Tests whether a bound argument is either an atom or a ground structure.
  Returns t, if all bound arguments satisfy this condition, otherwise nil."
  (cond ((null ad) t)
        (t 
         (if (or (and (eq (car ad) 'b)
                      (ground-p (car args)))
                 (eq (car ad) 'f))
             (rem-seed-p (cdr ad) (cdr args))
             nil))))
 
(defun magic-seeds1 (seeds)
  "Adds a tag and 'magic' to the seeds."
  (and seeds
       (let ((magic (ad2mg (car seeds))))
         (append (list (list 'hn magic)) 
                 (magic-seeds1 (cdr seeds))))))
 
;-------------------------------------------------------------------------
;
; Modifying of the adorned rules with magic predicates
; 
;-------------------------------------------------------------------------
(defun mod-adorned (db)
  "Modifies the adorned database. Allmost in every rule a magic premise 
  is added."
  (and db
       (cons (mod-rule (car db))
             (mod-adorned (cdr db)))))
             
(defun mod-rule (rule)
  "Adds a magic premise to one rule of the adorned database."
  (let* ((tag (s-kind rule))
         (head (s-conclusion rule))
         (body (s-premises rule))
         (magic (ad2mg head)))
    (append (list tag
                  head)
            (append (and (cdr magic)
                         (list (inst-body-literal magic)))
                    body))))
 
;-------------------------------------------------------------------------
;
; Transformation of the list of the adorned or magic predicatename 
; into one symbol
;
;-------------------------------------------------------------------------
(defun mod-db (db)
  (let* ((predlists (get 'adorn 'alr))
         (predlists1 (add-magics predlists)))
    (mod-db1 predlists1 db)))
    
(defun add-magics (predlists)
  (append (mapcar #'(lambda (x) (cons 'magic x)) predlists)
          predlists))
          
(defun mod-db1 (predlists db)
  (cond ((null predlists) db)
        (t
         (let* ((predlist (car predlists))
                (predname (list2symbol predlist))
                (new-db (subst predname predlist db :test #'equal)))
           (mod-db1 (cdr predlists) new-db)))))
           
(defun list2symbol (list)
  (string2symbol (list2string list)))
  
(defun list2string (predlist)
  (concatenate 'string  
               (cond ((atom (second predlist))
                      (concatenate 'string 
                                   (string-downcase (symbol-name 
                                                      (first predlist)))
                                   "."
                                   (string-downcase (symbol-name 
                                                      (second predlist)))))
                     (t
                      (string-downcase (symbol-name (first predlist)))))
               "-"
               (adorn (car (last predlist)))))
                  
(defun adorn (la)
  (concatenate 'string 
               (string-downcase (symbol-name (car la)))
               (cond ((null (cdr la)) "")
                     (t (adorn (cdr la)))))) 
 
(defun string2symbol (string)
  (read-from-string (princ-to-string (intern string))))
       
;-------------------------------------------------------------------------
;
; Removing the property-lists
;
;-------------------------------------------------------------------------
(defun remprops ()
  (remprop 'adorn 'yet)
  (remprop 'adorn 'alr)
  (remprop 'adorn 'dynamics)
  (remprop 'adorn 'seeds))
 
;-------------------------------------------------------------------------
;
;
;
;-------------------------------------------------------------------------
(defun putpropp (object value property)
  (setf (get object property) value))
 
(defun put-facts (adseeds)
  (and adseeds
       (princ (put-facts1 adseeds)))
  (values))
 
(defun put-facts1 (adseeds)
  ;(setq *rfi-spy* t)
  (let* ((seeds (mod-db adseeds))
         (goals (read-from-string (format nil
                                         "~A"
                                         (append seeds
                                                 (list (list 'inst seeds))))))
         (res (and-process (deanon-request
                            (if *rfi-static*
                                (flatten-request goals)
                                goals))
                           '((bottom))
                           (list *factbase*
                                 *derived-factbase*
                                 *fw-prelude*)
                           1
                           'tupof)))
        ;(setq *rfi-spy* nil)
        (subst-preds adseeds seeds res)))
 
(defun subst-preds (adseeds seeds res)
  (cond ((null seeds) res)
        (t
         (let* ((predname (caaar adseeds))
                (mod-predname (caar seeds))
                (new-res (subst predname mod-predname res :test #'equal)))
               (subst-preds (cdr adseeds) (cdr seeds) new-res)))))
 
