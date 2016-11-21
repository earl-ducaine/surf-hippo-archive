;-------------------------------------------------------------------------
;
; Partial Evaluator for selected clauses of a meta interpreter
; March 1991
; by Thomas Labisch
; Integration in COLAB
; September 1991
; by Thomas Labisch
;
;-------------------------------------------------------------------------
;
; Call in COLAB:
;
; noautomatic
; replace parteval-db
; replace-strategies parteval-df
;
; lisp (parteval '((hn (forward _Fact _Head)
;                      (clause `(id _Head | _Body))
;                      (findlit _Body _Fact _ToProve)
;                      (provelist _ToProve)
;                      (retain _Head)))
;                '((clause _Head _Body)
;                  (findlit _Body _Fact _ToProve)
;                  (provelist _ToProve)))
;
;------------------------------------------------------------------------

;------------------------------------------------------------------------
;
; Performs the call of the partial evaluator
;
;------------------------------------------------------------------------
(defun parteval (rule lof)
  (parteval1 (car rule) lof)
  t)



(defun parteval1
        (rule lof)
  (or (null rule) 
      (null lof) 
      (let* ((f-rules (pe *fc-strategies* 
                          (append *rule-database*
                                  *factbase*)
                          rule 
                          lof 
                          0)) 
             (mdb2 (throw-out lof (remove rule 
                                          *fc-strategies*
                                          :test #'equal)))
             (f-rules1 (unlevel-vars f-rules)) 
             (f-rules2 (de-inst-heads f-rules1))) 
            (setq *forward-rules* f-rules2)
            (setq *fc-strategies* mdb2))))


;------------------------------------------------------------------------
;
; Unfolds the whole lof from a rule
;
;------------------------------------------------------------------------
(defun pe 
        (mdb wdb rule lof level)
  (and lof
       (let* ((term (car lof))
              (lp (member term rule :test #'pe-match))
              (lop2 (cdr lp))
              (lop1 (set-difference rule lp :test #'pe-match))
              (new-rules (pe1 mdb
                              wdb
                              lop1
                              (list (car lp))
                              lop2
                              '((bottom))
                              level)))
             ;(terpri)(terpri)(terpri) (print new-rules)
             (cond ((null (cdr lof))
                    new-rules)
                   (t
                    (unfold-rules mdb
                                  wdb
                                  new-rules
                                  (cdr lof)
                                  (+ level (length new-rules))))))))



(defun pe-match 
        (x y)
  (and (consp x)
       (consp y)
       (eq (car x) (car y))
       (eql (length x) (length y))))



;------------------------------------------------------------------------
;
; Unfolding until the list lof is empty or a false-term occurs as a premise.
;
;------------------------------------------------------------------------
(defun pe1
        (mdb wdb lop1 lof lop2 env level)
  (let ((term (car lof))) 
       (cond ((null term)
              (list (ultimate-instant (append lop1 lop2) env))) 
             ((eq 'false term) nil)
             ((final-p term)
              (pe1 mdb wdb lop1 (cdr lof) lop2 env level))
             ((final-is-p term)
              (let ((isenv (unify (s-patt-is term) 
                                  (un-inst (s-expr-is term)) 
                                  env)))
                   (and isenv
                        (pe1 mdb 
                             wdb 
                             lop1
                             (cdr lof)
                             lop2
                             isenv 
                             level))))
             ((clause-t term) 
              (let ((env-list (unfold-clause wdb term '((bottom)) level)))
                    (build-rules env-list
                                 (append lop1 lop2)
                                 term)))
             ((ecal-t (un-is term)) ;(terpri) 
              ;(print (ultimate-assoc (un-inst (cadr (un-is term))) env)) 
              (pe1 mdb 
                   wdb 
                   (append lop1 
                           (list (ultimate-assoc (un-inst (cadr (un-is term))) 
                                                 env))) 
                   (cdr lof) 
                   lop2 
                   env 
                   level))
             ((forward-builtin-p (car (un-is term)))
              (pe1 mdb
                   wdb
                   lop1
                   (cons (gen-is term (relfun-exec (un-is term) env))
                         (cdr lof))
                   lop2
                   env
                   level))
             ((lisp-builtin-p (car (un-is term)))
              (pe1 mdb
                   wdb
                   (append lop1 term)
                   (cdr lof)
                   lop2
                   env
                   level))
             (t 
              (let* ((up-new-list (unfold1 mdb term env level))
                     (list-length (length up-new-list)))
               (unfold2 up-new-list
                        mdb
                        wdb
                        lop1
                        (cdr lof)
                        lop2
                        (+ level list-length)))))))


(defun forward-builtin-p (x)
  (member x '(literal-p premise-to-head)))


;------------------------------------------------------------------------
;
; Unfolding of a clause-term
;
;------------------------------------------------------------------------
(defun unfold-clause
        (wdb goal env level)
  (cond ((null wdb) nil)
        (t
         (let* ((assertion (rename-variables (car wdb) (list level)))
                (newenv (clause-unify1 goal assertion env)))
               (cond ((null newenv)
                      (unfold-clause (cdr wdb)
                                     goal
                                     env
                                     level))
                     (t
                      (cons newenv
                            (unfold-clause (cdr wdb)
                                           goal
                                           env
                                           (+ level 1)))))))))



;------------------------------------------------------------------------
;
; Performing unification for clause-terms
;
;------------------------------------------------------------------------
(defun clause-unify1
        (goal assertion environment)
  (let ((new-env (unify (clause-head goal)
                        (list 'inst (s-conclusion assertion))
                        environment)))
       (and new-env
            (unify (clause-body goal)
                   (cons 'tup (s-premises assertion))
                   new-env))))



;------------------------------------------------------------------------
;
; Building rules after unfolding a clause-premise
;
;------------------------------------------------------------------------
(defun build-rules
        (env-list rule unfold-term)
  (mapcar #'(lambda (env) (ultimate-instant (set-difference rule 
                                                            (list unfold-term)  
                                                            :test #'pe-match)
                                            env))
                    env-list))


;------------------------------------------------------------------------
;
; Unfolding of the rules by the list-of-unfolds
;
;------------------------------------------------------------------------
(defun unfold-rules
        (mdb wdb rules list-of-unfolds level)
  (cond ((null rules) nil)
        (t
         (let* ((new-rules (pe mdb wdb (car rules) list-of-unfolds level))
                (rule-length (length new-rules)))
               (append new-rules 
                       (unfold-rules mdb 
                                     wdb 
                                     (cdr rules) 
                                     list-of-unfolds 
                                     (+ level rule-length)))))))


;------------------------------------------------------------------------
;
; Unfolding of one ordinary premise
;
;------------------------------------------------------------------------
(defun unfold1
        (mdb premise env level)
  (cond ((null mdb) nil)
        (t
         (let* ((ass (rename-variables (car mdb) (list level)))
                (newenv (unify (un-inst premise) (s-head ass) env)))
              (cond (newenv
                     (cons (list (s-premises ass) newenv)
                           (unfold1 (cdr mdb)
                                    premise
                                    env
                                    (+ level 1))))
                    (t
                     (unfold1 (cdr mdb)
                              premise
                              env
                              level)))))))





;------------------------------------------------------------------------
;
; Unfolding of the list with the premises and environment
;
;------------------------------------------------------------------------
(defun unfold2
        (up-new-list mdb wdb lop1 lof lop2 level)
  (cond ((null up-new-list) nil)
        (t
         (let* ((upnew (caar up-new-list))
                (env (cadar up-new-list))
                (rulelist (pe1 mdb
                               wdb
                               lop1
                               (append upnew lof)
                               lop2 
                               env
                               level))
                (list-length (length rulelist)))
               (append rulelist
                       (unfold2 (cdr up-new-list)
                                mdb
                                wdb
                                lop1
                                lof
                                lop2
                                (+ level list-length)))))))



;------------------------------------------------------------------------
;
; Removes the no more needed predicates in lof with there rules from
; the meta-database
;
;------------------------------------------------------------------------
(defun throw-out
        (lof mdb)
  (cond ((null mdb) nil)
        (t
         (let ((head (s-conclusion (car mdb))))
              (cond ((member head lof :test #'pe-match)
                     (throw-out lof (cdr mdb)))
                    (t
                     (cons (car mdb)
                           (throw-out lof (cdr mdb)))))))))



;------------------------------------------------------------------------
;
; Deletes the levels from the variables
;
;------------------------------------------------------------------------
(defun unlevel-vars
        (rules)
  (cond ((null rules) nil)
        ((atom rules) rules)
        ((vari-t rules)
         (list 'vari (second rules)))
        (t
         (cons (unlevel-vars (car rules))
               (unlevel-vars (cdr rules))))))




;------------------------------------------------------------------------
;
; Deletes backquotes in all conclusions of the new rules
;
;------------------------------------------------------------------------
(defun de-inst-heads
        (rules)
  (mapcar #'(lambda (rule)
                    (cons (s-kind rule)
                          (cons (de-inst (s-conclusion rule))
                                (s-premises rule))))
          rules))



;------------------------------------------------------------------------
;
; Modified or-process to interpret the clause command
;
;------------------------------------------------------------------------
(defun or-process
  (db-left db-right database terms-left goal environment level silent)
  (cond ((and (null db-left) (null db-right)) nil)
        ((null db-left)
         (or-process (s-first-db db-right)
                     (s-rest-db db-right)
                     database
                     terms-left
                     goal
                     environment
                     level
                     silent))
        (t
           (let ((new-environment
                  (cond ((clause-t (un-is goal))
                         (unify (de-inst (s-clause (un-is goal)))
				(rename-variables (car db-left)
						  (list level))
				environment))
			(t
			 (unify (de-inst (un-is goal))
				(rename-variables (s-conclusion (car db-left))
						  (list level))
				environment)))))
             (cond ((null new-environment)
                    (or-process (cdr db-left)
                                db-right
                                database
                                terms-left
                                goal
                                environment
                                level
                                silent))
                   (t (let* ((premises
                              (cond ((clause-t (un-is goal)) 
				     (list (list 'inst (car db-left))))
                                    (t
                                     (s-premises (car db-left)))))
                             (cutail (member '! premises)))
                        (cond (cutail
                               (let ((guards (ldiff premises cutail)))
                                 (cond ((null guards)
                                        (and-process
                                         (append-is goal
                                                    (car db-left)
                                                    (rename-variables
                                                     (cdr cutail)
                                                     (list level))
                                                    terms-left)
                                         new-environment
                                         database
                                         (1+ level)
                                         silent))
                                       (t (let ((val-env-lev
                                                 (and-process
                                                  (append-guards
                                                   (cdr cutail)
                                                   (car db-left)
                                                   (rename-variables
                                                    guards
                                                    (list level)))
                                                  new-environment
                                                  database
                                                  (1+ level)
                                                  'once)))
                                            (cond (val-env-lev
                                                   (and-process
                                                    (append-is 
                                                     goal
                                                     (car db-left)
                                                     (cons (car val-env-lev)
                                                           (rename-variables
                                                            (cdr cutail)
                                                            (list level)))
                                                     terms-left)
                                                    (cadr val-env-lev)
                                                    database
                                                    (caddr val-env-lev)
                                                    silent))
                                                  (t
                                                   (or-process
                                                    (cdr db-left)
                                                    db-right
                                                    database
                                                    terms-left
                                                    goal
                                                    environment
                                                    level
                                                    silent))))))))
                              ((eq 'tupof silent)
                                 (append
                                  (and-process
                                   (append-is goal
                                              (car db-left)
                                              (rename-variables
                                               premises
                                               (list level))
                                              terms-left)
                                   new-environment
                                   database
                                   (1+ level)
                                   silent)
                                  (or-process (cdr db-left)
                                              db-right
                                              database
                                              terms-left
                                              goal
                                              environment
                                              level
                                              silent)))
                              (t (or
                                  (and-process
                                   (append-is goal
                                              (car db-left)
                                              (rename-variables
                                               premises
                                               (list level))
                                              terms-left)
                                   new-environment
                                   database
                                   (1+ level)
                                   silent)
                                  (or-process (cdr db-left)
                                              db-right
                                              database
                                              terms-left
                                              goal
                                              environment
                                              level
                                              silent)))))))))))



(defun clause-unify
        (goal assertion environment)
  (let ((new-env (unify (clause-head goal)
                        (s-conclusion assertion)
                        environment)))
       (and new-env
            (unify (clause-body goal)
                   (cons 'tup (s-premises assertion))
                   new-env))))



(defun clause-head (term)
  (if (inst-t (cadr term))
      (second (cadadr term))
      (second (cadr term))))




(defun clause-body (term)
  (if (inst-t (cadr term))
      (fourth (cadadr term))
      (fourth (cadr term))))




(defun clause-t (term)
  (and (consp term)
       (eq 'clause (car term))))
      


(defun s-clause (term)
  (cadr term))

(defun append-guards (tail assertion guards)
  (cond ((and (ft-t assertion) (null tail)) guards)
	(t (append guards '(true)))))

