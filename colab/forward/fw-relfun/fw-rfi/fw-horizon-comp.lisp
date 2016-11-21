;-------------------------------------------------------------------------
;
; Uebersetzung und Verarbeitung von RELFUN HN-Klauseln in forward Klauseln
;
; -------------------------------------------------------------------------
; urspruengliche Version von Klaus Elsbernd,  Oktober 1990
; Knut Hinkelmann, Juli 1991
; -------------------------------------------------------------------------
 
;;;;;;;;;;; to be declared in relfun 
(defvar *WAM-builtins* nil "Builtins, which are realized as WAM procedures.")
;;;;;;;;;;;


(defun init-fc ()
  (or (member 'push-fact-retain *lisp-extras*)
      (setq *lisp-extras*
            (append '(push-fact-retain   
		      init-retain-stack   
                      get-open-node 
                      get-actual-node
;;;;;;;;;;            get-reached-node
                      collect-facts
                      subsumes-value
                      filter
                      literal-p		
                      premise-to-head)
                      *lisp-extras*)))
  (or (member 'fc-initialize *lisp-predicates*)
      (setq *lisp-predicates*
            (append '(fc-initialize
                      reset-retain
                      next-open-node
                      not-open-node-at-end)
                      *lisp-predicates*)))
  
  (or (member 'push-fact-retain *WAM-builtins*)
      (setq *WAM-builtins*
	    (append '(push-fact-retain
		      get-open-node
                ;;;;; get-actual-node
		      collect-facts
		      subsumes-value
                      filter
		      fc-initialize
		      reset-retain
		      next-open-node
		      not-open-node-at-end))))
  )
 
(init-fc)
 
 
(defun premise-to-head
        (x)
  (cond ((atom x) x)
        ((inst-t x) (cadr x))
        (t (cons (premise-to-head (car x))
                 (premise-to-head (cdr x))))))
 
 
 
(defun literal-p
        (x)
"Tests if a predicate is a rule which should be transformed. This indicates that
 the predicate is not an is-predicate, an atom, a naf-term, a meta-call
 or a builtin"
  (not (or (atom x)
           (vari-t x)
           (ecal-t x)
           (is-t x)
	   (naf-t x)
           (lisp-builtin-p (car x))
           (relfun-builtin-p (car x)))))
 
 
(defun fw-transform-db (db) 
" Collects all horn clauses and generates forward rules."
  (append-list-elements (mapcar-not-nil #'generate-forward 
                                        db)))
 
(defun append-list-elements (list)
"Moves listelements one level up."
  (and list
       (append (car list) (append-list-elements (rest list)))))
 
;(defun rl-or-up? (clause)
;"Returns true, if clause is a rl- or up-clause; otherwhise it returns nil."
;  (cond ((or (eq (car clause) 'rl)
;             (eq (car clause) 'up)) clause)
;        (t nil)))

(defconstant arrow '<- "the sign, to seperate the multiple conclusions from the premisses")

(defun conclusions (clause)
  "Returns all the conclusions of a (multiple) rule (in reverse order)."
  (do* ((ll (cdr clause)(cdr ll))
	(concs (list (car ll)) (cons (car ll) concs))
	)
       ((or (eq (car ll) arrow)
	    (null ll))
	(if (eq (car ll) arrow)
	    (cdr concs)
	  (list (cadr clause))))))

(defun premisses(clause)
  "Returns all the premisses of a (multiple) rule"
  (let ((res (member arrow clause)))
    (if res (cdr res)
      (cddr clause))))
 
(defun generate-forward (clause)
"Takes a clause and generates for almost each premisse one forward-clause.
Clause: ( {up|rl} conc_1 ... conc_N <- premisse_1 ... premisse_N)."
 
  (mapcar-not-nil #'(lambda (x) (generate-rule x 
                                               (conclusions  clause) 
                                               (premisses clause)))
                  (premisses clause)))
 
(defun generate-rule (predicate head predicates)
" If the predicate is a horn-clause (not an is-primitive or a lisp-builtin function),
  generate one forward rule."
  (if (literal-p predicate)
      (make-forward-rule predicate head (remove predicate predicates))))
 
 
;-------------------------------------------------------------------------
; forward-Klauseln haben als zweites Argument die ehemalige Konklusion
 
(defun make-forward-rule (predicate head predicates)
  "The function takes a predicate and generates the corresponding forward-rule.
(hn (forward (<predicate> _conclusion))
 <predicate>_1 ...  <predicate>_i-1  <predicate>_i+1 ...  <predicate>_N
 {(is _conclusion <conc>)|(member _conclusion (tup <conc>_1 ...<conc>_N))}
 (retain _conclusion)
)"

  (let ((one-conc (= 1 (length head))))
    (append (list 'HN (list 'forward (de-inst predicate) '(VARI conclusion)))
          predicates
          (list
           (if one-conc   ;; only one conclusion
               (list 'is '(VARI conclusion)
                     (list 'inst (car head)))
             (list 'member '(VARI conclusion)
                   (cons 'tup (mapcar #'mk-inst head)))))
           (list (list 'retain '(VARI conclusion))))))
 
 
  
                          
#- :colab (defun mapcar-not-nil (fun l)
"A useful function, which removes nil-results as the return-values from the 
 function fun, which will be called by mapcar using an element of the list l"
    (remove nil (mapcar fun l)))
 
 
;----------------------- splitting and hornifying
 
 
(defun split-rulebase (rulebase)
  "Splits rulebase to *up-rulebase* (up/rl->up)
  and *hn-rulebase* (rl->hn). (rulebases are set! to new values)
  Rules are transformed with rl2up-split / rl2hn-split.
  Rules are appended to the rulebases.
  Predicates are not! inserted in *up-preds*,*backward-preds*."
 
  (let ((split-hn-rules nil)    ;; the splitted rules
        (split-up-rules nil)
        )
    (mapcar #'(lambda(rule)
                (let ((tag (car rule))
                       )
                  (cond ((eq tag 'rl)
                         (setf split-up-rules
                               (append (rl2up-split rule) split-up-rules))
                         (setf split-hn-rules 
                               (append (rl2hn-split rule) split-hn-rules))
                         )
                        
                        ((eq tag 'up)
                         (setf split-up-rules (cons rule split-up-rules))
                         )
                        (t (error "in split-rulebase: unknown type in rulebase")))))
            rulebase)
    (setf *up-rulebase* (reverse split-up-rules))
    (setf *hn-rulebase* (reverse split-hn-rules))
    ))
    
    
(defun rl2hn-split(rule)
  "Transforms the rule(RL <conc1> .. <concN> -> <premises>) to
the corresponding horn rules."
  (hornify-rule rule 'hn))
 
(defun rl2up-split(rule)
  "Transforms the rule(RL <conc1> .. <concN> -> <premises>) to
the corresponding up-rules."
  (list (cons 'up (cdr rule ))))
 
 
(defun hornify-rulebase(rulebase dest-tag)
  "Hornifies the rulebase with dest-tag. "
  (append-list-elements (mapcar #'(lambda(rule)
				    (hornify-rule rule dest-tag)) rulebase)))
 
 
    
(defun hornify-rule (rule dest-tag)
  "For each conclusion of the rule, a seperate rule with <dest-tag> is generated.
If <dest-tag> is nil then there is no change of the tag."
  (let ( (tag (if dest-tag
                  dest-tag
                (car rule)))
         (conclusions (conclusions rule))
         (premisses (premisses rule))
         )
    (if (= (length conclusions) 1 )        ;;; only one conclusion 
        (list (append `(,tag ,(car conclusions))
                      premisses))
                                        ;;; several conclusions
      (mapcar #'(lambda(conc) (append `(,tag ,conc ) premisses))
              conclusions))))
 
 
(defun pred_rule(rule)
  "Returns the list with the predicates of <rule>"
  (if (atom (caadr rule))
      (list (caadr rule))
    (mapcar #'car (cadr rule))))
 
 
;; initialisation of retain , in inter and emul mode
(defun init-retain-stack()
  (init-retain-stack-inter)
  (init-retain-system)
  t
  )

  
 
 
 
 

