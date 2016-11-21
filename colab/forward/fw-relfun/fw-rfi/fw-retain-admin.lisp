;-------------------------------------------------------------------------
;
; Verwaltung des RETAIN-Stacks, der die waehrend der Vorwartsverarbeitung
; abgeleiteten Klauseln enthaelt
;
; -------------------------------------------------------------------------
; urspruengliche Version von Klaus Elsbernd,  Oktober 1990
; Knut Hinkelmann, Juli 1991
;
; Knut Hinkelmann, September 1991
; Rekursive Aufrufe der Vorwaertsverarbeitung sind moeglich.
; -------------------------------------------------------------------------




(defvar *retain-stack*    nil  "retain-stack for forward chaining     ")
(defvar *fc-on*               -1   "points to an open node in the retain-stack")
(defvar *fc-an*               -1   "points to an actual node in the retain-stack")
(defvar *fc-rn*               -1   "points to an open node in the retain-stack")

; Die Variable *retain-stack-history* verwaltet vorherige Aufrufe des
; Vorwaertsverarbeitung. Sie wird durch fc-initialize gesetzt.
(defvar *retain-stack-history* '((-1 -1 -1 nil)) "List of triples, which are previous values of *fc-on*, *fc-an*, *fc-rn*, and *retain-stack*")




(defun not-reached-e (x)
"nur dann ein Relfun-atom, wenn nicht nil und constant"
  (cond ((member x *retain-stack* :test #'equal) nil)
        (t t)))

(defun subsumes-value (rule)
"Tests if their is a clause in the *retain-stack* which will subsume rule.
 If one clause is found, nil will be returned, otherwhise true."
  (let ((subs t))
    (do ((rs 0 (1+ rs)))
        ((or (> rs *fc-an*) (not subs)))
      (cond ((subsumes (nth rs *retain-stack*) rule (list '(1 1)))
             (setq subs nil)))
     )
    (if subs 'true nil)
  )
)

(defun subsumes (term1 term2 environment)
  "Tests if term1 subsumes term2. Its almost the same function than the relfun-unify
 function. Only the case is removed, that term2 is a variable which will unify
 with an expression of term1."
  (let* ((x (ultimate-assoc term1 environment))
         (y (ultimate-assoc term2 environment))
         )
    (cond ((equal x y) environment)
          ((anonymous-p x) environment)
          ((vari-t x)
           ; Damit eine Variable in x nicht an zwei verschiedene
           ; Variablen in y gebunden werden kann, werden Variablen in
           ; vor der Bindung mit eindeutigen Konstanten instantiiert.
           (cond ((anonymous-p y)
                  (cons (list x (gentemp "var")) environment))
                 ((vari-t y)
                  (let ((new-constant (gentemp "var")))
                    (cons (list x new-constant)
                          (cons (list y new-constant) environment))))
			  (t (cons (list x y) environment))))
          ((anonymous-p y) nil)
          ((or (atom x) (atom y)) nil)
          (t
           (let ((new-environment (subsumes (car x) (car y) environment)))
             (and new-environment
                  (subsumes (cdr x) (cdr y) new-environment))))
          )
    )
  )


; Funktionen zur Verwaltung des Retain Stacks

(defun fc-initialize nil
"fc-initialize restores the initial values of the pointers of the
retain-stack actual, open and reached node.
The retain-stack itself is restored, too."
  (setq *retain-stack-history*
	(cons (list *fc-on* *fc-an* *fc-rn* *retain-stack*)
	      *retain-stack-history*))
  (setq *fc-on* -1)
  (setq *fc-an* -1)
  (setq *fc-rn* -1)
  (setq *retain-stack* nil)
   t
)

(defun push-fact-retain (fact)
"The fact is appended at the right side of the retain-stack, which means at the 
 top of the stack. The actual node pointer is incremented."
  (setq *retain-stack* (nconc *retain-stack* (list fact)))
  (setq *fc-an* (1+ *fc-an*))
  (setq *fc-rn* (1+ *fc-rn*))
  t
)

(defun unique-variables (literal)
  (car (make-unique-vars literal nil)))

(defun make-unique-vars (literal vars)
  (cond ((atom literal) (cons literal vars))
	((vari-t literal)
         (let ((new-name (assoc literal vars :test #'equal)))
	   (if new-name
	       (cons (cdr new-name) vars)
	       (let ((new-var (make-new-var literal)))
		 (cons new-var (cons (cons literal new-var) vars))))))
	(t (let* ((subst-car (make-unique-vars (car literal) vars))
		  (subst-cdr (make-unique-vars (cdr literal) (cdr subst-car))))
             (cons (cons (car subst-car) (car subst-cdr))
		   (cdr subst-cdr))))))

(defun make-new-var (variable)
  (cons (car variable)
        (cons (gentemp (princ-to-string (cadr variable)))
              (cddr variable))))

(defun next-open-node nil
"The pointer to the open noce is incremented"
   (setq *fc-on* (1+ *fc-on*))
   t
)
  

(defun get-actual-node nil
"Fetches the actual node pointed by '*fc-an'."
   (nth *fc-an* *retain-stack*)
)

(defun get-open-node nil
"Fetches the open node pointed by '*fc-on'."
   (nth *fc-on* *retain-stack*)
)

(defun not-open-node-at-end nil
   (> *fc-an* *fc-on*)
)

(defun get-reached-node nil
"Fetches the reached node pointed by '*fc-rn'."
   (nth *fc-rn* *retain-stack*)
)

(defun collect-facts nil
  (cons 'tup *retain-stack*)
)

(defun filter (pattern list)
"Returns a tup-list with elements unifiable with pattern (no bindings are made)"
  (let ((list-withoput-tups (cdr list)))
   (cons
    'tup
    (mapcar-not-nil #'(lambda (x) (apply-filter pattern x))
		    list-withoput-tups)
   )
  )
)

(defun apply-filter (pattern structure)
  (if (unify pattern structure '((bottom)))
      structure
      nil)
)

(defun init-retain-stack-inter nil
  (setq *fc-on* -1)
  (setq *fc-an* -1)
  (setq *fc-rn* -1)
  (setq *retain-stack* nil)
  (setq *retain-stack-history* nil))

(init-retain-stack-inter)

(defun reset-retain nil
  (let ((previous (if (null *retain-stack-history*)
		      '(-1 -1 -1 *retain-stack*)
		      (car *retain-stack-history*))))
    (if (not (null *retain-stack-history*))
	(setq *retain-stack-history* (cdr *retain-stack-history*)))
    (setq *fc-on* (first previous))
    (setq *fc-an* (second previous))
    (setq *fc-rn* (third previous))
    (setq *retain-stack* (fourth previous))
    t))
