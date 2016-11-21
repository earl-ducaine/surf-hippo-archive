(defun rfi-modes (userline) 
  (let ((mode-query (second userline))
        (query-list (rest userline))
        (query-length (length (rest userline)))
        (database *rfi-database*))
    (cond ((eql query-length 1)
           (cond ((one-goal-p mode-query)
                  (compute-modes mode-query database))
                 ((predicate-p mode-query)
                  (rfi-modes (list 'modes (mk-goal mode-query))))
                 (t
                  (progn
                    (princ "Use syntax: modes (cl-name mode1 ... modeN)") (terpri)))))
          (t
           (cond 
            ((predicates-p query-list)
             (rfi-modes (cons 'modes (mk-conjunction query-list))))
            ((conjunction-p query-list)
             (let ((varList (mk-new-var-list query-list 0 nil))
                   (md-query (mk-query query-list))
                   (md-clause nil))
               (setf md-clause (mk-md-Clause query-list varList))
               (compute-modes md-query (cons md-clause database))))
            (t 
             (progn
               (princ "Use syntax: modes (cl-name mode1 ... modeN)") (terpri))))
          ))))

(defun compute-modes (md-query database)
  (let ((ExtTable (mode-interpret md-query database)))
    (pp-extTable ExtTable)
    ExtTable))

(defun one-goal-p (query)
  (cond ((atom query)
         nil)
        ((not (numberp (second query)))
         t)
        (t nil)))

(defun predicate-p (query)
  (cond ((atom query)
         nil)
        ((numberp (second query))
         t)
        (t nil)))
  
(defun conjunction-p (query-list)
  (list-and (mapcar #'one-goal-p query-list)))

(defun predicates-p (query-list)
  (list-and (mapcar #'predicate-p query-list)))

(defun list-and (l)
  (cond ((null l) t)
        (t (and (first l)
                (list-and (rest l))))))

  
(defun mk-conjunction (predicates)
  (mapcar #'mk-goal predicates))

(defun mk-goal (predicate)
  (cons (first predicate)
         (make-list  (second predicate) :initial-element 'dontknow)))

(defun mk-new-var-list (conjunction varnum varlist)
  (cond ((null conjunction) varlist)
        (t 
         (let ((goal (first conjunction)))
           (mk-new-var-list (rest conjunction)
                            (+ varnum (list-length (rest goal)))
                            (append varlist (mk-varList varnum (list-length (rest goal))))))
        )))

(defun mk-varList (start count)
  (cond ((= count 0) nil)
        (t 
         (cons (list 'vari (intern (princ-to-string start)))
               (mk-varList (1+ start) (1- count))))))

            
          
(defun mk-query (conjunction)
  (cons 'md-query (apply #'append (mapcar #'cdr conjunction))))

(defun mk-md-Clause (conjunction varList)
  (let ((cl-tag 'ft)
        (cl-head (cons 'md-query varList))
        (cl-body (project-var conjunction varList)))
  (append (list cl-tag cl-head)
          cl-body)))

(defun project-var (conjunction varList)
  (cond ((null conjunction) nil)
        (t
         (let* ((goal (first conjunction))
                (arglist (project-goal-var (rest goal)
                                               varList)))
           (cons (cons (first goal)
                       argList)
                 (project-var (rest conjunction)
                              (nthcdr (length arglist) varList)))))))

(defun project-goal-var (arglist varList)
  (cond ((null arglist) nil)
        (t 
         (cons (first varList)
               (project-goal-var (rest arglist) (rest varList))))))
       
      
    

(defun pp-extTable (extTable)
  (cond ((null extTable) nil)
        (t
         (let ((proc-name nil);  predicate-name and arity
               (proc-entry (first extTable)))
           (setf proc-name (first proc-entry))
           (cond ((eq (first proc-name) 'md-query)
                  ; the help predicate for handling multi query approximation
                  nil)
                 (t (terpri)
                  (princ "Procedure: ") (princ (first proc-name)) (princ "/") (princ (second proc-name))
                  (princ "  Aliasing: ") (princ (if (s-aliasesBit proc-entry) "yes" "no")) (terpri)
                  (pp-succpat-Table (s-succpat-Table proc-entry)))
                 ))
         (pp-extTable (rest extTable)))
        ))


(defun pp-succpat-Table (succpat-list)
  (cond ((null succpat-list) nil)
        (t
         (princ "calling: ")
         (pp-pattern (first (first succpat-list)))
         (princ " returning: ")
         (pp-pattern (second (first succpat-list)))
         (terpri)
         (pp-succpat-Table (rest succpat-list)))))

(defun pp-pattern (pattern)
  (princ "<")
  (dolist (x pattern)
    (princ x) (princ " "))
  (princ ">"))


(defun lispcall-p (goal)
  (lisp-builtin-p (s-functor goal)))
           
