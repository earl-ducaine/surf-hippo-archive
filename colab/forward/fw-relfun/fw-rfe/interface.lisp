(defprocedure retain/1             ;;;; basic version of retain
  (not-r-subsumed 1)
  (push-fact-retain 1)
  (proceed))

(defprocedure open-node/1           ;;; basic version of open-node
  (put_value_temp 1 2)              ;;;;;  Save A1 in A2 ..
  (get-open-node 1) 
  (get_value_temp 1 2)              ;;;;;  unify the Values !
  (proceed))


(defprocedure subsumes-value/1
  (not-r-subsumed 1)
  (put_constant true 1)
  (proceed))

(defprocedure push-fact-retain/1
  (push-fact-retain 1)
  (proceed))

(defprocedure get-open-node/0
  (get-open-node 1) 
  (proceed))

(defprocedure get-actual-node/0
  (actual-node 1) 
  (proceed))



(defprocedure next-open-node/0
  (next-open-node)
  (put_constant true 1)
  (proceed))

(defprocedure not-open-node-at-end/0
  (some-more-open-nodes)
  (put_constant true 1)
  (proceed))

(defprocedure fc-initialize/0
  (enter-retain-stack)
  (put_constant true 1)
  (proceed))

(defprocedure reset-retain/0
  (leave-retain-stack)
  (put_constant true 1)
  (proceed))

(defprocedure collect-facts/2   ;; _result , _inference-pattern
  (allocate 1)
  (get_variable_perm 2 1)  ;;; save _result
  (put_nil 2)
  (reset-ON)               ;;; Register zum Aufsammeln zuruecksetzen
  (collect-retain-facts/2)
  (get_value_perm 2 1)     ;;;  unify _result with A1  
  (proceed))


(defprocedure forward/2          ;;; _fact , _inference
                                 ;;; the new derived _fact is unified with _inf
  (allocate 1)
  (get_variable_perm 1 2)        ;; save _inf in Y1
  (forw-mcall 1 1)               ;; return value= derived-fact !! (A1)
  (get_value_perm 1 1)           ;; unify A1 Y1
  (deallocate)
  (proceed)
  )



  
