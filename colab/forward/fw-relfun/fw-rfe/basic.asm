(gasm title "forward basic"
      author "Martin Harm"
      date "July 1992"
      comment "the clauses retain and open-node"
      code (

      (.module user)
      (.import-module forward-system)
      (.module user)
      .dynamic


      .proc
retain/1             ;;;; basic version of retain
      (not-r-subsumed 1)
      (push-fact-retain 1)
      (proceed)
      
      .proc
open-node/1           ;;; basic version of open-node
      (put_value_temp 1 2)              ;;;;;  Save A1 in A2 ..
      (get-open-node 1) 
      (get_value_temp 1 2)              ;;;;;  unify the Values !
      (proceed)

))
