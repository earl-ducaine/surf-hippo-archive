(gasm title "forward interface"
      author "Martin Harm"
      date "July 1992"
      comment "the interface of: retain-stack and forward-code-area"
      code (

      (.module forward-system)
      (.import-module prelude)
      (.import-module system)


      .proc
subsumes-value/1
      (not-r-subsumed 1)
      (put_constant true 1)
      (proceed)

      .proc
subsumes/2
      (subsumes 1 2)
      (proceed)
      
      .proc 
filter-aux/2
      (try_me_else 0 2)
      (get_nil 2)
      (put_nil 1)
      (proceed)
      0
      (retry_me_else 1 2)
      (allocate 3)
      (get_variable_perm 3 1)
      (get_list 2)
      (unify_variable_perm 1)
      (unify_variable_perm 2)
      (put_value_perm 1 2)
      (call subsumes/2 3)
      (put_value_perm 3 1)
      (put_value_perm 2 2)
      (call filter-aux/2 1)
      (get_variable_temp 2 1)
      (put_value_perm 1 1)
      (deallocate)
      (execute cns/2)
      1
      (trust_me_else_fail 2)
      (get_list 2)
      (unify_variable_temp 3)
      (unify_variable_temp 4)
      (put_value_temp 4 2)
      (execute filter-aux/2)

      .proc
filter/2  ;;; a once of filter-aux/2
      (allocate 0)
      (save_cut_pointer)
      (call filter-aux/2 0)
      (last_cut)
      (deallocate)
      (proceed)
      
      .proc
collect-facts/0
      (begin-of-retain-stack 1) ;; in A1 
      (proceed)

      .proc
push-fact-retain/1
      (push-fact-retain 1)
      (proceed)

      .proc
get-open-node/0
      (get-open-node 1) 
      (proceed)

      .proc
get-actual-node/0
     (actual-node 1) 
     (proceed)

     .proc
next-open-node/0
     (next-open-node)
     (put_constant true 1)
     (proceed)
     
     .proc
not-open-node-at-end/0
     (some-more-open-nodes)
     (put_constant true 1)
     (proceed)

     .proc
fc-initialize/0
     (enter-retain-stack)
     (put_constant true 1)
     (proceed)

     .proc
reset-retain/0
     (leave-retain-stack)
     (put_constant true 1)
     (proceed)
      
     .proc
forward/2          ;;; _fact , _inference
                   ;;; the form _fact derived <new_fact> is unified with _inf
     (allocate 1)
     (get_variable_perm 1 2)          ;; save _inf in Y1
     (put_constant forward-code 3)   ;; the module to be called
     (mcall/mod  1 1 3)               ;; return value= derived-fact !! (A1)
     (get_value_perm 1 1)             ;; unify A1 Y1
     (deallocate)
     (proceed)
))
