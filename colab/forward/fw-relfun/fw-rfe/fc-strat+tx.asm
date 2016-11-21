;;; The strategies ... as direct WAM programs.
;;; Compilation of tx-additions.rf 
;;; --> diff to fc-strategies only open-node (+ open-node-aux/1), retain
;;;     new the data, data-attributes, tx-unify
;;; ( uses the GNU-emacs folding-mode)

(gasm author "Knut Hinkelman (source) Martin Harm (optimization)"
      comment "strategies compiled and optimized "
      code (
      (.module user)
      (.import-module forward-system)
      (.import-module prelude)
      (.import-module system)

      .dynamic
 
;;{{{ data/3

  .proc
  data/3
  (allocate 2)
  (get_variable_perm 2 2)
  (get_variable_perm 1 3)
  (get_variable_temp 2 1)
  (put_constant instances 1)
  (cl-func tx 2)
  (get_variable_temp 2 1)
  (put_value_perm 2 1)
  (call member/2 2)
  (put_value_perm 2 1)
  (put_value_perm 1 2)
  (call data-attributes/2 1)
  (get_value_perm 1 1)
  (put_constant true 1)
  (deallocate)
  (proceed)

;;}}}

;;{{{ data-attributes/2

  .proc
  data-attributes/2
  (try_me_else 0 2)
  (get_nil 2)
  (put_constant true 1)
  (proceed)
  0
  (retry_me_else 1 2)
  (allocate 2)
  (get_variable_temp 3 1)
  (put_list 4)
  (unify_variable_perm 2)
  (unify_nil)
  (put_list 5)
  (unify_variable_perm 1)
  (unify_value_temp 4)
  (get_list 2)
  (unify_value_temp 5)
  (unify_nil)
  (put_value_perm 1 2)
  (put_constant attr-filler 1)
  (cl-func tx 3)
  (get_value_perm 2 1)
  (put_list 2)
  (unify_value_temp 1)
  (unify_nil)
  (put_list 3)
  (unify_value_perm 1)
  (unify_value_temp 2)
  (put_list 1)
  (unify_value_temp 3)
  (unify_nil)
  (deallocate)
  (proceed)
  1
  (trust_me_else_fail 2)
  (allocate 6)
  (get_variable_perm 6 1)
  (put_list 4)
  (unify_variable_perm 2)
  (unify_nil)
  (put_list 5)
  (unify_variable_perm 1)
  (unify_value_temp 4)
  (put_list 4)
  (unify_variable_perm 5)
  (unify_nil)
  (put_list 6)
  (unify_variable_perm 4)
  (unify_value_temp 4)
  (put_list 4)
  (unify_value_temp 6)
  (unify_variable_perm 3)
  (get_list 2)
  (unify_value_temp 5)
  (unify_value_temp 4)
  (put_value_perm 1 2)
  (put_value_temp 1 3)
  (put_constant attr-filler 1)
  (cl-func tx 3)
  (get_value_perm 2 1)
  (put_list 3)
  (unify_value_perm 5)
  (unify_nil)
  (put_list 4)
  (unify_value_perm 4)
  (unify_value_temp 3)
  (put_list 2)
  (unify_value_temp 4)
  (unify_value_perm 3)
  (put_value_perm 6 1)
  (call data-attributes/2 2)
  (get_variable_temp 2 1)
  (put_list 3)
  (unify_value_perm 2)
  (unify_nil)
  (put_list 4)
  (unify_value_perm 1)
  (unify_value_temp 3)
  (put_list 1)
  (unify_value_temp 4)
  (unify_value_temp 2)
  (deallocate)
  (proceed)

;;}}}

;;{{{ satisfied/1

  .proc
  satisfied/1
  (try_me_else 0 1)
  (get_nil 1)
  (put_constant true 1)
  (proceed)
  0
  (trust_me_else_fail 1)
  (allocate 1)
  (get_list 1)
  (unify_variable_temp 2)
  (unify_variable_perm 1)
  (put_value_temp 2 1)
  (call ecal/1 1)
  (put_value_perm 1 1)
  (call satisfied/1 0)
  (put_constant true 1)
  (deallocate)
  (proceed)

;;}}}

;;{{{ retain/1

  .proc
  retain/1
  (try_me_else 0 1)
  (allocate 3)
  (get_structure (add-data 3) 1)
  (unify_variable_perm 3)
  (unify_variable_perm 2)
  (unify_variable_perm 1)
  (put_structure (data 3) 1)
  (unify_value_perm 3)
  (unify_value_perm 2)
  (unify_value_perm 1)
  (call not-reached/1 3)
  (put_value_perm 2 2)
  (put_value_perm 3 3)
  (put_value_perm 1 4)
  (put_constant assert-ind? 1)
  (cl-func tx 4)
  (get_constant t 1)
  (cl-extra rf-terpri 0)
  (put_constant "derived fact asserted into taxon abox:"
		1)
  (cl-extra rf-pprint 1)
  (put_list 2)
  (unify_value_perm 1)
  (unify_nil)
  (put_list 3)
  (unify_value_perm 2)
  (unify_value_temp 2)
  (put_list 1)
  (unify_value_perm 3)
  (unify_value_temp 3)
  (call tup2struct*/1 3)
  (cl-extra rf-pprint 1)
  (put_structure (data 3) 1)
  (unify_value_perm 3)
  (unify_value_perm 2)
  (unify_value_perm 1)
  (deallocate)
  (execute push-fact-retain/1)
  0
  (trust_me_else_fail 1)
  (allocate 1)
  (get_variable_perm 1 1)
  (put_structure (add-data 3) 2)
  (unify_variable_temp 3)
  (unify_variable_temp 4)
  (unify_variable_temp 5)
  (call nou/2 1)
  (put_value_perm 1 1)
  (call not-reached/1 1)
  (put_value_perm 1 1)
  (deallocate)
  (execute push-fact-retain/1)

;;}}}

;;{{{ tx-unify/2

  .proc
  tx-unify/2
  (get_value_temp 1 2)
  (put_constant true 1)
  (proceed)

;;}}}

;;{{{ open-node/1

  .proc
  open-node/1
  (try_me_else 0 1)
  (allocate 1)
  (get_variable_perm 1 1)
  (call next-open-node/0 1)
  (put_value_perm 1 1)
  (call open-node-aux/1 0)
  (put_constant true 1)
  (deallocate)
  (proceed)
  0
  (trust_me_else_fail 1)
  (allocate 1)
  (get_variable_perm 1 1)
  (some-more-open-nodes)
  (put_value_perm 1 1)
  (call open-node/1 0)
  (put_constant true 1)
  (deallocate)
  (proceed)

;;}}}

;;{{{ open-node-aux/1

  .proc
  open-node-aux/1
  (try_me_else 0 1)
  (allocate 4)
  (get_variable_perm 4 1)
  (call get-open-node/0 4)
  (get_variable_temp 2 1)
  (put_structure (data 3) 1)
  (unify_variable_temp 3)
  (unify_variable_perm 3)
  (unify_variable_perm 2)
  (call tx-unify/2 4)
  (put_value_perm 3 2)
  (put_constant concept-closure 1)
  (cl-func tx 2)
  (get_variable_temp 2 1)
  (put_variable_perm 1 1)
  (call member/2 4)
  (put_value_perm 4 1)
  (get_structure (data 3) 1)
  (unify_local_value_perm 1)
  (unify_value_perm 3)
  (unify_value_perm 2)
  (put_constant true 1)
  (deallocate)
  (proceed)
  0
  (trust_me_else_fail 1)
  (allocate 1)
  (get_variable_perm 1 1)
  (call get-open-node/0 1)
  (get_value_perm 1 1)
  (put_structure (data 3) 2)
  (unify_variable_temp 3)
  (unify_variable_temp 4)
  (unify_variable_temp 5)
  (call nou/2 0)
  (put_constant true 1)
  (deallocate)
  (proceed)
 

;;}}}

;;{{{ fc-strategies except open-node and retain 

;;{{{ df-enum/2

  .proc 
  df-enum/2
  (try_me_else 0 2)
  (allocate 3)
  (get_variable_perm 3 2)
  (get_list 1)
  (unify_variable_perm 2)
  (unify_variable_perm 1)
  (enter-retain-stack)
  (put_list 1)
  (unify_value_perm 2)
  (unify_value_perm 1)
  (call satisfied/1 3)
  (put_list 1)
  (unify_value_perm 2)
  (unify_value_perm 1)
  (put_value_perm 3 2)
  (deallocate)
  (execute df-elist/2)
  0
  (retry_me_else 1 2)
  (allocate 2)
  (get_variable_perm 2 1)
  (get_variable_perm 1 2)
  (put_list 2)
  (unify_variable_temp 3)
  (unify_variable_temp 4)
  (call nou/2 2)
  (enter-retain-stack)
  (put_value_perm 2 1)
  (call ecal/1 2)
  (put_value_perm 2 1)
  (put_value_perm 1 2)
  (deallocate)
  (execute df-one/2)
  1
  (trust_me_else_fail 2)
  (leave-retain-stack)
  (fail)

;;}}}

;;{{{ df-elist/2

  .proc
  df-elist/2
  (try_me_else 0 2)
  (get_list 1)
  (unify_variable_temp 3)
  (unify_variable_temp 4)
  (put_value_temp 3 1)
  (execute df-one/2)
  0
  (trust_me_else_fail 2)
  (get_list 1)
  (unify_variable_temp 3)
  (unify_variable_temp 4)
  (put_value_temp 4 1)
  (execute df-elist/2)

;;}}}

;;{{{ df-one/2

  .proc
  df-one/2
  (allocate 2)
  (get_variable_perm 2 2)
  (put_variable_perm 1 2)
  (call forward/2 2)
  (put_unsafe_value_perm 1 1)
  (put_value_perm 2 2)
  (deallocate)
  (execute df-one-more/2)

;;}}}

;;{{{ df-one-more/2

  .proc
  df-one-more/2
  (try_me_else 0 2)
  (get_value_temp 1 2)
  (proceed)
  0
  (trust_me_else_fail 2)
  (execute df-one/2)

;;}}}
  
;;{{{ df-all/2

  .proc
  df-all/2
  (try_me_else 0 2)
  (allocate 3)
  (get_variable_perm 3 2)
  (get_list 1)
  (unify_variable_perm 2)
  (unify_variable_perm 1)
  (enter-retain-stack)
  (put_list 1)
  (unify_value_perm 2)
  (unify_value_perm 1)
  (call satisfied/1 3)
  (put_list 1)
  (unify_value_perm 2)
  (unify_value_perm 1)
  (put_value_perm 3 2)
  (deallocate)
  (execute df-alist/2)
  0
  (retry_me_else 1 2)
  (allocate 2)
  (get_variable_perm 2 1)
  (get_variable_perm 1 2)
  (put_list 2)
  (unify_variable_temp 3)
  (unify_variable_temp 4)
  (call nou/2 2)
  (call fc-initialize/0 2)
  (put_value_perm 2 1)
  (call ecal/1 2)
  (put_value_perm 2 1)
  (put_value_perm 1 2)
  (deallocate)
  (execute df-all1/2)
  1
  (retry_me_else 2 2)
  (allocate 1)
  (get_variable_perm 1 2)
  (call collect-facts/0 1)
  (get_variable_temp 2 1)
  (put_value_perm 1 1)
  (call filter/2 0)
  (get_variable_temp 2 1)
  (put_value_temp 2 1)
  (deallocate)
  (proceed)
  2
  (trust_me_else_fail 2)
  (leave-retain-stack)
  (fail)

;;}}}

;;{{{ df-alist/2

  .proc
  df-alist/2
  (try_me_else 0 2)
  (get_list 1)
  (unify_variable_temp 3)
  (unify_variable_temp 4)
  (put_value_temp 3 1)
  (execute df-all1/2)
  0
  (trust_me_else_fail 2)
  (get_list 1)
  (unify_variable_temp 3)
  (unify_variable_temp 4)
  (put_value_temp 4 1)
  (execute df-alist/2)

;;}}}

;;{{{ df-all1/2

  .proc
  df-all1/2
  (allocate 2)
  (get_variable_perm 2 2)
  (put_variable_perm 1 2)
  (call forward/2 2)
  (put_unsafe_value_perm 1 1)
  (put_value_perm 2 2)
  (deallocate)
  (execute df-all1/2)

;;}}}

;;{{{ bf-enum/2

  .proc
  bf-enum/2
  (try_me_else 0 2)
  (allocate 3)
  (get_variable_perm 3 2)
  (get_list 1)
  (unify_variable_perm 2)
  (unify_variable_perm 1)
  (enter-retain-stack)
  (put_list 1)
  (unify_value_perm 2)
  (unify_value_perm 1)
  (call satisfied/1 3)
  (put_list 1)
  (unify_value_perm 2)
  (unify_value_perm 1)
  (put_value_perm 3 2)
  (deallocate)
  (execute bf-elist/2)
  0
  (retry_me_else 1 2)
  (allocate 2)
  (get_variable_perm 2 1)
  (get_variable_perm 1 2)
  (put_list 2)
  (unify_variable_temp 3)
  (unify_variable_temp 4)
  (call nou/2 2)
  (enter-retain-stack)
  (put_value_perm 2 1)
  (call ecal/1 2)
  (put_value_perm 2 1)
  (put_value_perm 1 2)
  (call forward/2 1)
  (put_value_perm 1 1)
  (deallocate)
  (proceed)
  1
  (retry_me_else 2 2)
  (allocate 1)
  (get_variable_perm 1 2)
  (call ecal/1 1)
  (put_value_perm 1 1)
  (deallocate)
  (execute forward-one/1)
  2
  (trust_me_else_fail 2)
  (leave-retain-stack)
  (fail)

;;}}}

;;{{{ bf-elist/2

  .proc
  bf-elist/2
  (try_me_else 0 2)
  (allocate 1)
  (get_variable_perm 1 2)
  (get_list 1)
  (unify_variable_temp 3)
  (unify_variable_temp 4)
  (put_value_temp 3 1)
  (call forward/2 1)
  (put_value_perm 1 1)
  (deallocate)
  (proceed)
  0
  (retry_me_else 1 2)
  (get_list 1)
  (unify_variable_temp 3)
  (unify_variable_temp 4)
  (put_value_temp 4 1)
  (execute bf-elist/2)
  1
  (trust_me_else_fail 2)
  (get_nil 1)
  (get_variable_temp 1 2)
  (execute forward-one/1)

;;}}}
  
;;{{{ forward-one/1

  .proc
  forward-one/1
  (allocate 2)
  (get_variable_perm 1 1)
  (put_variable_perm 2 1)
  (call open-node/1 2)
  (put_unsafe_value_perm 2 1)
  (put_value_perm 1 2)
  (call forward/2 1)
  (put_value_perm 1 1)
  (deallocate)
  (proceed)

;;}}}

;;{{{ bf-all/2

  .proc
  bf-all/2
  (try_me_else 0 2)
  (allocate 3)
  (get_variable_perm 3 2)
  (get_list 1)
  (unify_variable_perm 2)
  (unify_variable_perm 1)
  (enter-retain-stack)
  (put_list 1)
  (unify_value_perm 2)
  (unify_value_perm 1)
  (call satisfied/1 3)
  (put_list 1)
  (unify_value_perm 2)
  (unify_value_perm 1)
  (put_value_perm 3 2)
  (deallocate)
  (execute bf-alist/2)
  0
  (retry_me_else 1 2)
  (allocate 1)
  (get_variable_perm 1 1)
  (put_list 3)
  (unify_variable_temp 2)
  (unify_variable_temp 4)
  (put_value_temp 3 2)
  (call nou/2 1)
  (call fc-initialize/0 1)
  (put_value_perm 1 1)
  (call ecal/1 1)
  (put_value_perm 1 1)
  (put_variable_temp 2 2)
  (call forward/2 0)
  (fail)
  (deallocate)
  (proceed)
  1
  (trust_me_else_fail 2)
  (allocate 2)
  (get_variable_perm 2 2)
  (call forward-all/0 2)
  (call collect-facts/0 2)
  (get_variable_temp 2 1)
  (put_value_perm 2 1)
  (call filter/2 1)
  (get_variable_perm 1 1)
  (leave-retain-stack)
  (put_unsafe_value_perm 1 1)
  (deallocate)
  (proceed)

;;}}}

;;{{{ bf-alist/2

  .proc
  bf-alist/2
  (try_me_else 0 2)
  (allocate 0)
  (get_list 1)
  (unify_variable_temp 3)
  (unify_variable_temp 2)
  (put_value_temp 3 1)
  (put_variable_temp 2 2)
  (call forward/2 0)
  (fail)
  (proceed)
  0
  (trust_me_else_fail 2)
  (get_list 1)
  (unify_variable_temp 3)
  (unify_variable_temp 4)
  (put_value_temp 4 1)
  (execute bf-alist/2)

;;}}}

;;{{{ forward-all/0

  .proc
  forward-all/0
  (try_me_else 0 0)
  (allocate 1)
  (put_variable_perm 1 1)
  (call open-node/1 1)
  (put_unsafe_value_perm 1 1)
  (put_variable_temp 2 2)
  (call forward/2 0)
  (fail)
  0
  (trust_me_else_fail 0)
  (proceed)

;;}}}
  
;;{{{ satisfied/1

  .proc
  satisfied/1
  (try_me_else 0 1)
  (get_nil 1)
  (proceed)
  0
  (trust_me_else_fail 1)
  (allocate 1)
  (get_list 1)
  (unify_variable_temp 2)
  (unify_variable_perm 1)
  (put_value_temp 2 1)
  (call ecal/1 1)
  (put_value_perm 1 1)
  (call satisfied/1 0)
  (deallocate)
  (proceed)

;;}}}

;;}}}
  

 	) 
 )
