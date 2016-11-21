;;; some utilities ... as WAM - programs

(gasm title "forward utils"
      athor "Martin Harm"
      date "July 1992"
      comment "some utils, used within the strategies. mustnot be compiled"
      code (

      (.module system)
      .proc
nou/2 ; (arg1 arg2)
     (try_me_else "noulabel" 2)
     (allocate 0)
     (save_cut_pointer)
     (get_value_temp 1 2)
     (first_cut)
     (fail)
     "noulabel"
     (trust_me_else_fail 2)
     (put_constant true 1)
     (proceed)

     (.module user)
     .proc
collect-tupof/1
     (collect-tupof 1)
     (proceed)

     (.module prelude)
     .proc
not-member/2  ; (expr) (list)
;;;; laeuft leider nicht, wg Fehler mit cut
#|
     (try_me_else 0 2)
     (allocate 0)
     (save_cut_pointer)
     (call member/2 0)
     (first_cut)
     (fail)
     0
     (trust_me_else_fail 2)
     (put_constant true 1)
     (proceed)
|#
;;;; lisp version.. ACHTUNG laeuft nur!! mit KONSTANTEN/VARIABLEN
;;;; keine Strukturen erlaubt !!
     (not-member 1 2)     
     (put_constant true 1)
     (proceed)

)    )
      
