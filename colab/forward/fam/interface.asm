(gasm title "fam prelude"
      author "Martin Harm"
      date "Nov 1992"
      comment "define the query of the FAM ."
      code (

      (.module user)


      .proc
fam-back*               ;;;; this is a dummy procedure 
      (fail)

      .proc
fam-query*
      (try "proc" 0)
      (trust "fail" 0)
"proc"
      (call fam-back* 0) ;;; the real address is calculated in prove 
      (fa-has-succeeded)
"fail"
      (fa-has-failed)

))


          