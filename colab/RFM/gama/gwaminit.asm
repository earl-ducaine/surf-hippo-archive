(gasm title "system prelude"
      author "Michael Sintek"
      date "Jan 1992"
      comment "define some necessary builtins in module system"
      code (

      (.module system)
      (.import-from user query)
      .static

      .proc
fail-and-terminate
      (has-failed)

      .proc
fail  (fail)

      .proc
query*
      (try "proc" 0)
      (trust fail-and-terminate 0)
"proc"
      (call query 0) ; imported from module user
      (has-succeeded)

      .proc
dcall/1 ; (structure)
      (allocate 0)
      (mcall/dy 1 0)
      (deallocate)
      (proceed)

      .proc
call/2 ; (structure module)
      (allocate 0)
      (mcall/mod 1 0 2)
      (deallocate)
      (proceed)

      .proc
ecal/1 ; (structure )
      (allocate 0)
      (put_constant USER 2)
      (mcall/mod 1 0 2)
      (deallocate)
      (proceed)

      .proc
apply/3 ; (operator args module)
      (allocate 0)
      (apply/mod 1 2 0 3)
      (deallocate)
      (proceed)

      .proc
tup2struct/1 ; (tup ...)
      (allocate 0)
      (tup2struct 1)
      (deallocate)
      (proceed)

      .proc
struct2tup/1 ; structure
      (allocate 0)
      (struct2tup 1)
      (deallocate)
      (proceed)

      .proc
once/1 ; preliminarily built in
      (allocate 0)
      (save_cut_pointer)
      (mcall/dy 1 0)
      (last_cut)
      (deallocate)
      (proceed)

      .proc ; collection of meta-logical test predicates
var/1 (set_index_number 1)
      (switch_on_term "fail" "fail" "fail" "fail" "proceed")

nonvar/1
      (set_index_number 1)
      (switch_on_term "proceed" "proceed" "proceed" "proceed" "fail")

struct/1
      (set_index_number 1)
      (switch_on_term "fail" "proceed" "fail" "fail" "fail")

not-struct/1
      (set_index_number 1)
      (switch_on_term "proceed" "fail" "proceed" "proceed" "proceed")

check-tup/1
      (set_index_number 1)
      (switch_on_term "fail" "fail" "proceed" "proceed" "proceed")

is-tupstruct/1
      (set_index_number 1)
      (switch_on_term "test-ts" "fail" "fail" "fail" "fail")
"test-ts"
      (cl-pred tupstruct-p 1)
      (proceed)

is-not-tupstruct/1 ; for atoms!
      (set_index_number 1)
      (switch_on_term "test-not-ts" "fail" "fail" "fail" "fail")
"test-not-ts"
      (cl-pred not-tupstruct-p 1)
      (proceed)

"proceed"
      (proceed)

"fail"
      (fail)


))

