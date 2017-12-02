; 13 probs 3 blocks, 2 goals

(setf *test-probs*
      '((setf (current-problem)
	 (create-problem
	  (name SS2-1)
	  (objects
	   (a b  BOX)
	   (key12 KEY)
	   (rm1 rm2 ROOM)
	   (dr12 DOOR))
	  (state
	   (and (dr-to-rm dr12 rm2)
		(dr-to-rm dr12 rm1)
		(connects dr12 rm2 rm1)
		(connects dr12 rm1 rm2)
		(arm-empty)
		(next-to b a)
		(unlocked dr12)
		(dr-closed dr12)
		(pushable b)
		(inroom b rm1)
		(inroom a rm1)
		(inroom key12 rm1)
		(inroom robot rm1)
		(carriable key12)
		(is-key dr12 key12)))
	  (goal (and (locked dr12) (arm-empty)))))
	
	(setf (current-problem)
	 (create-problem
	  (name ss2-2)
	  (objects
	   (a b c BOX)
	   (key12 KEY)
	   (rm1 rm2 ROOM)
	   (dr12 DOOR))
	  (state
	   (and (dr-to-rm dr12 rm2)
		(dr-to-rm dr12 rm1)
		(connects dr12 rm2 rm1)
		(connects dr12 rm1 rm2)
		(connects dr12 rm1 rm2)
		(connects dr12 rm2 rm1)
		(unlocked dr12)
		(dr-open dr12)
		(pushable a)
		(inroom c rm2)
		(inroom b rm1)
		(inroom a rm2)
		(inroom key12 rm1)
		(inroom robot rm1)
		(carriable key12)
		(arm-empty)
		(is-key dr12 key12)))
	  (goal (and (dr-closed dr12) (inroom robot rm2)))))
	
	(setf (current-problem)
	 (create-problem
	  (name SS2-3)
	  (objects
	   (a b c BOX)
	   (key12 KEY)
	   (rm1 rm2 ROOM)
	   (dr12 DOOR))
	  (state
	   (and (dr-to-rm dr12 rm2)
		(dr-to-rm dr12 rm1)
		(connects dr12 rm2 rm1)
		(connects dr12 rm1 rm2)
		(connects dr12 rm1 rm2)
		(connects dr12 rm2 rm1)
		(arm-empty)
		(unlocked dr12)
		(dr-closed dr12)
		(pushable c)
		(inroom c rm1)
		(inroom b rm2)
		(inroom a rm2)
		(inroom key12 rm1)
		(inroom robot rm2)
		(carriable key12)
		(is-key dr12 key12)))
	  (goal (and (inroom key12 rm1) (inroom c rm2)))))
	
	(setf (current-problem)
	 (create-problem
	  (name ss2-4)
	  (objects
	   (a b BOX)
	   (key12 KEY)
	   (rm1 rm2 ROOM)
	   (dr12 DOOR))
	  (state
	   (and (dr-to-rm dr12 rm2)
		(dr-to-rm dr12 rm1)
		(connects dr12 rm2 rm1)
		(connects dr12 rm1 rm2)
		(connects dr12 rm1 rm2)
		(connects dr12 rm2 rm1)
		(arm-empty)
		(unlocked dr12)
		(dr-open dr12)
		(pushable a)
		(inroom b rm2)
		(inroom a rm1)
		(inroom key12 rm2)
		(inroom robot rm1)
		(carriable key12)
		(is-key dr12 key12)))
	  (goal (and (inroom key12 rm1) (dr-closed dr12)))))
	
	
	(setf (current-problem)
	 (create-problem
	  (name SS2-5)
	  (objects
	   (a b c BOX)
	   (key12 KEY)
	   (rm1 rm2 ROOM)
	   (dr12 DOOR))
	  (state
	   (and (arm-empty)
		(dr-to-rm dr12 rm2)
		(dr-to-rm dr12 rm1)
		(connects dr12 rm2 rm1)
		(connects dr12 rm1 rm2)
		(unlocked dr12)
		(dr-closed dr12)
		(pushable B)
		(pushable A)
		(inroom C rm1)
		(inroom B rm2)
		(inroom A rm2)
		(inroom key12 rm1)
		(inroom robot rm2)
		(carriable key12)
		(is-key dr12 key12)))
	  (goal (and (inroom key12 rm2) (inroom robot rm2)))))
	
	(setf (current-problem)
	 (create-problem
	  (name SS2-6)
	  (objects
	   (a b BOX)
	   (key12 KEY)
	   (rm1 rm2 ROOM)
	   (dr12 DOOR))
	  (state
	   (and (arm-empty)
		(dr-to-rm dr12 rm2)
		(dr-to-rm dr12 rm1)
		(connects dr12 rm2 rm1)
		(connects dr12 rm1 rm2)
		(unlocked dr12)
		(dr-closed dr12)
		(carriable A)
		(inroom B rm2)
		(inroom A rm1)
		(inroom key12 rm1)
		(inroom robot rm2)
		(carriable key12)
		(is-key dr12 key12)))
	  (goal (and (arm-empty) (next-to B A)))))
	
	(setf (current-problem)
	 (create-problem
	  (name SS2-7)
	  (objects
	   (a b BOX)
	   (key12 KEY)
	   (rm1 rm2 ROOM)
	   (dr12 DOOR))
	  (state
	   (and (arm-empty)
		(dr-to-rm dr12 rm2)
		(dr-to-rm dr12 rm1)
		(connects dr12 rm2 rm1)
		(connects dr12 rm1 rm2)
		(next-to B A)
		(next-to A B)
		(unlocked dr12)
		(dr-open dr12)
		(carriable B)
		(pushable A)
		(inroom B rm1)
		(inroom A rm1)
		(inroom key12 rm2)
		(inroom robot rm2)
		(carriable key12)
		(is-key dr12 key12)))
	  (goal (and (locked dr12) (dr-closed dr12)))))
	
	(setf (current-problem)
	 (create-problem
	  (name SS2-8)
	  (objects
	   (a b BOX)
	   (key12 KEY)
	   (rm1 rm2 ROOM)
	   (dr12 DOOR))
	  (state
	   (and (arm-empty)
		(dr-to-rm dr12 rm2)
		(dr-to-rm dr12 rm1)
		(connects dr12 rm2 rm1)
		(connects dr12 rm1 rm2)
		(dr-closed dr12)
		(locked dr12)
		(carriable B)
		(inroom B rm1)
		(inroom A rm1)
		(inroom key12 rm1)
		(inroom robot rm1)
		(carriable key12)
		(is-key dr12 key12)))
	  (goal (and (dr-open dr12) (unlocked dr12)))))
	
	(setf (current-problem)
	 (create-problem
	  (name SS2-9)
	  (objects
	   (a b c BOX)
	   (key12 KEY)
	   (rm1 rm2 ROOM)
	   (dr12 DOOR))
	  (state
	   (and (arm-empty)
		(dr-to-rm dr12 rm2)
		(dr-to-rm dr12 rm1)
		(connects dr12 rm2 rm1)
		(connects dr12 rm1 rm2)
		(next-to A C)
		(next-to C A)
		(unlocked dr12)
		(dr-closed dr12)
		(carriable C)
		(carriable A)
		(inroom C rm2)
		(inroom B rm1)
		(inroom A rm2)
		(inroom key12 rm1)
		(inroom robot rm1)
		(carriable key12)
		(is-key dr12 key12)))
	  (goal (and (locked dr12) (next-to A C)))))
	
;;; note:  tell Jim about this problem.  dependency-directed
;;; backtracking works for this example.  ss2-10.

	(setf (current-problem)
	 (create-problem
	  (name SS2-10)
	  (objects
	   (a b BOX)
	   (key12 KEY)
	   (rm1 rm2 ROOM)
	   (dr12 DOOR))
	  (state
	   (and (arm-empty)
		(dr-to-rm dr12 rm2)
		(dr-to-rm dr12 rm1)
		(connects dr12 rm2 rm1)
		(connects dr12 rm1 rm2)
		(unlocked dr12)
		(dr-open dr12)
		(pushable B)
		(pushable A)
		(inroom B rm2)
		(inroom A rm1)
		(inroom key12 rm1)
		(inroom robot rm1)
		(carriable key12)
		(is-key dr12 key12)))
	  (goal (and (next-to B A) (inroom B rm2)))))
	
	(setf (current-problem)
	 (create-problem
	  (name SS2-11)
	  (objects
	   (a b BOX)
	   (key12 KEY)
	   (rm1 rm2 ROOM)
	   (dr12 DOOR))
	  (state
	   (and (arm-empty)
		(dr-to-rm dr12 rm2)
		(dr-to-rm dr12 rm1)
		(connects dr12 rm2 rm1)
		(connects dr12 rm1 rm2)
		(unlocked dr12)
		(dr-closed dr12)
		(pushable B)
		(carriable A)
		(inroom B rm1)
		(inroom A rm1)
		(inroom key12 rm2)
		(inroom robot rm2)
		(carriable key12)
		(is-key dr12 key12)))
	  (goal (and (inroom key12 rm1) (inroom robot rm1)))))
	
	(setf (current-problem)
	 (create-problem
	  (name SS2-12)
	  (objects
	   (a b BOX)
	   (key12 KEY)
	   (rm1 rm2 ROOM)
	   (dr12 DOOR))
	  (state
	   (and (arm-empty)
		(dr-to-rm dr12 rm2)
		(dr-to-rm dr12 rm1)
		(connects dr12 rm2 rm1)
		(connects dr12 rm1 rm2)
		(unlocked dr12)
		(dr-open dr12)
		(carriable B)
		(inroom B rm1)
		(inroom A rm2)
		(inroom key12 rm2)
		(inroom robot rm2)
		(carriable key12)
		(is-key dr12 key12)))
	  (goal (next-to A B))))
	
	(setf (current-problem)
	 (create-problem
	  (name SS2-13)
	  (objects
	   (a b BOX)
	   (key12 KEY)
	   (rm1 rm2 ROOM)
	   (dr12 DOOR))
	  (state
	   (and (arm-empty)
		(dr-to-rm dr12 rm2)
		(dr-to-rm dr12 rm1)
		(connects dr12 rm2 rm1)
		(connects dr12 rm1 rm2)
		(unlocked dr12)
		(dr-open dr12)
		(pushable B)
		(inroom B rm2)
		(inroom A rm1)
		(inroom key12 rm1)
		(inroom robot rm2)
		(carriable key12)
		(is-key dr12 key12)))
	  (SS2-13 (and (inroom robot rm1) (inroom B rm1)))))))


;; ebl code
					; unecessary
(setq *AUX-COMMANDS*
      '((SS2-2 (optimal-path (cadr *ALL-NODES*)))))
