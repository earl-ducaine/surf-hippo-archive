; 13 problems: 3 goals, 2 rooms (SW1)

(setf *test-probs*
      '((setf (current-problem)
	 (create-problem
	  (name SS4-1)
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
		(pushable C)
		(carriable A)
		(inroom C rm2)
		(inroom B rm2)
		(inroom A rm2)
		(inroom key12 rm2)
		(inroom robot rm2)
		(carriable key12)
		(is-key dr12 key12)))
	  (goal (and (inroom robot rm1) (dr-closed dr12) (inroom A rm1)))))
	
	(setf (current-problem)
	 (create-problem
	  (name SS4-2)
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
		(pushable A)
		(inroom B rm1)
		(inroom A rm2)
		(inroom key12 rm1)
		(inroom robot rm2)
		(carriable key12)
		(is-key dr12 key12)))
	  (goal (and (dr-closed dr12) (inroom A rm1) (inroom key12 rm1)))))
	
	(setf (current-problem)
	 (create-problem
	  (name SS4-4)
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
		(inroom key12 rm2)
		(inroom robot rm2)
		(carriable key12)
		(is-key dr12 key12)))
	  (goal (and (inroom B rm1) (locked dr12) (inroom key12 rm1)))))
	
	(setf (current-problem)
	 (create-problem
	  (name SS4-5)
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
		(next-to A B)
		(next-to B A)
		(dr-closed dr12)
		(locked dr12)
		(pushable A)
		(inroom B rm2)
		(inroom A rm2)
		(inroom key12 rm2)
		(inroom robot rm2)
		(carriable key12)
		(is-key dr12 key12)))
	  (goal (and (arm-empty) (dr-closed dr12) (inroom A rm1)))))
	
	(setf (current-problem)
	 (create-problem
	  (name SS4-6)
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
		(inroom B rm1)
		(inroom A rm2)
		(inroom key12 rm2)
		(inroom robot rm2)
		(carriable key12)
		(is-key dr12 key12)))
	  (goal (and (arm-empty) (dr-closed dr12) (inroom B rm2)))))
	
	(setf (current-problem)
	 (create-problem
	  (name SS4-7)
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
		(inroom A rm2)
		(inroom key12 rm1)
		(inroom robot rm2)
		(carriable key12)
		(is-key dr12 key12)))
	  (goal (and (locked dr12) (inroom key12 rm2)))))
	
	(setf (current-problem)
	 (create-problem
	  (name SS4-8)
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
		(next-to C A)
		(next-to A C)
		(unlocked dr12)
		(dr-closed dr12)
		(carriable C)
		(carriable A)
		(inroom C rm2)
		(inroom B rm1)
		(inroom A rm2)
		(inroom key12 rm2)
		(inroom robot rm1)
		(carriable key12)
		(is-key dr12 key12)))
	  (goal (and (unlocked dr12) (dr-closed dr12) (inroom A rm1)))))
	
	(setf (current-problem)
	 (create-problem
	  (name SS4-9)
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
		(is-room rm2)
		(is-room rm1)
		(is-door dr12)
		(pushable B)
		(is-object key12)
		(is-object B)
		(is-object A)
		(inroom B rm2)
		(inroom A rm1)
		(inroom key12 rm2)
		(inroom robot rm2)
		(carriable key12)
		(is-key dr12 key12)))
	  (goal (and (unlocked dr12) (inroom key12 rm1) (dr-open dr12)))))
	
	(setf (current-problem)
	 (create-problem
	  (name SS4-10)
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
		(is-room rm2)
		(is-room rm1)
		(is-door dr12)
		(carriable B)
		(pushable A)
		(is-object key12)
		(is-object B)
		(is-object A)
		(inroom B rm1)
		(inroom A rm2)
		(inroom key12 rm1)
		(inroom robot rm2)
		(carriable key12)
		(is-key dr12 key12)))
	  (goal (and (next-to A B) (inroom B rm1)))))
	
	(setf (current-problem)
	 (create-problem
	  (name SS4-11)
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
		(carriable A)
		(inroom B rm1)
		(inroom A rm1)
		(inroom key12 rm2)
		(inroom robot rm2)
		(carriable key12)
		(is-key dr12 key12)))
	  (goal (and (dr-closed dr12) (inroom key12 rm2) (inroom A rm2)))))
	
	(setf (current-problem)
	 (create-problem
	  (name SS4-12)
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
		(inroom A rm1)
		(inroom key12 rm1)
		(inroom robot rm2)
		(carriable key12)
		(is-key dr12 key12)))
	  (goal (and (inroom B rm2) (inroom robot rm1)))))
	
	(setf (current-problem)
	 (create-problem
	  (name SS4-13)
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
		(inroom B rm1)
		(inroom A rm2)
		(inroom key12 rm1)
		(inroom robot rm2)
		(carriable key12)
		(is-key dr12 key12)))
	  (goal (and (inroom key12 rm2) (inroom robot rm1) (dr-closed dr12)))))
	
	
	(setf (current-problem)
	 (create-problem
	  (name SS4-14)
	  (objects
	   (a b c BOX)
	   (key12 KEY)
	   (rm1 rm2 ROOM)
	   (dr12 DOOR))
	  (state (and (arm-empty)
		      (dr-to-rm dr12 rm2)
		      (dr-to-rm dr12 rm1)
		      (connects dr12 rm2 rm1)
		      (connects dr12 rm1 rm2)
		      (unlocked dr12)
		      (dr-open dr12)
		      (pushable B)
		      (inroom C rm2)
		      (inroom B rm1)
		      (inroom A rm2)
		      (inroom key12 rm2)
		      (inroom robot rm2)
		      (carriable key12)
		      (is-key dr12 key12)))
	  (goal (and (inroom key12 rm2) (dr-closed dr12) (next-to B C)))))))


;; ebl code

(setq *AUX-COMMANDS*
      '((SS4-4 (optimal-path (cadr *ALL-NODES*)))
	(SS4-14 (optimal-path (cadr *ALL-NODES*)))))
