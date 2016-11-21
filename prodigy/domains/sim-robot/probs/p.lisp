
(setf (current-problem)
      (create-problem
       (name p)
       (objects
	(rm1 rm2 ROOM)
	(dr12 DOOR)
	(B A OBJECT)
	(key12 KEY))
       (state
	(and (holding A)
	     (dr-to-rm dr12 rm2)
	     (dr-to-rm dr12 rm1)
	     (connects dr12 rm2 rm1)
	     (connects dr12 rm1 rm2)
	     (connects dr12 rm1 rm2)
	     (connects dr12 rm2 rm1)
	     (unlocked dr12)
	     (dr-closed dr12)
;	     (dr-open dr12)
	     (pushable B)
	     (carriable A)
	     (inroom B rm1)
	     (inroom A rm2)
	     (inroom key12 rm2)
	     (inroom robot rm2)
	     (carriable key12)
	     (is-key dr12 key12)
	     ))
       (igoal (and (arm-empty) (inroom robot rm1)))))

