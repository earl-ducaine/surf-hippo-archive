(defun symbolic-clash-=? (index_a obj_b)
  (or (set? index_a (real-ord-obj-*< obj_b))
      (set? index_a (real-ord-obj-*> obj_b))
      (set? index_a (real-ord-obj-<> obj_b))))

(defun symbolic-clash->? (index_a obj_b)
  (or (set? index_a (real-ord-obj-*< obj_b))
      (set? index_a (real-ord-obj-*<= obj_b))
      (set? index_a (real-ord-obj-= obj_b))))


(defun symbolic-clash->=? (index_a obj_b)
  (set? index_a (real-ord-obj-*< obj_b)))

(defun symbolic-clash-<>? (index_a obj_b)
  (set? index_a (real-ord-obj-= obj_b)))
