;;; 06-07-92 aa
;;; Makro fuer Assertion (i1 (a1 r1 a2) i2)
;;; expandieren zu (i1 a1 ni1) (ni1 r1 ni2) .. 

(defun make-new-extern-object ()
    (gensym)
)

(defun expand-filler-assertion (fa)
   (let ((i1 (get-ind1-of-filler-asse fa))
         (path (get-rel-of-filler-asse fa))
         (i2 (get-ind2-of-filler-asse fa)))
      (if (= (length path) 1)
          (fasse fa)
          (let ((ni (make-new-extern-object))
                (first-rel (list (get-first-relation-of-path path)))  ;; besser make-path
                (rest-rel  (get-rest-relations-of-path path)))
             (fasse (make-filler-assertion first-rel i1 ni))
             (expand-filler-assertion (make-filler-assertion  rest-rel ni i2))
          )
      )
   )
)

