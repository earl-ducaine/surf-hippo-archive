;;; 02-07-92 aa
;;; first subsumption test

(defvar *classify-list* nil)
(defvar *subsumption-graph* (make-a-graph (make-node :value '(top))
                                          (make-node :value '(bottom))))



(defmacro subs? (c1 c2)
   `(fsubs? ',c1 ',c2))

(defun fsubs? (c1 c2)
  (if (listed-subsumption? c1 c2)
      T
  (let* ((nc1 (get-CNF-of-conceptname c1))
         (nc2 (get-CNF-of-conceptname c2))
	 (abox *abox-table*)
         (swresult (subsumes-weakly? nc1 nc2)))
      (if (eq 'unknown swresult)
   (progn
   (clear-abox)
   (let ((result (not (satisfy-abox 
             (list
                 (make-membership-assertion
                      (make-conjunction-rule (list
                                              (make-conceptname-rule     nc2)
                                              (make-neg-conceptname-rule nc1)
                                              ))
                      (create-abstract-ind!  'dennis)
                  )))))
	 )
     (setf *abox-table* abox)
     result))
          swresult)
   )
)

)


(defun value-subsumes (a b)
    (let ((ac (if (not (listp a)) a (car a)))
          (bc (if (not (listp b)) b (car b))))
       (fsubs? ac bc)
     )
)


(defun classify-all ()
   (if *trace-classify
     (time 
   (mapcar #'classify
          (reverse *classify-list*)))
   (mapcar #'classify
          (reverse *classify-list*))
    )
    T
   
)

(defmacro classify-concept (co)
   `(classify ',co))


(defun classify (co)
   (if *trace-classify (princ (format nil "~% Classify Concept: ~A ~%" co)))
   (let* ((node (if (not (fsati-co? co))
                    (get-node-of-item 'bottom)
                    (value-classify (list co) *subsumption-graph*)))
          (nv (node-value node))
          (cnf (get-cnf-of-conceptname co)))
      (if (or  (and (symbolp nv) (not (eq nv co))) 
               (and (listp nv) (not (member co nv :test #'equal))))
               (let ((new-node-value (if (symbolp nv) (list co nv) (append (list co) nv))))
                   (if (and *trace-classify (visible-item? co)) (princ (format nil "~% ~A new member of equivalence class ~A ~%" co nv)))
                   (setf (node-value node) new-node-value)
                  
               )
      )
      (set-node! cnf node)
      (tbox-insert-hierarchy-node co node)
      T
   )
)

(defun pphi ()
  (if *filtered-graph?
     (print-whole-graph *filtered-graph*) 
     (print-whole-graph *subsumption-graph*)
))




(defun concept-subsumption-look-up (co1 co2)
   (let ((nc1 (get-node-of-item co1))
         (nc2 (get-node-of-item co2)))
        (if (or (null nc1) (null nc2)) nil
            (node-above *subsumption-graph* nc1 nc2))
   )
)
 


(defun subsumption-look-up (cnf1 cnf2)
  (let ((nc1 (get-node cnf1))
        (nc2 (get-node cnf2)))
   (if (or (null nc1) (null nc2)) nil
       (node-above *subsumption-graph* nc1 nc2)
    )
  )
)

(defun subsumes-look-up? (cnf1 cnf2)
   (subsumption-look-up cnf1 cnf2))

(defun subsumed-by-look-up? (cnf1 cnf2)
   (subsumption-look-up cnf2 cnf1))

(defmacro equi (conc)
   `(fequi ',conc)
)


#|
(defmacro iup (conc)
   `(fiup ',conc))
|#


(defmacro ilo (conc)
   `(filo ',conc))

(defmacro up (conc)
   `(fup ',conc))

(defmacro lo (conc)
   `(flo ',conc))

(defun fequi (conc)
   (node-value (get-node-of-item conc))
)

#|
(defun fiup (conc)
   (let ((nodes  (node-immediate-uppers (get-node-of-item conc))))
      (mapcar #'front-end-print-node nodes) T)
)|#

(defun fiup (conc)
   (let* ((conc (if (listp conc) (car conc) conc))
          (nodes  (node-immediate-uppers (get-node-of-item conc))))
      (flat (mapcar #'front-end-print-node nodes)))
)

(defun flat (list-of-lists)
  (let ((result nil))
   (dolist (actual list-of-lists (remove-if-not #'visible-item? result))
        (setf result (union actual result)))))


(defun iup (conc) (fiup conc))


(defun get-uppers (conc) 
  (node-immediate-uppers (get-node-of-item conc)))

(defun filo (conc)
   (let* ((conc (if (listp conc) (car conc) conc))
         (nodes  (node-immediate-lowers (get-node-of-item conc))))
      (flat (mapcar #'front-end-print-node nodes)))
)


(defun fup (conc)
   (let ((nodes (nodes-all-uppers (list (get-node-of-item conc)))))
     (mapcar #'front-end-print-node nodes)) 
)

(defun flo (conc)
   (let ((nodes (nodes-all-lowers (list (get-node-of-item conc)))))
     (mapcar #'front-end-print-node nodes)) 
)



(defun front-end-print-node (node)
   ;(print (node-value node))
   (node-value node)
)


