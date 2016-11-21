;***************************************************************************
;***************************************************************************
;*
;*  Module           : FRONT-END
;*  File             : listing, previous pp-taxon 
;*  Author           : Andreas Abecker
;*  Edited           : 01.08.91, modified 15-07-92
;*  Last change      : 
;*  Details changed  : 
;*
;*  Comment          : function produces a listing of the actual TAXON 
;*                     knowledge base, adapted for Taxon3 version
;*                     
;*  Status           : not released
;*
;****************************************************************************
   
(defun pp-taxon ()
   (l-all))  

(defun l-all ()
   (terpri)
   (princ (format nil "The TAXON Knowledge Base~%"))
   (princ (format nil "========================"))
   (terpri)
   (mapcar #'fl '(attr role sprim ofam cfam sprim prim cpred apred conc asse
   ))
   T
)      
   

(defmacro l (item)
   `(fl ',item))
  
(defun fl (item)
   (let ((number 0))
   (cond ((eq item 'conc) (terpri)
                          (princ "Concept Definitions:")
                          (terpri)
                          (princ "-------------------------")
                          (setf number (print-special-items 'conc))
                          (terpri)
                          (princ (format nil "~A concept definitions" number))
                          (terpri))
          ((eq item 'ofam) (terpri)
                          (princ "Open Families of Disjoint Primitives:")
                          (terpri)
                          (princ "-------------------------")
                          (setf number (print-special-items 'ofam))
                          (terpri)
                          (princ (format nil "~A open families" number))
                          (terpri))
          ((eq item 'cfam) (terpri)
                          (princ "Closed Families of Disjoint Primitives:")
                          (terpri)
                          (princ "-------------------------")
                          (setf number (print-special-items 'cfam))
                          (terpri)
                          (princ (format nil "~A closed families" number))
                          (terpri))
          ((eq item 'sfam) (terpri)
                          (princ "Subfamilies of Closed Families:")
                          (terpri)
                          (princ "-------------------------")
                          (setf number (print-special-items 'sfam))
                          (terpri)
                          (princ (format nil "~A subfamilies" number))
                          (terpri))
          ((eq item 'cpred) (terpri)
                          (princ "Concrete Predicate Definitions:")
                          (terpri)
                          (princ "-------------------------")
                          (setf number (print-special-items 'cpred))
                          (terpri)
                          (princ (format nil "~A concrete predicate definitions" number))
                          (terpri))
         ((eq item 'apred) (terpri)
                          (princ "Abstract Predicate Definitions:")
                          (terpri)
                          (princ "-------------------------")
                          (setf number (print-special-items 'apred))
                          (terpri)
                          (princ (format nil "~A abstract predicate definitions" number))
                          (terpri))
          ((eq item 'role) (terpri)
                          (princ "Role Declarations:")
                          (terpri)
                          (princ "------------------")
                          (setf number (print-special-items 'role))
                          (terpri)
                          (princ (format nil "~A role declarations" number)) 
                          (terpri))
           ((eq item 'attr) (terpri)
                          (princ "Attribute Declarations:")
                          (terpri)
                          (princ "-----------------------")
                          (setf number (print-special-items 'attr))
                          (terpri)
                          (princ (format nil "~A attribute declarations" number))
                          (terpri))
           ((eq item 'prim) (terpri)
                          (princ "Primitive Concepts:")
                          (terpri)
                          (princ "-------------------")
                          (setf number (print-special-items 'prim))
                          (terpri)
                          (princ (format nil "~A primitive concept declarations" number))
                          (terpri))
          
             ((eq item 'sprim) (terpri)
                          (princ "Disjoint Primitive Concepts:")
                          (terpri)
                          (princ "----------------------------")
                          (setf number (print-special-items 'sprim))
                          (terpri)
                          (princ (format nil "~A disjoint primitive concepts" number))
                          (terpri))
             ((eq item 'asse) (terpri)
                          (princ "Assertions:")
                          (terpri)
                          (princ "--------------------------------------------------------------------------")
                          (setf number (print-assertional-items))
                          (terpri)
                          (princ (format nil "~A assertions" number))
                          (terpri))
          
              ;;; fehlt noch: interesting concepts,
              ;;;            



          ( t (print  "Unknown Item given to the TAXON Pretty Printer") (throw :error nil)))
  )
 T)


(defun print-special-items (type)
   (let ((number 0))
    (if (not (eq type 'sprim))
    (progn
    (princ (format nil "~% Identifier              Type    Definition ~%"))
    (maphash #'(lambda (x y) (if (and (eq type (tbox-entry-type y)) (or (not (eq type 'conc))
                                                                        (funcall *draw-test* x)))
                                 (progn (setf number (+ number 1))
                                 (princ (format nil "~% ~A ~25T~A   ~A ~%" x type (if (or (eq 'conc type)
                                                                                          (member type '(ofam
sfam cfam)))
                                                                                      (tbox-entry-output y)
                                                                                      (tbox-entry-definition y))  ))
                                  ))
               )
              *tbox-table*
     )
    number
    )
    (progn
    (princ (format nil "~% Identifier              Type    Family           Type ~%"))
    (maphash #'(lambda (x y) (if (eq type (tbox-entry-type y))
                                 (progn (setf number (+ number 1))
                                 (princ (format nil "~% ~A ~25T~A   ~A  ~50T~A ~%" x type (tbox-entry-disjoint y)
(tbox-entry-family y)))
                                  ))
               )
              *tbox-table*
     )
    number
    ) 
    )
  )
)


(defun print-assertional-items ()
   (let ((number 0))
     (mapcar #'(lambda (x) 
                            (if (visible-assertion? x)
                                (progn  (setf number (+ number 1)) 
                                        (terpri) 
                                        (print-abox-entry x x x))))
               *abox-table*)
     number)
)




;; (l prim)


