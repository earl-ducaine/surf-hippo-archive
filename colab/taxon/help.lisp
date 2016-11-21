;*******************************************************************************
;*
;*	Help File -- not permanent
;*
;*******************************************************************************

;;; front-end functions

(defmacro obj (ud-object)
  (get-object ud-object))
(defmacro dobj (ud-object)
  (get-dereferenced-object ud-object))

(defmacro objn (number)
  (get-new-obj-of-number number))
(defmacro dobjn (number)
  (get-new-dereferenced-obj-of-number number))

(defmacro cnf (concept-name)
  (get-cnf-of-conceptname concept-name))

(defmacro cnfn (number)
  (get-cnf-of-new-concept number))

(defmacro tbe (name)
  (get-tbox-entry name))
(defmacro tben (number)
  (get-tbox-entry (get-new-conceptname-of-number number)))


