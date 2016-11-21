;*******************************************************************************
;*
;*	File : Make Functions -----------> DD 04.08.92
;*
;*******************************************************************************

(defun make-top-SR ()
  #'(lambda (cnf) t))
(defun make-bottom-SR ()
  #'(lambda (cnf) 'unknown))

(defun make-prim-SR (prim)
  #'(lambda (cnf) (prim-SR prim cnf)))
(defun make-neg-prim-SR (prim)
  #'(lambda (cnf) (neg-prim-SR prim cnf)))

(defun make-cprim-SR (prim)
  #'(lambda (cnf) (cprim-SR prim cnf)))
(defun make-oprim-SR (prim)
  #'(lambda (cnf) (oprim-SR prim cnf)))
(defun make-neg-sprim-SR (prim)
  #'(lambda (cnf) (neg-sprim-SR prim cnf)))
(defun make-family-SR (fam)
  #'(lambda (cnf) (family-SR fam cnf)))
(defun make-neg-family-SR (fam)
  #'(lambda (cnf) (neg-family-SR fam cnf)))
(defun make-sfamily-SR (sfam)
  #'(lambda (cnf) (sfamily-SR sfam cnf)))
(defun make-neg-sfamily-SR (sfam)
  #'(lambda (cnf) (neg-sfamily-SR sfam cnf)))

(defun make-concept-SR (cnf)
  #'(lambda (cnf2) (concept-SR cnf cnf2)))
(defun make-neg-concept-SR (cnf)
  #'(lambda (cnf2) (neg-concept-SR cnf cnf2)))

(defun make-forall-SR (first-roles)
  #'(lambda (cnf) (forall-SR first-roles cnf)))
(defun make-some-SR (first-roles)
  #'(lambda (cnf) (some-SR first-roles cnf)))

(defun make-or-SR ()
  #'(lambda (cnf2) 'unknown))
