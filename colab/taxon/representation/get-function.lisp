;************************************************************
;*                                                          *
;*                File : get-function
;*                                                          *
;************************************************************

(defun get-CNF-rule (term)
  (cond ((conceptname? term) 'CNF-tr-conceptname)
	((prim? term) 'CNF-tr-prim)
	((conjunction? term) 'CNF-tr-and)
	((disjunction? term) 'CNF-tr-or)
	((negation? term) 'CNF-tr-not)
	((family? term) 'CNF-tr-family)
	((subfamily? term) 'CNF-tr-subfamily)
	((cprim? term) 'CNF-tr-cprim)
	((oprim? term) 'CNF-tr-oprim)
	((some-frp-C-restriction? term) 'CNF-tr-some-frp-C)
	((forall-frp-C-restriction? term) 'CNF-tr-forall-frp-C)
	((some-rp-C-restriction? term) 'CNF-tr-some-rp-C)
	((forall-rp-C-restriction? term) 'CNF-tr-forall-rp-C)
	((restriction? term) 'CNF-tr-QPP)
	((agreement? term) 'CNF-tr-agreement)
	((disagreement? term) 'CNF-tr-disagreement)
	(t (error "UNKNOWN CONCEPTTERM in get-CNF-rule "))
	))
