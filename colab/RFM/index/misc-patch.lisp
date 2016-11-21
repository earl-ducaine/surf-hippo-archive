; MISC.lsp Patch
;==============


(defun loco (db)
 (setq *predicates* nil)
 (let (a)
;;;  (rf-pprint db)
;;;  (cr-return)
  (read-db db)
  (setq a (classify-db *predicates*))
;;;  (rf-pprint (setq a (classify-db *predicates*)))
;;;  (cr-return)
;;; (rf-pprint (mapcar #'code-gen-proc a))
; <------- indexing ------>
  (mapcar #'(lambda (x) (append
			 (mapcan2 
			  #'(lambda (y)
				    (icg.mk-header y (s-cg-arity-of-proc x)))
			  (iif.mk-tree x))
			 (code-gen-proc x)))
	  a)
 )
)
