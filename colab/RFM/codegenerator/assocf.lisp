;;;;;; ------------------------------------------------------------------------
;;;;;; y-x-usage-list: Assoc-List which indicates which Y-register has
;;;;;;                 been loaded in an A-Register.
;;;;;;                 If the Y-register is referenced again, the argument
;;;;;;                 register can be loaded from another argument register.
;;;;;;                 This saves on memory access cycle.
;;;;;;                 ((yx ax)(yz az) ... (ys as))

(defvar y-x-usage-list nil)

(defmacro is-y-in-x (y-vari y-x-usage-list)
 `(assoc ,y-vari ,y-x-usage-list))

;;;;;; Macro to add the yi/aj pair to the y-x-usage-list 
(defmacro add-y-x-list (y-vari x-reg y-x-usage-list)
 `(setq ,y-x-usage-list (cons (list ,y-vari ,x-reg) ,y-x-usage-list)))

;;;;;; Macro to access the argument register the Y-reg is already in.
(defmacro sel-x-from-usage-entry (x)
 `(cadr ,x))


(defmacro d_yreg_assoc (yreg alist)
 `(setq ,alist (del_yreg_assoc ,yreg ,alist))
)

(defun del_yreg_assoc (yreg alist)
 (cond 	((null alist) nil)
	((equal yreg (caar alist)) (del_yreg_assoc yreg (cdr alist)))
	(t (cons (car alist) (del_yreg_assoc yreg (cdr alist))))
 )
)

(defmacro d_xreg_assoc (xreg alist)
 `(setq ,alist (del_xreg_assoc ,xreg ,alist))
)

(defun del_xreg_assoc (xreg alist)
 (cond 	((null alist) nil)
	((equal xreg (cadar alist)) (del_xreg_assoc xreg (cdr alist)))
	(t (cons (car alist) (del_xreg_assoc xreg (cdr alist))))
 )
)

