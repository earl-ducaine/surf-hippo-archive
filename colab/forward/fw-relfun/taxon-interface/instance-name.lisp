; File instance-name.lsp
;
; Author: Knut Hinkelmann
; Stand:  16.9.1991
;
;
; The Function MAKE-INSTANCE-NAME (defined as a lisp function in RELFUN)
; has as Input a list of Atoms and concatenates them to a new Lisp atom. 

(if (rf-terpri)
    t
    (defun rf-terpri ()
      (if (rfi-script-mode-p)
          (terpri *rfi-script-output*))
      (terpri *rfi-standard-output*)
      t)
)

(if *lisp-functions* (if (not (member 'make-instance-name *lisp-functions*))
                        (setq *lisp-functions* (cons 'make-instance-name
                                                     *lisp-functions*))))

(defun make-instance-name (&rest components)
  (let ((args (make-arg-list components)))
    (intern (apply #'concatenate (cons 'string args)))))

(defun make-arg-list (list-of-atoms)
  (cond ((null (cdr list-of-atoms)) (list (symbol-name (car list-of-atoms))))
	(t (cons (symbol-name (car list-of-atoms))
	         (cons "-" (make-arg-list (cdr list-of-atoms)))))))
