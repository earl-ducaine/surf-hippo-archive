;-------------------------------------------------------------------------------------
;;;                       MK-TERM*CLAS
;;; Eingabe <term> Der Term der in eine Klasse eingeordnet werden soll.
;;;
;;; Wert  (<type> <value>)
;;;        <type>   Beschreibt die Klasse des Terms 
;;;        <value>  Der Term wird gegebenfalls modifiziert zurueckgegeben
;;;                            
;;;  Ordnet die Terme in Klassen ein! Statische Bindung der Variablen werden erkannt
;;;  (z. Z. nur Bindungen an Konstanten) und in *VAR-BINDINGS* vermerkt. (Siehe auch 
;;;  unter get-next*guards.) Zusaetzlich wird eine Normierung des IS-Primitivs vorgenommen. 

(defun mk-term*clas (term)
  (declare (special *VAR-BINDINGS*))
  (let* ((lc*functor (s-first term))
         (lc*arglist (s-rest term)))
    (cond ((eq lc*functor 'is)
           (let ((lc*lhs (s-first lc*arglist))
                 (lc*rhs (s-second lc*arglist)))
             (cond ((not (passive*term-p lc*rhs))            ;aktives IS-Primitiv
                    (list 'active_is (list (s-second (mk-term*clas lc*rhs))
                                           (list 'refl-Xreg lc*lhs))))
                   ((equal lc*rhs lc*lhs)                    ;(is _any _any)
                    (list 'passive_term lc*lhs))
                   ((constant-p lc*lhs)  
                    (cond ((constant-p lc*rhs)
                           (list 'fail 'unknown))
                          (t
                           (progn 
                             (if (variable-p lc*rhs)
                               (setq *var-bindings* (mk-binding lc*rhs
                                                                     lc*lhs
                                                                     *var-bindings*)))
                             (list 'builtin 
                                   (list 'is lc*rhs lc*lhs))))))
                   ((constant-p lc*rhs)
                    (progn 
                      (if (variable-p lc*lhs)
                        (setf *var-bindings* (mk-binding lc*lhs
                                                         lc*rhs
                                                         *var-bindings*)))
                      (list 'builtin term)))
                   (t (list 'builtin term)))))
          ((eq lc*functor 'Refl-Xreg)
           (list 'builtin term))
          ((eq lc*functor 'cl)
           (list 'lispcall (mk-lisp*call (s-first term) term)))
;;;;;;;; changed by MH 25.06.92 to treat the WAM-builtins
;;;;;;;          (((lisp*builtin-p lc*functor)
	  ((and (lisp*builtin-p lc*functor)
		(not (WAM*builtin-p lc*functor)))
           (list 'lispcall (mk-lisp*call lc*functor term)))
          (t (list 'call_lit term)))))

;;;;; inserted by MH 25.06.92
;;;;; functions which are treated in the interpreter as lisp-accesses
;;;;;           but in the emulator as WAM-programs
(defun WAM*builtin-p (functor)
  (declare (special *WAM-builtins*))
  (member functor *WAM-builtins*))
