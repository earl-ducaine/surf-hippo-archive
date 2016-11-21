
;-----------------------------------------------------------------------------
;;;                      CL1-CLAUSE
;;;            Pass 1 der Klauselklassifizierung
;;;
;;; Eingabe: <clause> Die zu klassifizierende Klausel.
;;;
;;; Wert:    <cl1*clause> Die Klausel, gegliedert in chunks und simplifiziert. 

(defun cl1-clause (clause)
  (let ((lc*clause*head (s-first clause))
        (lc*clause*body (s-rest clause)))
    (mk-cl1*clause
          (cl1-head lc*clause*head)
          (cl1-body lc*clause*body))))



;---------------------------------------------------------------------------
;;;                      CL1-HEAD

(defun cl1-head (head*lit)
  (declare (special *usrlit-arity*))
  (let* ((lc*head*name (s-first head*lit))
         (lc*arg*list (s-rest head*lit)))
    (progn (setq *usrlit-arity* (List (list 0 
                                            (length lc*arg*list))))
           (mk-cl1*lit lc*head*name
                       (cl1-arglist lc*arg*list 0 0)))))


;---------------------------------------------------------------------------
;;;                     CL1-BODY

(defun cl1-body (list*of*term)
  (cl1-chunk list*of*term 1))



;---------------------------------------------------------------------------
;;;                     CL1-CHUNK

(defun cl1-chunk (term*list nr*of*chunk)
  (cond ((null term*list)
         nil)
        (t
         (let* ((lc*fun2 (get-next*guards term*list nil)) ;(<t*rest> <guard*list>)
                (lc*guard*list (s-second lc*fun2))
                (lc*call*lit (s-first (s-first lc*fun2)))
                (lc*term*rest (s-rest (s-first lc*fun2))))
           (cons (append (cl1-guard*list lc*guard*list nr*of*chunk 1)
                         (cl1-call*lit lc*call*lit
                                       nr*of*chunk 
                                       (+ 1 (length lc*guard*list))))
                 (cl1-chunk lc*term*rest (+ nr*of*chunk
                                            1)))))))

(defun cl1-call*lit (call*lit nr*of*chunk nr*of*chunkLit)
  (declare (special *usrlit-arity*))
  (cond ((null call*lit) 
         nil)
        (t
         (let ((lc*functor (s-first call*lit)))
           (if (explicit*lispcall-p lc*functor)
             (mk-cl1*lispcall lc*functor 
                              (cl1-call*lit (s-second call*lit) nr*of*chunk nr*of*chunkLit))
             (let  ((lc*arglist (s-rest call*lit)))
               (setq *usrlit-arity* (cons (list nr*of*chunk (length lc*arglist))
                                          *usrlit-arity*))
               (list (cons  lc*functor
                            (cl1-arglist (s-rest call*lit)
                                         nr*of*chunk nr*of*chunkLit)))))))))

(defun cl1-guard*list (g*list nr*of*chunk nr*of*chunkLit)
  (cond ((null g*list )
         nil)
        (t
         (cons (cl1-guard (s-first g*list)
                          nr*of*chunk nr*of*chunkLit)
               (cl1-guard*list (s-rest g*list)
                               nr*of*chunk
                               (+ 1 nr*of*chunkLit))))))

;--------------------------------------------------------------------------------
;;;                             CL1-GUARD
;;;
;;; Eingabe  <guard>          Ein Guard-Term oder ein Guard-Argument.
;;;          <nr*of*chunk>    Die Nummer des aktuellen chunks.
;;;          <nr*of*chunkLit> Die Nummer des Literals realtiv zum chunk-Anfang
;;;
(defun cl1-guard (guard nr*of*chunk nr*of*chunkLit &optional (in*struc nil))
  (declare (special in*struc))
  (cond ((constant-p guard)
         guard)
        ((variable-p guard)
         (progn (mk-var*guard*occur guard nr*of*chunk nr*of*chunkLit)
                guard))
        (t
         (let ((lc*functor (s-first guard)))
           (cond ((eq 'is lc*functor)
                  (cl1-is*primitive guard nr*of*chunk nr*of*chunkLit))
                 ((eq 'refl-Xreg lc*functor)
                  (cl1-refl*primitive guard nr*of*chunk nr*of*chunkLit))
                 ((eq 'inst lc*functor)
                  (cl1-structure guard nr*of*chunk nr*of*chunkLit))
                 ((eq 'tup lc*functor)
                  (cl1-tupel guard nr*of*chunk nr*of*chunkLit))
                 ((explicit*lispcall-p lc*functor)
                  (cl1-LispCall guard nr*of*chunk nr*of*chunkLit))
                 (t ; Fehler
                  (progn (princ "CL1-GUARD: Arg1 =")
                         (pprint guard)
                         (break))))))))


;--------------------------------------------------------------------------------------
;;;                            CL1-IS*PRIMITIVE
;;;

(defun cl1-is*primitive (is*prim  nr*of*chunk nr*of*chunkLit)
  (let ((lhs (s-second is*prim))
        (rhs (s-third is*prim)))
        (cl1-guard lhs nr*of*chunk nr*of*chunkLit)
        (cl1-guard rhs nr*of*chunk nr*of*chunkLit)
          is*prim))

;--------------------------------------------------------------------------------------
;;;                            CL1-REFL*PRIMITIVE
;;;

(defun cl1-refl*primitive (refl*prim nr*of*chunk nr*of*chunkLit)
  (progn
    (cl1-guard (s-second refl*prim)
                          nr*of*chunk nr*of*chunkLit)
      refl*prim))

;--------------------------------------------------------------------------------------
;;;                           CL1-STRUCTURE
;;;

(defun cl1-structure (struc  nr*of*chunk nr*of*chunkLit)
  (let ((lc*structerm (s-second struc)))
    (if (passive*structure-p lc*structerm)
      (cl1-guard lc*structerm nr*of*chunk nr*of*chunkLit)
      (list 'inst (cons (s-first lc*strucTerm)
                        (cl1-struc*argList (s-rest lc*structerm) 
                                           nr*of*chunk nr*of*chunkLit))))))
  

;--------------------------------------------------------------------------------------
;;;                           CL1-TUPEL
;;;

(defun cl1-tupel (tupel  nr*of*chunk nr*of*chunkLit)
  (cons 'tup (cl1-tup*argList (s-rest tupel)
                              nr*of*chunk nr*of*chunkLit)))

;--------------------------------------------------------------------------------------
;;;                           CL1-LISPCALL
;;;

(defun cl1-lispCall (lispCall  nr*of*chunk nr*of*chunkLit)
  (let ((lc*lisp*class (s-first (lispCall)))
        (lc*lispExpression (s-second lispCall)))
    (list lc*lisp*class (cons (s-first lc*lispExpression)
                    (cl1-cl*argList (s-rest lc*lispExpression)
                              nr*of*chunk nr*of*chunkLit)))))





;---------------------------------------------------------------------------
;;;                  CL1-ARGLIST

(defun cl1-arglist (arg*list nr*of*chunk nr*of*chunkLit)
  (cl1-arglist2 arg*list 1 nr*of*chunk nr*of*chunkLit))


(defun cl1-arglist2 (arg*list nr*of*arg nr*of*chunk nr*of*chunkLit)
  (cond ((null arg*list) nil)
	(t (let* ((lc*current*arg (s-first arg*list))
	          (lc*arg*rest    (s-rest arg*list)))
             (cons (cl1-arg lc*current*arg nr*of*arg 
                            nr*of*chunk nr*of*chunkLit)
                   (cl1-arglist2 lc*arg*rest (1+ nr*of*arg)
                                 nr*of*chunk nr*of*chunkLit))))))

(defun cl1-arg (arg nr*of*arg nr*of*chunk nr*of*chunkLit)
  (cond ((constant-p arg) (cl1-constant arg))
        ((variable-p arg) (cl1-variable arg  nr*of*arg 
                                        nr*of*chunk nr*of*chunkLit))
        (t (progn (print arg)
                  (error "cl1-arg ->")))))


(defun cl1-constant (arg)
  arg)


;------------------------------------------------------------------------------------
;;;                      CL1-VARIABLE
;;;             Pass 1 der Variablenklassifizierung
;;;
;;; Fuer die Variable >>VARIABLE<< wird die Chunknummer und Argumentstelle vermerkt.
;;; Dies geschieht per Seiteneffekt in der Funktion MK-ADD*INFO bzw. MK-VAR*INFO 
;;; Diese Funktion wir in Datafun nur fuer Funktionen aufgerufen, welche im aktiven
;;; Literal des chunks oder im Kopfliteral auftreten.


(defun cl1-variable (variable nr*of*arg nr*of*chunk nr*of*chunkLit)
  (cond ((exist*var*info-p variable)
         (mk-add*info variable nr*of*arg 
                      nr*of*chunk nr*of*chunkLit))
        (t (mk-var*info variable nr*of*arg 
                        nr*of*chunk nr*of*chunkLit)))
 variable)


;---------------------------------------------------------------------------
;;;                  CL1-STRUC*ARGLIST

(defun cl1-struc*argList (arg*list nr*of*chunk nr*of*chunkLit)
  (cond ((null arg*list)
         nil)
        (t (cons (cl1-guard (s-first arg*list) nr*of*chunk nr*of*chunkLit)
                 (cl1-struc*argList (s-rest arg*list) nr*of*chunk nr*of*chunkLit)))))

;---------------------------------------------------------------------------
;;;                  CL1-TUP*ARGLIST

(defun cl1-tup*argList (arg*list nr*of*chunk nr*of*chunkLit)
  (cl1-struc*argList arg*list nr*of*chunk nr*of*chunkLit))

;---------------------------------------------------------------------------
;;;                  CL1-CL*ARGLIST

(defun cl1-cl*argList (arg*list nr*of*chunk nr*of*chunkLit)
  (cl1-struc*argList arg*list nr*of*chunk nr*of*chunkLit))


