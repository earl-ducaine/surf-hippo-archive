;-------------------------------------------------------------------------------------
;;;                       MK-ADD*CHUNK*USE
;;; Eingabe: <variable>    Variable oder Variablenname
;;;          <nr*of*chunk> Die Nummer des Chunks
;;;          <nr*of*arg>   Argumentnummer im Chunk-Literal 
;;;  
;;; Fuer Variablen die im aktiven Literal eines chunks vorkommen, vermerkt diese
;;; Funktion, das Auftreten in der Variablenbeschreibungsstruktur

(defun mk-add*chunk*use (variable nr*of*chunk nr*of*arg)
  (let* ((lc*var*name (s-var*name variable))
         (lc*use*chunk*list (s-use*chunk lc*var*name))
         (lc*use*chunk*lit (s-second (assoc nr*of*chunk lc*use*chunk*list))))
    (putniv lc*var*name 'use*chunk (mk-modify*list 
                                     nr*of*chunk 
                                     (cons nr*of*arg lc*use*chunk*lit)
                                     lc*use*chunk*list
                                     nil))))


;-------------------------------------------------------------------------------------
;;;                      S-TEMPVAR*USE*CHUNK*LIT
;;;  Eingabe: <temp*variable> Temporaere Varable in der Form <(vari name)> oder <name>
;;;  Wert (<arg_pos1> ... <arg_posN>)
;;;
;;;  Es wird eine Liste zurueckgegeben, die die Vorkommen der Variablen im aktiven
;;;  Literal des chunks angibt.

(defun s-tempvar*use*chunk*lit (temp*variable)
  (let ((lc*occur*chunk (s-max*in*list (s-oc*in*chunk (s-var*name temp*variable)))))
      (if (head-p lc*occur*chunk)
        nil
        (s-use*chunk*call*lit temp*variable lc*occur*chunk))))



;-------------------------------------------------------------------------------------
;;;                      GET-NEXT*GUARDS
;;; Eingabe <term*rest>   Liste von Termen 
;;;         <guard*list>  Liste von schon vorhandenen Guards
;;;
;;; Wert (<term*rest> <guard*list>)
;;;          <term*rest> Liste von Terme die nicht mehr betrachtet wurden sind.
;;;                      Sie ist entweder leer oder beginnt mit dem CALL_LIT des
;;;                      aktuellen chunks.
;;;         <guard*list> Liste von guards zum aktuellen chunk.                     
;;;
;;; Ermittelt die naechsten guards und das chunk-Ende.
;;; Guards, die keine Wirkung haben (z. B. eine Konstante mitten in der Klausel)
;;; werden ignoriert (Ÿberlesen). 
;;; Die Variable *VAR-BINDINGS* enthaelt aktuelle Variablenbindungen, die statisch zur
;;; Compilezeit festgestellt werden konnten. Sie wird in CLASSIFY-CLAUSE als special
;;; deklariert und ist somit "lokal globalisiert wurden"
;;;

(defun get-next*guards (term*list guard*list)
  (declare (special *VAR-BINDINGS*))
  (cond ((null term*list) 
         (list nil (reverse guard*list)))
        (t (let ((lc*term (rewrite-term 
                           (s-first term*list)
                           *VAR-BINDINGS*))
                 (lc*term*rest (s-rest term*list)))
             (cond ((eq lc*term 'unknown)
                    (get-next*guards nil (cons 'unknown guard*list)))
                   ((passive*structure-p lc*term)
                    (get-next*guards lc*term*rest 
                                     (if (null lc*term*rest)
                                       (cons lc*term guard*list)
                                       guard*list)))
                   (t (let* ((lc*fun2 (mk-term*clas lc*term))
                             (lc*tag (s-first lc*fun2))
                             (lc*value (s-second lc*fun2))) 
                        (case lc*tag
                          (fail          (get-next*guards nil 
                                                         (cons 'unknown 
                                                               guard*list)))
                          (passive_term  (get-next*guards lc*term*rest 
                                                           (if (null lc*term*rest)
                                                             (cons lc*value
                                                                   guard*list)
                                                             guard*list)))
                          (builtin       (get-next*guards lc*term*rest
                                                         (cons lc*value guard*list)))
                          (lispCall      (list (cons lc*value lc*term*rest) 
                                              (reverse guard*list)))
                          (active_is     (list (append lc*value lc*term*rest) 
                                              (reverse guard*list)))
                          (call_lit      (list (cons lc*value lc*term*rest) 
                                              (reverse guard*list)))))))))))


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
          ((lisp*builtin-p lc*functor)
           (list 'lispcall (mk-lisp*call lc*functor term)))
          (t (list 'call_lit term)))))


;--------------------------------------------------------------------------------------
;;;                              passive*term-p
;;; Eingabe TERM Ein gueltiger Term im Sinne von Datafun.
;;;
;;; Liefert den Wert t, falls es sich um kein aktives goal handelt.
;;;
                      
(defun passive*term-p (term)
  (cond ((atom term) t)
        (t (let ((lc*functor (s-first term)))
             (or
              (eq 'is lc*functor)
              (eq 'vari lc*functor)
              (eq 'refl-Xreg lc*functor)
              (eq 'inst lc*functor)
              (eq 'tup lc*functor))))))

;--------------------------------------------------------------------------------------
(defun lisp*builtin-p (functor)
  (declare (special *lisp-functions* *lisp-extras* *lisp-predicates* *relfun-extras*))
  (or (member functor *lisp-functions*)
      (member functor *lisp-predicates*)
      (member functor *lisp-extras*)
      (member functor *relfun-extras*)))

;--------------------------------------------------------------------------------------
;;;                           MK-VAR*GUARD*OCCUR
;;; Eingabe <variable>    Das aktuelle Argument ist eine Variable.
;;;         <nr*of*chunk> Die Nummer des chunks.
;;;         <nr*of*chunkLit> Die Nummer des Literals im chunk.
;;;         <in*struc>  Flag fuer vorkommen innerhalb einer Srtuktur.
;;;
;;; Wert    <variable>
;;;
;;; Per Seiteneffekt wird fuer die Variablen das Vorkommen im chunk auf den Variablen-
;;; beschreibungsstrukturen vermerkt.

(defun mk-var*guard*occur (variable nr*of*chunk nr*of*chunkLit)
  (let ((lc*var*name (s-var*name variable)))
    (putniv lc*var*name 'last*chunkLit nr*of*chunklit)
    (putniv lc*var*name 'occur*in*guard t)
    (putniv lc*var*name 'last*chunk nr*of*chunk)
    (if (exist*var*info-p variable)
      (if (member nr*of*chunk (s-oc*in*chunk lc*var*name))
        nil
        (addprop lc*var*name 'oc*in*chunk nr*of*chunk))

      (progn  ; ELSE-Zweig
        (addprop 'arguments 'variable lc*var*name)
        (putniv lc*var*name 'first*chunk nr*of*chunk)
        (putniv lc*var*name 'first*chunkLit nr*of*chunklit)
        (putniv lc*var*name 'saveness 'unsafe)
        (putniv lc*var*name 'oc*in*chunk (list nr*of*chunk)))
      )))


;--------------------------------------------------------------------------------------
;;;                           MK-CL2*HEAD*CHUNK
;;; Eingabe   <head*literal*cfc>  Das klassifizierte Kopfliteral der Klausel
;;;           <chunk*cfc>         Der klassifizierte Kopfchunk indem das Kopfliteral
;;;                               eingefuegt werden muss.
;;;
;;;  Wert  <chunk*cfc>  Der vollstaendig klassifizierte Kopfchunk.

(defun mk-cl2*head*chunk (head*literal*cfc chunk*cfc)
  (list 'chunk
        (cons head*literal*cfc
              (s-second chunk*cfc))
        (s-third chunk*cfc)))



;--------------------------------------------------------------------------------------
;;;                             MK-CHUNK*DESCR
;;; Eingabe <list*of*guard> Eine Liste der chunk-guards. Fuer die chunk-Beschreibung
;;;                         spielt nur der letzte Term eine Rolle, falls es sich um
;;;                         aktiven Term handelt.
;;;         <call*lit>      Das aktive Literal (Term) im chunk.
;;;         <nr*of*chunk>   Die Nummer des chunks.
;;;
;;; Wert     <chunk*cfc>    Der klassifizierte chunk.

(defun mk-chunk*descr (list*of*guard call*lit nr*of*chunk)
  (declare (special *next-chunkreg*))
  (let ((lc*lastReg (update-lastReg (s-next*chunkReg nr*of*chunk *next-chunkreg*) (s-new*freelist))))
    (cond ((null call*lit) 
           (list 'chunk list*of*guard (list lc*lastReg nil)))
        (t ; Der chunk besitzt ein aktiven Term.
         (list 'chunk (reverse (cons call*lit
                              (reverse list*of*guard)))
              (list lc*lastReg
                    (s-all*perm*occur*in*CalLit nr*of*chunk)))))))


(defun update-lastReg (old*reg free*regs)
  (cond ((member old*reg free*regs)
         (update-lastReg (1- old*reg) free*regs))
        (t old*reg)))

;--------------------------------------------------------------------------------------
;;;                               S-ALL*PERM*OCCUR*IN*CALLIT
;;;  Eingabe <nr*of*chunk>  Durch die Nummer des chunks ist das Literal eindeutig be-
;;;                         schrieben, da ein chunk nur ein call-Literal haben kann.
;;;
;;;  Wert ((var1 (v-pos1_1 ... v-pos1_N)) ... (varM (v-posM_1 ... v-posM_K)))

(defun s-all*perm*occur*in*calLit (nr*of*chunk)
  (s-all*perm*occur*in*calLit2 (s-perm*list)
                           nr*of*chunk
                           nil))


(defun s-all*perm*occur*in*calLit2 (perm*var*list nr*of*chunk value)
  (cond ((null perm*var*list) value)
        (t 
         (let* ((lc*cur*permVar (s-first perm*var*list))
                (lc*rest*permVar (s-rest perm*var*list))
                (lc*argPos*list (s-use*chunk*call*lit lc*cur*permVar nr*of*chunk)))
           (cond ((null lc*argPos*list)
                  (s-all*perm*occur*in*calLit2 lc*rest*permVar nr*of*chunk value))
                 (t
                  (s-all*perm*occur*in*calLit2 lc*rest*permVar 
                                               nr*of*chunk
                                               (cons (list (list 'vari lc*cur*permVar)
                                                           lc*argPos*list)
                                                     value))))))))

