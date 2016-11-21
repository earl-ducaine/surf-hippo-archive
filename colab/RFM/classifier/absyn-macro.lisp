;;;       
;;;             absyn.macro
;;;
;;; Definition der abstrakten Syntax.
;;;  Selektoren, Konstruktoren, Praedikate als Macro-Definitionen realisiert.
;;;  
;;; Thomas Krause
;;; Universitaet Kaiserslautern
;;; DFKI ARC-TEC Teil C

(defmacro addprop (symbol indicator value)
  `(putniv ,symbol ,indicator (cons ,value (get ,symbol ,indicator))))

(defmacro variable-p (argument)
  `(and (listp ,argument)
        (eq 'vari (car ,argument))))


;;;
;;;      1. SELEKTOREN
;;;


;;;
;;;    Allgemeine Selektoren
;;;

(defmacro s-first (x)
   `(car ,x))

(defmacro s-second (x)
   `(cadr ,x))

(defmacro s-third (x)
   `(caddr ,x))

(defmacro s-fourth (x)
   `(cadddr ,x))

(defmacro s-fifth (x)
   `(s-fourth (cdr ,x)))

(defmacro s-rest (x)
    `(cdr ,x))

(defmacro s-last (l)
  `(s-first (reverse ,l)))

(defun s-max*in*list (l)
  (cond ((null l) 0)
        (t (apply #'max l))))

(defun s-min*in*list (l)
  (cond ((null l) 0)
        (t (apply #'min l))))

;;;
;;;  Zugriff auf die Elemente der  Variablenbeschreibungsstruktur
;;;


(defmacro s-variable*list ()
  `(get 'arguments 'variable))

(defmacro s-perm*list ()
  `(get 'arguments 'perm))

(defmacro s-temp*list ()
  `(get 'arguments 'temp))


(defmacro s-head*const*occur ()
  `(get 'arguments 'head*const))


(defmacro s-var*name (variable)
 `(if (variable-p ,variable)
                        (s-second ,variable)
                        ,variable))

(defmacro s-var*descr (var*withDescr)
  `(s-second ,var*withDescr))

(defmacro s-perm*var*descr (variable perm*var*list)
  `(s-second (assoc (s-var*name ,variable)  ,perm*var*list :test #'equal)))

(defmacro s-temp*var*descr (variable temp*var*list)
  `(s-second (assoc (s-var*name ,variable) ,temp*var*list :test #'equal)))

(defmacro s-reg (variable)
  `(get  (s-var*name ,variable) 'reg))

(defmacro s-reg*state (variable)
  `(get  (s-var*name ,variable) 'reg*state))

(defmacro s-last*chunk (variable)
 `(get (s-var*name ,variable) 'last*chunk))

(defmacro s-first*chunk (variable)
 `(get (s-var*name ,variable) 'first*chunk))

(defmacro s-use*head (variable)
 `(get (s-var*name ,variable) 'use*head))

(defmacro s-use*chunk (variable)
  `(get (s-var*name ,variable) 'use*chunk))

(defmacro s-use*chunk*call*lit (variable nr*of*chunk)
        `(s-second (assoc ,nr*of*chunk (s-use*chunk ,variable))))

(defmacro s-first*arg (variable)
  `(get (s-var*name ,variable) 'first*arg))

(defun s-oc*in*chunk (variable)
  (get (s-var*name variable) 'oc*in*chunk))

(defun s-last*ChunkLit (variable)
  (get (s-var*name variable) 'last*chunkLit))

;;; LOKALE VARIABLENBESCHREIBUNG

(defmacro s-saveness*from*cfc (var*cfc)
  `(s-second (s-var*descr ,var*cfc)))

(defmacro s-var_clas*from*cfc (var*cfc)
  `(s-third (s-var*descr ,var*cfc)))

(defmacro s-occurrence (var*descr)
  `(s-first ,var*descr))

(defmacro s-occurrence*from*cfc (var*cfc)
  `(s-occurrence (s-var*descr ,var*cfc)))

(defmacro s-saveness (variable)
 `(get (s-var*name ,variable) 'saveness))




;;; 
;;; Selektoren fuer die Klausel und den chunks
;;;

(defmacro s-chunk*term*list (chunk)
  `(s-second ,chunk))

(defmacro s-chunk*call*lit (chunk &optional (with*cfc t))
  `(let ((lc*literal (s-last (s-chunk*term*list ,chunk))))
     (if ,with*cfc
       lc*literal
       (if (passive*term-p lc*literal)
         lc*literal
         (if (variable-p (s-first lc*literal))
           (s-first lc*literal)
           (s-second lc*literal))))))

(defmacro s-cl2*head (clause)
  `(s-first ,clause))

(defmacro s-cl1*body (clause)
  `(s-rest ,clause))

(defmacro s-cl2*body (clause)
  `(s-rest ,clause))


(defmacro s-lit*descr (lit*cfc) ; cfc means classification
  `(s-second ,lit*cfc))


(defmacro s-arity (lit*descr)
  `(cond ((null ,lit*descr) 0)
        (t (s-first ,lit*descr))))

(defmacro s-env*size (lit*descr)
  `(s-second ,lit*descr))

(defmacro s-var*occur (variable)
  `(get (s-var*name ,variable) 'var*occur))

(defmacro s-occur*in*chunk (variable)
`(get (s-var*name ,variable) 'oc*in*chunk))

(defmacro s-arg*seq*const ()
  `(get 'arg*seq 'const))


(defmacro s-arg*seq*perm ()
  `(get 'arg*seq 'perm))


(defmacro s-arg*seq*temp ()
  `(get 'arg*seq 'temp))


(defmacro s-arg*seq*multi*use*perm ()
  `(get 'arg*seq 'multi*use*head*perm))


(defmacro s-arg*seq*multi*use*temp ()
  `(get 'arg*seq 'multi*use*head*temp))


(defmacro s-arg*seq*mark*multi*use*temp ()
  `(get 'arg*seq 'mark*multi*head*temp))



;;;
;;;     2. KONSTRUKTOREN   
;;;

(defmacro mk-new*temp*var( temp*var)
  `(addprop 'arguments 'temp ,temp*var))

(defmacro mk-new*perm*var( perm*var)
  `(addprop 'arguments 'perm ,perm*var))



;---------------------------------------------------------------------------------
;;;                         MK-VAR*INFO
;;;  Die Variable <variable> hat ihr erstes Vorkommen in einem chunk-literal. 
;;;  Es gilt also insbesondere, dass die Variable nicht im Kopf der Klausel vor-
;;;  kommt also UNSAFE ist.
;;;
;;;  Eingabe: <variable>      : Die Variable in der Form (vari name)
;;;           <nr*of*arg>     : Die Argumentstelle im Literal
;;;           <nr*of*chunk>   : Die Nummer des chunks (head-chunk = 0).
;;;           <nr*of*chunkLit>: Die Literalnummer relativ zum chunk-Anfang.

(defmacro mk-var*info (variable nr*of*arg nr*of*chunk nr*of*chunkLit) 
  `(let ((lc*var*name (s-var*name ,variable))
         (lc*head*chunk (head-p ,nr*of*chunk)))
     (addprop 'arguments 'variable lc*var*name)
     (putniv lc*var*name 'first*chunk ,nr*of*chunk)
     (putniv lc*var*name 'first*chunkLit ,nr*of*chunkLit)
     (putniv lc*var*name 'first*arg ,nr*of*arg)
     (putniv lc*var*name 'last*chunk ,nr*of*chunk)
     (putniv lc*var*name 'last*chunkLit ,nr*of*chunkLit)
     (if lc*head*chunk
       (progn (putniv lc*var*name 'saveness 'safe)   ; Variable im Kopf der Klausel
              (putniv lc*var*name 'use*head (list ,nr*of*arg)))
       (progn (putniv lc*var*name 'saveness 'unsafe)
              (mk-add*chunk*use lc*var*name ,nr*of*chunk ,nr*of*arg)))
     (putniv lc*var*name 'oc*in*chunk (list ,nr*of*chunk))))


;---------------------------------------------------------------------------------
;;;                         MK-ADD-INFO
;;;  Bei jedem weiteren Auftreten der Variable wird dessen Informations-Liste
;;;  aktualisiert.

(defmacro mk-add*info (variable nr*of*arg nr*of*chunk nr*of*chunkLit) 
`(let ((lc*var*name (s-var*name ,variable))
       (lc*head*chunk (head-p ,nr*of*chunk)))
   (putniv lc*var*name 'last*chunk ,nr*of*chunk)
   (putniv lc*var*name 'last*chunkLit ,nr*of*chunkLit)
   (if lc*head*chunk
     (addprop lc*var*name 'use*head ,nr*of*arg)
     (mk-add*chunk*use lc*var*name ,nr*of*chunk ,nr*of*arg))
   (if (member ,nr*of*chunk (s-occur*in*chunk lc*var*name))
     nil
     (addprop lc*var*name 'oc*in*chunk ,nr*of*chunk))))

(defmacro mk-set*Xreg (temp*var value)
  `(putniv (s-var*name ,temp*var) `reg ,value))

(defmacro mk-reg*state (temp*var state)
  `(putniv (s-var*name ,temp*var) `reg*state ,state))




;--------------------------------------------------------------------------------------
;;;                        MK-VAR*OCCUR
;;; Behandelt die occurence einer Variablen (FIRST/NON_FIRST). Die Funktion wird in 
;;; Pass 2 aufgerufen. Ist eine Variable FIRST, wird sie im folgenden auf NON-FIRST ge-
;;; setzt.

(defmacro mk-var*occur (variable occur)
  `(let ((lc*var*name (s-var*name ,variable)))
    (putniv lc*var*name 'var*occur ,occur)))
                         

(defmacro mk-occurence (variable)
  `(let* ((lc*var*name (s-var*name ,variable))
          (lc*occur (s-var*occur lc*var*name)))
     (if (eq 'nonfirst lc*occur)
       lc*occur
       (progn (mk-var*occur ,variable 'nonfirst)
              'FIRST))))


;---------------------------------------------------------------------------------
;;;                           MK-SAVENESS
;;;  Die saveness einer Variablen wird ermittelt. Ist dies das letzte Literal im
;;;  chunk, indem die Variable vorkommt, wird sie globalisiert. Fuer diesen Fall
;;;  wechselt die saveness auf SAFE

(defmacro mk-saveness (variable nr*of*chunk nr*of*chunkLit &optional (in*term nil))
  `(let* ((lc*var*name (s-var*name ,variable))
          (lc*saveness (s-saveness lc*var*name)))
     (cond (,in*term
            (putniv lc*var*name 'saveness 'global))
           ((and (not (eq lc*saveness 'global))
                 (eq (s-last*chunk lc*var*name) ,nr*of*chunk)
                 (eq (s-last*chunkLit lc*var*name) ,nr*of*chunkLit))
            (putniv lc*var*name 'saveness 'safe)))
     lc*saveness))


(defmacro mk-lit*arity (arg*list)
  `(length ,arg*list))

(defmacro mk-var*classification (var*name occurrence saveness var*class)
  `(list ,var*name
     (list ,occurrence ,saveness ,var*class)))

(defmacro mk-perm*var*descr (var*name Y*reg use*head last*chunk last*chunkLit)
  `(list (list 'vari ,var*name) 
         (list ,Y*reg 
               ,use*head 
               (list ,last*chunk ,last*chunkLit))))

(defmacro mk-temp*var*descr (var*name X*reg use*head use*chunklit)
  `(list (list 'vari ,var*name)
         (list  ,X*reg 
                ,use*head 
                ,use*chunklit)))

#|
(defmacro mk-procedure (procedure*name  clause*count list*of*clause)
  `(cons 'proc 
         (cons ,procedure*name
               (cons ,clause*count 
                     ,list*of*clause))))
|#

(defmacro mk-procedure (procedure*name clause*count list*of*clause)
  ; INDEXING -- added by real-fun
  `(cons 'proc
         (cons ,procedure*name
               (cons ,clause*count
                     (cons (mk-index-struct ,procedure*name  ;; real-fun
                                           ,clause*count
                                            ,list*of*clause)
                           ,list*of*clause)))))



(defmacro mk-clause*classification ( cl*type  perm*list  temp*list  lit*list)
  `(append (list ,cl*type ,perm*list ,temp*list)
          ,lit*list))


(defmacro mk-lit*descr (lit*arity  lit*env*size arg*seq is*head)
  `(list ,lit*arity ,lit*env*size
              (if ,is*head 
                (append (reverse (s-first ,arg*seq))  ; Konstanten
                        (s-second ,arg*seq)           ; Permantente Var. mehrmals im Kopf
                        (reverse (s-third ,arg*seq))  ; Temporaere Var.      --- " ----
                        (reverse (s-fourth ,arg*seq)) ; restliche permanente Var
                        (reverse (s-fifth ,arg*seq))) ; restliche temporaere Var.
                (append (s-second ,arg*seq)           ; Permantente Var. mehrmals im Kopf
                        (reverse (s-fourth ,arg*seq)) ; restliche permanente Var
                        (reverse (s-first ,arg*seq))  ; Konstanten
                        (reverse (s-third ,arg*seq))  ; Temporaere Var. mehrmals im Kopf
                        (reverse (s-fifth ,arg*seq)))))) ; restliche temporaere Var



(defmacro mk-cl1*lit (lit*name lit*arg*list)
  `(cons ,lit*name  ,lit*arg*list))
  
(defmacro mk-cl2*clause (chunk  list*of*chunk)
  `(cons ,chunk  ,list*of*chunk))


(defmacro mk-cl2*lit (lit*name lit*arg*list  lit*descr)
  `(list 'usrlit (cons ,lit*name  ,lit*arg*list) ,lit*descr))

(defmacro mk-cl2*const*descr (c*name c*list c*arg*list p*arg*list t*arg*list)
  `(cons ,c*name (list (list ,c*list ,c*arg*list ,p*arg*list ,t*arg*list))))

(defmacro mk-cl2*var*descr (var*name occur saveness var*class)
  `(cons ,var*name (list (list ,occur ,saveness ,var*class))))


(defmacro mk-add*arg*seq (indicator value)
  `(addprop 'arg*seq ,indicator ,value))

(defmacro mk-clear*head*const*list ()
  `(remprop 'arguments 'head*const))

(defmacro mk-add*list (l x)
   `(cons ,x ,l))
