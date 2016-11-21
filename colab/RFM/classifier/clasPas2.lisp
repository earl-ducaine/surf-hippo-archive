
;-----------------------------------------------------------------------------------
;;;                      CL2-CLAUSE
;;;            Pass 2 der Klauselklassifizierung
;;;
;;; Eingabe: <LIST*OF*CHUNK> Liste der chunks, die die KLausel darstellen. Der erste
;;;                          chunk wird besonders behandelt.
;;;
;;; Wert:    <CLASSIFIED-CLAUSE> Liste der klassifizierten chunks der KLausel 
;;;


(defun cl2-clause (list*of*chunk)
  (mk-var*in*foot*struc (s-last list*of*chunk)) 
  (let* ((lc*head*chunk (s-first list*of*chunk))
         (lc*body*chunk*list (s-rest list*of*chunk))
         (lc*head*chunk*cfc (cl2-head*chunk lc*head*chunk)))
    (mk-cl2*clause lc*head*chunk*cfc
                   (cl2-body*chunk lc*body*chunk*list 2))))


;------------------------------------------------------------------------------------
;;;                            CL2-body*chunk
                

(defun cl2-body*chunk (list*of*chunk nr*of*chunk)
  (cond ((null list*of*chunk) nil)
        (t (cons (cl2-chunk (s-first list*of*chunk)
                            nr*of*chunk)
                 (cl2-body*chunk (s-rest list*of*chunk)
                                 (+ 1 nr*of*chunk))))))





;-----------------------------------------------------------------------------------
;;;                     CL2*HEAD*CHUNK
;;; Eingabe HEAD*CHUNK Der erste chunk in der KLausel.
;;;
;;; Wert   CHUNK_DESCR Der klassifizierte head*chunk.
;;;
;;;  Der erste chunk in einer Klausel ist ausgezeichnet und erfaehrt hier eine Spezial-
;;;  behandlung.

(defun cl2-head*chunk (head*chunk)
  (let ((lc*head*lit (s-first head*chunk))
        (lc*rest*term (s-rest head*chunk)))
    (mk-clear*reassign*list)
    (mk-clear*new*freelist)
    (mk-cl2*head*chunk (cl2-head lc*head*lit)
                       (cl2-chunk lc*rest*term 1)))) ; nr*of*chunk = 1


;-----------------------------------------------------------------------------------
;;;                      CL2-HEAD
;;; Eingabe <HEAD*LIT> Das Kopfliteral der Klausel. 
;;;         <CALL*LIT> Das aktive Literal der Klausel, wenn es mind. ein aktives Literal
;;;                    besitzt.
;;;
;;; Wert    <literal_classification> Das klassifizierte Kopfliteral.
;;;      
;;;  Die globale Variable *next-chunkreg* gibt für jedes chunk, die groesste von
;;;  temporaeren Variablen benutzte Registernummer an.


(defun cl2-head ( head*lit) 
  (declare (special *next-chunkreg*))
  (mk-clear*head*const*list)
  (init-lit)
  (let* ((lc*head*name (s-first head*lit))
         (lc*arg*list (s-rest head*lit))
         (lc*env*size (mk-env*size 0)) ; nr*of*chunk=0 fuer den Kopf der Klausel
         (lc*arity (s-head*arity))
         (lc*head*cfc (cl2-arglist nil lc*arg*list 0 0))
         (lc*code*list (mk-arg*seq)))
    (setq *next-chunkreg*
          (mk-set*all*Xreg (s-arg*seq 'mark*multi*use*temp)
                           *next-chunkreg*
                           (append (s-arg*seq 'const) 
                                   (s-arg*seq 'multi*use*head*perm))
                           nil ; Flag: A-Reg duerfen nicht ueberschrieben werden
                           ))
    (setq *next-chunkreg*
          (mk-set*all*Xreg (s-temp*list)
                           *next-chunkreg*
                           (append (s-arg*seq 'const) 
                                   (s-arg*seq 'multi*use*head*perm)
                                   (s-arg*seq 'multi*use*head*temp)
                                   (s-arg*seq 'perm))
                           ;t ; Flag: A-Reg sind jetzt wieder verfuegbar!
                           nil ; Optimierung wie oben macht Schwierigkeiten, wenn temp. Variablen
                               ; mit kleiner Stelligkeit im chunk vorkommen, aber nich im call-Literal
                           ))
    (mk-cl2*lit
            lc*head*name
            lc*head*cfc
            (mk-lit*descr lc*arity lc*env*size lc*code*list t))))


;-----------------------------------------------------------------------------------
;;;                          CL2-CHUNK
;;; Eingabe <chunk> Ein chunk bestehend aus beliebig vielen chunk*guards und hoechstens 
;;;                 einem aktiven Literal (USRLIT). 
;;;
;;; Wert  <chunk_classification> Der klassifizierte chunk.
;;;

(defun cl2-chunk (chunk nr*of*chunk)
  (cond ((null chunk)
         (list nil nil)) 
        (t (if (not (eql 1 nr*of*chunk))
             (progn (mk-clear*reassign*list)
                    (mk-clear*new*freelist)))
           (let* ((lc*revChunk (reverse chunk))
                 (lc*lastTerm (s-first lc*revChunk)))
             (if (passive*term-p lc*lastTerm)
               (mk-chunk*descr (cl2-chunk*guard*list chunk
                                                     nr*of*chunk
                                                     1) ;  nr*of*chunkLit = 1
                               nil
                               nr*of*chunk)
               (mk-chunk*descr (cl2-chunk*guard*list (reverse (s-rest lc*revChunk))
                                                     nr*of*chunk
                                                     1)
                               (cl2-lit lc*lastTerm
                                        nr*of*chunk
                                        (length chunk))
                               nr*of*chunk))
               ) ; END-IF
             )))

 


;-----------------------------------------------------------------------------------
;;;                           CL2-CHUNK*GUARD*LIST
;;; Eingabe <list*of*guard> Die Liste der chunk-guards fuer den chunk.
;;;         <nr*of*chunk>   Die Nummer des chunks
;;;
;;; Wert   <nr*of*chunkLit> Eine Liste der klassifizierten chunks.       


(defun cl2-chunk*guard*list (list*of*guard nr*of*chunk  nr*of*chunkLit)
  (cond ((null list*of*guard) nil)
        (t (cons (cl2-chunk*guard (s-first list*of*guard) 
                                  nr*of*chunk  nr*of*chunkLit)
                 (cl2-chunk*guard*list (s-rest list*of*guard)
                                       nr*of*chunk 
                                       (+ 1 nr*of*chunkLit))))))


;------------------------------------------------------------------------------------
;;;                           CL2-CHUNK*GUARD
;;; Eingabe <chunk*guard> Der zu klassifizierende chunk-guard
;;;         <nr*of*chunk> Die Nummer des chunks.
;;;         <nr*of*chunkLit> Nummer der Literals im chunk.
;;;
;;; Wert    <CHUNK*GUARD*CFC> Der klassifizierte chunk*guard. Hier werden nur die
;;;                           Argumente der guards klassifiziert, da noch keine
;;;                           Beschreibung fuer guards besteht.

(defun cl2-chunk*guard (chunk*guard nr*of*chunk nr*of*chunkLit)
  (cond ((eq chunk*guard 'UNKNOWN)
         chunk*guard)
        ((or (constant-p chunk*guard)
             (variable-p chunk*guard))
        (cl2-guard*arg chunk*guard 
                       nr*of*chunk nr*of*chunkLit))
        ((atom chunk*guard)
         (progn
           (princ "CL2-CHUNK*GUARD: chunk*guard hat den Wert:") 
           (print chunk*guard)   
           (terpri)
           (print "function: CL2-CHUNK*GUARD")
           (break)))
        (t (let ((lc*functor (s-first chunk*guard))
                 (lc*argList (s-rest chunk*guard)))
             (cond ((explicit*lispcall-p lc*functor)
                    (cl2-lispCall lc*functor (s-first Lc*argList)
                                  nr*of*chunk nr*of*chunkLit))   
                   (t 
                    (case lc*functor
                      (IS ; passive IS-primitive
                       (cl2-is*primitive (s-first lc*argList)    ; lhs-term
                                         (s-second lc*argList)   ; rhs-term
                                         nr*of*chunk nr*of*chunkLit)) 
                      (refl-Xreg  
                       (cl2-refl*primitive lc*argList
                                           nr*of*chunk nr*of*chunkLit))
                      (inst
                      (cl2-structure (s-first Lc*argList)
                                     nr*of*chunk nr*of*chunkLit))
                      (tup
                       (cl2-tupel Lc*argList 
                                  nr*of*chunk nr*of*chunkLit))
                      (OTHERWISE
                       (progn
                         (princ "CL2-CHUNK*GUARD: chunk*guard hat den Wert:")
                         (print chunk*guard)   
                         (terpri)
                         (error "CL2-CHUNK*GUARD"))))))))))



      
;-----------------------------------------------------------------------------------
;;;                             CL2-LIT
;;; Eingabe <literal>   Das aktive Literal in einem chunk.
;;;         <nr*of*chunk> Die Nummer des aktuellen chunks.
;;;         <nr*of*chunkLit> Nummer der Literals im chunk.
;;;
;;; Wert    <literal_classification> Das klasssifizierte Literal.

(defun cl2-lit (literal nr*of*chunk nr*of*chunkLit) 
  (let ((lc*functor (s-first literal)))
    (if (explicit*lispcall-p lc*functor)
      (let* ((lc*lisp*cfc (cl2-lit (s-second literal) nr*of*chunk nr*of*chunkLit))
             (lc*lispTerm (s-second lc*lisp*cfc))
             (lc*term*cfc (s-third lc*lisp*cfc))) 
        (list lc*functor lc*lispTerm lc*term*cfc))
      (let* ((lc*const*list (if (eql 1 nr*of*chunk) ; FIRST*PREMISE??
                              (s-head*const*occur)
                              nil))
             (lc*arglist (s-rest literal))
             (lc*arity (s-usrlit*arity nr*of*chunk))
             (lc*env*size (mk-env*size nr*of*chunk))
             (lc*lit*cfc (cl2-arglist lc*const*list 
                                      lc*arglist
                                      nr*of*chunk
                                      nr*of*chunkLit))
             (lc*code*list (mk-arg*seq)))
        (mk-cl2*lit lc*functor
                    lc*lit*cfc
                    (mk-lit*descr lc*arity lc*env*size lc*code*list nil))))))
                         

;------------------------------------------------------------------------------------
;;;                       CL2-IS*PRIMITIVE
;;; Eingabe <lhs*term>   Linke Seite des is-Primitivs.
;;;         <rhs*term>   Entsprechend die rechte Seite. Weder rechts noch links stehen
;;;                      aktive Literalaufrufe. Diese wurden schon in Pass 1 in ent-
;;;                      sprechende Aufrufe mit (refl-Xreg ...) transformiert.
;;;         <nr*of*chunk> Die Nummer des chunks
;;;
;;; Wert  (IS <lhs*cfc> <rhs*cfc>)

(defun cl2-is*primitive (lhs*term rhs*term nr*of*chunk nr*of*chunkLit)
  (let ((lc*rhs*cfc (cl2-guard*arg rhs*term 
                                   nr*of*chunk nr*of*chunkLit))
        (lc*lhs*cfc (cl2-guard*arg lhs*term 
                                   nr*of*chunk nr*of*chunkLit)))
    (if (or (not (variable-p rhs*term))
            (greaterEqual*var-p lc*lhs*cfc lc*rhs*cfc))
      (list 'IS lc*lhs*cfc lc*rhs*cfc)
      (list 'IS lc*rhs*cfc lc*lhs*cfc))))



;------------------------------------------------------------------------------------
;;;                       CL2-refl*PRIMITIVE
;;; Eingabe <argList>     Die Argumenteliste. Im Normalfall enhaelt die Liste nur ein 
;;;                       Element.
;;;         <nr*of*chunk>    Die Nummer des chunks
;;;         <nr*of*chunkLit> Nummer des Literals im chunk
;;;
;;; Wert (refl-Xreg <arg*cf>)

(defun cl2-refl*primitive (argList nr*of*chunk nr*of*chunkLit)
  (list 'refl-Xreg 
        (cl2-guard*arg (s-first argList)
                       nr*of*chunk nr*of*chunkLit)))

;------------------------------------------------------------------------------------
;;;                       CL2-STRUCTURE
;;; Eingabe <strucTerm>     Die passive Struktur
;;;         <nr*of*chunk>    Die Nummer des chunks
;;;         <nr*of*chunkLit> Nummer des Literals im chunk
;;;
;;; Wert (inst <classified_StrucTerm>)

(defun cl2-structure (strucTerm nr*of*chunk nr*of*chunkLit)
  (list 'inst
        (cons (s-first strucTerm)
              (cl2-guard*argList (s-rest strucTerm)
                                 nr*of*chunk nr*of*chunkLit))))
;------------------------------------------------------------------------------------
;;;                       CL2-TUPEL
;;; Eingabe <tup*argList>     Die Listenargumente
;;;         <nr*of*chunk>    Die Nummer des chunks
;;;         <nr*of*chunkLit> Nummer des Literals im chunk
;;;
;;; Wert (tup <classified_StrucTerm>)

(defun cl2-tupel (tup*argList nr*of*chunk nr*of*chunkLit)
  (cons 'tup
        (cl2-guard*argList tup*argList
                                 nr*of*chunk nr*of*chunkLit)))

;------------------------------------------------------------------------------------
;;;                       CL2-LISPCALL
;;; Eingabe <lisp*class>     lisp-extra, lisp-predicate oder lisp-function
;;;         <cl*Expression>  Der Lispausdruck
;;;         <nr*of*chunk>    Die Nummer des chunks
;;;         <nr*of*chunkLit> Nummer des Literals im chunk
;;;
;;; Wert (cl <classified_ArgList>)

(defun cl2-lispCall (lisp*class cl*Expression nr*of*chunk nr*of*chunkLit)
  (list lisp*class
        (cons (s-first cl*Expression)
              (cl2-lisp*argList (s-rest cl*Expression)
                                 nr*of*chunk nr*of*chunkLit))))

      
;------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------                               
;;;
;;;                   ArgumentHandling fuer passive Literale
;;;                            (CHUNK-GUARDS)
;;;
;------------------------------------------------------------------------------------
;------------------------------------------------------------------------------------

;------------------------------------------------------------------------------------
;;;                        CL2-LISP*ARGLIST
;;; Eingabe <argList>      Liste der Argumente in einem Lisp-Primitiv  
;;;         <nr*of*chunk>  Die Nummer des chunks.
;;;         <nr*of*chunkLit> Nummer des Literals im chunk
;;;
;;; Wert    <argList*cfc>

(defun cl2-lisp*arglist (argList nr*of*chunk nr*of*chunkLit)
  (let ((lc*inStruc t)) ; Die Variable wird globalisiert!
    (cond ((null argList)
         nil)
        (t 
         (cons (cl2-guard*arg (s-first argList) 
                             nr*of*chunk nr*of*chunkLit lc*inStruc)
              (cl2-lisp*arglist (s-rest argList) 
                             nr*of*chunk nr*of*chunkLit))))))

;------------------------------------------------------------------------------------
;;;                        CL2-GUARD*ARGLIST
;;; Eingabe <argList>      Liste der Argumente in einem passiven chunk-Term 
;;;         <nr*of*chunk>  Die Nummer des chunks.
;;;         <nr*of*chunkLit> Nummer des Literals im chunk
;;;
;;; Wert    <argList*cfc>

(defun cl2-guard*arglist (argList nr*of*chunk nr*of*chunkLit)
  (cond ((null argList)
         nil)
        (t 
         (cons (cl2-guard*arg (s-first argList) 
                             nr*of*chunk nr*of*chunkLit t)
              (cl2-guard*arglist (s-rest argList) 
                             nr*of*chunk nr*of*chunkLit)))))

;------------------------------------------------------------------------------------
;;;                        CL2-GUARD*ARG
;;; Eingabe <arg>          Argument in einem passiven chunk-Term 
;;;         <nr*of*chunk>  Die Nummer des chunks.
;;;         <nr*of*chunkLit> Nummer des Literals im chunk
;;;         &optional <in*term> Flag fuer Vorkommen in der Struktur
;;;
;;; Wert    <arg*cfc>


(defun cl2-guard*arg (arg nr*of*chunk nr*of*chunkLit &optional (in*term nil))
  (let ((lc*guard*class (passive*structure-p arg)))
    (cond ((explicit*lispcall-p lc*guard*class)
            (cl2-lispCall lc*guard*class (s-second arg)
                   nr*of*chunk nr*of*chunkLit))
          (t 
           (case lc*guard*class
             (constant 
              (cl2-guard*constant arg))
             (variable 
              (cl2-guard*variable arg nr*of*chunk nr*of*chunkLit in*term))
             (inst
              (cl2-structure (s-second arg)
                             nr*of*chunk nr*of*chunkLit))
             (tup
              (cl2-tupel (s-rest arg)
                         nr*of*chunk nr*of*chunkLit))
             (otherwise 
              (progn
                (princ "CL2-GUARD*ARG: <arg> hat den Wert:") (print arg)   
                (terpri)
                (error "CL2-GUARD*ARG"))))))))

;------------------------------------------------------------------------------------
;;;                      CL2-GUARD*CONSTANT
;;;

(defun cl2-guard*constant (constant)
  constant)


;------------------------------------------------------------------------------------
;;;                      Cl2-GUARD*VARIABLE
;;;

(defun cl2-guard*variable (variable nr*of*chunk nr*of*chunkLit &optional (in*term nil))
  (cond ((temp*prop-p variable) 
         (cl2-guard*temp*variable variable
                                  nr*of*chunk nr*of*chunkLit in*term))
        (t (cl2-guard*perm*variable variable
                                    nr*of*chunk nr*of*chunkLit in*term))))



;------------------------------------------------------------------------------------
;;;                      CL2-GUARD*TEMP*VARIABLE
;;;


(defun cl2-guard*temp*variable (temp*var nr*of*chunk nr*of*chunkLit in*term)
  (let* ((lc*occur (mk-occurence temp*var))
         (lc*save (mk-saveness temp*var
                               nr*of*chunk nr*of*chunkLit in*term)))
    (cond ((eq 'first lc*occur)
           (cond ((optimize-reg*setting (s-var*name temp*var) nr*of*chunk nr*of*chunkLit)
                  (setq lc*occur 'reuse)))))
    (mk-cl2*var*descr temp*var lc*occur lc*save 'temp)))



;------------------------------------------------------------------------------------
;;;                      CL2-GUARD*PERM*VARIABLE
;;;
(defun cl2-guard*perm*variable (perm*var nr*of*chunk nr*of*chunkLit in*term)
  (let* ((lc*occur (mk-occurence perm*var))
         (lc*save (mk-saveness perm*var
                               nr*of*chunk nr*of*chunkLit in*term)))      
    (mk-cl2*var*descr perm*var lc*occur lc*save 'perm)))





                         
;;;
;;;                   ArgumentHandling fuer aktive Literale
;;;




;------------------------------------------------------------------------------------
;;;                   CL2-ARGLIST
;;; Eingabe:
;;;
;;;   <const*list>    Liste der Form ((const argpos) ...)
;;;                   Diese Liste wird gebraucht, um zu erkennen, dass fuer diese Konstante kein Code
;;;                   erzeugt werden braucht. 
;;;   <arg*list>      Liste der Argumente
;;;   <nr*of*chunk> Die Nummer des chunks, zu welchem die Argumentenliste gehoehrt.
;;;
;;; Wert:            Liste der Form (<arglist*classification>)
;;; Seiteneffekt:    In der Propertieliste COD_GEN werden unter den Tags 'perm 'temp 'const die Argu-
;;;                  mentenstellen vermerkt, fuer die Code generiert werden soll. Hierdurch ist es 
;;;                  moeglich: a.) Arg. zu markieren fuer die kein Code erzeugt werden braucht.
;;;                            b.) Eine Reihenfolge des Abarbeitung des Codegenerators zu erzwingen.

(defun cl2-arglist (const*list arg*list nr*of*chunk nr*of*chunkLit)
  (mk-clear*arg*seq)
  (cl2-arglist2 const*list arg*list
                1
                nr*of*chunk nr*of*chunkLit))

(defun cl2-arglist2 (const*list arg*list nr*of*arg nr*of*chunk nr*of*chunkLit)
  (cond ((null arg*list) NIL)
        (t (let* ((lc*current*arg (s-first arg*list))
	          (lc*arg*rest    (s-rest arg*list))
                  (lc*arg*cfc (cl2-arg const*list lc*current*arg
                                       nr*of*arg 
                                       nr*of*chunk nr*of*chunkLit)))
             (cons lc*arg*cfc
                   (cl2-arglist2 const*list lc*arg*rest 
                                  (+ nr*of*arg 1)
                                 nr*of*chunk nr*of*chunkLit))))))
 

                                  
(defun cl2-arg (const*list arg nr*of*arg  nr*of*chunk  nr*of*chunkLit)
  (cond ((constant-p arg)
         (cl2-constant const*list arg nr*of*arg nr*of*chunk))
        ((variable-p arg)
         (cl2-variable arg nr*of*arg nr*of*chunk  nr*of*chunkLit))
        (t (error "cl2-arglist2"))))


(defun cl2-constant (const*list c-arg nr*of*arg nr*of*chunk)
   (let* ((lc*const `(,c-arg ,nr*of*arg))
         (lc*const*occur (member lc*const const*list :test 'equal))) 
    (cond ((head-p nr*of*chunk) 
           (addprop  'arguments 'head*const lc*const)))
    (cond ((null lc*const*occur)
           (mk-add*arg*seq 'const nr*of*arg))
          (t t))
    c-arg))
    


(defun cl2-variable (variable nr*of*arg nr*of*chunk nr*of*chunkLit)
     (cond ((temp*prop-p variable) 
           (cl2-temp*variable variable nr*of*arg
                              nr*of*chunk nr*of*chunkLit))
          (t (cl2-perm*variable variable nr*of*arg
                                nr*of*chunk nr*of*chunkLit))))

(defun cl2-perm*variable (perm*var nr*of*arg nr*of*chunk nr*of*chunkLit)
  (let* ((lc*use*head (s-use*head perm*var))
         (lc*is*multi*use (not (null (cdr lc*use*head))))
         (lc*occur (mk-occurence perm*var))
         (lc*save (mk-saveness perm*var nr*of*chunk nr*of*chunkLit)))
    (cond ((and (head-p nr*of*chunk)
                lc*is*multi*use)
           (mk-add*arg*seq 'multi*use*head*perm nr*of*arg))
          ((and (first*chunk-p nr*of*chunk)
                (member nr*of*arg lc*use*head))
           nil)
          (t (mk-add*arg*seq 'perm nr*of*arg)))        
    (mk-cl2*var*descr perm*var lc*occur lc*save 'perm)))



(defun cl2-temp*variable (temp*var nr*of*arg nr*of*chunk nr*of*chunkLit)
  (let* ((lc*Xreg (s-reg temp*var)) ; NIL => Xreg muss noch bestimmt werden
         (lc*use*head (s-use*head temp*var))
         (lc*use*chunk (s-tempvar*use*chunk*lit temp*var))
         (lc*is*multi*use (not (null (cdr lc*use*head))))
         (lc*occur (mk-occurence temp*var))
         (lc*save (mk-saveness temp*var nr*of*chunk nr*of*chunkLit)))
    (cond ((null lc*Xreg)
           (setq lc*Xreg (mk-set*Xreg temp*var
                                      (s-last (intersection lc*use*head lc*use*chunk)))))
          (t t))
    (cond ((eql lc*Xreg nr*of*arg)
           (cond ((not (head-p nr*of*chunk))
                  (cond((member nr*of*arg lc*use*head)
                        nil)
                       ((and (not (var*occur*in*guard-p temp*var))
                             (eql (length lc*use*chunk) 1))
                        (mk-add*arg*seq 'temp nr*of*arg))
                       (t nil)))
                 (t nil)))
          (lc*is*multi*use 
            (progn 
              (mk-add*arg*seq 'multi*use*head*temp nr*of*arg) ;Die Argumentenstelle merken
              (cond ((mark*multi*use*temp-p temp*var) ; Variable schon als Multi-Use markiert?
                     nil)
                    (t                                 ; Nein! Also markieren.
                     (mk-add*arg*seq 'mark*multi*use*temp (s-var*name temp*var))))))
          (t (mk-add*arg*seq 'temp nr*of*arg)))
    (cond ((eq 'first lc*occur)
           (cond ((optimize-reg*setting (s-var*name temp*var) nr*of*chunk nr*of*chunkLit)
                  (setq lc*occur 'reuse)))))
    (mk-cl2*var*descr temp*var lc*occur lc*save 'temp)))

(defun optimize-reg*setting (temp*var nr*of*chunk nr*of*chunkLit)
  (cond ((optimized*reg-p temp*var)
         (progn (mk-add*reassignlist temp*var) nil))
        (t 
         (find-optimized*reg temp*var nr*of*chunk nr*of*chunkLit 
                            (reverse (s-reassignlist))))))

(defun find-optimized*reg (temp*var nr*of*chunk nr*of*chunkLit reassign*list)
  (cond ((null reassign*list) 
         (let* ((lc*free*list  ;;; Wiederverwendbare Registernummern
                 (if (var*in*foot*struc-p temp*var)
                   (remove 1 (s-new*freelist))
                   (s-new*freelist)))
                (lc*useReg (s-reg temp*var))
                (lc*minReg (s-max*in*list lc*free*list))) ; durch REUSE wiederverwendbar
          (cond ((and (not (null lc*free*list))
                       (> lc*useReg lc*minReg))
                  (progn (mk-add*new*freelist lc*useReg)
                         (mk-remove*new*freelist lc*minReg)
                         (mk-set*Xreg temp*var lc*minreg)
                         (mk-add*reassignlist temp*var)
                         nil))
                 (t 
                  (progn (mk-add*reassignlist temp*var) nil)))))
        (t (let* ((lc*useReg (s-reg temp*var))
                  (lc*useCallit (s-use*chunk*call*lit temp*var nr*of*chunk))
                  (lc*possible*reassign (s-first reassign*list))
                  (lc*rest*list (s-rest reassign*list))) 
             (cond ((and  reassign*list
                         (> nr*of*chunkLit (s-last*ChunkLit lc*possible*reassign))
                         (or (and (null lc*useCallit) ;;; denotatives foot-literal
                                  (not (and (var*in*foot*struc-p temp*var)
                                            (eql 1 (s-reg lc*possible*reassign)))))
                             (member (s-reg lc*possible*reassign) lc*useCallit)))  
                    (progn (mk-remove*reassignlist lc*possible*reassign)
                           (mk-add*reassignlist temp*var)
                           (mk-add*new*freelist lc*useReg)
                           (mk-set*Xreg temp*var (s-reg lc*possible*reassign))))
                    (t 
                     (find-optimized*reg temp*var nr*of*chunk 
                                         nr*of*chunkLit lc*rest*list)))))))