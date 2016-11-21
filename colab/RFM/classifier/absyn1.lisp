;;;       
;;;             absyn1.lisp
;;;
;;; Definition der abstrakten Syntax.
;;;  Selektoren, Konstruktoren, Praedikate als Funktion-Definitionen realisiert.
;;;  
;;; Thomas Krause
;;; Universitaet Kaiserslautern
;;; DFKI ARC-TEC Teil C

;;;
;;;                 1. Selektoren
;;;

;------------------------------------------------------------------------------------
;;;                           S-CLAUSE*TYPE
;;; Eingabe <clause> Eine Liste der klassifizierten Chunks einer Klausel.
;;;
;;; Ausgabe <clause*type> Der Typ der Klausel.
;;;          Dieser Selektor berechnet effektiv den Klauseltyp und selektiert ihn nicht
;;;          (wie ueblicherweise bei Selektoren) aus einer Datenstruktur.

(defun s-clause*type (clause)
  (let* ((lc*head*chunk (s-first clause))
         (lc*body*chunk*list (s-rest clause))
         (lc*head*chunk*call*lit (s-chunk*call*lit lc*head*chunk nil)))
    (cond ((null lc*body*chunk*list)
           (cond ((null (s-rest (s-second lc*head*chunk)))
                  'rel0)
                 (t 
                  (if (passive*term-p lc*head*chunk*call*lit)
                    'fun1den
                    'fun1eva))))
           (t
            (let ((lc*chunk*call*lit (s-chunk*call*lit (s-last lc*body*chunk*list) nil)))
              (if (passive*term-p lc*chunk*call*lit)
                'fun*den
                'fun*eva))))))



;---------------------------------------------------------------------------------
;;;                    S-ARG*SEQ...
;;; Eingabe <tag>: Der Index innerhalb der Datenstruktur. Moegliche Werte 
;;;                CONST, PERM, TEMP, MULTI*USE*PERM, MULTI*USE*TEMP,
;;;                MARK*MULTI*USE*TEMP
;;; Selektoren fuer die Datenstruktur ARG*SEQ.

(defun s-arg*seq (tag)
     (get 'arg*seq tag))

(defun s-head*arity ()
  (declare (special *usrlit-arity*))
  (s-second (assoc 0 *usrlit-arity*)))

(defun s-usrlit*arity (nr*of*chunk)
  (declare (special *usrlit-arity*))
  (let ((lc*arity (s-second (assoc nr*of*chunk *usrlit-arity* ))))
    (if lc*arity
      lc*arity
      0)))

(defun s-next*chunkreg (nr*of*chunk regList)
  (let ((nextReg (s-second (assoc nr*of*chunk regList))))
    (if nextReg
      nextReg
      (s-usrlit*arity nr*of*chunk))))

(defun s-reassignlist ()
  (declare (special *possible-reassignment*))
  *possible-reassignment*)

(defun s-new*freelist ()
  (declare (special *new-freelist*))
  *new-freelist*)


;;;
;;;     2. KONSTRUKTOREN   
;;;

(defun mk-env*size (lit*nr)
  (mk-env*size2 (s-perm*list) lit*nr 0))

(defun mk-env*size2 (perm*list nr*of*chunk env*size)
  (cond ((null perm*list) env*size)
        (t (cond ((<= (s-last*chunk (s-first perm*list)) nr*of*chunk)
                  (mk-env*size2 (s-rest perm*list) nr*of*chunk env*size))
                 ( t  (mk-env*size2 (s-rest perm*list)
                                    nr*of*chunk
                                    (+ env*size 1)))))))

(defun mk-cl1*clause (cl*head cl*chunk*list)
  (cons (cons cl*head 
              (s-first  cl*chunk*list)) ; head*chunk
        (s-rest cl*chunk*list)))

(defun mk-cl1*lispcall (lisp*tag lisp*call)
  (list (cons lisp*tag lisp*call)))

;--------------------------------------------------------------------------
;;;                   MK-MODIFY*LIST
;;;  In einer Liste <old*list> der Form ( (t1 descr1) ... (tn descrn)) wird das 
;;;  das Tag ti==<tag> gesucht und descri durch <descr> ersetzt. Ist die Suche
;;;  erfolglos wird (<tag> <descr>) in die Liste eingefuegt.

(defun mk-modify*list ( tag  descr  old*list new*list)
  (let ((lc*current  (s-first old*list))
        (lc*rest     (s-rest  old*list)))
    (cond ((null old*list) (cons (list tag descr) new*list))
          ((equal tag (s-first lc*current)) (cons (list tag descr) (append lc*rest new*list)))
          (t (mk-modify*list tag descr lc*rest (cons lc*current new*list))))))

(defun mk-perm/temp*list () 
  (mk-perm/temp*list2 (s-variable*list)))


(defun mk-perm/temp*list2 (var*list)
  (cond ((null var*list) t) 
        (t (let ((lc*var*name (s-first var*list))
                 (lc*rest*var*list (s-rest var*list)))  
             (cond ((perm-p `(vari ,lc*var*name))
                    (mk-sorted*perm*insert  lc*var*name))
                   (t (mk-new*temp*var lc*var*name)))
             (mk-perm/temp*list2 lc*rest*var*list)))))


(defun mk-set*all*Yreg (perm*list start*reg)
  (cond ((null perm*list) nil)
        (t (progn (putniv (s-first perm*list) 'reg start*reg)
                  (mk-set*all*Yreg (s-rest perm*list) (+ start*reg 1))))))

(defun mk-perm*var*list (perm*list)  ; Y-Reg get its value
  (cond ((null perm*list) nil)
        (t (let* ((lc*var*name (s-first perm*list))
                  (lc*rest*var (s-rest perm*list)))
             (cons (mk-perm*var*descr lc*var*name 
                                     (s-reg lc*var*name )
                                     (s-use*head lc*var*name)
                                     (s-last*chunk lc*var*name)
                                     (s-last*ChunkLit lc*var*name))
                   (mk-perm*var*list lc*rest*var))))))


;;;           MK-SET*ALL*XREG
;;; Eingabe  <temp*list>  Liste der temporaeren Variablen
;;;          <next*regnr> Liste der Form
;;;                                 ((<chunk*nr> <next*reg>) ... )
;;;                       die fuer jeden chunk, das letzte benutzte Register kennzeichnet.
;;;          <free*regs>  Liste von Registernummern, die frei sind, da die zugehoerigen
;;;                       Argumentpositionen breits abgehandelt sind.
;;;         <is*free*args> TRUE-> Die Register mit kleineren Nummer als die aktuelle
;;;                               Argumentnummer sind als FREI zu betrachten.
;;;                        FALSE-> Die oben gemachte Annahme muss nicht zutreffen.;;; 
;;; Wert    die modifizierte Liste der temporaeren Variablen. (X_reg wurde bestimmt)
;;;
;;; Bem.: Es wird die Aritaet vom Kopf und der ersten Praemisse der Klausel bestimmt.
;;;       Das Maximium davon ist die Anfangsnr. der Hilfsregister, die den temp. Variablen zuge-
;;;       ordnet werden koennen. Es wird die Funktion MK-TEMP*VAR*LIST2 aufgerufen, welche dann
;;;       das Register fuer die temp. Variable nach dem der Projektarbeit beschriebene Alg.
;;;       bestimmt.

(defun mk-set*all*Xreg (temp*list  next*regnr free*regs is*free*args)
  (let ((lc*max*arity (max (s-head*arity) 
                           (s-usrlit*arity 1))))
    (setq next*regnr (cond ((null next*regnr) 
                            (cons (list 1 
                                        (if (> lc*max*arity 0)
                                          lc*max*arity
                                          1))
                                  nil))
                           (t next*regnr)))
    (mk-set*all*Xreg2 temp*list
                      next*regnr
                      free*regs
                      is*free*args)))
                          

(defun mk-set*all*Xreg2 (temp*list listOfnext*Regnr 
                                   free*regs is*free*args)   
  (cond ((null temp*list) listOfnext*Regnr)
        (t (let* ((lc*var*name  (s-first temp*list))
                  (lc*rest*var (s-rest temp*list))
                  (lc*use*head (if (var*in*foot*struc-p lc*var*name)
                                 (remove 1 (s-use*head lc*var*name))
                                 (s-use*head lc*var*name)))
                  (lc*use*chunk*lit (s-tempvar*use*chunk*lit lc*var*name))
                  ;(lc*max*use*head (s-max*in*list lc*use*head))
                  (lc*max*use*chunk*lit (s-max*in*list lc*use*chunk*lit))
                  (lc*occur*in*chunk (s-max*in*list (s-occur*in*chunk lc*var*name)))
                  (lc*Xreg (s-reg lc*var*name))) 
             (setq lc*Xreg (cond (lc*Xreg     ; Ist Xreg schon gefunden? (<> NIL)
                                  lc*Xreg)
                                 ((null lc*use*head) ; Vorkommen nicht im Kopf
                                  (cond ((null lc*use*chunk*lit)   ; Vorkommen nur in Guards!
                                         (let ((nextReg (+ (s-next*chunkreg lc*occur*in*chunk
                                                                            listOfnext*Regnr)
                                                           1)))
                                           (setq nextReg (if (and (denotative*chunk-p lc*occur*in*chunk)
                                                                  (eql 1 nextReg))
                                                           2
                                                           nextReg))
                                           (setq listOfnext*Regnr (mk-modify*list lc*occur*in*chunk
                                                                                  nextReg
                                                                                  listOfnext*Regnr
                                                                                  nil))
                                           (mk-reg*state lc*var*name 'not*optimized)
                                           nextReg))
                                        ((equal 1 lc*occur*in*chunk) ; Vorkommen im usrLit von chunk 1
                                         (cond ((s-last (intersection  lc*use*chunk*lit 
                                                                       free*regs)))
                                               ((< (s-head*arity) lc*max*use*chunk*lit)
                                                lc*max*use*chunk*lit)
                                               (t 
                                                (let ((nextReg (+ (s-next*chunkreg lc*occur*in*chunk
                                                                            listOfnext*Regnr)
                                                           1)))
                                           (setq listOfnext*Regnr (mk-modify*list lc*occur*in*chunk
                                                                                  nextReg
                                                                                  listOfnext*Regnr
                                                                                  nil))
                                           (mk-reg*state lc*var*name 'not*optimized)
                                           nextReg))))
                                        (t 
                                         lc*max*use*chunk*lit)))
                                  ((null lc*use*chunk*lit)
                                   (s-min*in*list lc*use*head))
                                  ; Vorkommen in Kopf und PrŠmisse
                                  ((s-first (intersection lc*use*head
                                                          lc*use*chunk*lit)))
                                  ((> lc*max*use*chunk*lit  (s-head*arity))
                                   lc*max*use*chunk*lit)
                                  ((s-first (intersection lc*use*chunk*lit
                                                             free*regs)))
                                  ((and is*free*args
                                        (> (s-min*in*list lc*use*head)
                                           (s-min*in*list lc*use*chunk*lit)))
                                   (s-min*in*list lc*use*chunk*lit))
                                  ;(t
                                  ;(s-min*in*list lc*use*head))
                                  ;((> lc*max*use*head lc*max*use*chunk*lit)
                                   ;lc*max*use*chunk*lit)
                                  (t 
                                   (let ((nextReg (+ (s-next*chunkreg 1
                                                                      listOfnext*Regnr)
                                                     1)))
                                   (setq listOfnext*Regnr (mk-modify*list 1
                                                                           nextReg
                                                                           listOfnext*Regnr
                                                                           nil))
                                   (mk-reg*state lc*var*name 'not*optimized)
                                    nextReg)))) 
             (putniv lc*var*name `reg lc*Xreg)
             (mk-set*all*Xreg2 lc*rest*var listOfnext*Regnr 
                               (remove lc*Xreg free*regs) is*free*args)))))


(defun mk-temp*var*list (temp*list)
  (cond ((null temp*list) nil)
        (t (let* ((lc*var*name (s-first temp*list))
                 (lc*rest*list (s-rest temp*list)))  
             (cons (mk-temp*var*descr lc*var*name
                                      (s-reg lc*var*name)
                                      (s-use*head lc*var*name)
                                      (s-tempvar*use*chunk*lit lc*var*name))
                   (mk-temp*var*list lc*rest*list))))))


(defun mk-arg*seq ()
  (list (get 'arg*seq 'const)
        (get 'arg*seq 'multi*use*head*perm)
        (get 'arg*seq 'multi*use*head*temp)
        (get 'arg*seq 'perm)
        (get 'arg*seq 'temp)))

(defun mk-clear*arg*seq ()
  (remprop 'arg*seq 'const)
  (remprop 'arg*seq 'multi*use*head*perm)
  (remprop 'arg*seq 'multi*use*head*temp)
  (remprop 'arg*seq 'perm)
  (remprop 'arg*seq 'temp)
  (remprop 'arg*seq 'mark*multi*use*temp))

(defun mk-lisp*call (functor term)
  (declare (special *lisp-functions* *lisp-predicates* *lisp-extras* *relfun-extras*))
  (cond ((member functor *lisp-functions*)
         (list 'cl-func term))
         ((member functor *lisp-predicates*)
          (list 'cl-pred term))
         ((member functor *lisp-extras*)
          (list 'cl-extra term))
         ((member functor *relfun-extras*)
          (list 'cl-relf term))
         (t (progn (princ "functor: ") (print functor) (terpri)
                   (print "term:     ") (print term)(terpri)
                   (error "mk-lisp*call")))))

(defun explicit*lispcall-p (functor)
  (member functor '(cl-func cl-pred cl-extra cl-relf)))

(defun mk-clear*reassign*list ()
  (declare (special *possible-reassignment*))
  (setq *possible-reassignment* nil))

(defun mk-add*reassignlist (variable)
  (declare (special *possible-reassignment*))
  (setq *possible-reassignment* 
        (cons (s-var*name variable) *possible-reassignment*)))

(defun mk-remove*reassignlist (variable)
  (declare (special *possible-reassignment*))
  (setq *possible-reassignment*
        (remove (s-var*name variable) *possible-reassignment*)))

(defun mk-clear*new*freelist ()
  (declare (special *new-freelist*))
  (setq *new-freelist* nil))

(defun mk-add*new*freelist (reg*nr)
  (declare (special *new-freelist*))
  (setq *new-freelist* 
        (cons reg*nr *new-freelist*)))

(defun mk-remove*new*freelist (reg*nr)
  (declare (special *new-freelist*))
  (setq *new-freelist*
        (remove reg*nr *new-freelist*)))


(defun mk-sorted*perm*insert (var*name)
  (putniv
	 'arguments 'perm (mk-perm*insert var*name (s-perm*list))))

(defun mk-perm*insert (x sort*list)
  (cond ((null sort*list) (list x))
        ((>*perm-p x (s-first sort*list)) (cons x sort*list))
        (t (cons (s-first sort*list) (mk-perm*insert x (s-rest sort*list))))))

(defun mk-var*in*foot*struc (last*chunk)
  (let ((last*lit (s-last last*chunk)))
    (cond ((constant-p last*lit)
           nil)
          ((eq 'inst (s-first last*lit))
           (set-var*in*foot*struc (s-second last*lit)))
          (T nil)))) 

(defun set-var*in*foot*struc (arglist)
  (cond ((null arglist) t)
        ((variable-p (s-first arglist))
         (putniv (s-var*name (s-first arglist))
                 'in*foot*struc
                 t) 
         (set-var*in*foot*struc (s-rest arglist)))
        (t 
         (set-var*in*foot*struc (s-rest arglist)))))
        

;;;
;;;     3. Praedikate
;;;


(defun >*perm-p (perm*var1 perm*var2)
  (> (s-last*chunk perm*var1) (s-last*chunk perm*var2)))
         

(defun constant-p (x)
  (atom x))

(defun passive*structure-p (term)
  (cond ((constant-p term) 'constant)
        ((variable-p term) 'variable)
        (t
         (let ((lc*functor (s-first term)))
           (cond ((eq lc*functor 'inst) 'inst)
                 ((eq lc*functor 'tup) 'tup)
                 ((explicit*lispcall-p lc*functor) lc*functor)
                 (t nil))))))

(defun head-p (x)
  (eql x 0))

(defun first*chunk-p (x)
  (eql x 1))

(defun denotative*chunk-p (nr*of*chunk)
  (declare (special *usrlit-arity*))
  (not (s-second (assoc nr*of*chunk *usrlit-arity* ))))
    
    
(defun temp*prop-p (variable)
        (member (s-second variable) (s-temp*list)))

(defun perm-p (var*name)
    (cond ((or (= (s-last*chunk var*name) 1)
               (= (s-last*chunk var*name) (s-first*chunk var*name)))
           nil)
          (t t)))

(defun var*occur*in*guard-p (variable)
  (get (s-var*name variable) 'occur*in*guard))

(defun exist*var*info-p (variable)
    (member (s-var*name variable) (s-variable*list)))

(defun mark*multi*use*temp-p (temp*var)
  (let ((lc*var*name (s-var*name temp*var)))
    (cond ((member lc*var*name (get 'arg*seq 'mark*multi*use*temp)) t)
          (t nil))))

(defun greaterEqual*var-p (var1*cfc var2*cfc)
  (cond ((not (eq (s-occurrence*from*cfc var1*cfc)
                  (s-occurrence*from*cfc var2*cfc)))
         (greaterEqual*occurrence-p var1*cfc var2*cfc)); FIRST > NONFIRST
        ((not (eq (s-var_clas*from*cfc var1*cfc)
                  (s-var_clas*from*cfc var2*cfc)))
         (greaterEqual*var_clas-p var1*cfc var2*cfc)) ; TEMP > PERM
        (t
         (greaterEqual*saveness-p var1*cfc var2*cfc)) ; GLOBAL>SAFE>UNSAFE
        ))

(defun greaterEqual*occurrence-p (var1*cfc var2*cfc)
  (let ((lc*var1*occurrence (s-occurrence*from*cfc var1*cfc))
        (lc*var2*occurrence (s-occurrence*from*cfc var2*cfc)))
    (cond ((or (eq lc*var1*occurrence 'FIRST)
               (eq lc*var2*occurrence 'NONFIRST))
           t)
          (t nil))))

(defun greaterEqual*var_clas-p (var1*cfc var2*cfc)
  (let ((lc*var1*var_clas (s-var_clas*from*cfc var1*cfc))
        (lc*var2*var_clas (s-var_clas*from*cfc var2*cfc)))
    (cond ((or (eq lc*var1*var_clas 'TEMP)
               (eq lc*var2*var_clas 'PERM))
           t)
          (t nil))))

(defun greaterEqual*saveness-p (var1*cfc var2*cfc)
  (let ((lc*var1*saveness (s-saveness*from*cfc var1*cfc))
        (lc*var2*saveness (s-saveness*from*cfc var2*cfc)))
    (cond ((or (eq lc*var1*saveness lc*var2*saveness)
               (eq lc*var1*saveness 'GLOBAL)
               (eq lc*var2*saveness 'UNSAFE))
           t)
          (t
           nil))))
               
(defun optimized*reg-p (variable)
  (not (s-reg*state variable)))

(defun var*in*foot*struc-p (variable)
  (get (s-var*name variable) 'in*foot*struc))
