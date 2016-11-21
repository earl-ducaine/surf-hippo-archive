;;; -*- Package: CONTAX; Syntax: CLtL; Base: 10; Mode: LISP -*-
					 
;************************************************************************
;
;       Die Predikate A und S fuer den Algorithmus HAC
;
;       Benutzung:  (init-hac) -> create fuer alle Constraints,
;                                 die in der Liste *active-constraints*
;                                 enthalten sind.
;
;                   (create-look-up '<constraint>) -> create fuer das
;                                  angegebene constraint
;                   (look-up-a '<constraint> '<dom-liste> '<val-liste>)
;                                Abfrage des Eintrags. Erst sinnvoll,
;                                wenn einer mit obigen Fkt. angelegt 
;                                wurde. Sonst gibt es eine Fehlermeldung
;
;                   (look-up-s     ...           ...           ...    )
;                                Siehe oben. Genaue Erl"auterugen stehen
;                                 bei den Funktionen selbst
;
;************************************************************************



;***********************************************************************
;
;       Funktionen fuer die Hierarchie
;
;*********************************************************************** 


( in-package "CONTAX" ) 

( export '(init-hac))

( defun id (x) x)

; ************* die datenstruktur, die alle eintraege fuer die 
;               hierarchische propagrierung enth"alt. 
;               Sie besteht aus 2 hashtabellen.
;               1. Tabelle: Schlussel: Constraintname,
;                                      Key1, Key2, ... KeyN.
;                           Wert:     cons-pair aus 2 hashtabellen
;               2. cons-pair: (TabA . TabS)
;                  TabA: Schluessel: Wert1,Wert2, ... WertN
;                        Wert:       Gilt Constraint(Wert1, ... WertN) 
;                                    fuer alle subconcepte von Wert1 ???
;                                        -----
;                                      Nil: gilt nicht.
;                                      non-nil: gilt.
;                  TabS: Schluessel: Wert1,Wert2, ... WertN
;                        Wert:       Gilt Constraint(Wert1, ... WertN) 
;                                    fuer ein subconcept von Wert1 ???
;                                        -----
;                                      Nil: gilt nicht.
;                                      non-nil: gilt.
;************************************************************************


(defvar *hac-look-up* nil)

;****** liefert fuer alle elemente aus l die soehne, oder
;       falls das element keine hat, das element selbst

(defun sons-of-all (l)
  (mapcar #'(lambda (x)
		    (cond ((is-leaf x) (list x))
			   (t (sons-of x)))
		    )
	  l)
  )

;************************************************

(defun sons-and-parents (p s)
  (do ((l nil (append l (list (car r))))
       (r p (cdr r))
       (rest-s s (cdr rest-s))
       (erg nil erg)
       )
       ((null r) erg)
       (setq erg (cons (append l (cons (car rest-s)
				       (cdr r)))
		       erg))
       )
  )


;************************************************
(defun multiply-sons (l)
  (cond ((null l) nil)
	((listp (car l))
	 (mapcar #'(lambda (one-l)
			   (cons one-l (cdr l)))
		 (car l)))
	(t (mapcar #'(lambda (one-erg)
			      (cons (car l) one-erg))
		    (multiply-sons (cdr l))))
        )
  )

;************************************************
(defun multiply-all (l)
  (remove l
  (mapcan #'multiply-sons (sons-and-parents l (sons-of-all l)))
	  :test 'equal)
  )


;**********************************************************************
;**********************************************************************
;**********************************************************************
;
;     Die beiden Predikate zum Auffuellen der look-up-Tabellen
;
;***********************************************************************
; testet rekursiv, ob die werte in (cons l arg)  element der relation r sind
; rekursion stoppt, wenn alle element in (cons l arg) 
;  blaetter der hierarchie sind.
; l ist das linkeste argument. es wird gefragt, ob fuer ALLE subconcepte
; von l die relation (r l arg) haelt. 
; hierzu dient die verknuepfung der rekursiven aufrufe mit EVERY
; fuer die werte in arg muss nur eine kombination gefunden werden,
; deshalb wird hier mit SOME verknuepft

(defun predicate-a (r  dom l arg ty)
  ;(format *trace-output* ".")
  (cond ((and (is-leaf l)
	      (every 'is-leaf arg))
	 (store 'a-fct r dom l arg (do-eval r dom l arg) ty))
        ((is-leaf l)
	 (store 'a-fct r dom l arg 
                        (some 'id 
			      (mapcar #'(lambda (x) 
				      (predicate-a r  dom l x ty))
				      (multiply-all arg))) ty))
	(t 
	 (store 'a-fct r dom l arg 
                        (every 'id 
			       (mapcar #'(lambda (x)
						 (predicate-a r dom x arg
							      ty )
						 )
				       (sons-of l))) ty))
	)
  )


;**************************************************************
; fasst, wie oben, aber es wird nur gefragt, ob mindestens ein
; subconcpet aus l die relation erfuellen kann, fuer eine kombination
; aus subconcepten aus arg
; die rekursion wird deshalb in beiden faellen mit SOME
; verknueptf.


(defun predicate-s (r  dom l arg ty)
  (cond ((and (is-leaf l)
	      (every 'is-leaf arg))
	 (store 's-fct r dom l arg (do-eval r dom l arg) ty))
	((is-leaf l)
	 (store 's-fct r dom l arg 
                        (some 'id 
			      (mapcar #'(lambda (x) 
				      (predicate-s r dom l x ty))
				      (multiply-all arg))) ty))
	(t
	 (store 's-fct r dom l arg 
                         (some 'id 
			       (mapcar #'(lambda (x)
						 (predicate-s r dom x arg
							      ty)
						 )
				       (sons-of l))) ty))
	)
  )

;*****************************************************************
;
;      Funktionen von Contax	
;
;*****************************************************************

;************** liefert die subconcepte eines elementes.
;               ausnahmen: das element ist selbst blatt:
;                          (element) wird zurueckgegeben
;                          das element ist ein intervall:
;                          (i j) -> (i i+1 ... j-1 j)

(defun sons-of (x)
  (cond ((is-leaf x) (list x))
;	((interval-p x) (read-intervalls x))
	(t (subconcepts x))
  )
)

;*************************** wie oben, allerdings werden
;                             zahlen anders behandelt.

;(defun intervall-sons (x)
;  (cond ((interval-p x) (read-intervalls (list x)))
;	((numberp x) (list x))
;	(t (leaves x)))
;)

;************************** eine element ist ein blatt, falls
;                            es keine subconcepte hat, oder eine zahl
;                           ist

(defun is-leaf (x)
 (or (numberp x) (null (subconcepts x)))
  )

(defun non-leaf (x)
 (not (is-leaf x))
)

;*************************** die funktionen sind die schnittstellen
;                            auf die objekte in den anderen files
;                            von contax

(defun get-domain (r)
  (domains r)
 )

(defun get-parameter-names (r)
  (parameter-names (find-constraint r))
 )

(defun get-relation (r)
  (comp-c-def r)
)

;***********************************************************
;       sortiert werte von arg und l wieder in die richtige
;       position, dh. in der sie in der relation r definiert sind
;       dies wird gebraucht, da jedes element der argumentliste
;       einmal all-quantifiziert wird, d.h. es muss im wert l 
;       liegen. der test, ob r haelt geht aber nur in der 
;       urspruenglichen reihenfolge

(defun get-org-position (r swap-dom l arg)
  (do ((left nil (append left (list (car right))))
       (right (cdr swap-dom) (cdr right))
       (left-val nil (append left-val (list (car right-val))))
       (right-val arg (cdr right-val))
       )
       ((equal (get-parameter-names r) (append left (cons (car swap-dom) right)))
	(append left-val (cons l right-val)))
    )
  )


;****************************************************************
;
;                  VORLAEUFIGE DEFINITIONEN
;
;****************************************************************

;****************** test ob die kombination (nach tauschen der
;                   positionen in die urspruengliche)
;                   im constraint name enthalten ist 

(defun do-eval (name dom l arg)
  (member (get-org-position name dom l arg) (get-relation name) :test 'equal)
  )


;****************************************************************
;
;                Speicherstruktur HAC (Hashtables)
;
;****************************************************************

;**************** baut die erste hashtabelle auf

(defun init-all-look-up ()
 (setq *hac-look-up* (make-hash-table :test #'equal))
 )
(init-all-look-up)

;*********************************
;                 baut alle eintrage in der ersten hashtabelle auf,
;                 d.h. fuer alle kombinationen von keys, d.h. 
;                 (k1 k2 k3) -> (k1 k2 k3), (k2, k3, k1), (k3, k1,k2).
;                 wird eine zeile mit je einem conspaar von tabellen 
;                 angelegt.

(defun init-look-up (r var ty)
 (mapcar #'(lambda (one-combination)
	     (init-line-look-up ty one-combination)
	     )
	 (each-first var)
	 )
 )

(defun init-line-look-up (r var)
  (setf (gethash (cons r var) *hac-look-up*)
	(cons (make-hash-table :test 'equal) (make-hash-table :test 'equal))
	)
  )

;************ fuer die vertauschung aus. jedes element von l
;             kommt einmal an die erste stelle.

(defun each-first (l)
 (do ((left nil (append left (list (car right))))
      (right l  (cdr right))
      (erg nil (cons (cons (car right) (append left (cdr right))) erg))
      )
     ((null right) erg)
   )
)

;***************** sucht die tabellen aus dem conspaaar
;                     der erste eintrag ist die A-tabelle (All)
;                     der zweite "      ist die S-tabelle (Some)

(defun s-fct (x)
 (funcall 'cdr x)
)

(defun a-fct (x)
 (funcall 'car x)
)

;*****************************************************************
;
;            Fuellen der Hashtables mit Aufrufen von A und S
;;*****************************************************************

;speichert fuer eine kombination eine wertekombination in die tabelle,
; also ob die werte bie entsprechender quantifizierung
; die relation r erfuellen. dies fuert zu einem eintrag in der
;  s- oder der a- tabelle 

(defun store (fct r var l arg val ty)
  ;(format t " Save  ~A ~A ~A ~A~%"  var l arg val)
  (setf (gethash (cons l arg) (funcall fct (gethash  (cons ty var) *hac-look-up*)))
        val)
  val
  )

;*************** init hac baut die conspaare von hashtabllen auf,
;                und tr"agt die werte fuer alle primitiven constraints
;                ein, die in der schlange *activeconstraits* liegen.

; changed 01-09-92 fs : now initializing ALL constraints
; changed 02-09-92 fs : now using PRIMITIVE-CONSTRAINT-P 
(defun init-hac ()
;  (mapcar #'(lambda (ein-tupel)
;            (create-look-up (car ein-tupel) (cadr ein-tupel))
;	    )
;	  (cstr-without-type (remove-if-not #'primitive-constraint-p ( constraint-list ))))
  ( warn "You are still using HAC, which is absolutely not necessary." )
  ( values t )
  )

(defun constraints-of (l)
 (mapcap #'cdr l)
)

;****************************************************************************
;
;          Hilfsfunktionen, damit jeder Typ nur einmal bearbeitet wird
;
;*****************************************************************************

; um eintr"age zu sparen, ist es nur erforderlich, dass jeder constraint
; TYP einmal erfasst ist. die zuordnung zu verschiedenen instanzen
; erfolgt ueber die parameter liste

(defun cstr-and-type (l)
  (mapcar #'(lambda (c) (list c (class-name (class-of c)))) l)
  )

(defun rm-types (l)
  (cond ((null l) nil) 
	((member (cadr (car l))
		 (mapcar #'cadr (cdr l)))
	 (rm-types (cdr l)))
	(t (cons (car l) (rm-types (cdr l))))
	)
  )

(defun cstr-without-type (l)
 (mapcar #'(lambda (ein-tupel)
		    (list (name-von (car ein-tupel)) (cadr ein-tupel))
		    )
		     (rm-types (cstr-and-type l)))
       
  )

(defmethod only-primitive ((pc primitive-constraint))
  pc)

(defmethod only-primitive ((lc lisp-constraint))
 nil)

(defun domain-of-param (rel param)
 (get-domain-of-param param (get-domain rel) (get-parameter-names rel))
)

(defun get-domain-of-param (p doms params)
 (cond ((null doms) nil)
       ((equal p (car params)) (car doms))
       (t (get-domain-of-param p (cdr doms) (cdr params)))
       )
)

(defun domain-param (r params)
 (mapcar #'(lambda (p) (domain-of-param r p)) params)
)

;*****************************************************************************

;********* wird von init hac aufgerufen, und fuhert den aufruf 
;         fuer der alle wertekombinationen kreiert durch

(defun create-look-up (r ty)
 (init-look-up r (get-parameter-names r) ty)
 (mapcar #'(lambda (one-dom)
	     (create-all-calls r one-dom ty)
	     )
	 (each-first (get-parameter-names r)))
 t
 )

;************************ startet den aufrur, der alle
;                         wertekombinationen rekursiv erzeugt

(defun create-all-calls (r  param ty)
  (create-p-calls 'predicate-s r param (domain-param r param) ty)
  (create-p-calls 'predicate-a r param (domain-param r param) ty)
)

; ************************************** startet die rekursion 

(defun create-p-calls (p r dom val ty)
 (funcall p r dom (car val) (cdr val) ty)
 (cond ((some 'non-leaf (cdr val))
	(mapcar #'(lambda (one-comb)
		    (create-p-calls p r dom (cons (car val) one-comb)
				    ty)
		    )
		(multiply-all (cdr val))))
       (t nil)
       )
 )
	
;***************************************************************
;
;          Abfragefunktionen, die in den Tabellen nachsehen
;
;**************************************************************

;holt die beiden hash-tables fuer some und exists
; falls die werte fuer die abfrage noch nicht angelegt wurden,
; gibts einen fehler

(defun get-hac-tables (r var)
 (let ((table  (gethash ( cons ( class-name ( class-of ( find-constraint r )))
					var )
				 *hac-look-up* )))
 (cond ((null table) (error  "HAC Table not found"))
       (t table))
 )
)

;holt den wert fuer den all-quantor

(defun look-up-a (r var val)
 (let ((result (gethash val ( a-fct (get-hac-tables r var))))
       )
  result
 ))


;holt den wert fuer den exists quantor

(defun look-up-s (r var val)
 (let ((result (gethash val ( s-fct (get-hac-tables r var))))
       )
   result 
 ))


;die alten funktionen, ohne test auf nil und fehlermeldung
;***********************************************************

(defun look-up-a-old (r var val)
  (gethash val ( a-fct ( gethash ( cons ( class-name ( class-of ( find-constraint r )))
					var )
				 *hac-look-up* )))
)

(defun look-up-s-old  (r var val)
  (gethash val (s-fct (gethash (cons 
				(class-name (class-of (find-constraint r )))
				     var) *hac-look-up*)))
)







