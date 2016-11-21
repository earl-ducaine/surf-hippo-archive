
;;;                CLASSIFY-DB
;;;      Klassifizierung der Datenbasis
;;;
;;; Eingabe: <list*of*predicates> Liste der Praedikatname/Aritaet der DB
;;;
;;; Wert:    Klassifizierte Datenbasis:
;;;          (classified_proc1 ... classified_procN)
;;;
;;; Die Datenbasis befindet sich auf Property-Listen. Jede Prozedur, also
;;; Praedikatname/Aritaet, ist eine Property-Liste; unter dem Tag 'clause
;;; stehen die KLauseln einer Prozedur zu einer Liste zusammengefasst.
;;; Die Funktion CLASSIFY-DB klassifiziert nacheinander die Prozeduren, die 
;;; durch list*of*predicates gekennzeichnet sind.


(defun classify-db (list*of*predicates) ; Klassifizeirung  der Datenbank
  (cond ((null list*of*predicates) nil)
        (t (let ((lc*procedure*name (s-first  list*of*predicates)))
             (cons (classify-procedure (get  lc*procedure*name 'clause) lc*procedure*name)
                 (classify-db (s-rest list*of*predicates)))))))

;----------------------------------------------------------------------------
;;;                  CLASSIFY-PROCEDURE
;;;          Klassifizierung einer Prozedur
;;; 
;;; Eingabe: <list*of*clause> Liste der Klauseln, welche die Prozedur beschreiben
;;;          <procedure*name> Name/Aritaet der Prozedur
;;;
;;; Wert   : Klassifizierte Prozedur 
;;;          (proc <proc_name> <clause_count>
;;;               <list_of_classified_clauses>)
;;;
;;;  Die eigentliche Arbeit macht CLASSIFY-PROCEDURE*CLAUSE
;;;  Hier wird nur die Anzahl der KLauseln bestimmt (length ..) 

(defun classify-procedure (list*of*clause procedure*name) 
  (mk-procedure procedure*name 
                (length list*of*clause)
                (classify-procedure*clause list*of*clause))) 


;-----------------------------------------------------------------------------
;;;                  CLASSIFY-PROCEDURE*CLAUSE
;;;          Klassifizierung einer Prozedur
;;; 
;;; Eingabe: <list*of*clause> Liste der Klauseln, welche die Prozedur beschreiben
;;;
;;; Wert   : Liste von klassifizierten Klauseln einer Prozedur 
;;;          (<classified_clause1> ... <classified_clauseN>)

                
(defun classify-procedure*clause (list*of*clause)   
  (cond ((null list*of*clause) nil)
        (t (cons (classify-clause (s-first list*of*clause))
                 (classify-procedure*clause (s-rest list*of*clause))))))


;------------------------------------------------------------------------------
;;;                  CLASSIFY-CLAUSE
;;;       Klassifizierung einer einzelnen Klausel
;;;
;;; Eingabe: <clause> Die zu klassifizierende Klausel.
;;;
;;; Wert   : Die Klassifizierte Klausel
;;;          (<clause_type> <perm_var_list> <temp_var_list>
;;;            (<head-chunk_cfc1> <chunk_cfc1>   <chunk_cfcN>) N >= 0
;;;
;;;  Eine Klausel wird in zwei Schritten klassifiziert. Im ersten Schritt "cl1-..."
;;;  werden Informationen ueber die Variablen gesammelt, um sie in permanent und
;;;  temporaer einteilen zu koennen. Weiterhin wird die Klausel in chunks gegeliedert und
;;;  durch ein einfaches Rewrite-System simplifiziert (partial evaluation).
;;;  Im zweiten Schritt "cl2-..." erfolgt die Bestimmung von occurence, saveness, arg_seq,
;;;  X/Y-Reg, usw.

(defun classify-clause (clause)
  (let ((*var-bindings* nil) ;Regeln der Form (variable -> constant)
        (*usrlit-arity* nil) ;Liste der Form (chunk-Nr  arity)
        (*next-chunkreg* nil);Liste der Form (chunk-Nr next*free*reg)
        (*possible-reassignment* nil);       (var*name1 var*name2 )  
        (*new-freelist* nil)
        (cl2*clause nil)
        (cl1*clause nil))
    (declare (special *var-bindings* 
                      *usrlit-arity*
                      *next-chunkreg*
                      *possible-reassignment*
                      *new-freelist*))
    (init-clause)
    (setq cl1*clause (cl1-clause clause)); pass1, per Seiteneffekt Inf. ueber Variablen
                                          ;sammeln und die chunks bilden.
    (mk-perm/temp*list) ; Variablen in permanent und temporaer unterscheiden.
    (mk-set*all*Yreg (s-perm*list) 1) ; Zuweisung der Yreg im local-stack.(Start mit 1)
    (setq cl2*clause (cl2-clause cl1*clause)) 
    (mk-clause*classification (s-clause*type cl2*clause)
                            (mk-perm*var*list (s-perm*list))
                            (mk-temp*var*list (s-temp*list))
                            cl2*clause)))



(defun init-lit ()
  (mk-clear*arg*seq ))

(defun init-clause ()
  (dolist (x (s-variable*list))
    (progn
      (remprop x 'first*chunk)
      (remprop x 'first*arg)
      (remprop x 'last*chunk)
      (remprop x 'last*chunkLit)
      (remprop x 'first*chunkLit)
      (remprop x 'saveness)
      (remprop x 'use*head)
      (remprop x 'oc*in*chunk)
      (remprop x 'var*occur)
      (remprop x 'reg)
      (remprop x 'reg*state)
      (remprop x 'use*chunk)
      (remprop x 'occur*in*guard)
      (remprop x 'in*foot*struc)))
  (setf (symbol-plist 'arguments) nil))

(terpri)
(princ "Klassifizierer V 1.0 . Letzte Aenderung: 06. 10. 1990")
(terpri)
