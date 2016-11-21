;;;       
;;;             Simplify.lisp
;;;
;;;  Rewrite-Funktionen, die eine low-Level Simplifizierung einer Klausel
;;;  gestatten, derart dass sich eine Bindung einer Variablen an eine Kon-
;;;  stante vermerkt und weiter rechts die Variable durch die Konstante innehalb
;;;  der Klausel ersetzt wird. 
;;;  
;;; Thomas Krause
;;; Universitaet Kaiserslautern
;;; DFKI ARC-TEC Teil C


;---------------------------------------------------------------------------------------
;;;                 S-BINDING
;;; Eingabe <variable>  Die Bindung von Variable wird bestimmt.
;;;         <bindings>  ASSOC-Liste der vorhandenen Bindungen.
;;;
;;; Wert    <binding> Die Bindung der Variablen. Kann keine Bindung festgestellt
;;;                   werden, wird VARIABLE zurueckgegeben.

(defun s-binding (variable bindings)
  (let ((lc*binding (assoc variable bindings :test #'equal)))
    (if lc*binding 
      (s-second lc*binding)
      variable)))

  

;---------------------------------------------------------------------------------
;;;                 MK-BINDING
;;; Eingabe <variable>  Die Bindung von <variable> wird in <bindings> aufgenommen.
;;;         <bind*to>   Die Bindung <variable> -> <bind*to> wird eingerichtet.
;;;         <bindings>  ASSOC-Liste der vorhandenen Bindungen.
;;;
;;; Wert    <new-binding> Die neue Bindung der Variablen. 
;;; 
;;; Existiert bereits eine Bindung der Variablen, so wird diese ersetzt. Die Bindungen
;;; stellen gleichzeitig das ReWrite-Sytsem dar. In Zukunft kann/mu§ hier noch erweitert 
;;; werden

(defun mk-binding (variable bind*to bindings)
  (cond ((null bindings)
         (cons (list variable bind*to)
               nil))
        (t
         (let ((lc*first*binding (s-first bindings))
               (lc*rest*bindings (s-rest bindings)))
           (cond ((equal (s-first lc*first*binding)
                         variable)
                  (cons (list variable bind*to)
                        lc*rest*bindings))
                 (t
                  (cons lc*first*binding
                        (mk-binding variable bind*to lc*rest*bindings))))))))


;------------------------------------------------------------------------------------
;;;                 REWRITE-TERM
;;; Eingabe <term>           Ein Term, in welchem die Variablen durch ihre bindings
;;;                          ersetzt werden.
;;;         <rewrite*rules>  Ein System von ReWrite-Regeln..
;;;
;;; Wert    <rewrite-term>   Term nach dem simplifizieren.

(defun rewrite-term (term rewrite*rules)
  (cond ((constant-p term)
         term)
        ((variable-p term )
         (s-binding term rewrite*rules))
        (t
         (cons (s-first term) ; Funktor
               (rewrite-arglist (s-rest term) rewrite*rules)))))


;------------------------------------------------------------------------------------
;;;                 REWRITE-ARGLIST

(defun rewrite-arglist (arg*list  rewrite*rules)
  (cond ((null arg*list) 
         arg*list)
        (t
         (cons (rewrite-term (s-first arg*list)  rewrite*rules)
               (rewrite-arglist (s-rest arg*list)  rewrite*rules)))))

         



