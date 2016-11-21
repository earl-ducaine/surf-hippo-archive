
;;;;;; definiere neue Instruktionen fuer NyWam ver GAMA!

(definstr fa-has-failed ()()           ; returns from WAM-call
  :standard
  (gfam.has-failed
   (throw 'halt nil)))

(definstr fa-has-succeeded ()()        ; sammelt von WAM gefundene Bindungen ein
  :standard
  (gfam.has-succeeded
   (if (null *fam-user-variables*)
      (setq *fam-WAM-bindings* 'ok)     ; erfolgreicher Beweis ohne Bindungsgenerierung
      (setq *fam-WAM-bindings*          ; erfolgreicher Beweis und neue Bindungen generiert.
          (cons (mapcar #'(lambda (var)
			 
                            (list (first var) (fam-emu-lisp-wandel-extra `(:ref ,(second var)))))
                        *fam-user-variables*)
                *fam-WAM-bindings*)))    ; immer fail generieren um zum naechsten choice-
   (fail))                              ; point zurueckzukehren
  )





(defun copy-to-wam (fam-heap-address)                           ; FAM-heap -> WAM-heap
  (fam-construct-term (second (get-term fam-heap-address))))


(defvar *fam-query* nil "Address of the standard FAM query -> see query")
(defvar *fam-query-goal* nil "Address indirect of the goal in the FAM query")
(defvar *fam-user-variables* nil "Variables with theire address in the GAMA")
                                 ;;; ( ( <varname> <WAM-address>) ... )


(defun init-fam()
  (setf *fam-query-goal*
	(list ':address
	      (gasm.get-global-adr-entry 'fam-back* '(user))))
  (setf *fam-query*
	      (gasm.get-global-adr 'fam-query* '(user)))
  )

   ;;; call WAM instruction

(fa-definstr prove (source-mem result-mem proc)
   (when *fa-trace* (print (list 'prove source-mem result-mem proc)))

   (let* ((procname (first proc))
	  (varnamelist (rest proc))
	  (arg-nr (length varnamelist))
	  (name-len (p_a2p/a procname arg-nr)))
      (if (equal (top *cstack) *NP)
         (pop *cstack)
         (progn
           (setq *fam-WAM-bindings* nil)
           (setq *fam-user-variables* nil)
           ; initialisiere WAM
           (init)
           ; kopiere Bindungen und setze die Argumentregister der WAM                            
           (dotimes (i  arg-nr)               
              (let* ((var (second (nth i varnamelist))) ;; varnamelist= ( (vari <x>) ... )
                     (binding (fa-newest-binding source-mem var)))    ; Bindung holen falls exist.
		(if binding
                     (set-argument-reg (1+ i) (copy-to-wam binding))   ; Bindung kopieren
                     (let ((value (new-variable)))
                        (set-argument-reg (1+ i) value)
                        (setf *fam-user-variables* (cons `(,var ,(second value))
							 *fam-user-variables*))))))
           ;  pointer auf Adresse von spezial query setzen
	   (set-reg P *fam-query*)
	   ;  addresse der Aktuellen query in fam-back* eintragen
	   (setf (mem *fam-query-goal*)
		 (gasm.get-global-adr name-len '(user)))
	   
           ; WAM starten (return ueber fa-has-failed)
           (stepp)   
           (if (null *fam-WAM-bindings*)                ;fail falls keine Loesung von der WAM
               (fa-fail)
               (when (equal *fam-WAM-bindings* 'ok)     ;WAM als prover einer voll-
                     (setq *fam-WAM-bindings* nil)      ;instanziierten Regelinstanz,
                     (fa-set-new-inst result-mem       ;keine neuen Bindungen generiert
                           (fa-newest-partial-inst source-mem))))))
      ; naechste Loesung an Folgeinstruktion weitergeben
      (when (not (null *fam-WAM-bindings*))
         (let ((bindings (first *fam-WAM-bindings*)))

            (setq *fam-WAM-bindings* (rest *fam-WAM-bindings*))
            ; bekannte Bindungen aus source-mem in result-mem uebernehmen
            (fa-set-new-inst result-mem (fa-newest-partial-inst source-mem))
            ; neugenerierte Bindungen in *heap kopieren und in result-mem speichern             
            (mapcar #'(lambda (pair)  
                         (fa-set-new-binding result-mem (first pair) *TOP)
                         (put-term-on-top (second pair)))
                    bindings))
         (when *fam-WAM-bindings*
              (push *NP *cstack)
              (push *NP *cstack)))))




(defun gasm.get-global-adr-entry (label modules)
  (let ((label-adr
	   (gasm.find-global label modules 
			     (gcla.new-object 'state  ; state must be created!!
					      'current-unknown-label gasm.*default-unknown-label*))))
    (if label-adr (cadr label-adr)
      (gerror "gasm" "gasm.get-global-adr"
	      "label '~a' not found." label))))







;;; lisp --> WAM 1:1

(defun fam-construct-term (term)
  (cond
   ((null term)
    (constant-nil))
   ((atom term)
    (constant term))
   ((and (consp term) (eq (first term) 'vari))
    (fam-construct-variable (second term)))
   ((and (consp term) (eq (first term) 'tup))
    (fam-construct-list (cdr term)))
   (t
    (let ((temp (map 'list #'fam-construct-term (rest term)))
	  (ref
	   (new-struc (functor (list (first term) (length (rest term)))))))
     (mapc #'new-value temp)
     ref))))

(defun fam-construct-variable (var)
  (cond ((numberp var)
	 (new-value (list :ref var)))
	((assoc var *fam-user-variables*)
	 (new-value `(:ref ,(second (assoc var *fam-user-variables*)))))
	(t
	 (let ((temp (new-variable)))
	   (setf  *fam-user-variables* (cons `(,var ,(second temp)) *fam-user-variables*))
	   temp))))

(defun fam-construct-list (term)
  (let ((head (fam-construct-term (car term)))
	(tail (fam-construct-term (if (cdr term)
				      (cons 'tup (cdr term))
				    nil)))
	(ref (new-list-cell)))
    (new-value head)
    (new-value tail)
    ref))


;; WAM --> FAM 1:1


;;; difference to emu-lisp-wandel-extra:: the variables are replaced with the original values

(defun fam-emu-lisp-wandel-extra (x)
 (cond ((eq (word-tag x) :const) (const-or-empty-list (word-value x)))
       ((varp x) (let* ((dx (deref x))
			(var (reverse (assoc (word-value x)
			            (mapcar #'reverse *fam-user-variables*) ;; searching for the address
				    ))))
		   (if var
		       `(vari ,(first var))
		     (gerror "interface" "fam-emu-lisp-wandel-extra"
			     "Unknown Variable returned by the GAMA"))))
       ((eq (word-tag x) :ref) (fam-emu-lisp-wandel-extra (mem x)) )
       ((eq (word-tag x) :list)
	(cons 'tup (cons (fam-emu-lisp-wandel-extra (mem x))
			 (fam-emu-lisp-wandel-extra2 (mem (ref-plus x 1))))))
       ((eq (word-tag x) :struct)
          (let ((name (first (word-value (mem x))))
                (arity (second (word-value (mem x))))
                (ref   (ref-plus x 1)))
	       (if (tupstruct-p name) ; added for tupified structs  M.S. 6/92
		   (cons name 
			 (let ((term 
				(car (fam-emu-lisp-wandel-extra-terms ref arity))))
			      (if (fun-eq term 'tup)
				  (cdr term)
				  (dotted-pair term))))
		   (cons name (fam-emu-lisp-wandel-extra-terms ref arity)))))
       ( t (gerror "gwam" "emul-lisp-wandel-extra" "unknown tag"))))


(defun fam-emu-lisp-wandel-extra2 (x) ; handling lists (dotted pairs allowed!)
  (cond 
   ((eq (word-tag x) :list)
    (cons (fam-emu-lisp-wandel-extra (mem x))
	  (fam-emu-lisp-wandel-extra2 (mem (ref-plus x 1)))))
   ((and (eq (word-tag x) :const) ; nil
	 (null (word-value x)))
    nil)
   ((varp x) (dotted-pair
	      (let* ((dx (deref x))
		     (var (assoc (mapcar #'reverse *fam-user-variables*) ;; searching for the address
				 (word-value x))))
		(if var
		    `(vari ,var)
		  (gerror "interface" "fam-emu-lisp-wandel-extra"
			  "Unknown Variable returned by the GAMA")))
	      ))

   ((eq (word-tag x) :ref) (fam-emu-lisp-wandel-extra2 (mem x)))
   (T (dotted-pair (fam-emu-lisp-wandel-extra x)))))



(defun fam-emu-lisp-wandel-extra-terms (ref arity)
  (if (zerop arity) nil
      (cons (fam-emu-lisp-wandel-extra (mem ref))
              (fam-emu-lisp-wandel-extra-terms (ref-plus ref 1) (1- arity)))))



;;; read fam-back*, fam-query* ...
(gassem (namestring (logdir-file :fam "interface.asm")))

(init-fam)