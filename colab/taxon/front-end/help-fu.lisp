

(defvar *trace-h* nil)
(defun trace-h (e) (if *trace-h* (print e)e) ) 

(defun h-add-taxon (l) (trace-h (add-taxon l)) NIL)
(defun h-attr-filler (l) (print (apply 'fattr-filler l)) NIL)
(defun h-attr-value-pairs (l) (print (apply 'fattr-value-pairs l)) NIL)
(defun h-check (l) (print (check-abox)) NIL)
(defun h-classify (l) (trace-h (apply 'classify l)) NIL)
(defun h-classify-all (l)  (trace-h (apply 'classify-all nil)) NIL)
                            
(defun h-clear-abox (l)     ;(print (progn (apply 'clear-abox nil) (apply 'clear-realization nil))) NIL)
                            (print (clear-abox)) NIL)
(defun h-clear-tbox (l) (print (apply 'clear-tbox l)) NIL)
;(defun h-clear-edom (l)     ; (print (apply 'clear-edom l)) NIL)
;                            (print "not yet implemented") NIL)
;(defun h-clear-hi (l)       ; (print (apply 'init-verband l)) NIL)
;                            (print "not yet implemented") NIL)
;(defun h-clear-realize (l)  ; (print (apply 'clear-realization nil)) NIL)
;                            (print "not yet implemented") NIL)
(defun h-get-def (l) (print (apply 'get-def l)) NIL)
(defun h-hierarchy (l) (trace-h (apply 'fhierarchy (list l))) NIL)
(defun h-defconcept (l) (trace-h (apply 'fconc (list l))) NIL)
(defun h-deffeature (l) (trace-h (apply 'fattr l)) NIL)
(defun h-defind (l) (trace-h (apply 'fmake-indi (list l))) NIL)
(defun h-defpconcept (l) (trace-h (apply 'fprim l)) NIL)
(defun h-defpredicate (l) (trace-h (apply 'fcpred (list l))) NIL)
(defun h-defrole (l) (trace-h (apply 'frole l)) NIL)
(defun h-destroy-taxon (l) (print (apply 'destroy-taxon l)) NIL)
;(defun h-edom-instal (l)     ; (trace-h (apply 'edom-instal l)) NIL)
;                             (print "not yet implemented") NIL)
(defun h-equivalence-class (l) (print (apply 'fequi l)) NIL)
(defun h-filter-graph (l) (apply 'filter-subsumption-graph l) NIL)
(defun h-_get-concepts (l) (print (get-concept-names)) NIL)
(defun h-im-lowers (l) (print (apply 'filo l)) NIL)
(defun h-im-lowers-and-ec (l) ; (print (apply 'im-lowers-and-ec l)) NIL)
                              (print "not yet implemented") NIL)
(defun h-im-uppers (l) (print (apply 'fiup l)) NIL)
(defun h-im-uppers-and-ec (l) ; (print (apply 'im-uppers-and-ec l)) NIL)
                              (print "not yet implemented") NIL) 
(defun h-instance? (l) (print (apply 'finstance? l)) NIL)
(defun h-instances (l) (print (apply 'finstances l)) NIL)
(defun h-lowers (l) (print (apply 'flo l)) NIL)
(defun h-lsc (l) (apply 'lsc l) NIL)
(defun h-pp-taxon (l) (trace-h (apply 'l-all l)) NIL)
(defun h-pp-taxon-parts (l) (trace-h (apply 'fl l)) NIL)
(defun h-print-hierarchy (l) (trace-h (apply 'pphi l)) NIL)
(defun h-role-fillers (l) (print (apply 'frole-fillers l)) NIL)
(defun h-realize-individual (l) (print (apply 'realize-individual l)) NIL)
(defun h-role-value-pairs (l) (print (apply 'frole-value-pairs l)) NIL)
(defun h-sati? (l) (print (apply #'fsati-co? l)) NIL)                      ;; only conceptnames, no terms
(defun h-subs? (l) (print (apply 'fsubs? l)) NIL)            ;; computes, another fu. exists for look-up
(defun h-uppers (l) (print (apply 'fup l)) NIL)
(defun h-weakly-realize (l) (print (apply 'fweakly-realize l)) NIL)
(defun h-x-value-pairs (l) (print (apply 'fx-value-pairs l)) NIL)

(defun h-concept-closure (l) (print (apply 'concept-closure l)) NIL)

#|
(defun h-save-hierarchy (l)   ; (trace-h (apply 'save-hierarchy nil)) NIL)
                              (print "not yet implemented") NIL)
(defun h-save-realization (l) ; (trace-h (apply 'store-realizations nil)) NIL)
                              (print "not yet implemented") NIL)
(defun h-restore-hierarchy (l) ; (trace-h (apply 'restore-hierarchy nil)) NIL)
                               (print "not yet implemented") NIL)
(defun h-restore-realization (l)   ; (trace-h (apply 'restore-realizations nil)) NIL)
                                (print "not yet implemented") NIL)
|#

(defun h-switch (l) (print (apply 'switch l)) NIL)
(defun h-my-trace (l) (trace-h (apply 'my-trace l)) NIL)
(defun h-my-untrace (l) (trace-h (apply 'my-untrace l)) NIL)

#|(defun h-l-realize (l)      #| (print (if (concept? (car l))                 was soll das ??
                                        (get-instances (car l))
                                        (get-most-specialized-concepts (car l)))) NIL) |#
                             (print "not yet implemented") NIL)
|#

(defun h-assertion (l) (print (apply 'fasse l)) NIL)

(defun h-make-indi (l) (print (apply 'make-indi l)) NIL)
(defun h-make-asse (l) (print (apply 'make-asse l)) NIL)
(defun h-get-concept-names (l) (print (apply 'get-concept-names l)) NIL)
(defun h-get-instance-names (l) (print (apply 'get-instance-names l)) NIL)
(defun h-get-role-names (l) (print (apply 'get-role-names l)) NIL)
(defun h-get-attr-names (l) (print (apply 'get-attr-names l)) NIL)
(defun h-assert-ind? (l) (print (apply 'assert-ind? (list (second l) (first l) (third l)))) NIL)

(defun h-drawhi (l) (apply 'fdrawhi l) NIL)

(defun h-ihw (l) (init-hierarchy-window) NIL)
(defun h-shw (l) (select-hierarchy-window) NIL)
(defun h-dhw (l) (draw-hierarchy-window) NIL)
(defun h-chw (l) (clear-hierarchy-window) NIL)

(defun h-iaw (l) (init-object-window) NIL)
(defun h-saw (l) (select-object-window) NIL)
#+Symbolics (defun h-daw (l) (apply 'ai-draw-object-window (list l)) NIL)
#-Symbolics (defun h-daw (l) (princ (format nil "Only available under Symbolics CL~%")) (terpri))
(defun h-caw (l) (clear-object-window) NIL)


(defun h-apred (l) (trace-h (apply 'fapred (list l))) NIL)
(defun h-ofam (l) (trace-h (apply 'fofam  l)) NIL)
(defun h-cfam (l) (trace-h (apply 'fcfam l)) NIL)
(defun h-sfam (l) (trace-h (apply 'fsfam  l)) NIL)

