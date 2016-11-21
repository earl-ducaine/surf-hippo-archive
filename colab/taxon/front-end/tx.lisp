;***************************************************************************
;***************************************************************************
;*
;*  Module            TX-ACCESS
;*  File              tx
;*  Author            Andreas Abecker
;*  Edited            31.07.91
;*  Last change       
;*  Details changed   
;*
;*  Comment           access primitives for colab
;*  Status            not released
;*
;****************************************************************************


;(in-package 'tx-access)


(defvar *tx-readtable* '(sati? subs? equivalence-class im-lowers im-uppers
                         instances attr-filler role-fillers
                         attr-value-pairs role-value-pairs x-value-pairs
                         realize-individual concept-closure assert-ind?
                         make-indi make-asse 
                         get-instance-names get-concept-names get-attr-names get-role-names))

;(defmacro tx (opcode &rest operands)
;  (declare (special *tx-readtable*))
;  `(if (member ',opcode *tx-readtable*)
;      (eval (cons ',opcode ',operands))
;      (error "undefined access primitive given to TAXON  >> ")))

(setf *tx-opcode-table* '((sati? fsati-co?) (subs? fsubs?) (equivalence-class fequi) (im-lowers ilo) (im-uppers iup)
          (instances finstances) (attr-filler fattr-filler) (role-fillers frole-fillers) (attr-value-pairs fattr-value-pairs)
          (role-value-pairs frole-value-pairs) (x-value-pairs fx-value-pairs) (realize-individual realize-individual)
          (concept-closure concept-closure) (assert-ind? assert-ind?) (make-indi (lambda (x) (tx::fmake-indi (list x))))
          (make-asse tx::fasse)
          (get-instance-names get-instance-names) (get-concept-names get-concept-names) (get-attr-names get-attr-names)
          (get-role-names get-role-names)))

(defun tx (opcode &rest operands)
    (declare (special *tx-readtable*))
    (if (member opcode *tx-readtable*)
        (apply (second (assoc opcode *tx-opcode-table*)) operands)
        (progn (print "undefined access primitive given to TAXON")
               (princ opcode)
             (throw :error nil))))

;(let (#+genera (sys:inhibit-fdefine-warnings :just-warn))


(defun colab-consult-taxon (elem)
  (taxon::h-add-taxon elem)
  NIL)

(defun colab-destroy-taxon (dummy)
  (taxon::h-destroy-taxon dummy)
  NIL)

(defun colab-list-taxon (elem)
  (pp-taxon)
  NIL)

(defun taxon-test (x)(princ "taxon:")(print x) NIL)

(defun taxon-else (userline)
	(let ((*print-case* :downcase)) (princ (format nil "Invalid request: ~% ~A ~%" userline)))
	nil)

;)
