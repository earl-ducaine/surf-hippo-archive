(defun gwam.assemble-fw-db (list-of-preds) ; list-of-preds  is a list containg all procedure names
					   ; (of the form name/arity)
  (gassem (mapcan #'gwam.get-fw-asm-code list-of-preds)))

(defun gwam.get-fw-asm-code (name/arity)
  (append
   (list '.proc 
         '(.module forward-code)
	 name/arity
	 '(.module user))
   (copy-list (get name/arity 'forw-procedure)))) 


