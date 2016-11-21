; auxiliary functions
;;;;;;;;;;;;;;;;;;;;;


#|
(*module* aux "AUX: Auxiliary Functions")

(*export* 
          ppr
          sym2str
          str2list
	  sym2list
          string-concat
	  symbol<
	  export-dynamic
	  import-dynamic
          boring-list
          funcall*
          samebeginning
	  get-first-n-elements-and-rest
	  multiple-splitting
)
|#


; function pretty printer
; -----------------------

(defmacro ppr (fct)
  (pprint (symbol-function fct)))


; type casts etc.
; ---------------

(defmacro sym2str (symbol)
  `(string ,symbol))

(defun str2list (string)
  (aux.str2list-2 string 0 (length string)))

(defun aux.str2list-2 (string pos len)
  (cond ((equal pos len) nil)
	(T (cons (char string pos) (aux.str2list-2 string (1+ pos) len)))))

(defun sym2list (symbol)
  (str2list (sym2str symbol)))

(defun string-concat (str1 str2)
  (format nil "~A~A" str1 str2))

(defun symbol< (sym1 sym2)
  (string< (sym2str sym1) (sym2str sym2)))



; dynamic variables
; -----------------

(defmacro export-dynamic (&rest variables)
  `(declare ,@(aux.dynamic variables)))

(defun aux.dynamic (variables)
  (unless (null variables)
	  (cons (list 'special (car variables))
		(aux.dynamic (cdr variables)))))


(defmacro import-dynamic (&rest variables)
  `(export-dynamic ,@variables))


; misc
; ----

(defun boring-list (l) ; l = (e,e,...) -> returns e, nil otherwise
  (cond ((null (cdr l)) (car l))
	((equal (car l) (cadr l)) (boring-list (cdr l)))))


(defmacro funcall* (name &rest r)
  `(when ,name (funcall ,name ,@r)))


(defun samebeginning (l1 l2)
  (cond ((null l1))
	((equal (car l1) (car l2))
	 (samebeginning (cdr l1) (cdr l2)))
	(t nil)))

(defun get-first-n-elements-and-rest (n l)
  (if (= n 0)
      (cons nil l)
      (let ((f*r (get-first-n-elements-and-rest (1- n) (cdr l))))
	   (cons (cons (car l) (car f*r))
		 (cdr f*r)))))

(defun multiple-splitting (n l)
  (mapcar #'(lambda (l)
		    (get-first-n-elements-and-rest n l))
	  l))


; set macros
; ----------

(defmacro set-inc (var)
    `(setq ,var (1+ ,var)))

(defmacro set-cons (x l)
    `(setq ,l (cons ,x ,l)))

(defmacro set-cdr+ (l)
    `(setq ,l (cdr ,l)))



