;;;
;;; gcla.lsp -- general classifier utility
;;;
;;; (c) Michael Sintek             12/1991
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; plist  ::= { <key> <value> }        (property list)
; lalist ::= { ( <key> <value> ) }    (listified alist)
; clob   ::= ( <class> . <lalist> )   (classified object)


(defun gcla.plist2lalist (plist)
  (when plist
        (cons (list (car plist) (cadr plist))
              (gcla.plist2lalist (cddr plist)))))


(defun gcla.new-object (class &rest plist)
  (cons class (gcla.plist2lalist plist)))

(defun gcla.new-object-from-plist (class plist)
  (cons class (gcla.plist2lalist plist)))

(defun gcla.new-object-from-lalist (class lalist)
  (cons class lalist))

(defun gcla.new-object/check (class pclob)
  ; pclob = clas. obj. in plist form
  ; class = expected class of pclob
  ; used for input from files !
  (if (and (listp pclob) (eq class (car pclob)))
      (gcla.new-object-from-plist class (cdr pclob))
      (gerror "gasm" "gasm.new-object/check"
        "classfied object of class ~a expected, not ~a" class pclob)))


(defun gcla.add (clob key entry)
  ; key is supposed not to exist in lalist
  (cons (car clob)
        (cons (list key entry) (cdr clob))))



(defun gcla.put1 (clob key entry)
  ; same as gcla.add, but overwrite existing entries with same key
  (cons (car clob)
        (cons (list key entry)
              (remove-if #'(lambda (pair) (eq key (car pair))) (cdr clob)))))

(defun gcla.put (clob &rest plist)
  (gcla.put-from-lalist clob (gcla.plist2lalist plist)))

(defun gcla.put-from-lalist (clob lalist)
  (if lalist
      (gcla.put-from-lalist
        (gcla.put1 clob (caar lalist) (cadar lalist))
        (cdr lalist))
      clob))


(defun gcla.get (clob key)
  (cadr (assoc key (cdr clob))))


(defun gcla.del (clob key)
  (cons
    (car clob)
    (remove-if #'(lambda (pair) (eq key (car pair))) (cdr clob))))


(defun gcla.class (clob)
  (car clob))


(defun gcla.lalist (clob)
  (cdr clob))


(defun gcla.get-tags (clob)
  (mapcar #'car (gcla.lalist clob)))



