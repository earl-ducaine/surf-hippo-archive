;;; -*- Mode: LISP -*-


; remove normalizer to allow lisp functions

(defun normalize-database (db) db)




; new cl-extra + emu-lisp-wandel-extra for correct handling of lists
; and free variables


(definstr cl-extra (fun arity)
            (do ( (i arity (1- i))
                  argliste
                  temp
                )
                ( (= i 0)
                  (apply fun argliste))
                ;;; (if (eq (word-tag
			 (setq temp (deref (argument-reg i)))
		;;;	 ) 'ref)
                ;;;  (error "lispcall with variable IMPOSSIBLE")
                 (setq argliste (cons (emu-lisp-wandel-extra temp) argliste))
                ;;; ) ; if
            ) ; do
)


(defun emu-lisp-wandel-extra (x)
 (cond ((eq (word-tag x) 'const) (const-or-empty-list (word-value x)))
       ((varp x) (list 'vari '|Ny| (word-value x)))
       ((eq (word-tag x) 'ref) (emu-lisp-wandel-extra (mem x)) )
       ((eq (word-tag x) 'list)
	(cons 'tup (cons (emu-lisp-wandel-extra (mem x))
			 (emu-lisp-wandel-extra2 (mem (ref-plus x 1))))))
       ((eq (word-tag x) 'struct)
          (let ((name (first (word-value (mem x))))
                (arity (second (word-value (mem x))))
                (ref   (ref-plus x 1)))
            (cons name (emu-lisp-wandel-extra-terms ref arity))))
       ( t (error "don't know this tag"))))


(defun emu-lisp-wandel-extra2 (x) ; handling lists
  (cond 
   ((eq (word-tag x) 'list)
    (cons (emu-lisp-wandel-extra (mem x))
	  (emu-lisp-wandel-extra2 (mem (ref-plus x 1)))))
   ((and (eq (word-tag x) 'const)
	 (null (word-value x)))
    nil)
   (T (emu-lisp-wandel-extra x)))) ; dotted pairs should not be allowed !!!


(defun const-or-empty-list (x)
  (if x x '(tup)))


(defun emu-lisp-wandel-extra-terms (ref arity)
  (if (zerop arity) nil
      (cons (emu-lisp-wandel-extra (mem ref))
              (emu-lisp-wandel-extra-terms (ref-plus ref 1) (1- arity)))))


; replace show-term and show-terms by emu-lisp-extra
; (the only difference is the correct handling of
;  empty lists)

(defun show-term (x flag) ; flag ignored
  (emu-lisp-wandel-extra x))

(defun show-terms (x arity)
  (emu-lisp-wandel-extra-terms x arity))


