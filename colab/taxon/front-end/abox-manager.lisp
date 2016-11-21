;;; TBox-Manager
;;; TBox als Hash-Table sowie TBox-Eintraege als Strukte
;;; 17-03-92 aa


(defstruct (abox-entry 
                      (:print-function print-abox-entry)
            )
                      identifier
                      type
                      visible?
                      extern
                      intern
                      )

(defun print-abox-entry (r s k)
  
  (print-if (abox-entry-identifier r)        "Identifier                      : ")
  (print-if (abox-entry-type r)              "Type                            : ")
  (print-if (abox-entry-extern r)            " Extern                          : ")
  (terpri)
  (princ (format nil
"--------------------------------------------------------------------------"))
  (terpri)
  
  )

(defun abox-listing ()
   (terpri)
   (mapcar #'print-abox-entry
            *abox-table*)
    T)

(defvar *abox-table* nil)           ;; evt spaeter nach Individuen ordnen ??

(defun clear-abox ()
   (setf *abox-table* nil)
   (setf *individual-list* nil)
;   (clear-new-objects)
   (clear-real-ord)
   (clear-number-counter)
   (clrhash *msc-table)
   (clrhash *individual-table*)
)

(defun abox-insert (&key extern type intern identifier visible?)
   (if (abox-member? identifier)
       (progn
           (terpri)
           (princ (format nil "Warning -- second assertion ~a is ignored" identifier))
           (terpri)
       )
       (setf *abox-table*
             (adjoin
                 (make-abox-entry :identifier identifier
                                  :type type
                                  :intern intern
                                  :visible? visible?
                                  :extern extern
                 )
                 *abox-table*
                 :test #'equal
              )
        )
    )
)

(defun copy-abox ()
   (copy-list *abox-table*))

(defun get-abox-entry (item)         ;; not yet implemented
   NIL
)



(defun abox-member? (identifier)         ;; not yet
   NIL
)

(defun cut-abox ()
  (setf *abox-table* (rest *abox-table*))
)

(defun visible-assertion? (a)
   (abox-entry-visible? a))


 
;;;


(defun fattr-filler (attr ind)
  (let ((attr (if (symbolp attr) (list attr) attr)))
   (get-ind2-of-filler-asse
           (my-abox-entry-extern
                  (find-if #'(lambda (x) (and (equal 'attr-filler-assertion (abox-entry-type x))
                                              (visible-assertion? x)
                                           (equal attr (get-rel-of-filler-asse (abox-entry-extern x)))
                                           (equal ind  (get-ind1-of-filler-asse (abox-entry-extern x)))))
                           *abox-table*)))))           ;;; find-if - see STEELE p. 404

(defun my-abox-entry-extern (x)
   (if (not (null x))
       (abox-entry-extern x)))


(defun frole-fillers (role ind)
  (let ((role (if (symbolp role) (list role) role)))
   (mapcan #'(lambda (x)  (and (and (equal 'role-filler-assertion (abox-entry-type x))
                                    (visible-assertion? x)
                                    (equal ind  (get-ind1-of-filler-asse (abox-entry-extern x)))  
                                    (equal role (get-rel-of-filler-asse  (abox-entry-extern x))))
                               (list (get-ind2-of-filler-asse         (abox-entry-extern x)))))
           *abox-table*)))              ;;; mapcan as a filter - see STEELE p. 172

(defun fattr-value-pairs (ind)
   (mapcan #'(lambda (x)  (and (and (equal ind  (get-ind1-of-filler-asse (abox-entry-extern x))) 
                                    (visible-assertion? x) 
                                    (equal 'attr-filler-assertion (abox-entry-type x)))
                               (list (cdr (abox-entry-extern x)))))
           *abox-table*))              ;;; mapcan as a filter - see STEELE p. 172


(defun frole-value-pairs (ind)                 ;; besser mit dolist??
   (do ( (abox *abox-table* (cdr abox))
          (abox-entry (car *abox-table*) (cadr abox))
          (result nil) )
        ( (null abox) result )
         (let* ((ext-rep (abox-entry-extern abox-entry))
                (as-ind  (get-ind1-of-filler-asse ext-rep))
                (as-type (abox-entry-type abox-entry))
                (as-x    (get-rel-of-filler-asse ext-rep))
                (as-filler (get-ind2-of-filler-asse ext-rep)) )
              (if (and (equal ind as-ind)
                       (equal 'role-filler-assertion    as-type))
                  (if (not (assoc as-x result))
                      (setq result (acons as-x (list as-filler) result))
                      (let ((old (assoc as-x result)))
                          (rplacd old (cons as-filler (cdr old)))))))))
                

(defun fx-value-pairs (ind)
  (do ( (abox *abox-table* (cdr abox))
	(abox-entry (car *abox-table*) (cadr abox))
	(result nil) )
      ( (null abox) result )
      (let* ((ext-rep (abox-entry-extern abox-entry))
	     (as-ind  (get-ind1-of-filler-asse ext-rep))
	     (as-type (abox-entry-type abox-entry))
	     (as-x    (get-rel-of-filler-asse ext-rep))
	     (as-filler (get-ind2-of-filler-asse ext-rep)) )
	    (if (and (equal ind as-ind)
		     (visible-assertion? abox-entry)
		     (or  (equal 'role-filler-assertion    as-type)
			  (equal 'attr-filler-assertion as-type)))
		(if (not (assoc as-x result))
		    (setq result (acons as-x (list as-filler) result))
		    (let ((old (assoc as-x result)))
			 (rplacd old (cons as-filler (cdr old)))))))))



(defmacro attr-filler  (attr ind)
  `(fattr-filler ',attr ',ind))

(defmacro role-fillers (role ind)
  `(frole-fillers ',role ',ind))

(defmacro attr-value-pairs (ind)
  `(fattr-value-pairs ',ind))

(defmacro role-value-pairs (ind)
  `(frole-value-pairs ',ind))

(defmacro x-value-pairs (ind)
   `(fx-value-pairs ',ind))