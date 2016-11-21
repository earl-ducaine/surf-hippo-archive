(in-package 'our-fs)


(use-package '(user lisp #+Genera scl))
#-:our-fs (progn 
(princ "Our-FS  --  File Name Abstraction  18.02.92") (terpri)
(defvar *compile-in-use* nil)
(defvar compile-list nil)
(export '(compile-all))

(defun compile-all () 
	(mapcar
	 `(lambda (x)
		  (let ((*our-current-pck* *package*))
			  (in-package 'user)
			  (compile-file  x)
			  (setf *package* *our-current-pck*)))
	  (reverse our-fs::compile-list)))

#+Genera (progn 



;This file some functions are defined that abstract from physical
;pathnames.


(export '(*compile-in-use* use-file logdir-define  logdir-look-up
           logdir-extend1  logdir-cd1 logdir-file))

(defun use-file (x)
    (let ((x-lisp (send x :new-raw-type "lsp")))
      (if our-fs::*compile-in-use* (push x-lisp our-fs::compile-list))
      (let ((*our-current-pck* *package*)
	    (result nil))
	  (in-package 'user)
	  (setf result
		(if (not (load x :if-does-not-exist nil))
		    (load x-lisp)
		    t))
	  (setf *package* *our-current-pck*)
	  result)
    ))

(defvar table nil)

(defun logdir-define (key path)
	(declare (special table))
	(push (cons key (fs:merge-pathnames path))
              table))

(defun logdir-look-up (key)
	(declare (special table))
	(cdr (assoc key table)))

(defun logdir-cd1 (key one-step) ;".." allowed for one-step
	(declare (special table))
	(let ((p (logdir-look-up key)))
	    (truename
	     (send p
		   :new-raw-directory
		   (append (send p :raw-directory)
			   (list one-step))))))



(defun logdir-extend1 (key one-step)
	(declare (special table))
	(let* ((p (logdir-look-up key))
	       (dir (send p :raw-directory)))
	    (send p
		  :new-raw-directory (append (if (eq :root dir) nil dir)
					     (list one-step)))))

(defun logdir-file (key file)
	(declare (special table))
	(send (send (logdir-look-up key) :new-raw-name file) :new-type :unspecific))
(logdir-define :root (pathname "/"))
)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;kcl


;This file some functions are defined that abstract from physical
;pathnames.

#-Genera (progn 

(export '(  *compile-in-use* use-file logdir-define  logdir-look-up  logdir-extend1  logdir-file logdir-cd1))

(defun use-file (x &aux (x.lsp (format nil "~a.lsp" (namestring x))))
  (if (not (load x :if-does-not-exist nil))
      (progn (load x.lsp) (if *compile-in-use* (push x.lsp compile-list)))
      (progn (if *compile-in-use* (push x compile-list) t))))

(defvar table nil)

(defun logdir-define (key path)
	(declare (special table))
	(push (cons key (pathname-directory path))
              table))

(defun logdir-look-up (key)
	(declare (special table))
	(cdr (assoc key table)))

(defun logdir-extend1 (key one-step)
	(declare (special table))
	(make-pathname :directory (append (logdir-look-up key)
	                                  (list one-step))))

(defun logdir-cd1 (key one-step) ; ".." is allowed for one-step
	(declare (special table))
	(truename
	 (make-pathname :directory
			(append (logdir-look-up key)
				(list one-step)))))

      
(defun logdir-file (key file)
	(declare (special table))
	(make-pathname :name file
		       :directory (logdir-look-up key)))
(logdir-define :root (pathname "/"))
)

(pushnew :our-fs user::*features*)
)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
