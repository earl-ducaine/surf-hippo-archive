;;;
;;; gaux.lsp -- general auxiliary functions
;;;
;;; (c) Michael Sintek              12/1991
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun gerror (file fct text &rest args)
  (gformat "error: ")
  (apply #'gformat (cons text args))
  (gterpri)
  (gformat "[signalled by function '~a' in file '~a']~%~%" fct file)
  (error "execution halted."))


; print/read functions (should be patched in a real environment!)

#|
(defun gprint (arg)
  (print arg))

(defun gprinc (arg)
  (princ arg))

(defun gformat (&rest args) ; format using standard output!
  (apply #'format (cons t args)))

(defun gpprint (arg)
  (format t "~s~%" arg))

(defun gterpri ()
  (terpri))


|#


(defun gprint (arg)
  (rf-print arg))

(defun gprinc (arg)
  (rf-princ arg))

(defun gformat (&rest args)
  (apply #'rf-format args))

(defun gpprint (arg)
  (rf-pprint arg))

(defun gterpri ()
  (rf-terpri))


(defun g-y-or-n-p (&rest args)
  (apply #'gformat args)
  (gformat " (Y/N) ")
  (let ((answer (eq 'y (read))))
       (gterpri)
       answer))


