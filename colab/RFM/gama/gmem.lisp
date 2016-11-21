;;;
;;; gmem.lsp -- general memory manager
;;;
;;; (c) Michael Sintek         12/1991
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar gmem.*mem* nil)
(setq gmem.*mem* nil)

(defvar gmem.*freelist* nil)
(setq gmem.*freelist* nil)


(defun gmem.init (size)
  (setq gmem.*mem* (make-array size)
        gmem.*freelist* (cons (cons 0 (1- size)) nil)))


(defun gmem.put (n el)
  (setf (aref gmem.*mem* n) el))

(defun gmem.get (n)
  (aref gmem.*mem* n))

(defmacro gmem.put+ (n el)
  `(setf (svref gmem.*mem* ,n) ,el))

(defmacro gmem.get+ (n)
  `(svref gmem.*mem* ,n))



(defun gmem.alloc (n)
  (let ((res (gmem.alloc2 n gmem.*freelist*)))
       (setq gmem.*freelist* (cdr res))
       (car res)))

(defun gmem.alloc2 (n freelist)
  (cond ((null freelist)
         (gerror "gmem.lsp" "gmem.alloc" "cannot allocate ~a elements." n))
        ((<= n (1+ (- (cdar freelist) (caar freelist))))
         (let* ((start (caar freelist))
                (next (+ start n)))
               (if (> next (cdar freelist))
                   (cons start (cdr freelist))
                   (cons start
                         (cons (cons next (cdar freelist)) (cdr freelist))))))
        (T (cons (car freelist) (gmem.alloc2 n (cdr freelist))))))

(defun gmem.alloc/clr (n &optional x)
  ; allocate and clear (set to nil/(eval x))
  (let ((start (gmem.alloc n)))
       (dotimes (i n start)
         (gmem.put (+ start i) (eval x)))))


(defun gmem.dealloc (start n)
  (setq gmem.*freelist* (cons (cons start (1- (+ start n))) gmem.*freelist*)))

(defun gmem.dealloc-range (start end)
  (setq gmem.*freelist* (cons (cons start end) gmem.*freelist*)))


(defun gmem.defractionize ()
  ; clean up the free list
  (setq gmem.*freelist*
    (gmem.defrac1 (sort gmem.*freelist* #'(lambda (x y) (< (car x) (car y)))))))

(defun gmem.defrac1 (fl)
  (cond ((null (cdr fl)) fl)
	((= (1+ (cdar fl)) (caadr fl))
	 (gmem.defrac1 (cons (cons (caar fl) (cdadr fl)) (cddr fl))))
	(T (cons (car fl) (gmem.defrac1 (cdr fl))))))


; print functions
; ---------------

(defvar gmem.*print-fcts*)
(setq gmem.*print-fcts* nil)

(defun gmem.add-print-fct (fct)
  (setq gmem.*print-fcts* (cons fct gmem.*print-fcts*)))

(gmem.add-print-fct #'(lambda (x) (gprinc x) T)) ; default

(defun gmem.print-mem-cell (x)
  (gmem.print-mem-cell2 x gmem.*print-fcts*))

(defun gmem.print-mem-cell2 (x print-fcts)
  (unless (funcall (car print-fcts) x)
	  (gmem.print-mem-cell2 x (cdr print-fcts))))

(defun gmem.list (begin end)
  (do ((i begin (1+ i)))
      ((> i end))
      (gformat "~a: " i)
      (gmem.print-mem-cell (gmem.get i))
      (gterpri)))
      ;(gformat "~a: ~a~%" i (gmem.get i))))



