
; (*module* idx "IDX: Indexing")



; global variables and functions

(defvar idx.*indexing* t)

(defun idx ()
  idx.*indexing*)


(defvar idx.*dbg* nil)


(defvar idx.*min-no-of-proc-clauses* 2)

(defvar idx.*max-no-of-vars* 10)
(defvar idx.*maxdepth* 3)
(defvar idx.*numberofargs* 2)





(defun idx.show-idx-constants ()
  (rf-format "indexing ~a :min-clauses ~a :max-vars ~a :max-depth ~a :max-args ~a :debug ~a"
          (if idx.*indexing* "on" "off")
          idx.*min-no-of-proc-clauses*
          idx.*max-no-of-vars*
          idx.*maxdepth*
          idx.*numberofargs*
          (if idx.*dbg* "on" "off")))


(defun idx.idx-1cmd (paras); executes first command and returns rest paras
  (cond ((eq (car paras) 'on) (setq idx.*indexing* T) (cdr paras))
        ((eq (car paras) 'off) (setq idx.*indexing* nil) (cdr paras))
        ((and (eq (car paras) :max-vars)
              (consp (cdr paras))
              (numberp (cadr paras)))
         (setq idx.*max-no-of-vars* (cadr paras))
         (cddr paras))
        ((and (eq (car paras) :max-depth)
              (consp (cdr paras))
              (numberp (cadr paras)))
         (setq idx.*maxdepth* (cadr paras))
         (cddr paras))
        ((and (eq (car paras) :max-args)
              (consp (cdr paras))
              (numberp (cadr paras)))
         (setq idx.*numberofargs* (cadr paras))
         (cddr paras))
        ((and (eq (car paras) :min-clauses)
              (consp (cdr paras))
              (numberp (cadr paras)))
         (setq idx.*min-no-of-proc-clauses* (cadr paras))
         (cddr paras))
        ((and (eq (car paras) :debug)
              (consp (cdr paras))
              (member (cadr paras) '(on off)))
         (setq idx.*dbg* (eq (cadr paras) 'on))
         (cddr paras))
        (T (rf-format "Warning: unknown command in ~a~%" paras)
           (cdr paras))))

(defun idx.idx-cmd (paras) ; paras is a list of commands
  (if paras
      (idx.idx-cmd (idx.idx-1cmd paras))
      (idx.show-idx-constants)))




; auxiliary functions
; -------------------

(defun rf-format (&rest args)
  (when (rfi-script-mode-p) (apply #'format (cons *rfi-script-output* args)))
  (apply #'format (cons *rfi-standard-output* args)))


