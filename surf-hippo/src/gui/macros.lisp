;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: WINDOWS-HACK; Base: 10 -*-
;;; Lyle Borg-Graham, Equipe Cogniscience, Institut Alfred Fessard, CNRS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              The Surf-Hippo Neuron Simulator                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Surf-Hippo project at the  ;;;
;;; Center for Biological Information Processing, Department of     ;;;
;;; Brain and Cognitive Sciences, Massachusetts Institute of        ;;;
;;; Technology, and currently at the Equipe Cogniscience, Institut  ;;;
;;; Alfred Fessard, CNRS and has been placed in the public domain.  ;;;
;;; If you are using this code or any part of Surf-Hippo, please    ;;;
;;; contact surf-hippo@ai.mit.edu to be put on the mailing list.    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; GUI Source file: macros.lisp


(IN-PACKAGE "WINDOWS-HACK")

;;; Contains various macros and utilities that are required by the MENU-HACK and PLOT-HACK systems.

(defmacro s-value-list (schemae slot value)
  `(mapcar #'(lambda (schema) (s-value schema ,slot ,value)) (coerce-to-list ,schemae)))

(defmacro list-position (element list)
  `(let ((elt ,element))
    (loop for thing in ,list
     for count fixnum from 0
     when (equal elt thing) do (return count)
     finally (return nil))))

(defmacro list-position-number (number list)
  `(progn
    (loop for thing in ,list
     for count fixnum from 0
     when (= ,number thing) do (return count)
     finally (return nil))))

(defmacro list-position-single-float (number list)
  `(let ((num ,number))
    (loop for thing in ,list
     for count fixnum from 0
     when (= (the sf num) (the sf thing)) do (return count)
     finally (return nil))))

(defmacro string-case (keyform &body cases)
  "STRING-CASE Keyform {({(Key*) | Key} Form*)}*
Evaluates the Forms in the first clause with a Key STRING= to the value of
Keyform.  If a singleton key is T then the clause is a default clause."
  (case-body 'case keyform cases t 'string= nil nil))


(defmacro fixnum-max (num1 num2)
  `(let ((arg1 ,num1)
	 (arg2 ,num2))
     (if (> (the fixnum arg1) (the fixnum arg2)) arg1 arg2)))

(defmacro fixnum-min (num1 num2)
  `(let ((arg1 ,num1)
	 (arg2 ,num2))
     (if (< (the fixnum arg1) (the fixnum arg2)) arg1 arg2)))


(defmacro atom-coerce-to-double (val)
  `(let ((arg ,val))
     (if (numberp arg)
	 (the df (typecase arg
			   (double-float arg)
			   (t (coerce arg 'double-float))))
       (sim-error (format nil "~a is not a number (from COERCE-TO-DOUBLE)!" arg)))))

(defmacro coerce-to-double (arg)
  `(let ((internal-arg ,arg))
     (typecase internal-arg
	       (cons (loop for val in internal-arg collect (atom-coerce-to-double val)))
	       (t (atom-coerce-to-double internal-arg)))))

(defmacro d-flt (arg)
  "Returns the number ARG as a double-float."
  `(atom-coerce-to-double ,arg))

(defmacro d-flt-gen (arg)
  "Returns the number ARG as a double-float, or if ARG is a list of numbers, returns a list of double-floats."
  `(coerce-to-double ,arg))


(defmacro atom-coerce-to-single (val)
  `(let ((arg ,val))
     (if (numberp arg)
	 (the sf (typecase arg
			   (single-float arg)
			   (t (coerce arg 'single-float))))
       (sim-error (format nil "~a is not a number (from COERCE-TO-SINGLE)!" arg)))))

(defmacro coerce-to-single (arg)
  `(let ((internal-arg ,arg))
     (typecase internal-arg
	       (cons (loop for val in internal-arg collect (atom-coerce-to-single val)))
	       (t (atom-coerce-to-single internal-arg)))))

(defmacro s-flt (arg)
  "Returns the number ARG as a single-float."
  `(the sf (atom-coerce-to-single ,arg)))

(defmacro s-flt-gen (arg)
  "Returns the number ARG as a single-float, or if ARG is a list of numbers, returns a list of single-floats."
  `(coerce-to-single ,arg))

;;; DEFVARS and DEFVAR-W-VALUE are macros that allow the declaring and
;;; assigning of more than one global variable at a time.

(defmacro defvars (&rest list)
  `(progn ,@(loop for variable in list collect `(defvar ,variable))))

(defmacro defvars-w-value (&rest list)
   `(progn ,@(loop for (variable value) in list collect `(defvar ,variable ,value))))

;; (defmacro cond-every (&rest list)
;;  `(progn ,@(loop for (condit exec) in list collect `(if ,condit (progn ,exec)))))

(defmacro cond-every (&rest list)
  `(progn ,@(loop for clause in list
	     collect `(when ,(car clause)
		       ,(cons 'progn  (cdr clause))))))



#|
(defmacro defvar-string (var-name value)
  `(progn
    (proclaim '(special ,(read-from-string var-name)))
    (setq ,(read-from-string var-name) ,value)))

(defun defvar-string3 (var-name ;value
		       )
  (proclaim (special (read-from-string var-name)))
;  (setq (read-from-string var-name) value)
  )

(defmacro defvar-string2 (var-name value)
  `(progn
    (proclaim '(special (read-from-string ,var-name)))
    (setq (read-from-string ,var-name) ,value)))

(defmacro setq-string (var-name value)
  `(progn
    (setq ,(read-from-string var-name) ,value)))
     
(defun varstring (var-name value)
  (setf (symbol-value (read-from-string (string var-name))) value))
  
|#

(defun make-var (symbol) 
      (intern (string-upcase symbol)))



(defun create-output-symbol (name &optional (export-it t) &rest names)
  (let ((symbol
	 (intern
	  (loop for extra in names
		do (setq name (string-upcase  (format nil "~a-~a" name extra)))
		finally (return (string-upcase name)))
				       
	  *package*)))
    (when export-it (export symbol))
    symbol))



;; Maybe this wil be useful.
#+:cmu (deftype index () `(unsigned-byte 32))

#-:cmu (deftype index () 'fixnum)




;; **********************************
;;
;; Some Functions
;;
;; **********************************


;; we could also use this for window objects:
;; (member win (garnet-debug:windows))



;; Stolen from CMUCL17C code/query.lisp.
(defun yes-or-no-p-default-no (&optional format-string &rest arguments &key default)
  "Clears the input, beeps, prints the message, if any, and reads characters from *QUERY-IO* until the
user enters YES \(case insensitive\) as an affirmative. If user only RETURNs or types something else, then returns
DEFAULT [modification from cmucl 17c query.lisp]."
  (clear-input *query-io*)
#+garnet (inter:beep)
  (when format-string
    (fresh-line *query-io*)
    (apply #'format *query-io* format-string arguments)
    (force-output *query-io*))
  (if (string-equal (string-trim " 	" (read-line *query-io*)) "yes")
      t
      default))
  

(defun y-or-n-p-default-no (&optional format-string &rest arguments &key default)
  "Clears the input, beeps, prints the message, if any, and reads characters from *QUERY-IO* until the
user enters Y \(case insensitive\) as an affirmative. If user only RETURNs or types something else,
then returns DEFAULT [modification from cmucl 17c query.lisp]."
  (clear-input *query-io*)
#+garnet (inter:beep)
  (when format-string
    (fresh-line *query-io*)
    (apply #'format *query-io* format-string arguments)
    (force-output *query-io*))
  (if (string-equal (string-trim " 	" (read-line *query-io*)) "Y")
      t
      default))


(defun quit ()
  (if (Yes-OR-No-P-DEFAULT-NO
       "Do you really want to quit LISP? (RETURN for NO, yes/YES for YES): " 
       :default nil)
      (system::quit)))


(defvar *show-csh-result* nil)
(defvar *announce-shell-exec* nil "Announce execution of shell command by SHELL-EXEC.")
(defvar *wait-for-run-program* nil)

(defun shell-exec (command &optional (show-result *show-csh-result*))
  "Execute the COMMAND string in the shell, announcing the process when *ANNOUNCE-SHELL-EXEC* is T,
and announcing the result if SHOW-RESULT is T [default *SHOW-CSH-RESULT*]."
  (when *announce-shell-exec* (format t "~%;;; Shell command: ~s~%"command))
  (let* ((output-stream (make-string-output-stream))
	 (process
	  ;; (ext:run-program "csh" (list "-fc" (format nil "'~a'" command)) :output output-stream)
	  (ext:run-program "csh" (list "-fc" command) :output output-stream :wait *wait-for-run-program*)))
    (when show-result
      (format t "~A~%" (get-output-stream-string output-stream)))
    (ext::process-close process)
    nil))


(defun software-version ()
  "Returns a string describing version of the supporting software."
  (string-trim '(#\newline)
	       (with-output-to-string (stream)
		 (ext::run-program "/usr/bin/uname" '("-sr") :output stream))))

(defun sim-warning (message &rest args)
  "A warning has occured, print the message and continue."
  (inter::beep)  (inter::beep)  (inter::beep)
  (warn message)
;  (apply #'cerror (cons (string "continue") (cons (string message) args)))
  )

#|
Newsgroups: comp.lang.lisp
From: hall@aplcenmp.apl.jhu.edu (Marty Hall)
Subject: Re: Help
Organization: JHU/APL AI Lab, Hopkins P/T CS Faculty
Date: Mon, 2 Oct 1995 13:29:40 GMT

In article <KANDERSO.95Sep29123338@lager.bbn.com>
kanderso@lager.bbn.com (Ken Anderson) writes:
>Becareful when doing such experiments since results can be misleading.

Ken has an award-winning (*) paper on Lisp benchmarking/profiling that
I would recommend, except that the FTP site where I got it
(wheaton.bbn.com) is no longer accessible.

(*) Cleverest-title Award: "Courage in Profiles" :-)

>Generally both your test DEPTH2  function  and the function that calls TIME
>should be compiled.  The test  function should iterated enough time to make
>the times reported meaningful.

On the other hand, this is a nontrivial matter also. If the function
you are timing is small, a LOOP can skew your results, since the LOOP
overhead is included in your timing. Furthermore, if you compile a
LOOP, the compiler is free to take out things that don't affect the
result. For instance, (loop repeat 10000 do (* Var1 (+ Var2 (sqrt Var3))))
could be taken out altogether at compile time and replaced by NIL.

What I typically use is a macro that expands into a PROGN repeating
the function call N times. Here it is:

|#
;;; Part of http://www.apl.jhu.edu/~hall/lisp/Simple-Metering.lisp

;;;===========================================================================
;;; Time-Form: Takes a single form and optionally an integer N (default 20). It
;;; =========  runs the form N times and prints out the average time. Useful
;;;            for timing very small, fast functions when the time-step of
;;;            the builtin TIME function is too coarse.
;;;            > (Time-Form (Foo))     ; Call (Foo) 20 times + print avg time
;;;            > (Time-Form (Bar) 100) ; Call (Bar) 100 times + print avg time
;;; 1994 Marty Hall.

(defmacro Time-Form (Form &optional (Repetitions 20))
  "Runs FORM N times, printing avg execution time and returning FORM's value"
  (declare (optimize speed (safety 1) (compilation-speed 0)))
  `(let* ((Start (get-internal-run-time))
	  (Value (progn ,@(loop for I from 1 to Repetitions collecting Form)))
	  (Stop (get-internal-run-time)))
    (format t "~%Time to do ~S is ~0,5F sec."
     ',Form
     (float (/ (- Stop Start)
	       (* ,Repetitions internal-time-units-per-second))))
    Value))

#|
Example: (pprint (macroexpand-1 '(Time-Form (Foo 1 2 3) 10)))
(LET* ((START (GET-INTERNAL-RUN-TIME))
       (VALUE
        (PROGN
          (FOO 1 2 3)
          (FOO 1 2 3)
          (FOO 1 2 3)
          (FOO 1 2 3)
          (FOO 1 2 3)
          (FOO 1 2 3)
          (FOO 1 2 3)
          (FOO 1 2 3)
          (FOO 1 2 3)
          (FOO 1 2 3)))
       (STOP (GET-INTERNAL-RUN-TIME)))
  (FORMAT T "~%Time to do ~S is ~0,5F sec." '(FOO 1 2 3)
          (FLOAT
           (/ (- STOP START) (* 10 INTERNAL-TIME-UNITS-PER-SECOND))))
  VALUE) 

Note that if you really care, you could subtract off the time of one
call to get-internal-run-time. But since this only happens once, not
for each N, I usually ignore it.
						- Marty
(proclaim '(inline skates))

|#

(export '(s-value-list
	  string-case
	  LIST-POSITION
	  LIST-POSITION-number
	  LIST-POSITION-single-float
	  fixnum-min
	  fixnum-max
	
	  coerce-to-single
	  coerce-to-double
	  s-flt
	  d-flt
	  s-flt-gen
	  d-flt-gen
	  sim-warning
	  VARSTRING
	   setq-string
	  defvar-string
	  defvars defvars-w-value make-var create-output-symbol
	  cond-every


	  yes-or-no-p-default-no Y-OR-N-P-DEFAULT-NO quit
	  *announce-shell-exec*
	  *show-csh-result*
	  shell-exec


	  	  index
	   Time-Form
	  *wait-for-run-program*
	  software-version
	  ))