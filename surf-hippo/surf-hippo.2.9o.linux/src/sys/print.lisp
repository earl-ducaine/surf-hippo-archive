;;; -*- mode: lisp; Syntax: Common-lisp; package: surf ; base: 10;  -*-
;;; Lyle Borg-Graham, Equipe Cognisciences, Institut Alfred Fessard, CNRS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              The Surf-Hippo Neuron Simulator                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Surf-Hippo project at the  ;;;
;;; Center for Biological Information Processing, Department of     ;;;
;;; Brain and Cognitive Sciences, Massachusetts Institute of        ;;;
;;; Technology, and currently at the Equipe Cognisciences, Institut ;;;
;;; Alfred Fessard, CNRS and has been placed in the public domain.  ;;;
;;; If you are using this code or any part of Surf-Hippo, please    ;;;
;;; contact surf-hippo@ai.mit.edu to be put on the mailing list.    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; SYS Source file: print.lisp
(in-package "SURF-HIPPO")


;;					
;; General output executive, printing the circuit, making the output lists and output files.
;;

(defun get-surf-filename (pathname-directory extension &optional (trailer ""))
  "Generate a full pathname of the form:

           PATHNAME-DIRECTORY/filename.EXTENSION

where \"filename\" is either the current value of *SIMULATION-NAME*, when
*ADD-SIMULATION-TO-FILENAMES* is T, otherwise the current value of *TIME-STAMP*, in either case
postpended with TRAILER. Any repeated \"/\" will be replaced with a single \"/\"."
  (replace-repeated-character-w-single
   (concatenate-strings (string pathname-directory) "/"
			(if *ADD-SIMULATION-TO-FILENAMES* *simulation-name* (format nil "~a" *time-stamp*))		    
			(string trailer) "." (string extension))
   "/"))


;; The function PRINT-ANALYSIS is now defined in analysis.lisp

;;; SIMULATION-OUTPUT This is the overall function for preparing output lists, printing simulation information,
;;;  writing data files, and plotting results, if appropriate. It is called from SIM.
(defun simulation-output ()
  (analysis-output)			; Analyzes, if required.
  (after-simulation-print-circuit)

  (surf-plotter)			; Collects the plotting data and plots.

  (dump-simulation-files)		; Writes files, if required
  
  (unless *kill-extra-messages* (display-message (format nil "Done.~%"))))


(defun analysis-output ()
  (when *print-out-to-lisp* (print-simulation-stats))
  (when *print-out-to-info-window*
    (let (*DUMP-ANALYSIS-TO-FILE*)
      (OUTPUT-TEXT-tO-INFO-WIN 'print-simulation-stats))))

(defun print-numerical-details ()
  "Print out a number of numerical details relevant to the last or current simulation."
  (format t "Numerical Details:~%")
  (cond (*use-time-list* (format t "Shadowed time steps~%"))
	(*use-fixed-step* (format t " - User fixed step = ~A~%" *user-step*))
	(t				; LTE variable step
	 (format t " LTE/variable step: *USER-MAX-STEP* = ~A, *USER-MIN-STEP* = ~A~%"
		 *user-max-step* *user-min-step*)
	 
	 (if (not *LTE-NODE-CRITERIUM*)
	     (format t "*LTE-NODE-CRITERIUM* is NIL~%")
	     (let* ((lte-node-criterium (coerce-to-list *lte-node-criterium*))
		    (explicit-elements (coerce-to-list (element-node lte-node-criterium)))
		    (keywords (no-nils
			       (loop for keyword in '(:all :synapses :channels :axons :vsources
						      :isources :somas)
				     collect (find keyword lte-node-criterium)))))
	       (format t " *LTE-NODE-CRITERIUM* includes ")
	       (when keywords (format t "the keyword~p ~s" (length keywords) keywords))
	       (when (and keywords explicit-elements) (format t " and "))
	       (when explicit-elements (format t "~A explicit element~:p" (length explicit-elements)))
	       (format t "~%")))
	 (format t " *ABSOLUTE-VOLTAGE-ERROR* = ~A, *PICK-TIME-STEP-FUDGE* = ~A~%"
		 *absolute-voltage-error* *pick-time-step-fudge*)
	 (cond-every
	  (*calculate-particle-error*
	   (format t " *ABSOLUTE-PARTICLE-ERROR* = ~A~%" *absolute-particle-error*))
	  (*lte-was-punted*
	   (format t " The LTE estimater was ignored ~A ~a time~:p~%" 
		   (if (= *user-min-step* 0)
		       "(integer min step reached)"
		       "(*USER-MIN-STEP* reached)")
		   *lte-was-punted*))
	  (*count-error-step-change*
	   (if *full-error-step-change*
	       (format t " Voltage lte estimate set iteration/step ~A/~a time~:p~%"
		       (length (intersection *sim-reverse-time-list* *VOLTAGE-ERROR-STEP-CHANGES*))
		       (length *VOLTAGE-ERROR-STEP-CHANGES*)
		       )
	       (format t " Voltage lte estimate set step ~a time~:p~%"
		   
		       (length *VOLTAGE-ERROR-STEP-CHANGES*)
		       ))
	   (when *calculate-particle-error*
	     (if *full-error-step-change*
		 (format t " Particle lte estimate set iteration/step ~A/~a time~:p~A~%"
			 (length (intersection *sim-reverse-time-list* *particle-ERROR-STEP-CHANGES*))
			 (length *particle-ERROR-STEP-CHANGES*)
			 (if (> (length *PARTICLE-ERROR-STEP-LESS-THAN-MIN-STEP*) 0)
			     (format nil " (Particle LTE step < *MIN-STEP* ~a time~:p)"
				     (length *PARTICLE-ERROR-STEP-LESS-THAN-MIN-STEP*)) ""))
		 (format t " Particle lte estimate set iteration/step ~a time~:p~A~%"
			 (length *particle-ERROR-STEP-CHANGES*)
			 (if (> (length *PARTICLE-ERROR-STEP-LESS-THAN-MIN-STEP*) 0)
			     (format nil " (Particle LTE step < *MIN-STEP* ~a time~:p)"
				     (length *PARTICLE-ERROR-STEP-LESS-THAN-MIN-STEP*)) ""))))
	   (when *calculate-conc-int-error*
	     (format t " Conc integrator lte estimate set iteration/step ~A/~a time~:p~%"
		     (length (intersection *sim-reverse-time-list* *conc-int-ERROR-STEP-CHANGES*))
		     (length *conc-int-ERROR-STEP-CHANGES*)))))))
  (format t " Particle lookup precision: ~amV [interpolation ~A]~%"
	  *particle-look-up-table-precision*
	  (if *interpolate-particle-arrays* "ON" "OFF")))




  
(defun print-simulation-stats (&optional complete)
  "Print out the progress of the last or current simulation and a number of numerical details."
  (when *circuit-loaded* ; *simulation-finished*
    (when (or complete (not *kill-extra-messages*))
      (format t "~d ms simulation~A (~D/~D time points/iterations)~%"
	      (round *user-stop-time*)
	      (if (not (>= *real-time* *user-stop-time*)) (format nil " [Time reached: ~,1f ms]" *real-time*) "")
	      *total-num-time-points* *total-num-iterations*)
      (when (or complete *print-numerical-details*) (print-numerical-details))
      (format t "~&")
      (when (or complete *print-analysis*) (print-analysis)))))

(defun print-cell-element-elements (element)
  (concatenate-string-list 
   (no-nils  (loop for name in  *instance-model-names*
		   collect (let* ((elt-sym (read-from-string name))
				  (num-elts (length (get-node-elements-of-type
						     element elt-sym))))
			     (when (and (not (or (eq elt-sym 'segment)(eq elt-sym 'soma)))
					(> num-elts 0))
			       (format nil "~d ~a~p" num-elts name num-elts)))))
   :string-count-to-add-linefeed 7 :string-spacer ", "))



(defun dump-simulation-files (&optional force-data-out force-info-out description)
  (when (and *simulation-finished* (or *save-simulation-data-to-file* force-data-out))
    (write-element-data
     (unless *automatic-run* (when (go-ahead-menu "Prompt for each data list to save") :menu))))
  (when (or *save-simulation-info* force-info-out) (dump-info-file description)))

#|
;; DUMP-DATA-FILE Writes out plot data to file, according to *FILE-OUTPUT-VARIABLE-LIST*.
(defun dump-data-file (&optional select-each-element selected-element-names-and-slots)
  (let (*print-pretty*)
    (when *simulation-finished*
      (let* ((pathname-directory (get-surf-data-directory))
	     (data-filename (get-surf-filename pathname-directory "dat"))
	     (time-symbol (create-output-symbol *simulation-name* t 'time))
	     (approved-file-output-variable-list
	      (if (or selected-element-names-and-slots select-each-element)
		  (let ((*automatic-run* (or *automatic-run* (not select-each-element))))
		    (choose-list-values-from-keys	
		     (loop for var-info-list in *FILE-OUTPUT-VARIABLE-LIST*
			   collecting (list (format nil "~a" (nth 0 var-info-list)) var-info-list))
		     nil
		     :punt-if-only-one-entry nil
		     :selected-keys
		     (let (var-symb)
		       (loop for thing in *FILE-OUTPUT-VARIABLE-LIST*
			     when (setq var-symb
					(loop for name-and-slot in selected-element-names-and-slots
					      when  (and (string= (car name-and-slot) (element-name (nth 1 thing)))
							 (eq (cadr name-and-slot) (nth 2 thing)))
					      do (return (nth 0 thing))))
			     collect (format nil "~a" var-symb)))
		     :label "Choose Plotted Data To Dump To File"))
		  *file-output-variable-list*)))
	(when approved-file-output-variable-list
	  (unix:unix-mkdir (ext:unix-namestring pathname-directory nil) #o777)
	  (when (probe-file (ext:unix-namestring pathname-directory nil))
	    (with-open-stream (stream (open data-filename :direction :output :if-exists :supersede))
	      (format stream ";;; -*- Package: SURF; Mode: LISP -*-~%~%")
	      ;; VAR-INFO-LIST has the following format: (var-symb circuit-element data-slot)
	      (loop for var-info-list in approved-file-output-variable-list do
		    (write-lisp-list-to-stream (nth 0 var-info-list)
					       (element-data (nth 1 var-info-list) (nth 2 var-info-list))
					       stream))
	      ;; Now write out the time list.
	      (write-lisp-list-to-stream time-symbol (CURRENT-SIM-PLOT-TIME-LIST) stream)
	      ;; Now write out an archive variable name function.
	      (write-push-lisp-list-to-stream
	       '*archive-variable-list*
	       (list  *simulation-name*
		      time-symbol
		      (loop for var-info-list in approved-file-output-variable-list
			    collect (list (car var-info-list) (nth 2 var-info-list))))
	       stream)
	      (format t "File ~a written~%" data-filename))
	    (setq *last-simulation-file-path* *simulation-name*)))))))
|#

;; (make-nice-filename (or filename))



(defun parse-*FILE-OUTPUT-VARIABLE-LIST*-for-menu-entries (element-names-and-slots)
  (let (var-symb)
    (loop for thing in *FILE-OUTPUT-VARIABLE-LIST*
	  when (setq var-symb
		     (loop for name-and-slot in element-names-and-slots
			   when  (and (same-element-names (car name-and-slot) (element-name (nth 1 thing)))
				      (eq (cadr name-and-slot) (nth 2 thing))) ; Check the data type
			   do (return (nth 0 thing))))
	  collect (format nil "~a" var-symb))))


(defun approved-file-output-variable-list-menu (select-each-element element-names-and-slots)
  (let ((*automatic-run* (or *automatic-run* (not select-each-element))))
    (choose-list-values-from-keys	
     (loop for var-info-list in *FILE-OUTPUT-VARIABLE-LIST*
	   collecting (list (format nil "~a" (nth 0 var-info-list)) var-info-list))
     nil
     :punt-if-only-one-entry nil
     :selected-keys (parse-*FILE-OUTPUT-VARIABLE-LIST*-for-menu-entries element-names-and-slots)
     :label "Choose Plotted Data To Dump To File")))

(defun write-element-lisp-data (data-filename approved-file-output-variable-list time-symbol suppress-comments)
  (write-lisp-header data-filename)
  (with-open-stream (stream (open data-filename :direction :output :if-exists :supersede))
    (unless suppress-comments
      (let ((*standard-output* stream))
	(format t "#|~%")
	(print-circuit :terse)
	(format t "|#~%")))
    ;; VAR-INFO-LIST has the following format: (var-symb circuit-element data-slot)
    (loop for var-info-list in approved-file-output-variable-list do
	  (write-lisp-list-to-stream (nth 0 var-info-list)
				     (element-data (nth 1 var-info-list) (nth 2 var-info-list))
				     stream))
    ;; Now write out the time list.
    (write-lisp-list-to-stream time-symbol (CURRENT-SIM-PLOT-TIME-LIST) stream)
    ;; Now write out an archive variable name function.
    (write-push-lisp-list-to-stream
     '*archive-variable-list*
     (list *simulation-name*
	   time-symbol
	   (loop for var-info-list in approved-file-output-variable-list
		 collect (list (car var-info-list) (nth 2 var-info-list))))
     stream)
    (format t "File ~a written~%" data-filename)))

(defun write-element-columns-data (pathname-directory approved-file-output-variable-list
						      units-labels
						      suppress-comments)
  (loop for var-info-list in approved-file-output-variable-list
	for units-label in units-labels
	do
	(let* ((data-symbol (nth 0 var-info-list))
	       (ELEMENT-name (nth 1 var-info-list))
	       (SLOT (nth 2 var-info-list))
	       (data-filename (format nil "~A~A.dat" pathname-directory (make-nice-filename data-symbol))))
	  (write-lists-multi-column
	   (list (CURRENT-SIM-PLOT-TIME-LIST)
		 (element-data element-name slot))
	   :announce-write t
	   :comment (unless suppress-comments
		      (format nil "~%;; ~A: ~A ~A time (milliseconds) and data (~A), x y format"
			      *simulation-name*
			      element-name slot
			      units-label))
	   :filename data-filename))))


(defun write-element-data (&optional elements-and-slots &key (output-format :lisp) filename suppress-comments)
  "Writes ELEMENT-DATA from all elements referenced in ELEMENTS-AND-SLOTS. ELEMENTS-AND-SLOTS may be
a list whose members are either element references or sublists of element references and data types.
If ELEMENTS-AND-SLOTS is :MENU, then a menu will be generated. If ELEMENTS-AND-SLOTS is NIL or not
supplied, then all data saved from last simulation will be written to file. Remaining args are as
for GRAB-AND-STORE-PLOT-DATA. For :LISP OUTPUT-FORMAT, variable symbols are created from the
simulation, element and data type name. These symbols are used when the file is loaded into Lisp.
For :COLUMNS OUTPUT-FORMATseparate files are written, whose names are constructed as the case for
the variable symbols in the :LISP OUTPUT-FORMAT case. In both cases the created variable symbols are
listed in the global variable *FILE-OUTPUT-VARIABLE-LIST*.  If FILENAME is not supplied then the full
pathname is obtained with the GET-SURF-FILENAME function (directory obtained with the
GET-SURF-DATA-DIRECTORY function), with file extension \"dat\"."
  (let* (*print-pretty*
	 (pathname-directory (get-surf-data-directory))
	 (use-menu (eq elements-and-slots :menu))
	 (data-filename (or filename (get-surf-filename pathname-directory "dat")))
	 (time-symbol (create-output-symbol *simulation-name* t 'time))
	 (element-names-and-slots
	  (unless use-menu
	    (loop for elt-slot in (coerce-to-list elements-and-slots)
		  collect (list (element-name (if (consp elt-slot) (car elt-slot) elt-slot))
				(or (when (consp elt-slot) (cdr elt-slot))
				    (default-data-type elt-slot))))))
	 (approved-file-output-variable-list
	  (if (or use-menu element-names-and-slots)
	      (approved-file-output-variable-list-menu use-menu element-names-and-slots)
	      *file-output-variable-list*))
	 (units-labels
	  (coerce-to-list
	   (element-data-units
	    (loop for var-info-list in approved-file-output-variable-list
		  collect (ELEMENT-name (nth 1 var-info-list))))))
	 (dummy3 output-format)
	 (dummy1 data-filename)
	 (dummy4 suppress-comments))
    (when approved-file-output-variable-list
      (when use-menu
	(choose-variable-values
	 `((dummy1 "Edit filename (for LISP format only)" :string)
	   (dummy3 "Output format" :choose (:lisp :columns))
	   (dummy4 "Suppress file comments" :boolean)
	   (dummy2 "CANCEL" :boolean))
	 :title (format nil "Write Data From ~A" *simulation-name*)))
      (unless dummy2
	(let ((suppress-comments dummy4)
	      (output-format dummy3))
	  (when approved-file-output-variable-list
	    (multiple-value-bind (data-filename pathname)
		(provide-pathname-and-filename dummy1 pathname-directory)
	      (when pathname
		(case output-format
		  (:lisp (write-element-lisp-data data-filename approved-file-output-variable-list
						  time-symbol suppress-comments))
		  (:columns
		   (write-element-columns-data pathname-directory approved-file-output-variable-list
					       units-labels
					       suppress-comments)))
		(setq *last-simulation-file-path* *simulation-name*)
		nil))))))))

#|
;; fix this
(defun dump-synapse-waveform-file ()
  (let ((*print-pretty* nil))
    (if *simulation-finished*
	(let* ((pathname (get-surf-data-directory))
	       (data-filename (get-surf-filename pathname "dat" "-syn-waves")))
	  (unix:unix-mkdir (ext:unix-namestring pathname nil) #o777)
	  (with-open-stream (stream (open data-filename :direction :output :if-exists :supersede))
	    (format stream ";;; -*- Package: SURF; Mode: LISP -*-~%~%")
	    (format stream "#|~%~%")
	    (print-circuit)
	    (after-simulation-print-circuit stream)
	    (format stream "|#~%~%")
	    ;; VAR-INFO-LIST has the following format: (var-symb circuit-element data-slot)
	    (write-lisp-list-to-stream '*synapse-waveforms *synapse-waveforms stream)) 
	  (format t "File ~a written~%" data-filename))
	(setq *last-simulation-file-path* *simulation-name*))))
|#


(defun dump-info-file (&optional description-level)
  (let* ((pathname-directory (get-surf-data-directory))
	 (info-filename (get-surf-filename pathname-directory "info"))
	 (new-file (not (probe-file info-filename)))
	 (*print-pretty* nil))
    (unix:unix-mkdir (ext:unix-namestring pathname-directory nil) #o777)
    (when (probe-file (ext:unix-namestring pathname-directory nil))
      (with-open-stream (*standard-output*
			 (open info-filename :direction :output :if-exists :append :if-does-not-exist :create))
	(when new-file (format t ";;; -*- Package: SURF; Mode: LISP -*-~%~%"))
	(format t "#|~%~%")
	(print-circuit description-level)
	(after-simulation-print-circuit)
	(format t "~%~%")
	(let (*DUMP-ANALYSIS-TO-FILE*) (print-simulation-stats))
	(format t "|#~%~%"))
      (format t "File ~a written~%" info-filename))
    (setq *last-simulation-file-path* *simulation-name*)
    nil))


(defun add-element-doc-to-info-file (elt)
  (let* ((pathname-directory (get-surf-data-directory))
	 (info-filename (get-surf-filename pathname-directory "info"))
	 (new-file (not (probe-file info-filename)))
	 (*print-pretty* nil))
    (unix:unix-mkdir (ext:unix-namestring pathname-directory nil) #o777)
    (when (probe-file (ext:unix-namestring pathname-directory nil))
      (with-open-stream (*standard-output*
			 (open info-filename :direction :output :if-exists :append :if-does-not-exist :create))
	(when new-file (format t ";;; -*- Package: SURF; Mode: LISP -*-~%~%"))
	(format t "#|~%~%")
	(print-element elt nil t)
	(format t "~%~%")
	(format t "|#~%~%"))
      (format t "File ~a written~%" info-filename)
      (setq *last-simulation-file-path* *simulation-name*)
      nil)))

(defun dump-analysis-file (results &optional filename)
  (let* ((pathname-directory (get-surf-data-directory))
	 (filename (if filename
		       (format nil "~a/~a" pathname-directory filename)
		       (get-surf-filename pathname-directory "results"))))
    (unix:unix-mkdir (ext:unix-namestring pathname-directory nil) #o777)
    (when (probe-file (ext:unix-namestring pathname-directory nil)) 
      (unless (probe-file filename)
	(with-open-stream (*standard-output* (open filename :direction :output :if-does-not-exist :create))
	  (format t ";;; -*- Package: SURF; Mode: LISP -*-~%~%")
	  (write-lisp-list-to-stream '*archive-session-results* nil t 10 0 t)
	  (format t "~%~%~%")))
      (with-open-stream (*standard-output* (open filename :direction :output :if-exists :append))
	(format t "(push ~% '(")
	(format-list (list *simulation-name* *simulation-results-addendum* results) 1 t)
	(format t ")~%          *archive-session-results*)~%~%"))
      (format t "File ~a written~%" filename))
    nil))

(defun write-comment-to-analysis-file (comment &optional filename)
  (let* ((pathname-directory (get-surf-data-directory))
	 (filename (if filename
		     (format nil "~a/~a" pathname-directory filename)
		     (get-surf-filename pathname-directory "results"))))
    (unix:unix-mkdir (ext:unix-namestring pathname-directory nil) #o777)
    (when (probe-file (ext:unix-namestring pathname-directory nil))
      (unless (probe-file filename)
	(with-open-stream (*standard-output* (open filename :direction :output :if-does-not-exist :create))
			  (format t ";;; -*- Package: SURF; Mode: LISP -*-~%~%")
			  (write-lisp-list-to-stream '*archive-session-results* nil t 10 0 t)
			  (format t "~%~%")))
      (with-open-stream (*standard-output* (open filename :direction :output :if-exists :append))
			(format t "#|~%~%~a~%~%|#~%~%" comment))
      (format t "File ~a written~%" filename))))



    
(defun dump-all-circuit-elements-file ()
  (let ((*automatic-run* t))
    (dump-elements-file (loop for symbol in (reverse *object-type-symbols*) nconc (copy-list (things-in-circuit symbol))))))


(defun dump-DOCUMENTED-USER-VARIABLES-file ()
  "Write a loadable file of all documented user variables"
  (let* ((pathname-directory (get-surf-data-directory))
	 (elements-filename (get-surf-filename pathname-directory "vars"))
	 (*print-pretty* nil))
    (unix:unix-mkdir (ext:unix-namestring pathname-directory nil) #o777)
    (when (probe-file (ext:unix-namestring pathname-directory nil))
      (with-open-stream (*standard-output*
			 (open elements-filename :direction :output :if-exists :append :if-does-not-exist :create))
	(format t ";;; -*- Package: SURF; Mode: LISP -*-~%~%")
	(format t "#|~%")
	(let (*enable-print-DOCUMENTED-USER-VARIABLES*) (print-circuit :terse))
	(after-simulation-print-circuit)
	(format t "|#~%")
	(format t "~%~%")
	(print-DOCUMENTED-USER-VARIABLES))
	
      (format t "File ~a written~%" elements-filename)
      (setq *last-simulation-file-path* *simulation-name*)
      nil)))
  
(defun dump-elements-file (&optional elements-or-select-each-element)
  "Write a loadable file with TYPE-DEF forms for selected (loaded) elements which are element types, and CREATE forms
for selected elements such as channels, synapses, or sources. Selected elements are determined by
the ELEMENT-OR-SELECT-EACH-ELEMENTS argument - this arg can be either a single element, a list of
elements, non-NIL (generating a selection menu, or NIL which will select all loaded elements."
  (let* ((pathname-directory (get-surf-data-directory))
	 (elements-filename (get-surf-filename pathname-directory "elts"))
	 (*print-pretty* nil)
	 (mod-elts
	  (cond
	    ((element (car (coerce-to-list elements-or-select-each-element)))
	     (loop for elt in (coerce-to-list elements-or-select-each-element)
		   when (element-model elt)
		   collect (list (element-model elt) elt)))
	    (elements-or-select-each-element
	     (loop for type in (CHOOSE-ASSOCIATED-ELEMENTS-TYPES-FOR-DOCUMENTATION) nconcing
		   (let ((mod (type-symbol-model type)))
		     (choose-list-values-from-keys
		      (loop for elt being the hash-value of (model-hash-table mod)
			    when (and (instance-in-cell elt) (model-document-routine mod) )
			    collect (list (format nil "~a ~a" (element-name elt)(model-name mod))
					  (list mod elt)))
		      nil :punt-if-only-one-entry nil :label "Choose Elements To Dump To File"))))
	    (t
	     (loop for mod being the hash-value of *model-hash-table* nconcing
		   (loop for elt being the hash-value of (model-hash-table mod)
			 when (model-document-routine mod) collect (list mod elt)))))))
    (when mod-elts
      (unix:unix-mkdir (ext:unix-namestring pathname-directory nil) #o777)
      (when (probe-file (ext:unix-namestring pathname-directory nil))
	(with-open-stream (*standard-output*
			   (open elements-filename :direction :output :if-exists :append :if-does-not-exist :create))
	  (format t ";;; -*- Package: SURF; Mode: LISP -*-~%~%")
	  (format t "#|~%")
	  (print-circuit :terse)
	  (after-simulation-print-circuit)
	  (format t "|#~%")
	  (loop for mod-elt in mod-elts
		when (model-document-routine (nth 0 mod-elt))
		do
		;(format t "~%")
		(funcall (model-document-routine (nth 0 mod-elt)) (nth 1 mod-elt))
		(format t "~%")))
	(format t "File ~a written~%" elements-filename)
	(setq *last-simulation-file-path* *simulation-name*))
      nil)))


;; Not tested
(defun dump-object-to-file (object &optional filename-comp)
  (let* ((pathname-directory (get-surf-data-directory))
	 (data-filename (get-surf-filename pathname-directory "dat" filename-comp)))
    (when (unix:unix-mkdir (ext:unix-namestring pathname-directory nil) #o777)
      (with-open-stream (stream (open data-filename :direction :output :if-exists :supersede))
	(write object :stream stream)
	))))

(defun write-lisp-list-to-stream (name list stream &optional (values-per-line 10) (indent 0) defvar-only)
  (dotimes (i indent) (format stream " "))
  (format stream "(defvar ~s nil)~%" name)	; avoid warnings.
  (dotimes (i indent) (format stream " "))
  (unless defvar-only
    (format stream "(setq ~s '(~%" name)
    (format-list list values-per-line stream nil indent)
    (dotimes (i indent) (format stream " "))
    (format stream "~%))~%"))
  (format stream "~%"))

(defun write-lisp-sym-and-value-to-stream (sym stream &optional (values-per-line 10) (indent 0))
  (let ((value (symbol-value sym)))
    (typecase value
      (cons (write-lisp-sym-list-to-stream sym (symbol-value sym) stream values-per-line indent))
      (atom (format stream "~%(setq ~s ~s)~%" sym value)))))
  
(defun write-lisp-sym-list-to-stream (name list stream &optional (values-per-line 10) (indent 0)
					   (include-defvar t))
  (dotimes (i indent)
    (format stream " "))
  (when include-defvar (format stream "(defvar ~s)~%" name))	; avoid warnings.
  (dotimes (i indent)
    (format stream " "))
  (format stream "(setq ~s ~%" name)
					;  (format-list list values-per-line stream)
  (formatted-list-dump list stream nil values-per-line (1+ indent))
  (format stream "~%") 
  (dotimes (i indent)
    (format stream " "))
  (format stream " )~%~%"))

(defun write-push-lisp-list-to-stream (name list stream &optional (values-per-line 5))
  (format stream "(if (not (member ~% '(")
  (format-list list values-per-line stream)
  (format stream ")~% ~s :test 'equal))~%~%" name)
  (format stream " (push ~% '(")
  (format-list list values-per-line stream)
  (format stream ")~%   ~s))~%~%" name))
  

;;; PRINT-LIGHT-stimulus
(defun print-light-stimulus ()
  (when *light-stimulus*
    (format t "Light stimulus is a ~a, strength ~a~A~%" *light-stimulus* *light-stimulus-strength*
	    (if (= *light-background* 0) "" (format nil " (background level ~A)" *light-background*)))
    (format t "  Start time: ~a ms~a,~A.~%"
	    *light-stimulus-start-time*
	    (if (not *fast-full-field-spot*)
		(format nil " at (X=~a um, Y=~a um)"
			*light-start-position-x* *light-start-position-y*) "")
	    (if (> *light-stimulus-stop-time* *user-stop-time*)
		" active for entire simulation" (format nil " Stop time: ~a ms" *light-stimulus-stop-time*))
	      
	    (if *constant-light-input-from-negative-infinity*
		" (Light input @ t=0 holds for all t<0)" ""))	    
    (case *light-stimulus*
      ((:on-bar :off-bar :bar)
       (format t "  Stimulus orientation (width axis) ~a degrees~%" (rad-to-deg *light-theta*)))
      ((:moving-bar :on-moving-bar :off-moving-bar :moving-sine-grating :moving-bar-grating)
       (format t "  Stimulus trajectory orientation (width axis) ~a degrees~%" (rad-to-deg *light-theta*)))
      ((:moving-spot :on-moving-spot :off-moving-spot)
       (format t "  Stimulus trajectory orientation ~a degrees~%" (rad-to-deg *light-theta*))))
    (case *light-stimulus*
      (:apparent-motion (print-apparent-motion-parameters))
      ((:moving-bar :on-moving-bar :off-moving-bar) (print-moving-bar-parameters))
      ((:on-bar :off-bar :bar) (print-bar-parameters))
      (:moving-bar-grating (print-moving-bar-grating-parameters))
      (:moving-sine-grating (print-moving-sine-grating-parameters))
      ((:moving-spot :on-moving-spot :off-moving-spot) (print-moving-spot-parameters))
      (:annulus (print-spot-and-annulus-parameters))
      ((:on-spot :off-spot :spot) (print-spot-and-annulus-parameters)))))



(defun print-apparent-motion-parameters ()
  (format t "  Bar A intensity = ~a, Bar B intensity = ~a~%"  *bar-a-intensity* *bar-b-intensity*))


(defun print-bar-parameters ()
  (format t "  Bar width ~a um, length ~a um.~%" *bar-width* *bar-length*))


(defun print-moving-bar-parameters ()
  (print-bar-parameters)
  (format t "  Speed ~a um/ms," *light-speed*)
  (if *light-direction*
      (format t " in direction of orientation.~%")
      (format t " opposite direction of orientation.~%"))
  (if nil ; *fast-rf-bar
      (progn
	(format
	 t "  Fast RF Moving Bar (TM) convolution used - reference synapse(s) or node(s):~%   ")
	(loop for name in *SYNAPSE-NAMES-TO-DO-FIRST* do
	      (format t "~A " name)))))


(defun print-moving-bar-grating-parameters ()
  (format t "~&  Bar width ~a um, length ~a um.~%" *bar-width* *bar-length*)
  (format t "~&  Grating speed ~a um/ms," *light-speed*)
  (light-direction-end-string)
  (format t "~&  Grating spatial period = ~a~%" *grating-spatial-period*))

(defun light-direction-end-string () 
  (if *light-direction*
      (format t " in direction of orientation.~%")
      (format t " opposite direction of orientation.~%")))

(defun print-moving-sine-grating-parameters ()
  (format t "  Grating speed [microns per millisecond]= ~a, " *light-speed*)
  (light-direction-end-string)
  (format t "  Grating spatial period = ~a ~%" *grating-spatial-period*))

(defun print-moving-spot-parameters ()
  (format t "  Spot speed ~a um/ms, " *light-speed*)
  (light-direction-end-string))

#|
(defun parse-*SYNAPSE-NAMES-TO-DO-FIRST* (&optional (*SYNAPSE-NAMES-TO-DO-FIRST* *SYNAPSE-NAMES-TO-DO-FIRST*))
  (flatten-list
   (loop for name in *SYNAPSE-NAMES-TO-DO-FIRST* collect
	 (or (element-name name 'synapse)
	     (element-name (node-elements-of-type name 'synapse))))))
|#


(defun print-spot-and-annulus-parameters ()
  (if *fast-full-field-spot*
      (progn
;        (format t "   Fast RF Full Field Spot (TM) convolution used - reference synapse(s):~%")
;        (loop for name in (parse-*SYNAPSE-NAMES-TO-DO-FIRST*) do (format t "~A " name))
	(format t "    Full Field Spot~%"))
      (progn
	(format t "  Spot outside diameter ~a um, " *spot-outside-diameter*)
	(format t "inside diameter ~a um~%" *spot-inside-diameter*))))

(defun format-hash-table-count (table name &optional period-not-comma (correction 0))
  (let ((number-things-in-circuit
	 (+ correction
	    (loop for entry being the hash-value of table when (element-in-circuit entry) sum 1))))
    (when (> number-things-in-circuit 0) 
      (format t (concatenate-strings "~D " name "~:P" (if period-not-comma ".~%" ", "))
	      number-things-in-circuit))))

(defun format-type-symbol-count (type-symbol &optional period-not-comma)
  (let ((name (type-symbol-string type-symbol))
	(number-things-in-circuit (num-type-instances-in-circuit (element-type type-symbol))))
    (when (> number-things-in-circuit 0) 
      (format t (concatenate-strings "~D " name "~:P" (if period-not-comma ".~%" ", "))
	      number-things-in-circuit))))


;; There are some things which should be printed out about the circuit only after the simulation.
(defun after-simulation-print-circuit ()
  (when (> (hash-table-count (SYNAPSE-HASH-TABLE)) 0)
					; (format *t "~%")
					; (count-active-synapses *t)
					; (PRINT-TOTAL-CONNECTIVITY)
    ))



(defun print-connectivity (source-cell-type destination-cell-type synapse-type)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let* ((pre-syn-flag nil)
	 (source-cell-type (element source-cell-type 'cell-type)	 )
	 (destination-cell-type (element destination-cell-type 'cell-type))
	 ;;	 (source-cells (cell-type-cells source-cell-type))
	 ;;	 (destination-cells (cell-type-cells destination-cell-type))
	 (total-gbar 0.0d0))
    (declare (double-float total-gbar))
    (loop for syn in (synapses-of-type synapse-type)
	  when (cell-element-p (synapse-pre-synaptic-element syn))
	  do (setq pre-syn-flag t)
	  when (let ((synapse-cell (element-cell (synapse-cell-element syn))))
		 (and (eq (cell-type synapse-cell) destination-cell-type)
		      ;; (member synapse-cell destination-cells)
		      (or (and (not (CELL-ELEMENT-P (synapse-pre-synaptic-element syn)))
			       (equal source-cell-type destination-cell-type))
			  (and (CELL-ELEMENT-P (synapse-pre-synaptic-element syn))
			       (eq (cell-type (element-cell (synapse-pre-synaptic-element syn))) source-cell-type))
			  ;;			  (member (element-cell (synapse-pre-synaptic-element syn))  source-cells)
			  )))
	  do (setq total-gbar (the double-float (+ total-gbar (the double-float (synapse-gbar syn))))))
    (unless (= 0.0 total-gbar)
      (if pre-syn-flag
	(format t "Total gbar of synapse type ~a (cell type ~A -> cell type ~a): ~,2euS~%"
		(synapse-type-name synapse-type) (cell-type-name source-cell-type)
		(cell-type-name destination-cell-type)
		total-gbar)
	(format t "Total gbar of synapse type ~a (-> cell type ~a): ~,2euS~%"
		(synapse-type-name synapse-type) 
		(cell-type-name destination-cell-type)
		total-gbar)))))

(defun print-total-connectivity ()
  (let ((cell-types (cell-types)))
    (loop for destination-cell-type in cell-types do
	  (loop for source-cell-type in cell-types do
		(loop for synapse-type in (synapse-types)
		      do (print-connectivity source-cell-type destination-cell-type synapse-type))))))

(defun print-spaced-simulation-header ()
  (print-simulation-header)
  (format t "~%~%~%~%"))

(defun print-simulation-header ()
  (format t "~%***********************************************~% ")
  (decode-time-stamp)
  (when (> (length *username*) 0) (format t "   User: ~A" *username*))
  (format t "~%***********************************************~% ")
  (when *circuit-loaded*
    (if (> (length *simulation-name*) 0)
      (format t "~%Simulation '~a' " *simulation-name*)
      (when (> (length *circuit*) 0) (format t "~%Circuit '~a' " *circuit*)))))

(defun print-DOCUMENTED-USER-VARIABLES ()
  (when *enable-print-DOCUMENTED-USER-VARIABLES*
    (let (printed-setq)
      (loop for var in (delete-duplicates
			(concatenate 'list
				     (when *DOCUMENT-all-new-VARIABLES* (get-new-surf-variable-symbols)) 
				     (when (consp *DOCUMENTED-USER-VARIABLES*) *DOCUMENTED-USER-VARIABLES*)))
	    when (boundp var) do
	    (unless printed-setq
	      (format t "(setq~%")
	      (setq printed-setq t))
	  
	    (format t " ~A ~A~%"
		    (symbol-name var)
		    (typecase (symbol-value var)
		      (symbol (format nil "`~A" (symbol-value var)))
		      (string (format nil "~s"  (symbol-value var)))
		      (t (symbol-value var))))
	    finally (when printed-setq (format t " )~%"))))))

(defun print-circuit (&optional (description-level *SIMULATION-PRINT-DETAIL*))
  "Print out circuit details. The level of detail is given by the optional DESCRIPTION-LEVEL [default
given by global variable *SIMULATION-PRINT-DETAIL* - see this for more explanation]."
  ; (unless *simulation-finished* (UPDATE-SIMULATION-TIME-STAMP-AND-NAME))
  (let ((description-level
	 (cond (description-level description-level)
	       (*print-full-to-lisp* (if *include-segments-w-full-description* :FULL_With_SEGMENTS :full))
	       (*print-mini-to-lisp* :terse)
	       (t (or *simulation-print-detail* :none)))))
    (case description-level
      (:specific_elements (information-for-elements-menu))
      (t
       (print-simulation-header)
       (unless (or (not *circuit-loaded*)
		   *simulation-finished*
					; *simulation-in-progress*  
		   )
	 (format t "** Loaded circuit not simulated yet **~%"))
       (if (= (length *circuit*) 0)
	 (format t "~%** No circuit loaded **~%")
	 (unless (or (eq description-level :none) *kill-extra-messages* (= (hash-table-count (CELL-HASH-TABLE)) 0))
	   (when (> (length *loaded-circuit-parts*) 1)
	     (format t
		     (let ((string "[Composed from:"))
		       (if (> (length *loaded-circuit-parts*) 8)
			 (setq string (format nil "~A ~A ... ~A" string
					      (first *loaded-circuit-parts*)
					      (car (last *loaded-circuit-parts*))))
			 (loop for name in *loaded-circuit-parts* do (setq string (format nil "~A ~A" string name))))
		       (concatenate-strings string "]~%"))))
	   (cond
	    (*multiple-source-circuit*
	     (format t "Multiple sources defined this circuit.~%"))
	    (*input-is-function* (format t "[Compiled function: ~a]~%" *circuit*))
	    ((> (length *circuit-file*) 0)
	     (format t "[File: ~a]~%" (concatenate-strings *circuit-directory* *circuit-file*))))
	   (format t (concatenate-strings *comment-string* "~%"))

	   (when (and *neuron-tree-consolidated* (not (eq description-level :terse)))
	     (format t "Trees consolidated with a max electrotonic length of ~A~%" *maximum-electrotonic-length*))
      
	   (format-hash-table-count (CELL-TYPE-HASH-TABLE) "cell type")
	   (when *circuit-loaded*
	     (format-hash-table-count (CELL-HASH-TABLE) "cell")
	     (format t "~a nodes. " (1- (hash-table-count (NODE-HASH-TABLE))))
	     (if *ignore-q10*
	       (format t "Temperature dependence ignored.")
	       (format t "Temperature ~,2f degrees(C)." (- *Temperature* 273.16)))
	     (format t " ~a ms simulation.~%" *user-stop-time*))
	   (if *circuit-loaded*
	     (format t "There ~A " (if (> (length (somas)) 1) "are" "is"))
	     (format t "Loaded elements "))
	   (when *circuit-loaded*
	     (format-hash-table-count (SOMA-HASH-TABLE) "soma")
	     (format-hash-table-count (AXON-HASH-TABLE) "axon")
	     (format-hash-table-count (VSOURCE-HASH-TABLE) "voltage source")
	     (format-hash-table-count (ISOURCE-HASH-TABLE) "current source"))
	   (format-hash-table-count (CHANNEL-TYPE-HASH-TABLE) "channel type")
	   (when *circuit-loaded*
	     (format-hash-table-count (CHANNEL-HASH-TABLE) "channel")
	     (when *channel* (format t "~%")))
	   (format-hash-table-count (PARTICLE-TYPE-HASH-TABLE) "voltage-dep particle type")
	   (when *circuit-loaded*
	     (format-hash-table-count (PARTICLE-HASH-TABLE) "voltage-dep particle")
	     (when *particle* (format t "~%")))
	   (format-hash-table-count (CONC-PARTICLE-TYPE-HASH-TABLE) "concentration-dep particle type")
	   (when *circuit-loaded*
	     (format-hash-table-count (CONC-PARTICLE-HASH-TABLE) "concentration-dep particle")
	     (when *conc-particle* (format t "~%")))
	   (format-hash-table-count (CONC-INT-TYPE-HASH-TABLE) "concentration integrator type")
	   (when *circuit-loaded*
	     (format-hash-table-count (CONC-INT-HASH-TABLE) "concentration integrator")
	     (when *conc-int* (format t "~%")))
	   (format-hash-table-count (PUMP-TYPE-HASH-TABLE) "pump type")
	   (when *circuit-loaded* (format-hash-table-count (PUMP-HASH-TABLE) "pump"))
	   (format-hash-table-count (BUFFER-TYPE-HASH-TABLE) "buffer type")
	   (when *circuit-loaded*
	     (format-hash-table-count (BUFFER-HASH-TABLE) "buffer")
	     (when (or *pump* *buffer*) (format t "~%")))
	   (format-hash-table-count (SYNAPSE-TYPE-HASH-TABLE) "synapse type")
	   (when *circuit-loaded*
	     (format-hash-table-count (SYNAPSE-HASH-TABLE) "synapse")
	     (when *synapse* (format t "~%")))
	   (format-hash-table-count (ELECTRODE-HASH-TABLE) "electrode")
	   (format t "and ")
	   (if *segment*
	     (format-hash-table-count (SEGMENT-HASH-TABLE) "segment" t (- (hash-table-count (ELECTRODE-HASH-TABLE))))
	     (format t "no segments."))
	   (unless (eq description-level :terse)
	     (let ((num-soma-segs (loop for cell in (cells) sum (length (soma-segments cell)))))
	       (when (> num-soma-segs 0)
		 (format t "(~A segment~:p assigned to soma~p.)~%" num-soma-segs (length (somas)))))
	   
	     ;; (let ((num-elect (hash-table-count (ELECTRODE-HASH-TABLE))))
	     ;;   (when (> num-elect 0) (format t "(~A segment~:p are electrode models.)~%" num-elect)))

	     )
	   (when (and (channels) (not *active*)) (format t " ** All channels blocked **~%"))
	   (when (and (synapses) (not *enable-synapses*)) (format t " ** All synapses blocked **~%"))
	   (format t "~%")
	   (print-circuit-elements description-level)
	   (when (and *enable-light* (are-there-light-synapses)) (PRINT-LIGHT-stimulus))
	   (when (and (synapses) (or (eq description-level :medium) (eq description-level :terse)))
	     (count-active-synapses)
	     (PRINT-TOTAL-CONNECTIVITY))
	   (unless (or (eq description-level :terse)
		       (eq description-level :none))
	     (print-DOCUMENTED-USER-VARIABLES))
	   (when *include-simulation-annotation* (format t "~A~%" *simulation-annotation*))
	   (format t "~%")))))
    nil))
  

(defun print-circuit-elements (description-level)
  (loop for name
	in (flatten-list
	    (list "cell-type"
		  "isource" "vsource"
		  (when *circuit-loaded*
		    (case description-level
		      ((:medium :FULL_With_SEGMENTS :full)
		       (list "cell"
			     "synapse-type" "channel-type" "particle-type" "conc-particle-type"
			     "conc-int-type" "pump-type" "buffer-type"
			     "axon-type"
		  
			     (case description-level
			       ((:FULL_With_SEGMENTS :full)
				(list "synapse" "channel" "particle" "conc-particle"
				      "conc-int" "pump" "buffer"
				      "soma" "axon"
				      (case description-level
					(:FULL_With_SEGMENTS "segment")))))))))))
	do
	(let ((mod (gethash name *model-hash-table*)))
	  (when (and mod (> (hash-table-count (model-hash-table mod)) 0))
	    (let ((print-routine (if (or (eq description-level :medium) (eq description-level :terse))
				     (model-short-print-routine mod)
				     (model-print-routine mod)))
		  element-printed)
	      (when print-routine
		(loop for elt being the hash-value of (model-hash-table mod) do ; this is really obsolete??
		      (when (instance-in-cell elt)
			(setq element-printed t)
			(format t "  ")
			(funcall print-routine elt)))
		(when element-printed (format t  "~%"))))))))

(defun print-model-parameters (model-name)
  (loop for mod being the hash-value of *model-hash-table* 
	when (and (model-print-routine mod) (equal model-name (model-name mod)))
	do (return
	     (loop for elt being the hash-value of (model-hash-table mod) do (funcall (model-print-routine mod) elt)))))







