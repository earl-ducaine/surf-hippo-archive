;;; -*- mode: lisp; Syntax: Common-lisp; package: surf ; base: 10 ; -*-
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


;;; SYS Source file: circuit-input.lisp

(in-package "SURF-HIPPO")

;; Menu for setting *ONLY-LOAD-PASSIVE*, *CIRCUIT-SOURCE*, *CIRCUIT-FILE-TYPE*, and
;; *INITIALIZE-BEFORE-NEXT-CIRCUIT*. Returns T unless CANCEL is selected.
(defun circuit-source-menu ()
  (let (dummy1 (dummy2 (if *initialize-before-next-circuit* :initialize :add)))
    (choose-variable-values
     `((*circuit-source* "Get circuit definition from:" :choose (:Catalog_Function :Function :File) :vertical)
       ; (*circuit-file-type* "File type of circuit file:" :choose (:neurolucida :lisp))
       (*only-load-passive* "Load passive model only" :boolean)
       ,(when *circuit-loaded*
	  `(dummy2 ,(format nil "Initialize simulator before loading circuit~%or add new circuit to existing circuit:")
		   :choose (:initialize :add)))
       (dummy1 "Cancel" :boolean))
     :label "What Kind of Circuit Definition")
    (unless dummy1 (setq *initialize-before-next-circuit* (case dummy2 (:initialize t) (t nil))))
    (not dummy1)))

;; Returns T unless CANCEL.
(defun circuit-function-menu ()
  (let (dummy1 (dummy2 (if (stringp *circuit-function*) *circuit-function* "")))
    (choose-variable-values
     '((dummy2 "Circuit function" :string)
       (dummy1 "Cancel" :boolean))
     :label "Compiled Circuit Specification")
    (unless dummy1
      (setq *circuit-function* dummy2
	    *circuit-parts* (list dummy2)))
    (not dummy1)))

;; Returns NIL if nothing selected.
(defun catalog-function-menu ()
  (setq *CIRCUIT-FUNCTIONS* (delete-duplicates *CIRCUIT-FUNCTIONS*)) ; Clean it up.
  (setq *circuit-parts*
	(choose-list-values-from-keys
	 (key-values-from-symbol-list *CIRCUIT-FUNCTIONS*)
	 nil :direction :vertical :do-all-at-once t :text "Choose one or more functions" :label "Compiled Circuit Catalog")))

(defun load-circuit-menu ()
  (setq *initialize-before-next-circuit* (or *initialize-before-next-circuit* (not *circuit-loaded*)))
  (when (circuit-source-menu)
    (when (case *circuit-source*
	    (:FUNCTION (circuit-function-menu))
	    (:CATALOG_FUNCTION (catalog-function-menu))
	    (:FILE (unless (eq (circuit-file-browser) :CANCEL)
		     (setq *circuit-parts* (list *circuit-filename*)))))
      (input-*circuit*s *circuit-parts*))))


;; SET-*CIRCUIT-SOURCE* Tries to figure out what the CIRCUIT argument refers to - that is to either
;; a file name or a compiled function. Sets the variables *CIRCUIT-SOURCE*, *CIRCUIT-FILENAME*,
;; *CIRCUIT-FILE-TYPE* and *CIRCUIT* accordingly. CIRCUIT can be either a symbol or a string, and if
;; it refers to a file, may be either a string with the complete pathname or a file in the
;; *CIRCUIT-DIRECTORY* directory (default is surf-hippo/circuits/).
(defun set-*circuit-source* (circuit)
  (let (circuit-car circuit-cdr)
    (typecase circuit
      (cons (setq circuit-car (car circuit)
		  circuit-cdr (cdr circuit)))
      (t (setq circuit-car circuit)))
    (setq *circuit-source*
	  (cond ((if (stringp circuit-car)
		     (fboundp (read-from-string circuit-car))
		     (or (functionp circuit-car) (fboundp circuit-car)))
		 ;; CIRCUIT points to a function.
		 (setq *circuit-function* circuit
		       *circuit* (string circuit-car)
		       *input-is-function* t
		       *circuit-filename* nil)
		 ;; *CIRCUIT-SOURCE* may be either :CATALOG_FUNCTION or :FUNCTION.
		 (if (member *circuit-function* *CIRCUIT-FUNCTIONS*) :catalog_function :FUNCTION))
		((and
		  (not circuit-cdr)	; Don't allow a CIRCUIT list for filename parsing.
		  (or
		   ;; Is CIRCUIT a valid filename by itself?
		   (find-and-set-circuit-file-variables (string circuit))
		   ;; Or is CIRCUIT a valid filename in the *CIRCUIT-DIRECTORY*?
		   (find-and-set-circuit-file-variables (concatenate-strings *circuit-directory* (string circuit)))))
		 ;; CIRCUIT is a filename.
		 (setq *input-is-function* nil
		       *circuit-parts* (list *circuit-filename*)
		       *circuit-function* nil
		       *circuit-file-type* (anatomy-file-type circuit))
		 :FILE)
		(t (sim-error (format nil "Cannot find a source for circuit ~a!~%" circuit)))))))

(defun anatomy-file-type (filename)
  (let ((filename (replace-tilde-in-path filename)))
    (when (probe-file filename)
      (with-open-stream (stream (open filename :direction :input))
	(setq *circuit-file-type*
	      (if (and (search "VERSION" (string-upcase (read-line stream)))
		       (search "FILE ID" (string-upcase (read-line stream))))
		  :neurolucida
		  (when
		      (string= ".lisp" (string-tail (namestring filename) 5))
		    :lisp)))))))
      
;; This is also done by the QUIT-WITH-FILE function called by the CIRCUIT-FILE-BROWSER.
(defun find-and-set-circuit-file-variables (filename)
  (let ((filename (replace-tilde-in-path filename)))
    (when (probe-file filename)
      (setq *circuit-filename* (namestring filename)
	    *circuit-file* (file-namestring filename) 
	    *circuit-directory* (directory-namestring filename) 
	    *circuit* (pathname-name filename)))))

  

;;; Reads in the circuit function(s) or file(s) and processes the circuit structure.
(defun input-*circuit*s (&optional circuit suppress-process-circuit)
  (let ((*initialize-before-next-circuit* *initialize-before-next-circuit*)
	(circuit-parts (no-nils (coerce-to-list (or circuit *circuit-parts*)))))
    (when (and (not *within-topload*) *initialize-before-next-circuit*) (initialize-globals-for-circuit))
    (when (and (> (length circuit-parts) 1)
	       (and (not *use-simple-names*) (not *ADD-CELL-NAME-TO-SEGS*)))
      (choose-variable-values
       `((*ADD-CELL-NAME-TO-SEGS* "Add cell names to all segment names" :boolean))
       :label (format nil "Processing circuit with at least ~A parts" (length circuit-parts))))
    (loop for circuit-part in circuit-parts
	  do (read-in-circuit circuit-part) (setq *initialize-before-next-circuit* nil)))
  (if *soma*
      (progn (when (or (and (not *within-topload*) (not *initialize-before-next-circuit*))
		       (> (length *loaded-circuit-parts*) 1))
	       (make-compound-circuit-name))
	     (unless suppress-process-circuit (process-circuit-structure)))
      ;; Signal an non-fatal error.
      (display-message (format nil "Input circuit called with a non-circuit defining ~A."
			       (case *circuit-source*
				 ((:CATALOG_FUNCTION :FUNCTION) "function")
				 (:FILE "file"))))))


(defvar *read-in-circuit-level* 0)
(defun read-in-circuit (&optional circuit &key nts-cell-name (cell-origin-offset `(0.0 0.0 0.0)))
  "If CIRCUIT is supplied, then it is loaded. Otherwise, the current state of *CIRCUIT-SOURCE* is
referenced and, as appropriate, *CIRCUIT-FUNCTION* or *CIRCUIT-FILENAME* is loaded. NTS-CELL-NAME is
valid when CIRCUIT names an ntscable created file - if not supplied, cell name is taken from the
specification in the file. CELL-ORIGIN-OFFSET applies to all cells in the circuit created by the
current evaluation of READ-IN-CIRCUIT."
  (setq *circuit-processed* nil *circuit-loaded* nil)
  (cond-every
   ((= 0 *read-in-circuit-level*) (setq *session-name* *circuit*))
   (circuit (set-*circuit-source* circuit))
   (*circuit-source*
    (unless nil ; *within-topload*
      (display-message 
       (format nil "~a in circuit ~a..." (if *initialize-before-next-circuit* "Reading" "Adding") *circuit*)))
    (let ((previous-cells (cells))
	  (*monitor-circuit-modifications*)
	  (*read-in-circuit-level* (1+ *read-in-circuit-level*))
	  error)
      (case *circuit-source*
	((:CATALOG_FUNCTION :FUNCTION)
	 (let ((function-sym (typecase *circuit-function*
				       (string (read-from-string *circuit-function*))
				       (t *circuit-function*))))
	   (unless (member function-sym *CIRCUIT-FUNCTIONS*) (push function-sym *CIRCUIT-FUNCTIONS*))
	   (typecase function-sym
		     (cons (apply (car function-sym) (cdr function-sym)))
		     (t (funcall function-sym))) ; Call function that defines circuit.
	   (when (= 1 *read-in-circuit-level*) (push *circuit-function* *loaded-circuit-parts*))))
	(:FILE
	 (if (and (open *circuit-filename* :direction :probe)
		  (case *circuit-file-type*
		    (:neurolucida (READ-NEUROLUCIDA-FILE *circuit-filename*))
		    (t			; (:lisp :ntscable :surf-functions)
		     (setq *nts-cell-type* "" *nts-cell-name* "")
		     (load (pathname *circuit-filename*)) ; LOAD THE CELL FILE.
		     (when *process-ntscable-list* 
		       (unless (typecase *nts-cell-type*
					 (string (> (length *nts-cell-type*) 0))
					 (t *nts-cell-type*))
			 (setq *nts-cell-type* (pathname-name *circuit-file*)))
		       (unless (typecase *nts-cell-name*
					 (string (> (length *nts-cell-name*) 0))
					 (t *nts-cell-name*))
			 (setq *nts-cell-name* (pathname-name *circuit-file*))))
		     t)))
	     (when (= 1 *read-in-circuit-level*) (push (pathname-name *circuit-file*) *loaded-circuit-parts*))
	   (setq error (format t "File ~a does not exist!~%" *circuit-file*)))))
      (unless error
	(when *process-ntscable-list*
	  (process-ntscable-list (or nts-cell-name *nts-cell-name*))
	  (setq *process-ntscable-list* nil))
	;; ADJUST ORIGINS OF NEW CELLS.
	(unless (eq cell-origin-offset '(0.0 0.0 0.0))
	  (loop for cell in (cells) unless (member cell previous-cells)
		do (setf (cell-origin cell) (mapcar `+ (cell-origin cell) cell-origin-offset))))
	(setq *circuit-loaded* (true-p (and *soma* *cell*))))))))

						
(defun make-compound-circuit-name ()
  (setq *multiple-source-circuit* t
	; *ADD-CELL-NAME-TO-SEGS* t
	)
  (let ((names (copy-list *loaded-circuit-parts*))
	(*automatic-run* t)
	(dummy1 nil))
    (choose-variable-values
     `((*circuit* "What do you want to call this group" :string)
       (dummy1 ,(format nil "Ignore above and generate name~%composed of all the parts") :boolean)
       ; (*ADD-CELL-NAME-TO-SEGS* "To be safe, add cell name to all segment names" :boolean)
       )
     :label (let ((string "Naming circuit made from functions: "))
	      (loop for name in names do (setq string (concatenate-strings string (format nil "~A" name) " ")))
	      string))
    (when (or dummy1 (= 0 (length *circuit*)))
      (setq *circuit* (CONCATENATE-ATOMS-TO-STRING-LIST *loaded-circuit-parts* :string-spacer "_")))))


(defvar *disable-process-circuit-structure* nil)

	
(defun process-circuit-structure (&optional force circuit-element-to-update)
  (unless (and (not force)
	       (or (not *circuit-loaded*)
		   *disable-process-circuit-structure*
		   *circuit-processed*))
    (setq *circuit-loaded* t)
    (when (= (length *circuit*) 0)
      (setq *circuit* (cell-name *cell*))
      (setq *simulation-name* (format nil "~A" *circuit*)))
    (clear-miscellaneous-element-parameters)
    (connect-loose-segments)
    (destroy-zero-length-segments)
    (when *test-for-loops* (loop-check))
    (setq *num-nodes*
	  (loop for node being the hash-value of (NODE-HASH-TABLE) when (node-is-physical-cell-node node) sum 1))
    (collect-circuit-objects)
    (unless *kill-extra-messages* (display-message (format nil "Locating segments...")))
    (locate-all-nodes)
    (reorder-circuit)
    (print-branch-points)
    (choose-plot-data)
    (setq *circuit-processed* t)
    (set-circuit-elements-parameters nil circuit-element-to-update)
;    (update-simulation-time-stamp-and-name)
    ))

(defun clear-miscellaneous-element-parameters ()
  (loop for elt in (cell-elements) do
	(element-parameter elt 'adjacent-nodes-and-g-axials nil)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar *within-topload* nil)


(defmacro topload (&body body)
  "Load circuit definition expressed by BODY. If not called recursively, first clears all circuits.
BODY may be a (function) symbol, filename, or a series of Lisp forms."
  `(progn
    (when (and (not *within-topload*) *initialize-before-next-circuit*) (initialize-globals-for-circuit))
    (let* ((*initialize-before-next-circuit* (not *within-topload*))
	   (*within-topload* t)
	   (form (progn . ,body)))
      (if (or (stringp form) (extract-function form))
	  (input-*circuit*s form *within-topload*)
	  (when (and *soma* *cell*)
	    (progn (setq *circuit-source* :forms)
		   (process-circuit-structure t)))))))
       
(defmacro surfload (&body body)
  `(progn
     (when (and (not *within-topload*) *initialize-before-next-circuit*) (initialize-globals-for-circuit))
     (let* ((*initialize-before-next-circuit* (not *within-topload*))
            (*within-topload* t)
            (form (progn . ,body)))
       (if (or (stringp form) (extract-function form))
	   (input-*circuit*s form)
	 (when (and *soma* *cell*)
	   (progn (setq *circuit-source* :forms)
		  (process-circuit-structure t)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun add-circuit (circuit &optional ADD-CELL-NAME-TO-SEGS suppress-process-circuit)
  (let ((*automatic-run* t)
	(*initialize-before-next-circuit* nil)
	(*ADD-CELL-NAME-TO-SEGS* ADD-CELL-NAME-TO-SEGS))
    (input-*circuit*s circuit suppress-process-circuit)))


;; For backward compatibility
(defun input-*circuit* ()
  (input-*circuit*s))
;; For backward compatibility  
(defun input-several-*circuit*s (circuit-list)
  (let ((*circuit-parts* circuit-list))
    (input-*circuit*s)))


