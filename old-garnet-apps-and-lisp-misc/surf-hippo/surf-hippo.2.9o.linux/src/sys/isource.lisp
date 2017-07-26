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


;;; SYS Source file: isource.lisp

;;					;
;; the isource model
;;


(in-package "SURF-HIPPO")


(defun get-isource-current (source)
  (if (isource-enabled source) (isource-current source) 0.0))

(defun isource-current-value (source)
  (let ((src (element source 'isource)))
    (when src
      (if (isource-enabled src) (isource-current src) 0.0))))

(defun edit-isource (source)
  (let* ((isrc (element source 'isource))
	 (name (element-name source 'isource))
	 (dummy1 (isource-resistance isrc))
	 dummy2 (dummy3 t)
	 (dummy4 (element-parameter isrc :enable-isource-drop))
	 (dummy5 (element-parameter isrc :enable-bridge-balance))
	 (dummy6 (or (element-parameter isrc :bridge-balance) 0.0))

	 (dummy11 (or (element-parameter isrc 'pulse-transition) :fixed-slope))
	 (dummy12 (or (element-parameter isrc 'pulse-transition-slope) 100.0))
	 (dummy13 (or (element-parameter isrc 'pulse-transition-time) 0.1))
	 (dummy15 (isource-enabled isrc))
	 )
    (loop while (or dummy3 (= *pwl-isource-di-dt* 0))
	  do (setq dummy3 nil)
	  (choose-variable-values
	   '((dummy15 "Enable this source" :boolean)
	     (dummy1 "Current Source Resistance [Mohms]" :float)
	     (dummy4 "Add isource internal resistance drop to node voltage" :boolean)
	     (dummy5 "Subtract bridge balance drop from node voltage" :boolean)
	     (dummy6 "Bridge balance [mohms]" :float)
	   
	     (dummy11 "Transition between pulse stimuli defined by:"
	      :choose (:fixed-slope :fixed-transition-time))
	     (dummy12 "Pulse transition slope (nA/msec)" :float)
	     (dummy13 "Pulse transition time (msec)" :float)
	     (dummy2 "Edit stimulus" :boolean))
	   :label (format nil "Setting up parameters of current source ~A" name)
	   :text (when (= *pwl-isource-di-dt* 0) (format nil "Slope cannot be 0!")))
	  (setf (isource-enabled isrc) dummy15)
	  (element-parameter isrc :enable-isource-drop dummy4)
	  (element-parameter isrc :enable-bridge-balance dummy5)
	  (element-parameter isrc :bridge-balance dummy6)
    

 

	  (element-parameter isrc 'pulse-transition dummy11)
	  (element-parameter isrc 'pulse-transition-slope dummy12)
	  (element-parameter isrc 'pulse-transition-time dummy13)
	  (when dummy2 (edit-source-stimulus source))
	  (setf (isource-resistance isrc) dummy1))))

(defun menu-for-isources ()
  (loop for name in (select-hash-values-menu (ISOURCE-HASH-TABLE) "Select Current Sources" :punt-if-only-one-entry t)
  	do (edit-isource name)))

(defun document-isource (isrc)
  (let ((isource (element isrc 'isource)))
    (when isource
      (format t "#|~%")
      (print-isource isource)
      (format t "|#~%")
      (print-create-isource isource))))

(defun print-create-isource (isource &optional (indent-output 0))
  (when (> indent-output 0) (dotimes (i indent-output) (format t " ")))
  (format t "(add-isource ~s ~s)~%"
	  (element-name (element-cell-element isource))
	  (element-name isource))
  (when (element-parameter isource 'waveform-function)
    (when (> indent-output 0) (dotimes (i indent-output) (format t " ")))
    (document-waveform-function isource))
  (when (isource-use-pulse-list isource)
    (when (> indent-output 0) (dotimes (i indent-output) (format t " ")))
    (document-pulse-list isource)))


	      

(defun print-isource (isrc)
  (let ((isrc (element isrc 'isource)))
    (when isrc
      "Prints out the data associated with a isource."
      (cond ((= 0 (isource-type isrc))
	     (format t
		     "DC Isource ~a (Rint ~aMohms)~%    Value ~a~%"
		     (isource-name isrc)
		     (isource-resistance isrc)
		     (isource-current isrc)))
	    (t (format t
		       "Isource ~a (Rint ~aMohms, slope ~anA/msec) ~%"
		       (isource-name isrc)
		       (isource-resistance isrc)
		       *pwl-isource-di-dt*)
	       (cond
		 ((not (isource-enabled isrc))
		  (format t "  Source turned off~%"))
		 ((isource-use-pulse-list isrc)
		  (cond-every
		   ((and (element-parameter isrc 'enable-individual-pulses) (extract-pulse-list isrc))
		    (loop for pulse in (extract-pulse-list isrc)
			  do (format t
				     "      ~a nA pulse from ~a ms to ~a ms~%"
				     (nth 2 pulse) (nth 0 pulse) (nth 1 pulse))))
		   ((and (element-parameter isrc 'enable-pulse-train) (extract-pulse-train-args isrc))
		    (print-pulse-train-info "nA" (extract-pulse-train-args isrc)))))
		 ((extract-waveform-function isrc)
		  (print-spaces t 4)
		  (document-function-args (extract-waveform-function isrc)))
		 ((source-waveform-array isrc)
		  (format t "  Explicit waveform~%"))
		 (t
		  (format t "  No stimulus~%"))))))))


	  
	
(defun create-pwl-isource (cell-element &key (WAVEFORM-time-interval *default-waveform-step*)
					name
					enable-pulse-train
					node-1-cell-element
					(enable-individual-pulses t)
					pulse-train-args
					pulse-list
					(use-pulse-list t)
					waveform-spec)
  (let ((source-name (if name name (format nil "~a-isrc" (element-name cell-element))))
	(cell-element (element-cell-element cell-element)))
    (or (element source-name 'isource)
	(let* ((node1 (or (element-node node-1-cell-element)
			  (create-node "Ground" :cell (cell-name (element-cell cell-element)))))
	       (node2 (element-node cell-element))
	       (isrc (make-isource :name source-name :node-1 node1 :node-2 node2 
				   :cell-element cell-element :use-pulse-list use-pulse-list
				   :node-1-pointp (true-p node-1-cell-element)
				   :node-2-pointp t :type 1)))
	  (element-parameter isrc 'enable-pulse-train enable-pulse-train)
	  (element-parameter isrc 'enable-individual-pulses enable-individual-pulses)
	  (element-parameter isrc 'pulse-train-args pulse-train-args)
	  (element-parameter isrc 'waveform-function waveform-spec)
	  (pulse-list isrc pulse-list)
	  (setq *make-node-w/elements-array* t)
	  (setf (isource-current isrc) 0.0) ; ISOURCE-CURRENT is really a macro 
	  (push isrc (node-elements node2))
	  (when (true-p node-1-cell-element) (push isrc (node-elements node1)))
	  (push-onto-element-param-acons node2 :isources source-name)
	  (setf (gethash source-name (ISOURCE-HASH-TABLE)) isrc)
	  (when waveform-spec (add-waveform isrc :WAVEFORM-time-interval WAVEFORM-time-interval :waveform-spec waveform-spec :use-menu nil))
	  (setf
	   (isource-resistance isrc)
	   (let ((source-name-and-resistance
		  (car (member source-name *source-resistance-lists* :test #'equal :key 'caar))))
	     (if (eq (cdar source-name-and-resistance) 'isource)
	       (cdr source-name-and-resistance)
	       (progn
		 (push (cons (cons source-name 'isource) (isource-resistance isrc)) *source-resistance-lists*)
		 (float *isource-electrode-resistance*)))))
	  (setq *isource* isrc)))))



(defvar *debug-isource* nil)


(proclaim '(inline accumulate-node-isource-current))
(defun accumulate-node-isource-current (node isrc)
  (when *debug-isource*
    (format t "Isource ~A: ~,9f, old node-current ~,9f, " isrc (isource-current isrc) (node-current node)))
  (unless (node-has-ideal-voltage-source node)
    (setf (node-current node) (- (node-current node) (the sf (isource-current isrc)))))
  (when *debug-isource* (format t "new node-current ~,9f~%" (node-current node))))

(proclaim '(inline eval-isource))
(defun eval-isource (isrc)
  (when (isource-enabled isrc)
    ;; calculate the current send the values back where they go
    (accumulate-node-isource-current (isource-node-2 isrc) isrc)
    (when (isource-node-1-pointp isrc) (accumulate-node-isource-current (isource-node-1 isrc)) isrc)))

(defun eval-all-isources ()
  (loop for isrc in *isource-list* do
	(if (and (eq isrc *isource*) *isource*nodes*)
	    (loop for node in *isource*nodes* do
		  (let ((node (element-node node)))
		    (accumulate-node-isource-current node isrc)))
	    (eval-isource isrc))))   


(defun set-isources ()
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (loop for src in *isource-list* do
	(let ((time (the sf (if (isource-use-pulse-list src)
				(*input-time*)
				(- (*input-time*) (isource-delay src))))))
	  (unless (= 0.0 (the sf (isource-period src)))
	    (setq time (the sf (float-mod (the sf time) (the sf (isource-period src))))))
	  (if (isource-use-pulse-list src)
	      (setf (isource-current src)
		    (the sf (extract-pwl-value-single time src (isource-pwl-list src))))
	      (let ((array (the (simple-array single-float *) (isource-waveform-array src))))
		(multiple-value-bind (delayed-effective-time-integer-part delayed-effective-time-fractional-part)
		    (truncate (the sf (* (isource-waveform-time-interval-inverse src) time)))
		  (declare (single-float delayed-effective-time-fractional-part)
			   (fixnum delayed-effective-time-integer-part))
;                  
;                  (format t "delayed-effective-time-integer-part ~A time ~A val ~A~%"
;                          delayed-effective-time-integer-part time
;                          (interpolated-array-value array
;                                                    delayed-effective-time-integer-part
;                                                    delayed-effective-time-fractional-part))
		  (setf
		   (isource-current src)

		   (if (and (>= time 0.0) (< delayed-effective-time-integer-part (length array)))
		       (progn
			 (check-element-time-step-maximum (isource-waveform-time-interval-mrt src))
			 (the sf (interpolated-array-value array
							   delayed-effective-time-integer-part
							   delayed-effective-time-fractional-part)))
		       0.0)))))))
  nil)





  

