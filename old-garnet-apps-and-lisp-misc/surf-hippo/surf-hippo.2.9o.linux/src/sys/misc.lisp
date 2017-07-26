;; -*- mode: lisp; Syntax: Common-lisp; package: surf ; base: 10;  -*-
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


;;; SYS Source file: misc.lisp
(in-package "SURF-HIPPO")


(defun clear-user-variables (&optional variables-to-keep)
  "Unintern global variables defined during the current session, other than those given by the symbol
or list of symbols in VARIABLES-TO-KEEP [default NIL]."
  (let ((variables-to-keep (coerce-to-list variables-to-keep)))
    (loop for new-surf-variable-symbol in (get-new-surf-variable-symbols)
	  unless (member new-surf-variable-symbol variables-to-keep)
	  do (unintern new-surf-variable-symbol))))

  
(defun non-empty-hash-tables (tables)
  (loop for table in tables
	unless (HASH-TABLE-EMPTY table) collect table))

;; ************* ************* ************* *************
;;
;;          Geometry Related Functions
;;
;; ************* ************* ************* *************

;;; DISTANCE Returns the distance between the two points (x-1,y-1) and (x-2,y-2).
(proclaim '(inline distance))
(defun distance (x-1 y-1 x-2 y-2)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (single-float x-1 y-1 x-2 y-2))
  (let ((dx (- x-1 x-2))
	(dy (- y-1 y-2)))
    (the sf (sqrt (the sf (+ (the sf (* dx dx)) (the sf (* dy dy))))))))

(proclaim '(inline cartesian-distance-float))
(defun cartesian-distance-float (x1 y1 x2 y2)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (single-float x1 x2 y1 y2))
  (let ((dx (- x1 x2))
	(dy (- y1 y2)))
    (coerce (kernel::%sqrt (coerce (the sf (+ (the sf (* dx dx)) (the sf (* dy dy)))) 'double-float)) 'single-float)))

(defun cartesian-distance (x1 y1 x2 y2)
  (sqrt (+ (square (float (- x1 x2)))
	   (square (float (- y1 y2))))))

(proclaim '(inline cartesian-distance-3d-float))
(defun cartesian-distance-3d-float (x1 x2)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (do* ((x1 (the cons x1) (cdr x1))
	(x2 (the cons x2) (cdr x2))
	(x1comp (if (floatp (car x1)) (car x1) (float (the fn (car x1))))
		(if (floatp (car x1)) (car x1) (float (the fn (car x1)))))
	(x2comp (if (floatp (car x2)) (car x2) (float (the fn (car x2))))
		(if (floatp (car x2)) (car x2) (float (the fn (car x2)))))
	(sum (the single-float (square (the single-float (- (the single-float x1comp)
							    (the single-float x2comp)))))
	     (the single-float (+ sum (the single-float (square (the single-float (- (the single-float x1comp)
										     (the single-float x2comp)))))))))
       ((null x1) (the single-float (sqrt sum)))))

#|
(defun cartesian-distance-3d-float (x1 x2)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (do* ((x1 (the cons x1) (cdr x1))
	(x2 (the cons x2) (cdr x2))
	(x1comp (the sf (if (floatp (car x1)) (car x1) (float (the fn (car x1)))))
		(the sf (if (floatp (car x1)) (car x1) (float (the fn (car x1))))))
	(x2comp (the sf (if (floatp (car x2)) (car x2) (float (the fn (car x2)))))
		(the sf (if (floatp (car x2)) (car x2) (float (the fn (car x2))))))
	(sum (the sf (square (- x1comp x2comp)))
	     (the sf (+ sum (the sf (square (- x1comp x2comp)))))))
       ((null x1) (the sf (sqrt sum)))))
|#

(defun cartesian-distance-3d-float (x1 x2)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (do* ((x1 (the cons x1) (cdr x1))
	(x2 (the cons x2) (cdr x2))
	(sum (the sf (square (the sf (- (the sf (car x1)) (the sf (car x2))))))
	     (the sf (+ sum (the sf (square (the sf (- (the sf (car x1))
						       (the sf (car x2))))))))))
       ((null x1) (cond ((< sum 0) 0.0)
			(t (the sf (coerce (kernel::%sqrt (coerce sum 'double-float)) 'single-float)))))))

(defun cartesian-distance-3d-float-components (x1 y1 z1 x2 y2 z2)
  (declare (optimize (safety 0) (speed 3) (space 1))
	   (single-float x1 y1 z1 x2 y2 z2))
  (coerce
   (kernel::%sqrt (coerce (+ (square (- x1 x2))
			     (square (- y1 y2))
			     (square (- z1 z2)))
			  'double-float))
   'single-float))



(defun cartesian-distance-3d (x1 x2)
  (sqrt (loop for comp1 in x1
	      for comp2 in x2
	      summing (square (float (- comp1 comp2))))))


;; ************* ************* ************* *************
;;
;;   Time Stamp Related Functions
;;
;; ************* ************* ************* *************




(defun stamped-circuit-name ()
  (if (> (length *circuit*) 0)
    (format nil "~a-~a" *circuit* *time-stamp*)
    (format nil "~a" *time-stamp*)))

(defun update-simulation-time-stamp-and-name ()
  (encode-time-stamp)
  (setq *simulation-name* (stamped-circuit-name)))


;;; Generic function to display times

(defun display-time()
      (format t "Simulation time: ~5,0d. Stop time: ~5,0d~%" *real-time* *user-stop-time*))


#|
Subject: Re: format-time-string?
Date: 13 Nov 1997 09:09:45 -0800
From: tar@sevak.isi.edu (Thomas A. Russ)
Organization: USC-ISI
CC: sshteingold@cctrading.com
Newsgroups: comp.lang.lisp

Here's something to get you started.  It takes a universal time and
produces a formatted version.  It has some options, but not an awful
lot.  If you want to have a string returned instead of printed out, then
pass NIL to the stream argument with style :string.
|#
(defvar *format-time-smallest-unit* :second
  "One of :second, :minute, :hour, :day, :month, :year")
(defvar *format-time-style* :string
  "One of :string, :s-expression, nil.  Nil means no formatting.")
(defvar *format-time-include-date-p* t "Whether the date is included.")
(defvar *format-time-long-date-p* nil "t => February 22, 1958; nil => 2/22/58")

(defun format-time (universalTime &optional (stream t)
                    &key (smallest-unit *format-time-smallest-unit*)
                         (include-date-p *format-time-include-date-p*)
                         (long-date-p *format-time-long-date-p*)
                         (style *format-time-style*))
  ;; Formats "universalTime" on the stream "stream";
  ;; ":smallest-unit" is one of :second, :minute, :hour, :day, :month, :year
  ;;    and controls the smallest unit used in the formated time;
  ;; ":include-date-p" if non-nil means the date is included in the time
  ;;    representation.
  ;; ":long-date-p" if non-nil means to use the long form of the date;
  ;; ":style" is :string for string format, :s-expression for keyword list format, nil
  ;;    for no formatting.
  (unless (and (numberp universalTime)
               (plusp universalTime)    ; Safety valve.
               (member style '(:string :s-expression)))
    (format stream "~S" universalTime)
    (return-from format-time universalTime))
  (labels ((month-name (month)
            (aref '#("January" "February" "March" "April" "May" "June" "July"
                     "August" "September" "October" "November" "December")
                  (1- month)))
           (year-not-close-enough-p (year)
            ;; "year" is not close enough to the current year to abbreviate!
            (> (abs (- year 1997)) 45))
         (format-string-time (second minute hour date month year)
           (if long-date-p
               (ecase smallest-unit
                 (:second (format stream "~:[~3*~;~A ~D, ~D ~]~2,'0D:~2,'0D:~2,'0D"
                                  include-date-p (month-name month) date year
                                  hour minute second))
                 (:minute (format stream "~:[~3*~;~A ~D, ~D ~]~2,'0D:~2,'0D"
                                  include-date-p (month-name month) date year hour minute))
                 (:hour (format stream "~:[~3*~;~A ~D, ~D ~]~2,'0D"
                                include-date-p (month-name month) date year hour))
                 (:day (format stream "~:[~3*~;~A ~D, ~D~]"
                               include-date-p (month-name month) date year))
                 (:month (format stream "~:[~2*~;~A ~D~]"
                                 include-date-p (month-name month) year))
                 (:year (format stream "~:[~*~;~D~]" include-date-p year)))
               (ecase smallest-unit
                 (:second (format stream
                                  "~:[~3*~;~2,'0D/~2,'0D/~2,'0D ~]~2,'0D:~2,'0D:~2,'0D"
                                  include-date-p month date year hour minute second))
                 (:minute (format stream
                                  "~:[~3*~;~2,'0D/~2,'0D/~2,'0D ~]~2,'0D:~2,'0D"
                                  include-date-p month date year hour minute))
                 (:hour (format stream "~:[~3*~;~2,'0D/~2,'0D/~2,'0D ~]~2,'0D"
                                include-date-p month date year hour))
                 (:day (format stream "~:[~3*~;~2,'0D/~2,'0D/~2,'0D~]"
                               include-date-p month date year))
                 (:month (format stream "~:[~2*~;~2,'0D/~2,'0D~]"
                                 include-date-p month year))
                 (:year (format stream "~:[~*~;~D~]" include-date-p year)))))
         (format-s-expression-date (second minute hour date month year)
           (format stream "~S"
                   (ecase smallest-unit
                     (:second (if include-date-p
                                  `(:year ,year :month ,month :day ,date
                                    :hour ,hour :minute ,minute :second ,second)
                                  `(:hour ,hour :minute ,minute :second ,second)))
                     (:minute (if include-date-p
                                  `(:year ,year :month ,month :day ,date
                                    :hour ,hour :minute ,minute)
                                  `(:hour ,hour :minute ,minute)))
                     (:hour (if include-date-p
                                  `(:year ,year :month ,month :day ,date :hour ,hour)
                                  `(:hour ,hour)))
                     (:day (if include-date-p
                               `(:year ,year :month ,month :day ,date)
                               nil))
                     (:month (if include-date-p
                                 `(:year ,year :month ,month)
                                 nil))
                     (:year (if include-date-p
                                `(:year ,year)
                                 nil))))))

    (multiple-value-bind (second minute hour date month year)
        (decode-universal-time universalTime)
      (unless (or long-date-p
                  (eq smallest-unit :year)
                  (year-not-close-enough-p year))
        (setq year (mod year 100)))
      (case style
        (:string (format-string-time second minute hour date month year))
        (:s-expression (format-s-expression-date second minute hour date month year))
        (t (format stream "~S" universalTime))) )))


;; ************* ************* ************* *************
;;
;;   File Related Functions
;;
;; ************* ************* ************* *************

(defun update-surf-log-file (info-function &optional (info-function-args '()))
  (update-annotation-file info-function 
			  (concatenate-strings *surf-user-dir* "logs") "log"
			  info-function-args))

(defun load-and-compile-user-source (candidates &key src-dir bin-dir)
  "For the file names in CANDIDATES (namestrings w/o extensions), look in the SRC-DIR (if not
supplied, then the /circuits directory under SURFUSERHOME), compile file and write binary to the
BIN-DIR (if not supplied, same directory as above), and load binary."
  (let ((CIRCUIT-DIR (concatenate-strings (cdr-assoc :SURFUSERHOME lisp::*environment-list*) "/circuits/")))
    (compile-source-directory (or src-dir circuit-dir) (or bin-dir circuit-dir) candidates)))

(defun get-surf-directory (&optional sub-directory-name (time-stamp *actual-time-stamp*))
  (let ((pathname-directory (REMOVE-SPACES (concatenate-strings *surf-user-dir* "/" sub-directory-name "/"))))
    (if *make-circuit-subdir* (setq pathname-directory (concatenate-strings pathname-directory *circuit* "/")))
    (get-dated-directory pathname-directory time-stamp *make-circuit-subdir*)))

(defun get-top-level-surf-directory (&optional sub-directory-name) 
  (let ((pathname-directory (concatenate-strings *surf-user-dir* "/" (REMOVE-SPACES sub-directory-name) "/")))
    (get-dated-directory pathname-directory)))
    
(defun get-surf-data-directory ()
  "Create a directory based on the value of *SURF-USER-DIR*, and return its namestring. If
*MAKE-CIRCUIT-SUBDIR* if T, incoporate the current value of *CIRCUIT*:

   *SURF-USER-DIR*/data/*CIRCUIT*/M_D_Y/

where \"M_D_Y\" is the date. If *MAKE-CIRCUIT-SUBDIR* if NIL:

   *SURF-USER-DIR*/data/

"
  (get-surf-directory "data/"))

(defun get-surf-plot-directory ()
  "Create a directory based on the value of *SURF-USER-DIR*, and return its namestring. If
*MAKE-CIRCUIT-SUBDIR* if T, incoporate the current value of *CIRCUIT*:

   *SURF-USER-DIR*/plot/*CIRCUIT*/M_D_Y/

where \"M_D_Y\" is the date. If *MAKE-CIRCUIT-SUBDIR* if NIL:

   *SURF-USER-DIR*/plot/

"
  (get-surf-directory "plot/"))

#|
(defun wh::get-plot-directory ()
  (get-surf-directory "plot/"))

(defun wh::get-data-directory ()
  (get-surf-directory "data/"))
|#

(defun load-file-from-directory (filename dir)
  (let ((filename (REMOVE-SPACES filename)))
    (load
     (if (search "~" filename)
	 (if (= (search "~" filename) 0)
	     (concatenate-strings dir "/" (string-remove-head filename 1))
	     (sim-error (format nil "Can't parse this filename ~A" filename)))
	 filename))))

(defun load-surf-user-file (filename)
  "Loads FILENAME which must be in the Surf-Hippo user directory (as specified by the SURFUSERHOME
enviroment variable)."
  (load-file-from-directory filename *surf-user-home*))

(defun load-surf-home-file (filename)
  "Loads FILENAME which must be in the Surf-Hippo home directory (as specified by the SURFHOME
enviroment variable)."
  (load-file-from-directory filename *surf-home*))







;;; FUNCALLS-ACCORDING-TO-FLAGS Handy when the flags might be changed by some of the functions, for
;;; example with CHOOSE-VARIABLE-VALUES menus.
(defun funcalls-according-to-flags (flags-and-functions)
  (loop for flag-and-function in flags-and-functions
	for flag in (loop for flag-and-function in flags-and-functions 
			  collecting (symbol-value (car flag-and-function))) ; This preserves the boolean flags.
	when flag do (funcall (cadr flag-and-function))))



;; ************* ************* ************* *************
;;
;;    Some Window/Message Notification Related Functions
;;
;; ************* ************* ************* *************

(defun announce-warning (message)
  (unless *automatic-run*
    (let ((surf-NOTIFY-WINDOW (create-instance nil
					       inter:interactor-window
					       (:visible nil)
					       (:background-color opal:red)
					       (:title (GET-win-TITLE-STRING "SURF-HIPPO WARNING!"))
					       (:text-obj (create-instance nil opal:text
									   (:visible t) (:top 0) (:left 10)
									   (:font (opal:get-standard-font :serif :bold-italic :large))))
					       (:width (o-formula (+ (gvl :text-obj :width)
								     (gv wh::*ok-button* :width) 5 10 15)))
					       (:height (o-formula (+ 10 (max (gvl :text-obj :height)
									      (gv wh::*ok-button* :height)))))
					       (:aggregate (create-instance nil opal:aggregate)))))
      (add-ok-button surf-notify-window (g-value surf-notify-window :aggregate))
      (s-value surf-NOTiFY-WINDOW :visible t)
      (s-value surf-NOTiFY-WINDOW :text-obj :string message)
      (opal:add-component (g-value surf-notify-window :aggregate) (g-value surf-NOTiFY-WINDOW  :text-obj))
      (opal:update surf-NOTiFY-WINDOW t)      (opal:update surf-NOTiFY-WINDOW t)      (opal:update surf-NOTiFY-WINDOW t)
      (inter:wait-interaction-complete)
      (s-value surf-NOTiFY-WINDOW :visible nil)
      (opal:remove-component (g-value surf-notify-window :aggregate) (g-value surf-NOTiFY-WINDOW  :text-obj))
      (opal:update surf-NOTiFY-WINDOW t)
      )))

(defun running-window ()
  (let ((surf-NOTiFY-WINDOW (create-instance nil
					     inter:interactor-window
					     (:visible nil)
					     (:background-color opal:green)
					     (:title (GET-win-TITLE-STRING "SURF-HIPPO"))
					     (:text-obj (create-instance nil opal:text
									 (:visible t) (:top 0) (:left 0)
									 (:font (opal:get-standard-font :serif :bold-italic :large))))
					     (:width (o-formula (gvl :text-obj :width)))
					     (:height (o-formula (gvl :text-obj :height)))						
					     (:aggregate (create-instance nil opal:aggregate)))))
    (s-value surf-NOTiFY-WINDOW :visible t)
    (s-value surf-NOTiFY-WINDOW :text-obj :string "Simulation running")
    (opal:add-component (g-value surf-notify-window :aggregate) (g-value surf-NOTiFY-WINDOW  :text-obj))
    (opal:update surf-NOTiFY-WINDOW t)      (opal:update surf-NOTiFY-WINDOW t)
    (opal:update surf-NOTiFY-WINDOW t)
    surf-NOTiFY-WINDOW))


(defun surf-window-print-mini-info-inter-function (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (setq *standard-info-output* (create-info-window))
  (let ((win (g-value interactor :window)))
    (s-value *standard-info-output* :title (concatenate-strings "Information Window " (g-value win :title)))
    (OUTPUT-TEXT-TO-INFO-WIN  (list 'g-value win :mini-info) t)))

(create-instance 'window-info-Interactor inter:button-Interactor
		 (:continuous nil)
		 (:start-where t)
		 (:start-event :control-\i)
		 (:final-function #'surf-window-print-mini-info-inter-function))


;; Destroy all the created windows.
(defun wipeout ()			    
  (clear-plot-windows)
  (clear-histology-windows)
  (clear-info-windows)
  (dolist (win (garnet-debug:windows))
    (opal:destroy win)))

#|
(defvar w '())
(defvar phase-hi '())
(defvar phase-lo '())
(defvar mag-hi '())
(defvar mag-lo '())
(defun test-phase ()
  (setq w '() phase-hi '() phase-lo '() mag-hi '() mag-lo '())
  (do ((omega 0.01 (+ omega 0.01)))
      ((> omega 50.0))
    (setq w (cons omega w)
	  phase-lo (cons  (atan (- omega)) phase-lo)
	  phase-hi (cons  (atan (/ 1.0 omega)) phase-hi)
	  mag-lo (cons  (/ 1.0 (expt (+ 1 omega) 0.5))
			mag-lo)
	  mag-hi (cons  (/ 1.0 (expt (+ 1 (/ 1.0 omega)) 0.5))  mag-hi))))
  
(defun set-create-parameters (parameter-name model parameters)
  (let (parameter-value temp-parameter-value junk)
    (setf 
     parameter-value (eval (cdr-assoc parameter-name (model-default-params model)))
     temp-parameter-value (eval (cdr-assoc parameter-name (model-changed-params model)))
      junk (when temp-parameter-value (setf parameter-value temp-parameter-value))
      temp-parameter-value (eval (cdr-assoc parameter-name parameters))
      junk (when temp-parameter-value (setf parameter-value temp-parameter-value )))
    parameter-value))

|#




;; Some array related functions.

(defun change-2d-array-to-simple (old-array)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((new-array (make-array (array-dimensions (the (array single-float (* *)) old-array)) :element-type 'single-float)))
    (do ((i 0 (1+ i)))
	((= i (the fn (car (array-dimensions (the (array single-float (* *)) old-array))))))
      (declare (fixnum i))
      (do ((j 0 (1+ j)))
	  ((= j (the fn (cadr (array-dimensions (the (array single-float (* *)) old-array))))))
	(declare (fixnum j))
	(setf (aref new-array i j) (aref (the (array single-float (* *)) old-array) i j))))
    new-array))

(defun change-2d-array-to-adjustable (old-array)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((new-array (make-array (array-dimensions (the (simple-array single-float (* *)) old-array))
			       :element-type 'single-float :adjustable t)))
    (do ((i 0 (1+ i)))
	((= i (the fn (car (array-dimensions (the (simple-array single-float (* *)) old-array))))))
      (declare (fixnum i))
      (do ((j 0 (1+ j)))
	  ((= j (the fn (cadr (array-dimensions (the (simple-array single-float (* *)) old-array))))))
	(declare (fixnum j))
	(setf (aref new-array i j) (aref (the (simple-array single-float (* *)) old-array) i j))))
    new-array))

(defun change-1d-array-to-simple (old-array)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((new-array (make-array (array-dimensions old-array))))
    (do ((i 0 (1+ i)))
	((= i (the fn (car (array-dimensions old-array)))))
      (declare (fixnum i))
      (setf (aref new-array i) (aref old-array i)))
    new-array))

(defun change-1d-float-array-to-simple (old-array)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((new-array (make-array (array-dimensions old-array) :element-type 'single-float)))
    (do ((i 0 (1+ i)))
	((= i (the fn (car (array-dimensions old-array)))))
      (declare (fixnum i))
      (setf (aref new-array i) (aref old-array i)))
    new-array))

(defun change-1d-array-to-adjustable (old-array)
  (declare (optimize (safety 0) (speed 3) (space 1)))
  (let ((new-array (make-array (array-dimensions (the (simple-array single-float (*)) old-array))
			       :element-type 'single-float :adjustable t)))
    (do ((i 0 (1+ i)))
	((= i (the fn (car (array-dimensions (the (simple-array single-float (*)) old-array))))))
      (declare (fixnum i))
      (setf (aref new-array i) (aref (the (simple-array single-float (*)) old-array) i)))
    new-array))


(defun test-top-two-of-lists (list1 list2)
  (and (eq (car list1) (car list2))
       (eq (cadr list1) (cadr list2))))

