;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: SURF; Base: 10 -*-
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


;;; SYS Source file: info-hack.lisp
(IN-PACKAGE "SURF")


(create-instance 'info-window  inter:interactor-window
		 (:icon-title (o-formula (gvl :title)))
		 (:title "Surf-Hippo Information Window") (:visible nil)
		 (:mode :info))

(defun create-info-window (&key (width 750) (height 300) type)
  (let* ((win (create-instance nil info-window
			       (:width width)
			       (:title
				 (GET-win-TITLE-STRING
				  "Surf-Hippo Information Window"))
			       (:height height)
			       (:type type)
			       (:omit-title-bar-p *omit-title-bar*)))
	 (scroll-win (create-instance nil gg:motif-scrolling-window-with-bars
				      (:mode 'top-scroll)
				      (:left 0)
				      (:top 0)
				      (:width (o-formula (- (gvl :parent-window :width) (* 2 (gvl :border-width)))))
				      (:height (o-formula (floor
							   (- (gvl :parent-window :height)
							      (gvl :border-width)))))
				      (:parent-window win)
				      (:total-width (o-formula (+ (gvl :parent-window :text-object :width)
								  (gvl :parent-window :text-object :left)) 200))
				      (:total-height (o-formula (+ (gvl :parent-window :text-object :top)
								   (gvl :parent-window :text-object :height)) 200))
				      (:h-scroll-bar-p t)
				      (:v-scroll-bar-p t)))
	 (text (create-instance nil opal:multifont-text)))
    (s-value win :text-object text)
    (s-value win :visible t)
    (s-value scroll-win :visible t)
    (opal:update win)
    (s-value text :text-width (o-formula (gv scroll-win :clip-window :width)))
    (s-value text :scrolling-window scroll-win)
    (opal:update scroll-win)
    (opal:add-component (g-value scroll-win :inner-aggregate) text)
    (create-instance NIL wh::print-window-Interactor (:Window `(,win ,(g-value scroll-win :clip-window)
								,(g-value scroll-win :inner-window))))
    (create-instance NIL wh::destroy-window-Interactor (:Window `(,win ,(g-value scroll-win :clip-window)
								  ,(g-value scroll-win :inner-window))))
    (create-instance nil raise-all-menus-interactor  (:Window `(,win ,(g-value scroll-win :clip-window)
								  ,(g-value scroll-win :inner-window))))
    (s-value scroll-win :foreground-color OPAL:white)
    (add-title-to-info-window scroll-win)
    (setq *create-new-info-window* nil)
    (push win *output-windows*)
    win))

(defun get-info-window (info-pane-type &optional name &key (width 925) (height 300)
				       create-new-win session-name)
  (let ((win (if (not create-new-win) (find-info-window info-pane-type name))))
    (if (not win)
	(setq win (create-info-window :width width :height height :type info-pane-type)))
    (s-value win :title (GET-win-TITLE-STRING (string (or name info-pane-type))))
    (s-value win :locked *lock-all-windows*)
    (create-instance nil toggle-window-lock-Interactor (:window win))
    (if width (s-value win :width (COERCE-TO-EVEN-INT width)))
    (if height (s-value win :height (COERCE-TO-EVEN-INT height)))
    (s-value win :session-name (if session-name session-name (g-value win :title)))
    win))

(defun find-info-window (info-win-type &optional name)
  (loop for window in (clean-up-*output-windows*)	; Is there already the right kind of window?
	when (and (not (g-value window :locked))
		  (eq :info (g-value window :mode))
		  (string-equal info-win-type (g-value window :type))
		  (if (and *create-new-info-window* name)
		      (string-equal name (g-value window :title)) t))
	do (return window)))


(defun add-title-to-info-window (win)
  (s-value win :title (GET-win-TITLE-STRING (concatenate-strings *simulation-name* ": Information"))))
	   
	    


(defun print-circuit-to-info-window (&optional description)
  (let (*use-gc-announce-window* *gc-announce-text*)
    (OUTPUT-TEXT-TO-INFO-WIN (list 'print-circuit description)))
  (let (*DUMP-ANALYSIS-TO-FILE*)
    (OUTPUT-TEXT-TO-INFO-WIN (list 'print-simulation-stats))))





#|



(defun output-text-to-info-win (string-or-printing-function)
  (let (*use-gc-announce-window* *gc-announce-text* string)
    (when (or (not *standard-info-output*) *create-new-info-window*)
      (setq *standard-info-output* (create-info-window)))
    (s-value *standard-info-output* :session-name *simulation-name*)
    (add-title-to-info-window *standard-info-output*)
    (let ((input-stream
	   (make-string-input-stream
	    (with-output-to-string (*standard-output*)
	      (typecase string-or-printing-function
		(string (format t string-or-printing-function))
		(cons (apply (car string-or-printing-function)
			     (cdr string-or-printing-function)))
		(t (funcall string-or-printing-function)))))))
      (loop while (setq string (read-line input-stream nil nil nil))
            do (opal:insert-string (g-value *standard-info-output* :text-object)
                                   (format nil "~A~%" (replace-tabs-in-string string)))))
    (s-value *standard-info-output* :visible t)
    (opal:update *standard-info-output*)))

|#

(defun output-text-to-info-win (string-or-printing-function)
  (let (*use-gc-announce-window* *gc-announce-text*)
    (when (or (not *standard-info-output*) *create-new-info-window*)
      (setq *standard-info-output* (create-info-window)))
    (s-value *standard-info-output* :session-name *simulation-name*)
    (add-title-to-info-window *standard-info-output*)
    (opal:insert-string (g-value *standard-info-output* :text-object)
			(with-output-to-string (*standard-output*)
			  (typecase string-or-printing-function
			    (string (format t string-or-printing-function))
			    (cons (apply (car string-or-printing-function)
					 (cdr string-or-printing-function)))
			    (t (funcall string-or-printing-function)))))
    (opal:insert-string (g-value *standard-info-output* :text-object)
			(string #\newline))
    (s-value *standard-info-output* :visible t)
    (opal:update *standard-info-output*)))



(defun string-to-info-win (string &optional refresh-info-window (linespacer 1) (update t))
  (let (*use-gc-announce-window* *gc-announce-text*)
    (when (or (not *standard-info-output*) *create-new-info-window*)
      (setq *standard-info-output* (create-info-window)))
    (let ((text-gadget (g-value *standard-info-output* :text-object)))
      (cond (refresh-info-window
	     #+garnet-v2.1 (opal:set-strings text-gadget string)
	     #+garnet-v2.2 (opal:set-text text-gadget string)
	     (opal:go-to-end-of-text text-gadget))
	    (t (opal:insert-text text-gadget string)
	       (opal:go-to-end-of-text text-gadget)))
      (dotimes (i linespacer) (opal:go-to-next-line text-gadget))
      (s-value *standard-info-output* :visible t)
      (when update (opal:update *standard-info-output*)))))



(defun clear-info-windows ()
  (wh::clear-windows-of-mode 'info))




(defun help-window-inter-function (interactor final-obj-over)
  (declare (ignore final-obj-over))
  (let ((window (if interactor (g-value interactor :window))) *print-pretty* mode *automatic-run*)
    (when (consp window) (loop for win in window when (g-value win :mode)
			       do (setq window win)))
    (setq mode (if window (g-value window :mode) :top-level))
    (when (and (not (find-INFO-WINDOW mode nil))
	       (member mode '(:standard-plot :histology ; :top-level
					     )))
      (let ((win (get-INFO-WINDOW mode nil :width 750))
	    string)
	(s-value win :title
		 (GET-win-TITLE-STRING
		  (format nil "~a Help Window" mode)))
	(with-open-file (stream
			 (case mode
			   ;; (:top-level (concatenate-strings *surf-home* "doc/window-help/running.doc"))
			   ((:standard-plot plot) (concatenate-strings *surf-home* "doc/window-help/plotting-control.doc"))
			   (:histology (concatenate-strings *surf-home* "doc/window-help/histology-control.doc"))))
			(loop while (setq string (read-line stream nil nil nil))
			      do (opal:insert-string (g-value win :text-object)
						     (format nil "~A~%" (replace-tabs-in-string string)))))
	(s-value win :width (min (+ (g-value win :text-object :width) 50) 750))
	(resurrect-opal-win win :raise t :deiconify t :update t :visible t)))))

(s-value help-window-Interactor :final-function #'help-window-inter-function)

