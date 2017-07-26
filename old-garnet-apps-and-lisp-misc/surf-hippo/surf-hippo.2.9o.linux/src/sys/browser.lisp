;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: SURF-HIPPO; Base: 10 -*-
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


;;; SYS Source file: browser.lisp
;; NOT USED


(IN-PACKAGE "SURF-HIPPO")

;;; Stolen from Garnet demo-browser.lisp





;;   Schemata defined in the DO-GO procedure which are referenced by other
;; functions
;;

(defun PATHNAME-TO-STRING-FN (pathname)
  (if pathname
      (let ((file (file-namestring pathname)))
	(if (string= file "")   ; then pathname is a directory. So strip off the "/", get the
				; directory name, and restore the "/". 
	    (let ((directory (string-right-trim "/" (namestring pathname))))
	      (concatenate-strings (file-namestring directory) "/"))
	    file))  ; else we already got the file name.
      ""))


(defun DIRECTORY-FN (namestring)
  (let ((dir (loop for path in (directory  namestring :follow-links nil)
		   when (or (string= "lisp" (pathname-type (file-namestring path)))
			    (string= "sparcf" (pathname-type (file-namestring path)))
			    (string= "fasl" (pathname-type (file-namestring path)))
			    (string= "" (file-namestring path)))
		   collect path)))
    (if (or (null dir) (equal (car dir) namestring)) nil dir)))


(defvar status)

(defun quit-with-file (sm-item)
  (let ((file (file-namestring sm-item)))
    (unless (string= file "")		; Then pathname is a directory.
      (when (or (string= "lisp" (pathname-type file))
		(string= "sparcf" (pathname-type file))
		(string= "fasl" (pathname-type file)))
	(setq *circuit-filename* (namestring sm-item)
	      *circuit-file* file
	      *circuit-directory* (directory-namestring sm-item)
	      *circuit* (pathname-name file))))))


(defun circuit-file-browser ()
  (let (return-value)			; this function will return :cancel if CANCEL, full filename otherwise.
    (create-instance 'file-browser-win inter:interactor-window
		     (:left 400)(:top 10)(:width 300)(:height 270)
		     (:title "Surf Hippo Circuit File Browser") (:icon-title "File-Browser"))
    (s-value FILE-BROWSER-WIN :aggregate (create-instance 'FILE-BROWSER-TOP-AGG opal:aggregate))
    ;; Create FILE-BROWSER schema and add to window
    (create-instance 'FILE-BROWSER garnet-gadgets:browser-gadget
		     (:constant T :except :num-menus)
		     (:left 10) (:top 85)
		     (:num-menus 1)
		     (:additional-selection-p t)
		     (:additional-selection-function
		      #'(lambda (browser sm-item)
			  (declare (ignore browser))
			  (when sm-item
			    (setq return-value (if (quit-with-file sm-item)
						   *circuit-filename* :cancel))
			    (inter:interaction-complete))))
		     (:item-to-string-function #'PATHNAME-TO-STRING-FN)
		     (:menu-items-generating-function #'DIRECTORY-FN)
		     ;; This modification of :menu-function ensures the synchronization of the
		     ;; STATUS message with the feedback objects of the scrolling menu.
		     (:menu-function #'(lambda (browser sm-item)
					 (declare (ignore browser))
					 (and (g-value sm-item :highlighted)
					      (when (string= "" (file-namestring (g-value sm-item :item)))
						(let ((feed (g-value sm-item :parent :parent :feedback-obj)))
						  (s-value feed :obj-over sm-item)
						  (s-value sm-item :highlighted NIL)
						  (s-value STATUS :visible T)
						  (opal:update FILE-BROWSER-WIN)
						  (s-value sm-item :highlighted T)
						  (s-value feed :obj-over NIL)
						  (garnet-gadgets:push-first-item FILE-BROWSER (g-value sm-item :item))
						  (s-value STATUS :visible NIL)))))))
    (garnet-gadgets:push-first-item FILE-BROWSER (pathname surf::*circuit-directory*))
    (opal:add-component FILE-BROWSER-TOP-AGG FILE-BROWSER)
    (opal:update FILE-BROWSER-WIN)
    (create-instance 'CONTROL-PANEL opal:aggregadget
		     (:constant :left :top)
		     (:left 30) (:top 10)
		     (:parts
		      `((:QUIT-BUTTON ,garnet-gadgets:text-button-panel
			 (:left ,(o-formula (+ 150 (gvl :parent :left))))
			 (:top ,(o-formula (gvl :parent :top)))
			 (:text-offset 3) (:shadow-offset 5) (:gray-width 3)
			 (:final-feedback-p NIL)
			 (:items ("Cancel"))
			 (:selection-function
			  ,#'(lambda (gadget value)
			       (declare (ignore gadget value))
			       (setq return-value :cancel)
			       (inter:interaction-complete))))
			(:prev ,garnet-gadgets:text-button-panel
			 (:left ,(o-formula (gvl :parent :left)))
			 (:top ,(o-formula (gvl :parent :top)))
			 (:shadow-offset 5) (:gray-width 3) (:text-offset 3)
			 (:final-feedback-p NIL)
			 (:items ("Previous Directory"))
			 (:selection-function
			  ,#'(lambda (gadget value)
			       (declare (ignore gadget value))
			       (let* ((items (g-value FILE-BROWSER :items)))
				 (when items
				   (let* ((new-top-level-namestring
					   (directory-namestring (string-right-trim "/" (namestring (car items))))))
				     (when (not (string= "" new-top-level-namestring))
				       ;; Add the new item to the browser
				       (s-value STATUS :visible T)
				       (opal:update FILE-BROWSER-WIN)
				       (garnet-gadgets:push-first-item FILE-BROWSER (pathname new-top-level-namestring))
				       (s-value STATUS :visible NIL)))))))))))
    (create-instance 'STATUS opal:text
		     (:constant T :except :visible)
		     (:left 30) (:top (o-formula (+ 10 (opal:gv-bottom CONTROL-PANEL))))
		     (:string "Fetching directory information...")
		     (:font (create-instance NIL opal:font (:face :italic)))
		     (:visible NIL))
    
    (create-instance 'label opal:text
		     (:left 5) (:top (+ 10 240 ; (g-value FILE-BROWSER-WIN :height)
					))
		     (:string "Left (Middle) Mouse for Directory (File)")
		     (:font (create-instance NIL opal:font (:face :italic)))
		     (:visible t))
    (opal:add-components FILE-BROWSER-TOP-AGG CONTROL-PANEL STATUS
			 label
			 )
;    (format t "~A~%"  label)    (break)
    (opal:update FILE-BROWSER-WIN)
    (inter:wait-interaction-complete FILE-BROWSER-WIN)
    (opal:destroy FILE-BROWSER-WIN)
    return-value))














