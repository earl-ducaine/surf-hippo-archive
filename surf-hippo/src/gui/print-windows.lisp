;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: WINDOWS-HACK; Base: 10 -*-
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


;; GUI Source file: windows-hack.lisp


(IN-PACKAGE "WINDOWS-HACK")



(defvar *delete-delay-after-print* 2 "Delay erasing PS files after printing, in seconds. Needs to be
empirically determined.") 
(defvar *gc-every-print* nil)
(defvar *enable-printing* t)

(defvar *include-printer-flags* t)
;; For Solaris, *features* also has :sunos (as well as :solaris), so look for :solaris first.
(defvar *shell-lpr-command* (cond ((find :solaris *features*) "lp")
				  ((find :sunos *features*) "lpr")
				  (t "lpr")))
				  

(defvar *shell-lpr-printer-flag* (cond ((find :solaris *features*) "-d")
				       ((find :sunos *features*) "-P")
				       (t "-P"))) 





(defvar *maximum-ps-filename-length* 32) ; some problems with VMS server based printers.


;; Put all the garnet windows in the print menu, otherwise only the windows in *output-windows*.
(defvar *all-windows-to-print-window-menu* nil)

(defvar *print-now* t)
(defvar *print-windows-what-to-do* :print-now)
(defvar *print-together* nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;   Some Postscript file creation variables.   ;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar *ps-landscape-p* nil "T or NIL, to rotate 90 degrees or portrait.")
(defvar *ps-borders-p* t "T, NIL, :GENERIC, or :MOTIF, frames to print around windows.")
(defvar *ps-color-p* t "Enable color information in PS file.")
(defvar *ps-left-margin* 72 "Distance in points.")
(defvar *ps-right-margin* 72 "Distance in points.")
(defvar *ps-top-margin* 72 "Distance in points.")
(defvar *ps-bottom-margin* 72 "Distance in points.")

(defvar *ps-header-print-p* nil)	;add header page to printed ps files.
(defvar *lpr-paper-size* :a4 ":LETTER, :A4, or (WIDTH HEIGHT) in points specifies page size.")
(defvar *lpr-paper-width* 594)		; for a4, letter is 612
(defvar *lpr-paper-height* 842)		; for a4, letter is 792
(defvar *ps-position-x* :center ":LEFT, :CENTER, or :RIGHT.")
(defvar *ps-position-y* :center ":TOP, :CENTER, or :BOTTOM.")
(defvar *ps-scale-x* nil "Scale factor for image. Default is NIL, which means the image will be
automatically scaled to fit on the page.") 
(defvar *ps-scale-y* nil "Scale factor for image. Default is NIL, which means the image will be
automatically scaled to fit on the page.")
(defvar *ps-file-page-comment* "" "A string added to the lower left corner of all PS files.")
(defvar *kill-ps-margins* nil)
(defvar *include-filename-and-date-in-ps-files* t
  "Include filename and date in lower right corner of PS files.")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar *kill-multiple-window-printing* nil) ; In some setups printing more than one window at a
					     ; time may crash the system.

(defvar *flag-windows-for-borders* nil)

;; List of strings for which the titles of windows passed to PRINT-WINDOWS are checked - if a title
;; contains one of the strings, then that window is not printed. 
(defvar *print-windows-exclusion-list* nil)

;; List of strings for which the titles of windows passed to PRINT-WINDOWS are checked - if a title
;; contains one of the strings, then that window is printed. 
(defvar *print-windows-inclusion-list* nil)
(defvar *print-windows-include-title* nil)


;; (defvar *hard-copy-screen nil)


;;; PRINT-WINDOW-MENU Returns a list of windows selected from all the opal windows except for menu
;;; windows.
(defun print-window-menu (&optional pre-selected-window-list)
  (window-selection-menu
   "Select Windows For Saving As .PS Files"
   (if *all-windows-to-print-window-menu* (garnet-debug:windows) *output-windows*)
   pre-selected-window-list))


(defun make-ps-file (window-or-window-list filename &optional borders-p-for-windows)
  (let (*print-pretty*)
    (opal:make-ps-file window-or-window-list filename
		       :include-filename-and-date *include-filename-and-date-in-ps-files*
		       :page-comment *ps-file-page-comment*
		       :borders-p-for-windows borders-p-for-windows
		       :position-x *ps-position-x*
		       :position-y *ps-position-y*
		       :landscape-p *ps-landscape-p*
		       :borders-p *ps-borders-p*
		       :color-p *ps-color-p*
		       :left-margin *ps-left-margin*
		       :right-margin *ps-right-margin*
		       :top-margin *ps-top-margin*
		       :bottom-margin *ps-bottom-margin*
		       :paper-size *lpr-paper-size*
		       :SCALE-X *ps-scale-x*   :SCALE-Y *ps-scale-y*
		       )
    (unless (probe-file filename)
      (sim-error (format nil "PS file ~A not created!" filename)))))


(defun mouse-print-window (filename)
  (make-ps-file (nth 1 (garnet-debug::ident)) filename))


(defun ps-file-options-menu ()
  (let ((dummy1 (if (numberp *ps-scale-x*) *ps-scale-x* 1.0))
	(dummy2 (numberp *ps-scale-x*))
	(dummy3 (if (numberp *ps-scale-y*) *ps-scale-y* 1.0))
	(dummy4 (numberp *ps-scale-y*))
	(dummy5 (case *lpr-paper-size*
		  ((:letter :a4) *lpr-paper-size*)
		  (t :arbitrary)))
	(dummy6 (if *ps-landscape-p* :landscape :portrait)))
    (choose-variable-values
     `(					; (*ps-header-print-p* "Print with header page" :boolean)
       (dummy6 "Image orientation:" :choose (:landscape :portrait) :label-left)
       (*ps-borders-p* "Include window borders" :boolean)
       (*flag-windows-for-borders* "Flag individual windows for borders" :boolean)
       (*ps-color-p* "Include colors in PS file" :boolean)
       (*printer* "Specify printer" :string)
       (*kill-ps-margins* "Kill margins" :boolean)
       (*shell-lpr-command* "Shell line printer command" :string)
       (dummy2 "Set X scale to number below" :boolean)
       (dummy1 "Scale X:" :float)
       (dummy4 "Set Y scale to number below" :boolean)
       (dummy3 "Scale Y:" :float)
       (*include-printer-flags* "Include printer command line flags" :boolean)
       (*include-filename-and-date-in-ps-files* "Include filename and date in ps files" :boolean)
       (*ps-file-page-comment* "Comment to add to lower left hand corner of ps files" :string)
       (dummy5 "Paper size:" :choose (:letter :a4 :arbitrary) :label-left)
       (*lpr-paper-width* "Paper width in points (when Size = Arbitrary)" :integer)
       (*lpr-paper-height* "Paper height in points (when Size = Arbitrary)" :integer)
       )		      
     :label "Postscript File Options")
    (setq *ps-landscape-p* (eq dummy6 :landscape))
    (setq *lpr-paper-size*
	  (case dummy5
	    ((:letter :a4)  dummy5)
	    (:arbitrary (list *lpr-paper-width*   *lpr-paper-height*))))
    (setq *ps-scale-x* (and dummy2 dummy1)
	  *ps-scale-y* (and dummy4 dummy3))
    nil))


(defun select-and-filter-printed-windows (windows-to-print exclusion-list inclusion-list)
  (when exclusion-list
    (setq windows-to-print 
	  (loop for win in windows-to-print collect
		(loop for exclude-title-fragment in exclusion-list 
		      when (search exclude-title-fragment (g-value win :title)) do (return nil)
		      finally (return win)))))
  (when inclusion-list
    (setq windows-to-print 
	  (loop for win in windows-to-print collect
		(loop for include-title-fragment in inclusion-list 
		      when (search include-title-fragment (g-value win :title)) do (return win)
		      finally (return nil)))))
  (clean-up-list windows-to-print))


(defun print-window-options-comment ()
  (concatenate 'string
	       (format nil
		       ;; "Current settings: [~A, ~A, ~A, Print command ~A, Printer ~A]"
		       "(~A, ~A, ~A, Print command ~A, Printer ~A)"
		       (if *ps-landscape-p* "Landscape" "Portrait")
		       (if *ps-borders-p* "Borders" "No Borders")
		       (if *include-printer-flags* "Include flags" "Omit flags")
		       *shell-lpr-command*
		       *printer*)
	       (if (or *ps-scale-x* *ps-scale-y*)
		   (format nil "~%Scale-x: ~A, SCALE-Y: ~A" *ps-scale-x* *ps-scale-y*)
		   "")))


	       

(defun reorder-windows-to-print (windows-to-print)
  (let ((reordered-list
	 (reorder-list-menu windows-to-print (loop for win in windows-to-print collect (g-value win :title))
			    "Restack Printed Windows: 1 => Bottom")))
    (setq *output-windows* (delete-duplicates (nconc *output-windows* reordered-list)))
    reordered-list))
	  

(defun determine-borders-p-for-windows (windows-to-print)
  (when *flag-windows-for-borders*
    (let ((wins-w-borders
	   (window-selection-menu "Select Windows with Printed Borders"
				  windows-to-print (when *ps-borders-p* windows-to-print))))
      (loop for win in windows-to-print collect (true-p (member win wins-w-borders))))))


(defun get-candidate-ps-windows-filename (pathname-directory windows-to-print filename-extra)
  (when windows-to-print
    (let* ((windows-to-print (coerce-to-list windows-to-print))
	   (top-window (car windows-to-print)))
      (concatenate 'string
		   pathname-directory
		   (make-nice-filename
		    (string-tail 
		     (concatenate 'string  
				  (if (and (> (length windows-to-print) 1)
					   (> (length (g-value top-window :session-name)) 1))
				      (g-value top-window :session-name)
				      (strip-displayed-host-name-from-title (g-value top-window :title)))
				  filename-extra)
		     *MAXIMUM-PS-FILENAME-LENGTH*))
		   ".ps"))))


(defun generate-ps-files (windows-to-print directory filename-extra print-together-filename)
  (when windows-to-print
    (let* ((pathname-directory (fixup-pathname-directory directory))
	   (borders-p-for-windows (determine-borders-p-for-windows windows-to-print))
	   (multiple-windows-p (> (length windows-to-print) 1)))
      (if (or *print-together* print-together-filename (not multiple-windows-p))
	  (let ((dummy1 (or
			 (when (and (stringp print-together-filename)
				    (> (length print-together-filename) 0))
			   (concatenate 'string pathname-directory (make-nice-filename print-together-filename) ".ps"))
			 (get-candidate-ps-windows-filename pathname-directory windows-to-print filename-extra)))
		dummy2)
	    (choose-variable-values `((dummy1 "Filename" :string)
				      (dummy2 "CANCEL" :boolean))
				    :label "Postscript Filename")
	    (unless dummy2
	      (when (write-file-overwrite-authorization dummy1)
		(unix:unix-mkdir (ext:unix-namestring pathname-directory nil) #o777)
		(when (ext:unix-namestring pathname-directory nil)
		  (make-ps-file windows-to-print dummy1 borders-p-for-windows)
		  (format t ";; File ~a written~%" dummy1)
		  (list dummy1)))))
	  (no-nils
	   (loop for window-to-print in windows-to-print
		 for i upfrom 0
		 collecting
		 (let ((filename (get-candidate-ps-windows-filename pathname-directory window-to-print filename-extra)))
		   (when (write-file-overwrite-authorization filename)
		     (unix:unix-mkdir (ext:unix-namestring pathname-directory nil) #o777)
		     (when (ext:unix-namestring pathname-directory nil)
		       (make-ps-file window-to-print filename)
		       (format t ";; File ~a written~%" filename)
		       filename)))))))))


(defun make-lpr-flags-string ()
  (concatenate `string			; (if (not *ps-header-print-p*) "-h ")
	       (when (> (length *printer*) 0)
		 (format nil "~a ~a" *shell-lpr-printer-flag* (string *printer*)))))


(defun convert-what-to-do-to-choose-symbol (what-to-do)
  (case what-to-do
    (:Just-write :Just_write)
    (:Print-now :Print_now)
    (:print-&-destroy :print_&_destroy)))

(defun convert-choose-symbol-to-what-to-do (choose-symbol)
  (case choose-symbol
    (:Just_write :Just-write)
    (:Print_now :Print-now )
    (:print_&_destroy :print-&-destroy)))



(defun print-windows (windows
		      &key
		      (landscape *ps-landscape-p*)
		      (include-title *print-windows-include-title*)
		      (what-to-do *print-windows-what-to-do*)
		      (printer *printer*)
		      (print-together *print-together*)
		      directory
		      arrange
		      erase-files
		      HARD-COPY-SCREEN
		      use-menu
		      (exclusion-list *print-windows-exclusion-list*)
		      (inclusion-list *print-windows-inclusion-list*))
  "Generates and prints PS files of selected WINDOWS on PRINTER [default given by *PRINTER*].
WHAT-TO-DO options include:

  :PRINT-NOW       => Send PS files to PRINTER [default].
  :JUST-WRITE      => Only write PS files.
  :PRINT-&-DESTROY => Destroy windows after writing and
                      printing PS files.

DIRECTORY applies to the PS files. If not supplied, the file directory will be determined according
to the global variable *USE-*PLOT-DIRECTORY*: when T the directory is taken from *PLOT-DIRECTORY*,
otherwise from a call to GET-PLOT-DIRECTORY. When WINDOWS is :ALL, then all visible windows are
selected for printing according to the other options. Additional options include:

  ERASE-FILES      => PS files will be erased after printing
                      [default NIL]. BUG ALERT: This may erase file before printing occurs.
  HARD-COPY-SCREEN => A PS file including all visible windows is 
                      printed [default NIL].
  PRINT-TOGETHER   => All selected windows are printed in one 
                      file [default *PRINT-TOGETHER*]. If set
                      to a filename string, this is used to name
                      the PS file. Otherwise an ad-hoc filename
                      is constructed from the selected windows.
  ARRANGE          => Arrange selected windows so that they don't 
                      overlap [default NIL]. If this is a number,
                      then ARRANGE specifies the number of windows per row.
"
  (let* ((*automatic-run* (or (not use-menu) *automatic-run*))
	 (windows-to-print (select-and-filter-printed-windows
			    (print-window-menu (if (eq :all windows) :all (coerce-to-list windows)))
			    exclusion-list inclusion-list))
	 (num-windows-to-print (length windows-to-print))
	 (multiple-windows-p (> num-windows-to-print 1))
	 ;; Unix bug causes crash if we try to print out more than one file from this function.
	 (dummy1 (and *kill-multiple-window-printing* multiple-windows-p))
	 (dummy2 (convert-what-to-do-to-choose-symbol what-to-do))
	 (dummy3 erase-files)
	 (dummy4 hard-copy-screen)
	 dummy5 (dummy6 include-title)
	 dummy7 dummy8 (dummy10 *plot-comment*) dummy11 dummy12
	 (dummy13 (true-p print-together))
	 (dummy14 arrange)
	 (dummy15 (if DIRECTORY (format nil "~a" DIRECTORY) *plot-directory*))
	 (dummy16 (or (true-p DIRECTORY) *use-*plot-directory*))
					; *flag-windows-for-borders*
					; *KILL-PS-MARGINS*
	 )
    (setq *printer* printer
	  *ps-landscape-p* landscape)
    (when (or windows-to-print *automatic-run*)
      (choose-variable-values
       `((dummy2 "What to do:" :choose (:Print_now :Just_write :print_&_destroy :No_file/print) :label-left)
	 ,(when multiple-windows-p `(dummy13 ,(format nil "Put all ~d selected windows together" num-windows-to-print)
					       :boolean))
	 (dummy6 "Include titles for windows without titles" :boolean)
	 (dummy3 "Erase PS files after printing them" :boolean)
					; (:comment ,(print-window-options-comment))
	 (dummy8 ,(format nil "Change PS options~%~A" (print-window-options-comment)) :boolean)
	 ,(when multiple-windows-p `(dummy11 "Set window order" :boolean))
	 ,(when multiple-windows-p `(dummy14 "Retile selected windows" :boolean))
	 (dummy4 "Hardcopy all the windows now" :boolean)
	 (dummy16 "Use path defined below for files (otherwise use automatic path)" :boolean)
	 (dummy15 "Edit plot (PS file) directory" :string)
	 (dummy10 "Add this string to the filename(s) created" :string)
	 (dummy7 "CANCEL" :boolean))
       :label "Window Printing Menu")
      (unless dummy7
	(let ()
	  (setq *plot-directory* dummy15
		*use-*plot-directory* dummy16)
	  (cond-every
	   (dummy11 (setq windows-to-print (reorder-windows-to-print windows-to-print)))
	   (dummy14 (setq windows-to-print (arrange-windows :windows-per-row (when (numberp arrange) arrange)
							    :wins (reverse windows-to-print))))
	   (dummy8 (ps-file-options-menu))
	   (*KILL-PS-MARGINS* (setq *ps-left-margin* 0 *ps-right-margin* 0 *ps-top-margin* 0 *ps-bottom-margin* 0))
	   (dummy4 (setq windows-to-print (visible-windows))))
	  (let (
		(dummy2 (convert-choose-symbol-to-what-to-do dummy2))
		;; Make title on window visible for the ps file.
		(windows-w-new-titles (when (and (not dummy7) dummy6) (add-titles-to-those-without windows-to-print))))

	    (setq *print-together* (or dummy4 dummy13)
		  *print-windows-what-to-do* dummy2
		  *print-windows-include-title* dummy6)
	    (loop for win in windows-to-print do (update-title win))
	    (case dummy2
	      ((:print-now :print-&-destroy :just-write)
	       (loop for ps-file in (generate-ps-files windows-to-print
						       (if *use-*plot-directory* *plot-directory* (get-plot-directory))
						       dummy10 print-together)
		     when (or (eq dummy2 :print-now) (eq dummy2 :print-&-destroy))
		     do (print-ps-file ps-file (make-lpr-flags-string))
		     when dummy3 do (sleep *delete-delay-after-print*) (delete-file ps-file))))
	    (if (eq dummy2 :print-&-destroy)
		(progn
		  (unlock-windows windows-to-print)
		  (mapcar 'clear-window windows-to-print))
		;; Make title on windows invisible again, since we also have title on window manager title bar.
		(mapcar 'remove-title windows-w-new-titles)))
	  (reset-*ps-margin*s)
	  (setq *print-together* dummy13))))))


(defun hard-copy-screen ()
  (let ((*automatic-run* t))
    (print-windows nil
		   :what-to-do :print-now
		   :erase-files t
		   :hard-copy-screen t)))


(defun reset-*ps-margin*s ()
  (setq *ps-left-margin* 72 *ps-right-margin* 72 *ps-top-margin* 72 *ps-bottom-margin* 72)
  nil)


(defun print-ps-file (filename &optional lpr-flags-string)
  (when *gc-every-print* (ext:gc))
  (when *enable-printing*
    (let ((*announce-shell-exec* t))
      (shell-exec (concatenate `string
			       *shell-lpr-command*
			       " "
			       ;;	   " -s "
			       (when *include-printer-flags* lpr-flags-string)
			       (format nil "  '~A'" filename))))))


(defun arrange-plot-and-clear-standard-plots (&key
					      (print-together *print-together*)
					      (landscape-p *ps-landscape-p*)
					      (borders-p *ps-borders-p*)
					      (wins-per-row ; :max-pack
					       *arrange-windows-per-row*)
					      break-after-plot)
  (let* ((*print-together* print-together) 
	 (*ps-borders-p* borders-p)
	 (*ps-landscape-p* landscape-p)
	 (wins (reverse (windows-of-mode :standard-plot))) ; Put first win first.
	 (wins-per-row
	  (case wins-per-row
	    (:max-pack (round (sqrt (length wins))))
	    (t (round wins-per-row)))))
		      
    (arrange-windows :windows-per-row wins-per-row :wins wins)
    (print-windows wins)
    (when break-after-plot (break))
    (clear-windows (windows-of-mode :standard-plot) t)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(export '(*ps-position-x* *ps-position-y* *ps-scale-x* *ps-scale-y*
	  *DELETE-DELAY-AFTER-PRINT*
	  *include-filename-and-date-in-ps-files*
	  *PS-FILE-PAGE-COMMENT*
			  
	  *ps-landscape-p* *ps-borders-p* *ps-color-p* *ps-left-margin* *ps-right-margin*
	  *ps-top-margin* *ps-bottom-margin* *ps-header-print-p* *lpr-paper-size*
	  *lpr-paper-width* *lpr-paper-height*

	  *print-now* *PRINT-WINDOWS-WHAT-TO-DO*
	  *gc-every-print* *enable-printing* 
	  *print-together* *print-windows-inclusion-list* *print-windows-exclusion-list*
	  *print-windows-include-title*
	  *MAXIMUM-PS-FILENAME-LENGTH*

	  *shell-lpr-command*
	  *shell-lpr-printer-flag*
	  *include-printer-flags*
	  *flag-windows-for-borders*  
	  *kill-ps-margins*
	  *kill-multiple-window-printing*
	  
	  print-windows make-ps-file
	  HARD-COPY-SCREEN
	  PRINT-WINDOW-MENU
	  PS-FILE-OPTIONS-MENU

	  arrange-plot-and-clear-standard-plots))


