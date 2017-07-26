;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: #-PARALLEL SURF #+PARALLEL *SURF; Base: 10; -*-

;;; Taken from CLX Programmer's Reference, Introduction

;;(use-package `xlib)

(defvar *menu-item-margin* 10)

(defstruct (menu)
  "A simple menu of text strings."	;	  
  (title "Choose an item:")		;
  item-alist				;((item-window item-string))
  window
  gcontext
  width
  title-width
  item-width
  item-height
  (geometry-changed-p t))
					;nil if unchanged since displayed


(defun create-menu (parent-window text-color background-color
				  text-font)
  (make-menu
   ;; Create menu graphics context
   :gcontext (xlib:CREATE-GCONTEXT :drawable parent-window
			      :foreground text-color
			      :background background-color
			      :font text-font)
   ;; Create menu window
   :window  (xlib:CREATE-WINDOW
	     :parent  parent-window
	     :class   :input-output
	     :x       0			;temporary value
	     :y       0			;temporary value
	     :width   16		;temporary value
	     :height  16		;temporary value
	     :border-width 2
	     :border  text-color
	     :background background-color
	     :save-under :on
	     :override-redirect :on	;override window mgr when
					;positioning 
	     :event-mask (xlib:MAKE-EVENT-MASK  :leave-window
					   :exposure))))


(defun menu-set-item-list (menu &rest item-strings)
  ;; Assume the new items will change the menu's width and height
  (setf (menu-geometry-changed-p menu) t)

  ;; Destroy any existing item windows
  (dolist (item (menu-item-alist menu))
    (xlib:DESTROY-WINDOW (first item)))

  ;; Add (item-window item-string) elements to item-alist
  (setf (menu-item-alist menu)
	(let (alist)
	  (dolist (item item-strings (nreverse alist))
	    (push (list (xlib:CREATE-WINDOW
			 :parent   (menu-window menu)
			 :x       0	;temporary value
			 :y       0	;temporary value
			 :width   16	;temporary value
			 :height  16	;temporary value
			 :background (xlib:GCONTEXT-BACKGROUND
				      (menu-gcontext menu))
			 :event-mask (xlib:MAKE-EVENT-MASK  :enter-window
						       :leave-window
						       :button-press
						       :button-release))
			item)
		  alist)))))


(defun menu-recompute-geometry (menu)
  (when (menu-geometry-changed-p menu)
    (let* ((menu-font (gcontext-font (menu-gcontext menu)))
	   (title-width (text-extents menu-font (menu-title menu)))
	   (item-height (+ (font-ascent menu-font)
			   (font-descent menu-font)
			   *menu-item-margin*))
	   (item-width 0)
	   (items (menu-item-alist menu))
	   menu-width)
      ;; Find max item string width
      (setf item-width
	    (+ *menu-item-margin*
	       (dolist (next-item items item-width)
		 (setf item-width (max item-width
				       (xlib:TEXT-EXTENTS menu-font (second next-item)))))))
      ;; compute final menu width, taking margins into account
      (setf menu-width (max title-width (+ item-width *menu-item-margin*)))
      (let ((window (menu-window menu)))
	;; update width and height of menu window
	(xlib:WITH-STATE (window)
	  (setf (xlib:DRAWABLE-WIDTH window) menu-width
		(xlib:DRAWABLE-HEIGHT window) (* (1+ (length items)) item-height)))
	;; update width, height, position of item windows
	(let ((item-left (round (- menu-width item-width) 2))
	      (next-item-top (- item-height (round *menu-item-margin* 2))))
	  (dolist (next-item items)
	    (let ((window (first next-item)))
	      (xlib:WITH-STATE (window)
		(setf (xlib:DRAWABLE-HEIGHT window) item-height
		      (xlib:DRAWABLE-WIDTH window) item-width
		      (xlib:DRAWABLE-X window) item-left
		      (xlib:DRAWABLE-Y window) next-item-top)))
	    (incf next-item-top item-height))))
      ;; map all item windows
      (xlib:MAP-SUBWINDOWS (menu-window menu))
      ;; save item geometry
      (setf (menu-item-width menu) item-width
	    (menu-item-height menu) item-height
    	    (menu-width menu) menu-width
    	    (menu-title-width menu) title-width
	    (menu-geometry-changed-p menu) nil))))

(defun menu-refresh (menu)
  (let* ((gcontext (menu-gcontext menu))
	 (baseline-y (xlib:FONT-ASCENT (xlib:GCONTEXT-FONT gcontext))))
    ;; show title centered in *reverse-video*
    (let ((fg (xlib:GCONTEXT-BACKGROUND gcontext))
	  (bg (xlib:GCONTEXT-FOREGROUND gcontext)))
      (xlib:WITH-GCONTEXT (gcontext :foreground fg :background bg)
	(xlib:DRAW-IMAGE-GLYPHS
	 (menu-window menu)
	 gcontext
	 (round (- (menu-width menu)
		   (menu-title-width menu)) 2) ;start x
	 baseline-y			;start y
	 (menu-title menu))))
    ;; show each menu item (position is relative to item window)
    (let ((box-margin (round *menu-item-margin* 2)))
      (dolist (item (menu-item-alist menu))
	(xlib:DRAW-IMAGE-GLYPHS
	 (first item) gcontext
	 box-margin			;start x
	 (+ baseline-y box-margin)	;start y
	 (second item))))))

(defun menu-choose (menu x y)
  ;; display the menu so that first item is at x,y.
;  (menu-present menu x y)
  (let ((items (menu-item-alist menu))
	(mw (menu-window menu))
	selected-item)
    ;; event processing loop
    (do () (selected-item)
      (xlib:EVENT-CASE ((DRAWABLE-DISPLAY mw) :force-output-p t)
		  (:exposure
		   (count)
		   ;; discard all but final :exposure then display the menu
		   (when (zerop count) (menu-refresh menu))
		   t)
		  (button-release
		   (event-window)
		   ;; select an item
		   (setf selected-item (second (assoc event-window items)))
		   t)
		  (:enter-notify
		   (window)
		   ;; highlight an item
		   (menu-highlight-item menu (find window items :key #'first))
		   t)
		  (:leave-notify
		   (window kind)
		   (if (eql mw window)
		       ;; qut if pointer moved out of main menu window
		       (setf selected-item (when (eq kind :ancestor) :none))
		       ;; otherwise, unhighlight the item window left
		       (menu-unhightlight-item menu (find window items
							  :key #'first)))
		   t)
		  (otherwise
		   ()
		   ;; ignore and discard any other event
		   t)))
    ;; erase the menu
    (xlib:UNMAP-WINDOW mw)
    ;; return selected item string, if any
    (unless (eq selected-item :none) selected-item)))

(defun just-say-lisp (host &optional (font-name "fg-16"))
  (let* ((display (xlib:OPEN-DISPLAY host))
	 (screen (first (xlib:DISPLAY-ROOTS display)))
	 (fg-color (xlib:SCREEN-BLACK-PIXEL screen))
	 (bg-color (xlib:SCREEN-WHITE-PIXEL screen))
	 (nice-font (xlib:OPEN-FONT display font-name))
	 ;; create a menu as a child of the root window
	 (a-menu (create-menu (xlib:SCREEN-ROOT screen)
			      fg-color bg-color nice-font)))
    (setf (menu-title a-menu) "Please pick your favorite language:")
    (menu-set-item-list a-menu "Fortran" "APL" "Forth" "Lisp")
    ;; Bedevil the user until he picks lisp
    (unwind-protect
	 (loop
	  ;; determine the current root window position of the pointer
	  (multiple-value-bind (x y) (xlib:QUERY-POINTER (xlib:SCREEN-ROOT screen))
	    (let ((choice (menu-choose a-menu x y)))
	      (when (string-equal "Lisp" choice)
		(return))))))))
		      
