;;;; CarGo 3.0
;;;
;;; CarGo is copyright (c) 1994 by Peter Dudey Drake
;;;
;;; Code for user interface
;;; (Garnet version)
;;;


;;; PACKAGE

(in-package "GUI")


;;; GLOBAL VARIABLES

(defvar *selected-window* NIL)
(defvar *selected-aggregate* NIL)
(defvar *message-object* NIL)


;;; PROTOTYPES OF GRAPHIC OBJECTS

;; Fonts for labels, indices, et cetera
(create-instance
 'little-font font (:size :small) (:style :bold))

(create-instance
 'medium-font font (:style :bold))

(create-instance
 'big-font font (:size :large) (:style :bold))

;; Prototypes of common objects
(create-instance
 'proto-stone circle
 (:width (o-formula (gv *selected-window* :stone-width)))
 (:height (o-formula (gvl :width)))
 (:filling-style
  (o-formula (case (gvl :color)
	       (:black black-fill)
	       (:white white-fill)
	       (:gray gray-fill)
	       (:dark-gray dark-gray-fill)
	       (:light-gray light-gray-fill)))))


;;; INTERNAL FUNCTIONS

(defun create-message-window (text)
  "Creates a window for messages."
  (let* ((agg (create-instance				    ; Create an aggregate for the window
	       NIL aggregate))
	 (message (create-instance			    ; Create the message
		   NIL multi-text
		   (:string text) (:left 10) (:top 10)))
	 (window					    ; Create the window
	  (create-instance
	   NIL inter:interactor-window
	   (:aggregate agg) (:title "CarGo messages")
	   (:left (- *screen-width* 500))
	   (:top (- *screen-height* 150))
	   (:width 500)
	   (:height 150))))
    (add-component agg message)				    ; Add the message to the aggregate
    (update window)					    ; Update the window
    message))						    ; Return the message object

(defun create-board-background (window agg lines)
  "Creates the orange (or gray) square for the board picture."
  (s-value agg :board-background			    ; Create-button and add-indices will need this
	   (create-instance
	    NIL rectangle
	    (:line-style NIL)				    ; No outline
	    (:filling-style				    ; Make the square orange, or gray on b&w monitor
	     (if (g-value color :color-p)
		 orange-fill
	       light-gray-fill))
	    (:top (o-formula (gv window :stone-offset)))    ; The square grows with the window
	    (:left (o-formula (gvl :top)))
	    (:width (o-formula (* (gv window :stone-width) lines)))
	    (:height (o-formula (gvl :width))))))

(defun create-proto-line (window)
  "Creates a prototype grid line for the window."
  ;; Each window needs its own because the window is referred to in a formula.
  (create-instance
   NIL line
   (:line-style
    (o-formula (if (< (gv window :stone-width) 25)
		   line-1
		 line-2)))
   (:x1 (o-formula (gvl :left)))
   (:y1 (o-formula (gvl :top)))))

(defun create-horizontal-lines (proto-line board-background window lines)
  "Creates an aggrelist of horizontal lines for the board grid."
  (create-instance
   NIL aggrelist
   (:items lines)
   (:top (o-formula (gv window :stone-width)))
   (:left (o-formula (gvl :top)))
   (:width (o-formula (- (gv board-background :width)
			 (gv window :stone-width))))
   (:direction :vertical)				    ; The list, not the lines
   (:item-prototype
    `(,proto-line
      (:x2 ,(o-formula (+ (gvl :left)
			  (gvl :parent :width))))
      (:y2 ,(o-formula (gvl :y1)))))
   (:v-spacing (o-formula
		(- (gvl :top)
		   (if (< (gv window :stone-width) 25)
		       (gv line-1 :line-thickness)
		     (gv line-2 :line-thickness)))))))

(defun create-vertical-lines (horizontal-lines proto-line window)
  "Creates an aggrelist of vertical lines for the board grid."
  (create-instance
   NIL horizontal-lines
   (:direction :horizontal)                         ; The list, not the lines
   (:item-prototype
    `(,proto-line
      (:x2 ,(o-formula (gvl :x1)))
      (:y2 ,(o-formula (+ (gvl :top)
			  (gvl :parent :width))))))
   (:h-spacing
    (o-formula (- (gvl :left)
		  (if (< (gv window :stone-width) 25)
		      (gv line-1 :line-thickness)
		    (gv line-2 :line-thickness)))))))

(defun add-indices (agg window lines)
  "Adds labels and ends of grid lines for board coordinates.  Huge and ugly, but it does the trick."
  (let* ((left-indices					    ; Left indices
	  (create-instance
	   NIL aggrelist
	   (:visible					    ; Make indices invisible if window is too small
	    (o-formula (> (gv window :stone-offset)
			  (string-width little-font "888"))))
	   (:font (o-formula (gv window :font)))
	   (:left
	    (o-formula
	     (- (round (gv window :stone-width) 10/9)
		(string-width (gvl :font) "19"))))
	   (:top
	    (o-formula
	     (- (gv window :stone-width)
		(round (string-height (gvl :font) "1") 2))))
	   (:direction :vertical)
	   (:v-spacing
	    (o-formula
	     (- (gv window :stone-width)
		(string-height (gvl :font) "1"))))
	   (:items
	    (subseq
	     '("19" "18" "17" "16" "15" "14" "13" "12" "11" "10" " 9" " 8" " 7" " 6" " 5" " 4" " 3" " 2" " 1")
	     (- 19 lines)))
	   (:item-prototype
	    `(,text
	      (:font ,(formula '(gvl :parent :font)))
	      (:visible
	       ,(o-formula (gvl :parent :visible)))
	      (:string
	       ,(o-formula
		 (nth (gvl :rank)
		      (gvl :parent :items))))))))
         (right-indices					    ; Right indices
          (create-instance
           NIL left-indices
           (:left
            (o-formula
             (+ (gv agg :board-background :width)
                (round (gv window :stone-width) 10))))
           (:items
            (subseq
             '("19" "18" "17" "16" "15" "14" "13" "12" "11" "10" "9 " "8 " "7 " "6 " "5 " "4 " "3 " "2 " "1 ")
             (- 19 lines)))))
         (top-indices					    ; Top indices
          (create-instance
           NIL left-indices
           (:left
            (o-formula
             (- (gv window :stone-width)
                (round (string-width (gvl :font) "M") 2))))
           (:top
            (o-formula
             (- (round (gv window :stone-width) 10/9)
                (string-height (gvl :font) "M"))))
           (:direction :horizontal)
           (:h-spacing
            (o-formula (- (gv window :stone-width)
                          (string-width (gvl :font) "M"))))
           (:items
            (subseq
             '("a" "b" "c" "d" "e" "f" "g" "h" "j" "k" "l" "m" "n" "o" "p" "q" "r" "s" "t")
             0
             lines))))
         (bottom-indices				    ; Bottom indices
          (create-instance
           NIL top-indices
           (:top
            (o-formula
             (+ (gv agg :board-background :height)
                (round (gv window :stone-width) 10)))))))
    (add-components					    ; Add all of the index objects to AGG
     agg
     left-indices right-indices
     top-indices bottom-indices)))

(defun add-handicap-points (agg window lines)
  "Adds handicap points to the board."
  (let ((proto-handicap-point				    ; Prototype handicap point for this window
	 (create-instance
	  NIL circle
	  (:width
	   (o-formula (round (gv window :stone-width) 4)))
	  (:height (o-formula (gvl :width)))
	  (:filling-style black-fill)
	  (:left					    ; Width of a handicap point is 1/8 that of a stone
	   (o-formula
	    (+ (floor (gv window :stone-width) 8/7)
	       (* (car (gvl :point))
		  (gv window :stone-width)))))
	  (:top
	   (o-formula
	    (+ (floor (gv window :stone-width) 8/7)
	       (* (cdr (gvl :point))
		  (gv window :stone-width))))))))
    (dolist (point					    ; For each point appropriate to this board size...
	     (case lines
	       (5
		'((2 . 2)))
	       (9
		'((2 . 2) (2 . 6) (4 . 4) (6 . 2) (6 . 6)))
	       (13
		'((3 . 3) (3 . 6) (3 . 9)
		  (6 . 3) (6 . 6) (6 . 9)
		  (9 . 3) (9 . 6) (9 . 9)))
	       (19
		'((3 . 3) (3 . 9) (3 . 15)
		  (9 . 3) (9 . 9) (9 . 15)
		  (15 . 3) (15 . 9) (15 . 15)))
	       (t NIL)))
      (add-component					    ; ...Add a handicap point
       agg
       (create-instance
	NIL proto-handicap-point
	(:point point))))))

(defun create-board-picture (window agg lines fancy)
  "Creates the board background, lines, and indices and handicap points if appropriate."
  (let* ((board-aggregate
	  (create-instance NIL aggregate))
	 (board-background
	  (create-board-background window board-aggregate lines))
	 (proto-line
	  (create-proto-line window))
	 (horizontal-lines
	  (create-horizontal-lines
	   proto-line board-background window lines))
	 (vertical-lines
	  (create-vertical-lines
	   horizontal-lines proto-line window)))
    (add-components
     board-aggregate
     board-background horizontal-lines vertical-lines)
    (when fancy
      (add-indices board-aggregate window lines)
      (add-handicap-points board-aggregate window lines))
    (s-value agg :board-aggregate board-aggregate)	    ; Create-button will need to know this
    (add-component agg board-aggregate)))		    ; Add the board picture to TOP-AGGREGATE

(defun create-stone-and-label-aggregates (agg lines)
  "Creates the aggregates holding the stones and labels."
  (dotimes (column lines)
    (add-components
     agg
     (setf (svref (g-value agg :stone-aggregates)
		  (1+ column))
	   (create-instance
	    NIL aggregate
	    (:stones (make-array (1+ lines)))))		    ; An array of the colors of the stones
     (setf (svref (g-value agg :label-aggregates)
		  (1+ column))
	   (create-instance
	    NIL aggregate
	    (:labels (make-array (1+ lines))))))))	    ; An array of stacks of past labels, the car of each
							    ;  element being is the current one

(defun pixel-to-column (pixel window lines)
  "Returns the board column closest to a particular pixel position (in one dimension)."
  (let ((column
	 (1+ (round (- pixel (g-value window :stone-width))
		    (g-value window :stone-width)))))
    (cond ((>= column lines) lines)
	  ((<= column 1) 1)
	  (T column))))

(defun pixel-to-row (pixel window lines)
  (- lines -1 (pixel-to-column pixel window lines)))

(defun create-button (window agg lines)
  "Creates one big button that deals with any action in the window."
    (create-instance
     NIL inter:button-interactor
     (:continuous NIL)
     (:start-where `(:in ,(g-value agg :board-aggregate :board-background)))
     (:start-event '(:any-mousedown :any-keyboard))
     (:window window)
     (:final-function					    ; This is what the button does
      (function
       (lambda (button object-over)
	 (declare (ignore object-over))
	 (cargo:handle-event
	  (g-value button :start-char)
	  (cons (pixel-to-column
		 (inter::event-x inter::*current-event*)
		 window lines)
		(pixel-to-row
		 (inter::event-y inter::*current-event*)
		 window lines))))))))


;; A couple of utilities for putting new windows in an appropriate location

(proclaim '(inline
	     board-window-left
	     board-window-top))

(defun board-window-left ()
  (if (schema-p *selected-window*)
      (- (g-value *selected-window* :left) 25)
    (- *screen-width* 500)))

(defun board-window-top ()
  (if (schema-p *selected-window*)
      (+ (g-value *selected-window* :top) 25)
    0))

(defun create-board-window (lines fancy title)
  "Creates a window for a board."
  (let* ((agg						    ; Create an aggregate for the window
	  (create-instance
	   NIL aggregate
	   (:stone-aggregates (make-array (1+ lines)))
	   (:label-aggregates (make-array (1+ lines)))))
	 (window
	  (create-instance				    ; Create the window
	   NIL inter:interactor-window
	   (:aggregate agg) (:lines lines)
	   (:fancy fancy) (:title title)
	   (:left (board-window-left))
	   (:top (board-window-top))
	   (:width 500) (:height 500)
	   (:stone-width				    ; Width of a stone in pixels
	    (o-formula
	     (round (/ (min (gvl :width) (gvl :height))	    ; (varies with size of window)
		       (1+ (gvl :lines))))))
	   (:stone-offset (o-formula			    ; Half of :stone-width
			   (round (gvl :stone-width) 2)))
	   (:font					    ; This also varies with the size of the window
	    (o-formula (cond
			((> (gvl :stone-offset)
			    (string-width big-font "WWW"))
			 big-font)
			((> (gvl :stone-offset)
			    (string-width medium-font "WWW"))
			 medium-font)
			(T
			 little-font)))))))
    (create-board-picture window agg lines fancy)
    (create-stone-and-label-aggregates agg lines)
    (create-button window agg lines)
    (update window)					    ; Draw the window
    window))						    ; Return the window

(defun add-stone (location color)
  "Adds a stone of COLOR at LOCATION in *SELECTED-WINDOW*."
  (with-point
   (col row location)
   (let ((stone-agg
	  (svref (g-value *selected-aggregate*
			  :stone-aggregates)
		 col))
	 (lines (g-value *selected-window* :lines)))
     (add-component
      stone-agg
      (create-instance
       NIL proto-stone
       (:left
	(o-formula
	 (- (* col (gv *selected-window* :stone-width))
	    (gv *selected-window* :stone-offset))))
       (:top
	(o-formula
	 (+ (* (- lines row)
	       (gv *selected-window* :stone-width))
	    (gv *selected-window* :stone-offset))))
       (:row row)
       (:color color)))
     (setf (svref (g-value stone-agg :stones) row)	    ; Note the color of this stone
	   color))))

(defmacro cheap-kludge ()
  ;; Cheap kludge because Garnet doesn't notice changes to arrays
  '(awhen
    (find-if #'(lambda (x) (= (g-value x :row) row))
	     (g-value (svref
		       (g-value *selected-aggregate*
				:label-aggregates)
		       col)
		      :components))
    (recompute-formula it :fill-background-p)
    (recompute-formula it :line-style)))
  ;; End cheap kludge
  
(defun set-stone (location color)
  "Updates the graphical stone at LOCATION.  NIL means to remove any stone, T means to leave the stone as-is,
and any other value (e.g., :black, :white, :dark-gray) means to set the stone to that color."
  (with-point
   (col row location)
   (let ((stone-agg
	  (svref
	   (g-value *selected-aggregate*
		    :stone-aggregates)
	  col)))
     (cond
      ((not color)					    ; Color NIL -- remove any stone
       (setf (svref (g-value stone-agg :stones) row) NIL)
       (cheap-kludge)
       (aif
	(find-if #'(lambda (x) (= (g-value x :row) row))
		 (g-value stone-agg :components))
	(destroy it)))
      ((eq color T)					    ; Color T -- leave current stone
       NIL)
      ((not (svref (g-value stone-agg :stones) row))	    ; Color given, but no current stone -- create new stone
       (add-stone location color)
       (cheap-kludge))
      (T						    ; Color given -- change color of current stone
       (setf (svref (g-value stone-agg :stones) row) color)
       (cheap-kludge)
       (aif
	(find-if #'(lambda (x) (= (g-value x :row) row))
		 (g-value stone-agg :components))
	(s-value it :color color)))))))

(defun add-label (location label)
  "Adds a label of LABEL at LOCATION in *SELECTED-WINDOW*."
  (with-point
   (col row location)
   (let ((label-agg
	  (svref (g-value *selected-aggregate*
			  :label-aggregates)
		 col))
	 (lines (g-value *selected-window* :lines)))
     (add-component
      label-agg
      (create-instance
       NIL text
       (:font (o-formula (gv *selected-window* :font)))
       (:left
	(o-formula
	 (- (* col (gv *selected-window* :stone-width))
	    (round (gvl :width) 2))))
       (:top
	(o-formula
	 (- (* (- lines row -1)
	       (gv *selected-window* :stone-width))
	    (round (gvl :height) 2))))
       (:fill-background-p
	(o-formula
	 (not (svref (gv
		      (svref (gv *selected-aggregate*
				 :stone-aggregates)
			     col)
		      :stones)
		     row))))
       (:line-style
	(o-formula
	 (if (member
	      (svref (gv (svref (gv *selected-aggregate*
				    :stone-aggregates)
				col)
			 :stones)
		     row)
	      '(:black :dark-gray :gray))
	     white-line
	   default-line-style)))
       (:row row)
       (:string label)))
     (push label					    ; Note this label
	   (svref (g-value label-agg :labels) row)))))

(defun set-label (location label)
  "Updates the label at LOCATION.  If LABEL is T, no change is made.  If it is NIL, any label is removed.  If
it is :previous, the last label removed is replaced.  Otherwise, it should be a string, and is added."
  (with-point
   (col row location)
   (let ((label-agg
	  (svref
	   (g-value *selected-aggregate*
		    :label-aggregates)
	   col)))
     (cond
      ((not label)					    ; Label NIL -- remove any label
       (push NIL (svref (g-value label-agg :labels) row))
       (aif
	(find-if #'(lambda (x) (= (g-value x :row) row))
		 (g-value label-agg :components))
	(destroy it)))
      ((eq label T)					    ; Label T -- leave current label
       NIL)
      ((eq label :previous)				    ; Label :PREVIOUS -- pop a label
       (pop (svref (g-value label-agg :labels) row))	    ; Pop the current label
       (set-label					    ; Pop the next one and recursively add it
	location
	(pop (svref (g-value label-agg :labels) row))))
      ((not (stringp label))				    ; Non-string label given -- error
       (error "Labels must be strings, NIL, T, or :previous."))
      ((not (car (svref (g-value label-agg :labels) row)))  ; Label given, but no current label -- create new label
       (add-label location label))
      (T						    ; Label given -- push onto label list
       (push label
	     (svref (g-value label-agg :labels) row))
       (aif
	(find-if #'(lambda (x) (= (g-value x :row) row))
		 (g-value label-agg :components))
	(s-value it :string label)))
      ))))

(defun remove-stones-and-labels ()
  "Clears all stones and labels off of the selected window."
  (dotimes (col (1+ (g-value *selected-window* :lines)))    ; Recursively destroy the old aggregates...
    (aif (svref (g-value *selected-aggregate*
			 :stone-aggregates)
		col)
	 (destroy it))
    (aif (svref (g-value *selected-aggregate*
			 :label-aggregates)
		col)
	 (destroy it)))
  (create-stone-and-label-aggregates			    ; ...and create new ones.
   *selected-aggregate*
   (g-value *selected-window* :lines)))
    

;;; EXPORTED FUNCTIONS

(defun clear-board (&optional
		    (lines				    ; Number of lines on the board
		     (if (schema-p *selected-window*)
			 (g-value *selected-window* :lines)
		       19))
		    (fancy				    ; If non-NIL, use indices, numbers, etc.
		     (if (schema-p *selected-window*)
			 (g-value *selected-window* :fancy)
		       T))
		    (title				    ; Title for window
		     (if (schema-p *selected-window*)
			 (g-value *selected-window* :title)
		       "CarGo 3.0"))
		    (new-window NIL))			    ; If non-NIL, forces creation of new window
  "Creates a new window if necessary (or if new-window is non-NIL), or uses the selected window.  A clean
board is set up in that window, and the window is returned.."
  (unless (schema-p *selected-window*)			    ; If there isn't a current window, make one
    (setq new-window T))
  (unless (or new-window				    ; If no new window was requested, but the current one
	      (and					    ; can't be recycled, destroy the current one and make
	       (= lines (g-value *selected-window* :lines)) ; a new one.
	       (eq fancy
		   (g-value *selected-window* :fancy))))
    (funcall #'kill-graphics *selected-window*)
    (setq *selected-window* NIL)
    (setq new-window T))
  (cond
   (new-window						    ; Create a new window
    (setq *selected-aggregate*
	  (g-value
	   (setq *selected-window*
		 (create-board-window lines fancy title))
	   :aggregate)))
   (T							    ; Recycle an old window
    (remove-stones-and-labels)
    (s-value *selected-window* :title title)
    (update *selected-window*)
    *selected-window*)))

(defun kill-graphics (&rest windows)
  "Destroys the specified windows.  If no arguments are given, all windows are destroyed."
  (cond
   ((null windows)
    (clean-up :opal)
    (setq *selected-window* NIL)
    (setq *message-object* NIL))
   (T							    ; We'll want to update or check *selected-window*
    (dolist (window windows)
      (destroy window)
      (if (eq window *selected-window*)
	  (setq *selected-window* NIL)))
    (update-all))))

(defun select-window (window)
  "Selects WINDOW for input and output.  WINDOW should be the result of a call to clear-board."
  (setq *selected-aggregate*
	(g-value (setq *selected-window* window)
		 :aggregate)))

(defun set-stones (&rest stones)
  "Sets colors and labels on zero or more stones.  See set-stone for further documentation."
  ;; Example:  (set-stones '((3 . 2) :black "A") '((5 . 3) :white NIL))
  (dolist (stone stones)
    (set-stone (first stone) (second stone))
    (set-label (first stone) (third stone)))
  (update *selected-window*))
  
(defun message (format-string &rest args)
  "Prints a message in the message window, creating the window if necessary."
  (let ((text (apply #'format NIL format-string args)))
    (cond
     ((schema-p *message-object*)			    ; If *message-object* exists, set the :string slot
      (setf (g-value *message-object* :string) text)
      (update (g-value *message-object* :window)))
     (T							    ;  otherwise, create it
      (setq *message-object*
	    (create-message-window text))))))

(defun beep ()
  "Beeps.  This is in the GUI package because the command for doing it is liable to be platform-dependent."
  (inter:beep))
