(in-package "GEM")

(defun x-draw-line (window x1 y1 x2 y2 function line-style &optional drawable)
  (declare (optimize (safety 0) (speed 3) (space 0)))
  (let* ((display-info (g-value window :display-info))
	 (root-window  (opal::display-info-root-window display-info)))
	   
    ;; Provide the actual drawable of the window if you want to bypass drawing
    ;; into the buffer.  This is used by the gesture-interactor to draw lines
    ;; directly into the window, not the buffer.

    (unless drawable
      (setf drawable (the-drawable window)))
    
     (setf function (get function :x-draw-function))

    (if line-style
	(let* ((line-style-gc (opal::display-info-line-style-gc display-info))
	       (xlib-gc-line (opal::opal-gc-gcontext line-style-gc)))

;	  (format t "gem ~S ~s~%" line-style-gc xlib-gc-line)

	  (set-line-style line-style line-style-gc xlib-gc-line root-window function)
			  
	  (xlib:draw-line drawable xlib-gc-line x1 y1 x2 y2)))))


(proclaim '(inline x-draw-line-fast))
(defun x-draw-line-fast (window x1 y1 x2 y2
				function
				line-style
				display-info
				root-window
				line-style-gc
				xlib-gc-line
				&optional drawable drawable-display)
  (declare (optimize (safety 0) (speed 3) (space 0))
	   (ignore window display-info))
  (set-line-style line-style line-style-gc xlib-gc-line root-window function)
  (xlib::draw-line-fast drawable xlib-gc-line x1 y1 x2 y2
			nil
			drawable-display)
			
  nil)
	    
  


