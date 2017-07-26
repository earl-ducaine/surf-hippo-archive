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


;; GUI Source file: show-image.lisp


(IN-PACKAGE "WINDOWS-HACK")

(defun show-image (image &key (win nil window-supplied) title left top height width
			 crop image-left image-top (HORIZONTAL-SIDE-BORDER 0) (VERTICAL-SIDE-BORDER 0))
  "Returns a window, default WIN, that shows the bitmap IMAGE image. IMAGE can be either a Garnet
bitmap schema or a filename. If IMAGE is nil then browse. Image size is adjusted by
HORIZONTAL-SIDE-BORDER and VERTICAL-SIDE-BORDER. Window dimensions are given by HEIGHT or WIDTH when
non-nil, or by the adjusted image size if either CROP is non-nil or WIN is nil. Image placement is
given by IMAGE-LEFT and IMAGE-TOP when either is non-nil, otherwise is centered in window.
IMAGE-LEFT and IMAGE-TOP can either be specified in pixel coordinates, or by the keywords :LEFT or
:RIGHT, or :TOP or :BOTTOM, respectively, thus justifying the image position wrt the window as
indicated.  Place window at LEFT and TOP position in the screen, if included. Window TITLE defaults
to IMAGE, unless a supplied WIN already has a :title."
  (let* (path
	 (bitmap-object
	  (cond ((schema-p image) image)
		(t (setq path (if image
				  (pathname image)
				  (file-browser "Find Image File" "/home" '("xbm"))))
		   (create-instance nil opal:bitmap
				    (:filling-style (create-instance nil opal:filling-style
								     (:fill-style :stippled)))
				    (:image (opal:read-image (namestring path)))))))
	 (image-name (or (g-value bitmap-object :name)
			 (and path (file-namestring path))
			 "Unamed Image")))
    (when bitmap-object
      (let* ((win (or win (create-instance nil inter:interactor-window (:title (string (or title image-name)))
					   (:aggregate (create-instance nil opal::aggregate)))))
	     (image-width (+ (g-value bitmap-object :width)
			     (if HORIZONTAL-SIDE-BORDER (* 2 HORIZONTAL-SIDE-BORDER) 0)))
	     (image-height (+ (g-value bitmap-object :height)
			      (if VERTICAL-SIDE-BORDER (* 2 VERTICAL-SIDE-BORDER) 0))))
	(when (or width crop (not window-supplied)) (s-value win :width (or width image-width)))
	(when (or height crop (not window-supplied)) (s-value win :height (or height image-height)))
	(s-value bitmap-object :left (round (case image-left
					      (:left 0)
					      (:right (- (g-value win :width) image-width))
					      (t (or IMAGE-LEFT (- (/ (g-value win :width) 2) (/ image-width 2)))))))
	(s-value bitmap-object :top (round (case image-top
					     (:top 0)
					     (:bottom (- (g-value win :height) image-height))
					     (t (or IMAGE-top (- (/ (g-value win :height) 2) (/ image-height 2)))))))
	(opal:add-component (g-value win :aggregate) bitmap-object :where :back)
	(s-value win :bitmap bitmap-object)
	(s-value win :visible t)
	(s-value win :left (round (case left
				    (:right (- *screen-width* (g-value win :width)))
				    (:left 0)
				    (:center (/ (- *screen-width* (g-value win :width)) 2))
				    (t (or left (if window-supplied (g-value win :left) 0))))))
	(s-value win :top (round (case top
				   (:top 0)
				   (:bottom (- *screen-height* (g-value win :height)))
				   (:center (/ (- *screen-height* (g-value win :height)) 2))
				   (t (or top (if window-supplied (g-value win :top) 0))))))
	(resurrect-opal-win win :raise t :update t)
	win))))

(export '(show-image))


#|
(let ((left 0) (top 400))
  (loop for file in  (directory "/giga/garnet/garnet-3.0/lib/pixmaps/")
      do (setq left (+ 30 left (g-value (show-image (create-instance nil opal::pixmap
		 (:image (opal::read-xpm-file  file)))
		     :left left :top top
		     :title (namestring file)) :width)))
      when (> left 800) do (setq top (+ top 200) left 0)))

|#