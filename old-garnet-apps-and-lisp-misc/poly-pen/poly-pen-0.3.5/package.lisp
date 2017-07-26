
;;; Copyright (C) 2004-2005 Yannick Gingras <ygingras@ygingras.net>

;;; Yannick Gingras grants you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.

(defpackage #:poly-pen
  (:use #:common-lisp)
  (:export
   ;; color
   #:rgb->hsv
   #:hsv->rgb
   #:rgb
   #:hsv
   #:rgb-24
   #:load-gimp-gradient
   #:make-gradient-map
   #:make-color-spiral
   #:make-color-spiral-on-sat
   #:make-color-log-spiral
   #:make-color-log-spiral-on-sat
   ;; vector space
   #:point
   #:2d-point
   #:3d-point
   #:vect
   #:2d-vect
   #:3d-vect
   #:rect
   #:line
   #:coords
   #:map-coords
   #:reduce-coords
   #:every-coords
   #:some-coords
   #:add
   #:sub
   #:mult
   #:div
   #:inv
   #:middle
   #:height
   #:width
   #:top
   #:bottom
   #:left
   #:right
   #:top-left
   #:bottom-right
   #:size
   ;; rendering
   #:*default-backend*
   #:*default-width*
   #:*default-height*
   #:*default-font-size*
   #:*default-color*
   #:*default-text-color*
   #:*canvas*
   #:canvas
   #:with-canvas
   #:with-default-canvas
   #:with-default-backend
   #:with-default-colors
   #:with-defaults
   #:image
   #:with-image
   #:with-default-image
   #:free
   #:save
   #:draw
   #:draw-on
   ;; text
   #:font
   #:with-font
   #:with-default-font
   #:ascent
   #:descent
   ;; plots
   #:histogram
	   ))

;; arch-tag: 75d89bb2-74a8-47fb-b68f-9f0cc8e8a471
