
;;; Copyright (C) 2004-2005 Yannick Gingras <ygingras@ygingras.net>

;;; Yannick Gingras grants you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.

(in-package #:poly-pen)

(defun example1-core (input output backend)
  "draw a path on a map with a circle at stop points"
  (let* ((*canvas* (image input :backend backend))
         (*default-font* (font "Vera" :backend backend))
         ;; displacement vectors from one stop to the other
         (path (mapcar #'vect
                       '((50 50) (36 32) (23 40) (74 80) (20 43) (-90 7))))
         ;; those stops have a name
         (names '("foo" "bar" "baz" "qux" "quux" "asd"))
         ;; current location
         (loc (point '(195 36)))
         ;; circles radius
         (rad 7)
         ;; pick a few colors to show progression along the path
         (cols (make-color-spiral (length path) 1 1 .8 .55 .25 nil)))
    ;; 1st stop, draw a circle
    (draw (circle loc rad) :col (elt cols 0))
    (mapcar #'(lambda (seg col name)
                ;; compute the space to skip, we don't want to draw
                ;; the path inside the circle, rounded to the closest pixel
                (let ((pad (map-coords #'round (mult (normalize seg) rad))))
                  (draw (line (add loc pad)
                              (sub (setf loc (add loc seg)) pad))
                        :col col)
                  (draw (circle loc rad) :col col)
                  ;; label the stop point at 90 degree from the path
                  (draw (add name
                             (mult 2 (2d-vect (y pad) (- (x pad))))
                             (vect loc)) :col col))) 
            path (coerce cols 'list) names)
    (save output)))

(defun example1 ()
  (let ((source-image (merge-pathnames #p"home.jpg"
				       (asdf:component-pathname
					(asdf::find-system 'poly-pen)))))
    ;; convert source to PNG
    (save #p"home.png" :canvas (image source-image :backend :gd))
    ;; draw the path with SDL
    (example1-core source-image #p"home2.bmp" :sdl)
    ;; draw the path with GD
    (example1-core source-image #p"home2.jpg" :gd)))


(defun example2 ()
  (let ((*canvas* (canvas :width 200 :height 200)))
    (mapcar #'(lambda (shape col) (draw shape :col col))
            (list (rect '(10 10) '(100 100))
                  (circle '(100 100) 30)
                  (point '(100 100))
                  (line '(10 190) '(190 190))
                  (line '(190 190) '(190 10)))
            (list (hsv '(.4 1 1))
                  (rgb '(0 0 1))
                  (rgb '(1 0 0))
                  (hsv '(.2 .9 .9))
                  (hsv '(.25 .9 .9))))
    (save #p"foo.bmp")
    (free *canvas*)))

(defun example3 ()
  (let* ((msg "abcdefghijklmnop")
         (cols (make-color-spiral (length msg) .9))
         (angle (/ pi (length msg) .5)))
    (with-defaults (:backend
                    :gd
                    :canvas (:width 200 :height 200)
                    :font ("VeraMono")
                    :colors ((rgb '(0 0 0))
                             (rgb '(1 1 1))))
      (draw "fooj")
      (draw (add "Wooo" '(0 20)))
      (draw "joo" :col (rgb '(1 0 0)) :blended nil)
      (draw "baz" :font (font "VeraIt") :col (hsv '(.2 .9 .9)))
      (draw "qux" :col (hsv '(.1 .9 .9)))
      (dotimes (x (length msg))
        (draw (add (string (elt msg x))
                   (map-coords #'round
                               (2d-vect (* 60 (cos (* x angle)))
                                        (* 60 (sin (* x angle)))))
                   (2d-vect 90 100))
            
              :col (elt cols x)))
      (save #p"example3.png"))))

(defun example4 ()
  (let ((data nil))
    (with-defaults (:backend
                    :gd
                    :canvas (:width 400 :height 300)
                    :font ("FreeSans" :size 12))
      (dotimes (x (+ 50 (random 1000)))
        (push (random 100) data))
      (histogram data :text-col (hsv '(.1 .9 .6)))
      (free *default-font*)
      (save #p"example4.png"))))

(defun example5 ()
  (let* ((width 300)
         (height 200)
         (cols (make-color-spiral width .87)))
    (with-defaults (:backend
                    :sdl
                    :canvas (:width width :height height))
      (dotimes (x width)
        (draw (line (2d-point x (/ height 6))
                    (2d-point x (/ height 2.5)))
              :col (add (hsv (elt cols x)) '(0 -.4 1))
              :map-coords #'round)
        (draw (line (2d-point x (* 1.5 (/ height 2.5)))
                    (2d-point x (* 5 (/ height 6))))
              :col (elt cols x)
              :map-coords #'round))
      (save #p"/tmp/foo.bmp"))))

(defun example6 ()
  "Draw a spiral from the powers of 1+1i."
  (with-defaults (:colors
		  ((rgb '(0 0 0)))
		  :backend :gd
		  :canvas (:width 500 :height 500)
		  :font ("VeraBd" :size 24))
    (draw (rect (2d-point 3 3)
		(2d-point (- (width *canvas*) 4)
			  (- (height *canvas*) 4)))
	  :col (rgb '(1 1 1))
	  :outline-col (hsv '(.5 0 .7))
	  :outline-width 3
	  :filled t)
    (draw (line (2d-point (/ (width *canvas*) 2) 0)
		(2d-point (/ (width *canvas*) 2) (1- (height *canvas*))))
	  :col (hsv '(.5 0 .7)))
    (draw (line (2d-point 0 (/ (height *canvas*) 2))
		(2d-point (1- (width *canvas*)) (/ (height *canvas*) 2)))
	  :col (hsv '(.5 0 .7)))
    (draw (add "Re[z]"
	       (2d-vect (- (width *canvas*) (width "Re[z]") 5)
			(- (/ (height *canvas*) 2) (height "Re[z]"))))
	  :col (hsv '(.5 0 .5)))
    (draw (add "Im[z]"
	       (2d-vect (- (/ (width *canvas*) 2) (width "Im[z]"))
			(- (height *canvas*) (height "Im[z]") 5)))
	  :col (hsv '(.5 0 .5)))
    (dotimes (x 242)
      (let ((z (expt #c(1.1 1.1) (1+ (/ x 20)))))
	(draw (add (circle (2d-point (realpart z) (imagpart z)) 4)
		   (2d-vect (/ (width *canvas*) 2)
			    (/ (height *canvas*) 2)))
	      :map-coords #'round)))
    (save #p"/tmp/foo2.png")))

(defun example7 (funct args)
  "Try a color map.  Args should no contain nb-cols"
  (with-defaults (:backend
		  :png
		  :canvas (:width 256 :height 100))
    (let ((cols (apply funct 256 args)))
      (dotimes (x 256)
	(dotimes (y 100)
	  (draw (2d-point x y)
		:col (elt cols x)))
	))
    (save #p"/tmp/foo2.png")))



;; arch-tag: 36332c15-ca44-4e48-813b-d3dc7ae2bd85
