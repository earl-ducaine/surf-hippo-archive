;;;  Plot 2D
;;;  Author: Roman Belavkin
;;;  Contact: R.Belavkin@mdx.ac.uk
;;;  Date started: 01/04/2002
;;;  Date: 7/05/2005
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  This code was written at the University of Nottingham, and has  ;;;
;;;  been placed in the public domain.  If you are using this code   ;;;
;;;  or any part of Garnet, please contact garnet@cs.cmu.edu to be   ;;;
;;;  put on the mailing list.                                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;  This code requires Garnet
;;;-----------------------------------------------------------------------

;;; Load Garnet here, if necessary

#|
(load "home:garnet/src/garnet-loader.lisp")
|#

(defpackage :PLOT-2D (:use :COMMON-LISP :KR))

(in-package :PLOT-2D)

(eval-when (eval load compile)
  (export '(;; Functions
            build update
            nth-width nth-height
            do-go

            ;; Line styles
            black-line
            white-line
            gray-line
            red-line
            green-line
            blue-line
            yellow-line
            purple-line
            cyan-line
            orange-line

            ;; Labels
            vert-line horiz-line
            vert-bar horiz-bar
            bullet box diamond
            triangle-up triangle-down
            vert-tic horiz-tic
            vert-sdev vert-meter

            ;; List of labels
            list-of-labels

            ;; Data and function objects
            data-line
            data-wave
            function-line
            function-wave
            data-points
            y-distr
            histogram

            ;; Plot 2D class
            plot )))


;;; Data structures

(defstruct point
  "Points as typed structures"
  (x 0 :type integer)
  (y 0 :type integer))

;;; KR types

(def-kr-type points ()
  '(or list point null)
  "List of points")

(def-kr-type line-style-list ()
  '(or list (is-a-p opal::line-style) null)
  "List of line styles")

(def-kr-type filling-style-list ()
  '(or list (is-a-p opal::filling-style) null)
  "List of filling styles")

(def-kr-type draw-function-list ()
  '(or list
       (member :copy :xor :no-op :or :clear :set
               :copy-inverted :invert :and :equiv :nand :nor
               :and-inverted :and-reverse :or-inverted :or-reverse)
       null)
  "List of draw functions")

(def-kr-type '(or list integer))

(def-kr-type '(or function null))

(def-kr-type '(or point null))


;;; Functions

(defun f-to-n (f f0 f1 n0 n1)
  (declare (number f f0 f1) (integer n0 n1))
  (the integer (+ n0 (round (*  (- f f0) (/ (- n1 n0) (- f1 f0)))))))

(defun x-coordinate-of (x plot)
  (declare (number x) (values integer))
  (cond ((not (and (schema-p plot) (gv plot :width))) 0)
        ((and (> (gv plot :width) 0) (numberp x))
         (f-to-n x
                 (gv plot :x-from)
                 (gv plot :x-to)
                 (gv plot :canvas :left)
                 (opal:gv-right (gv plot :canvas))))
        (t (gv plot :canvas :left))))

(defun y-coordinate-of (y plot)
  (declare (number y) (values integer))
  (cond ((not (and (schema-p plot) (gv plot :height))) 0)
        ((and (> (gv plot :height) 0) (numberp y))
         (f-to-n y
                 (gv plot :y-from)
                 (gv plot :y-to)
                 (opal:gv-bottom (gv plot :canvas))
                 (gv plot :canvas :top)))
        (t (gv plot :canvas :top))))

(defun delta-x (&key plot (x1 0) (x2 0))
  (declare (number x1 x2))
  (the integer (- (x-coordinate-of x2 plot)
                  (x-coordinate-of x1 plot))))

(defun delta-y (&key plot (y1 0) (y2 0))
  (declare (number y1 y2))
  (the integer (- (y-coordinate-of y2 plot)
                  (y-coordinate-of y1 plot))))

(defun xy-points (&key plot xs ys (x-offset 0) (y-offset 0))
  (declare (type (or null integer) x-offset y-offset) (list xs ys))
  (loop for x in xs
        and y in ys collect
        (make-point :x (+ (x-coordinate-of x plot) (if x-offset x-offset 0))
                    :y (- (y-coordinate-of y plot) (if y-offset y-offset 0)))))

(defun point-list (points)
  (let ((point-list nil))
    (dolist (xy points)
            (push (point-x xy) point-list)
            (push (point-y xy) point-list))
    (reverse point-list)))

(defmacro gv-parent (x)
  `(gv (kr-path 0 :parent) ,x))

(defmacro pull-value (x)
  `(let ((x (gv-parent ,x)))
     (if (listp x) (nth (gvl :rank) x) x)))

(defun nth-width ()
  (round (/ (gvl :plot :canvas :width)
            (abs (- (gvl :plot :x-to) (gvl :plot :x-from))))))

(defun nth-height ()
  (round (/ (gvl :plot :canvas :height)
            (abs (- (gvl :plot :y-to) (gvl :plot :y-from))))))

(defun filter-list (xs test)
  (declare (list xs) (function test))
  (let ((nxs nil))
    (loop for x in xs do (and (funcall test x) (push x nxs)))
    (reverse nxs)))

(defun x-list (from to n &key test)
  (declare (number from to) (integer n) ((or function null) test))
  (let* ((range (- to from))
         (step  (if (> n 0) (/ range n) 1))
         (xs (loop for i to n collect (- (incf from step) step))))
    (if test (filter-list xs test) xs)))

(defun y-list (function xs &key test)
  (declare ((or function null) function test) (list xs))
  (and function (mapcar function (if test (filter-list xs test) xs))))

;;;
;;;  Additional line styles (use these because they can be changed later)
;;;

(create-instance 'BLACK-LINE opal:line-style
   (:foreground-color opal:black))

(create-instance 'WHITE-LINE opal:line-style
   (:foreground-color opal:white))

(create-instance 'GRAY-LINE opal:line-style
   (:foreground-color opal:motif-gray))

(create-instance 'RED-LINE opal:line-style
   (:foreground-color opal:red))

(create-instance 'GREEN-LINE opal:line-style
   (:foreground-color opal:green))

(create-instance 'BLUE-LINE opal:line-style
   (:foreground-color opal:blue))

(create-instance 'YELLOW-LINE opal:line-style
   (:foreground-color opal:yellow))

(create-instance 'PURPLE-LINE opal:line-style
   (:foreground-color opal:purple))

(create-instance 'CYAN-LINE opal:line-style
   (:foreground-color opal:cyan))

(create-instance 'ORANGE-LINE opal:line-style
   (:foreground-color opal:orange))


;;;
;;; Label objects (individual objects on the graph)
;;;
;;; TO-DO: make these prototypes for use in virtual aggregadgets


(create-instance 'LABEL opal:aggregadget
   :declare (:type ((or point null) :point)
                   (integer :x :y)
                   ((or list integer) :size)
                   (line-style-or-nil :line-style)
                   (filling-style-or-nil :filling-style)
                   (draw-function :draw-function))
   (:point (o-formula (pull-value :items)))
   (:x (o-formula (if (gvl :point) (point-x (gvl :point)) 0)))
   (:y (o-formula (if (gvl :point) (point-y (gvl :point)) 0)))
   (:size  (o-formula (pull-value :size) 0))
   (:line-style    (o-formula (pull-value :line-style)))
   (:filling-style (o-formula (pull-value :filling-style)))
   (:draw-function (o-formula (pull-value :draw-function) :copy))
   (:visible       (o-formula (pull-value :visible) t)))


(create-instance 'VERT-LINE label
   (:parts
    `((:line ,opal:line
         (:x1 ,(o-formula (gv-parent :x)))
         (:y1 ,(o-formula (gv-parent :y)))
         (:x2 ,(o-formula (gvl :x1)))
         (:y2 ,(o-formula (+ (gvl :y1) (gv-parent :size))))
         (:line-style ,(o-formula (gv-parent :line-style)))
         (:draw-function ,(o-formula (gv-parent :draw-function)))
         (:visible ,(o-formula (and (gv-parent :visible)
                                    (> (gvl :height) 0))))))))


(create-instance 'HORIZ-LINE label
   (:parts
    `((:line ,opal:line
         (:x1 ,(o-formula (gv-parent :x)))
         (:y1 ,(o-formula (gv-parent :y)))
         (:x2 ,(o-formula (+ (gvl :x1) (gv-parent :size))))
         (:y2 ,(o-formula (gvl :y1)))
         (:line-style ,(o-formula (gv-parent :line-style)))
         (:draw-function ,(o-formula (gv-parent :draw-function)))
         (:visible ,(o-formula (and (gv-parent :visible)
                                    (> (gvl :height) 0))))))))


(create-instance 'VERT-BAR label
   (:parts
    `((:line ,opal:line
         (:x1 ,(o-formula (gv-parent :x)))
         (:x2 ,(o-formula (gvl :x1)))
         (:y1 ,(o-formula (+ (gv-parent :y)
                             (floor (gv-parent :size) 2))))
         (:y2 ,(o-formula (- (gvl :y1) (gv-parent :size))))
         (:line-style ,(o-formula (gv-parent :line-style)))
         (:draw-function ,(o-formula (gv-parent :draw-function)))
         (:visible ,(o-formula (and (gv-parent :visible)
                                    (> (gvl :height) 0))))))))


(create-instance 'HORIZ-BAR label
   (:parts
    `((:line ,opal:line
         (:y1 ,(o-formula (gv-parent :y)))
         (:y2 ,(o-formula (gvl :y1)))
         (:x1 ,(o-formula (- (gv-parent :x)
                             (floor (gv-parent :size) 2))))
         (:x2 ,(o-formula (+ (gvl :x1) (gv-parent :size))))
         (:line-style ,(o-formula (gv-parent :line-style)))
         (:draw-function ,(o-formula (gv-parent :draw-function)))
         (:visible ,(o-formula (and (gv-parent :visible)
                                    (> (gvl :width) 0))))))))


(create-instance 'BULLET label
   (:parts
    `((:bullet ,opal:circle
         (:left ,(o-formula (- (gv-parent :x)
                               (floor (gvl :width)  2))))
         (:top  ,(o-formula (- (gv-parent :y)
                               (floor (gvl :height) 2))))
         (:width ,(o-formula (gv-parent :size) 0))
         (:height ,(o-formula (gvl :width)))
         (:line-style ,(o-formula (gv-parent :line-style)))
         (:draw-function ,(o-formula (gv-parent :draw-function)))
         (:filling-style ,(o-formula (gv-parent :filling-style)))
         (:visible ,(o-formula (and (gv-parent :visible)
                                    (> (gvl :width) 0))))))))


(create-instance 'BOX label
   (:parts
    `((:box ,opal:rectangle
         (:left ,(o-formula (- (gv-parent :x)
                               (floor (gvl :width)  2))))
         (:top  ,(o-formula (- (gv-parent :y)
                               (floor (gvl :height) 2))))
         (:width ,(o-formula (gv-parent :size) 0))
         (:height ,(o-formula (gvl :width)))
         (:line-style ,(o-formula (gv-parent :line-style)))
         (:filling-style ,(o-formula (gv-parent :filling-style)))
         (:draw-function ,(o-formula (gv-parent :draw-function)))
         (:visible ,(o-formula (and (gv-parent :visible)
                                    (> (gvl :width) 0))))))))


(create-instance 'DIAMOND label
   (:parts
    `((:box ,opal:polyline
         (:point-list ,(o-formula
                        (let ((h (floor (gv-parent :size) 2)))
                          (list
                           (- (gv-parent :x) h) (gv-parent :y)
                           (gv-parent :x) (- (gv-parent :y) h)
                           (+ (gv-parent :x) h) (gv-parent :y)
                           (gv-parent :x) (+ (gv-parent :y) h)
                           (- (gv-parent :x) h) (gv-parent :y)))))
         (:line-style ,(o-formula (gv-parent :line-style)))
         (:filling-style ,(o-formula (gv-parent :filling-style)))
         (:draw-function ,(o-formula (gv-parent :draw-function)))
         (:visible ,(o-formula (and (gv-parent :visible)
                                    (> (gv-parent :size) 0))))))))

(create-instance 'TRIANGLE-UP label
   (:parts
    `((:box ,opal:polyline
         (:point-list ,(o-formula
                        (let ((h (floor (gv-parent :size) 2)))
                          (list
                           (gv-parent :x) (- (gv-parent :y) h)
                           (+ (gv-parent :x) h) (+ (gv-parent :y) h)
                           (- (gv-parent :x) h) (+ (gv-parent :y) h)
                           (gv-parent :x) (- (gv-parent :y) h)))))
         (:line-style ,(o-formula (gv-parent :line-style)))
         (:filling-style ,(o-formula (gv-parent :filling-style)))
         (:draw-function ,(o-formula (gv-parent :draw-function)))
         (:visible ,(o-formula (and (gv-parent :visible)
                                    (> (gv-parent :size) 0))))))))


(create-instance 'TRIANGLE-DOWN label
   (:parts
    `((:box ,opal:polyline
         (:point-list ,(o-formula
                        (let ((h (floor (gv-parent :size) 2)))
                          (list
                           (gv-parent :x) (+ (gv-parent :y) h)
                           (+ (gv-parent :x) h) (- (gv-parent :y) h)
                           (- (gv-parent :x) h) (- (gv-parent :y) h)
                           (gv-parent :x) (+ (gv-parent :y) h)))))
         (:line-style ,(o-formula (gv-parent :line-style)))
         (:filling-style ,(o-formula (gv-parent :filling-style)))
         (:draw-function ,(o-formula (gv-parent :draw-function)))
         (:visible ,(o-formula (and (gv-parent :visible)
                                    (> (gv-parent :size) 0))))))))


(create-instance 'HORIZ-TIC label
   :declare (:type (integer :offset)
                   ((or null string) :string)
                   (font :font))
   (:string (o-formula (pull-value :string)))
   (:offset (o-formula (pull-value :offset)))
   (:font   (o-formula (pull-value :font)))
   (:parts
    `((:tic ,opal:line
            (:x1 ,(o-formula (gv-parent :x)))
            (:x2 ,(o-formula (gvl :x1)))
            (:y1 ,(o-formula (gv-parent :y)))
            (:y2 ,(o-formula (- (gvl :y1) (gv-parent :size))))
            (:line-style ,(o-formula (gv-parent :line-style)))
            (:draw-function ,(o-formula (gv-parent :draw-function))))
      (:text ,opal:text
             (:justification :left)
             (:string ,(o-formula (if (gv-parent :string)
                                      (gv-parent :string) "")))
             (:left   ,(o-formula (opal:gv-center-x-is-center-of
                                   (gv-parent :tic))))
             (:top    ,(o-formula (+ (opal:gv-bottom (gv-parent :tic))
                                     (gv-parent :offset))))
             (:font   ,(o-formula (gv-parent :font)))
             (:line-style ,(o-formula (gv-parent :line-style)))
             (:draw-function ,(o-formula (gv-parent :draw-function)))))))


(create-instance 'VERT-TIC label
   :declare (:type (integer :offset)
                   ((or null string) :string)
                   (font :font))
   (:string (o-formula (pull-value :string)))
   (:offset (o-formula (pull-value :offset)))
   (:font   (o-formula (pull-value :font)))
   (:parts
    `((:tic ,opal:line
            (:x1 ,(o-formula (gv-parent :x)))
            (:x2 ,(o-formula (+ (gvl :x1) (gv-parent :size))))
            (:y1 ,(o-formula (gv-parent :y)))
            (:y2 ,(o-formula (gvl :y1)))
            (:line-style ,(o-formula (gv-parent :line-style)))
            (:draw-function ,(o-formula (gv-parent :draw-function))))
      (:text ,opal:text
             (:justification :right)
             (:string ,(o-formula (if (gv-parent :string)
                                      (gv-parent :string) "")))
             (:left   ,(o-formula (- (gvl :parent :tic :left)
                                     (gvl :width)
                                     (gv-parent :offset))))
             (:top    ,(o-formula (opal:gv-center-y-is-center-of
                                   (gv-parent :tic))))
             (:font   ,(o-formula (gv-parent :font)))
             (:line-style ,(o-formula (gv-parent :line-style)))
             (:draw-function ,(o-formula (gv-parent :draw-function)))))))


(create-instance 'VERT-SDEV label
   :declare (:type (integer :upper-bound :lower-bound :bar-size)
                   (kr-boolean :visible))
   (:upper-bound (o-formula (pull-value :upper-bound)))
   (:lower-bound (o-formula (pull-value :lower-bound)))
   (:bar-size (o-formula (pull-value :bar-size) 2))
   (:visible (o-formula (and (pull-value :visible)
                             (gvl :size) (> (gvl :size) 0))))
   (:parts
    `((:upper-line ,opal:line
            (:line-style ,(o-formula (gv-parent :line-style)))
            (:draw-function ,(o-formula (gv-parent :draw-function)))
            (:x1 ,(o-formula (gv-parent :x)))
            (:y1 ,(o-formula (gv-parent :y)))
            (:x2 ,(o-formula (gvl :x1)))
            (:y2 ,(o-formula (- (gvl :y1)
                                (min (gv-parent :size)
                                     (- (gv-parent :upper-bound)))))))
      (:lower-line ,opal:line
            (:line-style ,(o-formula (gv-parent :line-style)))
            (:draw-function ,(o-formula (gv-parent :draw-function)))
            (:x1 ,(o-formula (gv-parent :x)))
            (:y1 ,(o-formula (gv-parent :y)))
            (:x2 ,(o-formula (gvl :x1)))
            (:y2 ,(o-formula (+ (gvl :y1)
                                (min (gv-parent :size)
                                     (gv-parent :lower-bound))))))
      (:upper-bar ,opal:line
            (:line-style ,(o-formula (gv-parent :line-style)))
            (:draw-function ,(o-formula (gv-parent :draw-function)))
            (:x1 ,(o-formula (- (gv-parent :x)
                                (gv-parent :bar-size))))
            (:x2 ,(o-formula (+ (gv-parent :x)
                                (gv-parent :bar-size))))
            (:y1 ,(o-formula (gvl :y2)))
            (:y2 ,(o-formula (gvl :parent :upper-line :y2)))
            (:visible ,(o-formula (and (gv-parent :visible)
                                       (< (gv-parent :size)
                                          (- (gv-parent :upper-bound)))))))
      (:lower-bar ,opal:line
            (:line-style ,(o-formula (gv-parent :line-style)))
            (:draw-function ,(o-formula (gv-parent :draw-function)))
            (:x1 ,(o-formula (gvl :parent :upper-bar :x1)))
            (:x2 ,(o-formula (gvl :parent :upper-bar :x2)))
            (:y1 ,(o-formula (gvl :y2)))
            (:y2 ,(o-formula (gvl :parent :lower-line :y2)))
            (:visible ,(o-formula (and (gv-parent :visible)
                                       (< (gv-parent :size)
                                          (gv-parent :lower-bound))))))
      )))


(create-instance 'VERT-METER label
   :declare (:type ((or point null) :zero-point)
                   (integer :x0 :y0))
   (:zero-point (o-formula (pull-value :zero-points)))
   (:x0 (o-formula (if (gvl :zero-point)
                       (point-x (gvl :zero-point)) 0)))
   (:y0 (o-formula (if (gvl :zero-point)
                       (point-y (gvl :zero-point)) 0)))
   (:parts
    `((:meter ,opal:rectangle
         (:left  ,(o-formula (- (gv-parent :x)
                                (floor (gvl :width)  2))))
         (:top   ,(o-formula (min (gv-parent :y) (gv-parent :y0))))
         (:width ,(o-formula (gv-parent :size) 0))
         (:height ,(o-formula (abs (- (gv-parent :y) (gv-parent :y0)))))
         (:line-style ,(o-formula (gv-parent :line-style)))
         (:filling-style ,(o-formula (gv-parent :filling-style)))
         (:draw-function ,(o-formula (gv-parent :draw-function)))
         (:visible ,(o-formula (and (gv-parent :visible)
                                    (> (gvl :width) 0))))))))


;;;
;;; Lists of labels
;;;
;;; TO-DO: make lists of labels virtual aggregadgets


(create-instance 'LIST-OF-LABELS opal:aggrelist
   :declare (:type (points :items)
                   ((or list integer) :size)
                   (draw-function-list :draw-function)
                   (filling-style-list :filling-style)
                   (line-style-list :line-style))
   (:direction nil)
   (:items (o-formula (gv-parent :points)))
   (:size  (o-formula (gv-parent :size)))
   (:draw-function (o-formula (gv-parent :draw-function)))
   (:filling-style (o-formula (gv-parent :filling-style)))
   (:line-style    (o-formula (gv-parent :line-style))))


(create-instance 'V-LINES list-of-labels
   (:item-prototype `(,vert-line)))


(create-instance 'H-LINES list-of-labels
   (:item-prototype `(,horiz-line)))


(create-instance 'TICS list-of-labels
   :declare (:type ((or list integer) :offset)
                   (font :font)
                   ((or list string) :string))
   (:offset (o-formula (gv-parent :labels-offset)))
   (:font   (o-formula (gv-parent :labels-font)))
   (:string (o-formula (gv-parent :labels))))


(create-instance 'H-TICS tics
   (:item-prototype `(,horiz-tic)))


(create-instance 'V-TICS tics
   (:item-prototype `(,vert-tic)))


(create-instance 'V-SDEVS list-of-labels
   :declare (:type ((or list integer) :upper-bound :lower-bound :bar-size))
   (:upper-bound (o-formula (gv-parent :upper-bound)))
   (:lower-bound (o-formula (gv-parent :lower-bound)))
   (:bar-size    (o-formula (gv-parent :bar-size)))
   (:item-prototype `(,vert-sdev)))


(create-instance 'V-METERS list-of-labels
   :declare (:type (points :zero-points))
   (:zero-points (o-formula (gv-parent :zero-points)))
   (:item-prototype `(,vert-meter)))


;;;
;;; Data objects
;;;


(create-instance 'DATA-OBJECT opal:aggregadget
   :declare (:type (kr-boolean :data-p)
                   ((or null string) :description)
                   ((or list (satisfies schema-p)) :plot)
                   (list :xs :ys :points)
                   (integer :x-offset :y-offset)
                   (draw-function-list :draw-function)
                   (filling-style-list :filling-style)
                   (line-style-list :line-style))
   (:data-p t)
   (:description nil)
   (:plot (o-formula (gv-parent :plot)))
   (:xs nil)
   (:ys nil)
   (:x-offset 0)
   (:y-offset 0)
   (:points (o-formula (xy-points :plot (gvl :plot)
                                  :xs (gvl :xs)
                                  :ys (gvl :ys)
                                  :x-offset (gvl :x-offset)
                                  :y-offset (gvl :y-offset))))
   (:line-style black-line)
   (:filling-style opal:black-fill)
   (:draw-function :copy))


(create-instance 'DATA-LINE data-object
   (:parts
    `((:line ,opal:polyline
         (:point-list ,(o-formula (point-list (gv-parent :points))))
         (:line-style ,(o-formula (gv-parent :line-style)))))))


(create-instance 'DATA-POINTS data-object
   :declare (:type ((or list (satisfies schema-p)) :bullet)
                   ((or list integer) :bullet-size :size))
   (:bullet bullet)
   (:bullet-size 5)
   (:size (o-formula (if (numberp (gvl :bullet-size)) (gvl :bullet-size) 5)))
   (:parts
    `((:bullets ,list-of-labels
         (:item-prototype ,(o-formula (gv-parent :bullet)))))))


(create-instance 'DATA-WAVE data-object
   :declare (:type (filling-style :filling-style)
                   ((or number null) :zero-x :zero-y)
                   (list :zero-xs :zero-ys)
                   (points :zero-points :wave-points))
   (:filling-style opal:motif-gray-fill)
   (:zero-x nil)
   (:zero-y nil)
   (:zero-xs (o-formula (if (gvl :zero-x) (list (gvl :zero-x) (gvl :zero-x))
                          (list (car (gvl :xs)) (car (reverse (gvl :xs)))))))
   (:zero-ys (o-formula (if (gvl :zero-y) (list (gvl :zero-y) (gvl :zero-y))
                          (list (car (gvl :ys)) (car (reverse (gvl :ys)))))))
   (:zero-points (o-formula (xy-points :plot (gvl :plot)
                                       :xs (gvl :zero-xs)
                                       :ys (gvl :zero-ys)
                                       :x-offset (gvl :x-offset)
                                       :y-offset (gvl :y-offset))))
   (:wave-points (o-formula (append (list (car (gvl :zero-points)))
                                    (gvl :points)
                                    (cdr (gvl :zero-points)))))
   (:parts
    `((:wave ,opal:polyline
         (:point-list ,(o-formula (point-list (gv-parent :wave-points))))
         (:line-style nil)
         (:draw-function ,(o-formula (gv-parent :draw-function)))
         (:filling-style ,(o-formula (gv-parent :filling-style))))
      (:line ,opal:polyline
         (:point-list ,(o-formula (point-list (gv-parent :points))))
         (:line-style ,(o-formula (gv-parent :line-style)))
         (:filling-style nil)))))


(create-instance 'Y-DISTR data-object
   :declare (:type (line-style :sd-style :bullet-style)
                   ((or list (satisfies schema-p)) :bullet)
                   ((or list integer) :sds :sd-size :bullet-size :size)
                   (integer :bar-size))
   (:sd-style (o-formula (gvl :line-style)))
   (:bullet-style (o-formula (gvl :sd-style)))
   (:sds nil)
   (:bar-size 2)
   (:bullet bullet)
   (:bullet-size 5)
   ;(:size (o-formula (if (numberp (gvl :bullet-size)) (gvl :bullet-size) 5)))
   (:size
    (o-formula
     (loop for sd in (gvl :sds)
           and y  in (gvl :ys) collect
           (abs (delta-y :plot (gvl :plot) :y1 y :y2 (+ y sd))))))
   (:upper-bound
    (o-formula
     (loop for sd in (gvl :sds)
           and y  in (gvl :ys) collect
           (+ (delta-y :plot (gvl :plot) :y1 y :y2 (gvl :plot :y-to))
              (gvl :y-offset)))))
   (:lower-bound
    (o-formula
     (loop for sd in (gvl :sds)
           and y  in (gvl :ys) collect
           (+ (delta-y :plot (gvl :plot) :y1 y :y2 (gvl :plot :y-from))
              (gvl :y-offset)))))
   (:parts
    `((:line ,opal:polyline
         (:point-list ,(o-formula (point-list (gv-parent :points))))
         (:line-style ,(o-formula (gv-parent :line-style)))
         (:filling-style nil))
      (:sdevs ,v-sdevs
         (:line-style ,(o-formula (gv-parent :sd-style))))
      (:bullets ,list-of-labels
         (:item-prototype ,(o-formula (gv-parent :bullet)))
         (:size ,(o-formula (gv-parent :bullet-size)))
         (:line-style ,(o-formula (gv-parent :bullet-style)))))))


(create-instance 'HISTOGRAM data-object
   :declare (:type ((or number null) :zero-x :zero-y)
                   (list :zero-xs :zero-ys)
                   (points :zero-points))
   (:filling-style opal:motif-gray-fill)
   (:size (o-formula (nth-width)))
   (:x-offset (o-formula (round (nth-width) 2) 0))
   (:zero-x nil)
   (:zero-y nil)
   (:zero-xs (o-formula
              (if (gvl :zero-x)
                  (loop for x in (gvl :xs) collect (gvl :zero-x)) (gvl :xs))))
   (:zero-ys (o-formula
              (if (gvl :zero-y)
                  (loop for y in (gvl :ys) collect (gvl :zero-y)) (gvl :ys))))
   (:zero-points (o-formula (xy-points :plot (gvl :plot)
                                       :xs (gvl :zero-xs)
                                       :ys (gvl :zero-ys)
                                       :x-offset (gvl :x-offset)
                                       :y-offset (gvl :y-offset))))
   (:parts
    `((:histogram ,v-meters))))


(create-instance 'FUNCTION-LINE data-line
   :declare (:type (kr-boolean :function-p :data-p)
                   ((or function null) :function :domain)
                   (list :xs :ys)
                   (integer :precision :n-of-points))
   (:function-p t)
   (:data-p nil)
   (:function nil)
   (:domain nil)
   (:precision 1)
   (:n-of-points (o-formula
                  (/ (gvl :plot :canvas :width)
                     (if (> (gvl :precision) 0) (gvl :precision) 1))))
   (:xs (o-formula (x-list (gvl :plot :x-from)
                           (gvl :plot :x-to)
                           (gvl :n-of-points)
                           :test (gvl :domain))))
   (:ys (o-formula (y-list (gvl :function) (gvl :xs)))))


(create-instance 'FUNCTION-WAVE data-wave
   :declare (:type (kr-boolean :function-p :data-p)
                   ((or function null) :function :domain)
                   (list :xs :ys)
                   (integer :precision :n-of-points))
   (:function-p t)
   (:data-p nil)
   (:function nil)
   (:domain nil)
   (:precision 1)
   (:n-of-points (o-formula
                  (/ (gvl :plot :canvas :width)
                     (if (> (gvl :precision) 0) (gvl :precision) 1))))
   (:xs (o-formula (x-list (gvl :plot :x-from)
                           (gvl :plot :x-to)
                           (gvl :n-of-points)
                           :test (gvl :domain))))
   (:ys (o-formula (y-list (gvl :function) (gvl :xs)))))


;;;
;;; Plot components
;;;

;;; Plot canvas and background


(create-instance 'BACKGROUND opal:rectangle
   (:left   (o-formula (gv-parent :left)))
   (:top    (o-formula (gv-parent :top)))
   (:width  (o-formula (gv-parent :width)))
   (:height (o-formula (gv-parent :height)))
   (:filling-style (o-formula (gv-parent :filling-style)))
   (:line-style (o-formula (gv-parent :boarder-style))))


(create-instance 'CANVAS opal:rectangle
   (:left   (o-formula (+ (gv-parent :left) (gv-parent :left-margin)
                          (gv-parent :x-axis-offset))))
   (:top    (o-formula (+ (gv-parent :top)  (gv-parent :top-margin))))
   (:width  (o-formula (let ((w (gv-parent :width))
                             (m (+ (gv-parent :left-margin)
                                   (gv-parent :right-margin)
                                   (gv-parent :x-axis-offset))))
                         (if (> w m) (- w m) 0))))
   (:height (o-formula (let ((h (gv-parent :height))
                             (m (+ (gv-parent :top-margin)
                                   (gv-parent :bottom-margin)
                                   (gv-parent :y-axis-offset))))
                         (if (> h m) (- h m) 0))))
   (:filling-style (o-formula (gv-parent :canvas-filling-style)))
   (:line-style (o-formula (gv-parent :canvas-boarder-style))))


;;; Plot title


(create-instance 'TITLE opal:text
   (:justification :left)
   (:string (o-formula (gv-parent :title)))
   (:left   (o-formula (opal:gv-center-x-is-center-of
                         (gvl :parent))))
   (:top    (o-formula (+ (gv-parent :top) 5)))
   (:font   (o-formula (gv-parent :title-font)))
   (:line-style (o-formula (gv-parent :title-style))))


;;; Axis parts


(create-instance 'X-LINE data-object
   (:xs (o-formula (list (gvl :plot :x-from))))
   (:ys (o-formula (list (gvl :plot :y-from))))
   (:y-offset (o-formula (- (gvl :plot :x-axis-offset)) 0))
   (:line-style (o-formula (gvl :plot :x-axis-style)))
   (:size (o-formula (delta-x :plot (gvl :plot)
                              :x1 (gvl :plot :x-from)
                              :x2 (gvl :plot :x-to))))
   (:parts `((:axis ,h-lines))))


(create-instance 'Y-LINE data-object
   (:xs (o-formula (list (gvl :plot :x-from))))
   (:ys (o-formula (list (gvl :plot :y-from))))
   (:x-offset (o-formula (- (gvl :plot :y-axis-offset)) 0))
   (:line-style (o-formula (gvl :plot :x-axis-style)))
   (:size (o-formula (delta-y :plot (gvl :plot)
                              :y1 (gvl :plot :y-from)
                              :y2 (gvl :plot :y-to))))
   (:parts `((:axis ,v-lines))))


(create-instance 'X-MARKS data-object
   (:xs   (o-formula (gvl :plot :x-tic-values)))
   (:ys   (o-formula (loop for x in (gvl :xs) collect (gvl :plot :y-from))))
   (:y-offset      (o-formula (- (gvl :plot :y-axis-offset)) 0))
   (:size          (o-formula (gvl :plot :x-tic-size)))
   (:labels-offset (o-formula (gvl :plot :x-labels-offset)))
   (:labels-font   (o-formula (gvl :plot :x-labels-font)))
   (:line-style    (o-formula (gvl :plot :x-axis-style)))
   (:labels        (o-formula (gvl :plot :x-labels)))
   (:parts `((:tics ,h-tics))))


(create-instance 'Y-MARKS data-object
   (:xs   (o-formula (loop for y in (gvl :ys) collect (gvl :plot :x-from))))
   (:ys   (o-formula (gvl :plot :y-tic-values)))
   (:x-offset      (o-formula (- (gvl :plot :x-axis-offset)) 0))
   (:size          (o-formula (gvl :plot :y-tic-size)))
   (:labels-offset (o-formula (gvl :plot :y-labels-offset)))
   (:labels-font   (o-formula (gvl :plot :y-labels-font)))
   (:line-style    (o-formula (gvl :plot :y-axis-style)))
   (:labels        (o-formula (gvl :plot :y-labels)))
   (:parts `((:tics ,v-tics))))


;;; Axis


(create-instance 'X-AXIS opal:aggregadget
   :declare (:type ((or list (satisfies schema-p)) :plot))
   (:plot  (o-formula (gvl :parent)))
   (:parts
    `((:axis ,x-line)
      (:tic-marks ,x-marks))))


(create-instance 'Y-AXIS opal:aggregadget
   :declare (:type ((or list (satisfies schema-p)) :plot))
   (:plot  (o-formula (gvl :parent)))
   (:text  (o-formula (gvl :plot :y-text)))
   (:parts
    `((:axis ,y-line)
      (:tic-marks ,y-marks))))


;;; Axis text


(create-instance 'X-TEXT opal:text
   (:justification :center)
   (:left (o-formula (opal:gv-center-x-is-center-of
                      (gv-parent :canvas))))
   (:top  (o-formula (+ (opal:gv-bottom (gv-parent :canvas))
                        (gv-parent :x-axis-offset)
                        (gvl :parent :x-axis :height)
                        (if (> (gv-parent :x-tic-size) 0)
                            (- (gv-parent :x-tic-size)) 0))))
   (:font (o-formula (gv-parent :x-text-font)))
   (:string (o-formula (gv-parent :x-string))))


(create-instance 'Y-TEXT opal:text
   (:justification :center)
   (:left (o-formula (- (gvl :parent :canvas :left)
                        (gv-parent :y-axis-offset)
                        (floor (gvl :width) 2))))
   (:top  (o-formula (- (gvl :parent :canvas :top)
                        (gvl :height) 10)))
   (:font (o-formula (gv-parent :y-text-font)))
   (:string (o-formula (gv-parent :y-string))))


;;; Grid lines


(create-instance 'X-GRID data-object
   :declare (:type ((or list (satisfies schema-p)) :plot))
   (:plot (o-formula (gvl :parent)))
   (:xs (o-formula (gvl :plot :x-grid-values)))
   (:ys (o-formula (loop for x in (gvl :xs) collect (gvl :plot :y-from))))
   (:line-style (o-formula (gvl :plot :x-grid-style)))
   (:size (o-formula (delta-y :plot (gvl :plot)
                              :y1 (gvl :plot :y-from)
                              :y2 (gvl :plot :y-to))))
   (:visible (o-formula (gvl :plot :x-grid)))
   (:parts `((:grid ,v-lines))))



(create-instance 'Y-GRID data-object
   :declare (:type ((or list (satisfies schema-p)) :plot))
   (:plot (o-formula (gvl :parent)))
   (:xs (o-formula (loop for y in (gvl :ys) collect (gvl :plot :x-from))))
   (:ys (o-formula (gvl :plot :y-grid-values)))
   (:line-style (o-formula (gvl :plot :y-grid-style)))
   (:size (o-formula (delta-x :plot (gvl :plot)
                              :x1 (gvl :plot :x-from)
                              :x2 (gvl :plot :x-to))))
   (:visible (o-formula (gvl :plot :y-grid)))
   (:parts `((:grid ,h-lines))))


;;;
;;; Data objects description
;;;

(defmacro object-property (object property)
  `(if (gvl :parent :data-object ,object)
       (gvl :parent :data-object ,object ,property)
     (case ,property
       (:line-style black-line)
       (:filling-style nil)
       (:draw-function :copy)
       (:visible nil)
       (t nil))))


(create-instance 'DESCRIPTION-TEXT opal:text
   (:left (o-formula (+ (gv-parent :left)
                        (gv-parent :example-width)
                        (gv-parent :margin))))
   (:top (o-formula (gv-parent :top) 0))
   (:string (o-formula (gvl :parent :data-object :description))))


(create-instance 'DESCRIPTION-LINE opal:line
   (:x1 (o-formula (gv-parent :left) 0))
   (:x2 (o-formula (+ (gvl :x1) (gv-parent :example-width))))
   (:y1 (o-formula (+ (gv-parent :top)
                      (floor (gvl :parent :text :height) 2)) 0))
   (:y2 (o-formula (+ (gv-parent :top)
                      (floor (gvl :parent :text :height) 2)) 0))
   (:line-style    (o-formula (object-property :line :line-style)))
   (:draw-function (o-formula (object-property :line :draw-function)))
   (:visible       (o-formula (and (gv-parent :visible)
                                   (object-property :line :visible)))))


(create-instance 'DESCRIPTION-WAVE opal:rectangle
   (:left   (o-formula (gv-parent :left) 0))
   (:width  (o-formula (gv-parent :example-width)))
   (:top    (o-formula (+ (gv-parent :top)
                          (floor (gvl :parent :text :height) 2)) 0))
   (:height (o-formula (floor (gvl :parent :text :height) 3) 0))
   (:line-style    (o-formula (object-property :wave :line-style)))
   (:filling-style (o-formula (object-property :wave :filling-style)))
   (:draw-function (o-formula (object-property :wave :draw-function)))
   (:visible       (o-formula (and (gv-parent :visible)
                                   (object-property :wave :visible)))))


(create-instance 'DESCRIPTION-BULLETS list-of-labels
   (:items
    (o-formula
     (let* ((x0 (gv-parent :left))
            (w  (gv-parent :example-width))
            (s  (if (gvl :size) (gvl :size) 0))
            (x1 (+ x0 (floor s 2)))
            (x2 (+ x0 (floor w 2)))
            (x3 (+ x0 w (- (floor s 2))))
            (y  (+ (gv-parent :top)
                   (floor (gvl :parent :text :height) 2))))
       (loop for x in (list x1 x2 x3) collect (make-point :x x :y y)))))
   (:item-prototype (o-formula (object-property :bullets :item-prototype)))
   (:size (o-formula (object-property :bullets :size)))
   (:line-style    (o-formula (object-property :bullets :line-style)))
   (:filling-style (o-formula (object-property :bullets :filling-style)))
   (:draw-function (o-formula (object-property :bullets :draw-function)))
   (:visible       (o-formula (and (gv-parent :visible)
                                   (object-property :bullets :visible)))))


(create-instance 'DESCRIPTION-HISTOGRAM opal:rectangle
   (:left   (o-formula (gv-parent :left) 0))
   (:width  (o-formula (gv-parent :example-width)))
   (:top    (o-formula (+ (gv-parent :top)
                          (floor (gvl :parent :text :height) 2)) 0))
   (:height (o-formula (floor (gvl :parent :text :height) 3) 0))
   (:line-style    (o-formula (object-property :histogram :line-style)))
   (:filling-style (o-formula (object-property :histogram :filling-style)))
   (:draw-function (o-formula (object-property :histogram :draw-function)))
   (:visible       (o-formula (and (gv-parent :visible)
                                   (object-property :histogram :visible)))))


(create-instance 'DESCRIPTION-ITEM opal:aggregadget
   :declare (:type ((or list (satisfies schema-p)) :data-object)
                   (font :font)
                   (integer :margin :example-width))
   (:data-object nil)
   (:example-width (o-formula (gv-parent :example-width) 40))
   (:margin (o-formula (gv-parent :margin) 10))
   (:font opal:default-font)
   (:parts
    `((:text ,description-text
        (:font ,(o-formula (gv-parent :font))))
      (:histogram ,description-histogram)
      (:wave ,description-wave)
      (:line ,description-line)
      (:bullets ,description-bullets) )))


(create-instance 'DESCRIPTION-LIST opal:aggrelist
   :declare (:type (integer :margin :example-width)
                   (font :font))
   (:left (o-formula (+ (gv-parent :left) (gvl :margin)) 0))
   (:top  (o-formula (+ (gv-parent :top)  (gvl :margin)) 0))
   (:items (o-formula (gv-parent :items)))
   (:example-width (o-formula (gv-parent :example-width)))
   (:margin (o-formula (gv-parent :margin)))
   (:v-spacing (o-formula (gv-parent :v-spacing) 0))
   (:font opal:default-font)
   (:item-prototype
    `(,description-item
      (:data-object ,(o-formula (nth (gvl :rank) (gv-parent :items))))
      (:font ,(o-formula (gv-parent :font))))))


(create-instance 'DESCRIPTION opal:aggregadget
   :declare (:type ((or list (satisfies schema-p)) :plot)
                   ((or list (satisfies schema-p)) :items)
                   (font :font)
                   (integer :v-spacing :margin :example-width)
                   (line-style-or-nil :line-style)
                   (filling-style-or-nil :filling-style))
   (:plot (o-formula (gvl :parent)))
   (:left (o-formula (- (opal:gv-right (gvl :plot :canvas))
                        (gvl :width) (gvl :margin)) 0))
   (:top  (o-formula (+ (gvl :plot :canvas :top) (gvl :margin))))
   (:items nil)
   (:v-spacing (o-formula (gvl :plot :description-spacing) 0))
   (:example-width (o-formula (gvl :plot :example-width) 30))
   (:margin (o-formula (gvl :plot :description-margin) 5))
   (:line-style (o-formula (gvl :plot :description-boarder-style)))
   (:filling-style (o-formula (gvl :plot :description-filling-style)))
   (:font (o-formula (gvl :plot :description-font)))
   (:visible (o-formula (and (gvl :plot :description :items))))
   (:parts
    `((:background ,opal:rectangle
         (:left ,(o-formula (gv-parent :left)))
         (:top  ,(o-formula (gv-parent :top)))
         (:width  ,(o-formula (+ (gvl :parent :list :width)
                                 (* 2 (gv-parent :margin)))))
         (:height ,(o-formula (+ (gvl :parent :list :height)
                                 (* 2 (gv-parent :margin)))))
         (:line-style ,(o-formula (gv-parent :line-style)))
         (:filling-style ,(o-formula (gv-parent :filling-style))))
      (:list ,description-list
         (:font ,(o-formula (gv-parent :font)))) )))


;;;
;;; Plot prototype
;;;

(create-instance 'PLOT opal:aggregadget
   ;;; Main frame
   (:left 0) (:top 0)
   (:width  (o-formula (gvl :window :width)))
   (:height (o-formula (gvl :window :height)))
   (:filling-style nil)
   (:boarder-style nil)
   ;;; Canvas
   (:margins 40)
   (:top-margin    (o-formula (gvl :margins)))
   (:bottom-margin (o-formula (gvl :margins)))
   (:left-margin   (o-formula (gvl :margins)))
   (:right-margin  (o-formula (gvl :margins)))
   (:canvas-filling-style nil)
   (:canvas-boarder-style nil)
   ;;; Plot titile text
   (:title "")
   (:title-font (opal:get-standard-font :serif :roman :large))
   (:title-style black-line)
   ;;; Axis
   (:axis-offset 0)
   (:x-axis-offset (o-formula (gvl :axis-offset)))
   (:y-axis-offset (o-formula (gvl :axis-offset)))
   (:axis-style black-line)
   (:x-axis-style (o-formula (gvl :axis-style)))
   (:y-axis-style (o-formula (gvl :axis-style)))
   ;;; Axis Range
   (:x-from 0)
   (:x-to 1)
   (:y-from 0)
   (:y-to 1)
   ;;; Tic marks
   (:tic-size 0)
   (:x-tic-size (o-formula (gvl :tic-size)))
   (:y-tic-size (o-formula (gvl :tic-size)))
   (:labels-offset 5)
   (:x-labels-offset (o-formula (gvl :labels-offset)))
   (:y-labels-offset (o-formula (gvl :labels-offset)))
   (:labels-font (opal:get-standard-font :sans-serif nil :small))
   (:x-labels-font (o-formula (gvl :labels-font)))
   (:y-labels-font (o-formula (gvl :labels-font)))
   (:x-tic-values nil)
   (:x-labels nil)
   (:y-tic-values nil)
   (:y-labels nil)
   ;;; Axis text
   (:x-string "")
   (:y-string "")
   (:text-font (opal:get-standard-font :sans-serif :bold :medium))
   (:x-text-font (o-formula (gvl :text-font)))
   (:y-text-font (o-formula (gvl :text-font)))
   ;;; Grid
   (:grid nil)
   (:x-grid (o-formula (gvl :grid)))
   (:y-grid (o-formula (gvl :grid)))
   (:grid-style opal:dotted-line)
   (:x-grid-style (o-formula (gvl :grid-style)))
   (:y-grid-style (o-formula (gvl :grid-style)))
   (:x-grid-values (o-formula (gvl :x-tic-values)))
   (:y-grid-values (o-formula (gvl :y-tic-values)))
   ;;; Data description
   (:description-font (o-formula (gvl :text-font)))
   (:description-boarder-style (o-formula (gvl :canvas-boarder-style)))
   (:description-filling-style (o-formula (gvl :canvas-filling-style)))
   (:description-spacing 0)
   (:description-margin 5)
   (:example-width 30)
   ;;; Parts
   (:parts
    `((:background ,background)
      (:canvas ,canvas)
      (:plot-title ,title)
      (:x-axis ,x-axis)
      (:y-axis ,y-axis)
      (:x-text ,x-text)
      (:y-text ,y-text)
      (:x-grid-lines ,x-grid)
      (:y-grid-lines ,y-grid)
      (:description ,description)
      ))
   (:interactors
    `((:move ,inter:move-grow-interactor
         (:window ,(o-formula (gv-local :self :operates-on :window)))
         (:start-where ,(o-formula
                         (list :in (gvl :operates-on :description))))
         (:slots-to-set ,(o-formula (list t t nil nil))) ))))

;;; Functions to parse data for plotting

(defmacro matrix-to-list (A)
  "Returns list of elements of ith row"
  (declare (type (simple-array real (* *)) A) (values list))
  `(loop with m = (array-dimension ,A 0)
         for i of-type fixnum below m collect
         (loop with n = (array-dimension ,A 1)
               for j of-type fixnum below n collect (aref ,A i j))))

(defmacro parse-data (A)
  "Returns list of lists from a matrix, a list or a list of lists"
  `(cond ((arrayp ,A) (matrix-to-list ,A))
         ((and (listp ,A) (listp (car ,A))) ,A)
         ((listp ,A) (list ,A))
         (t (list (list ,A)))))

(defun extend-list (list m)
  "Extends list to length of m"
  (declare (fixnum m) (values list))
  (let* ((l (if (listp list) list (list list)))
         (n (length l)))
    (and l (loop for i below m collect (nth (mod i n) l)))))

(defmacro make-xs (xs ys)
  "Generates a list of xss accordingly"
  `(let* ((m (length ,ys))
          (ns (loop for i below m collect (length (nth i ,ys)))))
     (and ,ys (extend-list
               (if ,xs (parse-data ,xs)
                 (loop for i below m collect
                       (loop for j below (nth i ns) collect j))) m))))

(defun get-min (list)
  "Returns smallest element"
  (declare (type (or list number) list) (values number))
  (cond ((null list) 0)
        ((numberp list) list)
        (t (loop for l in list minimize (get-min l)))))

(defun get-max (list)
  "Returns largest element"
  (declare (type (or list number) list) (values number))
  (cond ((null list) 1)
        ((numberp list) list)
        (t (loop for l in list maximize (get-max l)))))

(defun collect-n-mins (list n)
  (declare (list list) (fixnum n) (values list))
  (loop for i below n collect
        (loop with m = (length list)
              for j below m when (= (mod j n) i) collect (nth j list) into nls
              finally (return (floor (get-min nls))))))

(defun collect-n-maxs (list n)
  (declare (list list) (fixnum n) (values list))
  (loop for i below n collect
        (loop with m = (length list)
              for j below m when (= (mod j n) i) collect (nth j list) into nls
              finally (return (ceiling (get-max nls))))))

(defun make-froms (from list normalise n)
  (declare (type (or null number list) list) (symbol normalise)
           (fixnum n) (values list))
  (cond (from (extend-list from n))
        ((and list (> (length list) 1) normalise) (collect-n-mins list n))
        ((and list (> (length list) 1)) (extend-list (floor (get-min list)) n))
        (t (extend-list 0 n))))

(defun make-tos (to list normalise n)
  (declare (type (or null number list) list) (symbol normalise)
           (fixnum n) (values list))
  (cond (to (extend-list to n))
        ((and list (> (length list) 1) normalise) (collect-n-maxs list n))
        ((and list (> (length list) 1)) (extend-list (ceiling (get-max list)) n))
        (t (extend-list 1 n))))

(defmacro make-line-styles (ls ps)
  `(loop with n = (length ,ls) for i below n collect
         (create-instance NIL (nth i ,ls)
           (:line-style (if (nth i ,ps) :dash :solid))
           (:dash-pattern (nth i ,ps)))))

;;; Plotting functions

(defun build (&key ;;;Data
              data xs sds (data-object data-line) (bullet bullet) (bullet-size 5)
              data-description
              (data-line-style black-line) data-line-pattern
              (data-filling-style opal:motif-gray-fill)
              (data-x-offset 0) (data-y-offset 0)
              data-zero-y (data-draw-function :copy)
              ;;; Functions
              function domain (fn-object function-line) fn-description
              (fn-line-style black-line) fn-line-pattern
              (fn-filling-style opal:motif-gray-fill)
              (fn-x-offset 0) (fn-y-offset 0)
              fn-zero-y (fn-draw-function :copy)
              ;;; Window
              window (title "") (left 600) (top 400) (width 350) (height 200)
              (title-style black-line)
              ;;; Plots
              (n-of-plots 1) (filling-style opal:white-fill)
              (canvas-filling-style filling-style) canvas-boarder-style
              (description-boarder-style canvas-boarder-style)
              (description-filling-style canvas-filling-style)
              ;;; Axis
              normalise-x normalise-y
              (x-string "X") (y-string "Y") (axis-style black-line) (axis-offset 10)
              x-from x-to y-from y-to (x-tic-by 1) (y-tic-by 1) (tic-size -3)
              x-labels y-labels (x-label-by x-tic-by) (y-label-by y-tic-by)
              ;;; Grid
              x-grid y-grid (grid-style opal:dotted-line)
              (x-grid-by x-tic-by) (y-grid-by y-tic-by))
  "Opens a window with one or several plots, data objects or functions"
  (let* ((ys (parse-data data))
         (m (length ys))
         (xs (make-xs xs ys))
         (sds (extend-list (parse-data sds) m))
         (data-objects (extend-list data-object m))
         (bullets (extend-list bullet m))
         (bullet-sizes (extend-list bullet-size m))
         (data-descriptions (extend-list data-description m))
         (data-x-offsets (extend-list data-x-offset m))
         (data-y-offsets (extend-list data-y-offset m))
         (data-zero-ys (extend-list (if data-zero-y data-zero-y y-from) m))
         (data-draw-functions (extend-list data-draw-function m))
         (data-filling-styles (extend-list data-filling-style m))
         (data-line-styles (make-line-styles (extend-list data-line-style m)
                                             (extend-list data-line-pattern m)))
         (functions (if (listp function) function (list function)))
         (k (length functions))
         (domains (extend-list domain k))
         (fn-objects (extend-list fn-object k))
         (fn-descriptions (extend-list fn-description k))
         (fn-x-offsets (extend-list fn-x-offset k))
         (fn-y-offsets (extend-list fn-y-offset k))
         (fn-zero-ys (extend-list (if fn-zero-y fn-zero-y y-from) k))
         (fn-draw-functions (extend-list fn-draw-function k))
         (fn-filling-styles (extend-list fn-filling-style k))
         (fn-line-styles (make-line-styles (extend-list fn-line-style k)
                                           (extend-list fn-line-pattern k)))
         (titles  (extend-list title n-of-plots))
         (canvas-filling-styles (extend-list canvas-filling-style n-of-plots))
         (canvas-boarder-styles (extend-list canvas-boarder-style n-of-plots))
         (description-boarder-styles (extend-list description-boarder-style n-of-plots))
         (description-filling-styles (extend-list description-filling-style n-of-plots))
         (x-froms (make-froms x-from xs normalise-x n-of-plots))
         (x-tos   (make-tos x-to xs normalise-x n-of-plots))
         (y-froms (make-froms y-from ys normalise-y n-of-plots))
         (y-tos   (make-tos y-to ys normalise-y n-of-plots))
         (x-strings (extend-list x-string n-of-plots))
         (y-strings (extend-list y-string n-of-plots))
         (x-tic-bys (extend-list x-tic-by n-of-plots))
         (y-tic-bys (extend-list y-tic-by n-of-plots))
         (x-labelss (extend-list (parse-data x-labels) n-of-plots))
         (y-labelss (extend-list (parse-data y-labels) n-of-plots))
         (x-label-bys (extend-list x-label-by n-of-plots))
         (y-label-bys (extend-list y-label-by n-of-plots))
         (x-grids (extend-list x-grid n-of-plots))
         (y-grids (extend-list y-grid n-of-plots))
         (x-grid-bys (extend-list x-grid-by n-of-plots))
         (y-grid-bys (extend-list y-grid-by n-of-plots))
         (win
          (create-instance window inter:interactor-window
            (:title (format nil "~S" window))
            (:left left) (:top top) (:width width) (:height height)
            (:aggregate (create-instance NIL opal:aggregate))))
         (ps
          (loop for i below n-of-plots collect
                (create-instance NIL plot
                  (:rank i)
                  (:height (o-formula (floor (gvl :window :height) n-of-plots)))
                  (:top (o-formula (* (gvl :rank) (gvl :height))))
                  (:title (nth i titles))
                  (:title-style title-style)
                  (:filling-style filling-style)
                  (:canvas-filling-style (nth i canvas-filling-styles))
                  (:canvas-boarder-style (nth i canvas-boarder-styles))
                  (:description-boarder-style (nth i description-boarder-styles))
                  (:description-filling-style (nth i description-filling-styles))
                  (:x-string (nth i x-strings)) (:y-string (nth i y-strings))
                  (:x-from (nth i x-froms)) (:x-to (nth i x-tos))
                  (:y-from (nth i y-froms)) (:y-to (nth i y-tos))
                  (:tic-size tic-size)
                  (:axis-offset axis-offset)
                  (:axis-style axis-style)
                  (:x-tic-values
                   (loop with min = (nth i x-froms)
                         with max = (nth i x-tos)
                         with step = (nth i x-tic-bys)
                         for x from min to max by step collect x))
                  (:y-tic-values
                   (loop with min = (nth i y-froms)
                         with max = (nth i y-tos)
                         with step = (nth i y-tic-bys)
                         for y from min to max by step collect y))
                  (:x-labels
                   (loop with min = (nth i x-froms)
                         with max = (nth i x-tos)
                         with step = (nth i x-tic-bys)
                         with labels = (nth i x-labelss)
                         with labelstep = (nth i x-label-bys)
                         for x from min to max by step count x into j collect
                         (cond ((= labelstep 0) "")
                               ((and (> (mod (- x min) labelstep) 0)
                                     (null (nth (1- j) labels))) "")
                               ((nth (1- j) labels) (nth (1- j) labels))
                               (t (format nil "~A" x)))))
                  (:y-labels
                   (loop with min = (nth i y-froms)
                         with max = (nth i y-tos)
                         with step = (nth i y-tic-bys)
                         with labels = (nth i y-labelss)
                         with labelstep = (nth i y-label-bys)
                         for y from min to max by step count y into j collect
                         (cond ((= labelstep 0) "")
                               ((and (> (mod (- y min) labelstep) 0)
                                     (null (nth (1- j) labels))) "")
                               ((nth (1- j) labels) (nth (1- j) labels))
                               (t (format nil "~A" y)))))
                  (:x-grid (nth i x-grids)) (:y-grid (nth i y-grids))
                  (:grid-style grid-style)
                  (:x-grid-values
                   (o-formula (loop for x from (gvl :x-from) to (gvl :x-to)
                                    by (nth i x-grid-bys) collect x)))
                  (:y-grid-values
                   (o-formula (loop for y from (gvl :y-from) to (gvl :y-to)
                                    by (nth i y-grid-bys) collect y))))))
         (ds
          (loop for i below m collect
                (create-instance NIL (nth i data-objects)
                  (:xs (nth i xs)) (:ys (nth i ys)) (:sds (nth i sds))
                  (:x-offset (nth i data-x-offsets)) (:y-offset (nth i data-y-offsets))
                  (:bullet (nth i bullets))
                  (:bullet-size (nth i bullet-sizes))
                  (:zero-y (nth i data-zero-ys))
                  (:draw-function (nth i data-draw-functions))
                  (:line-style (nth i data-line-styles))
                  (:filling-style (nth i data-filling-styles))
                  (:description (nth i data-descriptions))
                  (:plot (nth (mod i n-of-plots) ps)))))
         (fs
          (loop for i below k collect
                (create-instance NIL (nth i fn-objects)
                  (:function (nth i functions)) (:domain (nth i domains))
                  (:x-offset (nth i fn-x-offsets)) (:y-offset (nth i fn-y-offsets))
                  (:zero-y (nth i fn-zero-ys))
                  (:draw-function (nth i fn-draw-functions))
                  (:line-style (nth i fn-line-styles))
                  (:filling-style (nth i fn-filling-styles))
                  (:description (nth i fn-descriptions))
                  (:plot (nth (mod i n-of-plots) ps))))))
    (loop for p in ps do
          (opal:add-component (gv win :aggregate) p))
    (loop for d in ds do
          (opal:add-component (gv win :aggregate) d)
          (and (gv d :description)
               (s-value (gv d :plot) :description :items
                        (append (gv d :plot :description :items) (list d)))))
    (loop for f in fs do
          (opal:add-component (gv win :aggregate) f)
          (and (gv f :description)
               (s-value (gv f :plot) :description :items
                        (append (gv f :plot :description :items) (list f)))))
    (opal:update win)))


(defun update (&key window data xs sds function domain nth)
  "Updates data objects or functions in specified window"
  (let* ((ys (parse-data data))
         (m (length ys))
         (xs (extend-list (parse-data xs) m))
         (sds (extend-list (parse-data sds) m))
         (functions (if (listp function) function (list function)))
         (k (length functions))
         (domains (extend-list domain k))
         (comps (gv window :aggregate :components))
         (ds (loop for c in comps when (gv c :data-p) collect c))
         (fs (loop for c in comps when (gv c :function-p) collect c)))
    (cond ((and nth (or ys xs sds) (nth nth ds))
           (and ys  (s-value (nth nth ds) :ys  (car ys)))
           (and xs  (s-value (nth nth ds) :xs (car xs)))
           (and sds (s-value (nth nth ds) :sds (car sds))))
          ((and nth (or functions domains) (nth nth fs))
           (and functions (s-value (nth nth fs) :function (car functions)))
           (and domains   (s-value (nth nth fs) :domain (car domains))))
          (t (loop for i below m do
                   (and (nth i ys)  (s-value (nth i ds) :ys (nth i ys)))
                   (and (nth i xs)  (s-value (nth i ds) :xs (nth i xs)))
                   (and (nth i sds) (s-value (nth i ds) :sds (nth i sds))))
             (loop for i below k do
                   (and (nth i functions) (s-value (nth i fs) :function (nth i functions)))
                   (and (nth i domains)   (s-value (nth i fs) :domain (nth i domains)))))))
  (opal:update window))



;;; Example

(defun Do-Go ()
  (let ((data (make-array '(5 20)
                :initial-contents
                (list (loop for x below 20 collect (+ 6 (random 1.0)))
                      (loop for x below 20 collect (random 3.0))
                      (loop for x below 20 collect (+ 1 (* 0.2 x) (random 1.0)))
                      (loop for x below 20 collect (+ 5 (* -0.9 (log (1+ x))) (random 1.0)))
                      (loop for x below 20 collect (random 3.0)))))
        (randomize (lambda (x) (let ((m (array-dimension x 0))
                                     (n (array-dimension x 1)))
                                 (dotimes (i m x)
                                   (dotimes (j n) (incf (aref x i j) (- (random .2) .1))))))))
    (build
     :window 'EXAMPLE
     :title "This is an example"
     :left 400
     :top 200
     :width 500
     :height 350
     :filling-style opal:motif-light-gray-fill
     :canvas-filling-style opal:blue-fill
     :canvas-boarder-style opal:white-line
     :axis-offset 10
     :tic-size -3
     :x-to 20
     :y-from 0 :y-to 10
     :x-label-by 5 :y-label-by 5
     :x-string "text on x"
     :y-string "text on y"
     :x-grid t :y-grid t
     :x-grid-by 2.5 :y-grid-by 2.5
     :grid-style gray-line
     :data-zero-y 0
     :data-object (list data-line data-wave data-points y-distr histogram)
     :data-description '("Line" "Area" "Scatter" "Distribution of Y" "Histogram")
     :data-line-style (list red-line black-line purple-line green-line black-line)
     :data-filling-style (list opal:white-fill
                               opal:motif-gray-fill
                               opal:yellow-fill
                               opal:white-fill
                               opal:motif-blue-fill)
     :data-x-offset '(0 0 0 0 10)
     :bullet (list nil nil bullet box nil)
     :bullet-size '(nil nil 10 5 nil)
     :data data
     :sds (loop for x below 20 collect 0.5)
     :function (lambda (x) (* 10 x (exp (- x))))
     :domain (lambda (x) (> x 0))
     :fn-line-style yellow-line
     :fn-description "Function"
     :fn-zero-y 0
     :fn-draw-function :xor
     :fn-object function-wave)
    (do ((cycle 0 (incf cycle))
         (data (funcall randomize data) (funcall randomize data))
         (fn (lambda (x) (* 10 x (exp (- x))))
             (lambda (x) (* 10 (/ cycle 100) x (exp (* x (/ cycle -100))))))
         (sds (loop for i below 20 collect (random 1.0))
              (loop for i below 20 collect (random 1.0))))
        ((= cycle 100) (update :data data :sds sds :function fn :window EXAMPLE))
        (update :data data :sds sds :function fn :window EXAMPLE))))
