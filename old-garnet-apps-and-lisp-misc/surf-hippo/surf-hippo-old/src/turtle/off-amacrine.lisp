;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: #-PARALLEL SURF #+PARALLEL *LISP; Base: 10; -*-
;;; AMACRINE
;Simulation of AMACRINE.
;Preamble


D,#TD1PsT[Begin using 006 escapes](1 0 (NIL 0) (NIL :ITALIC NIL) "CPTFONTI");This is the(2 0 (NIL 0) (NIL NIL NIL) "CPTFONT") 1stellate OFF-Center amacrine cell described by Ammermuller and Weiler, J. Comp. Neuro. 273:137-148, 19882 (A23/24?)1.

2(defun amacrines ()
  (off-amacrine "off-1" :cell-origin '(500 200 -10))
  (off-amacrine "off-2" :cell-origin '(-200 -600 -10))
;  (giant-off-amacrine "g-off-1" :cell-origin '(0 -300 0))
  )



(defun off-amacrine-1 ()
  (off-amacrine "off-1" :synapse t))

(defun off-amacrine-3 ()
  (off-amacrine "off-3" :synapse t :number-of-branches 8 :branch-length 400 :segment-length 100 :segment-diameter .5))

(defun off-amacrine-2 ()
  (off-amacrine "off-2" :cell-origin '(100 200 0) ))

(defun off-amacrine (cell-name &key (cell-origin '(0 0 0)) (synapse nil) (number-of-branches 29)
		     (branch-length 600.0)	(segment-length 100.0) (segment-diameter 0.50))
  (setq *soma-radius 8.0)
  (create-cell cell-name :cell-type-name "off-amacrine" :cell-origin cell-origin)
  (let ((soma (create-soma cell-name (* 2.0 *soma-radius) :g-shunt 0)))
    (create-channels '(na1 na2 na3 dr a)  soma :save-particle nil :plot-pane 3)
    (if (equal *clamp-type "Current clamp")
	(create-isource (format nil "~a-soma-stim" cell-name)  "Ground" (node-name (soma-node soma))
			"ipwl" cell-name (get-source-values "soma-stim" `isource))
	(create-vsource (format nil "~a-soma-stim" cell-name)  "Ground" (node-name (soma-node soma))
			"vpwl" cell-name (get-source-values "soma-stim" `vsource))))
  (create-segment   (format nil "~a-prox" cell-name)	1;This is the segment proximal to the soma.
2		    (soma-name (cell-soma (gethash cell-name cell-hash-table)))
		    cell-name 
		    (list '(v-leak . -70)
			  (cons 'length 12.0)(cons 'diameter 2.0) (cons 'phi (* 0.5 pi))))
  (do  ((i 0 (1+ i)))
       ((= i number-of-branches))
    (off-amacrine-branch cell-name (/ (* 2 3.1415 i) number-of-branches)
			 i branch-length segment-length segment-diameter synapse))
  (setf
    (cell-type-notes (gethash "off-amacrine" cell-type-hash-table))
    "1This is the2 1stellate OFF-Center amacrine cell2 1described by Ammermuller and Weiler, J. Comp. Neuro. 273:137-148, 1988.2~%")
  )

(defun off-amacrine-branch (cell-name branch-angle branch-number branch-length segment-length segment-diameter synapse)
  (let ((seg (create-segment   (format nil "~a-~a-~a" cell-name branch-number '0)
			       (format nil "~a-prox" cell-name)
			       (format nil "~a-~a-~a" cell-name branch-number '0)
			       cell-name 
			       (list '(v-leak . -70)
				     (cons 'length segment-length)
				     (cons 'diameter 1.0)
				     (cons 'theta branch-angle) (cons 'phi (* -0.5 pi))))))
    (if synapse	(create-synapse-type seg 'excitatory-1))
    (dotimes (i (round (/ branch-length segment-length)))
      (setq seg (create-segment (format nil "~a-~a-~a" cell-name branch-number (1+ i))
				(format nil "~a-~a-~a" cell-name branch-number i)
				cell-name 
				(list '(v-leak . -70)
				      (cons 'length segment-length)
				      (cons 'diameter segment-diameter))))
1;2      (print (segment-name seg))
    (if synapse	(create-synapse-type seg 'excitatory-1))
;    (if synapse
;	  (create-standard-synapse seg
;				   (* *synapse-g-leak-ratio
;				      (g-leak-mem (segment-length seg)
;						  (segment-diameter seg)
;						  (cell-type-membrane-resistivity (cell-type (segment-cell seg)))))
;				   (+ -70.0 (* (/ 20.0 200.0) (+ -200.0 (* (1+ i) segment-length))))))	1;this is to try to 
						;reproduce the OFF center synaptic input.
2      (if (and (= branch-number 0)
	       (= (+ 1 i) (round (/ branch-length segment-length)) ))
	  (create-isource "distal-stim" "Ground" (format nil "~a-~a-~a" cell-name branch-number (1+ i))
			  "ipwl" cell-name
			  (get-source-values "distal-stim" `ipwl))))))




