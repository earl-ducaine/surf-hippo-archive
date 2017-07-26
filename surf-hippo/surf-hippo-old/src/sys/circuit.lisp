D,#TD1PsT[Begin using 006 escapes];;; -*- mode: lisp; Syntax: Common-lisp; package: #-parallel surf #+parallel *surf; base: 10; fonts: CPTFONT ; -*-
;;; (c) Copyright 1985, Don Webber, Thinking Machines Corporation, Inc.
;;; File creation date: 6/10/85 15:41:23
;
; A test circuit
;

(defun r (name n1 n2 val)
  "define a resistor."
  (create-resistor name n1 n2 (string "r") (list (cons 'resistance val))))

(defun c (name n1 n2 val)
  "define a capacitor."
  (create-capacitor name n1 n2 (string "c") (list (cons 'capacitance val))))

(defun v (name n1 n2 val)
  "define a voltage source."
  (create-vsource name n1 n2 (string "vdc") (list (cons 'voltage val))))

(defun vpwl (name n1 n2 val period delay)
  "define a voltage source."
  (create-vsource name n1 n2 (string "vpwl") (list (cons 'values val)
						   (cons 'period period)
						   (cons 'delay delay))))

(defun i (name n1 n2 val)
  "define a current source."
  (create-isource name n1 n2 (string "idc") (list (cons 'current val))))

(defun ipwl (name n1 n2 val period delay)
  "define a voltage source."
  (create-isource name n1 n2 (string "ipwl") (list (cons 'values val)
						   (cons 'period period)
						   (cons 'delay delay))))

(defun set-up-mos ()
  "build some mos model instances."
  (create-model-instance (string "enh")
			 (string "mos1")
			 '((w . 2e-6) (l . 2e-6)
			   (vto . 0.8) (kp . 20.0e-6) (gamma . 0.0) (lambda . 0.0)
			   (tox . 0.0)))
  (create-model-instance (string "dep")
			 (string "mos1")
			 '((w . 2e-6) (l . 2e-6)
			   (vto . -3.0) (kp . 20.0e-6) (gamma . 0.0) (lambda . 0.0)
			   (tox . 0.0)))
  (create-model-instance (string "n")
			 (string "mos1")
			 '((w . 2e-6) (l . 2e-6)
			   (vto . 0.8) (kp . 20.0e-6) (gamma . 0.0) (lambda . 0.0)
			   (tox . 0.0)))
  (create-model-instance (string "nreal")
			 (string "mos1")
			 '((w . 2e-6) (l . 2e-6)
			   (vto . 0.8) (kp . 24.0e-5) (gamma . 1.25) (lambda . 0.025)
;			   (vto . 0.8) (kp . 24.0e-6) (gamma . 1.25) (lambda . 0.025)
			   (tox . 8e-8) (cgso . 3.3e-10) (cgdo . 3.3e-10)
			   (cgbo . 0) (mjsw . 0.33 ) (js . 1e-10) (cj . 3.52e-4)
			   (cjsw . 0) (mj . 0.5) (phi . 0.62)))
  (create-model-instance (string "p")
			 (string "mos1")
			 '((w . 2e-6) (l . 2e-6)
			   (vto . -0.8) (kp . 20.0e-6) (gamma . 0.0) (lambda . 0.0)
			   (tox . 0.0) (mtype . PMOS)))
  (create-model-instance (string "preal")
			 (string "mos1")
			 '((w . 2e-6) (l . 2e-6)
			   (vto . -0.7) (kp . 8.0e-6) (gamma . 0.65) (lambda . 0.055)
			   (tox . 8e-8) (cgso . 3.3e-10) (cgdo . 3.3e-10)
			   (cgbo . 0) (mjsw . 0.33 ) (js . 1e-10) (cj . 1.5e-4)
			   (cjsw . 0) (mj . 0.5) (phi . 0.62) (mtype . PMOS)))
  )

(defun enh (name d g s b params)
  "define a enh mosfet."
  (create-mos1 name d g s b  (string "enh") params))

(defun dep (name d g s b params)
  "define a dep mosfet."
  (create-mos1 name d g s b  (string "dep") params))

(defun n (name d g s b params)
  "define a n mosfet."
  (create-mos1 name d g s b  (string "n") params))

(defun p (name d g s b params)
  "define a p type mosfet."
  (create-mos1 name d g s b  (string "p") params))

(defun nreal (name d g s b params)
  "define a n mosfet."
  (create-mos1 name d g s b  (string "nreal") params))

(defun preal (name d g s b params)
  "define a p type mosfet."
  (create-mos1 name d g s b  (string "preal") params))

(defun test-mos1 ()
  (declare-ground 'gnd)
  (set-up-mos)
  (setf
    *init-value-list* '()
    user-stop-time 0.002
    user-max-step 0.0001
    user-min-step 0.0001
    *plot-nodes* '()
    cmin 0
    )
  (vpwl "vdd" 'vdd 'gnd '((0 . -.001) (.002 . .001)) 0 0)

  (dep "test" 'vdd 'vdd 'gnd 'gnd '())
  (c "ctest" 'foo 'gnd 1e-10)
  (r "rtest" 'foo 'gnd 1)
)

(defun pass ()
  "A pass gate"
  (declare-ground 'gnd)
  (set-up-mos)
  (setf
    *init-value-list* '((a . 5))
    user-stop-time 100e-9
    user-max-step 100e-9
    *plot-nodes* '(a b vdd)
    cmin 0
    )
  (vpwl "vdd" 'vdd 'gnd '((0 . 0) (100e-9 . 5)) 0 0)
;  (vpwl "vin" 'a 'gnd '((0 . 0) (1e-9 . 0) (2e-9 . 5) (5e-9 . 5)
;			  (6e-9 . 0) (10e-9 . 0))
;	10e-9 0)

  (nreal "pass" 'a 'vdd 'b 'gnd '((cgdo . 0) (cgso . 0) (gamma . 0) (vto . 0)))
  (c "ca" 'a 'gnd 1e-15)
  (c "cb" 'b 'gnd 1e-15)
)

(defun ram ()
  "A static RAM cell"
  (declare-ground 'gnd)
  (set-up-mos)
  (setf
    *init-value-list* '((q . 5) (d . 3) (db . 3))
;    user-start-time -1e-9
    user-stop-time 40e-9
    user-max-step 10e-9
    *plot-nodes* '(r dd ddb d db q qb)
    )
  (v "vdd" 'vdd 'gnd 5)
  (vpwl "r" 'r 'gnd '((0 . 0) (1e-9 . 5) (5e-9 . 5)
			  (6e-9 . 0) (10e-9 . 0))
	10e-9 0)
  (vpwl "dd" 'dd 'gnd '((0 . 0) (1e-9 . 5) (5e-9 . 5) (6e-9 . 0) (40e-9 . 0) ) 0 0)
  (v "dv" 'dv 'gnd 0)
  (vpwl "ddb" 'ddb 'gnd '((0 . 0) (20e-9 . 0) (21e-9 . 5) (25e-9 . 5) (26e-9 . 0)
			  (40e-9 . 0) ) 0 0)
  (v "dbv" 'dbv 'gnd 0)

  (r "rsub" 'gnd 'sub 1)
;  (c "csub" 'sub 'gnd 1e-14)

  ; The actual cell
  (nreal "nq" 'q 'qb 'sub 'sub '())
  (nreal "nqb" 'qb 'q 'sub 'sub '())
  (r "q-up" 'q 'vdd 1e5)
  (r "qb-up" 'qb 'vdd 1e5)
  (nreal "rq" 'd 'r 'q 'sub '((kp . 24e-6)))
  (nreal "rqb" 'db 'r 'qb 'sub '((kp . 24e-6)))

  ; row pullups
  (nreal "d-up" 'vdd 'vdd 'd 'sub '((l . 2e-6)))
  (nreal "db-up" 'vdd 'vdd 'db 'sub '((l . 2e-6)))
  (c "cd" 'd 'sub 1e-13)
  (c "cdb" 'db 'sub 1e-13)

  ; write logic
;  (r "drive-d" 'd 'dv 1e5)
;  (r "drive-db" 'db 'dbv 1e5)
  (nreal "drive-d" 'd 'dd 'dv 'sub '())
  (nreal "drive-db" 'db 'ddb 'dbv 'sub '())

)

(defun ram-cell (name d db r &aux q qb)
  "the basic cell for a static ram"
  (setf
    q (format nil "~a-~a" name "q")
    qb (format nil "~a-~a" name "qb"))
  
  ; The actual cell
  (nreal (format nil "~a-~a" name "nq") q qb 'gnd 'gnd '())
  (nreal (format nil "~a-~a" name "nqb") qb q 'gnd 'gnd '())
  (r "q-up" q 'vdd 1e5)
  (r "qb-up" qb 'vdd 1e5)
  (nreal "rq" d r q 'gnd '())
  (nreal "rqb" db r qb 'gnd '())
)

(defun small-ram ()
  (declare-ground 'gnd)
  (set-up-mos)
  (setf
    *init-value-list* '()
    user-stop-time 10e-9
    user-max-step 10e-9
    *plot-nodes* '(r d db "a-q" "a-qb")
    )
  (v "vdd" 'vdd 'gnd 5)
  (vpwl "vr" 'r 'gnd '((0 . 0) (1e-9 . 5) (5e-9 . 5) (6e-9 . 0)) 0 0)
  (v "vd" 'd 'gnd 0)
  (v "vdb" 'db 'gnd 5)
  (ram-cell "a" 'd 'db 'r)
)  

(defun dom1 ()
  "A small domino circuit"
  (declare-ground 'gnd)
  (set-up-mos)
  (setf
    *init-value-list* '((store . 5) (temp . 2))
    user-stop-time 10e-9
    user-max-step 10e-9
    *plot-nodes* '(phi store temp out)
    )
  (v "vdd" 'vdd 'gnd 5)
  (vpwl "phi" 'phi 'gnd '((0 . 0) (1e-9 . 0) (2e-9 . 5) (5e-9 . 5)
			  (6e-9 . 0) (10e-9 . 0))
	10e-9 0)
  (vpwl "in" 'in 'gnd '((0 . 5) (3e-9 . 5) (4e-9 . 0) ) 0 0)
  (preal "pullup" 'store 'phi 'vdd 'vdd '())
  (nreal "logic" 'store 'in 'temp 'gnd '())
  (nreal "pulldown" 'temp 'phi 'gnd 'gnd '())
  (preal "inv-up" 'out 'store 'vdd 'vdd '())
  (nreal "inv-down" 'out 'store 'gnd 'gnd '())
  (c "cout" 'out 'gnd 3e-14)
  (c "cstore" 'store 'gnd 1e-14)
)

(defun mirror ()
  "A tough current mirror"
  (declare-ground 'gnd)
  (set-up-mos)
  (setf
    *init-value-list* '()
    user-stop-time 100e-9
    *plot-nodes* '(1 2)
    *node-order* '(2 1)
    cmin 1e-14
;    *debug-all-iterations* t
    )
  (v "vdd" 'vdd 'gnd 5)
  (r "r1" 'vdd 1 1e4)
  (n "m1" 1 2 'gnd 'gnd '())
  (n "m2" 'vdd 1 2 'gnd '())
  (n "m3" 2 2 'gnd 'gnd '())
  )  

(defun sourcef ()
  "A source follower"
  (declare-ground 'gnd)
  (set-up-mos)
  (setf
    *init-value-list* '()
    user-stop-time 10e-9
    *plot-nodes* '(1 2 3 4)
    cmin 1e-14
;    *debug-all-iterations* t
    )
  (v "vdd" 'vdd 'gnd 5)
  (vpwl "vin" 'in 'gnd '((0 . 0) (1e-9 . 0) (3e-9 . 5) (5e-9 . 5) (7e-9 . 0) (10e-9 . 0)) 0 0)
  (r "r1" 'in 1 100)
  (r "r2" 'vdd 3 10000)
  (r "r3" 2 'gnd 100000)
  (r "r4" 'vdd 4 10000)
  (n "m1" 3 1 2 'gnd '())
  (n "m2" 4 2 'gnd 'gnd '())
  )

(defun chain ()
  "A chain of transistors"
  (declare-ground 'gnd)
  (set-up-mos)
  (setf
    *init-value-list* '()
    user-stop-time 10e-9
    *plot-nodes* '(1 2 3 4)
    cmin 1e-14
    *node-order* '(3 4 2 1)
;    *debug-all-iterations* t
    )
  (v "vdd" 'vdd 'gnd 5)
  (vpwl "vin" 'in 'gnd '((0 . 0) (1e-9 . 0) (3e-9 . 5) (5e-9 . 5) (7e-9 . 0) (10e-9 . 0)) 0 0)
  (n "m1" 1 'in 'gnd 'gnd '())
  (n "m2" 2 'in 1 'gnd '())
  (n "m3" 3 'in 2 'gnd '())
  (n "m4" 4 'in 3 'gnd '())
  (n "m5" 'vdd 'in 4 'gnd '())
  )

; circuit converted from Relax2 format.

(defun feedback ()
  (declare-ground 0)
  (create-model-instance "emos2" "mos1"
			 '((w . 1) (l . 1) (gamma . 0.324)
			   (phi . 0.6) (lambda . 0.01) (vto . 0.803) (kp . 0.00036)
			   (tox . 0) (pb . 0.8) (fc . 0.5) (js . 0) (is . 1e-14)
			   (vtherm . 0.0258))
			 )
  (create-model-instance "dmos2" "mos1"
			 '((w . 1) (l . 1) (gamma . 0.323)
			   (phi . 0.6) (lambda . 0.01) (vto . -2.86) (kp . 6e-06)
			   (tox . 0) (pb . 0.8) (fc . 0.5) (js . 0) (is . 1e-14)
			   (vtherm . 0.0258))
			 )
  
  (create-mos1 (gensym) 7 1 8 0 "emos2"
	       '((w . 1) (l . 1) (ad . 0) (as . 0) (pd . 0) (ps . 0) ))
  (create-mos1 (gensym) 11 8 0 0 "emos2"
	       '((w . 2) (l . 1) (ad . 0) (as . 0) (pd . 0) (ps . 0) ))
  (create-mos1 (gensym) 7 11 11 0 "dmos2"
	       '((w . 1) (l . 1) (ad . 0) (as . 0) (pd . 0) (ps . 0) ))
  (create-mos1 (gensym) 8 10 0 0 "emos2"
	       '((w . 1) (l . 1) (ad . 0) (as . 0) (pd . 0) (ps . 0) ))
  (create-capacitor (gensym) 1 11 "c" '((capacitance . 1e-11)))

;  (create-mos1 "a" 7 1 1 0 "dmos2" '((w . 1) (l . 1)))
;  (create-mos1 "b" 1 0 0 0 "emos2" '((w . 1) (l . 1)))
  (create-resistor "c" 1 7 "r" '((resistance . 1000)0))
  
  (create-dc-vsource (gensym) 7 0 "vdc" '((voltage . 5)))
  (create-dc-vsource (gensym) 10 0 "vdc" '((voltage . 1.2)))

  (setf
    user-stop-time 4e-06
    cmin 1e-13
    *max-num-relax-iterations* 40
    *iters-before-sor* 10
    *plot-nodes* '(1 8 11)

    *node-order* '(8 1 11)
    *node-order* '(8 11 1)
    )
  )

(defun qshare ()
  "A charge sharing experiment."
  (declare-ground 'gnd)
  (setf
    *init-value-list* '((2 . 0))
    user-stop-time 1
    user-max-step 1
    *plot-nodes* '(1 2)
    cmin 0
    iabs 1e-6
;    *debug-all-iterations* t
    )
  (c "c1" 1 2 2.0)
;  (r "r1" 1 2 100)
  (c "c2" 2 'gnd 1.0)
;  (r "r2" 2 'gnd 100)
  (vpwl "vin" 1 'gnd '((0 . 0) (0.5 . 1)) 0 0)
  )

(defun cinv2 ()
  "An cmos inverter chain."
  (declare-ground 0)
  (set-up-mos)
  (setf
    *init-value-list* '((3 . 5))
    user-stop-time 10e-9
    user-max-step 2e-9
    user-min-step 1e-15
    *plot-nodes* '(2 3 4)
    cmin 0)
  (v "vdd" 1 0 5e0)
  (vpwl "vin" 2 0 '((0 . 0) (1e-9 . 0) (1.5e-9 . 5) (5e-9 . 5) (5.5e-9 . 0) (10e-9 . 0)) 0 0)
;  (n "m1" 3 2 0 0 '())
;  (p "m2" 3 2 1 1 '())
;  (r "r23" 2 3 1e9)
  (c "c3" 3 0 1e-14)
;  (n "m3" 4 3 0 0 '())
;  (p "m4" 4 3 1 1 '())
;  (r "r34" 3 4 1e9)
  (c "cfeed1" 2 3 2e-14)
  (c "cfeed2" 3 4 2e-14)
  (c "c4" 4 0 1e-14)
)

(defun ccc ()
  "A two stage capacitor tree"
  (declare-ground 0)
  (setf
    user-stop-time 1
    iabs 1e-5
    *plot-nodes* '(1 2 3)
;    *debug-all-iterations* t
    )
  (vpwl "vin" 1 0 '((0 . 0) (0.5 . 1.0)) 0 0)
  (c "r1" 1 2 2.0)
  (c "r2" 2 0 1.0)
  (c "r3" 2 3 2.0)
  (c "r4" 3 0 1.0)
)

(defun cinv2-slow ()
  "An cmos inverter chain with a small resistor."
  (declare-ground 0)
  (set-up-mos)
  (setf
    *init-value-list* '((3 . 5) (13 . 5))
    user-stop-time 10e-9
    user-max-step 2e-9
    *plot-nodes* '(2 3 4 13)
    )
  (v "vdd" 1 0 5e0)
  (vpwl "vin" 2 0 '((0 . 0) (1e-9 . 0) (1.5e-9 . 5) (5e-9 . 5) (5.5e-9 . 0) (10e-9 . 0)) 0 0)
  (n "m1" 13 2 0 0 '())
  (p "m2" 13 2 1 1 '())
  (r "r1" 13 3 10)
  (c "c13" 13 0 1e-14)
  (c "c3" 3 0 1e-14)
  (n "m3" 4 3 0 0 '())
  (p "m4" 4 3 1 1 '())
  (c "c4" 4 0 1e-14)
)

(defun cinv2-cap ()
  "An cmos inverter chain with a large capacitor."
  (declare-ground 0)
  (set-up-mos)
  (setf
    *init-value-list* '((3 . 5) (13 . 5))
    user-stop-time 10e-9
    user-max-step 2e-9
    *plot-nodes* '(2 3 4 13)
    )
  (v "vdd" 1 0 5e0)
  (vpwl "vin" 2 0 '((0 . 0) (1e-9 . 0) (1.5e-9 . 5) (5e-9 . 5) (5.5e-9 . 0) (10e-9 . 0)) 0 0)
  (n "m1" 13 2 0 0 '())
  (p "m2" 13 2 1 1 '())
  (c "c1" 13 3 1e-12)
  (c "c13" 13 0 1e-13)
  (c "c3" 3 0 1e-13)
  (r "rup" 3 1 1e7)
  (r "rdown" 3 0 1e7)
  (n "m3" 4 3 0 0 '())
  (p "m4" 4 3 1 1 '())
  (c "c4" 4 0 1e-14)
)

(defun rinv ()
  "An resistor - inverter circuit."
  (declare-ground 0)
  (set-up-mos)
  (setf
    *init-value-list* '((3 . 5))
    user-stop-time 10e-9
    user-max-step 1e-9
    *plot-nodes* '(2 3)
    )
  (v "vdd" 1 0 5e0)
  (vpwl "vin" 2 0 '((0 . 0) (1e-9 . 0) (1.5e-9 . 5) (5e-9 . 5) (5.5e-9 . 0) (10e-9 . 0)) 0 0)
  (n "pulldown" 3 2 0 0 '((tox . 20e-9)))
;  (n "pulldown" 3 4 0 0 '((cgbo . 1e-7)))
;  (n "pulldown" 3 4 0 0 '())
;  (r "rin" 2 4 1000)
  (r "pullup"   3 1 1e5)
  (c "cout" 3 0 1e-14)
)

(defun rinv2 ()
  "An resistor - inverter circuit."
  (declare-ground 0)
  (set-up-mos)
  (setf
    *init-value-list* '((3 . 5) (4 . 0))
    user-stop-time 10e-9
    user-max-step 1e-9
    *plot-nodes* '(2 3 4)
    )
  (v "vdd" 1 0 5e0)
  (vpwl "vin" 2 0 '((0 . 0) (1e-9 . 0) (1.5e-9 . 5) (5e-9 . 5) (5.5e-9 . 0) (10e-9 . 0)) 0 0)
  (n "pulldown1" 3 2 0 0 '())
  (r "pullup1"   3 1 1e5)
  (c "cout1" 3 0 1e-14)
  (n "pulldown2" 4 3 0 0 '((cgbo . 1e-7)))
  (r "pullup2"   4 1 1e5)
  (c "cout2" 4 0 1e-14)
)

(defun rinv1 ()
  "An resistor - inverter circuit."
  (declare-ground 0)
  (set-up-mos)
  (setf
    *init-value-list* '((3 . 5))
    user-stop-time 10e-9
    user-max-step 1e-9
    *plot-nodes* '(2 3)
    *debug-at-time-steps* t
    *debug-all-iterations* t
    *print-matrix* t
    )
  (v "vdd" 1 0 5e0)
  (vpwl "vin" 4 0 '((0 . 0) (1e-9 . 0) (1.5e-9 . 5) (5e-9 . 5) (5.5e-9 . 0) (10e-9 . 0)) 0 0)
  (r "rin" 4 2 100)
  (n "pulldown1" 3 2 0 0 '())
  (r "pullup1"   3 1 1e5)
  (c "cout1" 3 0 1e-14)
)

(defun prinv ()
  "An pmos resistor - inverter circuit."
  (declare-ground 0)
  (set-up-mos)
  (setf
    *init-value-list* '((3 . 5))
    user-stop-time 10e-9
    user-max-step 1e-9
    *plot-nodes* '(2 3 4)
    )
  (v "vdd" 1 0 5e0)
  (vpwl "vin" 2 0 '((0 . 0) (1e-9 . 0) (1.5e-9 . 5) (5e-9 . 5) (5.5e-9 . 0) (10e-9 . 0)) 0 0)
  (p "pullup" 3 4 1 1 '((cgbo . 1e-7)))
;  (p "pullup" 3 4 1 1 '((cgso . 1e-7) (cgdo . 1e-7)))
  (r "rin" 2 4 1000)
  (r "pulldown"  3 0 1e5)
  (c "cout" 3 0 1e-14)
)

(defun cinv ()
  "An cmos inverter circuit."
  (declare-ground 0)
  (set-up-mos)
  (setf
    *init-value-list* '((3 . 5))
    user-stop-time 10e-9
    user-max-step 1e-9
    *plot-nodes* '(2 3)
    )
  (v "vdd" 1 0 5e0)
  (vpwl "vin" 2 0 '((0 . 0) (1e-9 . 0) (1.5e-9 . 5) (5e-9 . 5) (5.5e-9 . 0) (10e-9 . 0)) 0 0)
  (n "pulldown" 3 2 0 0 '((tox . 20e-9)))
  (p "pullup"   3 2 1 1 '((tox . 20e-9)))
  (c "cout" 3 0 1e-14)
)

(defun cnand ()
  "An cmos nand circuit."
  (declare-ground 'gnd)
  (set-up-mos)
  (setf
    *init-value-list* '((out . 5) (internal . 0))
    user-stop-time 15e-9
;    user-max-step 1e-9
    *plot-nodes* '(in1 in2 internal out)
    )
  (v "vdd" 'vdd 'gnd 5e0)
  (vpwl "vin1" 'in1 'gnd
	'((0 . 0) (1e-9 . 0) (1.5e-9 . 5) (3e-9 . 5) (3.5e-9 . 0) (8e-9 . 0)
	  (8.5e-9 . 5) (10e-9 . 5) (10.5e-9 . 0) (15e-9 . 0)) 0 0)
  (vpwl "vin2" 'in2 'gnd
	'((0 . 0) (5e-9 . 0) (5.5e-9 . 5) (12e-9 . 5) (12.5e-9 . 0) (15e-9 . 0)) 0 0)

  (nreal "pulldown2" 'internal 'in2 'gnd 'gnd '())
  (nreal "pulldown1" 'out 'in1 'internal 'gnd '())
  (preal "pullup2"   'out 'in2 'vdd 'vdd '())
  (preal "pullup1"   'out 'in1 'vdd 'vdd '())
  (c "cout" 'out 'gnd 1e-14)
  (c "cinternal" 'internal 'gnd 1e-14)
)

(defun rosc ()
  "An cmos inverter circuit."
  (declare-ground 0)
  (set-up-mos)
  (setf
    *init-value-list* '((2 . 0.01))
    user-stop-time 100e-9
;    user-max-step 1e-9
    *plot-nodes* '(2 3 4 5 6)
    )
  (v "vdd" 1 0 5e0)
;  (vpwl "vin" 2 0 '((0 . 0) (1e-9 . 0) (1.5e-9 . 5) (5e-9 . 5) (5.5e-9 . 0) (10e-9 . 0)) 0 0)
  (n "m1" 3 2 0 0 '())
  (p "m2" 3 2 1 1 '())
  (c "c3" 3 0 1e-13)
  (n "m3" 4 3 0 0 '())
  (p "m4" 4 3 1 1 '())
  (c "c4" 4 0 1e-13)
  (n "m5" 5 4 0 0 '())
  (p "m6" 5 4 1 1 '())
  (c "c5" 5 0 1e-13)
  (n "m7" 6 5 0 0 '())
  (p "m8" 6 5 1 1 '())
  (c "c6" 6 0 1e-13)
  (n "m9" 2 6 0 0 '())
  (p "m10" 2 6 1 1 '())
  (c "c2" 2 0 1e-13)
)

(defun cinv-sweep ()
  "An cmos inverter circuit."
  (declare-ground 0)
  (set-up-mos)
  (setf
    *init-value-list* '((3 . 0))
    user-stop-time 3e-9
    user-max-step 1e-9
    cmin 0e0
    *plot-nodes* '(2 3 )
    )
  (v "vdd" 1 0 5e0)
  (vpwl "vin" 2 0 '((0 . 5) (1e-9 . 0)) 0 0)
  (n "pulldown" 3 2 0 0 '((tox . 20e-9)))
  (p "pullup"   3 2 1 1 '((tox . 20e-9)))
  (c "cout" 3 0 1e-16)
)

(defun inv ()
  "An nmos inverter circuit."
  (declare-ground 0)
  (set-up-mos)
  (setf
    user-stop-time 20e-9
    user-max-step 1e-9
    user-min-step 1e-12
    *plot-nodes* '(2 3 )
    )
  (v "vdd" 1 0 5e0)
  (v "vin" 2 0 5e0)
  (enh "pulldown" 3 2 0 0 '((tox . 20e-9)))
  (dep "pullup" 1 3 3 0 '((tox . 20e-9) (l . 8e-6)))
;  (dep "pullup" 1 3 3 0 '((tox . 20e-9)))
  (c "cout" 3 0 1e-12)
)

(defun inv2 ()
  "An nmos inverter chain."
  (declare-ground 0)
  (set-up-mos)
  (setf
    *init-value-list* '((3 . 5))
    user-stop-time 10e-9
    user-max-step 2e-9
    *plot-nodes* '(2 3 4)
    )
  (v "vdd" 1 0 5e0)
  (vpwl "vin" 2 0 '((0 . 0) (1e-9 . 0) (1.5e-9 . 5) (5e-9 . 5) (5.5e-9 . 0) (10e-9 . 0)) 0 0)
  (enh "m1" 3 2 0 0 '((tox . 20e-9)))
  (dep "m2" 1 3 3 0 '((tox . 20e-9) (l . 8e-6)))
  (c "c3" 3 0 1e-14)
  (enh "m3" 4 3 0 0 '((tox . 20e-9)))
  (dep "m4" 1 4 4 0 '((tox . 20e-9) (l . 8e-6)))
  (c "c4" 4 0 1e-14)
)

(defun inv4 ()
  "An nmos inverter chain."
  (declare-ground 0)
  (set-up-mos)
  (setf
    *init-value-list* '((3 . 5) (5 . 5))
    user-stop-time 10e-9
    user-max-step 2e-9
    user-min-step 1e-12
    *plot-nodes* '(2 3 4 5 6)
    )
  (v "vdd" 1 0 5e0)
  (v "vin" 2 0 5e0)
  (enh "m1" 3 2 0 0 '((tox . 20e-9)))
  (dep "m2" 1 3 3 0 '((tox . 20e-9) (l . 8e-6)))
  (c "c3" 3 0 1e-14)
  (enh "m3" 4 3 0 0 '((tox . 20e-9)))
  (dep "m4" 1 4 4 0 '((tox . 20e-9) (l . 8e-6)))
  (c "c4" 4 0 1e-14)
  (enh "m5" 5 4 0 0 '((tox . 20e-9)))
  (dep "m6" 1 5 5 0 '((tox . 20e-9) (l . 8e-6)))
  (c "c5" 5 0 1e-14)
  (enh "m7" 6 5 0 0 '((tox . 20e-9)))
  (dep "m8" 1 6 6 0 '((tox . 20e-9) (l . 8e-6)))
  (c "cout" 6 0 1e-14)
)

(defun test-pwl ()
  "To test pwl sources."
  (declare-ground 0)
  (setf
    *plot-nodes* '( 1 2 )
    user-stop-time 3
    user-min-step 1e-4
    user-max-step .1)
  (vpwl "vin" 1 0 '((0 . 0) (.1 . 0) (.3 . 5) (.6 . 5) (.8 . 0) (1 . 0)) 1 0)
  (r "r1" 1 0 1e3)
  (vpwl "vin2" 2 0 '((0 . 0) (.1 . 0) (.3 . 5) (.6 . 5) (.8 . 0) (1 . 0)) 1 .2)
  (r "r2" 2 0 1e3)
)

(defun test-ipwl ()
  "To test ipwl sources."
  (declare-ground 0)
  (setf
    *plot-nodes* '( 1 2 )
    user-stop-time 3
    user-min-step 1e-4
    user-max-step .1)
  (ipwl "Iin" 0 1 '((0 . 0) (.1 . 0) (.3 . 5) (.6 . 5) (.8 . 0) (1 . 0)) 1 0)
  (r "r1" 1 0 1)
  (c "c1" 1 0 .1)
  (ipwl "Iin2" 0 2 '((0 . 0) (.1 . 0) (.3 . 5) (.6 . 5) (.8 . 0) (1 . 0)) 1 .2)
  (r "r2" 2 0 1)
  (c "c2" 2 0 .1)
)

(defun i1 ()
  "To try out current sources."
  (declare-ground 0)
  (setf
    user-stop-time 1e-5
    user-min-step 1e-9
    user-max-step 1e-5
    *plot-nodes* '(1)
;    *debug-at-time-steps* t
;    *debug-all-iterations* t
    )
  (i "in" 0 1 1e-3)
  (r "r1" 1 0 1e3)
  (c "c1" 1 0 1e-9)
)

(defun rc1 ()
  "Build a simple RC circuit."
  (declare-ground 0)
  (setf
    user-stop-time 1e-5
    user-min-step 1e-9
    user-max-step 1e-4
    cmin 0
    *plot-nodes* '(1 2)
    )
  (v "vin" 1 0 1e0)
  (r "r1" 1 2 1000e0)
  (c "c1" 2 0 1.0e-9)
  )

(defun rc2 ()
  "Build a simple RC circuit."
  (declare-ground 0)
  (setf
    user-stop-time 1e-4
    user-min-step 1e-9
    user-max-step 1e-5
    *plot-nodes* '(2 3)
    )
  (v "vin" 1 0 1e0)
  (r "r1" 1 2 1000e0)
  (r "r2" 3 0 1000e0)
  (c "c1" 2 0 2.0e-9)
  (c "c2" 2 3 1.0e-9)
  (c "c3" 3 0 2.0e-9)
  )

(defun r1 ()
  "A DC resistor circuit."
  (declare-ground 0)
  (setf
    user-stop-time 1e-5
    user-min-step 1e-10
    *plot-nodes* '(2 )
    )
  (v "vin" 1 0 2e0)
  (r "r1" 1 2 1000e0)
  (r "r2" 2 0 1000e0)
  (r "r3" 2 2 1e0)
  (c "c1" 2 0 1e-9)
)

(defun r2 ()
  "A DC resistor circuit."
  (declare-ground 0)
  (setf
    user-stop-time 1e-5
;    user-min-step 1e-10
    cmin 0
    *plot-nodes* '(2 3)
    )
  (v "vin" 1 0 2e0)
  (r "r1" 1 2 2000e0)
  (r "r2" 2 0 2000e0)
  (r "r3" 2 3 1000e0)
  (r "r4" 3 0 1000e0)
)

(defun r3 ()
  "A DC resistor circuit."
  (declare-ground 0)
  (setf
    *init-value-list* '((1 . 1))
    user-max-step 10000
    user-stop-time 1000
    cmin 1
    *plot-nodes* '(1 2 3)
    *node-order* '(2 3 1)
    )

  (create-model-instance (string "nn")
			 (string "mos1")
			 '((w . 1) (l . 1) (vto . 0.8) (kp . 1)))

  (v "vin" 'in 0  10)
  (create-mos1 "r1" 1 'in 2 0 "nn" '() )
  (r "r2" 2 3 2)
  (r "r3" 3 1 3)
)

(defun rc-chain (n nodes-to-plot)
  "A N stage RC chain."
  (declare-ground 'gnd)
  (set-up-mos)
  (setf
    user-stop-time 10e-9
;    user-stop-time 5e-9
    *plot-nodes* nodes-to-plot
    cmin 0
    )
;  (v 'vdd 'vdd 'gnd 5e0)
  (vpwl 'vin 0 'gnd
	'((0 . 0) (1e-9 . 0) (1.5e-9 . 5) (5e-9 . 5) (5.5e-9 . 0) (10e-9 . 0)) 0 0)
  (dotimes (i n)
    (r (zl:format nil "~a-~a" 'r i) i (1+ i) 1.0)
    (c (zl:format nil "~a-~a" 'c (1+ i)) (1+ i) 'gnd 1e-14))
)
(defun rc-chain1 () (rc-chain 1 '(0 1)))
(defun rc-chain2 () (rc-chain 2 '(0 1 2)))
(defun rc-chain4 () (rc-chain 4 '(0 1 2 3 4)))
(defun rc-chain8 () (rc-chain 8 '(0 1 2 3 4 5 6 7 8)))
(defun rc-chain16 () (rc-chain 16 '(0 1 2 3 4 5 6 7 8)))
(defun rc-chain64 () (rc-chain 64 '(0 1 2 3 4 5 6 7 8)))
(defun rc-chain256 () (rc-chain 256 '(0 1 2 3 4 5 6 7 8)))
(defun rc-chain1024 () (rc-chain 1024 '(0 1 2 3 4 5 6 7 8)))
(defun rc-chain2048 () (rc-chain 2048 '(0 1 2 3 4 5 6 7 8)))
(defun rc-chain4096 () (rc-chain 4096 '(0 1 2 3 4 5 6 7 8)))


(defun cinvn (n nodes-to-plot)
  "A N stage cmos inverter chain."
  (declare-ground 'gnd)
  (set-up-mos)
  (setf
    *init-value-list* (let ((temp nil))
		      (dotimes (i n)
			(if (oddp i)
			    (push (cons i 5) temp)))
		      temp)
    user-stop-time 10e-9
;    user-stop-time 5e-9
    *plot-nodes* nodes-to-plot
    )
  (v 'vdd 'vdd 'gnd 5e0)
  (vpwl 'vin 0 'gnd
	'((0 . 0) (1e-9 . 0) (1.5e-9 . 5) (5e-9 . 5) (5.5e-9 . 0) (10e-9 . 0)) 0 0)
  (dotimes (i n)
    (n (zl:format nil "~a-~a" 'n i) (1+ i) i 'gnd 'gnd '())
    (p (zl:format nil "~a-~a" 'p i) (1+ i) i 'vdd 'vdd '())
;    (n (zl:format nil "~a-~a" 'n i) (1+ i) i 'gnd 'gnd '((cgdo . 0.5e-8)))
;    (p (zl:format nil "~a-~a" 'p i) (1+ i) i 'vdd 'vdd '((cgdo . 0.5e-8)))
    (c (zl:format nil "~a-~a" 'c (1+ i)) (1+ i) 'gnd 1e-14))
)
#|
(defun cinvn-schematic (n nodes-to-plot)
  "A N stage cmos inverter chain. With interface to csim-frame"
  (declare-ground 'gnd)
  (set-up-mos)
  (setf
    *init-value-list* (let ((temp nil))
		      (dotimes (i n)
			(if (oddp i)
			    (push (cons i 5) temp)))
		      temp)
;    user-stop-time 10e-9
    user-stop-time 5e-9
    *plot-nodes* nodes-to-plot
    )
  (v "vdd" 'vdd 'gnd 5e0)
;  (vpwl "vin" 0 'gnd '((0 . 0) (1e-9 . 0) (1.5e-9 . 5) (5e-9 . 5) (5.5e-9 . 0) (10e-9 . 0)) 0 0)
  (vpwl "vin" 0 'gnd '((0 . 0) (1e-9 . 0) (1.5e-9 . 5) (3e-9 . 5) (3.5e-9 . 0) (5e-9 . 0)) 0 0)
  (let ((device-list
	  (loop for i from 0 to (1- n)
		for pullup = (p (zl:format nil "~a-~a" 'p i) (1+ i) i 'vdd 'vdd '())
		for pulldown = (n (zl:format nil "~a-~a" 'n i) (1+ i) i 'gnd 'gnd '())
		for cap = (c (zl:format nil "~a-~a" 'c (1+ i)) (1+ i) 'gnd 1e-14)
		collecting (list (cons pullup (zl:format nil "~a-~a" 'p i))
				 (cons pulldown (zl:format nil "~a-~a" 'n i))
				 (cons cap (zl:format nil "~a-~a" 'c i))) into cinvstages
		finally (return cinvstages))))
    (make-cinvn-schematic (isqrt n) (/ n (isqrt n)) device-list))
)

(defun make-cinvn-schematic (x y &optional (core-name-list))
  (loop with counter = -1
	for j from 1 to y
	for origin-y = 20 then (+ origin-y 200)
	appending
	  (loop for i from 1 to x
		for k = (incf counter)
		for stage-core-name = (nth k core-name-list)
		for pullup-core = (caar stage-core-name)
		for pullup-name = (cdar stage-core-name)
		for pulldown-core = (caadr stage-core-name)
		for pulldown-name = (cdadr stage-core-name)
		for capacitor-core = (caaddr stage-core-name)
		for capacitor-name = (cdaddr stage-core-name)
		for origin-x = 20 then (+ origin-x 120)
		appending (make-cmos-inverter origin-x origin-y :pullup-core pullup-core
					      :pulldown-core pulldown-core :pullup-name pullup-name :pulldown-name pulldown-name
					      :capacitor-core capacitor-core :capacitor-name capacitor-name)
		into inverter-row
		finally (return (append inverter-row
					(if (neq j y)
					    (join-by-wires
					      (+ origin-x 120) (+ origin-y 60)
					      (+ origin-x 160) (+ origin-y 60)
					      (+ origin-x 160) (+ origin-y 160)
					      10 (+ origin-y 160)
					      10 (+ origin-y 260)
					      20 (+ origin-y 260))))))))
|#
(defun relax-cinvn (n nodes-to-plot)
  "A N stage cmos inverter chain."
  (with-open-file (out-file "c:>r.out" :direction :output)
    (zl:format out-file "~%global gnd~%")
    (zl:format out-file "relax2 options stop=5e-9 dodc=0 dotran=1 dodirect=1~%")
    (zl:format out-file "model n nmos  vto=0.8 kp=20u phi=0.6 lambda=0.0 ~%")
    (zl:format out-file "model p pmos vto=-0.8 kp=20u phi=0.6 lambda=0.0 ~%")
    (zl:format out-file "ic \~%")
    (dotimes (i n)
      (if (oddp i)
	  (zl:format out-file " ~a = 5 \~%" i)))
    (zl:format out-file "~%")
    (zl:format out-file "plot \~%")
    (dolist (x nodes-to-plot)
      (zl:format out-file " ~a\~%" x))
    (zl:format out-file "~%~%")
    
    (zl:format out-file "vdd power gnd dc v=5~%")
    (zl:format out-file "vin 0 gnd pwl t0=0 v0=0 t1=1n v1=0 t2=1.5n v2=5 t3=3n v3=5 t4=1.5n v4=0 t5=5n v5=0~%~%")
    
    (dotimes (i n)
      (zl:format out-file "n~a ~a ~a gnd gnd n w=2u l=2u~%" i (1+ i) i )
      (zl:format out-file "p~a ~a ~a power power p w=2u l=2u~%" i (1+ i) i )
      (zl:format out-file "c~a ~a gnd c c=1e-14~%" (1+ i) (1+ i)))
    ))

(defun cinv4 () (cinvn 4 '(0 1 2 3)))
(defun cinv16 () (cinvn 16 '(0 1 2 3 15)))
(defun cinv64 () (cinvn 64 '(0 1 2 3 15)))
(defun cinv256 () (cinvn 256 '(0 1 2 3 15)))
(defun cinv512 () (cinvn 512 '(0 1 2 3 15)))
(defun cinv1024 () (cinvn 1024 '(0 1 2 3 15)))
(defun cinv2048 () (cinvn 2048 '(0 1 2 3 15)))
(defun cinv4096 () (cinvn 4096 '(0 1 2 3 15)))
(defun cinv16384 () (cinvn 16384 '(0 1 2 3 15)))
(defun cinv65536 () (cinvn 65536 '(0 1 2 3 15)))
(defun cinv50 () (cinvn 50 '(0 1 2 3 15)))
(defun cinv63 () (cinvn 63 '(0 1 2 3 15)))
(defun cinv1666 () (cinvn 1666 '(0 1 2 3 15)))

#|
(defvar cinv22)
(defun cinv4s () (setq cinv22  (cinvn-schematic 4 '(0 1 2 3))))

(defvar cinv60)
(defun cinv60s () (setq cinv60  (cinvn-schematic 60 '(0 1 2 3))))

(defvar cinv1024)
(defun cinv1024s () (setq cinv1024  (cinvn-schematic 1024 '(0 1 2 3))))
|#

(defun rcinv4 () (relax-cinvn 4 '(0 1 2 3)))
(defun rcinv16 () (relax-cinvn 16 '(0 1 2 3 15)))
(defun rcinv64 () (relax-cinvn 64 '(0 1 2 3 15)))
(defun rcinv256 () (relax-cinvn 256 '(0 1 2 3 15)))
(defun rcinv1024 () (relax-cinvn 1024 '(0 1 2 3 15)))
(defun rcinv4096 () (relax-cinvn 4096 '(0 1 2 3 15)))
(defun rcinv16384 () (relax-cinvn 16384 '(0 1 2 3 15)))
(defun rcinv65536 () (relax-cinvn 65536 '(0 1 2 3 15)))


(defun tree3 (depth)
  "A three tree circuit"
  (declare-ground 'gnd)
  (set-up-mos)
  (setf
;    init-value-list '((3 . 5))
    user-stop-time 10e-9
    user-max-step 1e-9
    *plot-nodes* '("A" "AA" "AAA")
;    *debug-all-iterations* t
    )
  (v "vdd" 'vdd 'gnd 5e0)
  (vpwl "vin" 'vin 'gnd
	'((0 . 0) (1e-9 . 0) (1.5e-9 . 5) (5e-9 . 5) (5.5e-9 . 0) (10e-9 . 0)) 0 0)
  (tree3-control depth 'vin "A"))

(defun tree3-control (depth input name)
  (if (not (= depth 0))
      (progn
	(tree3-element name input name)
	(tree3-control (1- depth) name (format nil "~a~a" name 'a))
	(tree3-control (1- depth) name (format nil "~a~a" name 'b))
	(tree3-control (1- depth) name (format nil "~a~a" name 'c)))))

(defun tree3-element (name input output)
  (inverter name input output)
;  (format t "~%Creating element ~a, input ~a, output ~a" name input output)
  )

(defun inverter (name input output)
  (nreal
    (format nil "~a~a" name 'pulldown)
    output input 'gnd 'gnd
    '((cgdo . 1e-8) (cgso . 1e-8))
    )
  (preal
    (format nil "~a~a" name 'pullup)
    output input 'vdd 'vdd
    '((cgdo . 1e-8) (cgso . 1e-8))
    )
  (c
    (format nil "~a~a" name 'load)
    output 'gnd
    1e-14))

(defun tree3-3 () (tree3 3))
(defun tree3-4 () (tree3 4))
(defun tree3-5 () (tree3 5))
(defun tree3-6 () (tree3 6))
(defun tree3-7 () (tree3 7))
(defun tree3-8 () (tree3 8))
(defun tree3-9 () (tree3 9))
(defun tree3-10 () (tree3 10))
