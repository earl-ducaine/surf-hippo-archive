;;; drlh.bat
; Werner Stein
; DFKI Kaiserslautern, 07. 1992

; This is an example for all testing operators of the RELFUN-program
; handling Directed Recursive Labelnode Hypergraphs
; The program was written by Harold Boley 

; for more details see ..................
; and the files:
; 	norm.rf
; 	partslist.rf
; 	path.rf
; 	drlh.rf
; 	proc.rf
; 	samples.rf
; 	unpack.rf

script "drlh.script"
inter
destroy
exec "load-all"

;--------Examples from:
;---------norm.rf
l tst1
(tst1)

l tst2
(tst2)

l tst3
(tst3)

l tst4
(tst4)

l tst5
(tst5)

l tst6
(tst6)

l tst7
(tst7)

l tst8
(tst8)

l tst9
(tst9)

;--------Examples from:
;----------partslist.rf
l partslist

l transport
(transport)

(wp)
(partslist (wp))

(wpv)
(partslist (wpv))

;--------Examples from:
;----------path.rf

; for more details see the paper

;--------Examples from:
;----------proc.rf

l ex1
(ex1)

l ex2
(ex2)

l ex3
(ex3)

l ex4
(ex4)

l hyperpath
(hyperpath)

l recursivepath
(recursivepath)

;--------Examples from:
;----------samples.rf

l johnunpack
(johnunpack)

l maryunpack
(maryunpack)

;----------end of script for file drlh.bat

endscript
