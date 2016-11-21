;;; Structure Sharing by the NORMALIZER
; Thomas Krause
; DFKI Kaiserslautern, 07. 02. 1991

; This example is also discuss in section 2 of the manual!

inter
destroy
consult "demo/demostruc"
l
(foo _u _v _w)
more


; Now normalising the database by the command HORIZON

horizon
l
; the program behaves as above

(foo _u _v _w)
more


verti

; The RFM-instructions
listclass foo/3
listcode foo/3

emul

; The RFM-code is correct
(foo _u _v _w)

inter


