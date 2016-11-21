;  A Self-Normalizing Term Representation of Rotation-Symmetric Workpieces
;
; by Harold Boley (program)
; and Thomas Krause (demo-script) 

; switching to interpreter mode
inter

; consulting the database
destroy
consult "demo/wp-demo"

; A workpiece (wp) is a list of curve terms (lines (l), circle-concave (ca) or points (p)
; A curve term is self-sormalizing for example the line which starts and ends in the same point
; degenerates to a point.
(l ` (p 0 0) ` (p 0 0))


; All workpieces are checked of 'well-formedness' by the RELFUN definition WP.
; We use the well-formed wp n For the rest of the demonstration.
l n

; Is (n) really well-formed?
l test-wp
(test-wp)

; Computing some property of n.
(wplength (n))

(wptypes (n))

(wpedges (n))

(wpxes (n))

(wpradius (n))


; Now compiling the database
; First computing the RELFUN kernel
horizon

; The representation of n after HORIZON
l n

; Generating the RFM instructions
verti

; Switching to the emualtor mode
emul

; Is n allready 'well-formed'
l test-wp
(test-wp)

; Computing some property of n.
(wplength (n))

(wptypes (n))

(wpedges (n))

(wpxes (n))

(wpradius (n))
