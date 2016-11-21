;;; Propositional Logic
; Thomas Krause
; DFKI Kaiserlautern, 07. 02. 1991


inter
destroy
consult "demo/prop-log"
l
(or-f _u _v)
more
more

(is true (imp-f _u _v))
more
more
more



(is true (imp-f _A (imp-f _B _A)))
more
more
more
more

(is false (imp-f _A (imp-f _B _A)))
; Compiling
horizon
verti
l or-f
listclass or-f/2
listcode or-f/2
emul
(is true (imp-f _A (imp-f _B _A)))

(is false (imp-f _A (imp-f _B _A)))
