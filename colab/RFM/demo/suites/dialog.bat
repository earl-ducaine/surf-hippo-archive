here (is _relfun reading) its first line `(tup binding the variable) _relfun

; ;;
; Instantiation of tuples (lists) ;;
(is _x 1) `(tup _x 2 _z)
(is _x 1) `(tup an instantiation which acts like quoting)
prelude  ; look if tup is defined in the prelude, hence can be used actively
(is _x 1) (tup _x 2 _z)
(is _x 1) (tup an instantiation which acts like quoting)
(is _x 1) (tup x _x `x `_x `(tup x) (tup x) `(tup _x) (tup _x) `(tup x _x 2))
consult instex
l
(f 1)
(f (f 1))
(g 1 2)
(try)
(recpat 1)
(recpat 5)
destroy

; ;;
; Pure list processing ;;
azft (cons-f _head _tail) `(tup _head | _tail)
(cons-f a b)
(cons-f a `(tup))
(cons-f a _rest)
(cons-f _first `(tup b c))
(cons-f _first _rest)
(is _x `(tup 1 2)) `(tup _x | _x)
(is _x `(tup 1 2)) (cons-f _x _x)
azft (car-f (tup _head | _tail)) _head
azft (cdr-f (tup _head | _tail)) _tail
(car-f `(tup leading element is leading))
(cdr-f `(tup leading element is leading))
(car-f `(tup _q 2 _q))
(cdr-f `(tup _q 2 _q))
(is _a (car-f `(tup _q 2 _q))) (is _q 1) (is _d (cdr-f `(tup _q 2 _q))) (cons-f _a _d)
azft (list-f | _this-is-the-list-already) _this-is-the-list-already  ; like tup
`(tup all of these elements)
(list-f all of these elements)
(list-f another | `(tup way of consing))
(list-f _e1 _e2 _e3)
(list-f _e)
(list-f | _e)
azhn (consp-r (tup _head | _tail))
(consp-r a)
(consp-r `(tup a h))
(consp-r `(tup))
(consp-r _what)
more
azhn (listp-r (tup))
azhn (listp-r _x) (consp-r _x)
(listp-r `(tup))
(listp-r a)
(listp-r _what)
more
more
listing
destroy

; ;;
; The function appfun ;;
consult sampler/brief-intro
l
(appfun `(tup a b) `(tup 1 2 3))
(appfun `(tup a b) _suffix)
(appfun `(tup a b) id)
(appfun _prefix `(tup 1 2 3))
more
more
(appfun id `(tup 1 2 3))
more
more
(appfun _prefix _suffix)
more
(appfun id id)
more
(appfun `(tup a b) (appfun `(tup 1 2) `(tup 3)))
azft (selfapp-f _half) (appfun _half _half)
(selfapp-f `(tup a b))
(selfapp-f _any)
more
more

; ;;
; The relation apprel ;;
(apprel `(tup a b) `(tup 1 2 3) _res)
(apprel `(tup a b) `(tup 1 2 3) id)
(apprel `(tup a b) _suffix _res)
(apprel `(tup a b) id _res)
(apprel _prefix `(tup 1 2 3) _res)
more
more
(apprel id `(tup 1 2 3) _res)
more
more
(apprel _prefix _suffix _res)
more
(apprel id id _res)
more
(apprel `(tup 1 2) `(tup 3) _x) (apprel `(tup a b) _x _res)
azhn (selfapp-r _half _lst) (apprel _half _half _lst)
(selfapp-r `(tup a b) _res)
(selfapp-r _any _res)
more
more
azhn (equalhalves-r _lst) (apprel _half _half _lst)
(equalhalves-r `(tup a b a b))
(equalhalves-r `(tup a b a))
(equalhalves-r _res)
more
more
destroy

; ;;
; The relation-function append-rf ;;
consult cmlist
l
(append-rf `(tup a b) `(tup 1 2 3) _res)
(append-rf `(tup a b) `(tup 1 2 3) id)
(append-rf `(tup a b) _suffix _res)
(append-rf `(tup a b) id _res)
(append-rf `(tup a b) id id)
(append-rf _prefix `(tup 1 2 3) _res)
more
more
(append-rf id `(tup 1 2 3) _res)
more
more
(append-rf id `(tup 1 2 3) id)
more
more
(append-rf _prefix _suffix _res)
more
(append-rf id id _res)
more
(append-rf id id id)
more
(append-rf `(tup a b) (append-rf `(tup 1 2) `(tup 3) id) id)
azft (selfapp-rf _half _lst) (append-rf _half _half _lst)
(selfapp-rf `(tup a b) _res)
(selfapp-rf _any _res)
more
more
azft (equalhalves-rf _lst) (append-rf _half _half _lst) _half
(equalhalves-rf `(tup a b a b))
(equalhalves-rf `(tup a b a))
(equalhalves-rf _res)
more
more

; ;;
; And further relation-functions ;;
(last-rf 3 `(tup 1 2 3))
(last-rf _what `(tup 1 2 3))
(last-rf 3 _where)
(last-rf _what _where)
more
more
more
(last-rf id `(tup 1 2 3))
(nextto-rf id house `(tup there is a funny house in the garden))
(member-rf 2 `(tup 1 2 3 4))
(member-rf _x `(tup a _x c))
more
more
more
(rev-rf `(tup a b c) _res)
(rev-rf _res `(tup a b c))
(efface-rf 2 `(tup 1 2 3 4) id)

; ;;
; Permutation relation-function ;;
az (ft (permutation-rf (tup) (tup)) `(tup))
az (ft (permutation-rf _lst (tup _head | _tail))
       (append-rf _v `(tup _head | _u) _lst)
       (append-rf _v _u _w)
       (permutation-rf _w _tail)
       `(tup _head | _tail))
(permutation-rf `(tup a b c) _res)
more
more
more
more
more
more
(permutation-rf `(tup a | _r) `(tup b | _s))
more
more
destroy

; ;;
; Relational facts ;;
az (hn (brother jesus _person))
az (hn (brother fred john))
az (hn (brother fred mark))
az (hn (father mark mary))
(brother _who _whom)
more
more
more
(brother fred _x) _x
(brother jesus _x) _x
(brother _who _x) _x
more
more
(brother fred _x) (father _x mary)
199z
; ;;
; Functional facts ;;
az (ft (fatherof mary) fred)
az (ft (fatherof mark) fred)
az (ft (brotherof fred) john)
az (hn (elder fred john))
az (ft (brotherof jesus) _person)
az (hn (elder-jesus-brother) (elder (brotherof jesus) (brotherof jesus)))
spy
(_test)
more
nospy
destroy

; ;;
; Higher-order functions ;;
az (ft (twice1 _f) `(compose _f _f))
az (ft ((twice2 _f) _a) (`(compose _f _f) _a))
az (ft ((compose _f _g) | _a) (_f (_g | _a)))
az (ft (adofad1) (twice1 1+))
az (ft (adofad2) `(twice2 1+))
az (ft (add2 _x) ((twice1 1+) _x))
(`(compose 1+ *) 2 3)
(twice1 1+)
((twice1 1+) 0)
((adofad1) 0)
(add2 0)
(twice1 twice1)
(((twice1 twice1) 1+) 0)
(is _fct (((twice1 twice1) twice1) 1+)) (tup _fct (_fct 0))
consult mapper
azft (ncons-f _e) `(tup _e)
l
(mapper ncons-f `(tup a _x (tup 1 2) (tup _y _x _y) b))
(mapper (twice1 ncons-f) `(tup a _x (tup 1 2) (tup _y _x _y) b))
destroy

; ;;
; Modal logic ;;
az (hn (knows john (brother fred mary)))
az (hn (ask _prop _ind)
       (is _prop `(brother fred _ind))
       (knows john _prop))
(knows john _proposition)
(ask _p mary)
destroy

; ;;
; Embedded non-determinism ;;
az (ft (pet-f mary) canary)
az (ft (pet-f mary) doggy)
az (ft (pet-f mary) pony)
az (hn (mammal doggy))
az (hn (mammal pony))
(mammal (pet-f mary))
more
more
(_predicate (pet-f _being))
more
more
more
more
more
destroy

; ;;
; The call feature ;;
consult callex
azft (first-of-pair (pair _1 _2)) _1
l
(foo 1)
`(foo 1)
@(foo 1)
@`(foo 1)
(is _variable `(foo 1)) @_variable
(bar `(foo 1))
(baz `(first-of-pair `(pair (foo 1) (foo 2))))
(emb `(foo 1))
(tst)
destroy

; ;;
; Explicit "unknown" and "false" ;;
true
unknown
false
1 true 3
1 unknown 3
1 false 3
(is _a 1) true 3
(is _a 1) unknown 3
(is _a 1) false 3
(is _x unknown)
(is _x false)
1 `unknown 3
1 `false 3
(is _x `unknown)
(is _x `false)
(is _x `unknown) _x
(is _x `false) _x
(is _x `unknown) @_x
(is _x `false) @_x
(is _x `unknown) (is _y _x)
(is _x `false) (is _y _x)
(is _x `unknown) (is _y @_x)
(is _x `false) (is _y @_x)
(is _x `unknown) (tup _x 1 _x)
(is _x `false) (tup _x 1 _x)
spy
(is _x `unknown) (tup @_x 1 @_x)
(is _x `false) (tup @_x 1 @_x)
nospy
az (ft (notp true) false)
az (ft (notp false) true)
az (ft (likes john mary) false)
az (hn (likes john jeany))
az (ft (likes john nelly) unknown)
az (ft (tst) (is _res (likes john _whom)) `(_res _whom))
(likes john mary)
(notp (likes john mary))
(tst)
more
more

; ;;
; Deriving negative facts ;;
az (ft (negat (married john)) (happy john))
az (ft (negat (happy john)) (married john))
az (hn (happy john))
az (ft (negat _p) (notp @_p))
az (ft (happy fred) false)
l
(happy fred)
(notp (happy fred))
(negat `(happy fred))
(married john)
(notp (married john))
(negat `(married john))
destroy

; ;;
; The initial cut operator ;;
consult fibcut
l
(fib 0)
more
(fib 1)
more
(fib _res)
more
(fib 2)
(fib 4)
destroy

; ;;
; A (tup) list generator ;;
az (ft (genlst) `(tup))
az (ft (genlst) (tup `(tup) | (genlst)))
(genlst)
more
more
more
destroy

; ;;
; Lazy evaluation through non-determinism ;;
consult lazydiff
l
(seqnat 1)
more
more
more
more
(seqsqu 1)
more
more
more
more
(nths 3 (seqsqu 1))
destroy

; ;;
; Anonymous variable "id" in clause heads ;;
az (hn (knows id _x) (president _x))
az (hn (president reagan))
(knows john reagan)
(knows mary _whom)
destroy

; ;;
; Static flattening ;;
az (ft (f _1 _2) (g (h _1 _2)) (is _4 (h _5 _6)) (h _1 _2) (g (h _8 _9)))
az (hn (h a b))
az (ft (g true) ok)
az (ft (tst) (f (is _x a) b))
az (ft (isis _x _y) (is _x (is _y (f a b))))
az (ft (intst) (is _x a) (f | `(tup _x b)))
azft (second (tup _1 _2 | _rem)) _2
l
(f a b)
spy
(f (first `(tup a b)) (second `(tup a b)))
nospy
(tst)
(isis _r _r)
(intst)
flatten
l
static
(f a b)
spy
(f (first `(tup a b)) (second `(tup a b)))
nospy
(tst)
(isis _r _r)
(intst)
destroy
az (ft (l) `(tup 2 3))
az (ft (catst dot) (is _x `(l)) (tup a | @_x))
az (ft (catst lst) (is _x `(l)) (tup a @_x))
flatten
l
(catst _which)
more

bye
