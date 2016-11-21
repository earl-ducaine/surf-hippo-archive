(setq test.lfns
      '(fac consinst
            purelist
            append-f
            append-r
            cmlist
            perm
            id
            reltst
            br
            nd
            genlst
            hior
            mapper
            arith
            statst
            modal
            byvalue-vs-quote
            instex
            mgu
            pet
            bird
            chair
            clyde
            palinr))

(setq fac
      '(((fac 0) 1)
        ((fac _n) (times _n (fac (sub1 _n))))
        ((times 3 2) 6)
        ((times 2 1) 2)
        ((times 1 1) 1)
        ((sub1 3) 2)
        ((sub1 2) 1)
        ((sub1 1) 0)))

(setq consinst '(((cons _a _b) `(_a . _b))))

(setq purelist
      '(((list . _elements) _elements)
        ((cons _x nil) (list _x))
        ((cons _x (_head . _tail)) (list _x _head . _tail))
        ((car (_head . _tail)) _head)
        ((cdr (_head . _tail)) _tail)
        ((null nil))
        ((listp nil))
        ((listp (_head . _tail)))))

(setq append-f
      '(((append-f nil _lst) _lst)
        ((append-f (_head . _tail) _lst) (cons-f _head (append-f _tail _lst)))
        ((cons-f _head _tail) `(_head . _tail))))

(setq append-r
      '(((append-r nil _lst _lst))
        ((append-r (_head . _tail) _lst (_head . _rest))
         (append-r _tail _lst _rest))))

(setq cmlist
      '(((last-rf _e (_e)) _e)
        ((last-rf _e (_head . _tail)) (last-rf _e _tail))
        ((nextto-rf _x _y (_x _y . _any)) `(_x _y))
        ((nextto-rf _x _y (_any . _z)) (nextto-rf _x _y _z))
        ((append-rf nil _lst _lst) _lst)
        ((append-rf (_head . _tail) _lst (_head . _rest))
         (append-rf _tail _lst _rest)
         `(_head . _rest))
        ((member-rf _e (_e . _rest)) `(_e . _rest))
        ((member-rf _x (_y . _rest)) (member-rf _x _rest))
        ((rev-rf nil nil) nil)
        ((rev-rf (_head . _tail) _lst)
         (rev-rf _tail _x)
         (append-rf _x `(_head) _lst)
         _lst)
        ((efface-rf _a (_a . _l) _l) _l)
        ((efface-rf _a (_b . _l) (_b . _m))
         (efface-rf _a _l _m)
         `(_b . _m))))

(setq perm
      '(((permutation-rf nil nil) nil)
        ((permutation-rf _lst (_head . _tail))
         (append-rf _v `(_head . _u) _lst)
         (append-rf _v _u _w)
         (permutation-rf _w _tail)
         `(_head . _tail))
        ((append-rf nil _lst _lst) _lst)
        ((append-rf (_head . _tail) _lst (_head . _rest))
         (append-rf _tail _lst _rest)
         `(_head . _rest))))

(setq id
      '(((inst-variable=variable) (is _x 1) (identity `_x))
        ((dot-anfrage) (identity (_r . _s)))
        ((identity _x) _x)))

(setq reltst
      '(((fatherof mary) fred)
        ((fatherof mark) fred)
        ((brotherof fred) john)
        ((elder fred john))
        ((brotherof jesus) _person)
        ((elder-jesus-brother) (elder (brotherof jesus) (brotherof jesus)))))

(setq br
      '(((brother jesus _person))
        ((brother fred john))
        ((brother fred mark))
        ((father mark mary))))

(setq nd '(((f _a) (g _a)) ((g _a) (i _a)) ((f _a) (h _a)) ((h _a))))

(setq genlst '(((genlst) nil) ((genlst) (cons nil (genlst)))))

(setq hior
      '(((twice1 _f) `(compose _f _f))
        (((twice2 _f) _a) (`(compose _f _f) _a))
        (((compose _f _g) . _a) (_f (_g . _a)))
        ((adofad1) (twice1 add1))
        ((adofad2) `(twice2 add1))
        ((add2 _x) ((twice1 add1) _x))))

(setq mapper
      '(((mapper _f nil) nil)
        ((mapper _f (_head . _tail)) (cons-f (_f _head) (mapper _f _tail)))
        ((cons-f _head _tail) (inst (_head . _tail)))))

(setq arith
      '(((addition _nat _rep) (is-zero _rep) _nat)
        ((addition _nat _rep) (s-succ (addition _nat (s-pred _rep))))
        ((is-zero (0)))
        ((is-zero (0 . _digs)) (is-zero _digs))
        ((s-succ nil) `(1))
        ((s-succ (0 . _digs)) `(1 . _digs))
        ((s-succ (1 . _digs)) `(2 . _digs))
        ((s-succ (2 . _digs)) `(3 . _digs))
        ((s-succ (3 . _digs)) `(4 . _digs))
        ((s-succ (4 . _digs)) `(5 . _digs))
        ((s-succ (5 . _digs)) `(6 . _digs))
        ((s-succ (6 . _digs)) `(7 . _digs))
        ((s-succ (7 . _digs)) `(8 . _digs))
        ((s-succ (8 . _digs)) `(9 . _digs))
        ((s-succ (9 . _digs))
         (is _succ-of-rest (s-succ _digs))
         `(0 . _succ-of-rest))
        ((s-pred (0 . _digs))
         (is _pred-of-rest (s-pred _digs))
         `(9 . _pred-of-rest))
        ((s-pred (1 . _digs)) `(0 . _digs))
        ((s-pred (2 . _digs)) `(1 . _digs))
        ((s-pred (3 . _digs)) `(2 . _digs))
        ((s-pred (4 . _digs)) `(3 . _digs))
        ((s-pred (5 . _digs)) `(4 . _digs))
        ((s-pred (6 . _digs)) `(5 . _digs))
        ((s-pred (7 . _digs)) `(6 . _digs))
        ((s-pred (8 . _digs)) `(7 . _digs))
        ((s-pred (9 . _digs)) `(8 . _digs))
        ((list . _elements) _elements)))

(setq statst
      '(((f _1 _2) (g (h _1 _2)) (is _4 (h _5 _6)) (h _1 _2) (g (h _8 _9)))
        ((h a b))
        ((g true) ok)
        ((tst) (f (is _x a) b))
        ((isis _x _y) (is _x (is _y (f a b))))
        ((intst) (is _x a) (f inst (_x b)))))

(setq modal
      '(((knows john (brother fred mary)))
        ((ask _prop _ind)
         (is _prop `(brother fred _ind))
         (knows john _prop))))

(setq byvalue-vs-quote
      '(((byvalue-vs-quote)
         (f (g `(h a) (k b)) (list q (r c) `(s d))))
        ((k b) e)
        ((r c) f)
        ((g (h a) _x) _x)
        ((f e (q f (s d))) quote)))

(setq instex
      '(((recpat 0) `(0))
        ((recpat _n) (is _h (recpat (sub1 _n))) `(_h _n _h))
        ((f _x) `(a b _x _x b a))
        ((g _y _z) (is _h (f (f _y))) `(_h _z _h))
        ((try) (g (g 1 2) (g 3 4)))))

(setq mgu '(((mgu _x _x)) ((mgi _x _x) _x)))

(setq pet
      '(((pet-f mary) canary)
        ((pet-f mary) doggy)
        ((pet-f mary) pony)
        ((mammal doggy))
        ((mammal pony))))

(setq bird
      '(((hap _x) (bird _x) wings)
        ((bird _x) (airbird _x))
        ((airbird tingy))
        ((can _x) (airbird _x) fly)))

(setq chair
      '(((red chair) `(red chair))
        ((big chair) `(big chair))
        ((big table) `(big table))
        ((chair)
         (append `(the) (big _thing) `(is a) (red _thing)))))

(setq clyde
      '(((elephant clyde))
        ((color _x red) (fire-hydrant _x))
        ((color _x grey) (elephant _x))))

(setq palinr
      '(((palindrome))
        ((palindrome _x))
        ((palindrome _x . _y) (append-r _z `(_x) _y) (palindrome . _z))))
