(setq test.extend.lfns
      '(cutlist falunk negat callex facut fibcut lazy ifthel inid stetst))

(setq cutlist
      '(((list . _x) _x)
        ((cons _x nil) (list _x))
        ((cons _x (_a . _d)) (list _x _a . _d))
        ((car (_a . _d)) _a)
        ((cdr (_a . _d)) _d)
        ((null nil))
        ((null (_a . _d)) false)
        (!(listp nil))
        (!(listp (_a . _d)))
        ((listp _x) false)
        (!(atom (_a . _d)) false)
        ((atom _x))))

(setq falunk
      '(((notp true) false)
        ((notp false) true)
        ((likes john mary) false)
        ((likes john jeany))
        ((likes john nelly) unknown)
        ((tst) (is _res (likes john _whom)) `(_res _whom))))

(setq negat
      '(((negat (married john)) (happy john))
        ((negat (happy john)) (married john))
        ((happy john))
        ((negat _p) (notp @_p))
        ((happy fred) false)
        ((notp true) false)
        ((notp false) true)))

(setq callex
      '(((foo _x) `(_x 2 3))
        ((bar _g) @_g)
        ((baz _h) (is _aux @_h) @_aux)
        ((emb _a) (list _a @_a a))
        ((fue _a) (@_a _a))
        ((tst)
         (is _x `(cons 1 2))
         (list _x
               `_x
               @_x
               `@_x
               @`_x
               (cons 1 2)
               `(cons 1 2)
               @(cons 1 2)
               `@(cons 1 2)
               @`(cons 1 2)))))

(setq facut '((!(fac 0) 1) ((fac _n) (times _n (fac (sub1 _n))))))

(setq fibcut
      '((!(fib 0) 1)
        (!(fib 1) 1)
        ((fib _n) (plus (fib (sub1 _n)) (fib (sub1 (sub1 _n)))))))

(setq lazy
      '(((seqnat _n) nil)
        ((seqnat _n) (cons _n (seqnat (add1 _n))))
        ((square _x) (times _x _x))
        ((seqsqu _n) (mapper square (seqnat _n)))
        ((nths 1 (_a . _d)) _a)
        ((nths _n (_a . _d)) (nths (sub1 _n) _d))))

(setq ifthel
      '(((ifthel true _then _else) @_then)
        ((ifthel false _then _else) @_else)))

(setq inid '(((knows id _x) (president _x)) ((president reagan))))

(setq stetst
      '(((l) `(2 3))
        ((catst dot) (is _x `(l)) (list a call _x))
        ((catst lst) (is _x `(l)) (list a @_x))))

