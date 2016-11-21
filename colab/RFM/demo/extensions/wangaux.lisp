(hn (atomic (_f | _r)) ! unknown)
(hn (atomic _x))


;(ft (member _x (tup _x | _r)) `(tup _x | _r))
;(ft (member _x (tup id | _y)) (member _x _y))

(hn (member _x (tup _x | id)))
(hn (member _x (tup id | _y)) (member _x _y))




;(ft (wang _l _r) (is _tree (work _l _r `(tup) `(tup))) ! _tree)
(ft (wang _l _r) (work _l _r `(tup) `(tup)) !)
(ft (wang _l _r) false)


(ft (2.45)
    (wang `(tup (not (or p q))) `(tup (not p))))

(ft (5.21)
    (wang `(tup) `(tup (impl (and (not p) (not q)) (equiv p q)))))

(ft (demorgan)
    (wang `(tup) `(tup (equiv (not (and p q)) (or (not p) (not q))))))

(ft (composition)
    (wang `(tup) `(tup (impl (impl p q) (impl (impl p r) (impl p (and q r)))))))

(ft (democomp)
    (wang `(tup)
          `(tup (equiv (equiv (not (and p q)) (or (not p) (not q)))
                       (impl (impl p q)
                             (impl (impl p r) (impl p (and q r))))))))

(ft (demonotcomp)
    (wang `(tup)
          `(tup (equiv (equiv (not (and p q)) (or (not p) (not q)))
                       (not
                        (impl (impl p q)
                              (impl (impl p r) (impl p (and q r)))))))))

(ft (associativity)
    (wang `(tup) `(tup (impl (and p (and q r)) (and (and p q) r)))))

(ft (notheorem)
    (wang `(tup) `(tup (or a (impl b a)))))
