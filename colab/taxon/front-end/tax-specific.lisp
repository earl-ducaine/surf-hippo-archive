(in-package "USER")

#| **************************************************************************
   taxon specific primitives 
   *********************************************************************** |#


(setq *col-list-of-taxon* ; List of commands, which taxon can interpret
  '(
    (asse "" nil taxon::h-assertion nil "make some A-box assertions (concept ind) (ind role/attr ind)")
    (attrterm "" nil taxon::h-assert-ind? nil "assert an attribute term")
    (attrfi "" nil taxon::h-attr-filler nil "<attribute> <individual>; a list of all attribute fillers (made known to the system by an assertion)")
    (attrpairs "" nil taxon::h-attr-value-pairs nil "<individual>; the fillers for all attributes (made known by assertions); output: ( (attribute-id filler-id+)* ) ")
    (check-abox "" nil taxon::h-check nil "check A-box for consistency")
    (clear-abox "" nil taxon::h-clear-abox nil "clear TAXON's ABox (delete individuals and assertions)")
    (clear-tbox "" nil taxon::h-clear-tbox nil "clear TAXON's TBox (delete TBox and subsumption hierarchy)")
    (concept-closure "" nil taxon::h-concept-closure nil "<individual>; all concepts an individual is an instance of (only for individuals inserted by attrterm)")
    (role "" nil taxon::h-defrole nil "define a role")
    (attr "" nil taxon::h-deffeature nil "define an attribute")
    (prim "" nil taxon::h-defpconcept nil "define a primitive concept")
    (ofam "" nil taxon::h-ofam nil "declare an open family of mutually disjoint primitive concepts")
    (cfam "" nil taxon::h-cfam nil "declare a closed family of mutually disjoint primitive concepts")
    (sfam "" nil taxon::h-sfam nil "declare a subset of a closed family as a subfamily")
    (cpred "" nil taxon::h-defpredicate nil "define a predicate of a concrete domain")
    (apred "" nil taxon::h-apred nil "define a predicate of the abstract domain")
    (conc "" nil taxon::h-defconcept nil "define a compound concept")
    (hierarchy "" nil taxon::h-hierarchy nil "mark concept names as interesting")

    (indi "" nil taxon::h-defind nil "define an individual of the abstract domain")
    (destroy "" nil taxon::h-destroy-taxon nil "clear all the TAXON specific definitions")
    (def "" nil taxon::h-get-def nil "get definition of a TBox object")
    (equi "" nil taxon::h-equivalence-class nil "<concept>; look-up equivalence class of concept (only for classified concepts)")
    (filter "" nil taxon::h-filter-graph nil "filter subsumption graph")

     (get-attr-names "" nil taxon::h-get-attr-names nil "a list of all attributes")
     (get-concept-names "" nil taxon::h-get-concept-names nil "a list of all interesting concepts")
     (get-instance-names "" nil taxon::h-get-instance-names nil "a list of all abstract individuals")
     (get-role-names "" nil taxon::h-get-role-names nil "a list of all role names")
     
    (ilo "" nil taxon::h-im-lowers nil "<concept>; look-up immediate lowers ( see equi )")
    (iup "" nil taxon::h-im-uppers nil "<concept>; look-up immediate uppers ( see equi )")
    (instance? "" nil taxon::h-instance? nil "compute wheather <individual> is an instance of <concept> ?")
    (instances "" nil taxon::h-instances nil "compute a list of all instances of <concept term>")
    (listing "" nil taxon::h-pp-taxon nil "get a pretty printed listing of the TAXON database")
    (lsc "" nil taxon::h-lsc nil "list separation classes of disjoint primitives")
    (l "" nil taxon::h-pp-taxon-parts nil "prim/sprim/ofam/cfam/sfam/conc/role/attr/asse/cpred/apred; list a part of the TAXON database")
    (lo "" nil taxon::h-lowers nil "all concepts being lower in the subsumption hierarchy than a given concept ( see equi )")
;   (l-realize "" nil taxon::h-l-realize nil "<concept> | <individual>; realization-results (only for individuals inserted by attrterm)")

;    (make-asse "" nil taxon::h-make-asse nil "make an assertion")
;    (make-indi "" nil taxon::h-make-indi nil "create an abstract individual")

    (ppco "" nil taxon::h-_get-concepts nil "a list of all concept names declared in the TBox")
   
    (pphi "" nil taxon::h-print-hierarchy nil "print the computed subsumption hierarchy")
    (realizei "" nil taxon::h-realize-individual nil "<individual>; compute realization")
    (rolefi "" nil taxon::h-role-fillers nil "<role> <individual>; find a list of all role fillers of an individual" )
    (rolepairs "" nil taxon::h-role-value-pairs nil "<individual>; find the fillers for all roles (made known by assertions); output: ( (role-id filler-id+)* ) ")
    (sati? "" nil taxon::h-sati? nil "<concept term>; satisfiable ?")
    (subs? "" nil taxon::h-subs? nil "Does <concept> subsume <concept> ?" )
    (switch "" nil taxon::h-switch nil "filtered-graph|reduced-abox|draw-all|draw-bottom|show -- t | nil")
    (spy "" nil taxon::h-my-trace nil "trace classify, assert-ind?, concept-closure")
    (nospy "" nil taxon::h-my-untrace nil "untrace classify, assert-ind?, concept-closure")
    (up "" nil taxon::h-uppers nil "<concept term>; look-up immediate uppers ( see equi )")
    (wrealize "" nil taxon::h-weakly-realize nil "compute most specific concepts compatible with <individual>")
    (xpairs "" nil taxon::h-x-value-pairs nil "<individual>; all role/attribute-fillers (made known by an assertion); output: ( (attr/role-id filler-id+)* )")

    (drawhi "" nil taxon::h-drawhi nil "draw hierarchy into ps-file; keywords (name hierarch.ps) (top top) (shrink nil) (insert nil)")

    (ihw "" nil taxon::h-ihw nil "initialize hierarchy window")
    (dhw "" nil taxon::h-dhw nil "draw hierarchy into the window")
    (shw "" nil taxon::h-shw nil "select hierarchy window")
    (chw "" nil taxon::h-chw nil "clear hierarchy window")

    (iaw "" nil taxon::h-iaw nil "initialize window for abox presentation")
    (daw "" nil taxon::h-daw nil "<individual>+; present assertions concerning the individuals")
    (saw "" nil taxon::h-saw nil "select abox presentation window")
    (caw "" nil taxon::h-caw nil "clear abox presentation window")
    
)
)


#|
;(let (#+genera (sys:inhibit-fdefine-warnings :just-warn))


(defun colab-consult-taxon (elem)
  (taxon::h-add-taxon elem)
  NIL)

(defun colab-destroy-taxon (dummy)
  (taxon::h-destroy-taxon dummy)
  NIL)

(defun colab-list-taxon (elem)
  (pp-taxon)
  NIL)

(defun taxon-test (x)(princ "taxon:")(print x) NIL)

(defun taxon-else (userline)
	(let ((*print-case* :downcase)) (princ (format nil "Invalid request: ~% ~A ~%" userline)))
	nil)

;)

|#
