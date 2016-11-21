#-(or :symbolics :kcl) (defpackage TAXON)

#-(or :symbolics :kcl) (in-package "TAXON")
#+(or :symbolics :kcl) (in-package "TAXON" :nicknames '(TX))

(use-package "COLAB-KEYS")

(export '(
          ; VARIABLES

          ; FUNCTIONS

            tx
            *tx-readtable*
    ;        *col-list-of-taxon*

            colab-consult-taxon
            colab-list-taxon
            colab-destroy-taxon
            taxon-test
            taxon-else

    ;        pp-taxon
            add-taxon
            destroy-taxon

          ; MACROS

          ; HELP FUNCTIONS

          ; KEYWORDS

         ;    all
        ;    asse
        ;    indi
        ;    doma
        ;    conc
        ;    pred
        ;    attr
        ;    role
        ;    prim

            sati-co?
            fsubs?
            sati?
            subs?
     ;       classify
            subsumption
            equivalence-class
            im-lowers
            im-uppers
            instances
            attr-filler
            role-fillers
            attr-value-pairs
            role-value-pairs
            x-value-pairs

            fattr-filler
            frole-fillers
            fattr-value-pairs
            frole-value-pairs
            fx-value-pairs

            assert-ind?
            concept-closure
            realize-individual
            save-hierarchy
            restore-hierarchy

            get-concept-names get-instance-names get-role-names get-attr-names


            ;; new for Th. Labisch
          make-indi
          make-asse
          get-instance-names
          get-concept-names
          get-attr-names
          get-role-names

           top
  
          h-add-taxon
         
           h-attr-filler  h-attr-value-pairs  h-check h-classify
h-classify-all h-clear-abox h-clear-tbox h-clear-edom   h-clear-hi h-clear-realize
h-defconcept h-deffeature h-defind h-defpconcept  h-defpredicate  h-defrole
h-destroy-taxon h-edom-instal h-equivalence-class h-_get-concepts 
 h-filter-graph h-im-lowers  h-im-uppers  h-instance?  h-instances h-lowers
h-pp-taxon h-pp-taxon-parts h-print-hierarchy h-role-fillers h-realize-individual
h-subs? h-role-value-pairs h-sati?  h-uppers h-weakly-realize h-x-value-pairs
h-assert-ind? h-concept-closure h-switch h-my-trace h-my-untrace
h-assertion  h-make-indi h-make-asse h-get-concept-names  h-get-instance-names
h-get-role-names h-get-attr-names h-get-def h-lsc h-ihw h-dhw h-shw h-chw
h-iaw h-saw h-daw h-caw

h-apred h-cpred h-ofam h-cfam h-sfam


#|

add  attrfi attrpairs check-abox classify classify-all clear-abox clear-tbox
clear-edom clear-hi clear-realize  concept-closure
destroy equi get-attr-names get-concept-names get-instance-names get-role-names
ilo iup iloeq iupeq instance? instances help listing l lo l-realize make-asse
make-indi ppco pphi realizei rolefi rolepairs replace restorehi restorere savehi savere
switch spy nospy wrealize xpairs |#
          


          ))

