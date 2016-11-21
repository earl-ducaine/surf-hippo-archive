#|	If someone uses a symbol of this package, he should write
	something like (use-package 'colab-keys)
	after this file has been loaded and before the keyword symbols are used
	in any way.
|#
#-:kcl (defpackage COLAB-KEYS)
(in-package "COLAB-KEYS")
(use-package "USER")
(export '( fw tx
	   cn
	   all
	   hn ft ad rl
           bottom top
	   asse doma conc pred attr role prim indi 
           agree disagree forall some apred cpred ofam cfam sfam sprim
	   equal unequal and or not
           ro <>
           rolefact attrfact fact attrterm hierarchy
	   lispeval up tt dd pc lc rc cc cv ci di hrcl dc
	   convert hacinit ctrace local global reconsult
	  ))
