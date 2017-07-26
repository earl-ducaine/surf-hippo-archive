;;; -*- Mode: lisp; package: user; base: 10;  Syntax: Common-lisp; -*-
;;; (c) Copyright 1985, Don Webber, Thinking Machines Corporation, Inc.
;;; alterations (c) Copyright 1985, Geoff Wong, Thinking Machines Corporation, Inc.
;;; alterations (c) Copyright 1990, Lyle Borg-Graham, MIT Center for Biological Information Processing
;;; File creation date: 6/09/85 11:23:13

(unless (fs:get-pathname-host "SURF" t) (fs:make-logical-pathname-host "SURF"))

; The following compiler flags are possible:
;(zl:sstatus feature parallel)	; This flag is for the code to run on the CM

;(zl:sstatus feature sun)
(zl:sstatus feature off-diag)	; Use tri-diagonal ( or perhaps arbitrary bandwidth ) matrix

; Uncomment them as desired, but be sure, if you change them, to recompile 
; the entire system ( i.e. (compile-system 'surf :recompile t) ).

#-parallel
(defpackage surf (:use SCL) :colon-mode)
#+parallel
(defpackage *surf (:use *lisp SCL )D,#TD1PsT[Begin using 006 escapes](1 0 (NIL 0) (NIL :ITALIC NIL) "CPTFONTI"))

0(defsystem surf
    (:journal-directory "surf:surf;patch;"
     :patchable t
     :default-pathname "surf:surf;"
     :default-package #-parallel 'surf #+parallel '*surf)
  (:serial
    "macros" "declare" "subs" "devices"
    #+parallel "struct"
    "init" "sim"     "node"       "step"
    #+parallel "return"
    (:parallel
      "res" "cap"
      "soma"  "segment"
      "vsource" "isource"
      (:serial (:parallel "currents" "new-currents" ) "conc-int" "channel" "conc-part"  "particle" "synapse")
      "cell"
      "misc"
      "fix" "read" "print"
      #+(and off-diag (not parallel)) "tri-ge"
      #+(and off-diag parallel) "cr11"
;This is for the Symbolics-based user interface
      #-sun (:serial "output-frame" "plot"  "cell-graphics")
      #-sun "symbolics-misc"
      #-sun "general-menus"
      #-sun "currents-menus"
      "read-me"
      "main")					;This is here for loading purposes.
; Some neuron circuit files....
;    "surf:hippocampus;hippos"
;    "surf:turtle;off-amacrine" "surf:turtle;a21" "surf:turtle;g20" "surf:turtle;ds-model"
;    "surf:rabbit;star-amacrine"
    ))

 