;;; -*- Mode: LISP; Syntax: Common-lisp; Package: #-parallel surf #+parallel *surf; Base: 10 -*-
;;; (c) Copyright 1985, Don Webber, Thinking Machines Corporation, Inc.
;;; alterations (c) Copyright 1990, Lyle Borg-Graham, MIT Center for Biological Information Processing


#-parallel
(in-package "SURF-HIPPO"
	    :use '("COMMON-LISP-USER" "COMMON-LISP")
	    :nicknames '("SURF"))
#+parallel 
(in-package "*SURF")


(defun create-core-element (elt nd)
  "Calls the create-core routine of the appropriate type"
  (typecase elt
    (resistor (funcall (model-template-create-core-routine
			 (gethash (string "resistor") *model-hash-table*))
		       elt nd))
    (capacitor (funcall (model-template-create-core-routine
			 (gethash (string "capacitor") *model-hash-table*))
		       elt nd))
    (isource (funcall (model-template-create-core-routine
			 (gethash (string "isource") *model-hash-table*))
		       elt nd))
    (vsource (funcall (model-template-create-core-routine
			 (gethash (string "vsource") *model-hash-table*))
		       elt nd))
    #+mos1 (mos1 (funcall (model-template-create-core-routine
			 (gethash (string "mos1") *model-hash-table*))
		       elt nd))
     (soma (funcall (model-template-create-core-routine
			 (gethash (string "soma") *model-hash-table*))
		       elt nd))
     (channel (funcall (model-template-create-core-routine
			 (gethash (string "channel") *model-hash-table*))
		       elt nd))
     (synapse (funcall (model-template-create-core-routine
			 (gethash (string "synapse") *model-hash-table*))
		       elt nd))
     (particle (funcall (model-template-create-core-routine
			 (gethash (string "particle") *model-hash-table*))
		       elt nd))
     (conc-part (funcall (model-template-create-core-routine
			 (gethash (string "conc-part") *model-hash-table*))
		       elt nd))
     (conc-int (funcall (model-template-create-core-routine
			 (gethash (string "conc-int") *model-hash-table*))
		       elt nd))
     (segment (funcall (model-template-create-core-routine
			 (gethash (string "segment") *model-hash-table*))
		       elt nd))
    (t (sim-error "Don't know type of element ~a" elt))))

(defun add-off-diag (elt nd1 nd2 off-diag)
  "Calls the add-off-diag routine of the appropriate type"
  (typecase elt
    (resistor (funcall (model-template-add-off-diag-routine
			 (gethash (string "resistor") *model-hash-table*))
		       elt nd1 nd2 off-diag))
    (capacitor (funcall (model-template-add-off-diag-routine
			 (gethash (string "capacitor") *model-hash-table*))
		       elt nd1 nd2 off-diag))
    (isource (funcall (model-template-add-off-diag-routine
			 (gethash (string "isource") *model-hash-table*))
		       elt nd1 nd2 off-diag))
    (vsource (funcall (model-template-add-off-diag-routine
			 (gethash (string "vsource") *model-hash-table*))
		       elt nd1 nd2 off-diag))
    #+mos1 (mos1 (funcall (model-template-add-off-diag-routine
			 (gethash (string "mos1") *model-hash-table*))
		       elt nd1 nd2 off-diag))
     (soma (funcall (model-template-add-off-diag-routine
			 (gethash (string "soma") *model-hash-table*))
		       elt nd1 nd2 off-diag))
     (channel (funcall (model-template-add-off-diag-routine
			 (gethash (string "channel") *model-hash-table*))
		       elt nd1 nd2 off-diag))
     (synapse (funcall (model-template-add-off-diag-routine
			 (gethash (string "synapse") *model-hash-table*))
		       elt nd1 nd2 off-diag))
     (particle (funcall (model-template-add-off-diag-routine
			 (gethash (string "particle") *model-hash-table*))
		       elt nd1 nd2 off-diag))
     (conc-part (funcall (model-template-add-off-diag-routine
			 (gethash (string "conc-part") *model-hash-table*))
		       elt nd1 nd2 off-diag))
     (conc-int (funcall (model-template-add-off-diag-routine
			 (gethash (string "conc-int") *model-hash-table*))
		       elt nd1 nd2 off-diag))
     (segment (funcall (model-template-add-off-diag-routine
			 (gethash (string "segment") *model-hash-table*))
		       elt nd1 nd2 off-diag))
    (t (sim-error "Don't know type of element ~a" elt))))

(defun find-element-coupling (nd elt)
  "Returns a pair of (node . value) telling how coupled that node is to nd."
  (typecase elt
    (resistor (funcall (model-template-find-coupling-routine
			 (gethash (string "resistor") *model-hash-table*))
		       nd elt))
    (capacitor (funcall (model-template-find-coupling-routine
			 (gethash (string "capacitor") *model-hash-table*))
		       nd elt))
    (isource (funcall (model-template-find-coupling-routine
			 (gethash (string "isource") *model-hash-table*))
		       nd elt))
    (vsource (funcall (model-template-find-coupling-routine
			 (gethash (string "vsource") *model-hash-table*))
		       nd elt))
    #+mos1 (mos1 (funcall (model-template-find-coupling-routine
			 (gethash (string "mos1") *model-hash-table*))
		       nd elt))
     (soma (funcall (model-template-find-coupling-routine
			 (gethash (string "soma") *model-hash-table*))
		       nd elt))
     (channel (funcall (model-template-find-coupling-routine
			 (gethash (string "channel") *model-hash-table*))
		       nd elt))
     (synapse (funcall (model-template-find-coupling-routine
			 (gethash (string "synapse") *model-hash-table*))
		       nd elt))
     (particle (funcall (model-template-find-coupling-routine
			 (gethash (string "particle") *model-hash-table*))
		       nd elt))
     (conc-part (funcall (model-template-find-coupling-routine
			 (gethash (string "conc-part") *model-hash-table*))
		       nd elt))
     (conc-int (funcall (model-template-find-coupling-routine
			 (gethash (string "conc-int") *model-hash-table*))
		       nd elt))
     (segment (funcall (model-template-find-coupling-routine
			 (gethash (string "segment") *model-hash-table*))
		       nd elt))
    (t (sim-error "Don't know type of element ~a" elt))))
