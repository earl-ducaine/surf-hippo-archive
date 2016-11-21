;;; -*- Base: 10; Syntax: Cltl; Mode: LISP; Package: CONTAX -*-


;;; module: CONVERT HIERACHY

;;; providing a compilation of TAXON hierarchies towards CONTAX hierarchies


;;; FIXED ON 12-05-1992 fs

;;; Please enter your name and the date together with a short description,
;;; (at least the name of the function) if you change the sources.

;;; CHANGES:
;;; 13-06-92 fs
;;;   Symbols containing a TAXON package reference will only be read, if TAXON has been
;;;   loaded (ie. *features* contains :taxon). Thus, if you want to run this module, you
;;;   will have to load it AFTER TAXON has been loaded.

( in-package 'contax )

;;mm;; (export 'colab-convert-hierarchy)

( defun colab-convert-hierarchy ( dummy )
  ;; top is the upper bound of the TAXON hierarchy
  ( declare ( ignore dummy ))
  ( build-hierarchy-recursively 'colab-keys:top )
  ( values T ))

( defun colab-convert-subhierarchy ( concept-id )
  ( tx::classify-all )
  ( build-hierarchy-recursively concept-id ))

(defun build-hierarchy-recursively (TAXON-concept-id)
  (cond ((null TAXON-concept-id))	; finished
	(t (let (( direct-subconcepts ( tx::filo TAXON-concept-id ))
		 ( CONTAX-concept-id  ( if ( eq TAXON-concept-id 'colab-keys:top )
					  'top
					  TAXON-concept-id )))
		      ;; bottom is not used in the hierarchy but may be added
		      ;; later on: change this condition to:
		      ;;  (def-concept-function TAXON-concept-id 'colab-keys:bottom)
			 (if (equal direct-subconcepts '( colab-keys:bottom ))
			     ;; def-concept-function is predefined by Christoph
			     (contax::def-concept-fn CONTAX-concept-id nil)
			     (progn
			       (contax::def-concept-fn CONTAX-concept-id direct-subconcepts)
			       ;; call recursively for all subconcepts
			       (mapc #'build-hierarchy-recursively direct-subconcepts)))))))






