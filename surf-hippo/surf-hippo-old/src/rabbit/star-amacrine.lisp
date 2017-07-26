;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: #-PARALLEL SURF #+PARALLEL *surf; Base: 10; -*-



#-parallel
(in-package "SURF-HIPPO"
	    :use '("COMMON-LISP-USER" "COMMON-LISP")
	    :nicknames '("SURF"))
#+parallel 
(in-package "*SURF")



;;; Cholinergic rabbit retina starburst AMACRINE cell.

(defun star-amacrine (cell-name &key (cell-origin '(0.0 0.0 0.0)) (synapse nil)
				(segment-diameter 0.20)(include-distal-a-current)
				(extras-list '()))
  (setq *soma-radius 8.0)
  (create-cell-type "star-amacrine" :soma-resistivity *r-mem-soma)
  (create-cell cell-name :cell-type-name "star-amacrine" :cell-origin cell-origin)
  (let ((soma (create-soma (format nil "~a-soma" cell-name) cell-name (* 2.0 *soma-radius))))
    (create-channels '(na1 na2 na3 dr a) soma :save-current t)
    (if *include-sources (create-source *clamp-type soma))
    (setf
     (cell-type-notes (gethash "star-amacrine" cell-type-hash-table))
     "Rabbit ACh starburst cell (cell 7) (about 1.25 mm from streak) of Tauchi and Masland (1984).~%")
    (create-tree soma

;;; This file contains the coordinates of the nodes for Cell 7 (about
;;; 1.25 mm from streak) of Tauchi and Masland (1984).  The metric
;;; unit for the table's coordinate entries corresponds to about 6.06
;;; microns.  To a good approximation, the cell body is an ellipsoid
;;; with vertical major axis (of about 18 micrometers) and horizontal
;;; minor axis (of about 12 micrometers).  The two first strings in
;;; each entry are the names of the proximal and distal nodes of each
;;; segment.  On the other hand, the last two strings are the
;;; coordinates of the distal node.

;;; Famiglietti (1983) notes - Little overlap in dendritic fields.
;;; Tapering of primary branches until approx 2nd order branch on
;;; average 50 uM from soma.  Proximal region gradually tapering
;;; dendrites of no more than 1.5 uM at origin.  At the 3rd or 4th
;;; order of branching dendrites become thin rather abruptly
;;; (transition between proximal and intermediate zones).  3 annular
;;; zones (proximal,intermediate and distal). Intemediate and distal
;;; zones are approx .2 to .4 uM diameter.  Distal zone marked by
;;; varicosities and boutons.

;;; Miller and Bloomfield (1983) notes - They claim that distal
;;; regions (with varicosities) are thicker than the intermediate
;;; zone, and imply that this difference is due to different staining
;;; technique than Famiglietti (who uses Golgi) (Miller and Bloomfield
;;; use HRP) (Masland uses DAPI and LY). Soma on the order of 11uM
;;; diameter. Thin dendrites (intermediate zone ??) are estimated at
;;; much less than 0.5 uM, perhaps 0.1 uM.  !!!!!! They claim that 1st
;;; order branches are typically about 0.1 uM in diameter. !!!!!! If
;;; true, this of course changes the communication between major
;;; branches drastically.
;;

;;;; The morphological coordinates are from Tauchi and Masland (1984).
;;;; The dendrite diameters are inferred from Famiglietti (1983). Note
;;;; that diameter entries are in microns - the xy entries are in
;;;; units as noted above.

;;; The segment-list format is as follows: (mother-segment-name
;;; segment-name x y z diameter extras-list)
		 `(
		   (soma		1a		7		-1  -5          1.2    ,extras-list)
		   (1a		1b		12		-3  -7          0.6    ,extras-list)
		   (1b		11		18		-5  -10     nil      ,extras-list)
		   (11		111		20		-3  -10 nil ,extras-list)
		   (111		1111		30		-3  -10 nil ,extras-list)
		   (111		1112a		30		-5  -10 nil ,extras-list)
		   (1112a		1112b		33		-5  -10 nil ,extras-list)
		   (11		112		30		-6  -10 nil ,extras-list)
		   (11		113a		17		-7  -10 nil ,extras-list)
		   (113a		113b		24		-9  -10 nil ,extras-list)
		   (113b		1131		28		-9  -10 nil ,extras-list)
		   (113b		1132		24		-11  -10 nil ,extras-list)
		   (1b		12a		11		-6  -10          nil ,extras-list)
		   (12a		12b		24		-14  -10         nil ,extras-list)
		   (12b		121		28		-14  -10 nil ,extras-list)
		   (12b		122		34		-17  -10 nil ,extras-list)
		   (soma		2		-2		0  -5          1.2  ,extras-list)
		   (2		21		-3		-2  -7         1.0  ,extras-list)
		   (21		211		5		-6  -10       0.6  ,extras-list)
		   (211		2111		16		-14  -10 nil ,extras-list)
		   (2111		21111a		17		-13  -10 nil ,extras-list)
		   (21111a		21111b		21		-15  -10 nil ,extras-list)
		   (21111b		211111a		23		-16  -10 nil ,extras-list)
		   (211111a		211111b		27		-16  -10 nil ,extras-list)
		   (21111b		211112a		22		-17  -10 nil ,extras-list)
		   (211112a		211112b		25		-20  -10 nil ,extras-list)
		   (2111		21112		19		-18  -10 nil ,extras-list)
		   (2111		21113		21		-23  -10 nil ,extras-list)
		   (211		2112		9		-13  -10 nil ,extras-list)
		   (2112		21121a		13		-14  -10 nil ,extras-list)
		   (21121a		21121b		13		-17  -10 nil ,extras-list)
		   (21121b		211211		18		-22  -10 nil ,extras-list)
		   (21121b		211212		17		-24  -10 nil ,extras-list)
		   (2112		21122		11		-16  -10 nil ,extras-list)
		   (2112		21123a		8		-16  -10 nil ,extras-list)
		   (21123a		21123b		12		-23  -10 nil ,extras-list)
		   (21123b		211231		14		-24  -10 nil ,extras-list)
		   (21123b		211232		12		-26  -10 nil ,extras-list)
		   (21		212a		-5		-3  -10     0.8  ,extras-list)
		   (212a		212b		-5		-5  -10     0.6  ,extras-list)
		   (212b		2121		-4		-7  -10 nil ,extras-list)
		   (2121		21211		3		-11  -10 nil ,extras-list)
		   (21211		212111		10		-22  -10 nil ,extras-list)
		   (21211		212112		3		-18  -10 nil ,extras-list)
		   (212112		2121121		9		-25  -10 nil ,extras-list)
		   (212112		2121122a		1		-19  -10 nil ,extras-list)
		   (2121122a		2121122b		5		-23  -10 nil ,extras-list)
		   (21211		212113a		-1		-11  -10 nil ,extras-list)
		   (212113a		212113b		0		-16  -10 nil ,extras-list)
		   (2121		21212a		-7		-10  -10 nil ,extras-list)
		   (21212a		21212b		-7		-13  -10 nil ,extras-list)
		   (21212b		212121a		5		-24  -10 nil ,extras-list)
		   (212121a		212121b		3		-25  -10 nil ,extras-list)
		   (21212b		212122		-7		-17  -10 nil ,extras-list)
		   (212122		2121221a		-2		-19  -10 nil ,extras-list)
		   (2121221a		2121221b		0		-27  -10 nil ,extras-list)
		   (212122		2121222		-7		-20  -10 nil ,extras-list)
		   (2121222		21212221		-5		-26  -10 nil ,extras-list)
		   (2121222		21212222		-5		-29  -10 nil ,extras-list)
		   (212b		2122a		-8		-6  -10 nil ,extras-list)
		   (2122a		2122b		-12		-13  -10 nil ,extras-list)
		   (2122b		21221a		-10		-14  -10 nil ,extras-list)
		   (21221a		21221b		-14		-26  -10 nil ,extras-list)
		   (2122b		21222		-13		-15  -10 nil ,extras-list)
		   (21222		212221		-14		-20  -10 nil ,extras-list)
		   (212221		2122211a		-14		-23  -10 nil ,extras-list)
		   (2122211a		2122211b		-16		-23  -10 nil ,extras-list)
		   (212221		2122212		-17		-23  -10 nil ,extras-list)
		   (2122212		21222121		-19		-26  -10 nil ,extras-list)
		   (2122212		21222122		-19		-20  -10 nil ,extras-list)
		   (21222		212222		-16		-16  -10 nil ,extras-list)
		   (2		22		-4		0  -10        1.0 ,extras-list)
		   (22		221		-8		-2  -10       0.8 ,extras-list)
		   (221		2211		-10		-6  -10       0.6 ,extras-list)
		   (2211		22111a		-10		-8  -10 nil ,extras-list)
		   (22111a		22111b		-19		-16  -10 nil ,extras-list)
		   (22111b		221111a		-25		-23  -10 nil ,extras-list)
		   (221111a		221111b		-27		-22  -10 nil ,extras-list)
		   (22111b		221112a		-20		-15  -10 nil ,extras-list)
		   (221112a		221112b		-26		-18  -10 nil ,extras-list)
		   (2211		22112		-13		-8  -10 nil ,extras-list)
		   (22112		221121		-18		-13  -10 nil ,extras-list)
		   (221121		2211211		-20		-14  -10 nil ,extras-list)
		   (221121		2211212		-24		-14  -10 nil ,extras-list)
		   (2211212		22112121		-24		-16  -10 nil ,extras-list)
		   (2211212		22112122		-28		-16  -10 nil ,extras-list)
		   (22112		221122		-19 		-10  -10 nil ,extras-list)
		   (221122		2211221a		-21		-12  -10 nil ,extras-list)
		   (2211221a		2211221b		-22		-13  -10 nil ,extras-list)
		   (221122		2211222		-30		-16  -10 nil ,extras-list)
		   (221		2212		-14		-2  -10      0.6  ,extras-list)
		   (2212		22121		-29		-8  -10 nil ,extras-list)
		   (22121		221211a		-29		-12  -10 nil ,extras-list)
		   (221211a		221211b		-32		-12  -10 nil ,extras-list)
		   (22121		221212		-33		-9  -10 nil ,extras-list)
		   (2212		22122		-17		-1  -10 nil ,extras-list)
		   (22122		221221		-19		-2  -10 nil ,extras-list)
		   (221221		2212211		-21		-3  -10 nil ,extras-list)
		   (2212211		22122111		-21		-4  -10 nil ,extras-list)
		   (2212211		22122112		-23		-4  -10 nil ,extras-list)
		   (22122112		221221121		-29		-5  -10 nil ,extras-list)
		   (22122112		221221122		-27		-4  -10 nil ,extras-list)
		   (221221		2212212		-20		-2  -10 nil ,extras-list)
		   (22122		221222a		-19		0  -10 nil ,extras-list)
		   (221222a		221222b		-20		-1  -10 nil ,extras-list)
		   (221222b		2212221		-30		-3  -10 nil ,extras-list)
		   (221222b		2212222a		-28		-1  -10 nil ,extras-list)
		   (2212222a		2212222b		-33		-3  -10 nil ,extras-list)
		   (22		222		-15		4  -10    0.6 ,extras-list)
		   (222		2221		-18		4  -10 nil ,extras-list)
		   (2221		22211		-27		3  -10 nil ,extras-list)
		   (22211		222111a		-32		3  -10 nil ,extras-list)
		   (222111a		222111b		-39		0  -10 nil ,extras-list)
		   (22211		222112		-35		5  -10 nil ,extras-list)
		   (2221		22212		-28		8  -10 nil ,extras-list)
		   (22212		222121a		-30		6  -10 nil ,extras-list)
		   (222121a		222121b		-35		6  -10 nil ,extras-list)
		   (22212		222122		-34		11  -10 nil ,extras-list)
		   (222		2222		-25		10  -10 nil ,extras-list)
		   (22		223		-4		4  -10       0.8 ,extras-list)
		   (223		2231		-11		8  -10    0.6  ,extras-list)
		   (2231		22311		-15		9  -10 nil ,extras-list)
		   (22311		223111		-21		10  -10 nil ,extras-list)
		   (223111		2231111		-29		12  -10 nil ,extras-list)
		   (2231111		22311111		-30		11  -10 nil ,extras-list)
		   (2231111		22311112a		-32		13  -10 nil ,extras-list)
		   (22311112a		22311112b		-34		12  -10 nil ,extras-list)
		   (223111		2231112		-25		14  -10 nil ,extras-list)
		   (2231112		22311121a		-29		18  -10 nil ,extras-list)
		   (22311121a		22311121b		-31		17  -10 nil ,extras-list)
		   (2231112		22311122a		-25		16  -10 nil ,extras-list)
		   (22311122a		22311122b		-26		16  -10 nil ,extras-list)
		   (22311		223112		-18		12  -10 nil ,extras-list)
		   (223112		2231121		-22		14  -10 nil ,extras-list)
		   (223112		2231122		-28		20  -10 nil ,extras-list)
		   (2231		22312		-19		18  -10 nil ,extras-list)
		   (22312		223121		-22		21  -10 nil ,extras-list)
		   (223121		2231211a		-24		21  -10 nil ,extras-list)
		   (2231211a		2231211b		-25		23  -10 nil ,extras-list)
		   (223121		2231212a		-21		22  -10 nil ,extras-list)
		   (2231212a		2231212b		-26		26  -10 nil ,extras-list)
		   (22312		223122		-18		20  -10 nil ,extras-list)
		   (223		2232		-8		9  -10   0.6  ,extras-list)
		   (2232		22321		-18		21  -10 nil ,extras-list)
		   (22321		223211a		-20		21  -10 nil ,extras-list)
		   (223211a		223211b		-21		24  -10 nil ,extras-list)
		   (22321		223212a		-19		24  -10 nil ,extras-list)
		   (223212a		223212b		-23		25  -10 nil ,extras-list)
		   (2232		22322		-9		16  -10 nil ,extras-list)
		   (22322		223221		-11		18  -10 nil ,extras-list)
		   (223221		2232211		-17		21  -10 nil ,extras-list)
		   (223221		2232212		-18		29  -10 nil ,extras-list)
		   (22322		223222a		-8		17  -10 nil ,extras-list)
		   (223222a		223222b		-14		29  -10 nil ,extras-list)
		   (223		2233a		-1		7  -10      0.6 ,extras-list)
		   (2233a		2233b		-4		10  -10 nil ,extras-list)
		   (2233b		22331		-12		28  -10 nil ,extras-list)
		   (2233b		22332a		-3		18  -10 nil ,extras-list)
		   (22332a		22332b		-12		31  -10 nil ,extras-list)
		   (soma		3		0		2  -5       1.2  ,extras-list)
		   (3		31		4		9  -7       0.6  ,extras-list)
		   (31		311		2		11  -10 nil ,extras-list)
		   (311		3111a		-1		11  -10 nil ,extras-list)
		   (3111a		3111b		-2		18  -10 nil ,extras-list)
		   (311		3112		3		17  -10 nil ,extras-list)
		   (3112		31121		-4		30  -10 nil ,extras-list)
		   (3112		31122		5		25  -10 nil ,extras-list)
		   (31		312		8		12  -10 nil ,extras-list)
		   (312		3121		7		19  -10 nil ,extras-list)
		   (312		3122		10		13  -10 nil ,extras-list)
		   (3122		31221a		10		14  -10 nil ,extras-list)
		   (31221a		31221b		15		20  -10 nil ,extras-list)
		   (3122		31222a		12		14  -10 nil ,extras-list)
		   (31222a		31222b		22		20  -10 nil ,extras-list)
		   (31221b		312211		14		22  -10 nil ,extras-list)
		   (31221b		312212		18		22  -10 nil ,extras-list)
		   (31		313a		8		8  -10 nil ,extras-list)
		   (313a		313b		18		16  -10 nil ,extras-list)
		   (3		32		6		3  -10         0.9 ,extras-list)
		   (32		321		12		5  -10 0.6  ,extras-list)
		   (321		3211		18		9  -10     nil ,extras-list)
		   (3211		32111a		17		13  -10 nil ,extras-list)
		   (32111a		32111b		22		19  -10 nil ,extras-list)
		   (3211		32112		32		17  -10 nil ,extras-list)
		   (321		3212a		14		4  -10 nil ,extras-list)
		   (3212a		3212b		19		6  -10 nil ,extras-list)
		   (3212b		32121		33		13  -10 nil ,extras-list)
		   (3212b		32122		30		9  -10 nil ,extras-list)
		   (32		322		18		2  -10   0.6 ,extras-list)
		   (322		3221		23		3  -10 nil ,extras-list)
		   (3221		32211		35		5  -10 nil ,extras-list)
		   (3221		32212		33		0  -10 nil ,extras-list)
		   (322		3222		26		-1  -10 nil ,extras-list)
		   ) 
		 :default-diameter segment-diameter :synapse synapse :xy-factor 6.06)

    (if include-distal-a-current
	(dolist (segment-name *distal-nodes*)
	  (create-a-channel (gethash segment-name segment-hash-table)
			    cell-name :gbar-density 500.0 :save-particle t))))
  )


; this gets most of the distal nodes.
(defvar *distal-nodes* '("1112B" "1132" "211112B" "211212" "211232" "2121122B"  "2121221B" 
			 "21221B" "2122211B"    "21222122"  "221111B"    "221112B" "22112122" 
			 "2211222" "221211B" "221212" "221221122"  "2212222B" "222111B"    "222112"
			 "222121B" "222122"  "2222" "22311112B" "22311121B" "22311122B"  "2231122"
			 "2231211B" "2231212B"  "223211B" "223212B" "2232211"    "2232212" "223222B"
			 "22331" "22332B" "3111B" "31121" "31122" "3121"  "31222B" "312211" "312212"
			 "313B"    "32111B" "32112" "32121"    "32122" "32211" "32212" "3222"
			 "21222121" "122"  "112" "21212222" ))

