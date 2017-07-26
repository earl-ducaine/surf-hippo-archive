;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: #-PARALLEL SURF #+PARALLEL *surf; Base: 10; -*-



(defun a21-test ()
  (a21 'test :extras-list '((synapse  EXCITATORY-FACILITATION)(synapse INHIBITORY-1-OFFSET))))




;;This cell is from Kolb 82, Figure 8. Type A21 (1.6mm from visual streak) example. Cell stratifies
;;in S4.

(defun a21 (name &key (extras-list '()))
  (create-cell-type "kolb-a21" :soma-resistivity 10000 :membrane-resistivity 60000)
  (create-cell name :cell-type-name  "kolb-a21")
  (let ((soma (create-soma (format nil "~a-soma" name) name 1)))
    (if *include-sources (create-source *clamp-type soma))
    (create-tree
      soma
      `((45 46 246 184 0 0.5 ,extras-list) (SOMA SOMA 348 204 0 12) (2 3 511 184 0 0.5 ,extras-list)
	(3 4 577 180 0 0.5 ,extras-list) (4 5 644 181 0 0.5 ,extras-list) (5 6 719 181 0 0.5 ,extras-list)
	(6 7 785 190 0 0.5 ,extras-list) (7 8 839 182 0 0.5 ,extras-list) (8 9 929 179 0 0.5 ,extras-list)
	(9 10 941 183 0 0.5 ,extras-list) (12 13 575 191 0 0.5 ,extras-list) (13 14 636 189 0 0.5 ,extras-list)
	(14 15 707 195 0 0.5 ,extras-list) (15 16 766 205 0 0.5 ,extras-list) (16 17 813 213 0 0.5 ,extras-list)
	(19 20 496 207 0 0.5 ,extras-list) (20 21 553 199 0 0.5 ,extras-list) (21 22 615 205 0 0.5 ,extras-list)
	(22 23 689 203 0 0.5 ,extras-list) (23 24 743 207 0 0.5 ,extras-list) (24 25 806 217 0 0.5 ,extras-list)
	(25 26 849 234 0 0.5 ,extras-list) (26 27 901 243 0 0.5 ,extras-list) (27 28 935 244 0 0.5 ,extras-list)
	(32 33 639 220 0 0.5 ,extras-list) (33 34 700 237 0 0.5 ,extras-list) (35 36 372 235 0 0.5)
	(36 37 438 227 0 0.5 ,extras-list) (37 38 492 239 0 0.5 ,extras-list) (38 39 522 232 0 0.5 ,extras-list)
	(39 40 589 236 0 0.5 ,extras-list) (40 41 657 235 0 0.5 ,extras-list) (41 42 716 246 0 0.5 ,extras-list)
	(42 43 764 256 0 0.5 ,extras-list) (44 45 298 192 0 0.5 ,extras-list) (47 48 152 176 0 0.5 ,extras-list)
	(48 49 125 160 0 0.5 ,extras-list) (49 50 100 152 0 0.5 ,extras-list) (48 51 119 170 0 0.5 ,extras-list)
	(51 52 98 139 0 0.5 ,extras-list) (54 55 189 205 0 0.5 ,extras-list) (SOMA 1 400 179 0 1 ,extras-list)
	(46 47 202 184 0 0.5 ,extras-list) (SOMA 18 415 210 0 1 ,extras-list) (18 29 446 221 0 1 ,extras-list)
	(29 30 496 212 0 1 ,extras-list) (30 31 540 220 0 1 ,extras-list) (31 32 590 215 0 1 ,extras-list)
	(SOMA 35 340 217 0 1)
	(35 44 328 223 0 1) (44 53 288 213 0 1 ,extras-list) (53 54 235 208 0 1 ,extras-list)
	(1 2 454 169 0 1 ,extras-list)
	(18 19 455 197 0 0.5 ,extras-list) (1 11 452 194 0 1 ,extras-list) (11 12 513 196 0 1 ,extras-list))

      :default-diameter 0.2 :synapse *include-synapse :xy-factor (/ 1.0 1.0001999 ))))



 