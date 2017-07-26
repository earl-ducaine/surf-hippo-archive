; -*- mode: lisp; Syntax: Common-lisp; package: #-parallel surf #+parallel *surf; base: 10; fonts: CPTFONT ; -*-
;;;  (c) Copyright 1990, Lyle Borg-Graham, MIT Center for Biological Information Processing

;; This file contains miscellaneous functions which require the Symbolics environment.


(in-package #+parallel '*surf #-parallel 'surf)

(defun display-time ()
  (send *surf-interaction-pane :SET-CURSORpos 0 (send *surf-interaction-pane :cursor-y))
  (send *surf-interaction-pane :CLEAR-rest-of-line)
  (format t "Simulation time: ~a. Stop time: ~a." *real-time user-stop-time))

(defun display-message (message-string)
  (send *surf-interaction-pane :SET-CURSORpos 0 (send *surf-interaction-pane :cursor-y))
  (send *surf-interaction-pane :CLEAR-rest-of-line)
  (format t message-string))

(defun quick-light-test ()
  (MENU-FOR-SYNAPSE-PARAMETERS)
  (draw-cells))

(defun surf-screen (&optional window)
  (if (or window (and *hard-copy-screen (setq window *surf-interaction-pane)))
      (let ((temp-window tv:selected-window))
	(send window :select)
	;(setq HARDCOPY:*DEFAULT-BITMAP-PRINTER* something)
	(HCI::HARDCOPY-SCREEN)
	(send temp-window :select))))


;;; CHOOSE-PLOT-DATA This function goes through all the cells and their segment and membrane nodes, and their
;;; currents and generates menus for plotting these values.  Note that *old-plot-nodes* and *plot-nodes* (and
;;; analogous lists for other device data) behave somewhat differently depending on whether the Sympbolics
;;; version is used. Specifically, the non-Symbolics version ALWAYS uses the value of *plot-nodes*, which may be
;;; supplied by the circuit definition function. In the Symbolics version, if no change is requested, then the
;;; value of *old-plot-nodes* (the nodes plotted during the last simulation) is used. To use a simfile defined
;;; *plot-nodes*, then *change-plotted-nodes must be T and the appropriate response to the prompt "Modify
;;; previous run's plotted nodes [Y] or use simfile *plot-nodes* [N]?" must be given.
(defun choose-plot-data ()
  (choose-plot-nodes)
  (choose-analysis-nodes)
  (choose-plot-currents)
  (choose-plot-conductances)
  (choose-plot-particles)
  )

;;; CHOOSE-PLOT-NODES A version of *plot-nodes* may or may not be supplied by the simfile. *old-plot-nodes* are the
;;; nodes used in the last run.
(defun choose-plot-nodes ()
  (let ((temp-plot-nodes '()))
    (cond ((and (not *automatic-run) *change-plotted-nodes)
	   (if (y-or-n-p "Clear all plotted nodes?")
	       (setq *plot-nodes* '())
	       (if (y-or-n-p "Modify previous run's plotted nodes [Y] or use simfile *plot-nodes* [N]?")
		   (setq *plot-nodes* *old-plot-nodes*)))
	   (dolist (node-name *plot-nodes*)	;This is to pick up either the previous run's plotted nodes, or
						;the ones specified in the sim file.
	     (let ((nd (gethash  node-name node-hash-table)))	;Make sure that the previous
						;plotted node still exists. If not,  
	       (if (not nd) (delete node-name *plot-nodes*))))	;remove it from *plot-nodes*.
						;Now nodes in *plot-nodes* are valid ones.
	   (dolist (cell-name *cells*)		;Work the cells to make menus which add nodes to
						;temp-plot-nodes.
	     (setq temp-plot-nodes (choose-cell-plot-nodes cell-name temp-plot-nodes)))
	   (setq *plot-nodes* temp-plot-nodes))
	  (t
	   (setq *plot-nodes* *old-plot-nodes*)))	;If *change-plotted-nodes is NIL, use the previous run plotted nodes.
    (setq *old-plot-nodes* *plot-nodes* )))

;;; CHOOSE-ANALYSIS-NODES A version of *analysis-nodes* may or may not be supplied by the simfile. *old-analysis-nodes* are the
;;; nodes used in the last run.
(defvar *old-analysis-nodes* '())
(defun choose-analysis-nodes ()
  (let ((temp-analysis-nodes '()))
    (cond ((and (not *automatic-run) *change-plotted-nodes)		; *change-analysisted-nodes)
	   (if (y-or-n-p "Clear all analyzed nodes?")
	       (setq *analysis-nodes* '())
	       (if (y-or-n-p "Modify previous run's analyzed nodes [Y] or use simfile *analysis-nodes* [N]?")
		   (setq *analysis-nodes* *old-analysis-nodes*)))
	   (dolist (node-name *analysis-nodes*)	;This is to pick up either the previous run's analysisted nodes, or
						;the ones specified in the sim file.
	     (let ((nd (gethash  node-name node-hash-table)))	;Make sure that the previous
						;analysisted node still exists. If not,  
	       (if (not nd) (delete node-name *analysis-nodes*))))	;remove it from *analysis-nodes*.
						;Now nodes in *analysis-nodes* are valid ones.
	   (dolist (cell-name *cells*)		;Work the cells to make menus which add nodes to
						;temp-analysis-nodes.
	     (setq temp-analysis-nodes (choose-cell-analysis-nodes cell-name temp-analysis-nodes)))
	   (setq *analysis-nodes* temp-analysis-nodes))
	  (t
	   (setq *analysis-nodes* *old-analysis-nodes*)))
    (setq *old-analysis-nodes* *analysis-nodes* )))

;;; CHOOSE-CELL-PLOT-NODES This generates a menu of the cell-name cell's nodes and adds nodes to be plotted to the
;;; temp-plot-nodes list, which is returned.
(defun choose-cell-plot-nodes (cell-name temp-plot-nodes)
  (let ((keyword-alist (list '(PLOT "")))(results))	;Now set up a menu to change things.
    (let ((temp-list (make-segments-and-membranes-list cell-name))	; Get a list of all seg and memb nodes
	  (menu-list '()))			; for this cell.
      (dolist (name temp-list)
	(setq menu-list				;We want the members of *plot-nodes* to be checked on the menu.
	      (nconc menu-list
		     (list (list name name (list (list 'PLOT (member name *plot-nodes* :test #'equal) )))))))
      (setq results
	    (tv:multiple-choose (format nil "Setting Plotted Nodes for Cell ~a" cell-name)	;Now a menu.
				menu-list keyword-alist))
      (dolist (plot-flag-list results)		;Now update *plot-nodes*
	(if (eq 'PLOT (car (cdr plot-flag-list)))
	    (setq temp-plot-nodes (nconc temp-plot-nodes (list (car plot-flag-list)) ))))))
  temp-plot-nodes)

(defun choose-cell-analysis-nodes (cell-name temp-analyze-nodes)
  (let ((keyword-alist (list '(ANALYZE "")))(results))	;Now set up a menu to change things.
    (let ((temp-list (make-segments-and-membranes-list cell-name))	; Get a list of all seg and memb nodes
	  (menu-list '()))			; for this cell.
      (dolist (name temp-list)
	(setq menu-list				;We want the members of *analysis-nodes* to be checked on the menu.
	      (nconc menu-list
		     (list (list name name (list (list 'ANALYZE (member name *analysis-nodes* :test #'equal) )))))))
      (setq results
	    (tv:multiple-choose (format nil "Setting Analyzed Nodes for Cell ~a" cell-name)	;Now a menu.
				menu-list keyword-alist))
      (dolist (analyze-flag-list results)		;Now update *analysis-nodes*
	(if (eq 'ANALYZE (car (cdr analyze-flag-list)))
	    (setq temp-analyze-nodes (nconc temp-analyze-nodes (list (car analyze-flag-list)) ))))))
  temp-analyze-nodes)

;;; CHOOSE-PLOT-PARTICLES
(defun choose-plot-particles ()
  "Makes list of channel names whose particles will be saved"
  (if (and (not *automatic-run) *change-plotted-particles)
      (progn
	(if (y-or-n-p "Clear all plotted particles?") (setq  *plot-particles* '())
	    (if (y-or-n-p "Use previous run's plotted nodes [N uses simfile selected particles]?")
		(setq  *plot-particles* *old-plot-particles*)))
	(let ((keyword-alist (list '(plot "")))(results)(all-particles (make-particles-list))(menu-list '()))
	  (dolist (particle-name all-particles)
	    (setq menu-list			;We want the members of *plot-channel-particles* to be checked on the menu.
		  (nconc menu-list
			 (list
			   (list particle-name particle-name
				 (list
				   (list 'plot (member particle-name *plot-particles* :test #'string-equal))))))))
	  (if menu-list (setq results (tv:multiple-choose "Setting Plotted Particles" menu-list  keyword-alist)))
	  (setq *plot-particles* '())
	  (dolist (particle-name results)	;This creates a list of the selected plotted particle names.
	    (if (eq 'PLOT (car (cdr particle-name)))
		(setq *plot-particles* (nconc *plot-particles* (list (car particle-name))))))	   )
	(setq *old-plot-particles* *plot-particles*))
      (setq *plot-particles* *old-plot-particles*))
  nil)

;;; CHOOSE-PLOT-CURRENTS
(defun choose-plot-currents ()
  "Makes list of channel and synapse names whose currents will be saved"
  (if (and (not *automatic-run) *change-plotted-currents)
      (progn
	(if (y-or-n-p "Clear all plotted currents?") (setq  *plot-channel-currents* '() *plot-synapse-currents* '())
	    (if (y-or-n-p "Use previous run's plotted nodes [N uses simfile selected channels]?")
		(setq  *plot-channel-currents* *old-plot-channel-currents*
		       *plot-synapse-currents*	*old-plot-synapse-currents*)))
	(let ((keyword-alist (list '(plot "")))(results)(all-channels (make-channels-list))(menu-list '()))
	  (dolist (channel-name all-channels)
	    (setq menu-list			;We want the members of *plot-channel-currents* to be checked on the menu.
		  (nconc menu-list
			 (list
			   (list channel-name channel-name
				 (list
				   (list 'plot (member channel-name *plot-channel-currents* :test #'string-equal))))))))
	  (if menu-list (setq results (tv:multiple-choose "Setting Plotted Currents" menu-list  keyword-alist)))
	  (setq *plot-channel-currents* '())
	  (dolist (channel-name results)	;This creates a list of the selected plotted channel names.
	    (if (eq 'PLOT (car (cdr channel-name)))
		(setq *plot-channel-currents* (nconc *plot-channel-currents* (list (car channel-name))))))	   )
	(let ((keyword-alist (list '(plot ""))) (results) (all-synapses (make-synapses-list))(menu-list '()) )
	  (dolist (synapse-name all-synapses)
	    (if (not (synapse-channel (gethash synapse-name synapse-hash-table)))	;Avoid synapses with
						;channels.
		(setq menu-list			;We want the members of *synapse-currents-to-output* to be checked on the menu.
		      (nconc menu-list
			     (list
			       (list synapse-name synapse-name
				     (list
				       (list 'plot (member synapse-name *plot-synapse-currents* :test #'string-equal)))))))))
	  (if menu-list (setq results (tv:multiple-choose "Setting Plotted Currents" menu-list  keyword-alist)))
	  (setq *plot-synapse-currents* '())
	  (dolist (synapse-name results)	;This creates a list of the selected plotted synapse names.
	    (if (eq 'PLOT (car (cdr synapse-name)))
		(setq *plot-synapse-currents* (nconc *plot-synapse-currents* (list (car synapse-name)))))))
	(setq *old-plot-channel-currents* *plot-channel-currents* *old-plot-synapse-currents* *plot-synapse-currents*))
      (setq *plot-channel-currents* *old-plot-channel-currents* *plot-synapse-currents* *old-plot-synapse-currents*))
  nil)

;;; CHOOSE-PLOT-CONDUCTANCES
(defun choose-plot-conductances ()
  "Makes list of channel and synapse names whose conductances will be saved"
  (if  (and (not *automatic-run) *change-plotted-conductances)
      (progn
	(if (y-or-n-p "Clear all plotted conductances?")
	    (setq  *plot-channel-conductances* '() *plot-synapse-conductances* '())
	    (if (y-or-n-p "Use previous run's plotted nodes [N uses simfile selected channels]?")
		(setq  *plot-channel-conductances* *old-plot-channel-conductances*
		       *plot-synapse-conductances*	*old-plot-synapse-conductances*)))
	(let ((keyword-alist (list '(plot "")))(results)(all-channels (make-channels-list))(menu-list '()))
	  (dolist (channel-name all-channels)
	    (setq menu-list	;We want the members of *plot-channel-conductances* to be checked on the menu.
		  (nconc menu-list
			 (list
			   (list channel-name channel-name
				 (list
				   (list 'plot (member channel-name *plot-channel-conductances* :test #'string-equal))))))))
	  (if menu-list (setq results (tv:multiple-choose "Setting Plotted Conductances" menu-list  keyword-alist)))
	  (setq *plot-channel-conductances* '())
	  (dolist (channel-name results)	;This creates a list of the selected plotted channel names.
	    (if (eq 'PLOT (car (cdr channel-name)))
		(setq *plot-channel-conductances* (nconc *plot-channel-conductances* (list (car channel-name))))))	   )
	(let ((keyword-alist (list '(plot ""))) (results) (all-synapses (make-synapses-list))(menu-list '()) )
	  (dolist (synapse-name all-synapses)
	    (if (not (synapse-channel (gethash synapse-name synapse-hash-table)))	;Avoid synapses with
	;channels.
		(setq menu-list	;We want the members of *synapse-conductances-to-output* to be checked on the menu.
		      (nconc menu-list
			     (list
			       (list synapse-name synapse-name
				     (list
				       (list 'plot (member synapse-name *plot-synapse-conductances* :test #'string-equal)))))))))
	  (if menu-list (setq results (tv:multiple-choose "Setting Plotted Conductances" menu-list  keyword-alist)))
	  (setq *plot-synapse-conductances* '())
	  (dolist (synapse-name results)	;This creates a list of the selected plotted synapse names.
	    (if (eq 'PLOT (car (cdr synapse-name)))
		(setq *plot-synapse-conductances* (nconc *plot-synapse-conductances* (list (car synapse-name)))))))
	(setq *old-plot-channel-conductances* *plot-channel-conductances*
	      *old-plot-synapse-conductances*  *plot-synapse-conductances*))
      (setq *plot-channel-conductances* *old-plot-channel-conductances*
	    *plot-synapse-conductances* *old-plot-synapse-conductances*))
  nil)


;;; GROW-CELL-SPINES This generates a menu of all the segment and membrane nodes of cell "cell-name" in order to
;;; allow adding a standard spine to each of those nodes. The spines are then generated. Spines are named by the
;;; node from which they grow from with the addition of "-spine". The distal spine nodes are named by the name
;;; of the associated spine.
;(defun grow-cell-spines (cell-name)
;  (let ((spine-seg-node-names '())(keyword-alist (list '(grow-spine "")))(results)
;	(temp-list (make-segments-and-membranes-list cell-name))	;Pick up seg and memb nodes for cell.
;	(menu-list '()))
;    (dolist (name temp-list)			;Make a menu of all the seg and memb nodes to grow a spine from.
;      (setq menu-list
;	    (nconc menu-list (list (list name name (list (list 'grow-spine (member name spine-seg-node-names) )))))))
;    (setq results (tv:multiple-choose "Growing Spines from Segment Nodes" menu-list	;The menu.
;				      keyword-alist))
;
;    (dolist (grow-spine-flag-list results)	;Now update spine-seg-nodes from menu results.
;      (if (eq 'GROW-SPINE (car (cdr grow-spine-flag-list)))
;	  (setq spine-seg-node-names (nconc spine-seg-node-names (list (car grow-spine-flag-list)) ))))
;
;    (dolist (spine-seg-node-name spine-seg-node-names)		;Now create spine segments.
;      (let ((spine-node-name (format nil "~a-spine" spine-seg-node-name )))
;	(format t "~%Creating spine ~a, input node ~a, output node ~a"
;		spine-node-name spine-seg-node-name spine-node-name)
;	(create-standard-spine spine-node-name spine-seg-node-name :cell-name cell-name
;			 '((G-AXIAL . 0.037699112) (G-LEAK . 0.0002261947) (V-LEAK . -70.0) (CAPACITANCE . 0.009047787)))))))

