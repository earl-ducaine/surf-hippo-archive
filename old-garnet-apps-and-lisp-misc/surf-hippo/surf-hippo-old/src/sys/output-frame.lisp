;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: #-PARALLEL SURF #+PARALLEL *SURF; Base: 10; -*-
;;; (c) Copyright 1990, Lyle Borg-Graham, MIT Center for Biological Information Processing

;;; The surf-interface>*.lisp requires that Patrick O'Donnell's graphics package be loaded, 
;;; i.e. - (load "b:>pao>lispm>graphics>graphics-7")

(if (zl:status nofeature :plot-hack)
    (load "b:>pao>lispm>graphics>graphics-7"))

(defflavor tv:plotter-pane () (plot::plot-hack tv:pane-mixin))

(defflavor plot-frame	()
	   (tv:bordered-constraint-frame)	
  :settable-instance-variables
  (:default-init-plist   :activate-p t   :expose-p t   :save-bits t))

(defvar *histology-frame*
	(tv:make-window
	  'plot-frame
	  ':panes '((note-pane tv:window :label "")
		    (graphics-pane dw:dynamic-window :label "TURTLE RETINAL CELLS"))
	  ':configurations '((c1
			       (:layout
				 (c1 :column note-pane graphics-pane))
			       (:sizes		
				 (c1  (note-pane .1)
				      (graphics-pane  .9)))))))
;(defvar *histology-frame-2*
;	(tv:make-window
;	  'plot-frame
;	  ':panes '((note-pane tv:window :label "")
;		    (graphics-pane dw:dynamic-window :label ""))
;	  ':configurations '((c1
;			       (:layout
;				 (c1 :column note-pane graphics-pane))
;			       (:sizes		
;				 (c1  (note-pane .1)
;				      (graphics-pane  .9)))))))

(defvars-w-value (*histology-graphics-pane (send *histology-frame* :get-pane 'graphics-pane))
  (*histology-note-pane (send *histology-frame* :get-pane 'note-pane )))

(tv:add-select-key #\h  *histology-note-pane  "TURTLE RETINAL CELLS"  nil t)

;(defvars-w-value (*histology-graphics-pane (send *histology-frame-2* :get-pane 'graphics-pane))
;  (*histology-note-pane (send *histology-frame* :get-pane 'note-pane )))
;
;(tv:add-select-key #\h  *histology-graphics-pane  "TURTLE RETINAL CELLS"  nil t)

(defvar *surf-frame*
	(tv:make-window
	  'plot-frame
	  ':panes '((voltage-pane1 tv:plotter-pane :label "Node Voltages")
		    (voltage-pane2 tv:plotter-pane :label "Node Voltages")
;		    (voltage-pane3 tv:plotter-pane  :label "Node Voltages")
		    (current-pane tv:plotter-pane   :label "Node Values")
		    (particle-pane tv:plotter-pane  :label "Node Values")
		    (graphics-pane dw:dynamic-window :label "Histology")
		    (interaction-pane dw:dynamic-lisp-listener :label "SURF CELL TEST SIMULATION"))
	  ':configurations '((c1
			       (:layout
				 (c1 :column r1 graphics-pane)
				 (r1 :row interaction-pane c2)
				 (c2 :column voltage-pane1 voltage-pane2  current-pane particle-pane))
			       (:sizes
				 (c1 (r1 0.6)
				     (graphics-pane 0.4))
				 (r1  (interaction-pane 0.33333334)
				      (c2   0.6666667))
				 (c2  (voltage-pane1 0.25)
				      (voltage-pane2 0.25 )
				      (current-pane 0.25 )
				      (particle-pane  0.255 )
				        ))))))

(defvars-w-value (voltage-pane1 (send *surf-frame* :get-pane 'voltage-pane1))
		 (voltage-pane2 (send *surf-frame* :get-pane 'voltage-pane2))
;  (voltage-pane3 (send *surf-frame* :get-pane 'voltage-pane3))
		 (current-pane (send *surf-frame* :get-pane 'current-pane))
		 (graphics-pane (send *surf-frame* :get-pane 'graphics-pane))
			  (particle-pane (send *surf-frame* :get-pane 'particle-pane))
  (*surf-interaction-pane (send *surf-frame* :get-pane 'interaction-pane)))
;
;(defvar *demo-surf-frame*
;	(tv:make-window
;	  'plot-frame
;	  ':panes '((voltage-pane tv:plotter-pane :label "Node Voltages")
;		    (graphics-pane dw:dynamic-window :label "Histology")
;		    )
;	  ':configurations '((c1
;			       (:layout
;				 (c1 :column voltage-pane graphics-pane)
;				 )
;			       (:sizes
;				 (c1 (voltage-pane 0.5)
;				     (graphics-pane 0.5))
;				 )))))
;
;(defvars-w-value (voltage-pane1 (send *demo-surf-frame* :get-pane 'voltage-pane))
;		 (graphics-pane (send *demo-surf-frame* :get-pane 'graphics-pane)))
;
;
(tv:add-select-key #\circle  *surf-interaction-pane "Surf Test Frame" nil t)
;(tv:add-select-key #\-  demo-voltage-pane  "" nil t)

(defvar *ds-frame*
	(tv:make-window
	  'plot-frame
	  ':panes '((ds-pane-0 tv:plotter-pane
			     :label "Node Functions")
		    (ds-pane-1 tv:plotter-pane
			     :label "Node Functions")
		    (ds-pane-2 tv:plotter-pane
			     :label "Node Functions")
		    (ds-interaction-pane dw:dynamic-lisp-listener
					 :label "DS CELL TEST SIMULATION"))
	  ':configurations '((r1
			       (:layout
				 (r1 :row ds-interaction-pane c1)
				 (c1 :column ds-pane-0 ds-pane-1 ds-pane-2))
			       (:sizes		
				 (r1  (ds-interaction-pane .3)
				      (c1 .7))
				 (c1  (ds-pane-0 .333333)
				      (ds-pane-1 .333333)
				      (ds-pane-2 .333333)
				 ))))))
(tv:add-select-key #\=  (send *ds-frame* :get-pane 'ds-interaction-pane) "DS Results" nil t)
(send (send *ds-frame* :get-pane 'ds-interaction-pane) :expose)
(send (send *ds-frame* :get-pane 'ds-interaction-pane) :select)
(send (send *ds-frame* :get-pane 'ds-interaction-pane) :set-default-character-style '(:fix :roman :small))
(send (send *ds-frame* :get-pane 'ds-pane-1) :set-default-character-style '(:fix :roman :normal))
;
;
;
;(defvars-w-value (*ds-pane (send *ds-frame* :get-pane 'ds-pane))
;  (*ds-interaction-pane (send *ds-frame* :get-pane 'ds-interaction-pane)))
;
;;(tv:add-select-key #\triangle  *ds-interaction-pane "DS Frame" nil t) 
;(tv:add-select-key #\triangle  *ds-interaction-pane "DS Frame" nil t) 


