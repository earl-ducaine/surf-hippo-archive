;;; -*- mode: lisp; Syntax: Common-lisp; package: surf ; base: 10;  -*-
;;; Lyle Borg-Graham, Equipe Cognisciences, Institut Alfred Fessard, CNRS

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;              The Surf-Hippo Neuron Simulator                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; This code was written as part of the Surf-Hippo project at the  ;;;
;;; Center for Biological Information Processing, Department of     ;;;
;;; Brain and Cognitive Sciences, Massachusetts Institute of        ;;;
;;; Technology, and currently at the Equipe Cognisciences, Institut ;;;
;;; Alfred Fessard, CNRS and has been placed in the public domain.  ;;;
;;; If you are using this code or any part of Surf-Hippo, please    ;;;
;;; contact surf-hippo@ai.mit.edu to be put on the mailing list.    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; SYS Source file: structure-data-functions.lisp

;; (was structure-data-macros.lisp)

(in-package "SURF-HIPPO")

(defun node-voltage-data (node)
  "mV"
  (get-a-value :voltage-data (node-parameters node)))

(defun push-node-voltage-data (node data)
  "mV"
  (let ((data (s-flt data)))
    (if (node-parameters node)
	(push-a-value data :voltage-data (node-parameters node))
	(setf (node-parameters node) (acons :voltage-data (list data) '())))))

(defun node-voltage-derivative-data (node)
  "mV/ms"
  (get-a-value :voltage-derivative-data (node-parameters node)))

(defun push-node-voltage-derivative-data (node data)
  "mV/ms"
  (let ((data (s-flt data)))
    (if (node-parameters node)
	(push-a-value data :voltage-derivative-data (node-parameters node))
	(setf (node-parameters node) (acons :voltage-derivative-data (list data) '())))))

(defun segment-voltage-data (segment)
  "mV"
  (get-a-value :voltage-data (segment-parameters segment)))

#|
(defun push-segment-voltage-data (segment data)
  "mV"
  (let ((data (s-flt data)))
    (if (segment-parameters segment)
	(push-a-value data :voltage-data (segment-parameters segment))
	(setf (segment-parameters segment) (acons :voltage-data (list data) '())))))
|#

(defun push-segment-voltage-data (segment data)
  "mV"
  (if (segment-parameters segment)
      (push-a-value data :voltage-data (segment-parameters segment))
      (setf (segment-parameters segment) (acons :voltage-data (list data) '()))))

(defun segment-voltage-derivative-data (segment)
  "mV/ms"
  (get-a-value :voltage-derivative-data (segment-parameters segment)))

(defun push-segment-voltage-derivative-data (segment data)
  "mV/ms"
  (let ((data (s-flt data)))
    (if (segment-parameters segment)
	(push-a-value data :voltage-derivative-data (segment-parameters segment))
	(setf (segment-parameters segment) (acons :voltage-derivative-data (list data) '())))))

(defun soma-voltage-data (soma)
  "mV"
  (get-a-value :voltage-data (soma-parameters soma)))

#|
(defun push-soma-voltage-data (soma data)
  "mV"
  (let ((data (s-flt data)))
    (if (soma-parameters soma)
	(push-a-value data :voltage-data (soma-parameters soma))
	(setf (soma-parameters soma) (acons :voltage-data (list data) '())))))
|#

(defun push-soma-voltage-data (soma data)
  "mV"
  (if (soma-parameters soma)
      (push-a-value data :voltage-data (soma-parameters soma))
      (setf (soma-parameters soma) (acons :voltage-data (list data) '()))))

(defun soma-voltage-derivative-data (soma)
  "mV/ms"
  (get-a-value :voltage-derivative-data (soma-parameters soma)))

(defun push-soma-voltage-derivative-data (soma data)
  "mV/ms"
  (let ((data (s-flt data)))
    (if (soma-parameters soma)
	(push-a-value data :voltage-derivative-data (soma-parameters soma))
	(setf (soma-parameters soma) (acons :voltage-derivative-data (list data) '())))))

(defun soma-dendrite-current-data (soma)
  "nA"
  (get-a-value :dendrite-current-data (soma-parameters soma)))

(defun push-soma-dendrite-current-data (soma data)
  "nA"
  (let ((data (s-flt data)))
    (if (soma-parameters soma)
	(push-a-value data :dendrite-current-data (soma-parameters soma))
	(setf (soma-parameters soma) (acons :dendrite-current-data (list data) '())))))


(defun isource-current-data (isource)
  "nA"
  (get-a-value :current-data (isource-parameters isource)))

(defun push-isource-current-data (isource data)
  "nA"
  (let ((data (s-flt data)))
    (if (isource-parameters isource)
	(push-a-value data :current-data (isource-parameters isource))
	(setf (isource-parameters isource) (acons :current-data (list data) '())))))

(defun vsource-current-data (vsource)
  "nA"
  (get-a-value :current-data (vsource-parameters vsource)))

(defun push-vsource-current-data (vsource data)
  "nA"
  (let ((data (s-flt data)))
    (if (vsource-parameters vsource)
	(push-a-value data :current-data (vsource-parameters vsource))
	(setf (vsource-parameters vsource) (acons :current-data (list data) '())))))

(defun vsource-voltage-data (vsource)
  "mV"
  (get-a-value :voltage-data (vsource-parameters vsource)))

(defun push-vsource-voltage-data (vsource data)
  "mV"
  (let ((data (s-flt data)))
    (if (vsource-parameters vsource)
	(push-a-value data :voltage-data (vsource-parameters vsource))
	(setf (vsource-parameters vsource) (acons :voltage-data (list data) '())))))


(defun channel-current-data (channel)
  "nA"
  (get-a-value :current-data (channel-parameters channel)))

(defun push-channel-current-data (channel data)
  "nA"
  (let ((data (s-flt data)))
    (if (channel-parameters channel)
	(push-a-value data :current-data (channel-parameters channel))
	(setf (channel-parameters channel) (acons :current-data (list data) '())))))

(defun channel-conductance-data (channel)
  "uS"
  (get-a-value :conductance-data (channel-parameters channel)))

(defun push-channel-conductance-data (channel data)
  "uS"
  (let ((data (s-flt data)))
    (if (channel-parameters channel)
	(push-a-value data :conductance-data (channel-parameters channel))
	(setf (channel-parameters channel) (acons :conductance-data (list data) '())))))

(defun channel-reversal-potential-data (channel)
  "mV"
  (get-a-value :reversal-potential-data (channel-parameters channel)))

(defun push-channel-reversal-potential-data (channel data)
  "mV"
  (let ((data (s-flt data)))
    (if (channel-parameters channel)
	(push-a-value data :reversal-potential-data (channel-parameters channel))
	(setf (channel-parameters channel) (acons :reversal-potential-data (list data) '())))))


(defun particle-state-data (particle)
  (get-a-value :state-data (particle-parameters particle)))

(defun push-particle-state-data (particle data)
  (let ((data (s-flt data)))
    (if (particle-parameters particle)
	(push-a-value data :state-data (particle-parameters particle))
	(setf (particle-parameters particle) (acons :state-data (list data) '())))))

(defun conc-particle-state-data (particle)
  (get-a-value :state-data (conc-particle-parameters particle)))

(defun push-conc-particle-state-data (conc-particle data)
  (let ((data (s-flt data)))
    (if (conc-particle-parameters conc-particle)
	(push-a-value data :state-data (conc-particle-parameters conc-particle))
	(setf (conc-particle-parameters conc-particle) (acons :state-data (list data) '())))))


(defun synapse-current-data (synapse)
  "nA"
  (get-a-value :current-data (synapse-parameters synapse)))

(defun push-synapse-current-data (synapse data)
  "nA"
  (let ((data (s-flt data)))
    (if (synapse-parameters synapse)
	(push-a-value data :current-data (synapse-parameters synapse))
	(setf (synapse-parameters synapse) (acons :current-data (list data) '())))))

(defun synapse-conductance-data (synapse)
  "uS"
  (get-a-value :conductance-data (synapse-parameters synapse)))

(defun push-synapse-conductance-data (synapse data)
  "uS"
  (let ((data (s-flt data)))
    (if (synapse-parameters synapse)
	(push-a-value data :conductance-data (synapse-parameters synapse))
	(setf (synapse-parameters synapse) (acons :conductance-data (list data) '())))))

(defun synapse-reversal-potential-data (synapse)
  "mV"
  (get-a-value :reversal-potential-data (synapse-parameters synapse)))

(defun push-synapse-reversal-potential-data (synapse data)
  "mV"
  (let ((data (s-flt data)))
    (if (synapse-parameters synapse)
	(push-a-value data :reversal-potential-data (synapse-parameters synapse))
	(setf (synapse-parameters synapse) (acons :reversal-potential-data (list data) '())))))


(defun conc-int-shell-1-data (conc-int)
  "free ion, mM"
  (get-a-value :shell-1-data (conc-int-parameters conc-int)))

(defun push-conc-int-shell-1-data (conc-int data)
  "free ion, mM"
  (let ((data (s-flt data)))
    (if (conc-int-parameters conc-int)
	(push-a-value data :shell-1-data (conc-int-parameters conc-int))
	(setf (conc-int-parameters conc-int) (acons :shell-1-data (list data) '())))))

(defun conc-int-shell-2-data (conc-int)
  "free ion, mM"
  (get-a-value :shell-2-data (conc-int-parameters conc-int)))

(defun push-conc-int-shell-2-data (conc-int data)
  "free ion, mM"
  (let ((data (s-flt data)))
    (if (conc-int-parameters conc-int)
	(push-a-value data :shell-2-data (conc-int-parameters conc-int))
	(setf (conc-int-parameters conc-int) (acons :shell-2-data (list data) '())))))

(defun conc-int-shell-3-data (conc-int)
  "free ion, mM"
  (get-a-value :shell-3-data (conc-int-parameters conc-int)))

(defun push-conc-int-shell-3-data (conc-int data)
  "free ion, mM"
  (let ((data (s-flt data)))
    (if (conc-int-parameters conc-int)
	(push-a-value data :shell-3-data (conc-int-parameters conc-int))
	(setf (conc-int-parameters conc-int) (acons :shell-3-data (list data) '())))))

(defun conc-int-total-data (conc-int)
  "mM, free ion concentration integrated over the entire element volume."
  (get-a-value :total-data (conc-int-parameters conc-int)))

(defun push-conc-int-total-data (conc-int data)
  "mM, free ion concentration integrated over the entire element volume."
  (let ((data (s-flt data)))
    (if (conc-int-parameters conc-int)
	(push-a-value data :total-data (conc-int-parameters conc-int))
	(setf (conc-int-parameters conc-int) (acons :total-data (list data) '())))))


(defun axon-voltage-data (axon)
  "mV"
  (get-a-value :voltage-data (axon-parameters axon)))

(defun push-axon-voltage-data (axon data)
  "mV"
  (let ((data (s-flt data)))
    (if (axon-parameters axon)
	(push-a-value data :voltage-data (axon-parameters axon))
	(setf (axon-parameters axon) (acons :voltage-data (list data) '())))))



(defun buffer-concentration-data (buffer)
  "mM"
  (get-a-value :concentration-data (buffer-parameters buffer)))

(defun push-buffer-concentration-data (buffer data)
  "mM"
  (let ((data (s-flt data)))
    (if (buffer-parameters buffer)
	(push-a-value data :concentration-data (buffer-parameters buffer))
	(setf (buffer-parameters buffer) (acons :concentration-data (list data) '())))))


(defun pump-current-data (pump)
  "mM/ms"
  (get-a-value :current-data (pump-parameters pump)))


(defun push-pump-current-data (pump data)
  "mM/ms"
  (let ((data (s-flt data)))
    (if (pump-parameters pump)
	(push-a-value data :current-data (pump-parameters pump))
	(setf (pump-parameters pump) (acons :current-data (list data) '())))))


(defun extracellular-electrode-field-potential-data (extracellular-electrode)
  "mM"
  (get-a-value :field-potential-data (extracellular-electrode-parameters extracellular-electrode)))
 

(defun push-extracellular-electrode-field-potential-data (extracellular-electrode data)
  "mM"
  (let ((data (s-flt data)))
    (if (extracellular-electrode-parameters extracellular-electrode)
	(push-a-value data :field-potential-data (extracellular-electrode-parameters extracellular-electrode))
	(setf (extracellular-electrode-parameters extracellular-electrode)
	      (acons :field-potential-data (list data) '())))))
   



