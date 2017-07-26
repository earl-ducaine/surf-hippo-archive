;;;; CarGo 3.0
;;;
;;; CarGo is copyright (c) 1994 by Peter Dudey
;;;
;;; Code for user interface
;;; (No graphics version)
;;;
;;; Load this, instead of gui.lisp, for long learning runs when you don't need the graphics.
;;;


;;; PACKAGE

(in-package "GUI")


;;; EXPORTED FUNCTIONS  (Obviously, these don't do anything)

(defun clear-board (&optional lines fancy title new-window))

(defun kill-graphics (&rest windows))

(defun select-window (window))

(defun set-stones (&rest stones))

(defun message (format-string &rest args))

(defun beep ())
