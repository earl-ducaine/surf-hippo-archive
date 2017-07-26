;;;; -*- Mode: Lisp -*- 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; File            : stdio.lisp
;;;; Author          : Blake Ward, Garret Pelton, Ralph Morelli, Joe Mertz
;;;; Created On      : Mon Mar 16 08:58:50 1992
;;;; Last Modified By: Gordon D. Baxter
;;;; Last Modified On: July 18, 1995
;;;; Update Count    : 10
;;;; 
;;;; PURPOSE
;;;;
;;;; This file provides general functions to support Lucid, Allegro
;;;; and Common Lisp. It is meant to be used in conjunction with the
;;;; file "socket.lisp" and provide utilities such as reading, writing
;;;; from socket streams
;;;;
;;;; LOCATION
;;;;   This code together with other codes is packaged together as a socket 
;;;;   support utility called MONGSU. They can be downloaded via anonymous
;;;;   FTP from host 128.243.40.7 (unicorn.ccc.nott.ac.uk, but many machines
;;;;   don't know it, so you may wish to use the numbers) in the directory 
;;;;   "/pub/lpzfr" (From within ftp only the part of the tree rooted at 
;;;;   /usr/ftp is visible).
;;;;
;;;; The following functions provide socket-support for SoarIO.
;;;;
;;;;     read-message-stream
;;;;             reads a "message" from the *socket-stream*.
;;;;     write-message sexpr stream
;;;;             writes a "message" to the stream.
;;;;
;;;; TABLE OF CONTENTS
;;;;   I.  Standard IO Support Utilities
;;;;
;;;; SECTIONS:
;;;; (C) Copyright 1991, Carnegie Mellon University, all rights reserved.
;;;; (C) Other sections Copyright 1995, Roberto L. Ong and Frank E. Ritter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Status       
;;;; HISTORY
;;;;   ??????? - Unknown
;;;;   19Aug94 - RLO put all Lisp-socket utilities here.
;;;;             Modified some existing utilities and added
;;;;             some more.
;;;;   18Jul95 - >>>MONGSU<<< added to all message strings
;;;;   (GDB)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;; load file into default package - "USER"
(in-package "USER")

;;; note load this file
(format t "~%loading stdio.lisp ...............")

;;;
;;;  I.   Standard IO Support Utilities
;;;

(defun read-message-stream ()
  "Read a list from the socket stream."
  ;; read alist
  (let ((inp nil))
    (if (listen *socket-stream*)
      (setq inp (read *socket-stream*)))
    (if *debug-on*
	(format t "~%>>>MONGSU<<< debug: read-message-stream ~s" inp))
  inp))

(defun write-message (alist)
  "Writes a message to *socket-stream* iff a socket link is already
   established."
  (if *debug-on*
      (format t "~%>>>MONGSU<<< |<>| send to Soar ~A |<>|~%" alist))
  (if *socket-connected*
      (progn 
	(write alist :stream *socket-stream*)
	(finish-output *socket-stream*))
    ;;else
    (format t ">>>MONGSU<<< Socket not connected, use connect-socket-io~%")))

(defun do-world()
  "Do stuff if a socket connection is already established."
  (if *socket-connected*
      (do ()(nil)
	(let ((message (read-message-stream)))
	  (if (listp message)
	      (world message)
	    ;;else
	    (format t ">>>MONGSU<<< Caught a non-list message~%"))))
    ;;else
    (format t ">>>MONGSU<<< Socket not connected, use connect-socket-io~%")))

(defun connect-socket-io (server-name server-port)
  "Connects to a client process iff there is no open socket."
  (if  *socket-connected*
       (progn
	 (format t ">>>MONGSU<<< Closing down current connection first~%")
	 (close-socket-io)))
  (connect-socket-stream server-name server-port))

(defun debug (message-string)
  "Debugs a message string."
  (case message-string
    (on (setq *debug-stdio* t))
    (off (setq *debug-stdio* nil))
    (otherwise
     (when *debug-stdio*
       (format t message-string)))))

;; The following utility functions are for setting up a process
;; that would read incoming data from Soar if there are any, and
;; evaluate them depending on the user's application.
;;
;; N.B. These functions are specially built for Lucid Lisp 4.1
;;      and Soar 6
;; -RLO (31Aug94)
 
(defvar *soar-read-loop-process* nil)

(defun launch-a-soar-read-loop-process ()
  "Spawn a process which is doing Soar interaction all the time
   RETURN the process."
  ;; If there was already a process running, kill it.
  (declare (special *socket-stream*))
  (when (lcl:processp *soar-read-loop-process*)
    (progn (lcl:kill-process *soar-read-loop-process*)
	   (setq *soar-read-loop-process* nil)))
  (unless (connected-with-soar-p)
    (set-up-socket-as-server))
  (setf *soar-read-loop-process*
	(lcl:make-process :name "Soar-read-loop-process"
			  :priority 10000 ;; the higher this value,
			                  ;; the better and faster
			                  ;; in terms of speed
			                  ;; -RLO (31Aug94)
			  :function 'soar-read-loop-process)))

(defun soar-read-loop-process ()
  "Creates the body of the Soar read loop process."
  (lcl:handler-bind
   ((lcl::error #'lcl:invoke-debugger))
   (loop
    (when (not (connected-with-soar-p))
      (accept-client-and-make-socket-stream nil))
    (when (and (connected-with-soar-p)
	       (listen *socket-stream*))
      ;; this is where incoming data is read and evaluated,
      ;; you can therefore add your own functions.
      ;; -RLO (31Aug94)
      (setq input (read-message-stream))))))
     	    
(defun connected-with-soar-p ()
  "Test quick and dirty whether a connection exists."
  ;; should later be done with *features*
  (declare (special *client-id*))
  (and *client-id*
       (boundp '*client-id*)
       (not (eq -1 *client-id*))))
