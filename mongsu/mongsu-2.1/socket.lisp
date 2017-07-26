;;;; -*- Mode: Lisp -*- 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; 
;;;; File            : socket.lisp
;;;; Author          : Blake Ward, Garret Pelton, Ralph Morelli, Joe Mertz
;;;; Created On      : Mon Mar 16 08:58:50 1992
;;;; Last Modified By: Gordon D. Baxter
;;;; Last Modified On: July 18, 1995
;;;; Update Count    : 71
;;;; 
;;;; PURPOSE
;;;;
;;;; This file provides low-level socket support in Lucid, CMU or
;;;; Allegro Common Lisp. It is meant to be used in conjunction with the
;;;; object file "std-soar-socket.o".  The ".o" is a compilation of the
;;;; functions in "std-soar-socket.c" which are imported into LISP and
;;;; declared as foreign functions.
;;;;
;;;; There are two modes for which the socket support has been written.
;;;; The first mode is for use when the Lisp process is a server and 
;;;; make-socket-stream is used to accept a client process (e.g., Soar).
;;;; The second mode is for use when the Lisp process is a client and the
;;;; connect-socket-stream call is used to connect to a Soar process.  
;;;;
;;;; LOCATION
;;;;   This code together with other codes is packaged together as a socket 
;;;;   support utility called MONGSU. They can be downloaded via anonymous
;;;;   FTP from host 128.243.40.7 (unicorn.ccc.nott.ac.uk, but many machines
;;;;   don't know it, so you may wish to use the numbers) in the directory 
;;;;   "/pub/lpzfr" (From within ftp only the part of the tree rooted at 
;;;;   /usr/ftp is visible).
;;;;
;;;; FOREIGN FUNCTIONS DECLARED (These correspond to Unix system calls)
;;;;    (create_socket)         -- creates a UNIX socket
;;;;    (bind_socket sock port) -- binds SOCK to port in the Unix address space
;;;;    (listen_socket sock)    -- wait at SOCK for external process to connect
;;;;    (accept_socket sock)    -- returns file descriptor of external socket
;;;;    (connect_socket sock name port) -- connects to server PORT on NAME
;;;;                                       and SOCK
;;;;    (close_socket sock)     -- closes the socket SOCK
;;;;    (shutdown_socket sock how) -- discontinues communication at socket SOCK
;;;;                                  and HOW (the method of shutdown)
;;;;
;;;; Based on the above foreign functions, the following functions provide
;;;; socket-support for SoarIO.
;;;;
;;;;     make-socket-stream
;;;;             creates a socket using a wildcard 'port', waits
;;;;             for someone to connect to it and then creates an
;;;;             input/output character stream that uses the socket
;;;;             and the socket. Uses global variables *socket-id*,
;;;;             *socket-stream*, *socket-port* and *client-id*.
;;;;     connect-socket-stream server_name server_port
;;;;             creates a socket and connects to server_port on system
;;;;             server_name
;;;;     shutdown-socket-stream
;;;;             Closes *socket-stream* and *socket-id* and resets the
;;;;             corresponding global variables. 
;;;;     close-socket-io
;;;;             Closes *socket-stream* and *client-id* and resets the
;;;;             corresponding global variables. 
;;;;     create-socket-process-file
;;;;             Creates a ".socket.process" file containing the hostname and
;;;;             port-number for automatic socket hookup.
;;;;
;;;; TABLE OF CONTENTS
;;;;
;;;;   0.    Preliminaries (ignore and goto section I to set up!)
;;;;   i.    User Settable Variable
;;;;   ii.   Variables most users won't care but could change
;;;;   iii.  System Variables
;;;;   iv.   Load Foreign Functions
;;;;   I.    Create a Server Process
;;;;   II.   Create a Client Process
;;;;   III.  Shutdown Socket Stream
;;;;   IV.   Close Socket IO
;;;;   V.    Socket-Process File Creation
;;;;   VI.   Test Code
;;;; 
;;;; Sections:
;;;; (C) Copyright 1991, Carnegie Mellon University, all rights reserved.
;;;; (C) Other sections Copyright 1995, Roberto L. Ong and Frank E. Ritter
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Status          : Debbugged only the Allegro code. (RM) Ditto. (JM)
;;;;                   Used routinely with Lucid Lisp (RLO)
;;;; HISTORY        
;;;; unknown - First version of this interface was created by BW for ET-Soar.
;;;;         - Second version was created by GAP for draw-Soar on cT.
;;;;         - Current version by RM for Soar/ITS (also for cT-Soar).
;;;;         - Added connect-socket-stream and *soar6-mode*.
;;;; 14Dec94 - Added create-socket-process-file
;;;; (RLO)
;;;; 11Jan95 - Added error messages in opening the *socket-process-filename*
;;;; (RLO)
;;;; 22Feb95 - Modified and added documentation strings.
;;;; (RLO)
;;;; 18Jul95 - >>>MONGSU<<< added to message strings
;;;; (GDB)
;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;
;;;   0.   Preliminaries (ignore and goto section I to set up!)
;;;

;; load file into default package - "USER"
;; -RLO (10Sep94)
(in-package "USER")

;; note load this file
(format t "~%loading socket.lisp ..............")

;; The following are for noting that the code has been loaded and its 
;; version. -RLO (17Jan95)
(push :mongsu *features*)
(push :mongsu-V2.0 *features*)


;;;
;;;   i.   User Settable Variable
;;;

;; define global variable
;; replace the path for *socket-o-file* with the appropriate path in the form:
;; "<soar-directory>/obj/nnpscm/<machine-directory>/std-soar-socket.o>"
;; -RLO (22Feb95)
(defparameter *socket-o-file*
  "/psyc/lang/soar/6/2/5/obj/nnpscm/sun4/std-soar-socket.o")


;;;
;;;   ii.   Variables most users won't care about but could change
;;;

;; change this only if you use this name for something else
(defparameter *socket-process-filename* "~/.socket.process")

(defparameter *socket-process-file-comment*
  ";; This file was created by the server process that contains
;; the <hostname> and <port-number>. This will be read by
;; the client process for use in connecting to the server
;; socket.
;; This file will always be deleted after the server socket
;; is shutdown.
;; -RLO (11Jan95)")


;;;
;;;   iii.   System Variables
;;;

(defparameter *soar6-mode* t
  "Flag for setting variables that only works on soar 6.")
(defparameter *debug-on* nil
  "Flag used for debugging purposes.")

;; initializations
(defparameter *sockets-on* t
  "For keeping track of the status of sockets.")
(defparameter *socket-connected* nil
  "For keeping track if both client and server sockets are connected.")
(defparameter *socket-id* nil
  "Initial value of the socket id.")
(defparameter *socket-port* nil
  "Initial value of the socket port.")
(defparameter *socket-host* nil
  "Initial name of the socket host.")
(defparameter *socket-stream* nil
  "Initial status of the socket stream.")
(defparameter *client-id* nil
  "Initial value of the client id.")

;; set *print-case* if using Soar 6
(if *soar6-mode* (setq *print-case* :downcase))

;;; gc before foreign function load -fer 14Sep90
#+:ALLEGRO
   (excl:gc t)

;;; commented because of garbage collection problems -RLO (29Aug94)
#-:LUCID
   (gc t)


;;;
;;;   iV.   Load Foreign Functions
;;;

;;; Definitions of foreign functions only works for LUCID 4.1.
;;; If other versions of Lucid Lisp is used, reference to the manual
;;; is strongly suggested.
;;; -RLO (09Sep94)
#+(and :LUCID :LCL4.1)
(if *sockets-on* 
    (progn
      (def-foreign-function (create_socket (:name "_create_socket")))
      (def-foreign-function (bind_socket (:name "_bind_socket")) sock)
      (def-foreign-function (listen_socket (:name "_listen_socket")) sock)
      (def-foreign-function (accept_socket (:name "_accept_socket")) sock)
      (def-foreign-function (close_socket (:name "_close_socket")) sock)
      (def-foreign-function
	(shutdown_socket (:name "_shutdown_socket")) sock how)
      (def-foreign-function
	(connect_socket (:name "_connect_socket"))
	server-fd server-name port-no)
      (load-foreign-files *socket-o-file*)))

#-(and :LUCID :LCL4.1)
(if *sockets-on* 
    (progn
      (define-foreign-function :c 'create_socket :integer)
      (define-foreign-function :c 'bind_socket   :integer)
      (define-foreign-function :c 'listen_socket :integer)
      (define-foreign-function :c 'accept_socket :integer)
      (define-foreign-function :c 'close_socket  :integer)
      (define-foreign-function :c 'shutdown_socket :integer)
      (define-foreigh-function :c 'connect_socket :integer)
      (load-foreign-files '(*socket-o-file*)) ))

#+:CMU
(if *sockets-on* 
    (progn
      (extensions:load-foreign "std-soar-socket.o")
      (extensions:def-c-routine "create_socket" (int) ())
      (extensions:def-c-routine "bind_socket" (int) (soc int))
      (extensions:def-c-routine "listen_socket" (int) (soc int))
      (extensions:def-c-routine "accept_socket" (int) (soc int))
      (extensions:def-c-routine "close_socket" (int) (soc int))
      (extensions:def-c-routine "shutdown_socket" (int) (soc int how int))
      (extensions:def-c-routine "connect_socket" (int)
				(soc int name string?   port-no int))
      ))

#+:DEC3100
(if *sockets-on*
    (load *socket-o-file*))

#+:ALLEGRO
(if *sockets-on*
    (progn
      (ff:defforeign 'create_socket :arguments '()
		     :return-type :integer)
      (ff:defforeign 'bind_socket :arguments '(fixnum) 
		     :return-type :integer)
      (ff:defforeign 'listen_socket :arguments '(fixnum)
		     :return-type :integer)
      (ff:defforeign 'accept_socket :arguments '(fixnum)
		     :return-type :integer)
      (ff:defforeign 'close_socket :arguments '(fixnum)
		     :return-type :integer)
      (ff:defforeign 'shutdown_socket :arguments '(fixnum fixnum)
		     :return-type :integer)
      (ff:defforeign 'connect_socket :arguments '(fixnum simple-string fixnum)
		     :return-type :integer)
      ))


;;;
;;;   I.   Create a Server Process
;;;

;;; Creates a server process by creating a socket,
;;; and listen for incoming requests for connection
(defun make-socket-stream (&optional (messages t))
  "Create a socket when the Lisp process is the server."
  ;; create a socket
  (setq *socket-id* (create_socket))               
  (if (= *socket-id* -1)
    (error ">>>MONGSU<<< Unable to create socket~%"))
  (if messages (format t ">>>MONGSU<<< Created socket ~d.~%" *socket-id*))

  ;; bind the socket
  (setq *socket-port* (bind_socket *socket-id*)) 
  (if (= *socket-port* -1)
    (error ">>>MONGSU<<< Unable to bind socket ~d.~%" *socket-id*))
  (if messages (format t ">>>MONGSU<<< Bound socket ~d to port ~d~%" *socket-id*
			 *socket-port*))  
  (if messages (format t ">>>MONGSU<<< Listening at socket ~d~%" *socket-id*))

  ;; wait for a connection
  (setq ret-code (listen_socket *socket-id*))   
  (if (= ret-code -1)
      (progn (format t ">>>MONGSU<<< Error code ~d~%" ret-code)
	     (error ">>>MONGSU<<< Unable to listen socket ~d.~%" *socket-id*))))



;;;
;;;   II.   Create a Client Process
;;;

;;; creates a client process
;;; Syntax: (connect-socket-stream "server-name" server-port-no)
(defun connect-socket-stream (server-name server-port-no
					  &optional (messages t))
  "Create a socket when the Lisp process is a client."
  ;; create a raw socket
  (setq *client-id* (create_socket))                
  (if (= *client-id* -1)
      (error ">>>MONGSU<<< Unable to create socket~%" ))
  (if messages (format t ">>>MONGSU<<< Created socket ~d.~%" *client-id*))

  ;; connect to a server
  (setq *socket-port* server-port-no)
  (setq retno (connect_socket *client-id* server-name server-port-no))
  (if (= retno -1)
      (error ">>>MONGSU<<< Error in connecting to socket~%"))
  (if messages (format t ">>>MONGSU<<< Connected to server, file descriptor is: ~d~%"
			 *client-id*))

  (setq *socket-connected* t)
  
  ;; make a Lisp stream for socket
  #+:LUCID
  (setq *socket-stream* 
	(make-lisp-stream :input-handle *client-id*
			  :output-handle *client-id* :stream-type :ephemeral))
  #+:CMU
  (setq *socket-stream*
	(lisp::make-terminal-stream *client-id* *client-id*))

  #+:ALLEGRO
  (setq *socket-stream*
        (excl::make-buffered-terminal-stream *client-id* *client-id* t t))
)


;;;
;;;   III.   Shutdown Socket Stream
;;;

;;; shutdown-socket-stream performs an orderly shutdown of the socket
;;; connection. It can be used during development & debugging.
(defun shutdown-socket-stream ()
  "Shutdown a socket stream."
  (if *socket-stream*
      (progn
	(shutdown_socket *socket-id* 2)
	(format t ">>>MONGSU<<< Channel interface is shutdown.~%")
	(setq *client-id* nil)
	(close *socket-stream* :abort t)
	(setq *socket-stream* nil)))
  (close_socket *socket-id*)
  (format t ">>>MONGSU<<< Socket ~d closed.~%" *socket-id*)
  (setq *socket-connected* nil)  
  (setq *socket-id* nil)
  (setq *socket-port* nil)
  (setq *sockets-on* nil)
  (delete-file *socket-process-filename*))


(eval-when (eval compile load)
  (if (not *soar6-mode*) (soarsyntax)))

;;; The following statements are commented since cT is not used
;;; in our system -RLO (19Aug94)
;;  (and (not (member :ct *features*))
;;       (pushnew :ct *features*))


;;;
;;;   IV.   Close Socket IO
;;;

;; Modified this function to work on closing a
;; client socket (if Lisp is a client).
;; -RLO (15Sep94)
(defun close-socket-io ()
  "Close an open client socket."
  (if *socket-connected*
      (progn
	(shutdown_socket *client-id* 2)
	(format t ">>>MONGSU<<< Channel interface is shutdown.~%")
	(close_socket *client-id*)
	(setq *socket-stream* nil)
	(setq *client-id* nil)
	(setq *socket-id* nil)
	(setq *socket-port* nil)
	(setq *sockets-on* nil)
	(setq *socket-connected* nil))
    ;; else
    (format t ">>>MONGSU<<< Socket not connected~%")))


;;;
;;;   V.   Socket-Process File Creation
;;;

(defun create-socket-process-file ()
  "Create a socket-process file containing <hostname> and <port-number>."
  (let (port-number)
    (with-open-file
     (socket-stream *socket-process-filename*
		    :direction :output)
     (setq *socket-host* (machine-instance))
     (setq port-number (format nil "~s" *socket-port*))
     (write-line *socket-host* socket-stream)
     (write-line port-number socket-stream)
     (terpri socket-stream)
     (write-line *socket-process-file-comment* socket-stream))))


;;;
;;;   VI.   Test Code
;;;

;;; The following functions tests the socket code by setting up a simple
;;; "Lisp server".  It creates a socket, accepts Lisp Sexprs from it,
;;; evaluates them and returns the result. It does no error checking,
;;; so is not robust if the form contains an error. This is used in
;;; conjunction with "clienttest.out" (complied version of "clienttest.c")
;;; -RLO (31Aug94)

(defun set-up-socket-as-server ()
  "Creates a server socket iff there is no existing open socket."
  (if (not (probe-file *socket-process-filename*))
      (progn
	(setq *sockets-on* t)
;	(setq *debug-on* nil)
	(make-socket-stream)
	;; process the file only if it does not exists
	(create-socket-process-file))
    (format t "~%>>>MONGSU<<< ERROR: ~s already exists.
There is already an existing socket.
Shutdown existing socket-stream before creating another socket."
	    *socket-process-filename*)))

(defun accept-client-and-make-socket-stream (&optional (messages t))
  "Accepts a client."
  (setq *client-id* (accept_socket *socket-id*))
  (when (and messages (= *client-id* -1))
    (format t ">>>MONGSU<<< Client has not yet connected.~%"))
  (when (and messages (not (= *client-id* -1)))
      (format t ">>>MONGSU<<< Connected to client, file descriptor is: ~d~%"
              *client-id*))  

  #+:LUCID
  (unless (= *client-id* -1)  
    (setq *socket-stream* 
	  (make-lisp-stream :input-handle *client-id*
			    :output-handle *client-id*
			    :stream-type :ephemeral)))
  
  #+:CMU
  (unless (= *client-id* -1)  
    (setq *socket-stream*
	  (lisp::make-terminal-stream *client-id* *client-id*)))

  #+:ALLEGRO
  (unless (= *client-id* -1)
    (setq *socket-stream*
	  (excl::make-buffered-terminal-stream *client-id* *client-id* t t)))

  (unless (= *client-id* -1)
    (setq *socket-connected* t)))

(defun do-stuff ()
  "Do-stuff when socket is already connected."
  (when (= *client-id* -1)
    (accept-client-and-make-socket-stream))
  (unless (= *client-id* -1)
    (setq input (read-message-stream))
    (format t ">>>MONGSU<<< Received message: ~a~%" input)
    (format t ">>>MONGSU<<< Evaluated message: ~a~%" (eval input))
    (shutdown-socket-stream)))

;;; The following functions tests the socket code by setting up a simple
;;; "Lisp client".  It connects to a named socket and port-number.
;;; A list  is then sent to the server and printed out. This is used in
;;; conjunction with "servertest.out" (compiled version of "servertest.c").
;;; -RLO (31Aug94)

;; Syntax: set-up-socket-as-client
(defun set-up-socket-as-client ()
  "Creates a server process."
  (if (probe-file *socket-process-filename*)
      (let (socket-port-no)
	;; process the file only if it exists
	(with-open-file
	 (socket-stream *socket-process-filename*
			:direction :input)
	 (setq *socket-host* (read-line socket-stream))
	 (setq socket-port-no (read-from-string
			       (read-line socket-stream)))
	 (connect-socket-io *socket-host* socket-port-no)))
    (format t "~%>>>MONGSU<<< ERROR: Unable to open ~s file.
There is no existing server socket.
First create a server socket before connecting to it."
	    *socket-process-filename*)))

(defun test-write ()
  "Writes a message to the server and prints it."
  (if *socket-connected*
      (progn
	(write-message 'hello-there)
	(write-message 'this-is-a-test)
	(close-socket-io))))
