;;; -*- Mode: LISP; Syntax: Common-Lisp; Package: OPAL; Base: 10 -*-

;; A small change to garnet-restart-function.

(in-package "OPAL")

(defun garnet-restart-function ()
  (when nil ; (string= *username* "lyle")
    (format t "*** Restarting Garnet ~A image created with opal:make-image ***~%"
	    user::Garnet-Version-Number)
    (if (boundp 'garnet-image-date)
      (format t "*** Image creation date: ~A ***~%" garnet-image-date)))
  (opal:reconnect-garnet)
  )


;; LBG Added 7/2/00 since the original appears to choke on DOS names.

;; This is an industrial-strength version of opal:directory-p.  The difference
;; is that extra work is done to ensure that single and double quotes are
;; passed to the shell correctly.  Since it does more work, only use this
;; version if you find you really need it.  This code was contributed by
;; Bruno Haible.

(defun directory-p (pathname)
  ;; Must quote the pathname since Unix shells interpret characters like
  ;; #\Space, #\', #\<, #\>, #\$ etc. in a special way. This kind of quoting
  ;; should work unless the pathname contains #\Newline and we call csh.
  (flet ((shell-quote (string) ; surround a string by single quotes
	   (let ((qchar nil) ; last quote character: nil or #\' or #\"
		 (qstring (make-array 10 :element-type 'character
				      :adjustable t :fill-pointer 0)))
	     (map nil #'(lambda (c)
			  (let ((q (if (eql c #\') #\" #\')))
			    (unless (eql qchar q)
			      (when qchar (vector-push-extend qchar qstring))
			      (vector-push-extend (setq qchar q) qstring))
			    (vector-push-extend c qstring)))
		  string)
	     (when qchar (vector-push-extend qchar qstring))
	     qstring)))
    ;; command-string is the string that's going to be executed.
    (let ((command-string
	   (concatenate 'string "test -d " (shell-quote pathname) " && echo 1")))
      (unless (equal "" (shell-exec command-string))
	T))))
		   
