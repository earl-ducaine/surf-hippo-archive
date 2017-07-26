
;;; Copyright (C) 2004-2005 Yannick Gingras <ygingras@ygingras.net>

;;; Yannick Gingras grants you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.

(in-package #:poly-pen)

(defun mklist (obj)
  (if (listp obj)
      obj
    (list obj)))

(defun flatten (x)
  (mapcan #'(lambda (x)
              (if (atom x)
                  (mklist x)
                (flatten x)))
          x))

(defun file-extension (path)
  (let* ((name (namestring path))
	 (pos (position #\. name :from-end t)))
    (if pos
	(subseq name pos)
	"")))

(defun scan-dir-for-font (dir font-hash)
  (osicat:mapdir
   #'(lambda (file)
       (let ((fullpath (merge-pathnames file dir)))
	 (cond  ((eql (osicat:file-kind file) :directory)
		 (scan-dir-for-font fullpath font-hash))
		((equal (file-extension file) ".ttf")
		 (setf (gethash (pathname-name file) font-hash)
		       fullpath)))))
   dir))

(defun find-fonts ()
  ;; TODO: add the other strange places where we can find
  ;; fonts
  (let ((font-hash (make-hash-table :test #'equal)))
    (mapcar #'(lambda (dir)
		(if (eql (osicat:file-kind dir) :directory)
		    (scan-dir-for-font dir font-hash)))
	    '(#p"/usr/share/fonts/"
	      #p"/usr/X11R6/lib/X11/fonts/"))
    font-hash))

(defun run-program (prg args)
  (or #+cmu (ext:run-program prg args)
      #+sbcl (sb-ext:run-program prg args)
      (error "unimplemented")))

(defun tempfile (&optional (template "XXXXXX"))
  "a bit like mkstemp(3)"
  ;; TODO: use sb-posix:mkstemp when merged in a stable release
  ;; TODO: maybe we should give up after a few attempts
  (labels ((helper (template dir)
	     (let ((file (open (merge-pathnames
				(format nil "~a~X~a"
					(subseq template 0
						(or (position #\X template)
						    0))
					(random (1- (expt 2 24)))
					(subseq template 
						(1+ (or (position #\X template
								  :from-end t)
							-1))))
				dir)
			       :direction :io
			       :if-exists nil)))
	       (if file file (helper template dir)))))
    (or #+unix (helper template #p"/tmp/")
	(progn (warn "no temp-dir defined for your platform, using ~a"
		     (namestring *default-pathname-defaults*))
	       (helper template *default-pathname-defaults*)))))

(defun make-log-pos (nb-pos fact)
  "compute NB-POS numbers in [0..1]
progressing by a factor FACT (1.0 for linear)"
  (declare (optimize speed (debug 1) (safety 0))
	   (type fixnum nb-pos))
  (let* ((fact (* 1d0 fact))
	 (inc (expt fact (/ 1d0 (1- nb-pos))))
	 (positions nil))
    (declare (type double-float fact inc)
	     (type list positions))
    (dotimes (i (1- nb-pos))
      (declare (type fixnum i))
      (push (* i (/ (expt inc i) fact (1- nb-pos))) positions))
    (push 1d0 positions)
    (nreverse positions)))

;; arch-tag: 99007806-5f4e-471b-b1c7-d98112836f7d
