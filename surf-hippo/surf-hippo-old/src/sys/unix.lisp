;;; -*- Mode: Lisp; Syntax: Common-lisp; Package: #-PARALLEL SURF #+PARALLEL *SURF; Base: 10; -*-
;;;  (c) Copyright 1990, Lyle Borg-Graham, MIT Center for Biological Information Processing


(defun print-ps-file (file)
  (unix::run-program  (concatenate 'string "lpr " file) nil))



