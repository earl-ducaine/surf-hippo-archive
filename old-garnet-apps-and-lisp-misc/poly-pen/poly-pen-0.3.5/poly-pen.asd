;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-

;;; Copyright (C) 2004-2005 Yannick Gingras <ygingras@ygingras.net>
;;; Yannick Gingras grants you the rights to distribute
;;; and use this software as governed by the terms
;;; of the Lisp Lesser GNU Public License
;;; (http://opensource.franz.com/preamble.html),
;;; known as the LLGPL.

(defpackage :poly-pen-system (:use #:asdf #:cl))
(in-package #:poly-pen-system)
(defsystem "poly-pen"
    :description "poly-pen: a graphic proxy layer"
    :version "0.3.5"
    :author "Yannick Gingras <ygingras@ygingras.net>"
    :licence "LLGPL"
    :depends-on (sdl sdl-img sdl-ttf cl-gd osicat salza cl-ppcre)
    :components ((:file "examples"
			:depends-on ("renderer" "plot"))
		 (:file "plot"
			:depends-on ("renderer"))
		 (:file "skippy-renderer"
			:depends-on ("renderer" "skippy"))
		 (:file "skippy")
		 (:file "png-renderer"
			:depends-on ("renderer" "png"))
		 (:file "png")
		 (:file "gd-renderer"
			:depends-on ("renderer"))
		 (:file "sdl-renderer"
			:depends-on ("renderer"))
		 (:file "renderer"
			:depends-on ("vector-space" "color" "utils"))
		 (:file "gradient"
			:depends-on ("color"))
		 (:file "color"
			:depends-on ("color-convert" "vector-space"))
		 (:file "color-convert"
			:depends-on ("package"))
		 (:file "utils"
			:depends-on ("package"))
		 (:file "vector-space"
			:depends-on ("package"))
		 (:file "package")
		 (:doc-file "README.txt")
		 (:static-file "ChangeLog")
		 (:module doc
			  :components ((:doc-file "doc.ps")
				       (:doc-file "doc.pdf")
				       (:html-file "index")
				       (:static-file "home.jpg")
				       (:static-file "home2.jpg")
				       (:static-file "example2.jpg")
				       (:static-file "example3.jpg")
				       (:static-file "example4.jpg")
				       (:static-file "example5.jpg")))
		 ))

;; arch-tag: 58b7e297-240d-40f1-a5f8-fb8520a33282
