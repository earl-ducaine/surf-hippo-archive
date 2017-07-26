;;;; 
;;;; skippy.lisp
;;;; 
;;;; Created: 2005-03-06 by Zach Beane <xach@xach.com>
;;;; 
;;;; GIF-writing functions. Only two functions are exported. Example
;;;; usage:
;;;;
;;;; (make-instance 'gif
;;;;                :height 10 :width 10 :bpp 8
;;;;                :color-table <768-entry color table>
;;;;                :image-data <100-entry index vector>)
;;;;
;;;; (write-gif <gif instance> pathname)
;;;;
;;;;
;;;; The color table must be an (unsigned-byte 8) vector with (* 3
;;;; (expt bpp 2)) entries; the table is in the format
;;;;
;;;;    #(red0 green0 blue0 ... redN greenN blueN)
;;;;
;;;;
;;;; See also:
;;;;
;;;;   - CompuServe Incorporated, "Graphics Interchange Format, Version 89a"
;;;;
;;;;   - Steve Blackstock, "LZW and GIF explained"
;;;;
;;;; Both of the above are available from:
;;;;
;;;;   http://www.dcs.ed.ac.uk/home/mxr/gfx/2d-hi.html
;;;;
;;;;
;;;; This file is released into the public domain.
;;;;
;;;; $Id: skippy.lisp,v 1.3 2005/03/07 01:20:50 xach Exp $


(defpackage :skippy
  (:nicknames :gif)
  (:use :cl)
  (:export :gif :write-gif :write-gif-to-stream))

(in-package :gif)

(defun write-uint16 (number stream)
  (write-byte (logand #xFF number) stream)
  (write-byte (ash number -8) stream))


;;; The GIF structure

(defclass gif ()
  ((height :initarg :height :reader height)
   (width :initarg :width :reader width)
   (bpp :initarg :bpp :reader bpp)
   (image-data :initarg :image-data :reader image-data)
   (color-table :initarg :color-table :reader color-table)))


(defmethod initialize-instance :after ((gif gif) &key &allow-other-keys)
  (assert (= (length (image-data gif)) (* (height gif) (width gif))))
  (assert (= (length (color-table gif)) (* 3 (expt 2 (bpp gif))))))



;;; Image data is output in blocks of 255 bytes. The bytes are filled
;;; in variable-length chunks.
;;;
;;; FIXME: This is a very good candidate for optimization.

(defclass image-data-stream ()
  ((bit-index :initform 0 :accessor bit-index)
   (byte-index :initform 0 :accessor byte-index)
   (buffer :initform (make-array 255
                                 :element-type '(unsigned-byte 8)
                                 :initial-element 0)
           :reader buffer)
   (output-stream :initarg :output-stream :reader output-stream)))

(defgeneric reset-stream (stream))
(defgeneric write-buffer (stream))
(defgeneric write-bit (bit stream))
(defgeneric write-bits (integer length stream))

(defmethod reset-stream ((stream image-data-stream))
  (setf (bit-index stream) 0
        (byte-index stream) 0)
  (fill (buffer stream) 0))

(defmethod write-buffer ((stream image-data-stream))
  (with-slots (output-stream byte-index bit-index)
      stream
    (unless (and (zerop bit-index) (zerop byte-index))
      (unless (zerop bit-index)
        (incf byte-index))
      (write-byte byte-index output-stream)
      (write-sequence (buffer stream) output-stream :end byte-index)
      (reset-stream stream))))

(defmethod write-bit (bit (stream image-data-stream))
  (with-slots (bit-index byte-index buffer output-stream)
      stream
    (setf (aref buffer byte-index)
          (logior (ash bit bit-index) (aref buffer byte-index)))
    (incf bit-index)
    (when (= bit-index 8)
      (setf bit-index 0)
      (incf byte-index))
    (when (= byte-index 255)
      (write-buffer stream))))

(defmethod write-bits (integer length (stream image-data-stream))
  (loop for i = integer then (ash i -1)
        repeat length do
        (write-bit (logand #x01 i) stream)))


;;; Testing this crap

(defun file->octets (file)
  (with-open-file (stream file
                   :direction :input
                   :element-type '(unsigned-byte 8))
    (let ((buffer (make-array (file-length stream)
                              :element-type '(unsigned-byte 8))))
      (read-sequence buffer stream)
      buffer)))

(defun test-write-bits (file)
  (with-open-file (stream file
                   :direction :output
                   :element-type '(unsigned-byte 8)
                   :if-exists :supersede)
    (let ((data-stream (make-instance 'image-data-stream
                                      :output-stream stream)))
      (dotimes (i 4)
        (write-bits 1 10  data-stream))
      (write-buffer data-stream)))
  (file->octets file))

(defmacro def-bit-test (name (&rest pairs) result)
  `(defun ,name ()
     (with-open-file (stream "bits.dat"
                      :direction :output
                      :element-type '(unsigned-byte 8)
                      :if-exists :supersede)
       (let ((data-stream (make-instance 'image-data-stream
                                         :output-stream stream)))
         ,@(loop for (number width) on pairs by #'cddr
                 collect `(write-bits ,number ,width data-stream))
         (write-buffer data-stream)))
     (let ((bytes (file->octets "bits.dat")))
       (when (not (equalp bytes ,result))
         (format t "FAILED: ~A /= ~A~%" bytes ,result)))))

(def-bit-test bit-test.1
    (1 8 1 8 1 8 7 7)
  #(4 1 1 1 7))

(def-bit-test bit-test.2
    (0 4 #b1111 4)
  #(1 #xF0))

(def-bit-test bit-test.3
    (#b1111 4 0 4)
  #(1 #x0F))

(def-bit-test bit-test.4
    (1 7 0 7)
  #(2 1 0))

(def-bit-test bit-test.5
    (1 3  1 3  0 3  1 3)
  #(2 #b00001001 #b00000010))
       

                  
        

;;; Converting image data to a compressed form

(defun lzw-compress (vector code-size stream)
  (let ((iv 0)
        (data-stream (make-instance 'image-data-stream :output-stream stream)))
    (flet ((next-input ()
             (when (< iv (length vector))
               (prog1
                   (aref vector iv)
                 (incf iv)))))
      (let* ((string-table (make-hash-table))
             (clear-code (expt 2 code-size))
             (end-of-input-code (1+ clear-code))
             (index (+ 2 clear-code))
             (compression-size (1+ code-size))
             (max-index (1- (expt 2 compression-size)))
             (prefix (next-input))
             (next-char nil))
        (flet ((output-code (code)
                 (write-bits code compression-size data-stream)))
          (output-code clear-code)
          (loop
           (setf next-char (next-input))
           (when (null next-char)
             (output-code prefix)
             (output-code end-of-input-code)
             (write-buffer data-stream)
             (return))
           (let* ((key (logior (ash prefix 8) next-char))
                  (entry (gethash key string-table)))
             (cond (entry
                    (setf prefix entry))
                   (t
                    (output-code prefix)
                    (setf (gethash key string-table) index)
                    (when (> index max-index)
                      (setf max-index (1- (expt 2 (incf compression-size)))))
                    (incf index)
                    (setf prefix next-char))))
           (when (= index #xFFF)
             ;; The index isn't allowed to be this big, so the string
             ;; table must be cleared out and restarted
             (output-code clear-code)
             (setf compression-size (1+ code-size))
             (setf max-index (1- (expt 2 compression-size)))
             (clrhash string-table)
             (setf index (+ 2 clear-code)))))))))



;;; Writing out the GIF file format

(defun string->octets (string)
  (map 'vector #'char-code string))

(defconstant +global-color-table-present+ 0
  "Global color tables are not supported.")

(defconstant +pixel-aspect-ratio+ 0
  "Pixel aspect ratios are not set.")

(defun write-gif-header (gif stream)
  (write-sequence (string->octets "GIF89a") stream)
  (write-uint16 (width gif) stream)
  (write-uint16 (height gif) stream)
  (write-byte (ash (1- (bpp gif)) 4) stream)
  (write-byte +global-color-table-present+ stream)
  (write-byte +pixel-aspect-ratio+ stream))


(defconstant +image-separator-code+ #x2C)

(defconstant +image-left-position+ 0
  "Since there's only one image in the stream, put it all the way to
the left.")

(defconstant +image-top-position+ 0
  "Since there's only one image in the stream, put it all the way at
the top.")

(defconstant +local-color-table-flag+ #x80
  "The bit for the local color table is always set.")


(defun write-image-descriptor (gif stream)
  (write-byte +image-separator-code+ stream)
  (write-uint16 +image-left-position+ stream)
  (write-uint16 +image-top-position+ stream)
  (write-uint16 (width gif) stream)
  (write-uint16 (height gif) stream)
  (write-byte (logior +local-color-table-flag+
                      (1- (bpp gif)))
              stream))

(defun write-local-color-table (gif stream)
  (write-sequence (color-table gif) stream))

(defun write-image-data (gif stream)
  (write-byte (bpp gif) stream)
  (lzw-compress (image-data gif) (bpp gif) stream)
  (write-byte 0 stream))


(defconstant +gif-trailer-code+ #x3B
  "The end-of-GIF marker.")

(defun write-gif (gif file &key (if-exists :supersede))
  (with-open-file (stream file
                   :direction :output
                   :if-exists if-exists
                   :element-type '(unsigned-byte 8))
    (write-gif-header gif stream)
    (write-image-descriptor gif stream)
    (write-local-color-table gif stream)
    (write-image-data gif stream)
    (write-byte +gif-trailer-code+ stream)
    file))

(defun write-gif-to-stream (gif stream)
  (write-gif-header gif stream)
  (write-image-descriptor gif stream)
  (write-local-color-table gif stream)
  (write-image-data gif stream)
  (write-byte +gif-trailer-code+ stream))

;; arch-tag: 71110d54-58ee-455c-b0ab-c160be7a0ef7
