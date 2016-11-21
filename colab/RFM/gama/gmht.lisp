;;;
;;; gmht.lsp -- hashtables for gmem
;;;
;;; (c) Michael Sintek      12/1991
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


; any hashtable entry consists of 3 cells:
; 1. key (symbol, string or integer)  (or nil for empty entries)
; 2. address ...
; 3. additional information

(defun gmht.make-ht (n)
  ; returns (ht <start-address> <size>)
  (list 'ht (gmem.alloc/clr (* 3 n)) n))

(defun gmht.remove-ht (ht)
  (gmem.dealloc (cadr ht) (* 3 (caddr ht))))

(defun gmht.hash (x size)
  (rem (sxhash x) size))

(defun gmht.put (ht x y z) ; returns pos of first cell
  (let* ((size (caddr ht))
         (start (cadr ht))
         (end (+ start (* size 3)))
         (n (gmht.hash x size))
         (npos (+ start (* n 3)))
         (pos (+ (* 3 n) start)))
       (when (gmem.get pos) ; find other place
             (loop
               (setq pos (+ 3 pos))
               (cond ((= pos end) (setq pos (- start 3))) ; wrap
                     ((null (gmem.get pos)) (return)) ; place found
                     ((= pos npos)
                      (gerror "gmht.lsp" "gmht.put" "hashtable ~a full." ht)))))
       (gmem.put pos x)
       (gmem.put (1+ pos) y)
       (gmem.put (+ 2 pos) z)
       pos))


(defun gmht.get (ht x) ; return position of hashtable entry or nil
  (let* ((size (caddr ht))
         (start (cadr ht))
         (end (+ start (* size 3)))
         (n (gmht.hash x size))
         (npos (+ start (* n 3)))
         (pos (+ (* 3 n) start))
         (key (gmem.get pos)))
       (when (and key (not (equal key x))) ; find other place
             (loop
               (setq pos (+ 3 pos))
               (cond ((= pos end) (setq pos (- start 3))) ; wrap
                     (T (setq key (gmem.get pos))
                        (cond ((or (equal key x) (null key)) (return))
                              ((= pos npos) (setq key nil) (return)))))))
       (when key pos)))


(defun gmht.delete (ht pos) ; mark entry as deleted
  ; ...
  )



(defun gmht.show (ht)
  (do* ((start (cadr ht) (+ start 3))
        (end (+ start (* (caddr ht) 3))))
       ((= start end))
       (when (gmem.get start)
	     (gmem.list start (+ start 2))
	     (gterpri))))

