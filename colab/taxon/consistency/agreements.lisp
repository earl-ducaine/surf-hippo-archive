;************************************************************
;*                                                          *
;*                File : Agreement - Disagreement Manager
;*                                                          *
;************************************************************

(defun make-internal-repr-of-agreement ()
  (cons nil nil))

(defun set-first-arg! (agree arg)
  (rplaca agree arg))

(defun no-arg-so-far? (agree)
  (not (car agree)))

(defun get-first (agree)
  (car agree))


