(defvar options)
(setq options "-x")

(setq *PRINT-CASE* :downcase)

(defun ht (&aux input)
  (format t "Help Test~%~%")
  (loop
   (format t "> ")
   (setq input (read-from-string (format nil "(~a)" (read-line))))
   (cond ((eq (car input) 'help)
	  (system (format nil "help ~a -d . ~a" (cadr input) options)))
	 ((eq (car input) 'opt)
	  (setq options (cadr input))))))



