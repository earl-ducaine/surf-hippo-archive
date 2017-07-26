

(defparameter abbrev-weekday-table
  '#("Mon" "Tue" "Wed" "Thu" "Fri" "Sat" "Sun"))

(defparameter long-weekday-table
  '#("Monday" "Tuesday" "Wednesday" "Thursday" "Friday" "Saturday"
     "Sunday"))

(defparameter abbrev-month-table
  '#("Jan" "Feb" "Mar" "Apr" "May" "Jun" "Jul" "Aug" "Sep" "Oct" "Nov"
     "Dec"))

(defparameter long-month-table
  '#("January" "February" "March" "April" "May" "June" "July" "August"
     "September" "October" "November" "December"))

(defconstant timezone-table
  '#("GMT" "" "" "" "" "EST" "CST" "MST" "PST"))

(defun valid-destination-p (destination)
  (or (not destination)
      (eq destination 't)
      (streamp destination)
      (and (stringp destination)
	   (array-has-fill-pointer-p destination))))

(defun format-universal-time (destination universal-time
					  &key (timezone nil)
					  (style :short)
					  (date-first t)
					  (print-seconds t)
					  (print-meridian t)
					  (print-timezone t)
					  (print-weekday t))
  "Format-Universal-Time formats a string containing the time and date
   given by universal-time in a common manner.  The destination is any
   destination which can be accepted by the Format function.  The
   timezone keyword is an integer specifying hours west of Greenwich.
   The style keyword can be :short (numeric date), :long (months and
   weekdays expressed as words), :abbreviated (like :long but words are
   abbreviated), or :government (of the form \"XX Mon XX XX:XX:XX\")
   The keyword date-first, if nil, will print the time first instead
   of the date (the default).  The print- keywords, if nil, inhibit
   the printing of the obvious part of the time/date."
  (unless (valid-destination-p destination)
    (error "~A: Not a valid format destination." destination))
  (unless (integerp universal-time)
    (error "~A: Universal-Time should be an integer." universal-time))
  (when timezone
    (unless (and (rationalp timezone) (<= -24 timezone 24))
      (error "~A: Timezone should be a rational between -24 and 24." timezone))
    (unless (zerop (rem timezone 1/3600))
      (error "~A: Timezone is not a second (1/3600) multiple." timezone)))

  (multiple-value-bind (secs mins hours day month year dow dst tz)
		       (if timezone
			   (decode-universal-time universal-time timezone)
			   (decode-universal-time universal-time))
    (declare (ignore dst) (fixnum secs mins hours day month year dow))
    (let ((time-string "~2,'0D:~2,'0D")
	  (date-string
	   (case style
	     (:short "~D/~D/~2,'0D")             ;;  MM/DD/YY
	     ((:abbreviated :long) "~A ~D, ~D")  ;;  Month DD, YYYY
	     (:government "~2,'0D ~:@(~A~) ~D")      ;;  DD MON YY
	     (t
	      (error "~A: Unrecognized :style keyword value." style))))
	  (time-args
	   (list mins (max (mod hours 12) (1+ (mod (1- hours) 12)))))
	  (date-args (case style
		       (:short
			(list month day (mod year 100)))
		       (:abbreviated
			(list (svref abbrev-month-table (1- month)) day year))
		       (:long
			(list (svref long-month-table (1- month)) day year))
		       (:government
			(list day (svref abbrev-month-table (1- month))
			      (mod year 100))))))
      (declare (simple-string time-string date-string))
      (when print-weekday
	(push (case style
		((:short :long) (svref long-weekday-table dow))
		(:abbreviated (svref abbrev-weekday-table dow))
		(:government (svref abbrev-weekday-table dow)))
	      date-args)
	(setq date-string
	      (concatenate 'simple-string "~A, " date-string)))
      (when (or print-seconds (eq style :government))
	(push secs time-args)
	(setq time-string
	      (concatenate 'simple-string time-string ":~2,'0D")))
      (when print-meridian
	(push (signum (floor hours 12)) time-args)
	(setq time-string
	      (concatenate 'simple-string time-string " ~[am~;pm~]")))
      (apply #'format destination
	     (if date-first
		 (concatenate 'simple-string date-string " " time-string
			      (if print-timezone " ~A"))
		 (concatenate 'simple-string time-string " " date-string
			      (if print-timezone " ~A")))
	     (if date-first
		 (nconc date-args (nreverse time-args)
			(if print-timezone
			    (list
			     (let ((which-zone (or timezone tz)))
			       (if (or (= 0 which-zone) (<= 5 which-zone 8))
				   (svref timezone-table which-zone)
				   (format nil "[~D]" which-zone))))))
		 (nconc (nreverse time-args) date-args
			(if print-timezone
			    (list
			     (let ((which-zone (or timezone tz)))
			       (if (or (= 0 which-zone) (< 5 which-zone 8))
				   (svref timezone-table which-zone)
				   (format nil "[~D]" which-zone)))))))))))
