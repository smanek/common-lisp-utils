(in-package :postabon-helper)

(defun day-of-week (day month year)  
  "Returns the day of the week as an integer.
Monday is 0."
  (declare (type (and fixnum (integer 1 31)) day)
	   (type (and fixnum (integer 1 12)) month)
	   (type (and fixnum (integer 0)) year))
    (nth-value
     6
     (decode-universal-time
      (encode-universal-time 0 0 0 day month year 0)
      0)))

(defun days-in-month (year month)
  (declare (type (and fixnum (integer 1 12)) month)
	   (type (and fixnum (integer 0)) year))
  (cond ((or (= month 9) (= month 10) (= month 4) (= month 6)) 30)
	((= month 2) (if (= 0 (mod year 4)) 29 28))
	('t 31)))

(defun smart-date (date) 
  (let ((diff (- (get-universal-time) date)))
    (cond ((< diff 60) (format nil "~A second~:p ago" diff))
	  ((< diff 3600) (format nil "~A minute~:p ago" (floor (/ diff 60))))
	  ((< diff 86400) (format nil "~A hour~:p ago" (floor (/ diff 3600))))
	  (t (format nil "~A day~:p ago" (floor (/ diff 86400)))))))

(defun pretty-print-utime (utime &key (stream t)) 
  (let ((day-names '("Monday" "Tuesday" "Wednesday"
		     "Thursday" "Friday" "Saturday"
		     "Sunday")))    
    (multiple-value-bind
	  (second minute hour date month year day-of-week dst-p tz)
	(decode-universal-time utime)
      (declare (ignore dst-p tz))
      (format stream "~d/~2,'0d/~d, ~2,'0d:~2,'0d:~2,'0d ~a"
	      month
	      date
	      year
	      hour
	      minute
	      second
	      (nth day-of-week day-names)))))

(defun pretty-print-date (&key (utime (get-universal-time)))
  (let ((days (list "Monday"
		    "Tuesday"
		    "Wednesday"
		    "Thursday"
		    "Friday"
		    "Saturday"
		    "Sunday"))
	(months (list "January"
		      "February"
		      "March"
		      "April"
		      "May"
		      "June"
		      "July"
		      "August"
		      "September"
		      "October"
		      "November"
		      "December")))
    (multiple-value-bind 
	  (second minute hour date month year day-of-week dst-p tz)
	(decode-universal-time utime)
      (declare (ignore second minute hour dst-p tz year))
      (format nil "~A, ~A ~A" (elt days day-of-week)
	                       (elt months month)
			       (ordinal date)))))

(defun get-current-year () 
  (multiple-value-bind
	(second minute hour date month year day-of-week dst-p tz)
      (decode-universal-time (get-universal-time))
    (declare (ignore second minute hour date month day-of-week dst-p tz))
    year))

(defun utime-yesterday ()
  (- (get-universal-time)
     (* 24 60 60)))

(defun utime-last-week ()
  (- (get-universal-time)
     (* 7 24 60 60)))

