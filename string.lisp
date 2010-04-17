(in-package :postabon-helper)

(defun split-on-delimiter (seq delimiter) 
  (declare (type string seq)
	   (type character delimiter))
  (loop for i = 0 then (1+ j)
     as j = (position delimiter seq :start i)
     collect (subseq seq i j)
     while j))

(defun trim (string)
  (declare (type (or string null) string))
  (when (not (null string)) 
    (string-trim (list #\Space #\Tab #\Newline) string)))

;;have to deal with, e.g.:
;;4
;;-4.2
;;.2
;;0.2
;;4.
;;etc
(defun parse-float (float)
  (let ((trimmed (trim float)))
    (cond ((null trimmed) nil)
	  ((string= "" trimmed) nil)
	  ((null (cl-ppcre:scan "^[+-]?\\d*\.?\\d*$" trimmed)) nil)
	  (t (%parse-float-helper trimmed)))))

(defun %parse-float-helper (float-string &key (start 0) (end nil) (radix 10)
		                      (type 'single-float)
		                      (decimal-character #\.))
  (let ((radix-array (radix-values radix))
        (integer-part 0)
        (mantissa 0)
        (mantissa-size 1)
        (sign 1))
    (with-input-from-string (float-stream (string-upcase float-string) :start start :end end)
      (labels ((peek () (peek-char nil float-stream nil nil nil))
	       (next () (read-char float-stream nil nil nil))
               (sign () ;; reads the (optional) sign of the number
                 (cond
                   ((char= (peek) #\+) (next) (setf sign 1))
                   ((char= (peek) #\-) (next) (setf sign -1)))
                 (integer-part))
               (integer-part ()
                 (cond
                   ((position (peek) radix-array)
                    ;; the next char is a valid char
                    (setf integer-part (+ (* integer-part radix)
                                          (position (next) radix-array)))
                    ;; again
                    (return-from integer-part (integer-part)))
                   ((null (peek))
                    ;; end of string
                    (done))
                   ((char= decimal-character (peek))
                    ;; the decimal seperator
                    (next)
                    (return-from integer-part (mantissa)))                   
                   ;; junk
                   (t (bad-string))))
               (mantissa ()                 
                 (cond
                   ((position (peek) radix-array)
                    (setf mantissa (+ (* mantissa radix)
                                      (position (next) radix-array))
                          mantissa-size (* mantissa-size radix))
                    (return-from mantissa
                      (mantissa)))
                   ((null (peek))
                    ;; end of string
                    (done))
                   (t (bad-string))))
               (bad-string ()
                 ;(error "Unable to parse ~S." float-string)
		 nil)
               (done ()
                 (return-from %parse-float-helper
                   (coerce (* sign (+ integer-part (/ mantissa mantissa-size))) type))))
        (sign)))))

(defun radix-values (radix)
  (assert (<= 2 radix 36)
          (radix)
          "RADIX must be between 2 and 35 (inclusive), not ~D." radix)
  (make-array radix
              :displaced-to "0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ"
              :displaced-index-offset 0
              :element-type 'character))

(defun smart-distance (distance) 
  (let ((df (ceiling (* distance 3.2808399)))) ;;meters to feet
    (cond ((< df 1000) (format nil "~D feet" df))
	  ((< df 10560) (format nil "~1$ miles" (/ df 5280))) ;;2 miles
	  (t (format nil "~D miles" (floor (/ df 5280)))))))

(defun join-strings (delim strings)
  (apply #'concatenate 'string  (cdr (loop for w in strings appending
					  (list delim w)))))

(defun split-into-words (text)
  (let ((words (cl-ppcre:split "[\\s-]" (string-downcase text)))) ;;downcase, and split on whitespace or dashes 
    (setf words (mapcar #'(lambda (x) 
			    (cl-ppcre:regex-replace "^\\W*(.*?)\\W*$" x "\\1")) ;;strip leading/trailing punctuation
			words)) 
    (remove-if #'(lambda (x)
		   (or  (equal "" x) ;;remove empty matches
			(null (cl-ppcre:scan "^\[a-zA-Z'\.\]+$" x)))) ;;or matches that don't consist solely of letters or limited punctuation
	       words)))

(defun ordinal (n)
  (let* ((str (write-to-string n))
	 (reversed-digits (nreverse 
			   (mapcar #'(lambda (char) 
				       (parse-integer (string char))) 
				   (concatenate 'list str))))
	 (last (car reversed-digits))
	 (second-last (cadr reversed-digits))
	 (suffix (cond ((and second-last  
			     (= second-last 1)) "th")
		       ((= last 1) "st")
		       ((= last 2) "nd")
		       ((= last 3) "rd")
		       (t "th"))))
    (concatenate 'string str suffix)))

(defun make-keyword (str)
  (let ((res   (intern (string-upcase str) "KEYWORD")))
    (when (not (or (eq res :||) (eq res :nil)))
      res)))

(defun nonempty-string (str)
  (let ((trimmed (trim str)))
    (cond ((string= "" trimmed) nil)
	  (t trimmed))))

(defun nonempty-boolean (str)
  (when (and str (not (string= str "")))
    t))

(defun unescape-for-html (str)
  (loop for (escaped . unescaped) in (list (cons "&lt;" "<")
					   (cons "&gt;" ">")
					   (cons "&quot;" "\"")
					   (cons "&#039;" "'")
					   (cons "&amp;" "&"))
     with res = str
     do (setf res (cl-ppcre:regex-replace-all escaped res unescaped))
     finally (return res)))

(defun nonempty-float (str) 
  (let ((trimmed (trim str)))
    (cond ((null trimmed) nil)
	  ((string= "" trimmed) nil)
	  (t (parse-float trimmed)))))

(defun truncate-string (str len)
  (if (< (length str) len)
      str
      (concatenate 'string (subseq str 0 (- len 3)) "...")))