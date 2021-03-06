(in-package :postabon-helper)

(defun alist-as-csv (alist)
  (string-right-trim (list #\Newline)
		     (apply #'concatenate 'string 
			    (loop for (key . val) in alist
			       collect (format nil "~A,~A~%" key val)))))

(defun write-csv (data)
  (cond ((stringp data) data)
	((atom data) (write-to-string data))
	(t (format nil "~{~A~^,~}" data))))

(defun write-csv-recursive (data &optional (new-line nil))
  (typecase data
    (cons (concatenate 'string
		       (when new-line (string #\Newline))
		       (write-csv-recursive (car data) (when (typep (car data) 'cons) t)) 
		       (when (not (null (cdr data))) ",") 
		       (write-csv-recursive (cdr data))))
    (null "")
    (string data)
    (atom (write-to-string data))))

(defun read-csv-stream (stream &key (fn #'identity) (collect t) (state 0) (working-result nil) (results nil) (line nil))
  (let ((cur-char (read-char stream nil)))
    (cond ((null cur-char) 
           (when (or line working-result) ;;Catch in case csv doesn't have a trailing newline
	     (setf results (cons (reverse (cons working-result line)) results)))
           (reverse results))
          ((and (= state 0) 
                (equal cur-char #\")) (read-csv-stream stream :fn fn :collect collect :state 1 :results results :line line))
          ((and (= state 0) (equal cur-char #\,)) 
           (read-csv-stream stream :fn fn :collect collect :state 0 :results results :line (cons working-result line)))
          ((and (= state 0) (equal cur-char #\Newline)) 
           (read-csv-stream stream :fn fn :collect collect :state 0 :results (append (if collect 
											 (list (funcall fn (reverse (cons working-result line))))
											 (progn (funcall fn (reverse (cons working-result line)))
												nil)) 
										     results)))
          ((= state 0) 
           (read-csv-stream stream :fn fn :collect collect :state 0 :results results :working-result
                            (concatenate 'string working-result (string cur-char)) :line line))       
          ((and (= state 1) (equal cur-char #\")) 
           (read-csv-stream stream :fn fn :collect collect :state 2 :results results :line line :working-result working-result))
          ((= state 1) 
           (read-csv-stream stream :fn fn :collect collect :state 1 :results results :line line :working-result 
                            (concatenate 'string working-result (string cur-char))))        
          ((and (= state 2) (equal cur-char #\Newline)) 
           (read-csv-stream stream :fn fn :collect collect :state 0 :results (append (if collect 
											 (list (funcall fn (reverse (cons working-result line))))
											 (progn (funcall fn (reverse (cons working-result line)))
												nil))
									   results)))
          ((and (= state 2) (equal cur-char #\,)) 
           (read-csv-stream stream :fn fn :collect collect :state 0 :results results :line (cons working-result line)))
          ((and (= state 2) (equal cur-char #\")) 
           (read-csv-stream stream :fn fn :collect collect :state 1 :results results :line line :working-result 
                            (concatenate 'string working-result (string cur-char))))
          ((= state 2) 
           (read-csv-stream stream :fn fn :collect collect :state 2 :results results :line line))
          (t (error "Slipped through the FSM. This should be impossible. Cur-char is ~A state is ~A~%" cur-char state)))))

(defun parse-comma-delimited-list (string) 
  (cond ((null string) nil)
	((string= string "") nil)
	(t (mapcar #'(lambda (s)
		       (trim s))
		   (split-on-delimiter string #\,)))))

(defun comma-delimited-list-keywords (string)
  (mapcar #'make-keyword (parse-comma-delimited-list string)))

(defun comma-delimited-list-integers (str) 
  (remove-if #'null 
	     (mapcar #'(lambda (str)
			 (handler-case
			     (parse-integer str)
			   (t nil)))
		     (parse-comma-delimited-list str))))

(defun comma-delimited-list-floats (str) 
  (let ((res (mapcar #'parse-float 
	  (parse-comma-delimited-list str))))
    (if (some #'null res)
	(error "Unable to parse ~A into floats" str)
	res)))

