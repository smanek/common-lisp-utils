(in-package :postabon-helper)

(defun get-system-info ()
  (list (cons :type (machine-type))
	(cons :version (machine-version))
	(cons :name (machine-instance))
	(cons :ip (get-ips))
	(cons :heap (get-heap-stats))
	(cons :uptime (get-uptime))
	(cons :mem (get-mem-stats))))

(defun run-at-shell (bin &optional args)
  (let* ((exit-code 0)
	 (str (with-output-to-string (str)
		(setf exit-code (sb-ext:process-exit-code 
				 (sb-ext:process-wait 
				  (sb-ext:run-program bin args :search t :output str)))))))
    (values str exit-code)))

;;TODO: It just gets the first IP for now. Eventually, I want them all ...
(defun get-ips ()
  (multiple-value-bind (whole match)
      (cl-ppcre:scan-to-strings "inet addr:(\\d{1,3}\.\\d{1,3}\.\\d{1,3})" 
				(run-at-shell "/sbin/ifconfig"))
    (declare (ignore whole))
    match))

(defun get-heap-stats ()
  (with-output-to-string (*standard-output*)
    (room t)))

(defun get-uptime ()
  (run-at-shell "/usr/bin/uptime"))

(defun get-mem-stats ()
  (let ((hash (make-hash-table :test #'equalp)))
    (mapcar #'(lambda (l)
		(multiple-value-bind (all matches) 
		    (cl-ppcre:scan-to-strings "^([a-z-A-Z]+):\\s+(\\d+.*)$" l)
		  (declare (ignore all))
		  (setf (gethash (aref matches 0) hash) (aref matches 1))))
	    (cl-ppcre:all-matches-as-strings 
	     (cl-ppcre:create-scanner "([a-zA-Z]+(Total|Free).*)" :case-insensitive-mode t :multi-line-mode t) 
	     (slurp "/proc/meminfo")))
    hash))