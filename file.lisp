(in-package :postabon-helper)

(defun path-equal (a b)
  (equal (truename a)
	 (truename b)))

(defun slurp (path)
  (with-open-file (strm path)
    (let ((string (make-string (file-length strm))))
      (read-sequence string strm)
      string)))

