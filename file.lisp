(in-package :postabon-helper)

(defun path-equal (a b)
  (equal (truename a)
	 (truename b)))

(defun slurp (path)
  (with-open-file (stream path :direction :input)
    (with-output-to-string (out)
      (do ((line (read-line stream nil stream) (read-line stream nil stream)))
	  ((eq line stream))
	(write-line line out)))))
