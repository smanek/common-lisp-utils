(in-package :postabon-helper)

(defun class-slot-names (class-name &key (direct t))
  (mapcar #'sb-mop:slot-definition-name
	  (funcall (if direct
		       #'sb-mop:class-direct-slots
		       #'sb-mop:class-slots) (find-class class-name))))

(defmacro while (expression &body body)
  `(tagbody
    start (if (not ,expression) (go end))
      ,@body
      (go start)
    end))

(defun squash (statement)
  (declare (type (or null atom list) statement))
  "returns all atoms in statement as an un-nested list"
  (cond 
   ((null statement) nil)
   ((atom statement) (list statement))
   ('t (append (squash (first statement))
               (squash (rest statement))))))

(defun palinp (s)
  (declare (type (or atom cons simple-array) s))
  (labels ((palinp-helper (s)
	     (cond ((atom s) t)
		   ('t (and 
			(equal (first s) (first (last s))) 
			(palinp-helper (rest (butlast s))))))))
    (cond ((typep s 'simple-array) (palinp-helper (concatenate 'list s)))
	  (t (palinp-helper s)))))

(defun range (start end &optional (res nil))
  (declare (type fixnum start end))
  (if (> start end)
    res
    (range start (1- end) (cons end res))))

(defun safe-subseq (ls start &optional end)
  (cond ((< (length ls) start) nil)
	(t (subseq ls start (when end (min (length ls) end))))))

;;I should probably rewrite this with 'constantly'.
(defun always-true (&rest others)
  (declare (ignore others))
  t)

