(in-package :postabon-helper)

(defun alist-to-plist (alist)
  (loop for (k . v) in alist
     appending (list k v)))


(defun alist-to-hash (alist)
  (loop for (k . v) in alist
     with hash = (make-hash-table :test #'equal)
     do (setf (gethash k hash) v)
     finally (return hash)))

(defun class-slot-names (class &key (direct t))
  (typecase class
    (symbol (class-slot-names (find-class class)))
    (class  (mapcar #'sb-mop:slot-definition-name
		     (funcall (if direct
				  #'sb-mop:class-direct-slots
				  #'sb-mop:class-slots) class)))
    (t (error "Unknown class"))))

(defun object-to-hash (object)
  (let ((slots (class-slot-names (class-of object)))
	(h (make-hash-table :test #'equal)))
    (mapc #'(lambda (slot)
	      (setf (gethash (string-downcase slot) h) (slot-value object slot)))
	  slots)
    h))

(defun print-hash-table (h)
  (loop for k being the hash-key of h
     collecting (format nil "~A => ~A" k (gethash k h)) into line
     finally (return (format nil "~{~A~^~%~}" line))))


;;note: assuming I don't need to make it tail recursive, since I can't imagine
;;a class hierarchy that deep.
(defun get-subclasses (class)
  (typecase class
    (symbol (get-subclasses (find-class class)))
    (class (cons class (mapcan #'(lambda (c)
				   (get-subclasses c))
			       (sb-mop:class-direct-subclasses class))))
    (null  nil)))

(defun subclass-of (child super)
  (handler-case 
      (cond ((typep child 'symbol)
	     (subclass-of (find-class child) super))
	    ((typep super 'symbol)
	     (subclass-of child (find-class super)))
	    (t (member child
		       (get-subclasses super) :test #'equalp)))
    (t () nil)))

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

(defun make-numeric-test (&key min max)
  #'(lambda (x)
      (when (and (or (not min)
		     (< min x))
		 (or (not max)
		     (> max x)))
	t)))

(defun multi-mapcar (list &rest fns)
  (assert (not (null fns)))
  (mapcar #'(lambda (a)
	      (loop for fn in fns
		 with res = a
		 do (setf res (funcall fn res))
		 finally (return res)))
	  list))

;;TODO: this is O(n^2).
;;Might want to optimize to be O(n) when test is eq, equal, or equalp 
;;(by using a hash table)
(defun all-unique-p (lst &key (test #'eq))
  (let ((uniques (loop for item in lst
		    with res = nil
		    do (pushnew item res :test test)
		    finally (return res))))
    (if (= (length uniques) (length lst))
	t)))