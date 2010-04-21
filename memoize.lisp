(in-package :postabon-helper)

;;Grossly suboptimal. I should really write my own linked hash table
;;Reads: O(log(n)) - the fibonacci heap has to delete an arbitrary key and reinsert
;;Write: O(1) when empty, O(log(n)) when full
(defclass lru ()
  ((capacity     :initform (error "Must supply capacity")
		 :initarg :capacity
		 :reader get-capacity)
   (size         :initform 0
		 :accessor get-size)
   (heap         :initform (make-instance 'cl-heap:fibonacci-heap :key #'car)
		 :reader get-heap)
   (hash         :initform (make-hash-table :test #'equalp)
		 :reader get-hash)
   (counter      :accessor get-counter
		 :initform 0)))


(defun make-lru (capacity)
  (make-instance 'lru :capacity capacity))

(defmethod add-lru ((l lru) k v)
  (multiple-value-bind (old-val old-present)
      (gethash k (get-hash l))
    (if old-present
	(cl-heap:delete-from-heap (get-heap l) (second old-val))
	(progn 
	  (incf (get-size l))
	  (when (> (get-size l) (get-capacity l))
	    (delete-least-recently-used l))))

    (setf (gethash k (get-hash l))
	  (list v
		(multiple-value-bind (i heap-key)
		    (cl-heap:add-to-heap (get-heap l) (list (incf (get-counter l)) k))
		  (declare (ignore i))
		  heap-key)))))

(defmethod delete-least-recently-used ((l lru))
  (let ((k (cl-heap:pop-heap (get-heap l))))
    (when k
      (remhash (second k) (get-hash l))
      (decf (get-size l)))))

(defmethod display-contents ((l lru))
  (print-hash-table (get-hash l)))

(defmethod get-lru ((l lru) k)
  (multiple-value-bind (res present-p)
      (gethash k (get-hash l))
    (if present-p
	(progn
	  (cl-heap:delete-from-heap (get-heap l) (second res))
	  (multiple-value-bind (i heap-key)
	      (cl-heap:add-to-heap (get-heap l) (list (incf (get-counter l)) k))
	    (declare (ignore i))
	    (setf (cdr (gethash k (get-hash l))) (list heap-key)))
	  (values (car res) t))
	(values nil nil))))

(defun make-memo (fn &key (expires 30) (size 100))
  (let ((cache (make-lru size)))
    #'(lambda (&rest args)
	(multiple-value-bind (val found-p) 
	    (get-lru cache args)
	  (let ((expiration (car val))
		(res (second val)))
	    (if (and found-p (or (null expiration)
				 (> expiration (get-universal-time))))
		res
		(cadar
		 (add-lru cache args 
			  (list (when expires
				  (+ (get-universal-time) expires))
				(apply fn args))))))))))

(defun memoize-fn (fn-name &key (expires 30) (size 100))
  (cond ((not (fboundp fn-name)) (error "~A is not a function" fn-name))
	(t (let ((wrapper (make-memo (symbol-function fn-name) :expires expires :size size)))
	     (setf (symbol-function fn-name) wrapper)
	     fn-name))))

(defmacro with-memoization (fns &body body)
  (mapc #'(lambda (fn)
	    (assert (fboundp fn)))
	fns)
  ;;take advantage of the fact that this is a lisp-2, and bind the closure to a var of the same name
  ;;as the original function
  `(let ,(mapcar #'(lambda (fn)
		     `(,fn (make-memo (quote ,fn) :expires 600 :size 1000)))
		 fns)
     (labels ,(mapcar #'(lambda (fn) ;;lexically over-ride the function-symbol with a memoizing version
			  `(,fn (&rest args)
				(apply ,fn args)))
		      fns)
       ,@body)))