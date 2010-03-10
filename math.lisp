(in-package :postabon-helper)

(defun degrees-to-radians (deg)
  (* deg (/ pi 180)))

(defun naive-primep (num)
  (declare (type (and fixnum (integer 0)) num))
  (if (= num 1)
      nil
      (progn 
	(do ((i 2 (1+ i)))
	    ((> i (floor (sqrt num))))
	  (if (equal (mod num i) 0)
	      (return-from naive-primep nil)))
	t)))

(defun mod-exp (b e m)
  (declare (type (and fixnum (integer 0)) b e m))
  (let ((result 1))
    (while (> e 0)
      (when (= (logand e 1) 1)
	(setf result (mod (* result b) m)))
      (setf e (ash e -1))
      (setf b (mod (expt b 2) m)))
    result))

(defun fermat-primep (num &optional (k 30))
  (declare (type (and fixnum (integer 0)) num k))
  (if (or (< num 2) (not (typep num 'integer)))
      (return-from fermat-primep nil))
  (dotimes (i (- k 1))
    (let ((r (+ 1  (random (- num 1)))))
      (if (not (equal (mod-exp r (- num 1) num) 1) )
	  (return-from fermat-primep NIL))))
  't)

(defun prime-factorization (n)
  (declare (type (and fixnum (integer 0)) n))
  (let ((factors nil))
    (loop for i from 1 upto (isqrt n)
       do (when (= 0 (mod n i))
	    (when (fermat-primep i) (setf factors (cons i factors)))
	    (when (fermat-primep (/ n i)) (setf factors (cons (/ n i) factors))))
       finally (return factors))))

(defun permutations (bag)
  "Return a list of all the permutations of the input."
  (declare (type sequence bag))
  (if (null bag) 
      '(())
      (mapcan #'(lambda (e)
		  (mapcar #'(lambda (p) (cons e p))
			  (permutations
			   (remove e bag :count 1))))
	      bag)))

(defun meters-to-miles (m)
  (* m 0.000621371192))

