(in-package :postabon-helper)

(defun get-random-string (length &key (alphabetic nil) (numeric nil) (punctuation nil))
  (assert (or alphabetic numeric))
  (let ((alphabet nil))

    (when alphabetic
      (setf alphabet (append alphabet (concatenate 'list "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz"))))
    (when numeric
      (setf alphabet (append alphabet (concatenate 'list "0123456789"))))
    (when punctuation
      (setf alphabet (append alphabet (concatenate 'list "!?;:\".,()-"))))

    (setf alphabet (make-array (length alphabet) :element-type 'character :initial-contents alphabet))
    (loop for i from 1 upto length
       collecting (string (elt alphabet (random (length alphabet)))) into pass
       finally (return (apply #'concatenate 'string pass)))))

(defun get-random-guid (&optional (bits 256))
  (format nil "~36R" (random (expt 2 bits))))

(defun get-random-item (bag)
  (elt bag (random (length bag))))