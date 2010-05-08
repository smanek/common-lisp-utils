(in-package :postabon-helper)

(defvar *earth-radius* 6371010) ;;in meters

(defclass coordinate ()
  ((lat
    :initarg :lat
    :type single-float
    :reader get-lat)
   (long
    :initarg :long
    :type single-float
    :reader get-long)
   (address
    :initarg :address
    :type (or null string)
    :initform nil
    :reader get-address)))

(defmethod print-object ((c coordinate) stream)
  (print-unreadable-object (c stream :type t)
    (with-slots (lat long) c
      (format stream "[~A, ~A]" lat long))))

(defun deg-to-radian (a)
  (declare (type (or single-float fixnum) a))
  (the single-float (* a (/ (coerce pi 'single-float) 180))))

(defun make-coordinate (lat long &optional address)
  (declare (type (or single-float null) lat long)
	   (type (or string null) address))
  (when (and (not (null lat)) (not (null long)))
    (make-instance 'coordinate :lat lat :long long :address address)))


(defparameter *cambridge* (make-instance 'coordinate :lat 42.376804 :long -71.106788))
(defparameter *naperville* (make-instance 'coordinate :lat 41.735315 :long -88.139566))
(defparameter *nyc* (make-coordinate 40.71426891 -74.0059729))

;;computed the distance between two degrees of longitude at various latitudes and got
;;a chart like:
;;Latitude | Distance per 1 deg change in Long
;; 0       | 111.32 km
;; 15      | 107.55 km
;; 30      |  96.49 km
;; 45      |  78.85 km
;; 60      |  55.80 km
;; 75      |  28.90 km
;; 90      |   0.00 km
;;
;; Then, I did a least-squares regression analysis.
;; A linear regression yields pretty poor, results
;; But cubic gives:
;; y = 0.0000577366 x^3-0.0192043 x^2+0.0249291 x+111.332
;; which has an R^2 of 0.999944
;;The actual formula involves a lot of trig, and is too slow
(defun longitude-dist-at (lat)
  (+ (* 0.0577366 (expt lat 3))
     (* -19.2043 (expt lat 2))
     (* 24.9291 lat)
     111332))

(defmethod distance-approx ((s coordinate) (f coordinate))
  (cond ((null (get-lat s)) nil)
	((null (get-long s)) nil)
	((null (get-lat f)) nil)
	((null (get-long f)) nil)
	(t (let* ((lat-s (abs (get-lat s)))
		  (long-s (abs (get-long s)))
		  (lat-f (abs (get-lat f)))
		  (long-f (abs (get-long f)))
		  (mean-lat (/ (+ lat-f lat-s) 2))
		  (distance-lat (* 111000 (- lat-f lat-s))) ;;avg distance between 1 deg of lat is 111KM
		  (distance-long (* (longitude-dist-at mean-lat) (- long-f long-s))))
	     (* (sqrt (+ (expt distance-lat 2)
			 (expt distance-long 2))))))))

(defmethod distance ((s coordinate) (f coordinate))
;  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (cond ((null s) nil)
	((null f) nil)
	((null (get-lat s)) nil)
	((null (get-long s)) nil)
	((null (get-lat f)) nil)
	((null (get-long f)) nil)
	(t (let* ((phi-s (deg-to-radian (the single-float (get-lat s))))
		  (lambda-s (deg-to-radian (the single-float (get-long s))))
		  (phi-f (deg-to-radian (the single-float (get-lat f))))
		  (lambda-f (deg-to-radian (the single-float (get-long f))))
		  (delta-phi (the single-float (- phi-f phi-s)))
		  (delta-lambda (the single-float (- lambda-f lambda-s))))
	     (declare (type single-float phi-s lambda-s phi-f lambda-f delta-phi delta-lambda))
	     (* (the integer *earth-radius*) 
		(atan (/ (sqrt (+ (expt (* (cos phi-f) (sin delta-lambda)) 2)
				  (expt (- (* (cos phi-s) (sin phi-f))
					   (* (sin phi-s) (cos phi-f) (cos delta-lambda)))
					2)))
			 (+ (* (sin phi-s) (sin phi-f))
			    (* (cos phi-s) (cos phi-f) (cos delta-phi))))))))))

;;is a south of b?
(defmethod southp ((a coordinate) (b coordinate))
  (< (the single-float (get-lat a)) (the single-float (get-lat b))))

(defmethod northp ((a coordinate) (b coordinate))
  (not (southp a b)))

(defmethod eastp ((a coordinate) (b coordinate))
  (> (the single-float (get-long a)) (the single-float (get-long b))))

(defmethod westp ((a coordinate) (b coordinate))
  (not (eastp a b)))

;;note: disregards the 'address'
(defmethod coordinate= ((a coordinate) (b coordinate))
  (when (and (= (get-lat a) (get-lat b))
	     (= (get-long a) (get-long b)))
    t))

(defmethod near ((c coordinate) &optional (delta 0.1))
  (make-coordinate (+ (get-lat c)
		      (- (random delta) (/ delta 2)))
		   (+ (get-long c)
		      (- (random delta) (/ delta 2)))))