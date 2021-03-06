(in-package :postabon-helper)

(defun octet-array-string (arr)
  (format nil "~(~{~2,'0X~}~)" (map 'list #'identity arr)))

(defun md5sum-file (path)
  (ironclad:byte-array-to-hex-string
   (ironclad::digest-file :md5 path)))

(defun md5sum-string (str)
  (ironclad:byte-array-to-hex-string
   (ironclad::digest-sequence :md5 (SB-EXT:STRING-TO-OCTETS str))))

(defun hash-password (str)
  (declare (type string str))
  (ironclad:byte-array-to-hex-string
   (ironclad::digest-sequence :sha256 (SB-EXT:STRING-TO-OCTETS str))))

(defun get-signature (secret &rest rest)
  (let ((hmac (ironclad:make-hmac (ironclad:ascii-string-to-byte-array secret) :sha256)))
    (loop for data in rest
	 do (ironclad:update-hmac hmac (ironclad:ascii-string-to-byte-array data))
	 finally (return (ironclad:byte-array-to-hex-string (ironclad:hmac-digest hmac))))))