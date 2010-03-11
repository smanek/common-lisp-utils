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

;;TODO
;;there might be a hash extension vulnerability here
;;would probably be safer to move to HMAC
;;I should look into that when I have some time
(defun get-signature (secret &rest rest)
  (hash-password (format nil "~A-~{~A~^-~}" secret rest)))