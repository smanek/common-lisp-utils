(in-package :postabon-helper)

(defun octet-array-string (arr)
  (format nil "~(~{~2,'0X~}~)" (map 'list #'identity arr)))

(defun md5sum-file (path)
  (octet-array-string (md5:md5sum-file path)))

(defun md5sum-string (str)
  (octet-array-string (md5:md5sum-sequence str)))

(defun hash-password (str)
  (declare (type string str))
  (ironclad:byte-array-to-hex-string
   (ironclad::digest-sequence :sha256 (SB-EXT:STRING-TO-OCTETS str))))