(in-package :cl-user)

(defpackage :postabon-helper-asd
  (:use :cl :asdf))

(in-package :postabon-helper-asd)

(defsystem :postabon-helper
  :depends-on (:cl-ppcre :ironclad)
  :version "0.01"
  :components
  ((:file "packages")
   (:file "date"
	  :depends-on ("packages"))
   (:file "crypto"
	  :depends-on ("packages"))
   (:file "file"
	  :depends-on ("packages"))
   (:file "string"
	  :depends-on ("packages"))
   (:file "math"
	  :depends-on ("packages" "misc"))
   (:file "random"
	  :depends-on ("packages"))
   (:file "csv"
	  :depends-on ("packages" "string"))
   (:file "misc"
	  :depends-on ("packages"))))