(in-package :cl-user)

(defpackage "POSTABON-HELPER"
  (:nicknames "helper" "misc")
  (:use :cl :asdf)
  (:export :day-of-week ;;date
	   :days-in-month
	   :smart-date
	   :pretty-print-utime
	   :pretty-print-date
	   :get-current-year
	   :utime-yesterday
	   :utime-last-week

	   :octet-array-string ;;crypto
	   :md5sum-file
	   :md5sum-string
	   :hash-password
	   :get-signature
	   
	   :path-equal ;;file
	   :slurp

	   :get-current-commit ;;git

	   :split-on-delimiter ;;string
	   :trim
	   :parse-float
	   :radix-values
	   :smart-distance
	   :radix-values
	   :smart-distance
	   :join-strings
	   :split-into-words
	   :ordinal
	   :make-keyword
	   :nonempty-string
	   :unescape-for-html
	   :nonempty-float
	   :truncate-string
	   

	   :degrees-to-radians ;;from math.lisp
	   :naive-primep
	   :mod-exp
	   :fermat-primep
	   :prime-factorization
	   :permutations
	   :meters-to-miles
	   
	   :get-random-string ;;random.lisp
	   :get-random-guid
	   :get-random-item
	   :get-random-english

	   :alist-as-csv ;;csv.lisp
	   :write-csv
	   :read-csv-stream
	   :parse-comma-delimited-list
	   :comma-delimited-list-integers
	   :comma-delimited-list-floats
	   :comma-delimited-list-keywords
	   
	   :alist-to-plist ;;misc.lisp
	   :class-slot-names
	   :object-to-hash
	   :print-hash-table
	   :while
	   :squash
	   :palinp
	   :range
	   :safe-subseq
	   :always-true
	   :make-numeric-test
	   :get-subclasses
	   :subclass-of))