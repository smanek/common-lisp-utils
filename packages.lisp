(in-package :cl-user)

(defpackage "POSTABON-HELPER"
  (:nicknames "helper" "misc")
  (:use :cl :asdf)
  (:export    
	   :day-of-week ;;date
	   :days-in-month
	   :smart-date
	   :pretty-print-utime
	   :pretty-print-date
	   :utime-to-date
	   :get-current-year
	   :utime-yesterday
	   :utime-last-week

	   :curry  ;;functional
	   :compose 
	   :pmapcar
	   
	   :octet-array-string ;;crypto
	   :md5sum-file
	   :md5sum-string
	   :hash-password
	   :get-signature
	   
	   :memoize-fn ;;memoize
	   :with-memoization

	   :path-equal ;;file
	   :slurp
	   
	   :get-system-info ;;system
	   :run-at-shell

	   :get-current-commit ;;git

	   :split-on-delimiter ;;string
	   :trim
	   :parse-float
	   :nonempty-boolean
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
	   :title-to-url

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
	   :write-csv-recursive
	   :read-csv-stream
	   :parse-comma-delimited-list
	   :comma-delimited-list-integers
	   :comma-delimited-list-floats
	   :comma-delimited-list-keywords
	   
	   :alist-to-plist ;;misc.lisp
	   :alist-to-hash
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
	   :subclass-of
	   :multi-mapcar
	   :all-unique-p))