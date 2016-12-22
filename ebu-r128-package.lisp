
(in-package #:CL-USER)

(defpackage :ebu-r128
  (:use #:common-lisp)
  (:export
   #:r128-rating
   #:r128-ratings
   #:r128-album-rating
   ))

#|
;; generate HTML documentation
(in-package :ebu-r128)
(doctools:gen-docs
 :asdf-system-name :ebu-r128
 :package-name     :ebu-r128
 :directory        (translate-logical-pathname "PROJECTS:LISP;")
 :subtitle         "a batck EBU-R128 tool for audio measurement")
|#
