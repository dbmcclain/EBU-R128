
(in-package :ebu-r128)

#||#
(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *version* "")
  (defun c-name (str)
    (concatenate 'string str *version*)))
  
(fli:disconnect-module :hsiirlib :remove t)
(fli:register-module :hsiirlib
                     ;; :connection-style :immediate
                     :dlopen-flags t ;; non-nil needed for Mac to unload dylib on disconnect-module
                     :real-name
                     (merge-pathnames
                      (concatenate 'string "libHsIIR" *version*
                                   #+:MAC   ".dylib"
                                   #+:WIN32 ".dll")
                      (translate-logical-pathname "PROJECTS:DYLIB;xxx")))

(fli:define-foreign-function (_hsiir_init #$(c-name "hsiir_init") :source)
    ((coffs  (:pointer :double)))
  :result-type :void
  :language :ansi-c
  :module :hsiirlib)

(fli:define-foreign-function (_hsiir_eval #$(c-name "hsiir_eval_blk") :source)
    ((buf   (:pointer :float))
     (nsamp :long)
     (ans   (:pointer :float)))
  :result-type :void
  :language :ansi-c
  :module :hsiirlib)

(fli:define-foreign-function (_hsiir_test #$(c-name "hsiir_test") :source)
    ()
  :result-type :long
  :language :ansi-c
  :module :hsiirlib)
#||#

#|
(xfli:disconnect-module :hsiirlib)
(xfli:register-module :hsiirlib
                      :real-name
                      (merge-pathnames
                       (concatenate 'string "libHsIIR"
                                    #+:MAC   ".dylib"
                                    #+:WIN32 ".dll")
                       (translate-logical-pathname "PROJECTS:DYLIB;xxx")))


(xfli:define-foreign-function (_hsiir_init "hsiir_init")
    ((coffs  (:pointer :double)))
  :result-type :void
  :language :ansi-c
  :module :hsiirlib)

(xfli:define-foreign-function (_hsiir_eval "hsiir_eval_blk")
    ((buf   (:pointer :float))
     (nsamp :long)
     (ans   (:pointer :float)))
  :result-type :void
  :language :ansi-c
  :module :hsiirlib)

(xfli:define-foreign-function (_hsiir_test "hsiir_test")
    ()
  :result-type :long
  :language :ansi-c
  :module :hsiirlib)
    
|#
