
(in-package :ebu-r128)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defparameter *version* "")
  (defun c-name (str)
    (concatenate 'string str *version*)))
  
(fli:disconnect-module :hsiirlib :remove t)
(fli:register-module :hsiirlib
                     :connection-style :immediate
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

