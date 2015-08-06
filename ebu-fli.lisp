
(in-package :ebu-r128)

(fli:disconnect-module :hsiirlib :remove t)
(fli:register-module :hsiirlib
                     ;; :connection-style :immediate
                     :dlopen-flags t ;; non-nil needed for Mac to unload dylib on disconnect-module
                     :real-name
                     (merge-pathnames
                      #+:MAC   "libHsIIR.dylib"
                      #+:WIN32 "libHsIIR.dll"
                      (translate-logical-pathname "PROJECTS:DYLIB;xxx")))

(fli:define-foreign-function (_hsiir_init "hsiir_init" :source)
    ((coffs  (:pointer :double)))
  :result-type :void
  :language :ansi-c
  :module :hsiirlib)

(fli:define-foreign-function (_hsiir_eval "hsiir_eval_blk" :source)
    ((buf   (:pointer :float))
     (nsamp :long)
     (ans   (:pointer :float)))
  :result-type :void
  :language :ansi-c
  :module :hsiirlib)

(fli:define-foreign-function (_hsiir_test "hsiir_test" :source)
    ()
  :result-type :long
  :language :ansi-c
  :module :hsiirlib)
