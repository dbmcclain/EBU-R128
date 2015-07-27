
(asdf:defsystem "ebu-r128"
  :description "ebu-r128: a batch EBU-R128 audio measurement tool"
  :version     "1.0"
  :author      "D.McClain <dbm@spectrodynamics.com>"
  :license     "Copyright (c) 2015 by SpectroDynamics, LLC. All rights reserved."
  :components  ((:file "ebu-r128-package")
                (:file "ebu-r128-remembered-filenames")
                (:file "ebu-r128-vmath")
                (:file "ebu-r128"))
  :serial t
  :depends-on  ())
