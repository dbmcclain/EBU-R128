
(in-package :ebu-r128)

;; ------------------------------------------------

(defun file-advance (f relpos)
  (file-position f (+ (file-position f) relpos)))

;; ------------------------------------------------

(defun extract-ule (vec start ixstart)
  (declare (optimize (speed 3)
                     (safety 0)
                     (float 0)))
  (declare (type fixnum start ixstart))
  (do ((val 0)
       (ix ixstart (1- ix)))
      ((minusp ix) val)
    (declare (type fixnum val ix))
    (setf val (logior (ash val 8)
                      (sys:typed-aref '(unsigned-byte 8) vec (+ start ix))
                      ))))
    
(defun extract-ube (vec start ixstop)
  (declare (optimize (speed 3)
                     (safety 0)
                     (float 0)))
  (declare (type fixnum start ixstart))
  (do ((val  0)
       (ix   0  (1+ ix)))
      ((>= ix ixstop) val)
    (declare (type fixnum ix)
             (type integer val))
    ;; watch out -- sometimes we extract 80-bit values here
    ;; so val is not always a fixnum
    (setf val (logior (ash val 8)
                      (sys:typed-aref '(unsigned-byte 8) vec (+ start ix))
                      ))))

(defun signing-16 (val)
  (declare (optimize (speed 3)
                     (safety 0)
                     (float 0)))
  (declare (type fixnum val))
  (if (logbitp 15 val)
      (- val #.(ash 1 16))
    val))

(defun signing-24 (val)
  (declare (optimize (speed 3)
                     (safety 0)
                     (float 0)))
  (declare (type fixnum val))
  (if (logbitp 23 val)
      (- val #.(ash 1 24))
    val))

(defun convert-u32-to-flt32 (val)
  (declare (optimize (speed 3)
                     (safety 0)
                     (float 0)))
  (declare (type fixnum val))
  (let* ((exp  (- (ldb (byte 8 23) val) 127 23))
         (mant (dpb 1 (byte 1 23) (ldb (byte 23 0) val)))
         (fval (scale-float (sfloat mant) exp)))
    (declare (type fixnum sgn exp mant)
             (type single-float fval))
    (if (logbitp 31 val)
        (- fval)
      fval)))

(defun convert-u80-to-flt80 (val)
  (declare (optimize (speed 3)
                     (safety 0)
                     (float 0)))
  (declare (type integer val))
  (let* ((exp  (- (ldb (byte 15 64) val) #x4000 62))
         (mant (ldb (byte 64 0) val))
         (fval (scale-float (float mant 1d0) exp)))
    (declare (type fixnum sgn exp)
             (type integer mant)
             (type double-float fval))
    (if (logbitp 79 val)
        (- fval)
      fval)))

;; ------------------------------------------------

(defun extract-u16le (vec &optional (start 0))
  (declare (optimize (speed 3)
                     (safety 0)
                     (float 0)))
  (declare (type fixnum start))
  (sys:typed-aref '(unsigned-byte 16) vec start))

(defun extract-u16be (vec &optional (start 0))
  (extract-ube vec start 2))

(defun extract-u24le (vec &optional (start 0))
  (declare (optimize (speed 3)
                     (safety 0)
                     (float 0)))
  (declare (type fixnum start))
  (dpb (sys:typed-aref '(unsigned-byte 8) vec (+ start 2))
       (byte 8 16)
       (sys:typed-aref '(unsigned-byte 16) vec start)))

(defun extract-u24be (vec &optional (start 0))
  (extract-ube vec start 3))

(defun extract-u32le (vec &optional (start 0))
  (declare (optimize (speed 3)
                     (safety 0)
                     (float 0)))
  (declare (type fixnum start))
  (sys:typed-aref '(unsigned-byte 32) vec start))

(defun extract-u32be (vec &optional (start 0))
  (extract-ube vec start 4))

;; -----------------------------------------------

(defun extract-s16le (vec &optional (start 0))
  (declare (optimize (speed 3)
                     (safety 0)
                     (float 0)))
  (declare (type fixnum start))
  (sys:typed-aref '(signed-byte 16) vec start))

(defun extract-s16be (vec &optional (start 0))
  (signing-16 (extract-u16be vec start)))

(defun extract-s24le (vec &optional (start 0))
  (signing-24 (extract-u24le vec start)))

(defun extract-s24be (vec &optional (start 0))
  (signing-24 (extract-u24be vec start)))

(defun extract-flt32le (vec &optional (start 0))
  (declare (optimize (speed 3)
                     (safety 0)
                     (float 0)))
  (declare (type fixnum start))
  (sys:typed-aref 'single-float vec start))

(defun extract-flt32be (vec &optional (start 0))
  (convert-u32-to-flt32 (extract-u32be vec start)))

(defun extract-u80be (vec &optional (start 0))
  (extract-ube vec start 10))

(defun extract-flt80be (vec &optional (start 0))
  (convert-u80-to-flt80 (extract-u80be vec start)))

;; ------------------------------------------------

(defvar *scratch-4*
  (sys:make-typed-aref-vector 4))

(defun read-bytes (f v nel)
  (declare (optimize (speed 3)
                     (safety 0)
                     (float 0)))
  (declare (type fixnum nel))
  (dotimes (ix nel)
    (setf (sys:typed-aref '(unsigned-byte 8) v ix) (read-byte f))))

(defun read-scratch (f nel)
  (read-bytes f *scratch-4* nel)
  *scratch-4*)

(defun read-16 (f)
  (read-scratch f 2))

(defun read-24 (f)
  (read-scratch f 3))

(defun read-32 (f)
  (read-scratch f 4))

;; ------------------------------------------------

(defun read-u32le (f)
  (extract-u32le (read-32 f)))

(defun read-u32be (f)
  (extract-u32be (read-32 f)))

(defun read-flt32le (f)
  (convert-u32-to-flt32 (read-u32le f)))

(defun read-u16le (f)
  (extract-u16le (read-16 f)))

(defun read-u16be (f)
  (extract-u16be (read-16 f)))

(defun read-u24le (f)
  (extract-u24le (read-24 f)))

(defun read-u24be (f)
  (extract-u24be (read-24 f)))

;; ------------------------------------------------

(defun read-u32 (f endian)
  (case endian
    (:le (read-u32le f))
    (:be (read-u32be f))))

(defun read-u16 (f endian)
  (case endian
    (:le (read-u16le f))
    (:be (read-u16be f))))

;; ------------------------------------------------

(defun find-chunk (f chunk-magic &key (endian :le))
  (let* ((magic (make-string 4)))
    (read-sequence magic f)
    (unless (string= magic chunk-magic)
      (let ((offs (read-u32 f endian)))
        (file-advance f offs)
        (find-chunk f chunk-magic :endian endian)))
    ))

(defun read-audio-format (f &optional (endian :le))
  (let* ((dlen (read-u32 f endian))
         (fmt  (sys:make-typed-aref-vector dlen)))
    (read-bytes f fmt dlen)
    fmt))

(defstruct wave-file
  f fname fsamp nsamp bits-per-sample nchan endian)

(defun get-wav-info (f fname)
  (let* ((magic (make-string 4)))
    (file-advance f 4)
    (read-sequence magic f)
    (assert (string= magic "WAVE"))
    (find-chunk f "fmt ")
    (let* ((fmt   (read-audio-format f))
           (nchan (extract-u16le fmt 2))
           (fsamp (extract-u32le fmt 4))
           (bits-per-sample (extract-u16le fmt 14)))
      (find-chunk f "data")
      (let* ((nb    (read-u32le f))
             (nsamp (truncate (* 8 nb) (* nchan bits-per-sample))))
        (make-wave-file :f      f
                        :fname  fname
                        :fsamp  fsamp
                        :nsamp  nsamp
                        :bits-per-sample bits-per-sample
                        :nchan  nchan
                        :endian :le)))))

(defun get-aif-info (f fname)
  (let* ((magic (make-string 4)))
    (file-advance f 4)
    (read-sequence magic f)
    (let ((endian (cond ((string= magic "AIFF") :be)
                        ((string= magic "AIFC") :le))))
      (find-chunk f "COMM" :endian :be)
      (let* ((fmt   (read-audio-format f :be))
             (nchan (extract-u16be fmt 0))
             (nsamp (extract-u32be fmt 2))
             (bits-per-sample (extract-u16be fmt 6))
             (fsamp (extract-flt80be fmt 8)))
        (find-chunk f "SSND" :endian :be)
        (file-advance f 12)
        (make-wave-file :f f
                        :fname fname
                        :fsamp fsamp
                        :nsamp nsamp
                        :bits-per-sample bits-per-sample
                        :nchan nchan
                        :endian endian)))))

(defun get-audio-info (f fname)
  (let* ((magic (make-string 4)))
    (read-sequence magic f)
    (cond ((string= magic "RIFF") ;; .WAV format
           (get-wav-info f fname))
          
          ((string= magic "FORM") ;; .AIF format
           (get-aif-info f fname))
          )))

(defun do-with-open-wav-file (filename fn)
  (with-remembered-filename (fname :com.sd.wav.last-wave-file)
      (or filename
          (capi:prompt-for-file "Select input file"
                                :filter "*.wav;*.aif;*.aiff"
                                :pathname fname))
    (with-open-file (f fname
                       :direction :input
                       :element-type '(unsigned-byte 8))
      (funcall fn (get-audio-info f fname))
      )))

(defmacro with-wav-file ((wf &optional filename) &body body)
  `(do-with-open-wav-file ,filename
                          (lambda (,wf)
                            ,@body)))

(defmethod make-wave-data-getter ((wf wave-file) nsamp &key dst)
  (declare (optimize (speed 3)
                     (safety 0)
                     (float 0)))
  (with-accessors ((bps    wave-file-bits-per-sample)
                   (nchan  wave-file-nchan)
                   (endian wave-file-endian)
                   (f      wave-file-f)) wf
    (assert (<= 1 nchan 2))
    (let* ((nbs     (truncate bps 8))
           (nsraw   (* nsamp nchan))
           (nbraw   (* nbs nsraw))
           (rawdata (sys:make-typed-aref-vector nbraw))
           (data    (or dst
                        (make-array (* nsamp (max 2 nchan))
                                    :element-type 'single-float)))
           (extractor (ecase bps
                        (16  (case endian
                               (:le (lambda (vec pos)
                                      (scale-float (sfloat (extract-s16le vec pos)) -15)))
                               (:be (lambda (vec pos)
                                      (scale-float (sfloat (extract-s16be vec pos)) -15)))
                               ))
                        (24  (case endian
                               (:le (lambda (vec pos)
                                      (scale-float (sfloat (extract-s24le vec pos)) -23)))
                               (:be (lambda (vec pos)
                                      (scale-float (sfloat (extract-s24be vec pos)) -23)))
                               ))
                        (32  (case endian
                               (:le  'extract-flt32le)
                               (:be  'extract-flt32be)
                               ))
                        )))
      (lambda ()
        (read-bytes f rawdata nbraw)
        (when (= nchan 1)
          (fill data 0e0))
        (loop repeat nsraw
              for ix from 0 by nbs
              for jx from 0 by (if (= nchan 1) 2 1)
              do
              (setf (aref data jx) (funcall extractor rawdata ix)))
        data))))
     
(defmethod get-wave-data ((wf wave-file) nsamp &key dst)
  (funcall (make-wave-data-getter wf nsamp :dst dst)))

#|
(with-wav-file (wf)
  (inspect wf))

(with-wav-file (wf)
  (let* ((data (get-wave-data wf 1024))
         (ldata (loop for ix from 0 below 2048 by 2 collect
                      (aref data ix)))
         (rdata (loop for ix from 1 below 2048 by 2 collect
                      (aref data ix))))
    (plt:plot 'plt ldata :clear t)
    (plt:plot 'plt rdata :color :red)))
  
|#

