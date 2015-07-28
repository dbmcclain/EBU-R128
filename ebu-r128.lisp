
(in-package :ebu-r128)

(defun rcurry (fn &rest suf-args)
  (lambda (&rest pref-args)
    (apply fn (nconc pref-args suf-args))))

(defun safe-log10 (x)
  (log (max 1e-6 (abs x)) 10))

(defun db10 (x)
  (* 10 (safe-log10 x)))

(defun db20 (x)
  (* 20 (safe-log10 x)))

;; ---------------------------------------------------
;; ITU K filters for loudness measurement
(defvar *f1*
  ;; a1 a2 b0 b1 b2  for 48 kHz Fsamp
  ;;
  ;; H(z) = (b0 + b1/z + b2/z^2) / (1 - a1/z - a2/z^2)
  ;;
  (destructuring-bind (a1 a2 b0 b1 b2)
      '(1.69065929318241d0
        -0.73248077421585d0
        1.53512485958697d0
        -2.69169618940638d0
        1.19839281085285d0)
    (list b0 b1 b2 a1 a2)))

(defvar *f2*
  (destructuring-bind (a1 a2 b0 b1 b2)
      '(1.99004745483398d0
        -0.99007225036621d0
        1d0
        -2d0
        1d0)
    (list b0 b1 b2 a1 a2)))
    

(defun iir (f filt)
  (destructuring-bind (b0 b1 b2 a1 a2) filt
    (let* ((1/z (cis (* -2 pi f))))
      (/ (+ b0
            (* b1 1/z)
            (* b2 1/z 1/z))
         (- 1
            (* a1 1/z)
            (* a2 1/z 1/z))) )))

(defun db-filt (f filt fs)
  (db20 (iir (/ f fs) filt)))

(defun dbk-itu (f)
  (+ (db-filt f *f1* 48)
     (db-filt f *f2* 48)))

(defun hishelf (g f0 q)
  (let* ((a  (expt 10 (/ g 40)))
         (w0 (* 2 pi f0))
         (cw0 (cos w0))
         (alpha (/ (sin w0) (* 2 q)))
         (a0 (- (+ a 1)
                (* (- a 1) cw0)
                (* -2 (sqrt a) alpha)))
         (a1 (* 2 (- (- a 1)
                     (* (+ a 1) cw0))))
         (a2 (- (+ a 1)
                (* (- a 1) cw0)
                (* 2 (sqrt a) alpha)))
         (b0 (* a (+ (+ a 1)
                     (* (- a 1) cw0)
                     (* 2 (sqrt a) alpha))))
         (b1 (* -2 a (+ (- a 1)
                        (* (+ a 1) cw0))))
         (b2 (* a (+ (+ a 1)
                     (* (- a 1) cw0)
                     (* -2 (sqrt a) alpha)))))
    (list (/ b0 a0)
          (/ b1 a0)
          (/ b2 a0)
          (- (/ a1 a0))
          (- (/ a2 a0)))
    ))
          

(defun hpf (f0 q)
  (let* ((w0    (* 2 pi f0))
         (cw0   (cos w0))
         (alpha (/ (sin w0) (* 2 q)))
         (a0    (+ 1 alpha))
         (a1    (* -2 cw0))
         (a2    (- 1 alpha))
         (b0    (/ (+ 1 cw0) 2))
         (b1    (- (+ 1 cw0)))
         (b2    (/ (+ 1 cw0) 2)))
    (list (/ b0 a0)
          (/ b1 a0)
          (/ b2 a0)
          (- (/ a1 a0))
          (- (/ a2 a0)))
    ))
          
#|
(progn
  (plt:fplot 'filt '(0.01 20) (rcurry 'db-filt *f1* 48)
             :clear t
             :xlog t
             :title  "High Shelf Filter"
             :xtitle "Frequency [kHz]"
             :ytitle "Amplitude [dB]"
             :legend "ITU Recommended"
             )
  (plt:fplot 'filt '(0.01 20) (rcurry 'db-filt (hishelf 4 (/ 1.5 48) 0.707) 48)
             :color :red
             :legend "Garden variety High Shelf"
             )
  (plt:draw-text 'filt
                 "Fc = 1500 Hz, Q = 0.707, Gain = 4 dB"
                 `(0.02 3.5))
  )

(progn
  (plt:fplot 'filt '(0.01 20) (rcurry 'db-filt *f2* 48)
             :clear t
             :xlog t
             :title  "Highpass Filter"
             :xtitle "Frequency [kHz]"
             :ytitle "Amplitude [dB]"
             :legend "ITU Recommended"
             )
  (plt:fplot 'filt '(0.01 20) (rcurry 'db-filt (hpf (/ 0.0375 48) 0.5) 48)
             :color :red
             :legend "Garden variety 2-pole HPF"
             )
  (plt:draw-text 'filt
                 "Fc = 37.5 Hz, Q = 0.5"
                 `(0.1 -15))
  )

(progn
  (plt:fplot 'filt '(0.01 20) (lambda (f)
                                (+ (db-filt f *f2* 48)
                                   (db-filt f *f1* 48)))
             :clear t
             :xlog t
             :title  "Highpass Filter"
             :xtitle "Frequency [kHz]"
             :ytitle "Amplitude [dB]"
             :legend "ITU Recommended"
             )
  (let* ((hpf  (hpf       (/ 0.0375 48) 0.5))
         (hsh  (hishelf 4 (/ 1.5 48) 0.707)))
    (plt:fplot 'filt '(0.01 20) (lambda (f)
                                  (+ (db-filt f hpf 48)
                                     (db-filt f hsh 48)))
               :color :red
               :legend "Garden variety 2-pole HPF"
               )
    (plt:draw-text 'filt
                   "Fc = 37.5 Hz, Q = 0.5"
                   `(0.1 -15))
    ))
|#           
;; ------------------------------------------------------------------
;; Let's examine the TruePeak (TP) 4x upsampling filter...
;; FIR Taps for 48 kHz Fsamp
;; Report in units of dB TP when upsample rate >= 192 kHz

;; order 48, 4-phase, FIR interpolating filter
;; DC gain is 12 dB to make up for insertion of zeros

(defvar *ph0*
  (make-array 12
              :element-type 'single-float
              :initial-contents
              (mapcar (um:rcurry 'float 1e0)
                      (reverse
                       '(0.0017089843750d0
                        0.0109863281250d0
                        -0.0196533203125d0
                        0.0332031250000d0
                        -0.0594482421875d0
                        0.1373291015625d0
                        0.9721679687500d0
                        -0.1022949218750d0
                        0.0476074218750d0
                        -0.0266113281250d0
                        0.0148925781250
                        -0.0083007812500d0)))))

(defvar *ph1*
  (make-array 12
              :element-type 'single-float
              :initial-contents
              (mapcar (um:rcurry 'float 1e0)
                      (reverse
                       '(-0.0291748046875d0
                         0.0292968750000d0
                         -0.0517578125000d0
                         0.0891113281250d0
                         -0.1665039062500d0
                         0.4650878906250d0
                         0.7797851562500d0
                         -0.2003173828125d0
                         0.1015625000000d0
                         -0.0582275390625d0
                         0.0330810546875d0
                         -0.0189208984375d0)))))
              
(defvar *ph2*
  (make-array 12
              :element-type 'single-float
              :initial-contents  
              (mapcar (um:rcurry 'float 1e0)
                      (reverse
                       '(-0.0189208984375d0
                         0.0330810546875d0
                         -0.0582275390625d0
                         0.1015625000000d0
                         -0.2003173828125d0
                         0.7797851562500d0
                         0.4650878906250d0
                         -0.1665039062500d0
                         0.0891113281250d0
                         -0.0517578125000d0
                         0.0292968750000d0
                         -0.0291748046875d0)))))

(defvar *ph3*
  (make-array 12
              :element-type 'single-float
              :initial-contents
              (mapcar (um:rcurry 'float 1e0)
                      (reverse
                       '(-0.0083007812500d0
                         0.0148925781250d0
                         -0.0266113281250d0
                         0.0476074218750d0
                         -0.1022949218750d0
                         0.9721679687500d0
                         0.1373291015625d0
                         -0.0594482421875d0
                         0.0332031250000d0
                         -0.0196533203125d0
                         0.0109863281250d0
                         0.0017089843750d0)))))

#|
(let* ((x  (loop for x0 across (reverse *ph0*)
                 for x1 across (reverse *ph1*)
                 for x2 across (reverse *ph2*)
                 for x3 across (reverse *ph3*)
                 collect x0
                 collect x1
                 collect x2
                 collect x3))
       (fs   (map 'vector (rcurry '* 192/32) (iramp 32)))
       (fdb  (fft:fwd-magnitude-db (coerce x 'vector))))
  (plt:plot 'plt x :clear t)
  (plt:plot 'fplt fs fdb :clear t))
|#

(defparameter *bufl*
  (make-array 24
              :element-type 'single-float
              :initial-element 0e0))
(defparameter *bufr*
  (make-array 24
              :element-type 'single-float
              :initial-element 0e0))
(defparameter *bix* 0)
(defparameter *phs*
  (list *ph3*
        *ph2*
        *ph1*
        *ph0*))

(defun fir-init ()
  (fill *bufl* 0e0)
  (fill *bufr* 0e0)
  (setf *bix*  0))

(defun fir-ph-filt (coffs &optional (buf *bufl*))
  (declare (optimize (speed 3)
                     (safety 0)
                     (float 0)))
  (declare (type (array single-float (*)) coffs)
           (type (array single-float (*)) buf))
  (loop for c across coffs
        for jx fixnum from *bix*
        sum (* (aref buf jx) c)))

(defun store-fir-sample (x buf)
  (declare (optimize (speed 3)
                     (safety 0)
                     (float 0)))
  (declare (type (array single-float (*)) buf)
           (type single-float x))
  (decf *bix*)
  (if (minusp *bix*)
      (setf *bix* 11))
  (setf (aref buf *bix*) x
        (aref buf (+ *bix* 12)) x))

(defun fir-filt-samp (x &optional (buf *bufl*))
  (declare (optimize (speed 3)
                     (safety 0)
                     (float 0)))
  (declare (type (array single-float (*)) buf)
           (type single-float x))
  (store-fir-sample x buf)
  (loop for ph in *phs*
        collect (fir-ph-filt ph buf)))

(defun fir-maxabs-samp (x &optional (buf *bufl*))
  (declare (optimize (speed 3)
                     (safety 0)
                     (float 0)))
  (declare (type (array single-float (*)) buf)
           (type single-float x))
  (store-fir-sample x buf)
  (loop for ph in *phs*
        maximize (abs (fir-ph-filt ph buf))))

(defun fir-filt-list (lst)
  (let ((ans (make-array (* 4 (length lst))
                         :initial-element 0)))
    (loop for x in lst
          for ix from 0 by 4
          do
          (replace ans (fir-filt-samp x) :start1 ix))
    ans))

(defun fir-filt-vec (v)
  (let ((ans (make-array (* 4 (length v))
                         :initial-element 0)))
    (loop for x across v
          for ix from 0 by 4
          do
          (replace ans (fir-filt-samp x) :start1 ix))
    ans))

(defun max-abs (a b)
  (max (abs a) (abs b)))
  
(defmethod true-peak ((v vector))
  (db20
   (loop for x across v maximize
         (fir-maxabs-samp x))))

(defmethod true-peak ((v list))
  (db20
   (loop for x in v maximize
         (fir-maxabs-samp x))))
          
#|
(let* ((x  (mapcar (um:rcurry 'float 1e0)
                   (append '(-1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
                                1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
                           (make-list 64 :initial-element -1))))
       (xmax (true-peak x))
       (y    (progn
               (fir-init)
               (fir-filt-list x))))
  (plt:plot 'yplt y :clear t)
  (list xmax
        (db20 (reduce 'max y))
        (db20 (reduce 'min y))))

(let* ((x  (vops:voffset -1 (vm:unoise 64 2.0)))
       (xmax (true-peak x))
       (y  (fir-filt-vec x)))
  (plt:plot 'yplt y :clear t)
  (list xmax
        (db20 (reduce 'max y))
        (db20 (reduce 'min y))))
|#

#|
(labels ((win (ix)
           (if (<= 0 ix 1023)
               (let ((v (sin (* pi 1/1024 (+ ix 1/2)))))
                 (* 1/2 (sin (* pi 1/2 v v))))
             0.0)))
  (plt:fplot 'win '(0 1024) #'win
             :yrange '(0 1.1)
             :clear t)
  (plt:fplot 'win '(0 1024) (lambda (ix)
                              (win (+ ix 512)))
             :color :red)
  (plt:fplot 'win '(0 1024) (lambda (ix)
                              (loop for jx from -4096 to 4096 by 128 sum
                                    (let ((v (win (+ ix jx))))
                                      (* v v))))
             :color :blue))
                                       
|#

(defvar *itu-filter*)
(defvar *itu-db-corr*)

(defvar *itu-stateL*
  (make-array 6
              :element-type 'single-float
              :initial-element 0e0))

(defvar *itu-stateR*
  (make-array 6
              :element-type 'single-float
              :initial-element 0e0))

(defun init-itu-filter (fs)
  (let* ((hpf  (hpf       (/ 0.0375 fs) 0.5))
         (hsh  (hishelf 4 (/ 1.5 fs)    0.707))
         (coffs (make-array 10
                            :element-type     'single-float
                            :initial-contents
                            (mapcar (um:rcurry 'float 1e0)
                                    (append hpf hsh)))))
    (setf *itu-filter* coffs
          *itu-db-corr* (- (+ (db-filt 1 hsh fs)
                              (db-filt 1 hpf fs))))
    (fill *itu-stateL* 0e0)
    (fill *itu-stateR* 0e0)
    (fir-init)))
  
(init-itu-filter 48)

(defun itu-filt1 (state v)
  (declare (optimize (speed 3)
                     (safety 0)
                     (float 0)))
  (declare (type (array single-float (6)) state)
           (type single-float v))
  (let ((coeffs *itu-filter*))
    (declare (type (array single-float (10)) coeffs))
    (labels ((filter1 (x six cix)
               (declare (type fixnum six cix)
                        (type single-float x))
               (let ((y (+ (* x                      (aref coeffs (+ cix 0)))
                           (* (aref state (+ six 0)) (aref coeffs (+ cix 1)))
                           (* (aref state (+ six 1)) (aref coeffs (+ cix 2)))
                           (* (aref state (+ six 2)) (aref coeffs (+ cix 3)))
                           (* (aref state (+ six 3)) (aref coeffs (+ cix 4))))))
                 (shiftf (aref state (+ six 1)) (aref state (+ six 0)) x)
                 y)))
      (let* ((y (filter1 v 0 0))
             (z (filter1 y 2 5)))
        (declare (type single-float y z))
        (shiftf (aref state 5) (aref state 4) z)
        z))))

(defstruct itu-filt-result
  tpl rss)

(defun itu-filt (buf ans)
  (declare (optimize (speed 3)
                     (safety 0)
                     (float 0)))
  (declare (type (array single-float (*)) buf))
  (let* ((nel (length buf))
         (sf  (/ 2.0 nel)))
    (declare (type fixnum nel)
             (type single-float sf))
    (setf (itu-filt-result-tpl ans)
          (float
           (max (loop for ix fixnum from 0 below nel by 2 maximize
                      (fir-maxabs-samp (aref buf ix) *bufl*))
                (loop for ix fixnum from 1 below nel by 2 maximize
                      (fir-maxabs-samp (aref buf ix) *bufr*)))
           1e0)
          
          (itu-filt-result-rss ans)
          (* sf
             (loop for ix fixnum from 0 below nel by 2
                   for jx fixnum from 1 by 2
                   sum
                   (let ((yl (itu-filt1 *itu-statel* (aref buf ix)))
                         (yr (itu-filt1 *itu-stater* (aref buf jx))))
                     (declare (type single-float yl yr))
                     (+ (* yl yl)
                        (* yr yr)))))
          )))
                
;; ------------------------------------------------

(defun rmsdb (rms)
  (+ (db10 rms)
     *itu-db-corr*))

(defun file-advance (f relpos)
  (file-position f (+ (file-position f) relpos)))

;; ------------------------------------------------

(defun extract-ule (vec start ixstart)
  (declare (optimize (speed 3)
                     (safety 0)
                     (float 0)))
  (declare (type (array (unsigned-byte 8) (*)) vec)
           (type fixnum start ixstart))
  (do ((val 0)
       (ix ixstart (1- ix)))
      ((minusp ix) val)
    (declare (type fixnum val ix))
    (setf val (logior (ash val 8)
                      (aref vec (+ start ix))))))
    
(defun extract-ube (vec start ixstop)
  (declare (optimize (speed 3)
                     (safety 0)
                     (float 0)))
  (declare (type (array (unsigned-byte 8) (*)) vec)
           (type fixnum start ixstart))
  (do ((val  0)
       (ix   0  (1+ ix)))
      ((>= ix ixstop) val)
    (declare (type fixnum val ix))
    (setf val (logior (ash val 8)
                      (aref vec (+ start ix))))))

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
  (let* ((sgn  (if (zerop (ldb (byte 1 31) val))
                   1
                 -1))
         (exp  (- (ldb (byte 8 23) val) 127 23))
         (mant (* sgn
                  (logior #.(ash 1 23) (ldb (byte 23 0) val)))))
    (declare (type fixnum sgn exp mant))
    (scale-float (float mant) exp)))

(defun convert-u80-to-flt80 (val)
  (declare (optimize (speed 3)
                     (safety 0)
                     (float 0)))
  (declare (type integer val))
  (let* ((sgn  (if (zerop (ldb (byte 1 79) val))
                   1
                 -1))
         (exp  (- (ldb (byte 15 64) val) #x4000 62))
         (mant (* sgn
                  (ldb (byte 64 0) val))))
    (declare (type fixnum sgn exp)
             (type integer mant))
    (scale-float (float mant 1d0) exp)))

;; ------------------------------------------------

(defun extract-u16le (vec &optional (start 0))
  (extract-ule vec start 1))

(defun extract-u16be (vec &optional (start 0))
  (extract-ube vec start 2))

(defun extract-u24le (vec &optional (start 0))
  (extract-ule vec start 2))

(defun extract-u24be (vec &optional (start 0))
  (extract-ube vec start 3))

(defun extract-u32le (vec &optional (start 0))
  (extract-ule vec start 3))

(defun extract-u32be (vec &optional (start 0))
  (extract-ube vec start 4))

;; -----------------------------------------------

(defun extract-s16le (vec &optional (start 0))
  (signing-16 (extract-u16le vec start)))

(defun extract-s16be (vec &optional (start 0))
  (signing-16 (extract-u16be vec start)))

(defun extract-s24le (vec &optional (start 0))
  (signing-24 (extract-u24le vec start)))

(defun extract-s24be (vec &optional (start 0))
  (signing-24 (extract-u24be vec start)))

(defun extract-flt32le (vec &optional (start 0))
  (convert-u32-to-flt32 (extract-u32le vec start)))

(defun extract-flt32be (vec &optional (start 0))
  (convert-u32-to-flt32 (extract-u32be vec start)))

(defun extract-u80be (vec &optional (start 0))
  (extract-ube vec start 10))

(defun extract-flt80be (vec &optional (start 0))
  (convert-u80-to-flt80 (extract-u80be vec start)))

;; ------------------------------------------------

(defvar *scratch-2*  (make-array 2
                               :element-type '(unsigned-byte 8)))

(defvar *scratch-3*  (make-array 3
                               :element-type '(unsigned-byte 8)))

(defvar *scratch-4*  (make-array 4
                               :element-type '(unsigned-byte 8)))

(defun read-16 (f)
  (let* ((v  *scratch-2*))
    (read-sequence v f)
    v))

(defun read-24 (f)
  (let* ((v  *scratch-3*))
    (read-sequence v f)
    v))

(defun read-32 (f)
  (let* ((v  *scratch-4*))
    (read-sequence v f)
    v))

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

(defun find-chunk (f chunk-magic &key (endian :le))
  (let* ((magic (make-string 4)))
    (read-sequence magic f)
    (unless (string= magic chunk-magic)
      (let ((offs (case endian
                    (:le (read-u32le f))
                    (:be (read-u32be f))
                    )))
        (file-advance f offs)
        (find-chunk f chunk-magic :endian endian)))
    ))

(defun read-wav-format (f)
  (let* ((dlen (read-u32le f))
         (fmt  (make-array dlen
                           :element-type '(unsigned-byte 8))))
    (read-sequence fmt f)
    fmt))

(defun read-aif-format (f)
  (let* ((dlen (read-u32be f))
         (fmt  (make-array dlen
                           :element-type '(unsigned-byte 8))))
    (read-sequence fmt f)
    fmt))

(defstruct wave-file
  f fname fsamp nsamp bits-per-sample nchan endian)

(defun do-with-open-wav-file (filename fn)
  (with-remembered-filename (fname :com.sd.wav.last-wave-file)
      (or filename
          (capi:prompt-for-file "Select input file"
                                :filter "*.wav;*.aif;*.aiff"
                                :pathname fname))
    (with-open-file (f fname
                       :direction :input
                       :element-type '(unsigned-byte 8))
      (let* ((magic (make-string 4)))
        (read-sequence magic f)
        (cond ((string= magic "RIFF") ;; .WAV format
               (file-advance f 4)
               (read-sequence magic f)
               (assert (string= magic "WAVE"))
               (find-chunk f "fmt ")
               (let* ((fmt   (read-wav-format f))
                      (nchan (extract-u16le fmt 2))
                      (fsamp (extract-u32le fmt 4))
                      (bits-per-sample (extract-u16le fmt 14)))
                 (find-chunk f "data")
                 (let* ((nb    (read-u32le f))
                        (nsamp (truncate (* 8 nb) (* nchan bits-per-sample))))
                   (funcall fn (make-wave-file :f   f
                                               :fname fname
                                               :fsamp fsamp
                                               :nsamp nsamp
                                               :bits-per-sample bits-per-sample
                                               :nchan nchan
                                               :endian :le)))
                 ))
              
              ((string= magic "FORM") ;; .AIF format
               (file-advance f 4)
               (read-sequence magic f)
               (let ((endian (cond ((string= magic "AIFF") :be)
                                   ((string= magic "AIFC") :le))))
                 (find-chunk f "COMM" :endian :be)
                 (let* ((fmt   (read-aif-format f))
                        (nchan (extract-u16be fmt 0))
                        (nsamp (extract-u32be fmt 2))
                        (bits-per-sample (extract-u16be fmt 6))
                        (fsamp (extract-flt80be fmt 8)))
                   (find-chunk f "SSND" :endian :be)
                   (file-advance f 12)
                   (funcall fn (make-wave-file :f f
                                               :fname fname
                                               :fsamp fsamp
                                               :nsamp nsamp
                                               :bits-per-sample bits-per-sample
                                               :nchan nchan
                                               :endian endian))
                   )))
               )))))

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
    (let* ((nbs     (truncate bps 8))
           (rawdata (make-array (* nsamp nbs nchan)
                                :element-type '(unsigned-byte 8)))
           (data    (or dst
                        (make-array (* nsamp nchan)
                                    :element-type 'single-float)))
           (extractor (ecase bps
                        (16  (case endian
                               (:le (lambda (vec pos)
                                      (scale-float (float (extract-s16le vec pos)) -15)))
                               (:be (lambda (vec pos)
                                      (scale-float (float (extract-s16be vec pos)) -15)))
                               ))
                        (24  (case endian
                               (:le (lambda (vec pos)
                                      (scale-float (float (extract-s24le vec pos)) -23)))
                               (:be (lambda (vec pos)
                                      (scale-float (float (extract-s24be vec pos)) -23)))
                               ))
                        (32  (case endian
                               (:le  'extract-flt32le)
                               (:be  'extract-flt32be)
                               ))
                        )))
      (lambda ()
        (read-sequence rawdata f)
        ;; (user::dump rawdata)
        (loop repeat (* nsamp nchan)
              for ix from 0 by nbs
              for jx from 0
              do
              (setf (aref data jx) (funcall extractor rawdata ix)))
        ;; (user::dump data)
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

(defun rnd1 (v)
  (float (* 0.1 (round v 0.1)) 1e0))

(defun r128-rating (&optional fname)
  (declare (optimize (speed 3)
                     (safety 0)
                     (float 0)))
  (with-wav-file (wf fname)
    (with-accessors ((nsamp  wave-file-nsamp)
                     (nch    wave-file-nchan)
                     (fsamp  wave-file-fsamp)
                     (fname  wave-file-fname)) wf
      (print fname)
      (let* ((ns100 (round (* 0.1 fsamp)))
             (data  (make-array (* 2 ns100)
                                :element-type 'single-float))
             (tp       0)
             (prms     0)
             (nblk     0)
             (rms4    (make-array 4
                                  :element-type 'single-float
                                  :initial-element 0.0))
             (rms4ix   0)
             (rms30   (make-array 30
                                  :element-type 'single-float
                                  :initial-element 0.0))
             (rms30ix 0)
             (hist    (make-array (truncate nsamp fsamp)
                                  :element-type    'single-float
                                  :adjustable      t
                                  :fill-pointer    0))
             (pk      0)
             (wget    (make-wave-data-getter wf ns100 :dst data))
             (iir-ans (make-itu-filt-result)))
        (assert (= nch 2)) ;; only interested in stereo music files
        (init-itu-filter (/ fsamp 1000))
        (do ((ns  nsamp  (- ns ns100)))
            ((> ns100 ns))
          (itu-filt (funcall wget) iir-ans)
          (with-accessors ((rss itu-filt-result-rss)
                           (tpl itu-filt-result-tpl)) iir-ans
            (setf tp  (max tp tpl)
                  (aref rms30 rms30ix)  rss
                  rms30ix  (mod (1+ rms30ix) 30)
                  (aref rms4 rms4ix)    rss
                  rms4ix   (mod (1+ rms4ix)   4))
            (let ((avg30 (/ (reduce '+ rms30) 30))
                  (avg4  (/ (reduce '+ rms4)   4)))
              ;; collect 3 sec windows for PR
              (when (> avg30 1e-7) ;; -70 dBFS absolute thresh
                (setf pk (max pk avg30))
                (when (zerop (mod rms30ix 10))
                  (vector-push-extend (float avg30 1e0) hist)))
              ;; collect 400 ms windows for PL
              (when (and (> avg4 1e-7)  ;; -70 dBFS absolute thresh
                         (> avg4 (* 0.1 prms))) ;; -10 dB relative thresh
                (setf prms (/ (+ avg4 (* prms nblk))
                              (incf nblk))))
              )))
        (destructuring-bind (p10 p95)
            (percentiles '(0.10 0.95)  ;; -20 dB rel gate
                            (remove-if (rcurry '< (* 0.01 prms)) hist))
          (let* ((pl (rmsdb prms)))
            (list
             :file fname
             :tpl  (rnd1 (db20 tp))
             :pl   (rnd1 pl)
             :lu23 (rnd1 (- pl -23))
             :lra  (rnd1 (- (rmsdb p95) (rmsdb p10)))
             :pr   (rnd1 (- (rmsdb pk) pl))
             )))))))

(defun r128-ratings (&optional files)
  (let* ((files (or files
                    (capi:prompt-for-files "Select Album Files"
                                           :filter "*.wav;*.aif;*.aiff"))))
    (when files
      (mapcar 'r128-rating files))
    ))

