
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

(defun sfloat (x)
  (float x 1e0))

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
              (mapcar 'sfloat
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
                        0.0148925781250d0
                        -0.0083007812500d0))))

(defvar *ph1*
  (make-array 12
              :element-type 'single-float
              :initial-contents
              (mapcar 'sfloat
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
                        -0.0189208984375d0))))
              
(defvar *ph2*
  (make-array 12
              :element-type 'single-float
              :initial-contents  
              (mapcar 'sfloat
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
                        -0.0291748046875d0))))

(defvar *ph3*
  (make-array 12
              :element-type 'single-float
              :initial-contents
              (mapcar 'sfloat
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
                        0.0017089843750d0))))

#|
(let* ((x  (loop for x0 across *ph0*
                 for x1 across *ph1*
                 for x2 across *ph2*
                 for x3 across *ph3*
                 collect x0
                 collect x1
                 collect x2
                 collect x3))
       (fs   (map 'vector (rcurry '* 192/64) (iramp 32)))
       (fdb  (fft:fwd-magnitude-db (coerce x 'vector))))
  (plt:plot 'plt x :clear t)
  (plt:plot 'fplt fs fdb :clear t))
|#

(defvar *bufl*
  (make-array 24
              :element-type 'single-float
              :initial-element 0e0))
(defvar *bufr*
  (make-array 24
              :element-type 'single-float
              :initial-element 0e0))
(defvar *bix* 0)
(defvar *phs*
  (list *ph0*
        *ph1*
        *ph2*
        *ph3*))

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
  (when (minusp *bix*)
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

(defun sqr (x)
  (declare (optimize (speed 3)
                     (safety 0)
                     (float 0)))
  (declare (type single-float x))
  (* x x))

(defun fir-maxabs-samp (x &optional (buf *bufl*))
  (declare (optimize (speed 3)
                     (safety 0)
                     (float 0)))
  (declare (type (array single-float (*)) buf)
           (type single-float x))
  (store-fir-sample x buf)
  (loop for ph in *phs*
        maximize (sqr (fir-ph-filt ph buf))))

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
(let* ((x  (mapcar 'sfloat
                   (append '(-1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1 -1
                                1 -1 -1 -1 -1 -1 -1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1)
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
;; --------------------------------------------------------------------------
(defstruct itu-filt-result
  tpl rss)

(defun c-hsiir-init (coffs)
  (fli:with-dynamic-foreign-objects ()
    (let* ((c-coffs (fli:allocate-dynamic-foreign-object
                     :type   :double
                     :nelems 10
                     :initial-contents coffs
                     )))
      (_hsiir_init c-coffs))))

(defun c-hsiir-eval (buf nsamp ans)
  (fli:with-dynamic-foreign-objects ()
    (let* ((c-buf (fli:allocate-dynamic-foreign-object
                   :type   :float
                   :nelems (* 2 nsamp)
                   ))
           (c-ans (fli:allocate-dynamic-foreign-object
                   :type :float :nelems 2)))
      (fli:replace-foreign-array c-buf buf :start1 0 :end1 (* 2 nsamp))
      (_hsiir_eval c-buf nsamp c-ans)
      (setf (itu-filt-result-tpl ans) (fli:dereference c-ans :index 1)
            (itu-filt-result-rss ans) (fli:dereference c-ans :index 0))
      )))

;; --------------------------------------------------------------------------

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
  (let* ((hpf    (hpf       (/ 0.0375 fs) 0.5))
         (hsh    (hishelf 4 (/ 1.5 fs)    0.707))
         (dcoffs (append hpf hsh))
         (coffs  (make-array 10
                             :element-type     'single-float
                             :initial-contents
                             (mapcar 'sfloat dcoffs))))
    (setf *itu-filter* coffs
          *itu-db-corr* (- (+ (db-filt 1 hsh fs)
                              (db-filt 1 hpf fs))))
    (fill *itu-stateL* 0e0)
    (fill *itu-stateR* 0e0)
    (fir-init)

    (c-hsiir-init dcoffs)
    ))
  
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

(defun itu-filt (buf ans)
  #|
  (declare (optimize (speed 3)
                     (safety 0)
                     (float 0)))
  (declare (type (array single-float (*)) buf))
  (let* ((nel (length buf))
         (sf  (/ 2.0 nel)))
    (declare (type fixnum nel)
             (type single-float sf))
    (setf (itu-filt-result-tpl ans)
          (sfloat
           (max (loop for ix fixnum from 0 below nel by 2 maximize
                      (fir-maxabs-samp (aref buf ix) *bufl*))
                (loop for ix fixnum from 1 below nel by 2 maximize
                      (fir-maxabs-samp (aref buf ix) *bufr*))))
          
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
          ))
  |#
  (c-hsiir-eval buf (truncate (length buf) 2) ans)
  )
                
;; ------------------------------------------------

(defun rmsdb (rms)
  (+ (db10 rms)
     *itu-db-corr*))

(defun rnd1 (v)
  (sfloat (* 0.1 (round v 0.1))))

(defstruct r128-state
  fname
  (tp    0.0)
  (prms  0.0)
  (pk    0.0)
  (nblk    0)
  (hist  (make-array 10000 ;; abt 2.8 hrs at 1/sec
                     :element-type 'single-float
                     :adjustable   t
                     :fill-pointer 0)))

(defun r128-summary (state)
  (with-accessors ((tp    r128-state-tp)
                   (prms  r128-state-prms)
                   (pk    r128-state-pk)
                   (hist  r128-state-hist)
                   (fname r128-state-fname)) state

    (destructuring-bind (p10 p95)
        (percentiles '(0.10 0.95)  ;; -20 dB rel gate
                     (remove-if (rcurry '< (* 0.01 prms)) hist))
      (let* ((shist (map 'vector (lambda (rss)
                                   (+ 23.0 (rmsdb rss)))
                         hist))
             (p95lu (+ 23.0 (rmsdb p95)))
             (p10lu (+ 23.0 (rmsdb p10)))
             (pl    (rmsdb prms)))

        #+:has-plotter
        (progn
          (plt:plot 's-record shist
                    :clear t
                    :yrange `(-18 ,(max 9 (reduce 'max shist)))
                    :title "Short Term (3s) History"
                    :xtitle "Time [s]"
                    :ytitle "Loudness [LU]")
          (plt:plot 's-record '(0 0) `(,p10lu ,p95lu)
                    :color :orange
                    :thick 5)
          (plt:histogram 's-histo shist
                         :clear t
                         :title "Short Term (3s) Histogram"
                         :xtitle "Loudness [LU]"
                         :ytitle "Counts")
          (plt:plot 's-histo `(,p10lu ,p95lu) '(0 0)
                    :color :orange
                    :thick 5))
        
        (list
         :file fname
         :tpl  (rnd1 (db10 tp))
         :pl   (rnd1 pl)
         :lu23 (rnd1 (- pl -23.0))
         :lra  (rnd1 (- p95lu p10lu))
         :pr   (rnd1 (- (rmsdb pk) pl))
         :hist shist
         )))))
  
(defun update-r128-state (state tpl avg4 avg30 1secp)
  ;; tpl   = true peak est
  ;; avg30 = 3 sec sliding window rss
  ;; avg4  = 400 ms sliding window rss
  (with-accessors ((tp    r128-state-tp)
                   (prms  r128-state-prms)
                   (nblk  r128-state-nblk)
                   (pk    r128-state-pk)
                   (hist  r128-state-hist)) state
    (setf tp (max tp tpl))
    ;; collect 3 sec windows for PR
    (when (> avg30 1e-7) ;; -70 dBFS absolute thresh
      (setf pk (max pk avg30)) ;; max 3 sec level
      (when 1secp ;; 1 sec intervals
        (vector-push-extend avg30 hist)))
    ;; collect 400 ms windows for PL
    (when (and (> avg4 1e-7)  ;; -70 dBFS absolute thresh
               (> avg4 (* 0.1 prms))) ;; -10 dB relative thresh
      (setf prms (/ (+ avg4 (* prms nblk))
                    (incf nblk))))
    ))

(defun accum-r128-rating (&optional fname (state (make-r128-state)))
  (declare (optimize (speed 3)
                     (safety 0)
                     (float 0)))
  (with-wav-file (wf fname)
    (with-accessors ((nsamp  wave-file-nsamp)
                     (nch    wave-file-nchan)
                     (fsamp  wave-file-fsamp)
                     (fname  wave-file-fname)) wf

      (print fname)
      (setf (r128-state-fname state) fname)
      
      (let* ((ns100 (round (* 0.1 fsamp))) ;; 100 ms increments
             (data  (make-array (* 2 ns100)
                                :element-type 'single-float))
             (rms4    (make-array 4
                                  :element-type 'single-float
                                  :initial-element 0.0))
             (rms4ix   0)
             (rms30   (make-array 30
                                  :element-type 'single-float
                                  :initial-element 0.0))
             (rms30ix 0)
             (wget    (make-wave-data-getter wf ns100 :dst data))
             (iir-ans (make-itu-filt-result)))
        
        (assert (= nch 2)) ;; only interested in stereo music files
        (init-itu-filter (/ fsamp 1000))
        
        (do ((ns  nsamp  (- ns ns100)))
            ((> ns100 ns))
          (itu-filt (funcall wget) iir-ans)
          (with-accessors ((rss itu-filt-result-rss)
                           (tpl itu-filt-result-tpl)) iir-ans
            (setf (aref rms30 rms30ix)  rss
                  rms30ix  (mod (1+ rms30ix) 30)
                  (aref rms4 rms4ix)    rss
                  rms4ix   (mod (1+ rms4ix)   4))
            (update-r128-state state
                               tpl
                               (/ (reduce '+ rms4)   4.0) ;; 400 ms sliding window
                               (/ (reduce '+ rms30) 30.0) ;; 3 s sliding window
                               (zerop (mod rms30ix 10)))
            ))
        (r128-summary state) ;; provide a walking visual summary
        state
        ))))

(defun get-file-collection (&optional files)
  (let* ((files (or files
                    (capi:prompt-for-files "Select Album Files"
                                           :filter "*.wav;*.aif;*.aiff"
                                           :pathname (remembered-filename :com.sd.wav.last-wave-file) ))))
    (when files
      (remember-filename :com.sd.wav.last-wave-file (car files)))
    files))
      
(defun r128-rating (&optional file)
  ;; obtain ratings for single audio file
  (r128-summary (accum-r128-rating file)))

(defun r128-ratings (&optional files)
  ;; obtain a collection of ratings from several files
  (let* ((files (get-file-collection files)))
    (when files
      (mapcar 'r128-rating files))
    ))

(defun r128-album-rating (&optional files)
  ;; obtain an overall album rating for several files
  (let* ((files (get-file-collection files)))
    (when files
      (let ((final (reduce (lambda (state file)
                             (accum-r128-rating file state))
                           files
                           :initial-value (make-r128-state))))
        (setf (r128-state-fname final) "Album")
        (r128-summary final)))
    ))
