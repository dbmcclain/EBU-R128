
(in-package :ebu-r128)

(defun select-kth (vec ixs k left right)
  (declare 
   #+:lispworks 
   (optimize (debug 1) (speed 3) (safety 0)) ;; what is this trying to achieve?
   (type fixnum k left right)
   (type (array fixnum 1) ixs)
   (type (array * 1) arr))
  (labels ((ind-aref (ix)
             (declare (type fixnum ix))
             (aref vec (the fixnum (aref ixs ix)))
             ))
    (let ((l   left)
          (r   right))
      (declare (type fixnum l r))
      (tagbody
       again-1
       (let ((v (ind-aref k))
             (i l)
             (j r))
         (declare (type fixnum i j))
         (tagbody
          again-2
          (let ((ip (do ((ix i (1+ ix)))
                        ((<= v (ind-aref ix)) ix)
                      (declare (type fixnum ix))
                      ))
                (jp (do ((ix j (1- ix)))
                        ((>= v (ind-aref ix)) ix)
                      (declare (type fixnum ix))
                      )))
            (declare (type fixnum ip jp))
            (if (<= ip jp)
                (let ((ipp (1+ ip))
                      (jpp (1- jp)))
                  (declare (type fixnum ipp jpp))
                  (rotatef (aref ixs ip) (aref ixs jp))
                  (setf i ipp
                        j jpp)
                  (if (<= ipp jpp)
                      (go again-2)))
              (setf i ip
                    j jp))
            ))
         (if (< j k) (setf l i))
         (if (< k i) (setf r j))
         (if (< l r) (go again-1))
         )))
    (values (ind-aref k) ixs)))

(defun iramp (nel)
  (declare (type fixnum nel))
  (declare (optimize (speed  3)
                     (safety 0)
                     ;; (debug  0)
                     (float  0)))
  (let ((v (make-array nel
                       :initial-element 0
                       :element-type 'fixnum)))
    (declare (type (simple-array fixnum (*)) v))
    (do ((ix (1- nel) (1- ix)))
        ((minusp ix) v)
      (declare (type fixnum ix))
      (setf (row-major-aref v ix) ix))
    ))

;; ----------------------------------------------
;;
(defmethod size-of ((v vector))
  ;; returns only the used size of v when v is possibly
  ;; an adjustable array with a larger total size but where
  ;; the fill pointer is smaller.
  (length v))

(defmethod size-of ((a array))
  (array-total-size a))

(defmethod vector-of (x)
  (make-array 1 :initial-element x))

(defmethod vector-of ((lst cons))
  (coerce lst 'vector))

(defmethod vector-of ((v vector))
  (if (array-has-fill-pointer-p v)
      (subseq v 0 (length v))
    v))

(defmethod vector-of ((arr array))
  (make-array (size-of arr)
              :displaced-to arr
              :element-type (array-element-type arr)))

;; -----------------------------------------------------
(defun percentile (pc arr)
  (let* ((vec   (vector-of arr))
         (len   (length vec))
         (limit (1- len))
         (ixs   (iramp len))
         (index (round (* limit pc) 100)))
    (values (select-kth vec ixs index 0 limit) index)))
      
(defun percentiles (pcs arr)
  (let* ((v       (vector-of arr))
         (len     (length v))
         (limit   (1- len))
         (ixs     (iramp len)))
    (mapcar #'(lambda (pc)
                (let ((nx (round (* pc limit))))
                  (select-kth v ixs nx 0 limit)))
            pcs)))


