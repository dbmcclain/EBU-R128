
(in-package :ebu-r128)

;; ---------------------------------------------------------------------

(defvar *db*)

(defun database-pathname ()
  (merge-pathnames "EBU R128 Album-LUFS.dat"
                   (sys:get-folder-path :documents)))

(defun load-database ()
  (let ((fname (database-pathname)))
    (if (probe-file fname)
        (with-open-file  (f (database-pathname)
                            :direction :input
                            :element-type '(unsigned-byte 8))
          (loenc:deserialize f))
      (make-hash-table :test #'string-equal))))

(defun save-database ()
  (when (gethash :updated *db*)
    (with-open-file  (f (database-pathname)
                        :direction :output
                        :if-exists :rename
                        :if-does-not-exist :create
                        :element-type '(unsigned-byte 8))
      (remhash :updated *db*)
      (loenc:serialize *db* f))))

#|
;; ----------------------------------------------------
;; for one-time reformatting of database keys...
(let* ((db2  (make-hash-table :test #'string-equal)))
  (with-hash-table-iterator (next-entry *db*)
    (loop
     (multiple-value-bind (more? key value) (next-entry)
       (declare (ignore key))
       (unless more? (return))
       (let* ((new-key (key-for-album value)))
         (setf (gethash new-key db2) value))
       ))
    (inspect db2)
    (setf *db* db2)))

;; ----------------------------------------------------
;; for one-time reformatting of database data...
(let* ()
  (with-hash-table-iterator (next-entry *db*)
    (loop
     (multiple-value-bind (more? key value) (next-entry)
       (declare (ignore key))
       (unless more? (return))
       (loop for rec in value do
             (labels ((fix (key rec)
                        (setf (slot-value rec key) (rnd1 (slot-value rec key))))
                      (fixem (keys rec)
                        (dolist (key keys)
                          (fix key rec)))
                      (fixvec (key rec)
                        (setf (slot-value rec key) (map 'vector #'rnd1 (slot-value rec key))))
                      (fixemv (keys rec)
                        (dolist (key keys)
                          (fixvec key rec))))
               (fixem '(lu23 lra pl tpl pr) rec)
               (fixem '(pl10 pl95) (r128-track-hist rec))
               (fixemv '(shist phist) (r128-track-hist rec))
               ))
       )))
  (inspect *db*))

;; ----------------------------------------------------
;; for one-time reformatting of database albums...

(let* ()
  (with-hash-table-iterator (next-entry *db*)
    (loop
     (multiple-value-bind (more? key album) (next-entry)
       (unless more? (return))
       (setf (r128-album-lu23 album) (get-max-lu23 (r128-album-tracks album)))
       ))))
  

;; ----------------------------------------------------
;; removing a database entry...
(r128-lookup "enya - ")
(key-for-data (second *))
(remhash * *db*)
(update-album-list)
(save-database)
|#
;; -------------------------------------------------------------------

(defun get-file-collection (&optional files)
  (let* ((files (or files
                    (capi:prompt-for-files "Select Album Files"
                                           :filter "*.wav;*.aif;*.aiff;*.sd2;*.mp3;*.m4a;*.mp4;*.caf"
                                           :pathname (remembered-filename :com.sd.wav.last-wave-file) ))))
    (when files
      (remember-filename :com.sd.wav.last-wave-file (car files)))
    files))

(defun abbrev (str nel)
  (if (< (length str) nel)
      str
    (concatenate 'string (um:take (ceiling (- nel 3) 2) str)
                 "..."
                 (um:drop (- (length str) (floor (- nel 3) 2)) str))
    ))

(defun key-for-album (summ-recs)
  (multiple-value-bind (artist album) (get-artist-album (first summ-recs))
    (concatenate 'string artist " ^^ " album)))

(defun title-for-album (album-rec)
  (format nil "~20A : ~30A"
          (abbrev (r128-album-artist album-rec) 20)
          (abbrev (r128-album-name   album-rec) 30)))

;; --------------------------------------------------------------------------------

(capi:define-interface albums-display ()
  ((private-data  :accessor private-data :initarg :private-data))
  (:panes
   (search-pane capi:text-input-pane
                :accessor    search-pane
                :title       "Search..."
                :callback    'search-albums)
                
   (albums-list capi:multi-column-list-panel
                :accessor           list-panel
                :visible-min-width  300
                :visible-min-height 300
                :selection-callback 'click-show-summary
                :callback-type      :item-element
                :drop-callback      'dropme
                :title              "Rated Albums"
                :columns '((:title "Artist"
                            :adjust :left
                            :width  (character 20))
                           (:title "Album"
                            :adjust :left
                            :width  (character 20))
                           )))
  (:layouts
   (main-layout capi:column-layout
                '(search-pane albums-list))
   ))

(defun click-show-summary (item element &optional which-track)
  (let* ((data  (private-data (capi:element-interface element)))
         (row   (find item data
                      :key  #'first
                      :test #'equalp)))
    (show-ratings (second row) which-track)))

(defun search-albums (text intf)
  (declare (ignore intf))
  (apply #'r128-lookup (um:split-string text)))

(defun update-album-list (&optional selection)
  (let ((items nil))
    (with-hash-table-iterator (next-entry *db*)
      (loop
       (multiple-value-bind (more? key album) (next-entry)
         (unless more? (return))
         (when (r128-album-p album)
           (push (list key album) items)))
       ))
    (let* ((items     (sort items
                            #'string-lessp
                            :key #'first))
           (selection (and selection
                           (position selection items
                                     :key #'first
                                     :test #'string-equal)))
           (top-intf    (capi:find-interface 'r128-scanner))
           (intf        (albums-pane top-intf))
           (albums-list (list-panel intf))
           (entries     (mapcar (lambda (pair)
                                  (destructuring-bind (key val) pair
                                    (declare (ignore key))
                                    (list (list (r128-album-artist val)
                                                (r128-album-name   val))
                                          val)))
                                items)))
      (setf (private-data intf) entries
            (capi:list-panel-unfiltered-items albums-list) (mapcar #'first entries)
            (capi:choice-selection albums-list) selection)
      )))
  
;; -------------------------------------------------------------------

(capi:define-interface tracks-display ()
  ((private-data :accessor private-data :initarg :private-data))
  (:panes
   (track-list  capi:multi-column-list-panel
                :accessor  track-list
                :visible-min-width  650
                :visible-min-height 300
                :selection-callback 'click-show-history
                :callback-type :item-element
                :drop-callback      'dropme
                :columns '((:title "Track"
                            :adjust :left
                            :width  (character 40))
                           (:title "LU23"
                            :adjust :right
                            :width  (character 10))
                           (:title "LRA"
                            :adjust :right
                            :width  (character 10))
                           (:title "PL"
                            :adjust :right
                            :width  (character 10))
                           (:title "TPL"
                            :adjust :right
                            :width  (character 10))
                           (:title "PR"
                            :adjust :right
                            :width  (character 10)))))
  (:layouts
   (main-layout capi:column-layout
                '(track-list))
   ))

(defun click-show-history (item element)
  (let* ((album-rec  (private-data (capi:element-interface element)))
         (row        (find (first item) (r128-album-tracks album-rec)
                           :key  #'r128-track-name
                           :test #'string-equal)))
    (show-plots row (r128-album-lu23 album-rec))))

(defun show-ratings (album-rec &optional which-track)
  (let* ((items  (labels ((fmt (key track)
                                     (format nil "~5,1F" (* 0.1 (slot-value track key))))
                                   (fmt-fields (keys track)
                                     (mapcar (um:rcurry #'fmt track) keys)))
                            (mapcar (lambda (track)
                                      (cons (r128-track-name track)
                                            (fmt-fields '(lu23 lra pl tpl pr) track)))
                                    (r128-album-tracks album-rec))))
         (selection (or which-track
                        (let ((lst (mapcar #'r128-track-lu23 (r128-album-tracks album-rec))))
                          (position (reduce #'max lst) lst))))
         (title     (title-for-album album-rec))
         (top-intf  (capi:find-interface 'r128-scanner))
         (intf      (tracks-pane top-intf))
         (tracks    (track-list intf)))
    (setf (capi:interface-title intf)               title
          (private-data intf)                       album-rec
          (capi:list-panel-unfiltered-items tracks) items
          (capi:choice-selection tracks)            selection)
    (show-plots (elt (r128-album-tracks album-rec) selection) (r128-album-lu23 album-rec)))
  album-rec)

;; -------------------------------------------------------------------

(capi:define-interface r128-scanner ()
  ()
  (:panes
   (albums-pane albums-display
                :accessor albums-pane)
   (tracks-pane tracks-display
                :title "Tracks"
                :accessor tracks-pane))
  (:layouts
   (main-layout capi:row-layout
                '(albums-pane tracks-pane)))
  (:default-initargs
   :title "EBU R128 Scanner"))

;; ---------------------------------------------------------------------

(defun dropme (pane drop-object stage)
  (flet ((set-effect-for-operation ()
           (dolist (effect '(:move :copy :link :generic))
             (when (capi:drop-object-allows-drop-effect-p drop-object effect)
               (setf (capi:drop-object-drop-effect drop-object) effect)
               (return t)))))
    (case stage
      (:formats         (capi:set-drop-object-supported-formats drop-object '(:string :filename-list)))
      ((:enter :drag)   (if (or (capi:drop-object-provides-format drop-object :filename-list)
                                (capi:drop-object-provides-format drop-object :string))
                            (set-effect-for-operation)))
      (:drop
       (handler-case
           (cond ((and (capi:drop-object-provides-format drop-object :string)
                       (set-effect-for-operation))
                  (tell-scanner #'do-r128-raiting
                                (capi:drop-object-get-object drop-object
                                                             pane
                                                             :string)))
                 
                 ((and (capi:drop-object-provides-format drop-object :filename-list)
                       (set-effect-for-operation))
                  (tell-scanner #'do-r128-ratings
                                (capi:drop-object-get-object drop-object
                                                             pane
                                                             :filename-list)))
                 )
         (error (err)
           (capi:display-message "Huh?" #| "Error: ~A" err |#))
         ))
      )))

;; -----------------------------------------------------------------

(defun start-intf ()
  (setf *db* (load-database))
  (update-album-list))

(start-intf)
