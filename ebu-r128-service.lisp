
(in-package :ebu-r128)

;; ---------------------------------------------------------------------
;; Set up a central service because the above code is not reentrant...

(defvar *ch-scan*         (rch:make-channel))
(defvar *scanner-service* nil)

(defun scanner-service ()
  (unwind-protect
      (loop while *scanner-service* do
            (rch:sync
             (rch:choose
              (rch:wrap (rch:recvEvt *ch-scan*)
                        (lambda (args)
                          (destructuring-bind (ch fn &rest scan-args) args
                            (rch:poke ch (apply #'um:capture-ans-or-exn fn scan-args)))
                          ))
              (rch:wrap (rch:timerEvt 30)
                        (lambda (args)
                          (declare (ignore args))
                          (tell-scanner #'save-database)
                          ))
              )))
    (setf *scanner-service* nil)
    (save-database)))

(defun ensure-scanner-service ()
  (when (sys:compare-and-swap *scanner-service* nil t)
    (rch:spawn #'scanner-service
               :name "Audio Scanner-")))

(defun tell-scanner (&rest args)
  (let ((ch (rch:make-channel)))
    (rch:poke *ch-scan* (cons ch args))
    (ensure-scanner-service)
    ch))

(defun ask-scanner (&rest args)
  (rch:sync (rch:wrap
             (rch:recvEvt (apply #'tell-scanner args))
             #'um:recover-ans-or-exn)))

(defun stop-scanner ()
  (tell-scanner #'(lambda ()
                    (setf *scanner-service* nil))))

;; -------------------------------------------------------------
;; actual worker functions

(defun do-r128-rating (file)
  (r128-summary (accum-r128-rating file)))

(defun do-r128-ratings (files &optional nest)
  (when files
    ;; (inspect files)
    (let* ((dirs  (remove-if-not #'lw:file-directory-p files))
           (files (remove-if #'lw:file-directory-p files))
           (data  (and files
                       (print (directory-namestring (first files)))
                       (and t
                            (remove nil (mapcar (lambda (file)
                                                  ;; (print file)
                                                  (ignore-errors
                                                    (do-r128-rating file)))
                                                files))))))
      (when data
        (multiple-value-bind (artist album-name) (get-artist-album (first data))
          (let* ((key   (key-for-album data))
                 (album (make-r128-album
                         :artist artist
                         :name   album-name
                         :lu23   (get-max-lu23 data)
                         :tracks (mapcar (lambda (track)
                                           (setf (r128-track-name track) (pathname-name (r128-track-name track)))
                                           track)
                                         data))
                        ))
            (setf (gethash key *db*) album
                  (gethash :updated *db*) t)
            (update-album-list key)
            (show-ratings album)
            )))
      #||#
      (dolist (dir dirs)
        (let ((home (hcl:get-working-directory)))
          (unwind-protect
              (progn
                (hcl:change-directory dir)
                (do-r128-ratings (directory "./" :directories t) t))
            (hcl:change-directory home))
          ))
      #||#
      #|
      (dolist (dir dirs)
        (do-r128-ratings (directory dir :directories t) t))
      |#
      (unless nest
        (print "Finished!"))
      )))

(defun do-r128-album-rating (files)
  (when files
    (let ((final (reduce (lambda (state file)
                           (accum-r128-rating file state))
                         files
                         :initial-value (make-r128-state))))
      (setf (r128-state-fname final) "Album")
      (r128-summary final))
    ))

;; -------------------------------------------------------------
;; manual interface to the worker functions and service

(defun r128-lookup (&rest lookup-keys)
  ;; searches for a fragment matching anywhere among (artist, album, track name)
  ;; all of the fragments must be found together
  (when lookup-keys
    (let* ((top-intf   (capi:find-interface 'r128-scanner))
           (intf       (albums-pane top-intf))
           (album-list (list-panel intf))
           (albums     (capi:list-panel-unfiltered-items album-list))
           (which-track nil)
           (selection  (position-if (lambda (entry)
                                      (let* ((album-rec (second entry))
                                             (artist    (r128-album-artist album-rec))
                                             (album     (r128-album-name   album-rec))
                                             (tracks    (r128-album-tracks album-rec)))
                                        (labels ((contains? (str target-str)
                                                   (search str target-str :test #'char-equal)))
                                          (some (lambda (track)
                                                  (let ((name (r128-track-name track)))
                                                    (and (every (lambda (str)
                                                                  (or (contains? str artist)
                                                                      (contains? str album)
                                                                      (contains? str name)))
                                                              lookup-keys)
                                                       (setf which-track (position track tracks)))
                                                    ))
                                                tracks))))
                                      (private-data intf))))
      (if selection
          (progn
            (setf (capi:choice-selection album-list) selection)
            (click-show-summary (elt albums selection) album-list which-track)
            (elt (private-data intf) selection))
        :not-found))))

(defun r128-rating (&optional file)
  ;; obtain ratings for single audio file
  (ask-scanner #'do-r128-rating file))

(defun r128-ratings (&optional files)
  ;; obtain a collection of ratings from several files
  (um:when-let (files (get-file-collection files))
    (ask-scanner #'do-r128-ratings files)))

(defun r128-album-rating (&optional files)
  ;; obtain an overall album rating for several files
  (um:when-let (files (get-file-collection files))
    (ask-scanner #'do-r128-album-rating files)))

