(in-package :lice)

(defun format-filename (filename)
  (declare (type pathname filename))
  (format nil "~a~@[.~a~]" 
	  (pathname-name filename)
	  (pathname-type filename)))

(defun slurp-file (filename)
  "Return the contents of FILENAME as a string."
  (declare (type pathname filename))
  (with-open-file (in filename)
    ;; Note the 1+ is to leave a 1 character gap, because a buffer
    ;; can't have a 0 length gap.
    (let* ((str (make-array (1+ (file-length in)) :element-type 'character)))
      (read-sequence str in)
      str)))

(defun make-file-buffer (filename)
  "Assumes filename has been verified to exist and is a file."
  ;; load the file, put it in a buffer
  (declare (type pathname filename))
  (let* ((data (slurp-file filename))
	 (b (make-instance 'buffer
			   :file filename 
                           :point (make-marker)
                           :mark (make-marker)
			   :data data
			   :mode-line *mode-line-format*
			   :name (format-filename filename)
			   ;; 1- because the data has been allocated with 1 extra character
			   :gap-start (1- (length data))
			   :gap-size 1 ;;(length +other-buf+)
			   :major-mode fundamental-mode)))
    (set-marker (buffer-point b) 0 b)
    (set-marker (mark-marker b) 0 b)
    b))

(defun find-file-no-select (filename)
  ;; TODO: verify the file is a file (not a dir) and it exists, etc.
  (let ((pn (parse-namestring filename)))
    ;; check that the directory exists
    (unless (ensure-directories-exist pn)
      (error "dir doesn't exist"))
    (if (probe-file pn)
	(let ((b (make-file-buffer pn)))
	  (push b *buffer-list*)
	  b)
      ;; It doesn't exist so open an empty buffer but give it a file,
      ;; so it can be saved.
      (let ((b (get-buffer-create (format-filename pn))))
	(setf (buffer-file b) pn)
	b))))

(defcommand find-file ((filename)
		       (:file "Find File: "))
  ""
  (let ((b (find-file-no-select filename)))
    (switch-to-buffer b)))

(defcommand save-buffer ()
  (let ((buffer (current-buffer)))
    (when (buffer-file buffer)
      (if (buffer-modified buffer)
	  (with-open-file (out (buffer-file buffer)
			       :direction :output
			       :if-exists :overwrite
			       :if-does-not-exist :create)
	    ;; write the data before the gap
	    (write-sequence (buffer-data buffer) out 
			    :start (buffer-min buffer)
			    :end (buffer-gap-start buffer))
	    ;; write the data after the gap
	    (write-sequence (buffer-data buffer) out 
			    :start (gap-end buffer)
			    :end (length (buffer-data buffer)))
	    (setf (buffer-modified buffer) nil)
	    (message "Wrote ~a~%" (buffer-file (current-buffer))))
	(message "(No changes need to be saved)")))))

(defun file-completions (base predicate other)
  "Return a list of possible file completions given the base file, BASE. OTHER is not used."
  (declare (ignore other))
  ;; FIXME: they need to be strings
  (let ((tester (or predicate
		    (lambda (s)
		      (string= base s :end2 (min (length base)
                                                 (length s)))))))
    (loop for elt in (mapcar 'princ-to-string (directory (merge-pathnames (make-pathname :name :wild) base)))
       when (funcall tester elt)
       collect elt)))

(defcommand load-file ((file)
                       (:file "Load file: "))
  "Load the Lisp file named FILE."
  (load file))

(provide :lice-0.1/files)
