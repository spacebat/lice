(declaim (optimize (debug 3)))

(in-package :lice)

(defconstant +beg+ 0)

(defvar *buffer-list* nil
  "All buffers managed by lice. buffers are sorted from most recently
accessed to least. the CAR is the most recent buffer.")

(defvar *current-buffer* nil
  "When this buffer is non-nil, it contains the current buffer. Calls
to `current-buffer' return this buffer. Otherwise, `current-buffer'
returns the current frames's current window's buffer.

This variable should never be set using `setq' or `setf'. Bind it with
`let' for as long as it needs to be set.")

(defclass pstring ()
  ((data :type string :initarg :data :accessor pstring-data)
   (intervals :type (or null interval) :initform nil :initarg :intervals :accessor intervals))
  (:documentation "The lice string implementation."))

(defmethod print-object ((obj pstring) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~s" (pstring-data obj))))

(defun pstring-length (ps)
  "Return the length of the string in PS"
  (declare (type pstring ps))
  (length (pstring-data ps)))

(defclass buffer ()
  ((file :type (or null pathname) :initarg :file :accessor buffer-file)
   (name :type string :initarg :name :accessor buffer-name)
   (point :type marker :initarg :point :accessor buffer-point)
   (mark :type marker :initarg :mark :accessor buffer-mark-marker)
   ;; A string containing the raw buffer
   (data :type (array character 1) :initarg :data :accessor buffer-data)
   (intervals :type (or null interval) :initform nil :initarg :intervals :accessor intervals)
   (gap-start :type integer :initarg :gap-start :accessor buffer-gap-start)
   (gap-size :type integer :initarg :gap-size :accessor buffer-gap-size)
   ;; mode-line
   (mode-line :type list :initarg :mode-line :initform nil :accessor buffer-mode-line)
   (mode-line-string :type string :initform "" :accessor buffer-mode-line-string)
   (modified :type boolean :initform nil :accessor buffer-modified)
   (read-only :type boolean :initform nil :accessor buffer-read-only)
   (tick :type integer :initform 0 :accessor buffer-modified-tick :documentation
	 "The buffer's tick counter. It is incremented for each change
in text.")
   (display-count :type integer :initform 0 :accessor buffer-display-count :documentation
	 "The buffer's display counter. It is incremented each time it
is displayed in a window.")
   (display-time :type integer :initform 0 :accessor buffer-display-time :documentation
	 "The last time the buffer was switched to in a window.")
   (major-mode :type major-mode :initarg :major-mode :accessor buffer-major-mode)
   (markers :type list :initform '() :accessor buffer-markers)
   (locals-variables :type hash-table :initform (make-hash-table) :accessor buffer-local-variables)
   (locals :type hash-table :initform (make-hash-table) :accessor buffer-locals))
  (:documentation "A Buffer."))

(defmethod print-object ((obj buffer) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a" (buffer-name obj))))

(define-condition args-out-of-range (lice-condition)
  () (:documentation "Raised when some arguments (usually relating to a
buffer) are out of range."))

(define-condition beginning-of-buffer (lice-condition)
  () (:documentation "Raised when it is an error that the point is at
the beginning of the buffer."))

(define-condition end-of-buffer (lice-condition)
  () (:documentation "Raised when it is an error that the point is at
the end of the buffer."))

(define-condition buffer-read-only (lice-condition)
  () (:documentation "Raised when there is an attempt to insert into a read-only buffer."))

(defun mark-marker (&optional (buffer (current-buffer)))
  "Return this buffer's mark, as a marker object.
Watch out!  Moving this marker changes the mark position.
If you set the marker not to point anywhere, the buffer will have no mark."
  ;; FIXME: marks can't be inactive ATM
  (buffer-mark-marker buffer))


;;; buffer locals

(defstruct buffer-local-binding
  symbol value doc-string)

(defvar *global-buffer-locals* (make-hash-table)
  "The default values of buffer locals and a hash table containing all possible buffer locals")

(defun buffer-local-exists-p (symbol)
  (multiple-value-bind (v b) (gethash symbol *global-buffer-locals*)
    (declare (ignore v))
    b))  

(defun make-buffer-local (symbol default-value &optional doc-string)
  (unless (buffer-local-exists-p symbol)
    (setf (gethash symbol *global-buffer-locals*)
	  (make-buffer-local-binding :symbol symbol
				     :value default-value
				     :doc-string doc-string))))

(defmacro define-buffer-local (symbol default-value &optional doc-string)
  "buffer locals are data hooks you can use to store values per
buffer. Use them when building minor and major modes. You
generally want to define them with this so you can create a
docstring for them. there is also `make-buffer-local'."
  `(make-buffer-local ,symbol ,default-value ,doc-string))

(defun (setf buffer-local) (symbol value)
  "Set the value of the buffer local in the current buffer."
  (setf (gethash symbol (buffer-locals *current-buffer*)) value)
  ;; create a global buffer local entry if needed.
  (make-buffer-local symbol nil))

(defun buffer-local (symbol)
  "Return the value of the buffer local symbol. If none exists
for the current buffer then use the global one. If that doesn't
exist, return nil."
  (multiple-value-bind (v b) (gethash symbol (buffer-locals *current-buffer*))
    (if b
	v
	(multiple-value-bind (v b) (gethash symbol *global-buffer-locals*)
	  (when b
	    (buffer-local-binding-value v))))))


;;; buffer related conditions

;;(define-condition end-of-buffer)


;;; Markers

(defclass marker ()
  ((position :type integer :initform 0 :accessor marker-position)
   (buffer :type (or buffer null) :initform nil :accessor marker-buffer))
  (:documentation "A Marker"))

(defmethod print-object ((obj marker) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a" (marker-position obj))))

(defun copy-marker (marker)
  "Return a new marker with the same point and buffer as MARKER."
  (make-marker (marker-position marker) (marker-buffer marker)))

(defun make-marker (&optional position buffer)
  "Return a newly allocated marker which does not point anywhere."
  (let ((m (make-instance 'marker)))
    (when (and position buffer)
      (set-marker m position buffer))
    m))

(defun set-marker (marker position &optional (buffer (current-buffer)))
  ;; TODO: make sure the position is within bounds
  (setf (marker-position marker) position)
  ;; remove the marker from its buffer, when appropriate
  (when (and (marker-buffer marker)
	     (or (null buffer)
		 (not (eq (marker-buffer marker) buffer))))
    (setf (buffer-markers (marker-buffer marker))
	    (delete marker (buffer-markers (marker-buffer marker)) :key #'weak-pointer-value)))
  ;; Update marker's buffer
  (setf (marker-buffer marker) buffer)
  ;; Put the marker in the new buffer's list (wrapped in a
  ;; weak-pointer), when appropriate.
  (when buffer
    (push (make-weak-pointer marker) (buffer-markers buffer))))

(defun update-markers-del (buffer start size)
  ;; First get rid of stale markers
  (purge-markers buffer)
  (dolist (wp (buffer-markers buffer))
    (let ((m (weak-pointer-value wp)))
      ;; paranoia, maybe the GC freed some stuff after the marker
      ;; purge.
      (when m
	;; markers are before the marker-position.
	(cond ((>= (marker-position m) (+ start size))
	       (decf (marker-position m) size))
	      ((> (marker-position m) start)
	       (setf (marker-position m) start)))))))

(defun update-markers-ins (buffer start size)
  ;; First get rid of stale markers
  (purge-markers buffer)
  (dolist (wp (buffer-markers buffer))
    (let ((m (weak-pointer-value wp)))
      ;; markers are before the marker-position.
      (when (and m (> (marker-position m) start)
		 (incf (marker-position m) size))))))

(defun purge-markers (buffer)
  "Remove GC'd markers."
  (setf (buffer-markers buffer)
	(delete-if (lambda (m)
		     (multiple-value-bind (v c) (weak-pointer-value m)
                       (declare (ignore v))
		       (not c)))
		   (buffer-markers buffer))))


;;;

(defun inc-buffer-tick (buffer)
  "Increment the buffer's ticker."
  (incf (buffer-modified-tick buffer)))

;;; Some wrappers around replace

(defun move-subseq-left (seq from end to)
  "Move the subseq between from and end to before TO, which is assumed to be
left of FROM."
  (replace seq seq :start1 (+ to (- end from) 1) :start2 to :end2 from)
  to)

(defun move-subseq-right (seq from end to)
  "Move the subseq between from and end to before TO, which is assumed to be
right of FROM."
  (replace seq seq :start1 from :start2 (1+ end) :end2 to)
  (+ from (- to end 1)))

(defun move-subseq (seq from end to)
  "Destructively move the gap subseq starting at from and ending at
end, inclusive, to before TO."
  (if (< to from)
      (move-subseq-left seq from end to)
    (move-subseq-right seq from end to)))
  
(defun fill-gap (buf)
  "For debugging purposes. fill the gap with _'s."
  (fill (buffer-data buf) #\_ :start (buffer-gap-start buf) :end (gap-end buf)))

(defun gap-move-to (buf to)
  "A basic function to move the gap. TO is in aref coordinates and the
gap is positioned before TO.

 BUFFER: ABC__DEF
 (gap-move-to buffer 6)
 BUFFER: ABCD__EF"
  (setf (buffer-gap-start buf) 
	(move-subseq (buffer-data buf) (buffer-gap-start buf) (1- (gap-end buf)) to))
  ;; for debugging purposes, we set the gap to _'s
  (fill-gap buf))

(defun gap-move-to-point (buf)
  "Move the gap to the point position in aref space.
ABCD___EF
 ^
=>
A___BCDEF
    ^
"
  (gap-move-to buf (buffer-char-to-aref buf (marker-position (buffer-point buf)))))

;;     ;; Move the gap to the end of the vector
;;     (replace data data :start1 gap-start :start2 gap-end)
;;     ;; open a space for the gap
;;     (replace data data :start1 (+ to (buffer-gap-size buf)) :start2 to)
;;     (setf (buffer-gap-start buf) to)))

(defun gap-end (buf)
  "The end of the gap. in aref space. gap-end is the first valid
buffer character."
  (declare (type buffer buf))
  (+ (buffer-gap-start buf) (buffer-gap-size buf)))

;; (defun gap-close (buf)
;;   "Move the gap to the end of the buffer."
;;   (let ((gap-start (buffer-gap-start buf))
;; 	(gap-end (gap-end buf)))
;;     (setf (buffer-gap-start buf) (- (length (buffer-data buf)) (buffer-gap-size buf)))
;;     (replace (buffer-data buf) (buffer-data buf) :start1 gap-start :start2 gap-end)))

(defun grow-buffer-data (buf size)
  "Grow the buffer data array to be SIZE. SIZE must be larger than before."
  ;; MOVITZ doesn't have adjust-array
  ;; ;; #\_ is used for debugging to represent the gap
  ;; (adjust-array (buffer-data buf) size :initial-element #\_ :fill-pointer t)
  (let ((newbuf (make-array size :initial-element #\_;;  :fill-pointer t
			    :element-type 'character)))
    (replace newbuf (buffer-data buf))
    (setf (buffer-data buf) newbuf)))

(defun gap-extend (buf size)
  "Extend the gap by SIZE characters."
  (let ((new-size (+ (length (buffer-data buf)) size))
	(old-end (gap-end buf))
	(old-size (buffer-size buf))
	(data (buffer-data buf)))
    (setf data (grow-buffer-data buf new-size))
    (incf (buffer-gap-size buf) size)
    (unless (= (buffer-gap-start buf) old-size)
      (replace data data
	       :start1 (gap-end buf)
	       :start2 old-end))
    ;; for debugging, mark the gap
    (fill-gap buf)))

(defun buffer-size (buf)
  "Return the length of the buffer not including the gap."
  (declare (type buffer buf))
  (- (length (buffer-data buf))
     (buffer-gap-size buf)))

(defun buffer-min (buf)
  "The beginning of the buffer in char space."
  (declare (type buffer buf)
           (ignore buf))
  0)

(defun buffer-max (buf)
  "The end of the buffer in char space."
  (declare (type buffer buf))
  (buffer-size buf))

(defun begv (&optional (buf (current-buffer)))
  "Position of beginning of accessible range of buffer."
  ;; TODO: handle buffer narrowing
  (buffer-min buf))

(defun zv (&optional (buf (current-buffer)))
  "Position of end of accessible range of buffer."
  ;; TODO: handle buffer narrowing
  (buffer-max buf))

(defun point (&optional (buffer (current-buffer)))
  "Return the point in the current buffer."
  (marker-position (buffer-point buffer)))

(defun point-marker (&optional (buffer (current-buffer)))
  "Return value of point, as a marker object."
  (buffer-point buffer))

(defun point-min (&optional (buffer (current-buffer)))
  "Return the minimum permissible value of point in the current buffer."
  (declare (ignore buffer))
  0)

(defun point-max (&optional (buffer (current-buffer)))
  "Return the maximum permissible value of point in the current buffer."
  (buffer-size buffer))

(defun goto-char (position &optional (buffer (current-buffer)))
  "Set point to POSITION, a number."
  (when (and (>= position (point-min buffer))
	     (<= position (point-max buffer)))
    (setf (marker-position (buffer-point buffer)) position)))

;; (defun buffer-char-before-point (buf p)
;;   "The character at the point P in buffer BUF. P is in char space."
;;   (declare (type buffer buf)
;; 	   (type integer p))
;;   (let ((aref (buffer-char-to-aref buf p)))
;;       (when (< aref (length (buffer-data buf)))
;; 	(aref (buffer-data buf) aref))))

(defun buffer-char-after (buf p)
  "The character at the point P in buffer BUF. P is in char space."
  (declare (type buffer buf)
	   (type integer p))
  (let ((aref (buffer-char-to-aref buf p)))
      (when (and (>= aref 0)
		 (< aref (length (buffer-data buf))))
	(aref (buffer-data buf) aref))))

(defun buffer-char-before (buf p)
  (buffer-char-after buf (1- p)))

(defun char-after (&optional (pos (point)))
  "Return character in current buffer at position POS.
***POS is an integer or a marker.
***If POS is out of range, the value is nil."
  (buffer-char-after (current-buffer) pos))

(defun char-before (&optional (pos (point)))
  "Return character in current buffer preceding position POS.
***POS is an integer or a marker.
***If POS is out of range, the value is nil."
  (char-after (1- pos)))

(defun buffer-aref-to-char (buf idx)
  "Translate the index into the buffer data to the index excluding the gap."
  (declare (type buffer buf)
	   (type integer idx))
  (if (>= idx (gap-end buf))
      (- idx (buffer-gap-size buf))
    idx))

(defun buffer-char-to-aref (buf p)
  ""
  (declare (type buffer buf)
	   (type integer p))
  (if (>= p (buffer-gap-start buf))
      (+ p (buffer-gap-size buf))
    p))

(defun buffer-point-aref (buf)
  "Return the buffer point in aref coordinates."
  (buffer-char-to-aref buf (point buf)))

(defun string-to-vector (s)
  "Return a resizable vector containing the elements of the string s."
  (declare (string s))
  (make-array (length s)
	      :initial-contents s
	      :element-type 'character
	      :adjustable t))


(defgeneric buffer-insert (buffer object)
  (:documentation "Insert OBJECT into BUFFER at the current point."))

(defmethod buffer-insert :after ((buf buffer) object)
  "Any object insertion modifies the buffer."
  (declare (ignore object))
  (setf (buffer-modified buf) t))

(defmethod buffer-insert ((buf buffer) (char character))
  "Insert a single character into buffer before point."
  ;; Resize the gap if needed
  (if (<= (buffer-gap-size buf) 1)
      (gap-extend buf 100))
  ;; Move the gap to the point
  (unless (= (point buf) (buffer-gap-start buf))
    (gap-move-to buf (buffer-point-aref buf)))
  (update-markers-ins buf (point buf) 1)
  ;; set the character
  (setf (aref (buffer-data buf) (buffer-gap-start buf)) char)
  ;; move the gap forward
  (incf (buffer-gap-start buf))
  (decf (buffer-gap-size buf))
  ;; expand the buffer intervals
  (offset-intervals buf (point buf) 1))

(defmethod buffer-insert ((buf buffer) (string string))
  ;; resize
  (when (<= (buffer-gap-size buf) (length string))
    (gap-extend buf (+ (length string) 100)))
  ;; move the gap to the point
  (unless (= (point buf) (buffer-gap-start buf))
    (gap-move-to buf (buffer-point-aref buf)))
  (update-markers-ins buf (point buf) (length string))
  ;; insert chars
  (replace (buffer-data buf) string :start1 (buffer-gap-start buf))
  (incf (buffer-gap-start buf) (length string))
  (decf (buffer-gap-size buf) (length string))
  ;; expand the buffer intervals
  (offset-intervals buf (point buf) (length string)))

(defmethod buffer-insert ((buf buffer) (string pstring))
  ;; insert string
  (buffer-insert buf (pstring-data string))
  ;; insert properties
  (graft-intervals-into-buffer (intervals string) 
			       (point buf)
			       (pstring-length string)
			       buf
			       t))

(defgeneric insert-move-point (buffer object)
  (:documentation "Insert OBJECT into BUFFER at the current point. Move the point
forward by its length."))

(defmethod insert-move-point ((buffer buffer) (object character))
  (buffer-insert buffer object)
  (incf (marker-position (buffer-point buffer))))

(defmethod insert-move-point ((buffer buffer) (object string))
  (buffer-insert buffer object)
  (incf (marker-position (buffer-point buffer)) (length object)))

(defmethod insert-move-point ((buffer buffer) (object pstring))
  (buffer-insert buffer object)
  (incf (marker-position (buffer-point buffer)) (pstring-length object)))

(defun insert (&rest objects)
  "Insert the arguments, either strings or characters, at point.
Point and before-insertion markers move forward to end up after the
inserted text. Any other markers at the point of insertion remain
before the text."
  (dolist (o objects)
    (insert-move-point (current-buffer) o)))

(defun buffer-delete (buf p length)
  "Deletes chars from point to point + n. If N is negative, deletes backwards."
  (cond ((< length 0)
	 (gap-move-to buf (buffer-char-to-aref buf p))
	 (let* ((new (max 0 (+ (buffer-gap-start buf) length)))
		(capped-size (- (buffer-gap-start buf) new)))
	   (update-markers-del buf new capped-size)
           (adjust-intervals-for-deletion buf new capped-size)
	   (incf (buffer-gap-size buf) capped-size)
	   (setf (buffer-gap-start buf) new)))
	 ((> length 0)
	  (unless (>= p (zv buf))
	    ;; can't delete forward if we're at the end of the buffer.
	    (gap-move-to buf (buffer-char-to-aref buf p))
	    ;; Make sure the gap size doesn't grow beyond the buffer size.
	    (let ((capped-size (- (min (+ (gap-end buf) length)
				       (length (buffer-data buf)))
				  (gap-end buf))))
	      (incf (buffer-gap-size buf) capped-size)
	      (update-markers-del buf p capped-size)
              (adjust-intervals-for-deletion buf p capped-size)))))
  (setf (buffer-modified buf) t)
  ;; debuggning
  (fill-gap buf))

(defun buffer-erase (&optional (buf (current-buffer)))
  ;; update properties
  (adjust-intervals-for-deletion buf 0 (buffer-size buf))
  (update-markers-del buf 0 (buffer-size buf))
  ;; expand the gap to take up the whole buffer
  (setf (buffer-gap-start buf) 0
	(buffer-gap-size buf) (length (buffer-data buf))
	(marker-position (buffer-point buf)) 0
	(buffer-modified buf) t)
  ;; debugging
  (fill-gap buf))

(defun buffer-scan-newline (buf start limit count)
  "Search BUF for COUNT newlines with a limiting point at LIMIT,
starting at START. Returns the point of the last newline or limit and
number of newlines found. START and LIMIT are inclusive."
  (declare (type buffer buf)
	   (type integer start limit count))
  (labels ((buffer-scan-bk (buf start limit count)
	     "count is always >=0. start >= limit."
	     (let* ((start-aref (buffer-char-to-aref buf start))
		    (limit-aref (buffer-char-to-aref buf limit))
		    (ceiling (if (>= start-aref (gap-end buf))
				 (gap-end buf)
			       limit-aref))
		    (i 0)
		    ;; :END is not inclusive but START is.
		    (start (1+ start-aref))
		    p)
	       (loop
		;; Always search at least once
		(setf p (position #\Newline (buffer-data buf) 
				  :start ceiling :end start :from-end t))
		(if p
		    (progn
		      ;; Move start. Note that start isn't set to (1+ p)
		      ;; because we don't want to search p again.
		      (setf start p)
		      ;; Count the newline
		      (incf i)
		      ;; Have we found enough newlines?
		      (when (>= i count)
			(return-from buffer-scan-bk (values (buffer-aref-to-char buf p)
							    i))))
		  ;; Check if we've searched up to the limit
		  (if (= ceiling limit-aref)
		      (return-from buffer-scan-bk (values limit i))
		    ;; if not, skip past the gap
		    (progn
		      (setf ceiling limit-aref)
		      (setf start (buffer-gap-start buf))))))))
	   (buffer-scan-fw (buf start limit count)
	     "count is always >=0. start >= limit."
	     (let* ((start-aref (buffer-char-to-aref buf start))
		    (limit-aref (1+ (buffer-char-to-aref buf limit)))
		    (ceiling (if (< start (buffer-gap-start buf))
				 (buffer-gap-start buf)
			       limit-aref))
		    (i 0)
		    (start start-aref)
		    p)
	       (loop
		;; Always search at least once
		(setf p (position #\Newline (buffer-data buf) :start start :end ceiling))
		(if p
		    (progn
		      ;; Move start. We don't want to search p again, thus the 1+.
		      (setf start (1+ p))
		      ;; Count the newline
		      (incf i)
		      ;; Have we found enough newlines?
		      (when (>= i count)
			(return-from buffer-scan-fw (values (buffer-aref-to-char buf p)
							    i))))
		  ;; Check if we've searched up to the limit
		  (if (= ceiling limit-aref)
		      (return-from buffer-scan-fw (values limit i))
		    ;; if not, skip past the gap
		    (progn
		      (setf ceiling limit-aref)
		      (setf start (gap-end buf)))))))))
    ;; make sure start and limit are within the bounds
    (setf start (max 0 (min start (1- (buffer-size buf))))
	  limit (max 0 (min limit (1- (buffer-size buf)))))
    ;; the search always fails on an empty buffer
    (when (= (buffer-size buf) 0)
      (return-from buffer-scan-newline (values limit 0)))
    (cond ((> count 0)
	   (dformat +debug-vv+ "scan-fw ~a ~a ~a~%" start limit count)
	   (buffer-scan-fw buf start limit count))
	  ((< count 0)
	   (dformat +debug-vv+ "scan-bk ~a ~a ~a~%" start limit count)
	   (buffer-scan-bk buf start limit (abs count)))
	  ;; 0 means the newline before the beginning of the current
	  ;; line. We need to handle the case where we are on a newline.
	  (t 
	   (dformat +debug-vv+ "scan-0 ~a ~a ~a~%" start limit count)
	   (if (char= (buffer-char-after buf start) #\Newline)
	       (buffer-scan-bk buf start limit 2)
	     (buffer-scan-bk buf start limit 1))))))

;; ;;; more stuff

;; (defparameter +scratch-buffer+ ";; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; ;; If you want to create a file, visit that file with C-x C-f,
;; ;; then enter the text in that file's own buffer.")

;; (defparameter +other-buf+
;; "678901234567890 abcdefghijklmnopqrstuvwxyz
;; 1 abcdefghijklmnopqrstuvwxyz
;; 2 abcdefghijklmnopqrstuvwxyz
;; 3 abcdefghijklmnopqrstuvwxyz
;; 4 abcdefghijklmnopqrstuvwxyz
;; 5 abcdefghijklmnopqrstuvwxyz
;; 6 abcdefghijklmnopqrstuvwxyz
;; 7 abcdefghijklmnopqrstuvwxyz
;; 8 abcdefghijklmnopqrstuvwxyz")

;; (defun buffer-read-from-stream (buffer stream)
;;   "Read the contents of stream until EOF, putting it in buffer-data"
;;   (loop for c = (read-char stream nil nil)
;; 	until (null c)
;; 	do (vector-push-extend c (buffer-data buffer))))

;; (defun buffer-read-from-file (buffer file)
;;   (with-open-file (s file :direction :input)
;;     (buffer-read-from-stream buffer s)))

;;; Mode-Line stuff

;; FIXME: this is a parameter for debugging
;; FIXME: be more emacs-like or make it better so we don't just have
;; lambda functions that process data and return a string.
(defparameter *mode-line-format* (list "--:" ;; fake it for hype
				       (lambda (buffer)
					 (format nil "~C~C"
					      ;; FIXME: add read-only stuff
					      (if (buffer-modified buffer)
						  #\* #\-)
					      (if (buffer-modified buffer)
						  #\* #\-)))
				       "  "
				       (lambda (buffer)
					 (format nil "~12,,,a" (buffer-name buffer)))
				       "   "
				       (lambda (buffer)
					 (format nil "(~a)" 
					      (major-mode-name (buffer-major-mode buffer)))))
  "The default mode line format.")

(defgeneric mode-line-format-elem (buffer elem)
  (:documentation "Given the element found in the buffer mode-line,
return a string that will be printed in the mode-line."))

(defmethod mode-line-format-elem ((b buffer) (elem string))
  "just return the string."
  (declare (ignore b))
  elem)

(defmethod mode-line-format-elem ((b buffer) (elem function))
  "Call the function. It is expected to return a string."
  (funcall elem b))

(defmethod mode-line-format-elem ((b buffer) (elem symbol))
  "elem is a symbol, so print its value."
  (princ "~a" elem))

(defun update-mode-line (buffer)
  "Given the buffer, refresh its mode-line string."
  (setf (buffer-mode-line-string buffer) 
	(format nil "~{~a~}" (mapcar (lambda (elem)
				(mode-line-format-elem buffer elem))
			      (buffer-mode-line buffer)))))

(defun truncate-mode-line (buffer len)
"return the buffers mode-line trunctated to len. If the mode-line is
shorter than len, it will be padded with -'s."
  (let ((s (make-array len :element-type 'character :initial-element #\-)))
    (replace s (buffer-mode-line-string buffer))))

;;; Buffer query/creation

(defgeneric get-buffer (name)
  (:documentation "Return the buffer named NAME. If there is no live
buffer named NAME, return NIL."))

(defmethod get-buffer ((name string))
  (find name *buffer-list* :key #'buffer-name :test #'string=))

(defmethod get-buffer ((buffer buffer))
  (find buffer *buffer-list*))

(defgeneric get-buffer-create (name)
  (:documentation "Return the buffer named NAME, or create such a buffer and return it.
A new buffer is created if there is no live buffer named NAME.
If NAME starts with a space, the new buffer does not keep undo information.
If NAME is a buffer instead of a string, then it is the value returned.
The value is never nil."))

(defmethod get-buffer-create ((name string))
  (or 
     (get-buffer name)
     (progn
       (when (zerop (length name))
	 (error "Empty string for buffer name is not allowed"))
       (let ((b (make-instance 'buffer
			       :file nil
			       :point (make-marker)
			       :mark (make-marker)
			       ;; Currently a buffer has to have a gap
			       ;; of at least size 1.
			       :data (string-to-vector "_")
			       :gap-start 0
			       :gap-size 1
			       :mode-line *mode-line-format*
			       :name name
			       :major-mode fundamental-mode)))
	 (set-marker (buffer-point b) 0 b)
	 (set-marker (mark-marker b) 0 b)
	 (push b *buffer-list*)
	 b))))

(defmethod get-buffer-create ((buffer buffer))
  buffer)

;;; 

(defun make-default-buffers ()
  "Called on startup. Create the default buffers, putting them in
*buffer-list*."
  ;; for the side effect
  (get-buffer-create "*messages*")
  (let ((scratch (get-buffer-create "*scratch*")))
    (buffer-insert scratch ";; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.")
    ;; FIXME: is this a hack?
    (setf (buffer-modified scratch) nil)
    (goto-char (point-min scratch) scratch)))

;;;

(defun generate-new-buffer-name (name &optional ignore)
  "Return a string that is the name of no existing buffer based on NAME.
If there is no live buffer named NAME, then return NAME.
Otherwise modify name by appending `<NUMBER>', incrementing NUMBER
until an unused name is found, and then return that name.
Optional second argument IGNORE specifies a name that is okay to use
 (if it is in the sequence to be tried)
even if a buffer with that name exists."
  (declare (type string name)
	   (type (or string null) ignore))
  (or (unless (get-buffer name)
	name)
      (loop for count from 1
	    ;; FIXME: there's gotta be a way to do this where s isn't
	    ;; "" to start with.
	    with s = ""
	    do (setf s (format nil "~a<~d>" name count))
	    when (and ignore 
		      (string= s ignore))
	    return ignore
	    unless (get-buffer s)
	    return s)))

(defmacro with-current-buffer (buffer &body body)
  "Execute the forms in BODY with BUFFER as the current buffer.
The value returned is the value of the last form in BODY.
See also `with-temp-buffer'."
  (let ((bk (gensym "BK")))
    `(progn
       (let ((,bk *current-buffer*))
	 (set-buffer ,buffer)
	 (unwind-protect
	      (progn ,@body)
	   (set-buffer ,bk))))))

(defmacro with-temp-buffer (&body body)
  "Create a temporary buffer, and evaluate BODY there like `progn'.
See also `with-temp-file'."
  (let ((temp-buffer (gensym "TEMP-BUFFER")))
    `(let ((,temp-buffer (get-buffer-create (generate-new-buffer-name "*temp*"))))
       (unwind-protect
	   (with-current-buffer ,temp-buffer
	     ,@body)
	 (and (get-buffer ,temp-buffer)
	      (kill-buffer ,temp-buffer))))))

(defun bring-buffer-to-front (buf)
  "Put buf at the front of *buffer-list*. Assumes BUF is in
*buffer-list*."
  (setf *buffer-list* (delete buf *buffer-list*))
  (push buf *buffer-list*))

(defun other-buffer (&optional (buffer (current-buffer)) visible-ok frame)
  "Return most recently selected buffer other than BUFFER.
Buffers not visible in windows are preferred to visible buffers,
unless optional second argument VISIBLE-OK is non-nil.
If the optional third argument FRAME is non-nil, use that frame's
buffer list instead of the selected frame's buffer list.
If no other buffer exists, the buffer `*scratch*' is returned.
If BUFFER is omitted or nil, some interesting buffer is returned."
  (declare (ignore frame))
  ;; TODO: honour FRAME argument
  (let* (vis
         (match (loop for b in *buffer-list*
                      unless (or (eq b buffer)
                                 (char= (char (buffer-name b) 0) #\Space))
		      if (and (not visible-ok)
			      (get-buffer-window b))
		      do (setf vis b)
		      else return b)))
    (or match
        vis
	(get-buffer-create "*scratch*"))))

(defun mark (&optional force (buffer (current-buffer)))
  "Return BUFFER's mark value as integer; error if mark inactive.
If optional argument FORCE is non-nil, access the mark value
even if the mark is not currently active, and return nil
if there is no mark at all."
  (declare (ignore force))
  ;; FIXME: marks can't be inactive ATM
  (marker-position (mark-marker buffer)))

(defun validate-region (start end &optional (buffer (current-buffer)))
  "Return a value pair of start and end for buffer. the 1st value
returned will always be <= the second. May raise an args out of range
error.

If START or END are marks, their positions will be used."
  (when (typep start 'marker)
    (setf start (marker-position start)))
  (when (typep end 'marker)
    (setf end (marker-position end)))
  (when (< end start)
    ;; MOVITZ doesn't have psetf
    (let ((tmp start))
      (setf start end
	    end tmp))
    ;; (psetf end start
    ;; 	   start end)
    )
  (when (or (< start (buffer-min buffer))
	    (> end (buffer-max buffer)))
    (signal 'args-out-of-range))
  (values start end))

(defun eobp (&optional (buffer (current-buffer)))
  "Return T when the point is at the end of the buffer."
  (= (buffer-max buffer) (point)))

(defun bobp (&optional (buffer (current-buffer)))
  "Return T when the point is at the beginning of the buffer."
  (= (buffer-min buffer) (point)))

(defun set-buffer (buffer)
  "Make the buffer BUFFER current for editing operations.
BUFFER may be a buffer or the name of an existing buffer.
See also `save-excursion' when you want to make a buffer current temporarily.
This function does not display the buffer, so its effect ends
when the current command terminates.
Use `switch-to-buffer' or `pop-to-buffer' to switch buffers permanently."
  (setf buffer (get-buffer buffer))
  (if buffer
      (progn
	(when *current-buffer*
	  (record-local-variables *current-buffer*))
	(set-local-variables buffer)
	(setf *current-buffer* buffer))
      (error "No buffer named ~s" buffer)))

(defun record-buffer (buffer)
  "**Move the assoc for buffer BUF to the front of buffer-alist.  
Since we do this each time BUF is selected visibly, the more recently
selected buffers are always closer to the front of the list.  This
means that other_buffer is more likely to choose a relevant buffer."
  (setf *buffer-list* (delete buffer *buffer-list* :test #'eq))
  (push buffer *buffer-list*))

(defun barf-if-buffer-read-only ()
  "Signal a `buffer-read-only' error if the current buffer is read-only."
  (when (buffer-read-only (current-buffer))
    (signal 'buffer-read-only)))

(defun bufferp (object)
  "Return t if object is an editor buffer."
  (typep object 'buffer))

(define-buffer-local :default-directory (truename "")
  "Name of default directory of current buffer.
To interactively change the default directory, use command `cd'.")

(defstruct local-variable-binding
  value backup)

(defun make-local-variable (symbol)
  "Make variable have a separate value in the current buffer.
Other buffers will continue to share a common default value.
 (The buffer-local value of variable starts out as the same value
variable previously had.)
Return variable."
  (setf (gethash symbol (buffer-local-variables (current-buffer))) 
	(make-local-variable-binding :value (symbol-value symbol)))
  symbol)

(defun record-local-variables (buffer)
  "Update the values BUFFER's local variables."
  (labels ((update (k v)
	     (if (boundp k)
		 (setf (local-variable-binding-value v) (symbol-value k)
		       (symbol-value k) (local-variable-binding-backup v))
		 (remhash k (buffer-local-variables buffer)))))
    (maphash #'update (buffer-local-variables buffer))))

(defun set-local-variables (buffer)
  "Set all variables to the buffer local value."
  (labels ((set-it (k v)
	     (if (boundp k)
		 (setf (local-variable-binding-backup v) (symbol-value k)
		       (symbol-value k) (local-variable-binding-value v))
		 (remhash k (buffer-local-variables buffer)))))
    (maphash #'set-it (buffer-local-variables buffer))))

(provide :lice-0.1/buffer)
