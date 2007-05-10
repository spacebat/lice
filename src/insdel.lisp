;;; buffer inserting, deleting, gap management, etc

(in-package "LICE")

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

;; (defun buffer-char-before-point (buf p)
;;   "The character at the point P in buffer BUF. P is in char space."
;;   (declare (type buffer buf)
;; 	   (type integer p))
;;   (let ((aref (buffer-char-to-aref buf p)))
;;       (when (< aref (length (buffer-data buf)))
;; 	(aref (buffer-data buf) aref))))

(defgeneric buffer-insert (buffer object)
  (:documentation "Insert OBJECT into BUFFER at the current point."))

(defmethod buffer-insert :after ((buf buffer) object)
  "Any object insertion modifies the buffer."
  (declare (ignore object))
  (setf (buffer-modified-p buf) t))

(defmethod buffer-insert ((buf buffer) (char character))
  "Insert a single character into buffer before point."
  ;; Resize the gap if needed
  (if (<= (buffer-gap-size buf) 1)
      (gap-extend buf 100))
  ;; Move the gap to the point
  (unless (= (pt buf) (buffer-gap-start buf))
    (gap-move-to buf (buffer-point-aref buf)))
  (update-markers-ins buf (pt buf) 1)
  ;; undo
  (record-insert (pt buf) 1 buf)
  ;; set the character
  (setf (aref (buffer-data buf) (buffer-gap-start buf)) char)
  ;; move the gap forward
  (incf (buffer-gap-start buf))
  (decf (buffer-gap-size buf))
  ;; expand the buffer intervals
  (offset-intervals buf (pt buf) 1))

(defmethod buffer-insert ((buf buffer) (string string))
  ;; resize
  (when (<= (buffer-gap-size buf) (length string))
    (gap-extend buf (+ (length string) 100)))
  ;; move the gap to the point
  (unless (= (pt buf) (buffer-gap-start buf))
    (gap-move-to buf (buffer-point-aref buf)))
  (update-markers-ins buf (pt buf) (length string))
  ;; undo
  (record-insert (pt buf) (length string) buf)
  ;; insert chars
  (replace (buffer-data buf) string :start1 (buffer-gap-start buf))
  (incf (buffer-gap-start buf) (length string))
  (decf (buffer-gap-size buf) (length string))
  ;; expand the buffer intervals
  (offset-intervals buf (pt buf) (length string)))

(defmethod buffer-insert ((buf buffer) (string pstring))
  ;; insert string
  (buffer-insert buf (pstring-data string))
  ;; insert properties
  (graft-intervals-into-buffer (intervals string) 
			       (pt buf)
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

(defun buffer-delete (buf p length)
  "Deletes chars from point to point + n. If N is negative, deletes backwards."
  (cond ((< length 0)
	 (gap-move-to buf (buffer-char-to-aref buf p))
	 (let* ((new (max 0 (+ (buffer-gap-start buf) length)))
		(capped-size (- (buffer-gap-start buf) new)))
	   (update-markers-del buf new capped-size)
           (record-delete new (make-buffer-string new (+ new capped-size) t buf))
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
              (record-delete p (make-buffer-string p (+ p capped-size) t buf))
	      (incf (buffer-gap-size buf) capped-size)
	      (update-markers-del buf p capped-size)
              (adjust-intervals-for-deletion buf p capped-size)))))
  (setf (buffer-modified-p buf) t)
  ;; debuggning
  (fill-gap buf))

(defun buffer-erase (&optional (buf (current-buffer)))
  ;; update properties
  (record-delete (begv buf) (make-buffer-string (begv buf) (zv buf) t buf) buf)
  (adjust-intervals-for-deletion buf 0 (buffer-size buf))
  (update-markers-del buf 0 (buffer-size buf))
  ;; expand the gap to take up the whole buffer
  (setf (buffer-gap-start buf) 0
	(buffer-gap-size buf) (length (buffer-data buf))
	(marker-position (buffer-point buf)) 0
	(buffer-modified-p buf) t)
  ;; debugging
  (fill-gap buf))

(defcommand erase-buffer ((&optional (buffer (current-buffer))))
"Delete the entire contents of the current buffer.
Any narrowing restriction in effect (see `narrow-to-region') is removed,
so the buffer is truly empty after this."
  (buffer-erase buffer))
