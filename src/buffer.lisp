(declaim (optimize (debug 3)))

(in-package :lice)

(defconstant +beg+ 0)

(defvar *buffer-list* nil
  "All buffers managed by lice. buffers are sorted from most recently
accessed to least. the CAR is the most recent buffer.")

(defvar *inhibit-read-only* nil
"*Non-nil means disregard read-only status of buffers or characters.
If the value is t, disregard `buffer-read-only' and all `read-only'
text properties.  If the value is a list, disregard `buffer-read-only'
and disregard a `read-only' text property if the property value
is a member of the list.")

(defvar *default-major-mode* 'fundamental-mode
  "*Major mode for new buffers.  Defaults to `fundamental-mode'.
A value of nil means use current buffer's major mode,
provided it is not marked as \"special\".

When a mode is used by default, `find-file' switches to it
before it reads the contents into the buffer and before
it finishes setting up the buffer.  Thus, the mode and
its hooks should not expect certain variables such as
`buffer-read-only' and `buffer-file-coding-system' to be set up.")

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


;;; gap basics

;; Some wrappers around replace
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
  
(defun gap-end (buf)
  "The end of the gap. in aref space. gap-end is the first valid
buffer character."
  (declare (type buffer buf))
  (+ (buffer-gap-start buf) (buffer-gap-size buf)))

(defun fill-gap (buf)
  "For debugging purposes. fill the gap with _'s."
  (fill (buffer-data buf) #\_ :start (buffer-gap-start buf) :end (gap-end buf)))

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

(defun begv-aref (&optional (buf (current-buffer)))
  "aref Position of beginning of accessible range of buffer."
  ;; TODO: handle buffer narrowing
  (buffer-char-to-aref buf (buffer-min buf)))

(defun zv (&optional (buf (current-buffer)))
  "Position of end of accessible range of buffer."
  ;; TODO: handle buffer narrowing
  (buffer-max buf))

(defun zv-aref (&optional (buf (current-buffer)))
  "aref Position of end of accessible range of buffer."
  ;; TODO: handle buffer narrowing
  (buffer-char-to-aref buf (buffer-max buf)))

(defmacro inc-aref (var buffer)
  "increment VAR one character forward in BUFFER, avoiding the gap."
  `(progn
     (incf ,var)
     (if (= (buffer-gap-start ,buffer) ,var)
         (setf ,var (gap-end ,buffer)))))

(defmacro inc-both (char-var aref-var buffer)
  `(progn
     (inc-aref ,aref-var ,buffer)
     (incf ,char-var)))

(defun aref-minus-1 (aref buffer)
  (if (= (gap-end buffer) aref)
      (1- (buffer-gap-start buffer))
      (1- aref)))

(defmacro dec-aref (var buffer)
  "increment VAR one character forward in BUFFER, avoiding the gap."
  `(setf ,var (aref-minus-1 ,var ,buffer)))

(defmacro dec-both (char-var aref-var buffer)
  `(progn
     (dec-aref ,aref-var ,buffer)
     (decf ,char-var)))

(defun pt (&optional (buffer (current-buffer)))
  "Return the point in the current buffer."
  (marker-position (buffer-point buffer)))

(defun buffer-point-aref (buf)
  "Return the buffer point in aref coordinates."
  (buffer-char-to-aref buf (pt buf)))

(defun set-point-both (buffer char-pos aref-pos)
  "Set point in BUFFER to CHARPOS, which corresponds to byte
position BYTEPOS.  If the target position is
before an intangible character, move to an ok place."
  (declare (ignore aref-pos))
  ;; TODO: implement
  (setf (marker-position (buffer-point buffer)) char-pos))

(defun set-point (char-pos &optional (buffer (current-buffer)))
  (set-point-both buffer char-pos nil))

(defun buffer-char-after (buf p)
  "The character at the point P in buffer BUF. P is in char space."
  (declare (type buffer buf)
	   (type (integer 0 *) p))
  (let ((aref (buffer-char-to-aref buf p)))
      (when (and (>= aref 0)
		 (< aref (length (buffer-data buf))))
	(aref (buffer-data buf) aref))))

(defun buffer-char-before (buf p)
  (buffer-char-after buf (1- p)))


(defun buffer-fetch-char (aref buf)
  (aref (buffer-data buf) aref))


;;; Markers

(defgeneric ensure-number (thing)
  (:documentation "Call this function when THING could be a number or a marker or...?"))

(defmethod ensure-number ((thing number))
  thing)

(defmethod ensure-number ((thing marker))
  (marker-position thing))

(defmacro check-number-coerce-marker (marker-var)
  "Verify that MARKER-VAR is a number or if it's a marker then
set the var to the marker's position."
  `(progn
     (check-type ,marker-var (or marker (integer 0 *)))
     (when (typep ,marker-var 'marker)
       (setf ,marker-var (marker-position ,marker-var)))))

(defun make-marker ()
  "Return a newly allocated marker which does not point anywhere."
  (make-instance 'marker))

(defun unchain-marker (marker)
  (when (marker-buffer marker)
    (setf (buffer-markers (marker-buffer marker))
          (delete marker (buffer-markers (marker-buffer marker)) :key #'weak-pointer-value))))

(defun chain-marker (marker buffer)
  (push (make-weak-pointer marker) (buffer-markers buffer)))  

(defun set-marker (marker position &optional (buffer (current-buffer)))
  ;; remove the marker from its buffer, when appropriate
  (when (null position)
    (unchain-marker marker)
    (return-from set-marker marker))
  ;; XXX handle dead buffers

  ;; normalize pos
  (setf (marker-position marker) (min (max position (begv buffer)) (zv buffer)))

  ;; update buffer stuff
  (unless (eq (marker-buffer marker) buffer)
    (unchain-marker marker)
    (setf (marker-buffer marker) buffer)
    (chain-marker marker buffer))
  marker)

(defun copy-marker (marker &optional (type :after))
  "Return a new marker pointing at the same place as MARKER.
If argument is a number, makes a new marker pointing
at that position in the current buffer.
**The optional argument TYPE specifies the insertion type of the new marker;
**see `marker-insertion-type'."
  (check-type marker (or marker integer))
  (check-type type marker-insertion-type)
  (let ((new (make-marker)))
    (set-marker new (ensure-number marker)
                (if (typep marker 'marker)
                    (marker-buffer marker)
                    (current-buffer)))
    (setf (marker-insertion-type new) type)
    new))

(defun purge-markers (buffer)
  "Remove GC'd markers."
  (setf (buffer-markers buffer)
	(delete-if (lambda (m)
		     (multiple-value-bind (v c) (weak-pointer-value m)
                       (declare (ignore v))
		       (not c)))
		   (buffer-markers buffer))))

(defun update-markers-del (buffer start size)
  ;; FIXME: insertion-type
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
  ;; FIXME: insertion-type
  ;; First get rid of stale markers
  (purge-markers buffer)
  (dolist (wp (buffer-markers buffer))
    (let ((m (weak-pointer-value wp)))
      ;; markers are before the marker-position.
      (when (and m (> (marker-position m) start)
		 (incf (marker-position m) size))))))


;;;

(defun inc-buffer-tick (buffer)
  "Increment the buffer's ticker."
  (incf (buffer-modified-tick buffer)))

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
(defvar *default-mode-line-format* nil
  "Default value of `mode-line-format' for buffers that don't override it.
This is the same as (default-value 'mode-line-format).")

(define-buffer-local *mode-line-format* nil
  "The buffer's mode line format.")
(make-variable-buffer-local '*mode-line-format*)

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
			      (buffer-local '*mode-line-format* buffer)))))

(defun truncate-mode-line (buffer len)
"return the buffers mode-line trunctated to len. If the mode-line is
shorter than len, it will be padded with -'s."
  (let ((s (make-array len :element-type 'character :initial-element #\-)))
    (replace s (buffer-mode-line-string buffer))))

;;; Buffer query/creation

(defun string-to-vector (s)
  "Return a resizable vector containing the elements of the string s."
  (declare (string s))
  (make-array (length s)
	      :initial-contents s
	      :element-type 'character
	      :adjustable t))

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
                               :major-mode '*fundamental-mode*
			       :gap-start 0
			       :gap-size 1
			       :name name)))
	 (set-marker (buffer-point b) 0 b)
	 (set-marker (mark-marker b) 0 b)
         (setf (buffer-local '*mode-line-format* b) *default-mode-line-format*)
	 (push b *buffer-list*)
	 b))))

(defmethod get-buffer-create ((buffer buffer))
  buffer)

;;; 

(defparameter *initial-scratch-message* ";; This buffer is for notes you don't want to save, and for Lisp evaluation.
;; If you want to create a file, visit that file with C-x C-f,
;; then enter the text in that file's own buffer.")

(defun make-default-buffers ()
  "Called on startup. Create the default buffers, putting them in
*buffer-list*."
  ;; for the side effect
  (let ((msg (get-buffer-create "*messages*")))
    (setf (buffer-undo-list msg) t))
  (get-buffer-create "*scratch*"))

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
	;; (when *current-buffer*
        ;; (record-local-variables *current-buffer*))
	;; (set-local-variables buffer)
	(setf *current-buffer* buffer))
      (error "No buffer named ~s" buffer)))

(defun record-buffer (buffer)
  "**Move the assoc for buffer BUF to the front of buffer-alist.  
Since we do this each time BUF is selected visibly, the more recently
selected buffers are always closer to the front of the list.  This
means that other_buffer is more likely to choose a relevant buffer."
  (setf *buffer-list* (delete buffer *buffer-list* :test #'eq))
  (push buffer *buffer-list*))

(defun buffer-read-only ()
"Non-nil if this buffer is read-only."
  (slot-value (current-buffer) 'read-only))

(defun (setf buffer-read-only) (value)
  (setf (slot-value (current-buffer) 'read-only) (and value t)))

(defun barf-if-buffer-read-only ()
  "Signal a `buffer-read-only' error if the current buffer is read-only."
  (when (buffer-read-only)
    (signal 'buffer-read-only)))

(defun buffer-modified-p (&optional (buffer (current-buffer)))
  "Return t if BUFFER was modified since its file was last read or saved.
No argument or nil as argument means use current buffer as BUFFER."
  (slot-value buffer 'modified-p))

(defun (setf buffer-modified-p) (flag &optional (buf (current-buffer)))
  "Mark current buffer as modified or unmodified according to FLAG.
A non-nil FLAG means mark the buffer modified."
  (setf (slot-value buf 'modified-p) (and flag t)))

(defun set-buffer-modified-p (flag)
  "Mark current buffer as modified or unmodified according to FLAG.
A non-nil FLAG means mark the buffer modified."
  (setf (buffer-modified-p) flag))

(defun bufferp (object)
  "Return t if object is an editor buffer."
  (typep object 'buffer))

(define-buffer-local *default-directory* (truename "")
  "Name of default directory of current buffer.
To interactively change the default directory, use command `cd'.")

;; (defstruct local-variable-binding
;;   value backup)

;; (defun make-local-variable (symbol)
;;   "Make variable have a separate value in the current buffer.
;; Other buffers will continue to share a common default value.
;;  (The buffer-local value of variable starts out as the same value
;; variable previously had.)
;; Return variable."
;;   (setf (gethash symbol (buffer-local-variables (current-buffer))) 
;; 	(make-local-variable-binding :value (symbol-value symbol)))
;;   symbol)

;; (defun record-local-variables (buffer)
;;   "Update the values BUFFER's local variables."
;;   (labels ((update (k v)
;; 	     (if (boundp k)
;; 		 (setf (local-variable-binding-value v) (symbol-value k)
;; 		       (symbol-value k) (local-variable-binding-backup v))
;; 		 (remhash k (buffer-local-variables buffer)))))
;;     (maphash #'update (buffer-local-variables buffer))))

;; (defun set-local-variables (buffer)
;;   "Set all variables to the buffer local value."
;;   (labels ((set-it (k v)
;; 	     (if (boundp k)
;; 		 (setf (local-variable-binding-backup v) (symbol-value k)
;; 		       (symbol-value k) (local-variable-binding-value v))
;; 		 (remhash k (buffer-local-variables buffer)))))
;;     (maphash #'set-it (buffer-local-variables buffer))))

(defun major-mode ()
  (symbol-value (buffer-major-mode (current-buffer))))

(define-buffer-local *fill-column* 70
"*Column beyond which automatic line-wrapping should happen.
Interactively, you can set the buffer local value using \\[set-fill-column].")

(defun buffer-list (&optional frame)
  "Return a list of all existing live buffers.
If the optional arg frame is a frame, we return the buffer list
in the proper order for that frame: the buffers in FRAME's `buffer-list'
frame parameter come first, followed by the rest of the buffers."
  ;; FIXME: handle frame
  (declare (ignore frame))
  *buffer-list*)

(define-buffer-local *auto-fill-function* nil
  "Function called (if non-nil) to perform auto-fill.
It is called after self-inserting any character specified in
the `auto-fill-chars' table.
NOTE: This variable is not a hook;
its value may not be a list of functions.")
(make-variable-buffer-local '*auto-fill-function*)

(define-buffer-local mark-active nil
  "Non-nil means the mark and region are currently active in this buffer.")
(make-variable-buffer-local 'mark-active)

(define-buffer-local tab-width 8
  "*Distance between tab stops (for display of tab characters), in columns.")
(make-variable-buffer-local 'tab-width)

(define-buffer-local left-margin 0
  "*Column for the default indent-line-function to indent to.
Linefeed indents to this column in Fundamental mode.")
(make-variable-buffer-local 'left-margin)

(define-buffer-local truncate-lines nil
  "*Non-nil means do not display continuation lines.
Instead, give each line of text just one screen line.

Note that this is overridden by the variable
`truncate-partial-width-windows' if that variable is non-nil
and this buffer is not full-frame width.")
(make-variable-buffer-local 'truncate-lines)

(define-buffer-local case-fold-search nil
  "*Non-nil if searches and matches should ignore case.")
(make-variable-buffer-local 'case-fold-search)

(defun make-buffer-string (start end props &optional (buffer (current-buffer)))
  "Making strings from buffer contents.

Return a Lisp_String containing the text of the current buffer from
START to END. If text properties are in use and the current buffer has
properties in the range specified, the resulting string will also have
them, if PROPS is nonzero.

We don't want to use plain old make_string here, because it calls
make_uninit_string, which can cause the buffer arena to be
compacted.  make_string has no way of knowing that the data has
been moved, and thus copies the wrong data into the string.  This
doesn't effect most of the other users of make_string, so it should
be left as is.  But we should use this function when conjuring
buffer substrings."
  (declare (ignore props))
  ;; If the gap intersects with the range we wanna grab, move it.
  (if (= start end)
      ""
    (progn
      (when (and (< start (buffer-gap-start buffer))
		 (< (buffer-gap-start buffer) end))
	(gap-move-to buffer start))
      (dformat +debug-v+ "substring: ~a ~a ~a~%" start end (length (buffer-data buffer)))
      (subseq (buffer-data buffer)
	      (buffer-char-to-aref buffer start)
	      (1+ (buffer-char-to-aref buffer (1- end)))))))

;;; Key bindings

(define-key *ctl-x-map* "b" 'switch-to-buffer)
(define-key *ctl-x-map* "k" 'kill-buffer)

(provide :lice-0.1/buffer)
