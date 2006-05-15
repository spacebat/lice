(in-package :lice)

(defun get-pos-property (position prop &optional (object (current-buffer)))
  "Return the value of property PROP, in OBJECT at POSITION.
It's the value of PROP that a char inserted at POSITION would get.
OBJECT is optional and defaults to the current buffer.
If OBJECT is a buffer, then overlay properties are considered as well as
text properties.
If OBJECT is a window, then that window's buffer is used, but
window-specific overlays are considered only if they are associated
with OBJECT."
  (when (typep object 'window)
    (setf object (window-buffer object)))
  (if (not (typep object 'buffer))
      (get-text-property position prop object)
    ;;; XXX: handle overlays.
    (let ((stickiness (text-property-stickiness prop position object)))
      (cond
       ((eq stickiness 'after)
	(get-text-property position prop object))
       ((eq stickiness 'before)
	(get-text-property (1- position) prop object))
       (t nil)))))

(defun find-field (pos merge-at-boundary &key beg-limit beg end-limit end (buf (current-buffer)))
  "Find the field surrounding POS and return the beginning and end of
the field in a values list.  If POS is nil, the value of point is used
instead. If BEG or END is nil then that boundary isn't calculated. 

BEG_LIMIT and END_LIMIT serve to limit the ranged of the returned
results; they do not effect boundary behavior.

If MERGE_AT_BOUNDARY is nonzero, then if POS is at the very first
position of a field, then the beginning of the previous field is
returned instead of the beginning of POS's field (since the end of a
field is actually also the beginning of the next input field, this
behavior is sometimes useful).  Additionally in the MERGE_AT_BOUNDARY
true case, if two fields are separated by a field with the special
value `boundary', and POS lies within it, then the two separated
fields are considered to be adjacent, and POS between them, when
finding the beginning and ending of the \"merged\" field.

Either BEG or END may be 0, in which case the corresponding value
is not stored."
  (let ((at-field-start nil)
	(at-field-end nil)
	before-field after-field)
    (unless pos
      (setf pos (point)))
    (setf after-field (get-char-property-and-overlay pos 'field buf nil)
	  before-field (if (> pos (begv buf))
			   (get-char-property-and-overlay (1- pos) 'field buf nil)
			 nil))
    ;; See if we need to handle the case where MERGE_AT_BOUNDARY is nil
    ;; and POS is at beginning of a field, which can also be interpreted
    ;; as the end of the previous field.  Note that the case where if
    ;; MERGE_AT_BOUNDARY is non-nil (see function comment) is actually the
    ;; more natural one; then we avoid treating the beginning of a field
    ;; specially.
    (unless merge-at-boundary
      (let ((field (get-pos-property pos 'field buf)))
	(when (not (eq field after-field))
	  (setf at-field-end t))
	(when (not (eq field before-field))
	  (setf at-field-start t))
	(when (and (null field)
		   at-field-start
		   at-field-end)
	  ;; If an inserted char would have a nil field while the surrounding
	  ;; text is non-nil, we're probably not looking at a
	  ;; zero-length field, but instead at a non-nil field that's
	  ;; not intended for editing (such as comint's prompts).
	  (setf at-field-end nil
		at-field-start nil))))
    ;; Note about special `boundary' fields:

    ;; Consider the case where the point (`.') is between the fields `x' and `y':

    ;; 	xxxx.yyyy

    ;; In this situation, if merge_at_boundary is true, we consider the
    ;; `x' and `y' fields as forming one big merged field, and so the end
    ;; of the field is the end of `y'.

    ;; However, if `x' and `y' are separated by a special `boundary' field
    ;; (a field with a `field' char-property of 'boundary), then we ignore
    ;; this special field when merging adjacent fields.  Here's the same
    ;; situation, but with a `boundary' field between the `x' and `y' fields:

    ;; 	xxx.BBBByyyy

    ;; Here, if point is at the end of `x', the beginning of `y', or
    ;; anywhere in-between (within the `boundary' field), we merge all
    ;; three fields and consider the beginning as being the beginning of
    ;; the `x' field, and the end as being the end of the `y' field.  */

    ;; Return field boundary
    (values (and beg
		 (if at-field-start
		     pos
		   (let ((p pos))
		     (if (and (null merge-at-boundary)
			      (eq before-field 'boundary))
			 (setf p (previous-single-char-property-change p 'field buf beg-limit))
		       (setf p (previous-single-char-property-change p 'field buf beg-limit)))
		     (or p
			 (begv buf)))))
	    (and end 
		 (if at-field-end
		     pos
		   (progn
		     (when (and (null merge-at-boundary)
				(eq after-field 'boundary))
		       (setf pos (next-single-char-property-change pos 'field buf end-limit)))
		     (setf pos (next-single-char-property-change pos 'field buf end-limit))
		     (or pos
			 (zv buf))))))))

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

(defun buffer-substring (start end &optional (buffer (current-buffer)))
  "Return the contents of part of the current buffer as a string.
The two arguments START and END are character positions;
they can be in either order.
The string returned is multibyte if the buffer is multibyte.

This function copies the text properties of that part of the buffer
into the result string; if you don't want the text properties,
use `buffer-substring-no-properties' instead."
  (multiple-value-setq (start end) (validate-region start end buffer))
  (make-buffer-string start end t buffer))

(defun buffer-substring-no-properties (start end &optional (buffer (current-buffer)))
  "Return the characters of part of the buffer, without the text properties.
The two arguments START and END are character positions;
they can be in either order."
  (multiple-value-setq (start end) (validate-region start end buffer))
  (make-buffer-string start end nil buffer))


(defun field-string (pos)
  "Return the contents of the field surrounding POS as a string.
A field is a region of text with the same `field' property.
If POS is nil, the value of point is used for POS."
  (multiple-value-bind (beg end) (find-field pos nil :beg t :end t)
    (make-buffer-string beg end t)))

(defun field-beginning (&optional pos escape-from-edge limit)
  "Return the beginning of the field surrounding POS.
A field is a region of text with the same `field' property.
If POS is nil, the value of point is used for POS.
If ESCAPE-FROM-EDGE is non-nil and POS is at the beginning of its
field, then the beginning of the *previous* field is returned.
If LIMIT is non-nil, it is a buffer position; if the beginning of the field
is before LIMIT, then LIMIT will be returned instead."
    (declare (ignore escape-from-edge))
  (multiple-value-bind (beg end) (find-field pos nil :beg-limit limit :beg t)
    (declare (ignore end))
    beg))

(defun field-end (&optional pos escape-from-edge limit)
  "Return the end of the field surrounding POS.
A field is a region of text with the same `field' property.
If POS is nil, the value of point is used for POS.
If ESCAPE-FROM-EDGE is non-nil and POS is at the end of its field,
then the end of the *following* field is returned.
If LIMIT is non-nil, it is a buffer position; if the end of the field
is after LIMIT, then LIMIT will be returned instead."
  (declare (ignore escape-from-edge))
  (multiple-value-bind (beg end) (find-field pos nil :end-limit limit :end t)
    (declare (ignore beg))
    end))

(defun npropertize (string &rest props)
  "Same as propertize but don't make a copy of STRING."
  (declare (type string string))
  (let ((ps (make-instance 'pstring
			   :data string)))
    (create-root-interval ps)
    (add-text-properties 0 (pstring-length ps) props ps)
    ps))

(defun propertize (string &rest props)
  "Return a copy of STRING with text properties added.
First argument is the string to copy.
Remaining arguments form a sequence of PROPERTY VALUE pairs for text
properties to add to the result.
usage: (propertize STRING &rest PROPERTIES)"
  (declare (type string string))
  (apply #'npropertize (copy-seq string) props))

(defun delete-region (start end &optional (buffer (current-buffer)))
  "Delete the text between point and mark.

expects two arguments, positions (integers or markers) specifying
the stretch to be deleted."
  (multiple-value-setq (start end) (validate-region start end buffer))
  (buffer-delete buffer start (- end start)))

(defmacro save-current-buffer (&body body)
  "Save the current buffer; execute BODY; restore the current buffer.
Executes BODY just like `progn'."
  (let ((cb (gensym "CB")))
    `(let ((,cb (current-buffer)))
       (unwind-protect (progn ,@body)
         (when (get-buffer ,cb)
           (set-buffer cb))))))

(defmacro save-excursion (&body body)
  "Save point, mark, and current buffer; execute BODY; restore those things.
Executes BODY just like `progn'.
The values of point, mark and the current buffer are restored
even in case of abnormal exit (throw or error).
*The state of activation of the mark is also restored.

*This construct does not save `deactivate-mark', and therefore
*functions that change the buffer will still cause deactivation
*of the mark at the end of the command.  To prevent that, bind
*`deactivate-mark' with `let'."
  (let ((cb (gensym "CB"))
        (point (gensym "POINT"))
        (mark (gensym "MARK")))
    `(let ((,cb (current-buffer))
           (,point (copy-marker (point-marker)))
           (,mark (copy-marker (mark-marker))))
       (unwind-protect (progn ,@body)
         (when (get-buffer ,cb)
           (set-buffer ,cb)
           (setf (buffer-mark-marker ,cb) ,mark
                 (buffer-point ,cb) ,point))))))
  
(defun insert-buffer-substring (buffer start end)
  "Insert before point a substring of the contents of buffer.
buffer may be a buffer or a buffer name.
Arguments start and end are character positions specifying the substring.
They default to the values of (point-min) and (point-max) in buffer."
  (let* ((buf (get-buffer buffer))
	 (s (buffer-substring start end)))
    (with-current-buffer buf
      (insert s))))

(defun preceding-char ()
  "Return the character preceding point.
At the beginning of the buffer or accessible region, return #\Nul."
  (or (char-before (point))
      #\Nul))

(defun bolp ()
  "Return t if point is at the beginning of a line."
  (or (= (point) (point-min))
      (char= (char-before (point)) #\Newline)))

(defun eolp ()
  "Return t if point is at the end of a line.
`End of a line' includes point being at the end of the buffer."
  (or (= (point) (point-max))
      (char= (char-after (point)) #\Newline)))

(defun delete-and-extract-region (start end)
  "Delete the text between start and end and return it."
  (multiple-value-setq (start end) (validate-region start end))
  (if (= start end)
      ""
    (prog1
	(make-buffer-string start end t)
      (delete-region start end))))

(provide :lice-0.1/editfns)
