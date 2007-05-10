(in-package "LICE")

(defvar *inhibit-field-text-motion* nil
  "Non-nil means text motion commands don't notice fields.")

(defvar *buffer-access-fontify-functions* nil
"List of functions called by `buffer-substring' to fontify if necessary.
Each function is called with two arguments which specify the range
of the buffer being accessed.")

(defvar *buffer-access-fontified-property* nil
"Property which (if non-nil) indicates text has been fontified.
`buffer-substring' need not call the `buffer-access-fontify-functions'
functions if all the text being accessed has this property.")

(defvar *system-name* nil
  "The host name of the machine Emacs is running on.")

(defvar *user-full-name* nil
  "The full name of the user logged in.")

(defvar *user-login-name* nil
  "The user's name, taken from environment variables if possible.")

(defvar *user-real-login-name* nil
  "The user's name, based upon the real uid only.")

(defvar *operating-system-release* nil
  "The release of the operating system Emacs is running on.")

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
      (setf pos (pt)))
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

(defun constrain-to-field (new-pos old-pos &optional escape-from-edge only-in-line inhibit-capture-property)
  "Return the position closest to NEW-POS that is in the same field as OLD-POS.

A field is a region of text with the same `field' property.
If NEW-POS is nil, then the current point is used instead, and set to the
constrained position if that is different.

If OLD-POS is at the boundary of two fields, then the allowable
positions for NEW-POS depends on the value of the optional argument
ESCAPE-FROM-EDGE: If ESCAPE-FROM-EDGE is nil, then NEW-POS is
constrained to the field that has the same `field' char-property
as any new characters inserted at OLD-POS, whereas if ESCAPE-FROM-EDGE
is non-nil, NEW-POS is constrained to the union of the two adjacent
fields.  Additionally, if two fields are separated by another field with
the special value `boundary', then any point within this special field is
also considered to be `on the boundary'.

If the optional argument ONLY-IN-LINE is non-nil and constraining
NEW-POS would move it to a different line, NEW-POS is returned
unconstrained.  This useful for commands that move by line, like
\\[next-line] or \\[beginning-of-line], which should generally respect field boundaries
only in the case where they can still move to the right line.

If the optional argument INHIBIT-CAPTURE-PROPERTY is non-nil, and OLD-POS has
a non-nil property of that name, then any field boundaries are ignored.

Field boundaries are not noticed if `inhibit-field-text-motion' is non-nil."
  (let ((orig-point 0)
        fwd prev-old prev-new)
    (unless new-pos 
      ;; Use the current point, and afterwards, set it.
      (setf new-pos (pt)
            orig-point new-pos))
    (check-type new-pos number)
    (check-type old-pos number)
    (setf fwd (> new-pos old-pos)
          prev-old (1- old-pos)
          prev-new (1- new-pos))
    (when (and (null *inhibit-field-text-motion*)
               (/= new-pos old-pos)
               (or (get-char-property new-pos 'field)
                   (get-char-property old-pos 'field)
                   ;; To recognize field boundaries, we must also look at the
                   ;; previous positions; we could use `get_pos_property'
                   ;; instead, but in itself that would fail inside non-sticky
                   ;; fields (like comint prompts).
                   (and (> new-pos (begv))
                        (get-char-property prev-new 'field))
                   (and (> old-pos (begv))
                        (get-char-property prev-old 'field)))
               (or (null inhibit-capture-property)
                   (and (null (get-pos-property old-pos inhibit-capture-property nil))
                        (or (<= old-pos (begv))
                            (null (get-char-property old-pos inhibit-capture-property))
                            (null (get-char-property prev-old inhibit-capture-property))))))
      ;; It is possible that NEW_POS is not within the same field as
      ;; OLD_POS; try to move NEW_POS so that it is.
      (let ((field-bound (if fwd
                             (field-end old-pos escape-from-edge new-pos)
                             (field-beginning old-pos escape-from-edge new-pos))))
        (when (and
               ;; See if ESCAPE_FROM_EDGE caused FIELD_BOUND to jump to the
               ;; other side of NEW_POS, which would mean that NEW_POS is
               ;; already acceptable, and it's not necessary to constrain it
               ;; to FIELD_BOUND.
               (if (< field-bound new-pos) fwd (not fwd))
               ;; NEW_POS should be constrained, but only if either
               ;; ONLY_IN_LINE is nil (in which case any constraint is OK),
               ;; or NEW_POS and FIELD_BOUND are on the same line (in which
               ;; case the constraint is OK even if ONLY_IN_LINE is non-nil). */
               (or (null only-in-line)
                   ;; This is the ONLY_IN_LINE case, check that NEW_POS and
                   ;; FIELD_BOUND are on the same line by seeing whether
                   ;; there's an intervening newline or not.
                   (progn
                     (multiple-value-bind (p nfound)
                         (buffer-scan-newline (current-buffer) new-pos field-bound (if fwd -1 1))
                       (declare (ignore p))
                       (zerop nfound)))))
          ;; Constrain NEW_POS to FIELD_BOUND.
          (setf new-pos field-bound))
        (when (and orig-point
                   (/= new-pos orig-point))
          (set-point new-pos))))
    new-pos))
                 
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

(defun point (&aux (buffer (current-buffer)))
  "Return the point in the current buffer."
  (pt buffer))

(defun point-marker (&aux (buffer (current-buffer)))
  "Return value of point, as a marker object."
  (buffer-point buffer))

(defun point-min (&aux (buffer (current-buffer)))
  "Return the minimum permissible value of point in the current buffer."
  (declare (ignore buffer))
  0)

(defun point-max (&aux (buffer (current-buffer)))
  "Return the maximum permissible value of point in the current buffer."
  (buffer-size buffer))

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
  
(defun insert (&rest objects)
  "Insert the arguments, either strings or characters, at point.
Point and before-insertion markers move forward to end up
 after the inserted text.
Any other markers at the point of insertion remain before the text.

If the current buffer is multibyte, unibyte strings are converted
to multibyte for insertion (see `string-make-multibyte').
If the current buffer is unibyte, multibyte strings are converted
to unibyte for insertion (see `string-make-unibyte').

When operating on binary data, it may be necessary to preserve the
original bytes of a unibyte string when inserting it into a multibyte
buffer; to accomplish this, apply `string-as-multibyte' to the string
and insert the result."
  (dolist (o objects)
    (insert-move-point (current-buffer) o)))

(defun insert-buffer-substring (buffer &optional (start (point-min)) (end (point-max)))
  "Insert before point a substring of the contents of buffer.
buffer may be a buffer or a buffer name.
Arguments start and end are character positions specifying the substring.
They default to the values of (point-min) and (point-max) in buffer."
  (check-number-coerce-marker start)
  (check-number-coerce-marker end)  
  (if (< end start)
      (psetf start end
             end start))
  (let* ((buf (get-buffer buffer)))
    (when (or (< start (buffer-min buf))
              (> end (buffer-max buf)))
      (signal 'args-out-of-range))
    (insert (make-buffer-string start end t buf))))

(defun preceding-char ()
  "Return the character preceding point.
At the beginning of the buffer or accessible region, return #\Nul."
  (or (buffer-char-before (current-buffer) (pt))
      #\Nul))

(defun following-char ()
  "Return the character following point, as a number.
At the end of the buffer or accessible region, return #\Nul."
  (if (>= (pt) (zv))
      #\Nul ; XXX return nil?
      (buffer-fetch-char (buffer-char-to-aref (current-buffer) (pt))
                         (current-buffer))))

(defun bolp ()
  "Return t if point is at the beginning of a line."
  (or (= (pt) (begv))
      (char= (buffer-char-before (current-buffer) (pt)) #\Newline)))

(defun eolp ()
  "Return t if point is at the end of a line.
`End of a line' includes point being at the end of the buffer."
  (or (= (pt) (zv))
      (char= (buffer-char-after (current-buffer) (pt)) #\Newline)))

(defun bobp (&optional (buffer (current-buffer)))
  "Return T when the point is at the beginning of the buffer."
  (= (begv buffer) (pt)))

(defun eobp (&optional (buffer (current-buffer)))
  "Return T when the point is at the end of the buffer."
  (= (zv buffer) (pt)))

(defun delete-and-extract-region (start end)
  "Delete the text between start and end and return it."
  (multiple-value-setq (start end) (validate-region start end))
  (if (= start end)
      ""
    (prog1
	(make-buffer-string start end t)
      (delete-region start end))))

(defun insert-char (character count &optional inherit)
  "Insert COUNT copies of CHARACTER.
Point, and before-insertion markers, are relocated as in the function `insert'.
**The optional third arg INHERIT, if non-nil, says to inherit text properties
**from adjoining text, if those properties are sticky."
  (declare (ignore inherit))
  (check-type character character)
  (check-type count number)
  (unless (< count 0)
    (dotimes (i count)
      (insert character))))

(defun line-beginning-position (n)
  "Return the character position of the first character on the current line.
With argument N not nil or 1, move forward N - 1 lines first.
If scan reaches end of buffer, return that position.

This function constrains the returned position to the current field
unless that would be on a different line than the original,
unconstrained result.  If N is nil or 1, and a front-sticky field
starts at point, the scan stops as soon as it starts.  To ignore field
boundaries bind `inhibit-field-text-motion' to t.

This function does not move point."
  ;; FIXME: inhibit-point-motion-hooks
  (let ((pt (save-excursion
              (forward-line (if n (1- n) 0))
              (pt))))
    (constrain-to-field pt (pt) (not (eql n 1)) t nil)))

(defun line-end-position (&optional (n 1))
  "Return the character position of the last character on the current line.
With argument N not nil or 1, move forward N - 1 lines first.
If scan reaches end of buffer, return that position.

This function constrains the returned position to the current field
unless that would be on a different line than the original,
unconstrained result.  If N is nil or 1, and a rear-sticky field ends
at point, the scan stops as soon as it starts.  To ignore field
boundaries bind `inhibit-field-text-motion' to t.

This function does not move point."
  (check-type n integer)
  (setf n (- n (if (<= n 0) 1 0)))
  (let* ((orig (pt))
         (end-pos (find-before-next-newline orig nil n)))
    (constrain-to-field end-pos orig nil t nil)))

(defun clip-to-bounds (lower num upper)
  (max (min num upper) lower))

(defun string-to-char (string)
  "Convert arg string to a character, the first character of that string.
A multibyte character is handled correctly."
  (char string 0))

(defun char-to-string ()
  (error "Unimplemented"))

(defun buffer-string ()
  (error "Unimplemented"))

(defun field-string-no-properties ()
  (error "Unimplemented"))

(defun delete-field ()
  (error "Unimplemented"))

(defmacro save-current-buffer ()
  (error "Unimplemented"))

(defun bufsize ()
  (error "Unimplemented"))

(defun point-min-marker ()
  (error "Unimplemented"))

(defun point-max-marker ()
  (error "Unimplemented"))

(defun gap-position ()
  (error "Unimplemented"))

(defun gap-size ()
  (error "Unimplemented"))

(defun position-bytes ()
  (error "Unimplemented"))

(defun byte-to-position ()
  (error "Unimplemented"))

(defun previous-char ()
  (error "Unimplemented"))

(defun insert-before-markers ()
  (error "Unimplemented"))

(defun insert-and-inherit ()
  (error "Unimplemented"))

(defun insert-and-inherit-before-markers ()
  (error "Unimplemented"))

(defun user-login-name ()
  (error "Unimplemented"))

(defun user-real-login-name ()
  (error "Unimplemented"))

(defun user-uid ()
  (error "Unimplemented"))

(defun user-real-uid ()
  (error "Unimplemented"))

(defun user-full-name ()
  (error "Unimplemented"))

(defun emacs-pid ()
  (error "Unimplemented"))

(defun current-time ()
  (error "Unimplemented"))

(defun format-time-string ()
  (error "Unimplemented"))

(defun float-time ()
  (error "Unimplemented"))

(defun decode-time ()
  (error "Unimplemented"))

(defun encode-time ()
  (error "Unimplemented"))

(defun current-time-string ()
  (error "Unimplemented"))

(defun current-time-zone ()
  (error "Unimplemented"))

(defun set-time-zone-rule ()
  (error "Unimplemented"))

(defun system-name ()
  (error "Unimplemented"))

(defun message-box ()
  (error "Unimplemented"))

(defun message-or-box ()
  (error "Unimplemented"))

(defun current-message ()
  (error "Unimplemented"))

(defun compare-buffer-substrings ()
  (error "Unimplemented"))

(defun subst-char-in-region (start end fromchar tochar &optional noundo)
  "From START to END, replace FROMCHAR with TOCHAR each time it occurs.
If optional arg NOUNDO is non-nil, don't record this change for undo
and don't mark the buffer as really changed.
Both characters must have the same length of multi-byte form."
  (declare (ignore noundo))
  (check-number-coerce-marker start)
  (check-number-coerce-marker end)
  (check-type fromchar character)
  (check-type tochar character)
  (multiple-value-setq (start end) (validate-region start end))

  ;; FIXME: handle noundo
  (let* ((buf (current-buffer))
         (start-aref (buffer-char-to-aref buf start))
         (end-aref (buffer-char-to-aref buf end)))
    (if (or (< (gap-end buf)
               start-aref)
            (> (buffer-gap-start buf)
               end-aref))
        (nsubstitute tochar fromchar (buffer-data buf)
                     :start start-aref
                     :end end-aref)
        (progn
          (gap-move-to buf start-aref)
          (nsubstitute tochar fromchar (buffer-data buf)
                       :start (buffer-char-to-aref buf start)
                       :end (buffer-char-to-aref buf end))))
    nil))

(defun translate-region-internal ()
  (error "Unimplemented"))

(defun widen ()
  (error "Unimplemented"))

(defun narrow-to-region ()
  (error "Unimplemented"))

(defun save-restriction ()
  (error "Unimplemented"))

(defun transpose-regions (startr1 endr1 startr2 endr2 &optional leave_markers)
  "Transpose region STARTR1 to ENDR1 with STARTR2 to ENDR2.
The regions may not be overlapping, because the size of the buffer is
never changed in a transposition.

Optional fifth arg LEAVE-MARKERS, if non-nil, means don't update
any markers that happen to be located in the regions.

Transposing beyond buffer boundaries is an error."
  (check-number-coerce-marker startr1)
  (check-number-coerce-marker endr1)
  (check-number-coerce-marker startr2)
  (check-number-coerce-marker endr2)
  (multiple-value-setq (startr1 endr1) (validate-region startr1 endr1))
  (multiple-value-setq (startr2 endr2) (validate-region startr2 endr2))
  (when (< startr2 startr1)
    (psetf startr1 startr2
           endr1 endr2
           startr2 startr1
           endr2 endr1))
  ;; no overlapping
  (assert (<= endr1 startr2))
  ;; FIXME: The emacs version looks optimized for a bunch of
  ;; cases. But we're gonna cheap out
  (let ((r1 (buffer-substring startr1 endr1))
        (r2 (buffer-substring startr2 endr2)))
    ;; do the 2nd one first so the positions remain valid.
    (delete-region startr2 endr2)
    (set-point startr2)
    (insert r1)
    (delete-region startr1 endr1)
    (set-point startr1)
    (insert r2)))

(defun goto-char (position &aux (buffer (current-buffer)))
  "Set point to POSITION, a number."
  (check-number-coerce-marker position)
  (when (and (>= position (begv buffer))
	     (<= position (zv buffer)))
    (set-point position buffer)))

(defun char-after (&optional (pos (pt)))
  "Return character in current buffer at position POS.
***POS is an integer or a marker.
***If POS is out of range, the value is nil."
  (check-number-coerce-marker pos)
  (buffer-char-after (current-buffer) pos))

(defun char-before (&optional (pos (pt)))
  "Return character in current buffer preceding position POS.
***POS is an integer or a marker.
***If POS is out of range, the value is nil."
  (check-number-coerce-marker pos)
  (buffer-char-after (current-buffer) (1- pos)))

(defun substring-no-properties (string &optional (from 0) (to (length string)))
  "Return a substring of string, without text properties.
It starts at index from and ending before to.
to may be nil or omitted; then the substring runs to the end of string.
If from is nil or omitted, the substring starts at the beginning of string.
If from or to is negative, it counts from the end.

With one argument, just copy string without its properties."
  (subseq string from to))

(provide :lice-0.1/editfns)
