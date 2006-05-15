(in-package :lice)

;; This function is not translated well
(defun validate-interval-range (object begin end force)
  (let (i searchpos)
;;   /* If we are asked for a point, but from a subr which operates
;;      on a range, then return nothing.  */
;;   if (EQ (*begin, *end) && begin != end)
;;     return NULL_INTERVAL;
    (when (> begin end)
    ;; MOVITZ doesn't have psetf
    (let ((tmp begin))
      (setf begin end
	    end tmp))
;;       (psetf begin end
;; 	     end begin)
      )
    (if (typep object 'buffer)
	(progn
	  (when (not (and (<= (buffer-min object) begin)
			  (<= begin end)
			  (<= end (buffer-max object))))
	    (signal 'args-out-of-range))
	  (setf i (intervals object))
	  (when (= (buffer-min object) (buffer-max object))
	    (return-from validate-interval-range (values nil begin end)))
	  (setf searchpos begin))
      (let ((len (length (pstring-data object))))
	(when (not (and (<= 0 begin)
			(<= begin end)
			(<= end len)))
	  (signal 'args-out-of-range))
	(setf i (intervals object))
	(when (zerop len)
	  (return-from validate-interval-range (values nil begin end)))
	(setf searchpos begin)))
    (if i
	(values (find-interval i searchpos) begin end)
      (if force
	  (values (create-root-interval object) begin end)
	(values i begin end)))))
	
(defun set-text-properties (start end properties &optional (object (current-buffer)))
  (let ((start-bk start)
	(end-bk end)
	i)
    (setf properties (validate-plist properties))
    ;; If we want no properties for a whole string, get rid of its
    ;; intervals.
    (when (and (null properties)
	       (typep object 'pstring)
	       (zerop start)
	       (= end (length (pstring-data object))))
      (when (null (intervals object))
	(return-from set-text-properties t))
      (setf (intervals object) nil)
      (return-from set-text-properties t))
    (multiple-value-setq (i start end)
      (validate-interval-range object start end nil))
    (when (null i)
      (when (null properties)
	(return-from set-text-properties nil))
      ;; /* Restore the original START and END values because
      ;; validate_interval_range increments them for strings.  */
      (setf start start-bk
	    end end-bk)
      (multiple-value-setq (i start end)
	(validate-interval-range object start end t))
      ;; /* This can return if start == end.  */
      (when (null i)
	(return-from set-text-properties nil)))

    ;; TODO: add this
;;     (when (typep object 'buffer)
;;       ;; modify_region (XBUFFER (object), XINT (start), XINT (end));
    (set-text-properties-1 start end properties object i)
    
;;   if (BUFFERP (object) && !NILP (signal_after_change_p))
;;     signal_after_change (XINT (start), XINT (end) - XINT (start),
;; 			 XINT (end) - XINT (start));
    t))

(defun set-text-properties-1 (start end properties buffer i)
  (let ((len (- end start))
	(prev-changed nil)
	unchanged)
    (when (zerop len)
      (return-from set-text-properties-1))
    (when (minusp len)
      (incf start len)
      (setf len (abs len)))
    (when (null i)
      (setf i (find-interval (intervals buffer) start)))
    (when (/= (interval-pt i) start)
      (setf unchanged i
	    i (split-interval-right unchanged (- start (interval-pt unchanged))))
      (when (> (interval-text-length i) len)
	(copy-properties unchanged i)
	(setf i (split-interval-left i len))
	(set-properties properties i buffer)
	(return-from set-text-properties-1))
      (set-properties properties i buffer)
      (when (= (interval-text-length i) len)
	(return-from set-text-properties-1))
      (setf prev-changed i)
      (decf len (interval-text-length i))
      (setf i (next-interval i)))
    (loop while (> len 0)
	  do (progn
	       (when (null i)
		 (error "borked."))
	       (when (>= (interval-text-length i) len)
		 (when (> (interval-text-length i) len)
		   (setf i (split-interval-left i len)))
		 (set-properties properties i buffer)
		 (when prev-changed
		   (merge-interval-left i))
		 (return-from set-text-properties-1))
	       (decf len (interval-text-length i))
	       ;; We have to call set_properties even if we are going
	       ;; to merge the intervals, so as to make the undo
	       ;; records and cause redisplay to happen.
	       (set-properties properties i buffer)
	       (if (null prev-changed)
		   (setf prev-changed i)
		 (setf prev-changed (merge-interval-left i)
		       i prev-changed))
	       (setf i (next-interval i))))))

(defun copy-properties (source target)
  (when (and (default-interval-p source)
	     (default-interval-p target))
    (return-from copy-properties))
  (setf (interval-plist target) (copy-list (interval-plist source))))

(defun set-properties (properties interval object)
  (when (typep object 'buffer)
    ;; record undo info
    )
  (setf (interval-plist interval) (copy-tree properties)))

(defun add-properties (plist i object)
  "Add the properties in plist to interval I. OBJECT should be the
string of buffer containing the interval."
  (declare (ignore object))
  (let ((changed nil))
    (doplist (sym val plist changed)
      (let ((found (getf (interval-plist i) sym)))
	(if found
	    (progn
	      ;; record-property-change
	      (when (not (eql found val))
		(setf (getf (interval-plist i) sym) val
		      changed t)))
	  (progn
	    ;; record-property-change
	    (setf (getf (interval-plist i) sym) val
		  changed t)))))))

(defun validate-plist (list)
  "/* Validate LIST as a property list.  If LIST is not a list, then
make one consisting of (LIST nil).  Otherwise, verify that LIST is
even numbered and thus suitable as a plist.  */"
  (cond ((null list) nil)
	((consp list)
	 (if (oddp (length list))
	     (error "odd length property list")
	   list))
	(t (list (list list nil)))))

(defun interval-has-all-properties (plist i)
"/* Return nonzero if interval I has all the properties, with the same
values, of list PLIST.  */"
  (doplist (sym val plist t)
    (let ((found (getf (interval-plist i) sym)))
      (if found
	  (when (not (eql found val))
	    (return-from interval-has-all-properties nil))
	(return-from interval-has-all-properties nil)))))

(defun add-text-properties (start end properties &optional (object (current-buffer)))
  "Add properties to the text from START to END.  The third argument
 PROPERTIES is a property list specifying the property values to add.
 If the optional fourth argument OBJECT is a buffer (or nil, which
 means the current buffer), START and END are buffer positions
 (integers or markers).  If OBJECT is a string, START and END are
 0-based indices into it.  Return t if any property value actually
 changed, nil otherwise."
  (let ((len (- end start))
	(modified 0)
	i unchanged)
    (setf properties (validate-plist properties))
    (when (null properties)
      (return-from add-text-properties nil))
    (multiple-value-setq (i start end) (validate-interval-range object start end t))
    (when (null i)
      (return-from add-text-properties nil))
    ;; If we're not starting on an interval boundary, we have to split
    ;; this interval.
    (when (/= (interval-pt i) start)
      (if (interval-has-all-properties properties i)
	  (let ((got (- (interval-text-length i) (- start (interval-pt i)))))
	    (when (>= got len)
	      (return-from add-text-properties nil))
	    (decf len got)
	    (setf i (next-interval i)))
	(progn
	  (setf unchanged i)
	  (setf i (split-interval-right unchanged (- start (interval-pt unchanged))))
	  (copy-properties unchanged i))))
    ;; if (BUFFERP (object))
    ;;   modify_region (XBUFFER (object), XINT (start), XINT (end));

    ;; We are at the beginning of interval I, with LEN chars to scan.
    (loop
     (when (null i)
       (error "BORK."))
     (when (>= (interval-text-length i) len)
       (when (interval-has-all-properties properties i)
	 (return-from add-text-properties modified))
       (when (= (interval-text-length i) len)
	 (add-properties properties i object)
	 (return-from add-text-properties t))
       (setf unchanged i
	     i (split-interval-left unchanged len))
       (copy-properties unchanged i)
       (add-properties properties i object)
       (return-from add-text-properties t))
     (decf len (interval-text-length i))
     (setf modified (add-properties properties i object)
	   i (next-interval i)))))

(defun put-text-property (start end property value object)
  "Set one property of the text from START to END.  The third and
fourth arguments PROPERTY and VALUE specify the property to add.  If
the optional fifth argument OBJECT is a buffer (or nil, which means
the current buffer), START and END are buffer positions (integers or
markers).  If OBJECT is a string, START and END are 0-based indices
into it."
  (add-text-properties start end (list property value) object))

(defun remove-properties (plist list i object)
  (declare (ignore object))
  (doplist (sym val plist)
  (declare (ignore val))
    (remf sym (interval-plist i)))
  (dolist (sym list)
    (remf sym (interval-plist i))))

(defun remove-text-properties (start end properties &optional (object (current-buffer)))
  "Remove some properties from text from START to END.  The third
 argument PROPERTIES is a property list whose property names specify
 the properties to remove.  \(The values stored in PROPERTIES are
 ignored.)  If the optional fourth argument OBJECT is a buffer (or nil,
 which means the current buffer), START and END are buffer positions
 (integers or markers).  If OBJECT is a string, START and END are
 0-based indices into it.  Return t if any property was actually
 removed, nil otherwise.

 Use set-text-properties if you want to remove all text properties."
  (let (i unchanged len (modified nil))
    (multiple-value-setq (i start end) 
      (validate-interval-range object start end nil))
    (when (null i)
      (return-from remove-text-properties nil))
    (setf len (- end start))
    (when (/= (interval-pt i) start)
      (if (not (interval-has-all-properties properties i))
	(let ((got (- (interval-text-length i) (- start (interval-pt i)))))
	  (when (>= got len)
	    (return-from remove-text-properties nil))
	  (decf len got)
	  (setf i (next-interval i)))
	(progn
	  (setf unchanged i
		i (split-interval-right unchanged (- start (interval-pt unchanged))))
	  (copy-properties unchanged i))))
      (loop
       (unless i
	 (error "BORK."))
       (when (>= (interval-text-length i) len)
	 (unless (interval-has-all-properties properties i)
	   (return-from remove-text-properties modified))
	 (when (= (interval-text-length i) len)
	   (remove-properties properties nil i object)
	   (return-from remove-text-properties t))
	 (setf unchanged i
	       i (split-interval-left i len))
	 (copy-properties unchanged i)
	 (remove-properties properties nil i object)
	 (return-from remove-text-properties t))
       (decf len (interval-text-length i))
       (setf modified (remove-properties properties nil i object)
	     i (next-interval i)))))

(defun next-single-char-property-change (position prop &optional (object (current-buffer)) limit)
  "/* Return the position of next text property or overlay change for a specific property.
Scans characters forward from POSITION till it finds
a change in the PROP property, then returns the position of the change.
If the optional third argument OBJECT is a buffer (or nil, which means
the current buffer), POSITION is a buffer position (integer or marker).
If OBJECT is a string, POSITION is a 0-based index into it.

The property values are compared with `eql' by default.
If the property is constant all the way to the end of OBJECT, return the
last valid position in OBJECT.
If the optional fourth argument LIMIT is non-nil, don't search
past position LIMIT; return LIMIT if nothing is found before LIMIT.  */"
  (if (typep object 'pstring)
      (progn
	(setf position (next-single-property-change position prop object limit))
	(unless position
	  (if (null limit)
	      (setf position (pstring-length object))
	    (setf position limit))))
    (let ((initial-value (get-char-property position prop object))
	  value)
;;       (when (and (typep object 'buffer)
;; 		 (not (eq object (current-buffer))))
;; 	(
      (when (null limit)
	(setf limit (buffer-max object)))
      (loop
       (setf position (next-char-property-change position limit object))
       (when (>= position limit)
	 (return limit))
       (setf value (get-char-property position prop object))
       (unless (eq value initial-value)
	 (return position))))))

(defun text-properties-at (position &optional (object (current-buffer)))
  (multiple-value-bind (i position) (validate-interval-range object position position t)
    (unless (null i)
      ;; If POSITION is at the end of the interval,
      ;; it means it's the end of OBJECT.
      ;; There are no properties at the very end,
      ;; since no character follows.
      (unless (= position (+ (interval-text-length i) (interval-pt i)))
	(interval-plist i)))))
    
(defun get-text-property (position prop object)
  (getf (text-properties-at position object) prop))

(defun get-char-property-and-overlay (position prop object overlay)
  (declare (ignore overlay))
  (get-text-property position prop object))

(defun get-char-property (position prop &optional (object (current-buffer)))
  (get-char-property-and-overlay position prop object 0))

(defun previous-single-char-property-change (position prop &optional (object (current-buffer)) limit)
  (cond ((typep object 'pstring)
	 (setf position (previous-single-property-change position prop object limit))
	 (when (null position)
	   (setf position (or limit
			      (pstring-length object)))))
	(t
	 (unless limit
	   (setf limit (buffer-min object)))
	 (if (<= position limit)
	     (setf position limit)
	   (let ((initial-value (get-char-property (1- position) prop object))
		 value)
	     (loop
	      (setf position (previous-char-property-change position limit object))
	      (when (<= position limit)
		(return limit))
	      (setf value (get-char-property (1- position) prop object))
	      (unless (eq value initial-value)
		(return position))))))))

(defun next-property-change (position &optional object limit)
  "Return the position of next property change.
Scans characters forward from POSITION in OBJECT till it finds
a change in some text property, then returns the position of the change.
If the optional second argument OBJECT is a buffer (or nil, which means
the current buffer), POSITION is a buffer position (integer or marker).
If OBJECT is a string, POSITION is a 0-based index into it.
Return nil if the property is constant all the way to the end of OBJECT.
If the value is non-nil, it is a position greater than POSITION, never equal.

If the optional third argument LIMIT is non-nil, don't search
past position LIMIT; return LIMIT if nothing is found before LIMIT."
  (let (i next)
    (multiple-value-setq (i position) (validate-interval-range object position position nil))
    (when (eq limit t)
      (setf next (if (null i)
		     i
		   (next-interval i)))
      (setf position (if (null next)
			 (cond ((typep object 'pstring) (pstring-length object))
			       ((typep object 'buffer) (buffer-max object)))
		       (interval-pt next)))
      (return-from next-property-change position))

    (when (null i)
      (return-from next-property-change limit))
    (setf next (next-interval i))
    (loop while (and next
		     (intervals-equal i next)
		     (or (null limit)
			 (< (interval-pt next)
			    limit)))
	  do (setf next (next-interval next)))
    (when (null next)
      (return-from next-property-change limit))
    (when (null limit)
      (setf limit (cond ((typep object 'pstring) (pstring-length object))
			((typep object 'buffer) (buffer-max object)))))
    (unless (< (interval-pt next) limit)
      (return-from next-property-change limit))
    ;; FIXME: This is silly code.
    (setf position (interval-pt next))
    position))
	  
(defun next-char-property-change (position &optional limit (buffer (current-buffer)))
  "Return the position of next text property or overlay change.
This scans characters forward in the current buffer from POSITION till
it finds a change in some text property, or the beginning or end of an
overlay, and returns the position of that.
If none is found, the function returns (point-max).

If the optional third argument LIMIT is non-nil, don't search
past position LIMIT; return LIMIT if nothing is found before LIMIT."
  ;;   temp = Fnext_overlay_change (position);
  (next-property-change position buffer (or limit (buffer-max buffer))))

(defun previous-char-property-change (position &optional limit (buffer (current-buffer)))
  "Return the position of previous text property or overlay change.
Scans characters backward in the current buffer from POSITION till it
finds a change in some text property, or the beginning or end of an
overlay, and returns the position of that.
If none is found, the function returns (point-max).

If the optional third argument LIMIT is non-nil, don't search
past position LIMIT; return LIMIT if nothing is found before LIMIT."
  (previous-property-change position buffer (or limit (buffer-min buffer))))

(defun next-single-property-change (position prop &optional (object (current-buffer)) limit)
  (let (i next here-val)
    (multiple-value-setq (i position)
      (validate-interval-range object position position nil))
    (when (null i)
      (return-from next-single-property-change limit))
    (setf here-val (getf (interval-plist i) prop)
	  next (next-interval i))
    ;; walk the intervals til we find one with a different plist val
    ;; for prop.
    (loop while (and next
		     (eql here-val (getf (interval-plist next) prop))
		     (or (null limit)
			 (< (interval-pt next) limit)))
	  do (setf next (next-interval next)))
    ;; FIXME: this code should be cleaned.
    (when (null next)
      (return-from next-single-property-change limit))
    (setf limit (or limit
		    (cond ((typep object 'pstring) (pstring-length object))
			  ((typep object 'buffer) (buffer-max object)))))
    (when (>= (interval-pt next) limit)
      (return-from next-single-property-change limit))
    (interval-pt next)))

(defun previous-single-property-change (position prop &optional (object (current-buffer)) limit)
  (let (i previous here-val)
    (multiple-value-setq (i position) (validate-interval-range object position position nil))
    (when (and i
	       (= (interval-pt i) position))
      (setf i (previous-interval i)))
    (unless i
      (return-from previous-single-property-change limit))
    (setf here-val (getf (interval-plist i) prop)
	  previous (previous-interval i))
    (loop while (and previous
		     (eql here-val (getf (interval-plist previous) prop))
		     (or (null limit)
			 (> (+ (interval-pt previous) (interval-text-length previous))
			    limit)))
	  do (setf previous (previous-interval previous)))
    ;; FIXME: this code should be cleaned.
    (when (null previous)
      (return-from previous-single-property-change limit))
    (setf limit (or limit
		    (cond ((typep object 'pstring) 0)
			  ((typep object 'buffer) (buffer-min object)))))
    (when (<= (+ (interval-pt previous) (interval-text-length previous))
	      limit)
      (return-from previous-single-property-change limit))
    (+ (interval-pt previous) (interval-text-length previous))))

(defun previous-property-change (position &optional (object (current-buffer)) limit)
  "Return the position of previous property change.
Scans characters backwards from POSITION in OBJECT till it finds
a change in some text property, then returns the position of the change.
If the optional second argument OBJECT is a buffer (or nil, which means
the current buffer), POSITION is a buffer position (integer or marker).
If OBJECT is a string, POSITION is a 0-based index into it.
Return nil if the property is constant all the way to the start of OBJECT.
If the value is non-nil, it is a position less than POSITION, never equal.

If the optional third argument LIMIT is non-nil, don't search
back past position LIMIT; return LIMIT if nothing is found until LIMIT."
  (let (i previous)
    (multiple-value-setq (i position) (validate-interval-range object position position nil))
    (unless i
      (return-from previous-property-change limit))
    (when (= (interval-pt i) position)
      (setf i (previous-interval i)))
    (setf previous (previous-interval i))
    (loop while (and previous
		     (intervals-equal previous i)
		     (or (null limit)
			 (> (+ (interval-pt previous)
			       (interval-text-length previous))
			    limit)))
	  do (setf previous (previous-interval previous)))
    ;; FIXME: this code needs cleaning
    (when (null previous)
      (return-from previous-property-change limit))
    (setf limit (or limit
		    (cond ((typep object 'pstring) 0)
			  ((typep object 'buffer) (buffer-min object)))))
    (when (<= (+ (interval-pt previous) (interval-text-length previous))
	      limit)
      (return-from previous-property-change limit))
    (+ (interval-pt previous) (interval-text-length previous))))

(defun text-property-stickiness (prop pos &optional (buffer (current-buffer)))
  "Return the direction from which the text-property PROP would be
inherited by any new text inserted at POS: AFTER if it would be
inherited from the char after POS, BEFORE if it would be inherited from
the char before POS, and NIL if from neither.
BUFFER can be either a buffer or nil (meaning current buffer)."
  (labels ((tmem (sym set)
             ;; Test for membership, allowing for t (actually any
	     ;; non-cons) to mean the universal set."
	     (if (consp set) 
		 (find sym set)
	       set)))
    (let ((is-rear-sticky t)
	  (is-front-sticky nil)
	  prev-pos front-sticky)
      (when (> pos (begv buffer))
	;; Consider previous character.
	(setf prev-pos (1- pos))
	(let ((rear-non-sticky (get-text-property prev-pos 'rear-nonsticky buffer)))
	  (when (tmem prop rear-non-sticky)
	    ;; PROP is rear-non-sticky
	    (setf is-rear-sticky nil))))
      ;; Consider following character.
      (setf front-sticky (get-text-property pos 'front-sticky buffer))
      (when (or (eq front-sticky t)
		(and (consp front-sticky)
		     (find prop front-sticky)))
	;; PROP is inherited from after
	(setf is-front-sticky t))
      ;; return the symbol
      (cond
       ;; Simple cases, where the properties are consistent.      
       ((and is-rear-sticky
	     (not is-front-sticky))
	'before)
       ((and (not is-rear-sticky)
	     is-front-sticky)
	'after)
       ((and (not is-rear-sticky)
	     (not is-front-sticky))
	nil)
       ((or (= pos (begv buffer))
	    (null (get-text-property prev-pos prop buffer)))
	'after)
       (t 
	'before)))))
	   
(provide :lice-0.1/textprop)
