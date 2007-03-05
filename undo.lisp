;;; undo code from undo.c

(in-package "LICE")

(defvar *last-undo-buffer* nil
  "Last buffer for which undo information was recorded.")

;; FIXME: a global used in these functions is probably bad wrt concurrency
(defvar *pending-boundary* nil
  "The first time a command records something for undo.
it also allocates the undo-boundary object
which will be added to the list at the end of the command.
This ensures we can't run out of space while trying to make
an undo-boundary.")

(defun ensure-pending-boundary ()
  "Allocate a cons cell to be the undo boundary after this command."
  (when (null *pending-boundary*)
    (setf *pending-boundary* (cons nil nil))))

(defun ensure-last-undo-buffer (&optional (buffer (current-buffer)))
  (unless (eq buffer *last-undo-buffer*)
    (undo-boundary buffer))
  (setf *last-undo-buffer* buffer))

(defun record-point (pt &optional (buffer (current-buffer)))
  "Record point as it was at beginning of this command (if necessary)
And prepare the undo info for recording a change.
PT is the position of point that will naturally occur as a result of the
undo record that will be added just after this command terminates."
  (let (at-boundary)
    (ensure-pending-boundary)
    (ensure-last-undo-buffer buffer)
    (if (consp (buffer-undo-list buffer))
        ;; Set AT_BOUNDARY to 1 only when we have nothing other than
        ;; marker adjustment before undo boundary.
        (setf at-boundary (loop 
                             for elt in (buffer-undo-list buffer)
                             while (typep elt 'undo-entry-marker)
                             finally (return (null elt))))
        (setf at-boundary t))
    ;; FIXME
    ;;   if (MODIFF <= SAVE_MODIFF)
    ;;     record_first_change ();
    (when (and at-boundary
               ;; If we're called from batch mode, this could be nil.
               (eq buffer *last-point-position-buffer*))
      ;; If we have switched windows, use the point value
      ;; from the window we are in.
      (unless (eq *last-point-position-window* (selected-window))
        (setf *last-point-position* (marker-position (window-point (selected-window)))))
      (when (/= *last-point-position* pt)
        (push *last-point-position* (buffer-undo-list buffer))))))

(defun record-insert (beg length &optional (buffer (current-buffer)))
  "Record an insertion that just happened or is about to happen, for
LENGTH characters at position BEG.  (It is possible to record an
insertion before or after the fact because we don't need to record
the contents.)"
  (when (eq (buffer-undo-list buffer) t)
    (return-from record-insert nil))
  (record-point beg buffer)
  ;; If this is following another insertion and consecutive with it
  ;; in the buffer, combine the two.
  (when (consp (buffer-undo-list buffer))
    (let ((elt (car (buffer-undo-list buffer))))
      (when (and (typep elt 'undo-entry-insertion)
                 (= (undo-entry-insertion-end elt) beg))
        (setf (undo-entry-insertion-end elt) (+ beg length))
        (return-from record-insert nil))))

  (push (make-undo-entry-insertion :beg beg :end (+ beg length)) (buffer-undo-list buffer)))

(defun record-delete (beg string &optional (buffer (current-buffer)))
  "Record that a deletion is about to take place, of the
characters in STRING, at location BEG."
  (when (eq (buffer-undo-list buffer) t)
    (return-from record-delete nil))
  (if (= (point) (+ beg (length string)))
      (progn
        (setf beg (- beg))
        (record-point (point)))
      (record-point beg))
  (push (make-undo-entry-delete :position beg :text string)
        (buffer-undo-list buffer)))

(defun record-marker-adjustment (marker adjustment &optional (buffer (current-buffer)))
  "Record the fact that MARKER is about to be adjusted by ADJUSTMENT.
This is done only when a marker points within text being deleted,
because that's the only case where an automatic marker adjustment
won't be inverted automatically by undoing the buffer modification."
  (when (eq (buffer-undo-list buffer) t)
    (return-from record-marker-adjustment nil))
  (unless *pending-boundary*
    (setf *pending-boundary* (cons nil nil)))
  (unless (eq buffer *last-undo-buffer*)
    (undo-boundary buffer))
  (setf *last-undo-buffer* buffer)
  (push (make-undo-entry-marker :marker marker :adjustment adjustment)
        (buffer-undo-list buffer)))

(defun record-change (beg length &optional (buffer (current-buffer)))
  "Record that a replacement is about to take place,
for LENGTH characters at location BEG.
The replacement must not change the number of characters."
  (record-delete beg (buffer-substring beg (+ beg length) buffer))
  (record-insert beg length))

(defun record-first-change (&optional (buffer (current-buffer)))
  "Record that an unmodified buffer is about to be changed.
Record the file modification date so that when undoing this entry
we can tell whether it is obsolete because the file was saved again."
  (when (eq (buffer-undo-list buffer) t)
    (return-from record-first-change nil))
  
  (unless (eq buffer *last-undo-buffer*)
    (undo-boundary buffer))
  (setf *last-undo-buffer* buffer)

  ;; FIXME
  ;;     if (base_buffer->base_buffer)
  ;;     base_buffer = base_buffer->base_buffer;

  ;; FIXME: implement modtime
  (push (make-undo-entry-modified :time nil)
        (buffer-undo-list buffer)))        

(defun record-property-change (beg length prop value buffer)
  "Record a change in property PROP (whose old value was VAL)
for LENGTH characters starting at position BEG in BUFFER."
  (let (boundary)
    (when (eq (buffer-undo-list buffer) t)
      (return-from record-property-change nil))
  
    (ensure-pending-boundary)
    (unless (eq buffer *last-undo-buffer*)
      (setf boundary t))
    (setf *last-undo-buffer* buffer)
    (when boundary
      (undo-boundary buffer))
    ;; FIXME
    ;;   if (MODIFF <= SAVE_MODIFF)
    ;;     record_first_change ();
  
    (push (make-undo-entry-property :prop prop :value value :beg beg :end (+ beg length))
          (buffer-undo-list buffer))))

(defun undo-boundary (&optional (buffer (current-buffer)))
  "Mark a boundary between units of undo.
An undo command will stop at this point,
but another undo command will undo to the previous boundary."
  (when (eq (buffer-undo-list buffer) t)
    (return-from undo-boundary nil))
  (when (car (buffer-undo-list buffer))
    ;; One way or another, cons nil onto the front of the undo list.
    (if *pending-boundary*
        ;; If we have preallocated the cons cell to use here, use that one. ; why the little dance? -sabetts
        (setf (cdr *pending-boundary*) (buffer-undo-list buffer)
              (buffer-undo-list buffer) *pending-boundary*
              *pending-boundary* nil)
        (push nil (buffer-undo-list buffer)))
    nil))

(defgeneric primitive-undo-elt (undo-elt)
  )

(defmethod primitive-undo-elt ((elt integer))
  "Handle an integer by setting point to that value."
  (set-point (clip-to-bounds (begv) elt (zv))))

(defmethod primitive-undo-elt ((elt undo-entry-insertion))
  (when (or (< (undo-entry-insertion-beg elt) (begv))
            (> (undo-entry-insertion-end elt) (zv)))
    (error "Changes to be undone are outside visible portion of buffer"))
  (goto-char (undo-entry-insertion-beg elt))
  (delete-region (undo-entry-insertion-beg elt) 
                 (undo-entry-insertion-end elt)))

(defmethod primitive-undo-elt ((elt undo-entry-delete))
  (let ((pos (undo-entry-delete-position elt))
        (text (undo-entry-delete-text elt)))
    (if (minusp pos)
        (progn
          (when (or (< (- pos) (begv))
                    (> (- pos) (zv)))
            (error "Changes to be undone are outside visible portion of buffer"))
          (set-point (- pos))
          (insert text))
        (progn
          (when (or (< pos (begv))
                    (> pos (zv)))
            (error "Changes to be undone are outside visible portion of buffer"))
          (set-point pos)
          (insert text)
          (set-point pos)))))

(defmethod primitive-undo-elt ((undo-elt undo-entry-modified))
  (error "unimplented"))

(defmethod primitive-undo-elt ((elt undo-entry-property))
  (put-text-property (undo-entry-property-beg elt)
                     (undo-entry-property-end elt)
                     (undo-entry-property-prop elt)
                     (undo-entry-property-value elt)
                     nil))

(defmethod primitive-undo-elt ((undo-elt undo-entry-apply))
  (error "unimplented"))

(defmethod primitive-undo-elt ((undo-elt undo-entry-selective))
  (error "unimplented"))

(defmethod primitive-undo-elt ((elt undo-entry-marker))
  (let ((marker (undo-entry-marker-marker elt)))
    (when (marker-buffer marker)
      (set-marker marker (- (marker-position marker)
                            (undo-entry-marker-distance elt))
                  (marker-buffer marker)))))

(defun primitive-undo (n list)
  "Undo N records from the front of the list LIST.
Return what remains of the list."
  (check-type n integer)
  (let ( ;; Don't let `intangible' properties interfere with undo.
        (*inhibit-point-motion-hooks* t)
        ;; In a writable buffer, enable undoing read-only text that is so
        ;; because of text properties.
        (*inhibit-read-only* t))
    (dotimes (arg n)
      (while (consp list)
        (let ((elt (pop list)))
          ;; Exit inner loop at undo boundary.
          (when (null elt)
            (return nil))
          (primitive-undo-elt elt))))
    ;; Make sure an apply entry produces at least one undo entry,
    ;; so the test in `undo' for continuing an undo series
    ;; will work right.
    list))

;;; undo code from simple.el

;; XXX: gnu emacs uses a weak hashtable that automatically removes
;; references. We need some mechanism to do similar.
(defvar undo-equiv-table (make-hash-table :test 'eq #|:weakness t|#)
  "Table mapping redo records to the corresponding undo one.
A redo record for undo-in-region maps to t.
A redo record for ordinary undo maps to the following (earlier) undo.")

(defvar undo-in-region nil
  "Non-nil if `pending-undo-list' is not just a tail of `buffer-undo-list'.")

(defvar undo-no-redo nil
  "If t, `undo' doesn't go through redo entries.")

(defvar pending-undo-list nil
  "Within a run of consecutive undo commands, list remaining to be undone.
If t, we undid all the way to the end of it.")

(defcommand undo ((&optional arg)
                  ;; XXX: what about the *?
                  :raw-prefix)
  "Undo some previous changes.
Repeat this command to undo more changes.
A numeric argument serves as a repeat count.

In Transient Mark mode when the mark is active, only undo changes within
the current region.  Similarly, when not in Transient Mark mode, just \\[universal-argument]
as an argument limits undo to changes within the current region."
  ;;(interactive "*P")
  ;; Make last-command indicate for the next command that this was an undo.
  ;; That way, another undo will undo more.
  ;; If we get to the end of the undo history and get an error,
  ;; another undo command will find the undo history empty
  ;; and will get another error.  To begin undoing the undos,
  ;; you must type some other command.
  (let ((modified (buffer-modified-p (current-buffer)))
	(recent-save (recent-auto-save-p))
	message)
    ;; If we get an error in undo-start,
    ;; the next command should not be a "consecutive undo".
    ;; So set `this-command' to something other than `undo'.
    (setq *this-command* 'undo-start)

    (unless (and (eq *last-command* 'undo)
		 (or (eq pending-undo-list t)
		     ;; If something (a timer or filter?) changed the buffer
		     ;; since the previous command, don't continue the undo seq.
		     (let ((list (buffer-undo-list (current-buffer))))
		       (while (eq (car list) nil)
			 (setq list (cdr list)))
		       ;; If the last undo record made was made by undo
		       ;; it shows nothing else happened in between.
		       (gethash list undo-equiv-table))))
      (message "guuuungh")
      (setq undo-in-region
	    (if transient-mark-mode *mark-active* (and arg (not (numberp arg)))))
      (if undo-in-region
	  (undo-start (region-beginning) (region-end))
          (undo-start))
      ;; get rid of initial undo boundary
      (undo-more 1))
    ;; If we got this far, the next command should be a consecutive undo.
    (setq *this-command* 'undo)
    ;; Check to see whether we're hitting a redo record, and if
    ;; so, ask the user whether she wants to skip the redo/undo pair.
    (let ((equiv (gethash pending-undo-list undo-equiv-table)))
      (or (eq (selected-window) (minibuffer-window))
	  (setq message (if undo-in-region
			    (if equiv "Redo in region!" "Undo in region!")
			  (if equiv "Redo!" "Undo!"))))
      (when (and (consp equiv) undo-no-redo)
	;; The equiv entry might point to another redo record if we have done
	;; undo-redo-undo-redo-... so skip to the very last equiv.
	(while (let ((next (gethash equiv undo-equiv-table)))
		 (if next (setq equiv next))))
	(setq pending-undo-list equiv)))
    (undo-more
     (if (or transient-mark-mode (numberp arg))
	 (prefix-numeric-value arg)
       1))
    ;; Record the fact that the just-generated undo records come from an
    ;; undo operation--that is, they are redo records.
    ;; In the ordinary case (not within a region), map the redo
    ;; record to the following undos.
    ;; I don't know how to do that in the undo-in-region case.
    (setf (gethash (buffer-undo-list (current-buffer)) undo-equiv-table)
          (if undo-in-region t pending-undo-list))
    ;; Don't specify a position in the undo record for the undo command.
    ;; Instead, undoing this should move point to where the change is.
    (let ((tail (buffer-undo-list (current-buffer)))
	  (prev nil))
        (message "its: ~s" tail)
      (while (car tail)
	(when (integerp (car tail))
	  (let ((pos (car tail)))
	    (if prev
		(setf (cdr prev) (cdr tail))
                (setf (buffer-undo-list (current-buffer)) (cdr tail)))
	    (setq tail (cdr tail))
	    (while (car tail)
	      (if (eql pos (car tail))
		  (if prev
		      (setf (cdr prev) (cdr tail))
                      (setf (buffer-undo-list (current-buffer)) (cdr tail)))
		(setq prev tail))
	      (setq tail (cdr tail)))
	    (setq tail nil)))
	(setq prev tail
              tail (cdr tail))))
    ;; Record what the current undo list says,
    ;; so the next command can tell if the buffer was modified in between.
    (and modified (not (buffer-modified-p (current-buffer)))
	 (delete-auto-save-file-if-necessary recent-save))
    ;; Display a message announcing success.
    (if message
	(message message))))

(defcommand buffer-disable-undo ((&optional buffer))
  "Make BUFFER stop keeping undo information.
No argument or nil as argument means do this for the current buffer."
  (with-current-buffer (if buffer (get-buffer buffer) (current-buffer))
    (setf (buffer-undo-list (current-buffer)) t)))

(defcommand undo-only ((&optional arg)
                       ;; XXX what about *
                       :prefix)
  "Undo some previous changes.
Repeat this command to undo more changes.
A numeric argument serves as a repeat count.
Contrary to `undo', this will not redo a previous undo."
  ;;(interactive "*p")
  (let ((undo-no-redo t)) (undo arg)))

(defvar undo-in-progress nil
  "Non-nil while performing an undo.
Some change-hooks test this variable to do something different.")

(defun undo-more (n)
  "Undo back N undo-boundaries beyond what was already undone recently.
Call `undo-start' to get ready to undo recent changes,
then call `undo-more' one or more times to undo them."
  (or (listp pending-undo-list)
      (error (concat "No further undo information"
                     (and transient-mark-mode *mark-active*
                          " for region"))))
  (let ((undo-in-progress t))
    (setq pending-undo-list (primitive-undo n pending-undo-list))
    (if (null pending-undo-list)
	(setq pending-undo-list t))))

;; Deep copy of a list
(defun undo-copy-list (list)
  "Make a copy of undo list LIST."
  (labels ((helper (elt)
             (if (typep elt 'structure-object)
                 (copy-structure elt)
                 elt)))
    (mapcar #'helper list)))

(defun undo-start (&optional beg end)
  "Set `pending-undo-list' to the front of the undo list.
The next call to `undo-more' will undo the most recently made change.
If BEG and END are specified, then only undo elements
that apply to text between BEG and END are used; other undo elements
are ignored.  If BEG and END are nil, all undo elements are used."
  (if (eq (buffer-undo-list (current-buffer)) t)
      (error "No undo information in this buffer"))
  (setq pending-undo-list
	(if (and beg end (not (= beg end)))
	    (undo-make-selective-list (min beg end) (max beg end))
            (buffer-undo-list (current-buffer)))))

(defvar undo-adjusted-markers)

(defun undo-make-selective-list (start end)
  "Return a list of undo elements for the region START to END.
The elements come from `buffer-undo-list', but we keep only
the elements inside this region, and discard those outside this region.
If we find an element that crosses an edge of this region,
we stop and ignore all further elements."
  (let ((undo-list-copy (undo-copy-list (buffer-undo-list (current-buffer))))
	(undo-list (list nil))
	undo-adjusted-markers
	some-rejected
	undo-elt temp-undo-list delta)
    (while undo-list-copy
      (setq undo-elt (car undo-list-copy))
      (let ((keep-this
	     (cond ((typep undo-elt 'undo-entry-modified) ;;(and (consp undo-elt) (eq (car undo-elt) t))
		    ;; This is a "was unmodified" element.
		    ;; Keep it if we have kept everything thus far.
		    (not some-rejected))
		   (t
		    (undo-elt-in-region undo-elt start end)))))
	(if keep-this
	    (progn
	      (setq end (+ end (cdr (undo-delta undo-elt))))
	      ;; Don't put two nils together in the list
	      (if (not (and (eq (car undo-list) nil)
			    (eq undo-elt nil)))
		  (setq undo-list (cons undo-elt undo-list))))
	  (if (undo-elt-crosses-region undo-elt start end)
	      (setq undo-list-copy nil)
              (progn
                (setq some-rejected t)
                (setq temp-undo-list (cdr undo-list-copy))
                (setq delta (undo-delta undo-elt))

                (when (/= (cdr delta) 0)
                  (let ((position (car delta))
                        (offset (cdr delta)))

                    ;; Loop down the earlier events adjusting their buffer
                    ;; positions to reflect the fact that a change to the buffer
                    ;; isn't being undone. We only need to process those element
                    ;; types which undo-elt-in-region will return as being in
                    ;; the region since only those types can ever get into the
                    ;; output

                    (dolist (undo-elt temp-undo-list)
                      (cond ((integerp undo-elt)
                             (if (>= undo-elt position)
                                 (setf (car temp-undo-list) (- undo-elt offset))))
                            ;;((atom undo-elt) nil)
                            ((typep undo-elt 'undo-entry-delete) ;(stringp (car undo-elt))
                             ;; (TEXT . POSITION)
                             (let ((text-pos (abs (undo-entry-delete-position undo-elt)))
                                   (point-at-end (< (undo-entry-delete-position undo-elt) 0 )))
                               (if (>= text-pos position)
                                   (setf (undo-entry-delete-position undo-elt) (* (if point-at-end -1 1)
                                                                                  (- text-pos offset))))))
                            ((typep undo-elt 'undo-entry-insertion) ;(integerp (car undo-elt))
                             ;; (BEGIN . END)
                             (when (>= (undo-entry-insertion-beg undo-elt) position)
                               (setf (undo-entry-insertion-beg undo-elt) (- (undo-entry-insertion-beg undo-elt) offset))
                               (setf (undo-entry-insertion-end undo-elt) (- (undo-entry-insertion-end undo-elt) offset))))
                            ((typep undo-elt 'undo-entry-property) ;(null (car undo-elt))
                             ;; (nil PROPERTY VALUE BEG . END)
                             (when (>= (undo-entry-property-beg undo-elt) position)
                               (setf (undo-entry-property-beg undo-elt) (- (undo-entry-property-beg undo-elt) offset))
                               (setf (undo-entry-property-end undo-elt) (- (undo-entry-property-end undo-elt) offset))))))))))))
      (setq undo-list-copy (cdr undo-list-copy)))
    (nreverse undo-list)))

(defun undo-elt-in-region (undo-elt start end)
  "Determine whether UNDO-ELT falls inside the region START ... END.
If it crosses the edge, we return nil."
  (cond ((integerp undo-elt)
	 (and (>= undo-elt start)
	      (<= undo-elt end)))
	((eq undo-elt nil)
	 t)
;; 	((atom undo-elt)
;; 	 nil)
	((typep undo-elt 'undo-entry-delete) ; (stringp (car undo-elt))
	 ;; (TEXT . POSITION)
	 (and (>= (abs (undo-entry-delete-position undo-elt)) start)
	      (< (abs (undo-entry-delete-position undo-elt)) end)))
	((typep undo-elt 'undo-entry-marker) ;(and (consp undo-elt) (markerp (car undo-elt)))
	 ;; This is a marker-adjustment element (MARKER . ADJUSTMENT).
	 ;; See if MARKER is inside the region.
	 (let ((alist-elt (assq (undo-entry-marker-marker undo-elt) undo-adjusted-markers)))
	   (unless alist-elt
	     (setq alist-elt (make-undo-entry-marker :marker (undo-entry-marker-marker undo-elt)
                                                     :distance (marker-position (undo-entry-marker-marker undo-elt))))
	     (setq undo-adjusted-markers
		   (cons alist-elt undo-adjusted-markers)))
	   (and (undo-entry-marker-distance alist-elt) ;(cdr alist-elt)
		(>= (undo-entry-marker-distance alist-elt) start)
		(<= (undo-entry-marker-distance alist-elt) end))))
	((typep undo-elt 'undo-entry-property) ;(null (car undo-elt))
	 ;; (nil PROPERTY VALUE BEG . END)
         (and (>= (undo-entry-property-beg undo-elt) start)
              (<= (undo-entry-property-end undo-elt) end)))
	((typep undo-elt 'undo-entry-insertion) ;(integerp (car undo-elt))
	 ;; (BEGIN . END)
	 (and (>= (undo-entry-insertion-beg undo-elt) start)
	      (<= (undo-entry-insertion-end undo-elt) end)))))

(defun undo-elt-crosses-region (undo-elt start end)
  "Test whether UNDO-ELT crosses one edge of that region START ... END.
This assumes we have already decided that UNDO-ELT
is not *inside* the region START...END."
  (cond ;; (atom undo-elt) nil)
	((typep undo-elt 'undo-entry-property) ;(null (car undo-elt))
	 ;; (nil PROPERTY VALUE BEG . END)
	 ;;(let ((tail (nthcdr 3 undo-elt)))
         (not (or (< (undo-entry-property-beg undo-elt) end)
                  (> (undo-entry-property-end undo-elt) start))))
	((typep undo-elt 'undo-entry-insertion) ;(integerp (car undo-elt))
	 ;; (BEGIN . END)
	 (not (or (< (undo-entry-insertion-beg undo-elt) end)
		  (> (undo-entry-insertion-end undo-elt) start))))))

;; Return the first affected buffer position and the delta for an undo element
;; delta is defined as the change in subsequent buffer positions if we *did*
;; the undo.
(defun undo-delta (undo-elt)
  (cond ((typep undo-elt 'undo-entry-delete) ;(stringp (car undo-elt))
         ;; (TEXT . POSITION)
         (cons (abs (undo-entry-delete-position undo-elt)) (length (undo-entry-delete-text undo-elt))))
        ((typep undo-elt 'undo-entry-insertion) ;(integerp (car undo-elt))
         ;; (BEGIN . END)
         (cons (undo-entry-insertion-beg undo-elt) (- (undo-entry-insertion-beg undo-elt) (undo-entry-insertion-end undo-elt))))
        (t
         '(0 . 0))))

(defcustom undo-ask-before-discard nil
  "If non-nil ask about discarding undo info for the current command.
Normally, Emacs discards the undo info for the current command if
it exceeds `undo-outer-limit'.  But if you set this option
non-nil, it asks in the echo area whether to discard the info.
If you answer no, there a slight risk that Emacs might crash, so
only do it if you really want to undo the command.

This option is mainly intended for debugging.  You have to be
careful if you use it for other purposes.  Garbage collection is
inhibited while the question is asked, meaning that Emacs might
leak memory.  So you should make sure that you do not wait
excessively long before answering the question."
  :type 'boolean
  :group 'undo
  :version "22.1")

(define-buffer-local *undo-extra-outer-limit* 'undo-outer-limit-truncate ;;nil
  "If non-nil, an extra level of size that's ok in an undo item.
We don't ask the user about truncating the undo list until the
current item gets bigger than this amount.

This variable only matters if `undo-ask-before-discard' is non-nil.")

;;(make-variable-buffer-local 'undo-extra-outer-limit)

;; When the first undo batch in an undo list is longer than
;; undo-outer-limit, this function gets called to warn the user that
;; the undo info for the current command was discarded.  Garbage
;; collection is inhibited around the call, so it had better not do a
;; lot of consing.
;;(setq undo-outer-limit-function 'undo-outer-limit-truncate)
(defun undo-outer-limit-truncate (size)
  (if undo-ask-before-discard
      (when (or (null *undo-extra-outer-limit*)
		(> size *undo-extra-outer-limit*))
	;; Don't ask the question again unless it gets even bigger.
	;; This applies, in particular, if the user quits from the question.
	;; Such a quit quits out of GC, but something else will call GC
	;; again momentarily.  It will call this function again,
	;; but we don't want to ask the question again.
	(setf *undo-extra-outer-limit* (+ size 50000))
	(if (let (*use-dialog-box* *track-mouse* *executing-kbd-macro* )
	      (yes-or-no-p (format nil "Buffer `~a' undo info is ~d bytes long; discard it? "
				   (buffer-name (current-buffer)) size)))
	    (progn (setf (buffer-undo-list (current-buffer)) nil)
		   (setf *undo-extra-outer-limit* nil)
		   t)
            nil))
      (progn
        (display-warning '(undo discard-info)
                         (concat
                          (format nil "Buffer `~a' undo info was ~d bytes long.~%"
                                  (buffer-name (current-buffer)) size)
                          "The undo info was discarded because it exceeded \
`undo-outer-limit'.

This is normal if you executed a command that made a huge change
to the buffer.  In that case, to prevent similar problems in the
future, set `undo-outer-limit' to a value that is large enough to
cover the maximum size of normal changes you expect a single
command to make, but not so large that it might exceed the
maximum memory allotted to Emacs.

If you did not execute any such command, the situation is
probably due to a bug and you should report it.

You can disable the popping up of this buffer by adding the entry
\(undo discard-info) to the user option `warning-suppress-types'.
")
                         :warning)
        (setf (buffer-undo-list (current-buffer)) nil)
        t)))

