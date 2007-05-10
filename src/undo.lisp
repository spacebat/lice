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
