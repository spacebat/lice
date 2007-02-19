;;; Indentation functions

(in-package "LICE")

(defun position-indentation (pos-aref buffer)
  ;; XXX: just cheap out on this puppy
  (let ((column 0))
    (loop
       (inc-aref pos-aref buffer)
       (when (>= pos-aref (zv-aref buffer))
         (return column))

       (let ((c (buffer-fetch-char pos-aref buffer)))
         (cond ((char= c #\Space)
                (incf column))
               ((char= c  #\Tab)
                ;; FIXME: tab width
                (incf column))
               (t
                ;; categories?
                (return column)))))))

(defun current-indentation (&aux (buffer (current-buffer)))
  "Return the indentation of the current line.
This is the horizontal position of the character
following any initial whitespace."
  (let ((pt (buffer-scan-newline buffer (point buffer) (begv buffer) -1)))
    (position-indentation (buffer-char-to-aref buffer pt) buffer)))

;; (defun current-column ()
;;   "Return the current column that the current buffer's point is on."
;;   (let ((bol (buffer-beginning-of-line)))
;;     (- (point) bol)))

(defun current-column (&aux (buffer (current-buffer)))
  "Return the horizontal position of point.  Beginning of line is column 0.
This is calculated by adding together the widths of all the displayed
representations of the character between the start of the previous line
and point (eg. control characters will have a width of 2 or 4, tabs
will have a variable width).
Ignores finite width of frame, which means that this function may return
values greater than (frame-width).
Whether the line is visible (if `selective-display' is t) has no effect;
however, ^M is treated as end of line when `selective-display' is t.
Text that has an invisible property is considered as having width 0, unless
`buffer-invisibility-spec' specifies that it is replaced by an ellipsis."
  (let ((pos-aref (buffer-char-to-aref buffer (point)))
        (column 0)
        c)
    (loop
       (dec-aref pos-aref buffer)
       (when (< pos-aref (begv-aref buffer))
         (return column))
       (setf c (buffer-fetch-char pos-aref buffer))
       (cond ((char= c #\Newline)
              (return column))
             ((char= c #\Tab)
              ;; TODO: tabs are > 1 column
              (incf column))
             ;; FIXME: what about control chars?
             (t (incf column))))))

(defun indent-to (column &optional (minimum 0))
  "Indent from point with tabs and spaces until COLUMN is reached.
Optional second argument MINIMUM says always do at least MINIMUM spaces
even if that goes past COLUMN; by default, MINIMUM is zero."
  (check-type column number)
  (check-type minimum number)
  (let* ((fromcol (current-column))
         (mincol (+ fromcol minimum)))
    (when (< mincol column)
      (setf mincol column))
    (when (= fromcol mincol)
      (return-from indent-to mincol))

    ;; TODO: use tabs to indent
    (insert-char #\Space column t)

    ;; TODO: cache the last known column
    mincol))
