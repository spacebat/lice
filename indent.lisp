;;; Indentation functions

(in-package "LICE")

(defvar *indent-line-function* 'indent-relative
  "Function to indent the current line.
This function will be called with no arguments.
If it is called somewhere where auto-indentation cannot be done
\(f.ex. inside a string), the function should simply return `noindent'.
Setting this function is all you need to make TAB indent appropriately.
Don't rebind TAB unless you really need to.")

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


(defcommand move-to-column ((column &optional force)
                            :prefix)
  "Move point to column COLUMN in the current line.
Interactively, COLUMN is the value of prefix numeric argument.
The column of a character is calculated by adding together the widths
as displayed of the previous characters in the line.
This function ignores line-continuation;
there is no upper limit on the column number a character can have
and horizontal scrolling has no effect.

If specified column is within a character, point goes after that character.
If it's past end of line, point goes to end of line.

Optional second argument FORCE non-nil means if COLUMN is in the
middle of a tab character, change it to spaces.
In addition, if FORCE is t, and the line is too short to reach
COLUMN, add spaces/tabs to get there.

The return value is the current column."
  (let* ((buffer (current-buffer))
         (col (current-column))
         (pos (point buffer))
         (pos-aref (buffer-char-to-aref buffer pos))
         (end (zv buffer)))
    ;; FIXME: this assumes each character is 1 column
    (while (and (< col column)
                (< pos end))
      (let ((c (buffer-fetch-char pos-aref buffer)))
        (when (char= c #\Newline)
          (return nil))
        (incf col)
        (inc-both pos pos-aref buffer)))
    (set-point pos buffer)
    (when (and force
               (< col column))
      (setf col column)
      (indent-to col))
    col))
          
(defcommand indent-relative ((&optional unindented-ok)
                             :raw-prefix)
  "Space out to under next indent point in previous nonblank line.
An indent point is a non-whitespace character following whitespace.
The following line shows the indentation points in this line.
    ^         ^    ^     ^   ^           ^      ^  ^    ^
If the previous nonblank line has no indent points beyond the
column point starts at, `tab-to-tab-stop' is done instead, unless
this command is invoked with a numeric argument, in which case it
does nothing.

See also `indent-relative-maybe'."
  ;; FIXME: abbrev mode
;;   (if (and abbrev-mode
;; 	   (eq (char-syntax (preceding-char)) :word-constituent))
;;       (expand-abbrev))
  (let ((start-column (current-column))
	indent)
    (save-excursion
      (beginning-of-line)
      (if (re-search-backward "\\n[^\\n]" :error nil) ;; XXX used to be "^[^\n]"
	  (let ((end (save-excursion (forward-line 1) (point))))
	    (move-to-column start-column)
	    ;; Is start-column inside a tab on this line?
	    (if (> (current-column) start-column)
		(backward-char 1))
	    (or (looking-at "[ \\t]")
		unindented-ok
		(skip-chars-forward (coerce '(#\^ #\Space #\Tab) 'string) end))
	    (skip-whitespace-forward end)
	    (or (= (point) end) (setq indent (current-column))))))
    (if indent
	(let ((opoint (point-marker)))
	  (indent-to indent 0)
	  (if (> opoint (point))
	      (goto-char opoint))
	  (set-marker opoint nil))
        ;; FIXME: implement and uncomment
;;       (tab-to-tab-stop)
      )))

(defvar *indent-region-function* 'lisp-indent-region
  "Short cut function to indent region using `indent-according-to-mode'.
A value of nil means really run `indent-according-to-mode' on each line.")

(defcommand indent-region ((start end &optional column)
                           :region-beginning :region-end :raw-prefix)
  "Indent each nonblank line in the region.
A numeric prefix argument specifies a column: indent each line to that column.

With no prefix argument, the command chooses one of these methods and
indents all the lines with it:

  1) If `fill-prefix' is non-nil, insert `fill-prefix' at the
     beginning of each line in the region that does not already begin
     with it.
  2) If `indent-region-function' is non-nil, call that function
     to indent the region.
  3) Indent each line as specified by the variable `indent-line-function'.

Called from a program, START and END specify the region to indent.
If the third argument COLUMN is an integer, it specifies the
column to indent to; if it is nil, use one of the three methods above."
  (message "guuh ~s ~s" column *prefix-arg*)

  (if (null column)
      (if (buffer-local :fill-prefix)
	  (save-excursion
	    (goto-char end)
	    (setq end (point-marker))
	    (goto-char start)
	    (let ((regexp (regexp-quote (buffer-local :fill-prefix))))
	      (while (< (point) (marker-position end))
		(or (looking-at regexp)
		    (and (bolp) (eolp))
		    (insert (buffer-local :fill-prefix)))
		(forward-line 1))))
          (if *indent-region-function*
              (funcall *indent-region-function* start end)
              (save-excursion
                (setq end (copy-marker end))
                (goto-char start)
                (while (< (point) (marker-position end))
                  (or (and (bolp) (eolp))
                      (funcall *indent-line-function*))
                  (forward-line 1))
                (set-marker end nil))))
      (progn
        (setq column (prefix-numeric-value column))
        (save-excursion
          (goto-char end)
          (setq end (point-marker))
          (goto-char start)
          (or (bolp) (forward-line 1))
          (while (< (point) (marker-position end))
            (delete-region (point) (progn (skip-whitespace-forward) (point)))
            (or (eolp)
                (indent-to column 0))
            (forward-line 1))
          (set-marker end nil)))))

(defun vertical-motion (lines &optional (window (selected-window)))
  "Move point to start of the screen line LINES lines down.
If LINES is negative, this means moving up.

This function is an ordinary cursor motion function
which calculates the new position based on how text would be displayed.
The new position may be the start of a line,
or just the start of a continuation line.
The function returns number of screen lines moved over;
that usually equals LINES, but may be closer to zero
if beginning or end of buffer was reached.

The optional second argument WINDOW specifies the window to use for
parameters such as width, horizontal scrolling, and so on.
The default is to use the selected window's parameters.

`vertical-motion' always uses the current buffer,
regardless of which buffer is displayed in WINDOW.
This is consistent with other cursor motion functions
and makes it possible to use `vertical-motion' in any buffer,
whether or not it is currently displayed in some window."
  (error "unimplemented")
  )