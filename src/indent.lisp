;;; Indentation functions

(in-package "LICE")

(define-buffer-local *indent-line-function* 'indent-relative
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
  (let ((pt (buffer-scan-newline buffer (pt buffer) (begv buffer) -1)))
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
  (let ((pos-aref (buffer-char-to-aref buffer (pt buffer)))
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
    ;; If we're starting past the desired column, back up to beginning
    ;; of line and scan from there.
    (when (> col column)
      (setf end pos
            pos (buffer-beginning-of-line)
            pos-aref (buffer-char-to-aref buffer pos)
            col 0))
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
      (if *fill-prefix*
	  (save-excursion
	    (goto-char end)
	    (setq end (point-marker))
	    (goto-char start)
	    (let ((regexp (regexp-quote *fill-prefix*)))
	      (while (< (point) (marker-position end))
		(or (looking-at regexp)
		    (and (bolp) (eolp))
		    (insert *fill-prefix*))
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
  (declare (ignore lines window))
  ;; FIXME: its cheap but it works, for now. It all assumes there
  ;; aren't pictures or variable width fonts, etc.
  (let* ((total lines)
         (old-pt (pt))
         (win (selected-window))
         (width (window-width win nil))
         (buf (current-buffer)))
    ;; go to the beginning of the line
    (decf old-pt (mod (current-column) width))
    (while (and (< old-pt (zv))
                (> lines 0))
      (setf old-pt (1+ (buffer-scan-newline buf old-pt (+ old-pt width) 1)))
      (decf lines))
    (while (and (> old-pt (begv))
                (< lines 0))
      (setf old-pt (buffer-scan-newline buf old-pt (- old-pt width) -2))
      ;; go past the newline except at the beginning of the buffer
      (unless (= old-pt (begv))
        (incf old-pt))
      (incf lines))
    (set-point (max (begv) (min (zv) old-pt)))
    (- total lines)))

(defun indent-line-to (column)
  "Indent current line to COLUMN.
This function removes or adds spaces and tabs at beginning of line
only if necessary.  It leaves point at end of indentation."
  (back-to-indentation)
  (let ((cur-col (current-column)))
    (cond ((< cur-col column)
	   (if (>= (- column (* (/ cur-col tab-width) tab-width)) tab-width)
	       (delete-region (point)
			      (progn (skip-chars-backward " ") (point))))
	   (indent-to column))
	  ((> cur-col column) ; too far right (after tab?)
	   (delete-region (progn (move-to-column column t) (point))
			  (progn (back-to-indentation) (point)))))))

(defun current-left-margin ()
  "Return the left margin to use for this line.
This is the value of the buffer-local variable `left-margin' plus the value
of the `left-margin' text-property at the start of the line."
  (save-excursion
    (back-to-indentation)
    (max 0
	 (+ left-margin (or (get-text-property
			     (if (and (eobp) (not (bobp)))
				 (1- (point)) (point))
			     'left-margin) 0)))))

(defcommand move-to-left-margin ((&optional (n 1) (force t))
                                 :prefix)
  "Move to the left margin of the current line.
With optional argument, move forward N-1 lines first.
The column moved to is the one given by the `current-left-margin' function.
If the line's indentation appears to be wrong, and this command is called
interactively or with optional argument FORCE, it will be fixed."
  ;;(interactive (list (prefix-numeric-value current-prefix-arg) t))
  (check-type n integer)
  (beginning-of-line n)
  (skip-chars-forward " \t")
  (if (minibufferp (current-buffer))
      (if (save-excursion (beginning-of-line) (bobp))
	  (goto-char (minibuffer-prompt-end))
	(beginning-of-line))
    (let ((lm (current-left-margin))
	  (cc (current-column)))
      (cond ((> cc lm)
	     (if (> (move-to-column lm force) lm)
		 ;; If lm is in a tab and we are not forcing, move before tab
		 (backward-char 1)))
	    ((and force (< cc lm))
	     (indent-to-left-margin))))))

;; This used to be the default indent-line-function,
;; used in Fundamental Mode, Text Mode, etc.
(defun indent-to-left-margin ()
  "Indent current line to the column given by `current-left-margin'."
  (indent-line-to (current-left-margin)))

(defcommand beginning-of-line-text ((&optional n)
                                    :prefix)
  "Move to the beginning of the text on this line.
With optional argument, move forward N-1 lines first.
From the beginning of the line, moves past the left-margin indentation, the
fill-prefix, and any indentation used for centering or right-justifying the
line, but does not move past any whitespace that was explicitly inserted
\(such as a tab used to indent the first line of a paragraph)."
  (beginning-of-line n)
  (skip-chars-forward " \t")
  ;; Skip over fill-prefix.
  (if (and *fill-prefix*
	   (not (string-equal *fill-prefix* "")))
      (if (equal *fill-prefix*
		 (buffer-substring
		  (point) (min (point-max) (+ (length *fill-prefix*) (point)))))
	  (forward-char (length *fill-prefix*)))
    (if (and adaptive-fill-mode adaptive-fill-regexp
	     (looking-at adaptive-fill-regexp))
	(goto-char (match-end 0))))
  ;; Skip centering or flushright indentation
  (if (memq (current-justification) '(center right))
      (skip-chars-forward " \t")))
