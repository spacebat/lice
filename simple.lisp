(in-package :lice)

(defvar *kill-ring* nil
  "The kill ring.")

(defvar *kill-ring-max* 60
  "Maximum length of kill ring before oldest elements are thrown away.")

(defvar *kill-ring-yank-pointer* nil
  "The tail of the kill ring whose car is the last thing yanked.")

(defcustom *eval-expression-print-level* 4
  "Value for `print-level' while printing value in `eval-expression'.
A value of nil means no limit."
  :group 'lisp
  :type '(choice (const :tag "No Limit" nil) integer)
  :version "21.1")

(defcustom *eval-expression-print-length* 12
  "Value for `print-length' while printing value in `eval-expression'.
A value of nil means no limit."
  :group 'lisp
  :type '(choice (const :tag "No Limit" nil) integer)
  :version "21.1")

(defcustom *eval-expression-debug-on-error* t
  "If non-nil set `debug-on-error' to t in `eval-expression'.
If nil, don't change the value of `debug-on-error'."
  :group 'lisp
  :type 'boolean
  :version "21.1")

(define-condition kill-ring-empty (lice-condition)
  () (:documentation "Raised when a yank is attempted but the kill ring is empty"))

;;   (when (or (and (< n 0)
;; 		 (< (point (current-buffer)) 0))
;; 	    (> (point (current-buffer))
;; 	       (point-max)))
;;     (decf (marker-position (buffer-point (current-buffer))) n))

(defcommand forward-char ((&optional (n 1))
			  :prefix)
  "Move the point forward N characters in the current buffer."
  (incf (marker-position (buffer-point (current-buffer))) n)
  (cond ((< (point) (begv))
	 (goto-char (begv))
	 (signal 'beginning-of-buffer))
	((> (point) (zv))
	 (goto-char (zv))
	 (signal 'end-of-buffer))))

(defcommand backward-char ((&optional (n 1))
			   :prefix)
  (forward-char (- n)))

(defun buffer-beginning-of-line ()
  "Return the point in the buffer that is the beginning of the line that P is on."
  (if (or (not (char-before))
	  (char= (char-before) #\Newline))
      (point)
    (let ((bol (buffer-scan-newline (current-buffer) (point) 0 0)))
      (if (and (char= (char-after bol) #\Newline)
	       (< bol (1- (buffer-size (current-buffer)))))
	  (1+ bol)
	bol))))

(defun buffer-end-of-line ()
  "Return the point in the buffer that is the end of the line that P is on."
  (if (or (not (char-after))
	  (char= (char-after) #\Newline))
      (point)
    (let ((eol (buffer-scan-newline (current-buffer) (point) (1- (buffer-size (current-buffer))) 1)))
      ;; XXX: a bit of a kludge. if the eol char isn't a newline then it
      ;; has to be the end of the buffer, so advance the point by one,
      ;; which is the actual end of the line.
      (if (char= (char-after eol) #\Newline)
	  eol
	(1+ eol)))))

(defun forward-line (n)
  "Move n lines forward (backward if n is negative).
Precisely, if point is on line I, move to the start of line I + n.
If there isn't room, go as far as possible (no error).
Returns the count of lines left to move.  If moving forward,
that is n - number of lines moved; if backward, n + number moved.
With positive n, a non-empty line at the end counts as one line
  successfully moved (for the return value)."
  (cond ((and (> n 0)
	      (= (point) (zv)))
	 (signal 'end-of-buffer))
	((and (< n 0)
	      (= (point) (begv)))
	 (signal 'beginning-of-buffer)))
  (if (> n 0)
      (multiple-value-bind (p lines) (buffer-scan-newline (current-buffer) 
                                                          (point (current-buffer))
                                                          (1- (buffer-size (current-buffer)))
                                                          n)
        ;; Increment p by one so the point is at the beginning of the
        ;; line.
        (when (or (char= (char-after p) #\Newline)
                  (= p (1- (buffer-size (current-buffer)))))
          (incf p))
        (goto-char p)
        (when (zerop lines)
          (signal 'end-of-buffer))
        (- n lines))
      (if (and (= n 0)
               (not (char-before)))
          0
          ;; A little mess to figure out how many newlines to search
          ;; for to give the proper output.
          (let ((lines (if (and (char-after (point))
                                (char= (char-after (point)) #\Newline))
                           (- n 2)
                           (1- n))))
            (multiple-value-bind (p flines) 
                (buffer-scan-newline (current-buffer) 
                                     (point) (begv)
                                     lines)
              (when (and (char= (char-after p) #\Newline)
                         (= flines (- lines)))
                (incf p))
              (goto-char p)
              (when (and (< n 0)
                         (zerop flines))
                (signal 'beginning-of-buffer))	
              (+ n flines))))))

(defcommand self-insert-command ((arg)
				 :prefix)
  "Insert the character you type.
Whichever character you type to run this command is inserted."
  (dformat +debug-v+ "currentb: ~a ~a~%" (current-buffer) *current-buffer*)
  (if (>= arg 2)
      (insert-move-point (current-buffer) (make-string arg :initial-element (key-char *current-event*)))
    (when (> arg 0)
      (insert-move-point (current-buffer) (key-char *current-event*)))))

(defcommand newline ((&optional n)
		     :prefix)
  "Insert N new lines."
  (insert-move-point (current-buffer) (make-string (or n 1) :initial-element #\Newline)))

(defcommand open-line ((n) :prefix)
  "Insert a newline and leave point before it.
**If there is a fill prefix and/or a left-margin, insert them on the new line
**if the line would have been blank.
With arg N, insert N newlines."
  (let ((loc (point)))
    (dotimes (i n) (newline 1))
    (goto-char loc)))

(defcommand next-line ((&optional (arg 1))
		       :prefix)
  "Move cursor vertically down N lines."
  (let ((col (current-column)))
    (forward-line arg)
    (if (<= col (- (buffer-end-of-line) (point)))
	(goto-char (+ (point) col))
      (goto-char (buffer-end-of-line)))))

(defcommand previous-line ((&optional (arg 1))
			   :prefix)
  "Move cursor vertically up N lines."
  (let ((col (current-column)))
    ;; FIXME: this is all fucked
    (forward-line (- arg))
    ;;(forward-line 0)
    ;;(backward-char 1)
    ;;(forward-line 0)
    (if (<= col (- (buffer-end-of-line) (point)))
	(goto-char (+ (point) col))
      (goto-char (buffer-end-of-line)))))

(defcommand delete-backward-char ()
  "Delete the previous N characters."
  (buffer-delete (current-buffer) (point (current-buffer)) -1))

(defcommand delete-char ()
  "Delete the following N characters."
  (buffer-delete (current-buffer) (point (current-buffer)) 1))

(defun line-move-invisible-p (pos)
  "Return non-nil if the character after POS is currently invisible."
  (let ((prop
	 (get-char-property pos 'invisible)))
    (if (eq *buffer-invisibility-spec* t)
	prop
      (or (find prop *buffer-invisibility-spec*)
	  (assoc prop (remove-if 'listp *buffer-invisibility-spec*))))))

(defcustom track-eol nil
  "*Non-nil means vertical motion starting at end of line keeps to ends of lines.
This means moving to the end of each line moved onto.
The beginning of a blank line does not count as the end of a line."
  :type 'boolean
  :group 'editing-basics)

(defcustom *line-move-ignore-invisible* t
  "*Non-nil means \\[next-line] and \\[previous-line] ignore invisible lines.
Outline mode sets this."
  :type 'boolean
  :group 'editing-basics)

(defcustom-buffer-local *goal-column* nil
  "*Semipermanent goal column for vertical motion, as set by \\[set-goal-column], or nil."
  :type '(choice integer
		 (const :tag "None" nil))
  :group 'editing-basics)

(defvar *temporary-goal-column* 0
  "Current goal column for vertical motion.
It is the column where point was
at the start of current run of vertical motion commands.
When the `track-eol' feature is doing its job, the value is 9999.")

(defun line-move (arg &optional noerror to-end try-vscroll)
  "This is like line-move-1 except that it also performs
vertical scrolling of tall images if appropriate.
That is not really a clean thing to do, since it mixes
scrolling with cursor motion.  But so far we don't have
a cleaner solution to the problem of making C-n do something
useful given a tall image."
  (declare (ignore try-vscroll))
  ;; XXX: Fuckit the vertical scrolling for now
;;   (if (and auto-window-vscroll try-vscroll
;; 	   ;; But don't vscroll in a keyboard macro.
;;            ;; FIXME: kbd macros
;; ;; 	   (not defining-kbd-macro)
;; ;; 	   (not executing-kbd-macro)
;;            )
;;       (let ((forward (> arg 0))
;; 	    (part (nth 2 (pos-visible-in-window-p (point) nil t))))
;; 	(if (and (consp part)
;; 		 (> (if forward (cdr part) (car part)) 0))
;; 	    (set-window-vscroll nil
;; 				(if forward
;; 				    (+ (window-vscroll nil t)
;; 				       (min (cdr part)
;; 					    (* (frame-char-height) arg)))
;; 				  (max 0
;; 				       (- (window-vscroll nil t)
;; 					  (min (car part)
;; 					       (* (frame-char-height) (- arg))))))
;; 				t)
;; 	  (set-window-vscroll nil 0)
;; 	  (when (line-move-1 arg noerror to-end)
;; 	    (when (not forward)
;; 	      ;; Update display before calling pos-visible-in-window-p,
;; 	      ;; because it depends on window-start being up-to-date.
;; 	      (sit-for 0)
;; 	      ;; If the current line is partly hidden at the bottom,
;; 	      ;; scroll it partially up so as to unhide the bottom.
;; 	      (if (and (setq part (nth 2 (pos-visible-in-window-p
;; 					  (line-beginning-position) nil t)))
;; 		       (> (cdr part) 0))
;; 		  (set-window-vscroll nil (cdr part) t)))
;; 	    t)))
    (line-move-1 arg noerror to-end))
;; ))

(defun line-move-1 (arg &optional noerror to-end)
  "This is the guts of next-line and previous-line.
Arg says how many lines to move.
The value is t if we can move the specified number of lines."
  ;; Don't run any point-motion hooks, and disregard intangibility,
  ;; for intermediate positions.
  (declare (ignore to-end))
  (let ((*inhibit-point-motion-hooks* t)
	(opoint (point))
	(forward (> arg 0)))
    (unwind-protect
	(progn
	  (if (not (find *last-command* '(next-line previous-line)))
	      (setq *temporary-goal-column*
		    (if (and track-eol (eolp)
			     ;; Don't count beg of empty line as end of line
			     ;; unless we just did explicit end-of-line.
			     (or (not (bolp)) (eq *last-command* 'move-end-of-line)))
			9999
		      (current-column))))

	  (if (and (not (integerp *selective-display*))
		   (not *line-move-ignore-invisible*))
	      ;; Use just newline characters.
	      ;; Set ARG to 0 if we move as many lines as requested.
	      (or (if (> arg 0)
		      (progn (if (> arg 1) (forward-line (1- arg)))
			     ;; This way of moving forward ARG lines
			     ;; verifies that we have a newline after the last one.
			     ;; It doesn't get confused by intangible text.
			     (end-of-line)
			     (if (zerop (forward-line 1))
				 (setq arg 0)))
		    (and (zerop (forward-line arg))
			 (bolp)
			 (setq arg 0)))
		  (unless noerror
		    (signal (if (< arg 0)
				'beginning-of-buffer
			      'end-of-buffer)
			    nil)))
	    ;; Move by arg lines, but ignore invisible ones.
	    (let (done)
	      (while (and (> arg 0) (not done))
		;; If the following character is currently invisible,
		;; skip all characters with that same `invisible' property value.
		(while (and (not (eobp)) (line-move-invisible-p (point)))
		  (goto-char (next-char-property-change (point))))
		;; Now move a line.
		(end-of-line)
		;; If there's no invisibility here, move over the newline.
		(cond
		 ((eobp)
		  (if (not noerror)
		      (signal 'end-of-buffer)
		    (setq done t)))
		 ((and (> arg 1)  ;; Use vertical-motion for last move
		       (not (integerp *selective-display*))
		       (not (line-move-invisible-p (point))))
		  ;; We avoid vertical-motion when possible
		  ;; because that has to fontify.
		  (forward-line 1))
		 ;; Otherwise move a more sophisticated way.
		 ((zerop (vertical-motion 1))
		  (if (not noerror)
		      (signal 'end-of-buffer)
		    (setq done t))))
		(unless done
		  (setq arg (1- arg))))
	      ;; The logic of this is the same as the loop above,
	      ;; it just goes in the other direction.
	      (while (and (< arg 0) (not done))
		(beginning-of-line)
		(cond
		 ((bobp)
		  (if (not noerror)
		      (signal 'beginning-of-buffer nil)
		    (setq done t)))
		 ((and (< arg -1) ;; Use vertical-motion for last move
		       (not (integerp *selective-display*))
		       (not (line-move-invisible-p (1- (point)))))
		  (forward-line -1))
		 ((zerop (vertical-motion -1))
		  (if (not noerror)
		      (signal 'beginning-of-buffer nil)
		    (setq done t))))
		(unless done
		  (setq arg (1+ arg))
		  (while (and ;; Don't move over previous invis lines
			  ;; if our target is the middle of this line.
			  (or (zerop (or *goal-column* *temporary-goal-column*))
			      (< arg 0))
			  (not (bobp)) (line-move-invisible-p (1- (point))))
		    (goto-char (previous-char-property-change (point))))))))
	  ;; This is the value the function returns.
	  (= arg 0))

      (cond ((> arg 0)
	     ;; If we did not move down as far as desired,
	     ;; at least go to end of line.
	     (end-of-line))
	    ((< arg 0)
	     ;; If we did not move up as far as desired,
	     ;; at least go to beginning of line.
	     (beginning-of-line))
	    (t
	     (line-move-finish (or *goal-column* *temporary-goal-column*)
			       opoint forward))))))

(defun line-move-finish (column opoint forward)
  (let ((repeat t))
    (while repeat
      ;; Set REPEAT to t to repeat the whole thing.
      (setq repeat nil)

      (let (new
	    (line-beg (save-excursion (beginning-of-line) (point)))
	    (line-end
	     ;; Compute the end of the line
	     ;; ignoring effectively invisible newlines.
	     (save-excursion
	       ;; Like end-of-line but ignores fields.
	       (skip-chars-forward "^\n")
	       (while (and (not (eobp)) (line-move-invisible-p (point)))
		 (goto-char (next-char-property-change (point)))
		 (skip-chars-forward "^\n"))
	       (point))))

	;; Move to the desired column.
	(line-move-to-column column)
	(setq new (point))

	;; Process intangibility within a line.
	;; With inhibit-point-motion-hooks bound to nil, a call to
	;; goto-char moves point past intangible text.

	;; However, inhibit-point-motion-hooks controls both the
	;; intangibility and the point-entered/point-left hooks.  The
	;; following hack avoids calling the point-* hooks
	;; unnecessarily.  Note that we move *forward* past intangible
	;; text when the initial and final points are the same.
	(goto-char new)
	(let ((*inhibit-point-motion-hooks* nil))
	  (goto-char new)

	  ;; If intangibility moves us to a different (later) place
	  ;; in the same line, use that as the destination.
	  (if (<= (point) line-end)
	      (setq new (point))
	    ;; If that position is "too late",
	    ;; try the previous allowable position.
	    ;; See if it is ok.
              (progn
                (backward-char)
                (if (if forward
                        ;; If going forward, don't accept the previous
                        ;; allowable position if it is before the target line.
                        (< line-beg (point))
                        ;; If going backward, don't accept the previous
                        ;; allowable position if it is still after the target line.
                        (<= (point) line-end))
                    (setq new (point))
                    ;; As a last resort, use the end of the line.
                    (setq new line-end)))))

	;; Now move to the updated destination, processing fields
	;; as well as intangibility.
	(goto-char opoint)
	(let ((*inhibit-point-motion-hooks* nil))
	  (goto-char
	   (constrain-to-field new opoint nil t
			       'inhibit-line-move-field-capture)))

	;; If all this moved us to a different line,
	;; retry everything within that new line.
	(when (or (< (point) line-beg) (> (point) line-end))
	  ;; Repeat the intangibility and field processing.
	  (setq repeat t))))))

(defun line-move-to-column (col)
  "Try to find column COL, considering invisibility.
This function works only in certain cases,
because what we really need is for `move-to-column'
and `current-column' to be able to ignore invisible text."
  (if (zerop col)
      (beginning-of-line)
      (let ((opoint (point)))
        (move-to-column col)
        ;; move-to-column doesn't respect field boundaries.
        (goto-char (constrain-to-field (point) opoint))))

  (when (and *line-move-ignore-invisible*
	     (not (bolp)) (line-move-invisible-p (1- (point))))
    (let ((normal-location (point))
	  (normal-column (current-column)))
      ;; If the following character is currently invisible,
      ;; skip all characters with that same `invisible' property value.
      (while (and (not (eobp))
		  (line-move-invisible-p (point)))
	(goto-char (next-char-property-change (point))))
      ;; Have we advanced to a larger column position?
      (if (> (current-column) normal-column)
	  ;; We have made some progress towards the desired column.
	  ;; See if we can make any further progress.
	  (line-move-to-column (+ (current-column) (- col normal-column)))
          ;; Otherwise, go to the place we originally found
          ;; and move back over invisible text.
          ;; that will get us to the same place on the screen
          ;; but with a more reasonable buffer position.
          (progn
            (goto-char normal-location)
            (let ((line-beg (save-excursion (beginning-of-line) (point))))
              (while (and (not (bolp)) (line-move-invisible-p (1- (point))))
                (goto-char (previous-char-property-change (point) line-beg)))))))))

(defcommand beginning-of-line ((&optional (n 1))
                               :prefix)
  "Move the point to the beginning of the line in the current buffer."
  (check-type n number)
  (set-point (line-beginning-position n)))

(defcommand move-beginning-of-line ((arg)
                                    :prefix)
  "Move point to beginning of current line as displayed.
\(If there's an image in the line, this disregards newlines
which are part of the text that the image rests on.)

With argument ARG not nil or 1, move forward ARG - 1 lines first.
If point reaches the beginning or end of buffer, it stops there.
To ignore intangibility, bind `inhibit-point-motion-hooks' to t."
  (or arg (setq arg 1))

  (let ((orig (point))
	start first-vis first-vis-field-value)

    ;; Move by lines, if ARG is not 1 (the default).
    (if (/= arg 1)
	(line-move (1- arg) t))

    ;; Move to beginning-of-line, ignoring fields and invisibles.
    (skip-chars-backward "\\n\\n") ;; FIXME: was "^\n"
    (while (and (not (bobp)) (line-move-invisible-p (1- (point))))
      (goto-char (previous-char-property-change (point)))
      (skip-chars-backward "\\n\\n")) ;; FIXME: was "^\n"
    (setq start (point))

    ;; Now find first visible char in the line
    (while (and (not (eobp)) (line-move-invisible-p (point)))
      (goto-char (next-char-property-change (point))))
    (setq first-vis (point))

    ;; See if fields would stop us from reaching FIRST-VIS.
    (setq first-vis-field-value
	  (constrain-to-field first-vis orig (/= arg 1) t nil))

    (goto-char (if (/= first-vis-field-value first-vis)
		   ;; If yes, obey them.
		   first-vis-field-value
                   ;; Otherwise, move to START with attention to fields.
                   ;; (It is possible that fields never matter in this case.)
                   (constrain-to-field (point) orig
                                       (/= arg 1) t nil)))))


(defcommand end-of-line ((&optional n)
                         :prefix)
  "Move the point to the end of the line in the current buffer."
  ;; FIXME: handle prefix
  (declare (ignore n))
  (setf (marker-position (buffer-point (current-buffer))) (buffer-end-of-line)))

(defcommand erase-buffer ((&optional (buffer (current-buffer))))
  "Erase the contents of the current buffer."
  (buffer-erase buffer))

(defcommand execute-extended-command ((prefix)
				      :raw-prefix)
  "Read a user command from the minibuffer."
  (let* ((name (read-command  (case (prefix-numeric-value prefix)
                                (1 "M-x ")
                                (4 "C-u M-x ")
                                (t (format nil "~a M-x " prefix)))))
         (cmd (lookup-command name)))
    (if cmd
	(progn
	  (dispatch-command name)
          (setf *this-command* (command-name cmd)))
      (message "No Match"))))

(defcommand switch-to-buffer ((buffer &optional norecord)
			      (:buffer "Switch To Buffer: " (buffer-name (other-buffer (current-buffer)))))
  "Select buffer buffer in the current window.
If buffer does not identify an existing buffer,
then this function creates a buffer with that name.

When called from Lisp, buffer may be a buffer, a string (a buffer name),
or nil.  If buffer is nil, then this function chooses a buffer
using `other-buffer'.
Optional second arg norecord non-nil means
do not put this buffer at the front of the list of recently selected ones.
This function returns the buffer it switched to.

WARNING: This is NOT the way to work on another buffer temporarily
within a Lisp program!  Use `set-buffer' instead.  That avoids messing with
the window-buffer correspondences."
  (unless buffer
    (setf buffer (other-buffer (current-buffer))))
  (let ((w (frame-current-window (selected-frame))))
    (when (typep w 'minibuffer-window)
      (error "its a minibuffer"))
    (setf buffer (get-buffer-create buffer))
    (set-buffer buffer)
    (unless norecord
      (record-buffer buffer))
    (set-window-buffer w buffer)))

(defcommand save-buffers-kill-emacs ()
  ;; TODO: save-some-buffers
  (throw 'lice-quit t))

(defcommand kill-buffer ((buffer)
			 (:buffer "Kill buffer: " (buffer-name (current-buffer)) t))
  "Kill the buffer BUFFER.
The argument may be a buffer or may be the name of a buffer.
defaults to the current buffer.

Value is t if the buffer is actually killed, nil if user says no.

The value of `kill-buffer-hook' (which may be local to that buffer),
if not void, is a list of functions to be called, with no arguments,
before the buffer is actually killed.  The buffer to be killed is current
when the hook functions are called.

Any processes that have this buffer as the `process-buffer' are killed
with SIGHUP."
  (let* ((target (get-buffer buffer))
         (other (other-buffer target)))
    (if target
        (progn
          ;; all windows carrying the buffer need a new buffer
          (loop for w in (frame-window-list (selected-frame))
                do (when (eq (window-buffer w) target)
                     (set-window-buffer w other)))
          (setf *buffer-list* (delete target *buffer-list*)))
      (error "No such buffer ~a" buffer))))

(defun eval-echo (string)
  ;; FIXME: don't just abandon the output
  (let* ((stream (make-string-output-stream))
         (*standard-output* stream)
         (*error-output* stream)
         (*debug-io* stream))
    (multiple-value-bind (sexpr pos) (read-from-string string)
      (if (= pos (length string))
          (message "~s" (eval sexpr))
          (error "Trailing garbage is ~a" string)))))

(defun eval-print (string)
  (multiple-value-bind (sexpr pos) (read-from-string string)
    (if (= pos (length string))
	(insert (format nil "~%~s~%" (eval sexpr)))
      (error "Trailing garbage is ~a" string))))

(defcommand eval-expression ((s)
			     (:string "Eval: "))
  ;;(handler-case 
      (eval-echo s))
    ;;(error (c) (message "Eval error: ~s" c))))

(defcommand exchange-point-and-mark ()
  (let ((p (point)))
    (goto-char (marker-position (mark-marker)))
    (set-marker (mark-marker) p)))

;; FIXME: this variable is here just so code compiles. we still need
;; to implement it.
(defvar transient-mark-mode nil)

(defcommand set-mark-command ()
  (set-marker (mark-marker) (point))
  (message "Mark set"))

(defun push-mark (&optional location nomsg activate)
  "Set mark at LOCATION (point, by default) and push old mark on mark ring.
If the last global mark pushed was not in the current buffer,
also push LOCATION on the global mark ring.
Display `Mark set' unless the optional second arg NOMSG is non-nil.
In Transient Mark mode, activate mark if optional third arg ACTIVATE non-nil.

Novice Emacs Lisp programmers often try to use the mark for the wrong
purposes.  See the documentation of `set-mark' for more information.

In Transient Mark mode, this does not activate the mark."
  (declare (ignore location activate))
  ;; TODO implement
  (set-marker (mark-marker) (point))
  (unless nomsg
    (message "Mark set")))

;; (defun kill-ring-save (beg end)
;;   "Save the region to the kill ring."

(defcommand scroll-up ((&optional arg)
                       :raw-prefix)
  (let ((win (get-current-window)))
    (window-scroll-up win (max 1 (or (and arg (prefix-numeric-value arg))
                                     (- (window-height win)
                                        *next-screen-context-lines*))))))

(defcommand scroll-down ((&optional arg)
                         :raw-prefix)
  (let ((win (get-current-window)))
    (window-scroll-down win (max 1 (or (and arg (prefix-numeric-value arg))
                                       (- (window-height win)
                                          *next-screen-context-lines*))))))

(defcommand end-of-buffer ()
  "Move point to the end of the buffer; leave mark at previous position.
With arg N, put point N/10 of the way from the end.

If the buffer is narrowed, this command uses the beginning and size
of the accessible part of the buffer."
  (set-mark-command)
  (goto-char (point-max)))

(defcommand just-one-space ((&optional (n 1))
                            :prefix)
  "Delete all spaces and tabs around point, leaving one space (or N spaces)."
  (let ((orig-pos (point)))
    (skip-chars-backward (coerce '(#\Space #\Tab) 'string))
    (constrain-to-field nil orig-pos)
    (dotimes (i n)
      (if (char= (following-char) #\Space)
	  (forward-char 1)
          (insert #\Space)))
    (delete-region
     (point)
     (progn
       (skip-whitespace-forward)
       (constrain-to-field nil orig-pos t)))))

(defcommand beginning-of-buffer ()
  "Move point to the beginning of the buffer; leave mark at previous position.
With arg N, put point N/10 of the way from the beginning.

If the buffer is narrowed, this command uses the beginning and size
of the accessible part of the buffer."
  (set-mark-command)
  (goto-char (point-min)))

(defcommand split-window-vertically ()
  (split-window (get-current-window)))

(defcommand split-window-horizontally ()
  (split-window (get-current-window) nil t))

(defcommand other-window ()
  (let ((w (next-window (get-current-window) t)))
    (if w
	(select-window w)
      (message "No other window."))))

(defcommand switch-to-buffer-other-window ((buffer)
					   (:buffer "Switch to buffer in other window: " (buffer-name (other-buffer (current-buffer)))))
  (let* ((cw (get-current-window))
	 (w (or (next-window cw)
		(split-window cw))))
    (select-window w)
    (switch-to-buffer buffer)))

(defcommand keyboard-quit ()
  (signal 'quit))

;;; kill ring

(defun kill-new (string &optional replace)
    "Make STRING the latest kill in the kill ring.
Set the kill-ring-yank pointer to point to it.
Optional second argument REPLACE non-nil means that STRING will replace
the front of the kill ring, rather than being added to the list."
    (if (and replace
	     *kill-ring*)
	(setf (car *kill-ring*) string)
      (push string *kill-ring*))
    (when (> (length *kill-ring*) *kill-ring-max*)
      (setf (cdr (nthcdr (1- *kill-ring-max*) *kill-ring*)) nil))
    (setf *kill-ring-yank-pointer* *kill-ring*))

(defun copy-region-as-kill (start end &optional (buffer (current-buffer)))
  (multiple-value-setq (start end) (validate-region start end buffer))
  (kill-new (buffer-substring start end buffer)))

(defcommand kill-ring-save ()
  (copy-region-as-kill (mark) (point)))

(defcommand kill-region ((beg end)
			 :region-beginning
			 :region-end)
  "Kill between point and mark.
The text is deleted but saved in the kill ring.
The command C-y can retrieve it from there.
 (If you want to kill and then yank immediately, use M-w.)"
  (copy-region-as-kill beg end)
  (delete-region beg end))


(defcommand kill-line ()
  (kill-region (point)
	       (progn
		 (when (eobp)
		   (signal 'end-of-buffer))
		 (if (char= (buffer-char-after (current-buffer) (point)) #\Newline)
		     (forward-line 1)
		   (goto-char (buffer-end-of-line)))
		 (point))))

(defun current-kill (n &optional do-not-move)
  "Rotate the yanking point by N places, and then return that kill.
If optional arg DO-NOT-MOVE is non-nil, then don't actually move the
yanking point; just return the Nth kill forward."
  (unless *kill-ring*
    (signal 'kill-ring-empty))
  (let ((argth-kill-element 
	 (nthcdr (mod (- n (length *kill-ring-yank-pointer*))
		      (length *kill-ring*))
		 *kill-ring*)))
    (unless do-not-move
      (setf *kill-ring-yank-pointer* argth-kill-element))
    (car argth-kill-element)))
  
(defcommand yank ()
  (set-mark-command)
  (insert (current-kill 0)))

(defcommand yank-pop ()
  (unless (eq *last-command* 'yank)
    (error "Previous command was not a yank: ~a" *last-command*))
  (setf *this-command* 'yank)
  (delete-region (mark) (point))
  (insert (current-kill 1)))

;;; universal argument

(defun prefix-numeric-value (prefix)
  "Return numeric meaning of raw prefix argument RAW.
A raw prefix argument is what you get from :raw-prefix.
Its numeric meaning is what you would get from :prefix."
  ;; TODO
  (cond ((null prefix)
	 1)
	((eq prefix '-)
	 -1)
	((and (consp prefix)
	      (integerp (car prefix)))
	 (car prefix))
	((integerp prefix)
	 prefix)
	(t 1)))

(defun prefix-arg ()
  "Return numeric meaning of *prefix-arg*"
  (prefix-numeric-value *prefix-arg*))

(defun raw-prefix-arg ()
  "Return the current prefix arg in raw form."  
  *prefix-arg*)

(defvar *overriding-map-is-bound* nil)
(defvar *saved-overriding-map* nil)
(defvar *universal-argument-num-events* nil)

(defvar *universal-argument-map*
  (let ((map (make-sparse-keymap)))
    ;;(define-key map (kbd "t") 'universal-argument-other-key)
    (define-key map t 'universal-argument-other-key)
    ;;(define-key map [switch-frame] nil)
    (define-key map (kbd "C-u") 'universal-argument-more)
    (define-key map (kbd "-") 'universal-argument-minus)
    (define-key map (kbd "0") 'digit-argument)
    (define-key map (kbd "1") 'digit-argument)
    (define-key map (kbd "2") 'digit-argument)
    (define-key map (kbd "3") 'digit-argument)
    (define-key map (kbd "4") 'digit-argument)
    (define-key map (kbd "5") 'digit-argument)
    (define-key map (kbd "6") 'digit-argument)
    (define-key map (kbd "7") 'digit-argument)
    (define-key map (kbd "8") 'digit-argument)
    (define-key map (kbd "9") 'digit-argument)
;;     (define-key map [kp-0] 'digit-argument)
;;     (define-key map [kp-1] 'digit-argument)
;;     (define-key map [kp-2] 'digit-argument)
;;     (define-key map [kp-3] 'digit-argument)
;;     (define-key map [kp-4] 'digit-argument)
;;     (define-key map [kp-5] 'digit-argument)
;;     (define-key map [kp-6] 'digit-argument)
;;     (define-key map [kp-7] 'digit-argument)
;;     (define-key map [kp-8] 'digit-argument)
;;     (define-key map [kp-9] 'digit-argument)
;;     (define-key map [kp-subtract] 'universal-argument-minus)
    map)
  "Keymap used while processing \\[universal-argument].")

(defun ensure-overriding-map-is-bound ()
  "Check `*overriding-terminal-local-map*' is `*universal-argument-map*'."
  (unless *overriding-map-is-bound*
    (setf *saved-overriding-map* *overriding-terminal-local-map*
	  *overriding-terminal-local-map* *universal-argument-map*
	  *overriding-map-is-bound* t)))

(defun restore-overriding-map ()
  "Restore `*overriding-terminal-local-map*' to its saved value."
  (setf *overriding-terminal-local-map* *saved-overriding-map*
	*overriding-map-is-bound* nil))

(defcommand universal-argument ()
  (setf *prefix-arg* (list 4)
	*universal-argument-num-events* (length (this-command-keys)))
  (ensure-overriding-map-is-bound))

(defcommand universal-argument-more ((arg)
				     :raw-prefix)
  (if (consp arg)
      (setf *prefix-arg* (list (* 4 (car arg))))
    (if (eq arg '-)
	(setf *prefix-arg* (list -4))
      (progn
	(setf *prefix-arg* arg)
	(restore-overriding-map))))
  (setf *universal-argument-num-events* (length (this-command-keys))))

(defcommand negative-argument ((arg)
			       :raw-prefix)
  "Begin a negative numeric argument for the next command.
\\[universal-argument] following digits or minus sign ends the argument."
  (cond ((integerp arg)
	 (setf *prefix-arg* (- arg)))
	((eq arg '-)
	 (setf *prefix-arg* nil))
	(t
	 (setf *prefix-arg* '-)))
  (setf *universal-argument-num-events* (length (this-command-keys)))
  (ensure-overriding-map-is-bound))

(defcommand digit-argument ((arg)
			    :raw-prefix)
  "Part of the numeric argument for the next command.
\\[universal-argument] following digits or minus sign ends the argument."
  (let* ((char (last-command-char))
	 (digit (- (logand (char-code char) #o177) (char-code #\0))))
    (cond ((integerp arg)
	   (setf *prefix-arg* (+ (* arg 10)
				 (if (< arg 0) (- digit) digit))))
	  ((eq arg '-)
	   ;; Treat -0 as just -, so that -01 will work.
	   (setf *prefix-arg* (if (zerop digit) '- (- digit))))
	  (t
	   (setf *prefix-arg* digit))))
  (setf *universal-argument-num-events* (length (this-command-keys)))
  (ensure-overriding-map-is-bound))

;; For backward compatibility, minus with no modifiers is an ordinary
;; command if digits have already been entered.
(defcommand universal-argument-minus ((arg)
				      :raw-prefix)
  (if (integerp arg)
      (universal-argument-other-key arg)
    (negative-argument arg)))

;; Anything else terminates the argument and is left in the queue to be
;; executed as a command.
(defcommand universal-argument-other-key ((arg)
					  :raw-prefix)
  (setf *prefix-arg* arg)
  (let* ((keylist (this-command-keys)))
    (setf *unread-command-events* keylist))
;; 	  (append (nthcdr *universal-argument-num-events* keylist)
;; 		  *unread-command-events*)))
  ;;FIXME: (reset-this-command-lengths)
  (restore-overriding-map))


;; (defcommand append-to-buffer ((buffer :buffer "Append to buffer: " (buffer-name (other-buffer (current-buffer))))
;; 			      (start :region-beginning)
;; 			      (end :region-end))
;;   "Append to specified buffer the text of the region.
;; It is inserted into that buffer before its point.

;; When calling from a program, give three arguments:
;; buffer (or buffer name), start and end.
;; start and end specify the portion of the current buffer to be copied."
;;   (let ((oldbuf (current-buffer)))
;;     (save-excursion
;;       (let* ((append-to (get-buffer-create buffer))
;; 	     (windows (get-buffer-window-list append-to t t))
;; 	     point)
;; 	(set-buffer append-to)
;; 	(setf point (point))
;; 	(barf-if-buffer-read-only)
;; 	(insert-buffer-substring oldbuf start end)
;; 	(dolist (window windows)
;; 	  (when (= (window-point window) point)
;; 	    (set-window-point window (point))))))))

(defcommand transpose-chars ((arg)
			:prefix)
  "Interchange characters around point, moving forward one character.
With prefix arg ARG, effect is to take character before point
and drag it forward past ARG other characters (backward if ARG negative).
If no argument and at end of line, the previous two chars are exchanged."
  (and (null arg) (eolp) (forward-char -1))
  (transpose-subr 'forward-char (prefix-numeric-value arg)))

(defcommand transpose-words ((arg)
			:prefix)
  "Interchange words around point, leaving point at end of them.
With prefix arg ARG, effect is to take word before or around point
and drag it forward past ARG other words (backward if ARG negative).
If ARG is zero, the words around or after point and around or after mark
are interchanged."
  ;; FIXME: `foo a!nd bar' should transpose into `bar and foo'.
  (transpose-subr 'forward-word arg))

;; (defun transpose-sexps ((arg)
;; 			:prefix)
;;   "Like \\[transpose-words] but applies to sexps.
;; Does not work on a sexp that point is in the middle of
;; if it is a list or string."
;;   (transpose-subr
;;    (lambda (arg)
;;      ;; Here we should try to simulate the behavior of
;;      ;; (cons (progn (forward-sexp x) (point))
;;      ;;       (progn (forward-sexp (- x)) (point)))
;;      ;; Except that we don't want to rely on the second forward-sexp
;;      ;; putting us back to where we want to be, since forward-sexp-function
;;      ;; might do funny things like infix-precedence.
;;      (if (if (> arg 0)
;; 	     ;;(looking-at "\\sw\\|\\s_")
;; 	     ;; FIXME: we don't have looking-at or syntax classes. Fake it for now
;; 	     (or (alpha-char-p (char-after (point)))
;; 		 (find (char-after (point)) "*/+-%$!@&"))
;; 	   (and (not (bobp))
;; 		(save-excursion (forward-char -1) 
;; 				;; (looking-at "\\sw\\|\\s_")
;; 				;; FIXME: we don't have looking-at or syntax classes. Fake it for now
;; 				(or (alpha-char-p (char-after (point)))
;; 				    (find (char-after (point)) "*/+-%$!@&"))
;; 				)))
;; 	 ;; Jumping over a symbol.  We might be inside it, mind you.
;; 	 (progn (funcall (if (> arg 0)
;; 			     'skip-syntax-backward 'skip-syntax-forward)
;; 			 "w_")
;; 		(cons (save-excursion (forward-sexp arg) (point)) (point)))
;;        ;; Otherwise, we're between sexps.  Take a step back before jumping
;;        ;; to make sure we'll obey the same precedence no matter which direction
;;        ;; we're going.
;;        (funcall (if (> arg 0) 'skip-syntax-backward 'skip-syntax-forward) " .")
;;        (cons (save-excursion (forward-sexp arg) (point))
;; 	     (progn (while (or (forward-comment (if (> arg 0) 1 -1))
;; 			       (not (zerop (funcall (if (> arg 0)
;; 							'skip-syntax-forward
;; 						      'skip-syntax-backward)
;; 						    ".")))))
;; 		    (point)))))
;;    arg 'special))

(defcommand transpose-lines ((arg)
			     :prefix)
  "Exchange current line and previous line, leaving point after both.
With argument ARG, takes previous line and moves it past ARG lines.
With argument 0, interchanges line point is in with line mark is in."
  (transpose-subr (function
		   (lambda (arg)
		     (if (> arg 0)
			 (progn
			   ;; Move forward over ARG lines,
			   ;; but create newlines if necessary.
			   (setq arg (forward-line arg))
			   (if (char/= (preceding-char) #\Newline)
			       (setq arg (1+ arg)))
			   (if (> arg 0)
			       (newline arg)))
		       (forward-line arg))))
		  arg))

(defun transpose-subr (mover arg &optional special)
  (let ((aux (if special mover
	       (lambda (x)
		 (cons (progn (funcall mover x) (point))
		       (progn (funcall mover (- x)) (point))))))
	pos1 pos2)
    (cond
     ((= arg 0)
      (save-excursion
	(setq pos1 (funcall aux 1))
	(goto-char (mark))
	(setq pos2 (funcall aux 1))
	(transpose-subr-1 pos1 pos2))
      (exchange-point-and-mark))
     ((> arg 0)
      (setq pos1 (funcall aux -1))
      (setq pos2 (funcall aux arg))
      (transpose-subr-1 pos1 pos2)
      (goto-char (car pos2)))
     (t
      (setq pos1 (funcall aux -1))
      (goto-char (car pos1))
      (setq pos2 (funcall aux arg))
      (transpose-subr-1 pos1 pos2)))))

(defun transpose-subr-1 (pos1 pos2)
  (when (> (car pos1) (cdr pos1)) (setq pos1 (cons (cdr pos1) (car pos1))))
  (when (> (car pos2) (cdr pos2)) (setq pos2 (cons (cdr pos2) (car pos2))))
  (when (> (car pos1) (car pos2))
    (let ((swap pos1))
      (setq pos1 pos2 pos2 swap)))
  (if (> (cdr pos1) (car pos2)) (error "Don't have two things to transpose"))
;;   (atomic-change-group
   (let (word2)
     ;; FIXME: We first delete the two pieces of text, so markers that
     ;; used to point to after the text end up pointing to before it :-(
     (setq word2 (delete-and-extract-region (car pos2) (cdr pos2)))
     (goto-char (car pos2))
     (insert (delete-and-extract-region (car pos1) (cdr pos1)))
     (goto-char (car pos1))
     (insert word2)))

;;; 

(defcustom-buffer-local *fill-prefix* nil
  "*String for filling to insert at front of new line, or nil for none."
  :type '(choice (const :tag "None" nil)
		 string)
  :group 'fill)

(defvar *fundamental-mode* 
  (make-instance 'major-mode
                 :name "Fundamental")
"Major mode not specialized for anything in particular.
Other major modes are defined by comparison with this one.")

(defun turn-on-auto-fill ()
  "Unconditionally turn on Auto Fill mode."
  ;; FIXME: implement
  )


;; FIXME: put this info in the following condition
;; (put 'mark-inactive 'error-conditions '(mark-inactive error))
;; (put 'mark-inactive 'error-message "The mark is not active now")

(define-condition mark-inactive (lice-condition)
  ())

(defvar activate-mark-hook nil
  "Hook run when the mark becomes active.
It is also run at the end of a command, if the mark is active and
it is possible that the region may have changed")

(defvar deactivate-mark-hook nil
  "Hook run when the mark becomes inactive.")

(defun mark (&optional force)
  "Return this buffer's mark value as integer, or nil if never set.

In Transient Mark mode, this function signals an error if
the mark is not active.  However, if `mark-even-if-inactive' is non-nil,
or the argument FORCE is non-nil, it disregards whether the mark
is active, and returns an integer or nil in the usual way.

If you are using this in an editing command, you are most likely making
a mistake; see the documentation of `set-mark'."
  (if (or force (not transient-mark-mode) mark-active mark-even-if-inactive)
      (marker-position (mark-marker))
    (signal 'mark-inactive nil)))

;; ;; Many places set mark-active directly, and several of them failed to also
;; ;; run deactivate-mark-hook.  This shorthand should simplify.
;; (defsubst deactivate-mark ()
;;   "Deactivate the mark by setting `mark-active' to nil.
;; \(That makes a difference only in Transient Mark mode.)
;; Also runs the hook `deactivate-mark-hook'."
;;   (cond
;;    ((eq transient-mark-mode 'lambda)
;;     (setq transient-mark-mode nil))
;;    (transient-mark-mode
;;     (setq mark-active nil)
;;     (run-hooks 'deactivate-mark-hook))))

(defun set-mark (pos)
  "Set this buffer's mark to POS.  Don't use this function!
That is to say, don't use this function unless you want
the user to see that the mark has moved, and you want the previous
mark position to be lost.

Normally, when a new mark is set, the old one should go on the stack.
This is why most applications should use `push-mark', not `set-mark'.

Novice Emacs Lisp programmers often try to use the mark for the wrong
purposes.  The mark saves a location for the user's convenience.
Most editing commands should not alter the mark.
To remember a location for internal use in the Lisp program,
store it in a Lisp variable.  Example:

   (let ((beg (point))) (forward-line 1) (delete-region beg (point)))."

  (if pos
      (progn
	(setq mark-active t)
	(run-hooks 'activate-mark-hook)
	(set-marker (mark-marker) pos (current-buffer)))
      ;; Normally we never clear mark-active except in Transient Mark mode.
      ;; But when we actually clear out the mark value too,
      ;; we must clear mark-active in any mode.
      (progn
        (setq mark-active nil)
        (run-hooks 'deactivate-mark-hook)
        (set-marker (mark-marker) nil))))

(define-buffer-local mark-ring nil
  "The list of former marks of the current buffer, most recent first.")
(make-variable-buffer-local 'mark-ring)
(setf (get 'mark-ring 'permanent-local) t)

(defcustom mark-ring-max 16
  "*Maximum size of mark ring.  Start discarding off end if gets this big."
  :type 'integer
  :group 'editing-basics)

(defvar global-mark-ring nil
  "The list of saved global marks, most recent first.")

(defcustom global-mark-ring-max 16
  "*Maximum size of global mark ring.  \
Start discarding off end if gets this big."
  :type 'integer
  :group 'editing-basics)

(defcommand pop-to-mark-command ()
  "Jump to mark, and pop a new position for mark off the ring
\(does not affect global mark ring\)."
  (if (null (mark t))
      (error "No mark set in this buffer")
      (progn
        (goto-char (mark t))
        (pop-mark))))

;; (defun push-mark-command (arg &optional nomsg)
;;   "Set mark at where point is.
;; If no prefix arg and mark is already set there, just activate it.
;; Display `Mark set' unless the optional second arg NOMSG is non-nil."
;;   (interactive "P")
;;   (let ((mark (marker-position (mark-marker))))
;;     (if (or arg (null mark) (/= mark (point)))
;; 	(push-mark nil nomsg t)
;;       (setq mark-active t)
;;       (run-hooks 'activate-mark-hook)
;;       (unless nomsg
;; 	(message "Mark activated")))))

(defcustom set-mark-command-repeat-pop nil
  "*Non-nil means that repeating \\[set-mark-command] after popping will pop.
This means that if you type C-u \\[set-mark-command] \\[set-mark-command]
will pop twice."
  :type 'boolean
  :group 'editing)

;; (defun set-mark-command (arg)
;;   "Set mark at where point is, or jump to mark.
;; With no prefix argument, set mark, and push old mark position on local
;; mark ring; also push mark on global mark ring if last mark was set in
;; another buffer.  Immediately repeating the command activates
;; `transient-mark-mode' temporarily.

;; With argument, e.g. \\[universal-argument] \\[set-mark-command], \
;; jump to mark, and pop a new position
;; for mark off the local mark ring \(this does not affect the global
;; mark ring\).  Use \\[pop-global-mark] to jump to a mark off the global
;; mark ring \(see `pop-global-mark'\).

;; If `set-mark-command-repeat-pop' is non-nil, repeating
;; the \\[set-mark-command] command with no prefix pops the next position
;; off the local (or global) mark ring and jumps there.

;; With a double \\[universal-argument] prefix argument, e.g. \\[universal-argument] \
;; \\[universal-argument] \\[set-mark-command], unconditionally
;; set mark where point is.

;; Setting the mark also sets the \"region\", which is the closest
;; equivalent in Emacs to what some editors call the \"selection\".

;; Novice Emacs Lisp programmers often try to use the mark for the wrong
;; purposes.  See the documentation of `set-mark' for more information."
;;   (interactive "P")
;;   (if (eq transient-mark-mode 'lambda)
;;       (setq transient-mark-mode nil))
;;   (cond
;;    ((and (consp arg) (> (prefix-numeric-value arg) 4))
;;     (push-mark-command nil))
;;    ((not (eq this-command 'set-mark-command))
;;     (if arg
;; 	(pop-to-mark-command)
;;       (push-mark-command t)))
;;    ((and set-mark-command-repeat-pop
;; 	 (eq last-command 'pop-to-mark-command))
;;     (setq this-command 'pop-to-mark-command)
;;     (pop-to-mark-command))
;;    ((and set-mark-command-repeat-pop
;; 	 (eq last-command 'pop-global-mark)
;; 	 (not arg))
;;     (setq this-command 'pop-global-mark)
;;     (pop-global-mark))
;;    (arg
;;     (setq this-command 'pop-to-mark-command)
;;     (pop-to-mark-command))
;;    ((and (eq last-command 'set-mark-command)
;; 	 mark-active (null transient-mark-mode))
;;     (setq transient-mark-mode 'lambda)
;;     (message "Transient-mark-mode temporarily enabled"))
;;    (t
;;     (push-mark-command nil))))

;; (defun push-mark (&optional location nomsg activate)
;;   "Set mark at LOCATION (point, by default) and push old mark on mark ring.
;; If the last global mark pushed was not in the current buffer,
;; also push LOCATION on the global mark ring.
;; Display `Mark set' unless the optional second arg NOMSG is non-nil.
;; In Transient Mark mode, activate mark if optional third arg ACTIVATE non-nil.

;; Novice Emacs Lisp programmers often try to use the mark for the wrong
;; purposes.  See the documentation of `set-mark' for more information.

;; In Transient Mark mode, this does not activate the mark."
;;   (unless (null (mark t))
;;     (setq mark-ring (cons (copy-marker (mark-marker)) mark-ring))
;;     (when (> (length mark-ring) mark-ring-max)
;;       (move-marker (car (nthcdr mark-ring-max mark-ring)) nil)
;;       (setcdr (nthcdr (1- mark-ring-max) mark-ring) nil)))
;;   (set-marker (mark-marker) (or location (point)) (current-buffer))
;;   ;; Now push the mark on the global mark ring.
;;   (if (and global-mark-ring
;; 	   (eq (marker-buffer (car global-mark-ring)) (current-buffer)))
;;       ;; The last global mark pushed was in this same buffer.
;;       ;; Don't push another one.
;;       nil
;;     (setq global-mark-ring (cons (copy-marker (mark-marker)) global-mark-ring))
;;     (when (> (length global-mark-ring) global-mark-ring-max)
;;       (move-marker (car (nthcdr global-mark-ring-max global-mark-ring)) nil)
;;       (setcdr (nthcdr (1- global-mark-ring-max) global-mark-ring) nil)))
;;   (or nomsg executing-kbd-macro (> (minibuffer-depth) 0)
;;       (message "Mark set"))
;;   (if (or activate (not transient-mark-mode))
;;       (set-mark (mark t)))
;;   nil)

;; (defun pop-mark ()
;;   "Pop off mark ring into the buffer's actual mark.
;; Does not set point.  Does nothing if mark ring is empty."
;;   (when mark-ring
;;     (setq mark-ring (nconc mark-ring (list (copy-marker (mark-marker)))))
;;     (set-marker (mark-marker) (+ 0 (car mark-ring)) (current-buffer))
;;     (move-marker (car mark-ring) nil)
;;     (if (null (mark t)) (ding))
;;     (setq mark-ring (cdr mark-ring)))
;;   (deactivate-mark))

(defcommand back-to-indentation ()
  "Move point to the first non-whitespace character on this line."
  (beginning-of-line 1)
  (skip-syntax-forward '(:whitespace) (line-end-position))
  ;; Move back over chars that have whitespace syntax but have the p flag.
  (backward-prefix-chars))

(provide :lice-0.1/simple)
