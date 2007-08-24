(in-package "LICE")

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


(defcommand end-of-line ((&optional (n 1))
                         :prefix)
"Move point to end of current line.
With argument N not nil or 1, move forward N - 1 lines first.
If point reaches the beginning or end of buffer, it stops there.
To ignore intangibility, bind `inhibit-point-motion-hooks' to t.

This function constrains point to the current field unless this moves
point to a different line than the original, unconstrained result.  If
N is nil or 1, and a rear-sticky field ends at point, the point does
not move.  To ignore field boundaries bind `inhibit-field-text-motion'
to t."
  (let (newpos)
    (loop
       (setf newpos (line-end-position n))
       (set-point newpos)
       (cond 
         ((and (> (point) newpos)
               (char= (buffer-fetch-char (1- (point)) (current-buffer)) 
                      #\Newline))
          ;; If we skipped over a newline that follows an invisible
          ;; intangible run, move back to the last tangible position
          ;; within the line.
          (set-point (1- (point)))
          (return))
         ((and (> (point) newpos)
               (< (point) (zv))
               (char/= (buffer-fetch-char (point) (current-buffer))
                       #\Newline))
          ;; If we skipped something intangible and now we're not
          ;; really at eol, keep going.
          (setf n 1))
         (t (return))))
    nil))

(defcommand execute-extended-command ((prefix)
				      :raw-prefix)
  "Read a user command from the minibuffer."
  (let* ((name (read-command  (case (prefix-numeric-value prefix)
                                (1 "M-x ")
                                (4 "C-u M-x ")
                                (t (format nil "~a M-x " prefix)))))
         (cmd (lookup-command name)))
    (if cmd
	(let ((*prefix-arg* prefix))
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
  (let ((w (frame-selected-window (selected-frame))))
    (when (typep w 'minibuffer-window)
      (error "its a minibuffer"))
    (setf buffer (get-buffer-create buffer))
    (set-buffer buffer)
    (unless norecord
      (record-buffer buffer))
    (set-window-buffer w buffer)))

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
  (let ((win (selected-window)))
    (window-scroll-up win (max 1 (or (and arg (prefix-numeric-value arg))
                                     (- (window-height win nil)
                                        *next-screen-context-lines*))))))

(defcommand scroll-down ((&optional arg)
                         :raw-prefix)
  (let ((win (selected-window)))
    (window-scroll-down win (max 1 (or (and arg (prefix-numeric-value arg))
                                       (- (window-height win nil)
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
  (split-window (selected-window)))

(defcommand split-window-horizontally ()
  (split-window (selected-window) nil t))

(defcommand switch-to-buffer-other-window ((buffer)
					   (:buffer "Switch to buffer in other window: " (buffer-name (other-buffer (current-buffer)))))
  (let* ((cw (selected-window))
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

(defun fundamental-mode ()
  (set-major-mode '*fundamental-mode*))

(defun turn-on-auto-fill ()
  "Unconditionally turn on Auto Fill mode."
  ;; FIXME: implement
  )

(define-buffer-local comment-line-break-function 'comment-indent-new-line
  "*Mode-specific function which line breaks and continues a comment.

This function is only called during auto-filling of a comment section.
The function should take a single optional argument, which is a flag
indicating whether it should use soft newlines.")

(defun do-auto-fill ()
  "This function is used as the auto-fill-function of a buffer
when Auto-Fill mode is enabled.
It returns t if it really did any work.
\(Actually some major modes use a different auto-fill function,
but this one is the default one.)"
  (let (fc justify give-up
	   (*fill-prefix* *fill-prefix*))
    (el:if (or (not (setq justify (current-justification)))
	    (null (setq fc (current-fill-column)))
	    (and (eq justify 'left)
		 (<= (current-column) fc))
	    (and auto-fill-inhibit-regexp
		 (save-excursion (beginning-of-line)
				 (looking-at auto-fill-inhibit-regexp))))
	nil ;; Auto-filling not required
      (el:if (memq justify '(full center right))
	  (save-excursion (unjustify-current-line)))

      ;; Choose a *fill-prefix* automatically.
      (when (and adaptive-fill-mode
		 (or (null *fill-prefix*) (string= *fill-prefix* "")))
	(let ((prefix
	       (fill-context-prefix
		(save-excursion (backward-paragraph 1) (point))
		(save-excursion (forward-paragraph 1) (point)))))
	  (and prefix (not (equal prefix ""))
	       ;; Use auto-indentation rather than a guessed empty prefix.
	       (not (and fill-indent-according-to-mode
			 (string-match "\\`[ \t]*\\'" prefix)))
	       (setq *fill-prefix* prefix))))

      (while (and (not give-up) (> (current-column) fc))
	;; Determine where to split the line.
	(let* (after-prefix
	       (fill-point
		(save-excursion
		  (beginning-of-line)
		  (setq after-prefix (point))
		  (and *fill-prefix*
		       (looking-at (regexp-quote *fill-prefix*))
		       (setq after-prefix (match-end 0)))
		  (move-to-column (1+ fc))
		  (fill-move-to-break-point after-prefix)
		  (point))))

	  ;; See whether the place we found is any good.
	  (el:if (save-excursion
		(goto-char fill-point)
		(or (bolp)
		    ;; There is no use breaking at end of line.
		    (save-excursion (skip-chars-forward " ") (eolp))
		    ;; It is futile to split at the end of the prefix
		    ;; since we would just insert the prefix again.
		    (and after-prefix (<= (point) after-prefix))
		    ;; Don't split right after a comment starter
		    ;; since we would just make another comment starter.
		    (and comment-start-skip
			 (let ((limit (point)))
			   (beginning-of-line)
			   (and (re-search-forward comment-start-skip
						   limit t)
				(eq (point) limit))))))
	      ;; No good place to break => stop trying.
	      (setq give-up t)
	    ;; Ok, we have a useful place to break the line.  Do it.
	    (let ((prev-column (current-column)))
	      ;; If point is at the fill-point, do not `save-excursion'.
	      ;; Otherwise, if a comment prefix or *fill-prefix* is inserted,
	      ;; point will end up before it rather than after it.
	      (el:if (save-excursion
		    (skip-chars-backward " \t")
		    (= (point) fill-point))
		  (funcall comment-line-break-function t)
		(save-excursion
		  (goto-char fill-point)
		  (funcall comment-line-break-function t)))
	      ;; Now do justification, if required
	      (el:if (not (eq justify 'left))
		  (save-excursion
		    (end-of-line 0)
		    (justify-current-line justify nil t)))
	      ;; If making the new line didn't reduce the hpos of
	      ;; the end of the line, then give up now;
	      ;; trying again will not help.
	      (el:if (>= (current-column) prev-column)
		  (setq give-up t))))))
      ;; Justify last line.
      (justify-current-line justify t t)
      t)))


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


;;; undo

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


(defcommand kill-word ((arg)
		       :prefix)
  "Kill characters forward until encountering the end of a word.
With argument, do this that many times."
  (kill-region (point) (progn (forward-word arg) (point))))

(defcommand backward-kill-word ((arg)
				:prefix)
  "Kill characters backward until encountering the end of a word.
With argument, do this that many times."
  (kill-word (- arg)))

(defcommand backward-word ((n) :prefix)
  "Move point forward ARG words (backward if ARG is negative).
Normally returns t.
If an edge of the buffer or a field boundary is reached, point is left there
and the function returns nil.  Field boundaries are not noticed if
`inhibit-field-text-motion' is non-nil."
  (forward-word (- n)))

(defcommand forward-word ((n) :prefix)
  "Move point forward ARG words (backward if ARG is negative).
Normally returns t.
If an edge of the buffer or a field boundary is reached, point is left there
and the function returns nil.  Field boundaries are not noticed if
`inhibit-field-text-motion' is non-nil."
  (labels ((isaword (c)
	     (find c +word-constituents+ :test #'char=)))
    (let ((buffer (current-buffer)))
      (cond ((> n 0)
	     (gap-move-to buffer (buffer-point-aref buffer))
	     ;; do it n times
	     (loop for i from 0 below n
		   while (let (p1 p2)
			   ;; search forward for a word constituent
			   (setf p1 (position-if #'isaword (buffer-data buffer) 
						 :start (buffer-point-aref buffer)))
			   ;; search forward for a non word constituent
			   (when p1
			     (setf p2 (position-if (complement #'isaword) (buffer-data buffer) :start p1)))
			   (if p2
			       (goto-char (buffer-aref-to-char buffer p2))
			     (goto-char (point-max)))
			   p2)))
	    ((< n 0)
	     (setf n (- n))
	     (gap-move-to buffer (buffer-point-aref buffer))
	     ;; do it n times
	     (loop for i from 0 below n
		   for start = (buffer-gap-start buffer) then (buffer-point-aref buffer)
		   while (let (p1 p2)
			   ;; search backward for a word constituent
			   (setf p1 (position-if #'isaword (buffer-data buffer) 
						 :from-end t
						 :end start))
			   ;; search backward for a non word constituent
			   (when p1
			     (setf p2 (position-if (complement #'isaword) (buffer-data buffer) :from-end t :end p1)))
			   (if p2
			       (goto-char (1+ (buffer-aref-to-char buffer p2)))
			     (goto-char (point-min)))
			   p2)))))))

(defvar line-number-mode nil
  )

(defvar column-number-mode nil
  )

(defun line-number-mode (&optional arg)
  ""
  (warn "Unimplemented line-number-mode"))

(defun column-number-mode (&optional arg)
  ""
  (warn "Unimplemented column-number-mode"))


(provide :lice-0.1/simple)
