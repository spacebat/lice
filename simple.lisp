(in-package :lice)

(defvar *kill-ring* nil
  "The kill ring.")

(defvar *kill-ring-max* 60
  "Maximum length of kill ring before oldest elements are thrown away.")

(defvar *kill-ring-yank-pointer* nil
  "The tail of the kill ring whose car is the last thing yanked.")

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
	  (multiple-value-bind (p lines) 
	      (buffer-scan-newline (current-buffer) 
				   (point) 0
				   ;; A little mess to figure out how
				   ;; many newlines to search for to
				   ;; give the proper output.
				   (if (zerop n)
				       n
				     (if (and (char-after (point))
					      (char= (char-after (point)) #\Newline))
					 (- n 2)
				       (1- n))))
	    (when (char= (char-after p) #\Newline)
	      (incf p))
	    (goto-char p)
	    (when (and (< n 0)
		     (zerop lines))
	      (signal 'beginning-of-buffer))	
    (+ n lines)))))

(defun current-column ()
  "Return the current column that the current buffer's point is on."
  (let ((bol (buffer-beginning-of-line)))
    (- (point) bol)))

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

(defcommand beginning-of-line ()
  "Move the point to the beginning of the line in the current buffer."
  (setf (marker-position (buffer-point (current-buffer))) (buffer-beginning-of-line)))

(defcommand end-of-line ()
  "Move the point to the end of the line in the current buffer."
  (setf (marker-position (buffer-point (current-buffer))) (buffer-end-of-line)))

(defcommand erase-buffer ((&optional buffer))
  "Erase the contents of the current buffer."
  (buffer-erase (or buffer (current-buffer))))

(defcommand execute-extended-command ((prefix)
				      :prefix)
  "Read a user command from the minibuffer."
  (let ((cmd (read-command  (case prefix
				       (1 "M-x ")
				       (4 "C-u M-x ")
				       (t (format nil "~a M-x " prefix))))))
    (if (lookup-command cmd)
	(progn
	  (setf *prefix-arg* prefix)
	  (dispatch-command cmd))
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
  (multiple-value-bind (sexpr pos) (read-from-string string)
    (if (= pos (length string))
	(message "~s" (eval sexpr))
      (error "Trailing garbage is ~a" string))))

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

(defcommand set-mark-command ()
  (set-marker (mark-marker) (point))
  (message "Mark set"))

;; (defun kill-ring-save (beg end)
;;   "Save the region to the kill ring."

(defcommand scroll-up ()
  (let ((win (get-current-window)))
    (window-scroll-up win (max 1 (- (window-height win)
				    *next-screen-context-lines*)))))

(defcommand scroll-down ()
  (let ((win (get-current-window)))
    (window-scroll-down win (max 1 (- (window-height win)
				      *next-screen-context-lines*)))))

(defcommand end-of-buffer ()
  "Move point to the end of the buffer; leave mark at previous position.
With arg N, put point N/10 of the way from the end.

If the buffer is narrowed, this command uses the beginning and size
of the accessible part of the buffer."
  (set-mark-command)
  (goto-char (point-max)))

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

(provide :lice-0.1/simple)
