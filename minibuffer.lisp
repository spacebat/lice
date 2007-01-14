(in-package :lice)

(defvar *history-length* 30
  "Maximum length for history lists before truncation takes place.
A number means that length; t means infinite.  Truncation takes place
just after a new element is inserted.  Setting the :HISTORY-LENGTH
property of a history variable overrides this default.")

(defvar *minibuffer-text-before-history* nil
  "Text that was in this minibuffer before any history commands.
This is nil if there have not yet been any history commands
in this use of the minibuffer.")

(defclass minibuffer-window (window)
  ())

(define-major-mode minibuffer-read-mode
  (:name "minibuffer mode"
   :map (let ((m (make-sparse-keymap)))
	  (define-key m (make-instance 'key :char #\m :control t) 'exit-minibuffer)
	  (define-key m (make-instance 'key :char #\Newline) 'exit-minibuffer)
	  (define-key m (make-instance 'key :char #\Return) 'exit-minibuffer)
          (define-key m (make-instance 'key :char #\j :control t) 'exit-minibuffer)
          (define-key m (make-instance 'key :char #\p :meta t) 'previous-history-element)
          (define-key m (make-instance 'key :char #\n :meta t) 'next-history-element)
	  (define-key m (make-instance 'key :char #\g :control t) 'abort-recursive-edit)
	  m))
  "minibuffer read mode"
  ;; empty init
  )

(define-major-mode minibuffer-complete-mode
  (:name "minibuffer mode"
   :map (let ((m (make-sparse-keymap)))
	  (define-key m (make-instance 'key :char #\m :control t) 'minibuffer-complete-and-exit)
	  (define-key m (make-instance 'key :char #\Newline) 'minibuffer-complete-and-exit)
	  (define-key m (make-instance 'key :char #\Return) 'minibuffer-complete-and-exit)
          (define-key m (make-instance 'key :char #\j :control t) 'minibuffer-complete-and-exit)
          (define-key m (make-instance 'key :char #\p :meta t) 'previous-history-element)
          (define-key m (make-instance 'key :char #\n :meta t) 'next-history-element)
          (define-key m (make-instance 'key :char #\i :control t) 'minibuffer-complete)
          (define-key m (make-instance 'key :char #\Tab) 'minibuffer-complete)
	  (define-key m (make-instance 'key :char #\g :control t) 'abort-recursive-edit)
	  m))
  "minibuffer complete mode"
  ;; empty init
  )

(defun make-minibuffer (major-mode)
  "Return a fresh minibuffer with major mode, MAJOR-MODE."
  ;; FIXME: Emacs prefixes it with a space so it doesn't show up in
  ;; buffer listings. How are we gonna do this?
  (let ((mb (get-buffer-create (generate-new-buffer-name " *minibuffer*"))))
    (setf (buffer-major-mode mb) major-mode
          (buffer-mode-line mb) nil)
    mb))

(defun make-minibuffer-window (height cols)
  (let* ((w (make-instance 'minibuffer-window
			   :x 0 :y (- height 1) :w cols :h 1
			   :line-state (make-array 1 :fill-pointer 1
						   :element-type 'integer :initial-element -1)
			   :cache (make-instance 'line-cache :valid t)
			   :top-line 0
			   :bottom-line 0
			   :point-col 0
			   :point-line 0
			   :buffer (make-minibuffer minibuffer-read-mode)
			   :top (make-marker)
			   :bottom (make-marker)
			   :bpoint (make-marker)
			   :point-col 0
			   :point-line 0)))
    (set-marker (window-top w) 0 (window-buffer w))
    (set-marker (window-bottom w) 0 (window-buffer w))
    w))

;; (defun clear-minibuffer ()
;;   "Erase the contents of the minibuffer when it isn't active."
;;   (let ((minibuffer (window-buffer (frame-minibuffer-window (selected-frame)))))
;;     (erase-buffer minibuffer)))

(defun minibuffer-window (&optional (frame (selected-frame)))
  "Return the window used now for minibuffers.
If the optional argument FRAME is specified, return the minibuffer window
used by that frame."
  (frame-minibuffer-window frame))

(defun message (string &rest arguments)
  "Print a one-line message at the bottom of the screen."
  ;; FIXME: properly implement the echo area
  (when (zerop (frame-minibuffers-active (selected-frame)))
    (let ((minibuffer (window-buffer (frame-minibuffer-window (selected-frame))))
          (msg (apply #'format nil string arguments)))
      (erase-buffer minibuffer)
      (buffer-insert minibuffer msg)
      (with-current-buffer (get-buffer-create "*messages*")
        (goto-char (point-max))
        (insert msg #\Newline)))))

(defun clear-minibuffer ()
  "Erase the text in the minibuffer, unless it's active."
  (when (zerop (frame-minibuffers-active (selected-frame)))
    (erase-buffer (window-buffer (frame-minibuffer-window (selected-frame))))))

(defun show-minibuffer-prompt (frame prompt)
  "Show PROMPT in the minibuffer. Flip a bit in FRAME to allow
switching to the minibuffer."
  (declare (type string prompt)
           (ignore frame))
  (let ((minibuffer (window-buffer (frame-minibuffer-window (selected-frame))))
	(field (npropertize prompt 'field 't 'front-sticky t 'rear-nonsticky t)))
    (dformat +debug-v+ "~a~%" field)
    (erase-buffer minibuffer)
    (buffer-insert minibuffer field)))

(defun minibuffer-prompt-end (&optional (minibuf (current-buffer)))
  "Return the buffer position of the end of the minibuffer prompt.
Return (point-min) if current buffer is not a mini-buffer."
  (let ((beg (begv minibuf)))
    (multiple-value-bind (start end) (find-field beg nil :beg t :end t :buf minibuf)
      (dformat +debug-v+ "exit-mb: ~a ~a ~a~%" start end (buffer-size minibuf))
      (if (and (= end (zv minibuf))
	       (null (get-char-property beg 'field minibuf)))
	  beg
	end))))
  
(defun minibuffer-contents (&optional (minibuf (current-buffer)))
  "Return the user input in a minbuffer as a string.
If MINIBUF is omitted, default to the current buffer.
MINIBUF must be a minibuffer."
  (buffer-substring (minibuffer-prompt-end minibuf) (zv minibuf) minibuf))

(defun minibuffer-contents-no-properties (&optional (minibuf (current-buffer)))
  "Return the user input in a minbuffer as a string.
If MINIBUF is omitted, default to the current buffer.
MINIBUF must be a minibuffer."
  (buffer-substring-no-properties (minibuffer-prompt-end minibuf) (zv minibuf) minibuf))

(defun delete-minibuffer-contents (&optional (minibuf (current-buffer)))
  "Delete all user input in a minibuffer.
MINIBUF must be a minibuffer."
  (let ((end (minibuffer-prompt-end minibuf)))
    (when (< end (zv minibuf))
      (delete-region end (zv minibuf)))))

(defun setup-minibuffer-for-read (major-mode prompt initial-contents history)
  (save-window-excursion
    ;; Create a new minibuffer
    (let* ((frame (selected-frame))
	   (*minibuffer-history-variable* history)
	   (*minibuffer-history-position* 0)
	   (*minibuffer-text-before-history* nil)
	   (old-minibuffer (window-buffer (frame-minibuffer-window frame)))
	   (new-minibuffer (make-minibuffer major-mode)))
    (window-save-point (get-current-window))
    ;; attach it to the current frame
    (set-window-buffer (frame-minibuffer-window frame) new-minibuffer)
    (select-window (frame-minibuffer-window frame))
    ;; Show the prompt
    (show-minibuffer-prompt frame prompt)
    ;; move to the end of input
    (setf (marker-position (buffer-point new-minibuffer)) (buffer-size new-minibuffer))
    (when initial-contents (insert initial-contents))
    ;; enter recursive edit
    (dformat +debug-v+ "ya ohoe~%")
    (incf (frame-minibuffers-active frame))
    (unwind-protect
	(progn
	  (recursive-edit)
	  (let* ((val (minibuffer-contents new-minibuffer))
		 (hist-string (when (> (length val) 0)
				val)))
	    (when (and *minibuffer-history-variable* hist-string)
	      (let ((hist-val (symbol-value *minibuffer-history-variable*)))
		;; If the caller wanted to save the value read on a history list,
		;; then do so if the value is not already the front of the list.
		(when (or (null hist-val)
			  (and (consp hist-val)
			       (not (equal hist-string (car hist-val)))))
		  (push hist-string hist-val)
		  (setf (symbol-value *minibuffer-history-variable*) hist-val))
		;; truncate if requested
		(let ((len (or (get *minibuffer-history-variable* :history-length)
			       *history-length*)))
		  (when (integerp len)
		    (if (< len 0)
			(setf (symbol-value *minibuffer-history-variable*) nil)
		      (let ((tmp (nthcdr len hist-val)))
			(when tmp
			  (rplacd tmp nil))))))))
		;; return the value
		val))
      ;; Restore the buffer
      (dformat +debug-v+ "minibuffer~%")
      (set-window-buffer (frame-minibuffer-window frame) old-minibuffer)
      (kill-buffer new-minibuffer)
      (decf (frame-minibuffers-active frame))))))

(defun read-from-minibuffer (prompt &key initial-contents keymap read (history '*minibuffer-history*) default-value)
  "Read a string from the minibuffer, prompting with string PROMPT.
The optional second arg INITIAL-CONTENTS is an obsolete alternative to
  DEFAULT-VALUE.  It normally should be nil in new code, except when
  HISTORY is a cons.  It is discussed in more detail below.
Third arg KEYMAP is a keymap to use whilst reading;
  if omitted or nil, the default is `minibuffer-local-map'.
If fourth arg READ is non-nil, then interpret the result as a Lisp object
  and return that object:
  in other words, do `(car (read-from-string INPUT-STRING))'
Fifth arg HISTORY, if non-nil, specifies a history list and optionally
  the initial position in the list.  It can be a symbol, which is the
  history list variable to use, or it can be a cons cell
  (HISTVAR . HISTPOS).  In that case, HISTVAR is the history list variable
  to use, and HISTPOS is the initial position for use by the minibuffer
  history commands.  For consistency, you should also specify that
  element of the history as the value of INITIAL-CONTENTS.  Positions
  are counted starting from 1 at the beginning of the list.
Sixth arg DEFAULT-VALUE is the default value.  If non-nil, it is available
  for history commands; but, unless READ is non-nil, `read-from-minibuffer'
  does NOT return DEFAULT-VALUE if the user enters empty input!  It returns
  the empty string.
Seventh arg INHERIT-INPUT-METHOD, if non-nil, means the minibuffer inherits
 the current input method and the setting of `enable-multibyte-characters'.
Eight arg KEEP-ALL, if non-nil, says to put all inputs in the history list,
 even empty or duplicate inputs.
If the variable `minibuffer-allow-text-properties' is non-nil,
 then the string which is returned includes whatever text properties
 were present in the minibuffer.  Otherwise the value has no text properties.

The remainder of this documentation string describes the
INITIAL-CONTENTS argument in more detail.  It is only relevant when
studying existing code, or when HISTORY is a cons.  If non-nil,
INITIAL-CONTENTS is a string to be inserted into the minibuffer before
reading input.  Normally, point is put at the end of that string.
However, if INITIAL-CONTENTS is \(STRING . POSITION), the initial
input is STRING, but point is placed at _one-indexed_ position
POSITION in the minibuffer.  Any integer value less than or equal to
one puts point at the beginning of the string.  *Note* that this
behavior differs from the way such arguments are used in `completing-read'
and some related functions, which use zero-indexing for POSITION."
  (declare (ignore default-value read keymap))
  (setup-minibuffer-for-read minibuffer-read-mode prompt initial-contents history))

(defun tree-find (tree obj &key (test #'eq))
  "find OBJ in TREE. Return the OBJ or nil."
  (cond ((typep tree obj)
	 (when (funcall test tree obj)
	   tree))
	(t (or (tree-find (car tree) obj :test test)
	       (tree-find (cdr tree) obj :test test)))))

(defun tree-sibling (tree obj &key (test #'eq))
  "Return the OBJ's sibling in tree or nil."
  (declare (type (or list window) tree))
  (cond ((typep tree obj)
	 nil)
	((funcall test obj (car tree))
	 (cdr tree))
	((funcall test obj (cdr tree))
	 (car tree))
	(t (or (tree-sibling (car tree) obj :test test)
	       (tree-sibling (cdr tree) obj :test test)))))

(defun frame-for-window (window)
  "Return the frame that holds WINDOW."
  (find-if (lambda (f)
	     (tree-find (frame-window-tree f) window)) *frame-list*))

(defcommand ask-user ()
  ""
  (message "user typed: ~a" (read-from-minibuffer "input: ")))

(defcommand exit-minibuffer ()
  ""
  (dformat +debug-v+ "exit-minibuffer~%")
  (throw 'exit nil))

(defcommand abort-recursive-edit ()
  (throw 'exit t))

(defgeneric all-completions (string alist &optional predicate hide-spaces)
  (:documentation "Return a list of possible matches."))

(defmethod all-completions (string (alist list) &optional predicate hide-spaces)
  (declare (ignore hide-spaces))
  (let ((tester (or predicate
		    (lambda (s)
		      (string= string s :end2 (min (length string)
						   (length s)))))))
    (loop for elt in alist
	  for i = (cond ((consp elt)
			 (car elt))
			((symbolp elt)
			 ;; FIXME: this is a hack. isn't there a
			 ;; global that decides whether they're
			 ;; printed upcase or not?
			 (string-downcase (symbol-name elt)))
			(t elt))
	  when (funcall tester i)
	  collect i)))

(defmethod all-completions (string (fn function) &optional predicate hide-spaces)
  (declare (ignore hide-spaces))
  (funcall fn string predicate nil))

(defun try-completion (string alist &optional predicate)
  (labels ((all-are-good (match strings)
	     (loop for i in strings
		   never (string/= match i :end2 (min (length match)
						      (length i))))))
    (let* ((possibles (all-completions string alist predicate))
	   (match (make-array 100 ; MOVITZ: the match can't be more than 100 chars
			      :element-type 'character
			      :fill-pointer 0
			      ;; :adjustable t
			      )))
      ;; FIXME: this dubplicates effort since the first (length string)
      ;; chars will be the same.
      (when possibles
	(loop for i from 0 below (length (first possibles))
	      do (vector-push-extend (char (first possibles) i) match)
	      unless (all-are-good match possibles)
              do (progn
                   (decf (fill-pointer match))
                   (return)))
	match))))

(defcommand minibuffer-complete-and-exit ()
  ;; FIXME: this should be done properly
  (throw 'exit nil))
  
(defvar *minibuffer-completion-table* nil
  "Alist or obarray used for completion in the minibuffer.
This becomes the ALIST argument to `try-completion' and `all-completions'.
The value can also be a list of strings or a hash table.

The value may alternatively be a function, which is given three arguments:
  STRING, the current buffer contents;
  PREDICATE, the predicate for filtering possible matches;
  CODE, which says what kind of things to do.
CODE can be nil, t or `lambda'.
nil means to return the best completion of STRING, or nil if there is none.
t means to return a list of all possible completions of STRING.
`lambda' means to return t if STRING is a valid completion as it stands.")

(defvar *minibuffer-history* nil
  "Default minibuffer history list.
This is used for all minibuffer input
except when an alternate history list is specified.")

(defvar *minibuffer-history-position* nil
  "Current position of redoing in the history list.")

(defvar *minibuffer-history-variable* '*minibuffer-history*
  "History list symbol to add minibuffer values to.
Each string of minibuffer input, as it appears on exit from the minibuffer,
is added with
**  (set minibuffer-history-variable
**  (cons STRING (symbol-value minibuffer-history-variable)))")

(defvar *minibuffer-completion-predicate* nil
  "Within call to `completing-read', this holds the PREDICATE argument.")

(define-condition history-end (lice-condition)
  () (:documentation "raised when at the end of the history"))

(define-condition history-beginning (lice-condition)
  () (:documentation "raised when at the begining of the history"))
	   
(defcommand next-history-element ((&optional n)
				  :prefix)
  (let ((narg (- *minibuffer-history-position* n))
	(minimum 0)
	elt)
    (when (and (zerop *minibuffer-history-position*)
	       (null *minibuffer-text-before-history*))
	(setf *minibuffer-text-before-history*
	      (minibuffer-contents-no-properties)))
    (when (< narg minimum)
      (signal 'history-end #|"End of history; no next item"|#))
    (when (> narg (length (symbol-value *minibuffer-history-variable*)))
      (signal 'history-beginning #|"Beginning of history; no preceding item"|#))
    (goto-char (point-max))
    (delete-minibuffer-contents)
    (setf *minibuffer-history-position* narg)
    (cond ((= narg 0)
	   (setf elt (or *minibuffer-text-before-history* "")
		 *minibuffer-text-before-history* nil))
	  (t 
	   (setf elt (nth (1- *minibuffer-history-position*)
			  (symbol-value *minibuffer-history-variable*)))))
    (insert
;;      (if (and (eq minibuffer-history-sexp-flag (minibuffer-depth))
;; 		  (not minibuffer-returned-to-present))
;; 	     (let ((*print-level* nil))
;; 	       (prin1-to-string elt))
     elt)
    (goto-char (point-max))))


(defcommand previous-history-element ()
  (next-history-element -1))

(defcommand minibuffer-complete ()
  (let* ((txt (minibuffer-contents))
         (match (try-completion txt *minibuffer-completion-table*)))
    (dformat +debug-v+ "txt: ~a match: ~a~%" txt match)
    (when match
      (if (= (length match)
             (length txt))
          ;; no new text was added, so list the possibilities
          (let* ((txt (minibuffer-contents))
                 (strings (all-completions txt *minibuffer-completion-table*)))
            (with-current-buffer (get-buffer-create "*Completions*")
              (erase-buffer)
              (insert (format nil "Here are the completions.~%"))
              (loop for c in strings
                    do (insert (format nil "~a~%" c)))
              (goto-char (point-min))
              (display-buffer (current-buffer))))
        (progn
          (goto-char (point-max))
          (insert (subseq match (length txt))))))))

(defun completing-read (prompt table &key predicate require-match
			       initial-input (history '*minibuffer-history*) def)
  "Read a string in the minibuffer, with completion.
PROMPT is a string to prompt with; normally it ends in a colon and a space.
TABLE is an alist whose elements' cars are strings, or an obarray.
TABLE can also be a function to do the completion itself.
PREDICATE limits completion to a subset of TABLE.
See `try-completion' and `all-completions' for more details
 on completion, TABLE, and PREDICATE.

If REQUIRE-MATCH is non-nil, the user is not allowed to exit unless
 the input is (or completes to) an element of TABLE or is null.
 If it is also not t, Return does not exit if it does non-null completion.
If the input is null, `completing-read' returns an empty string,
 regardless of the value of REQUIRE-MATCH.

If INITIAL-INPUT is non-nil, insert it in the minibuffer initially.
  If it is (STRING . POSITION), the initial input
  is STRING, but point is placed POSITION characters into the string.
  This feature is deprecated--it is best to pass nil for INITIAL.
HISTORY, if non-nil, specifies a history list
  and optionally the initial position in the list.
  It can be a symbol, which is the history list variable to use,
  or it can be a cons cell (HISTVAR . HISTPOS).
  In that case, HISTVAR is the history list variable to use,
  and HISTPOS is the initial position (the position in the list
  which INITIAL-INPUT corresponds to).
  Positions are counted starting from 1 at the beginning of the list.
DEF, if non-nil, is the default value."
  (declare (ignore require-match def))
  (let ((*minibuffer-completion-table* table)
	(*minibuffer-completion-predicate* predicate))
    (setup-minibuffer-for-read minibuffer-complete-mode prompt initial-input history)))

;; (defun y-or-n-p (prompt)
;;   "Ask user a \"y or n\" question.  Return t if answer is \"y\".
;; Takes one argument, which is the string to display to ask the question.
;; It should end in a space; `y-or-n-p' adds `(y or n) ' to it.
;; No confirmation of the answer is requested; a single character is enough.
;; Also accepts Space to mean yes, or Delete to mean no.  (Actually, it uses
;; the bindings in `query-replace-map'; see the documentation of that variable
;; for more information.  In this case, the useful bindings are `act', `skip',
;; `recenter', and `quit'.)

;; Under a windowing system a dialog box will be used if `last-nonmenu-event'
;; is nil and `use-dialog-box' is non-nil."
;;   ;; FIXME: This needs to be redone when the ECHO AREA works. 
;;   (string-equal "y" (read-from-minibuffer (concatenate 'string prompt "(y on n)"))))

(provide :lice-0.1/minibuffer)
