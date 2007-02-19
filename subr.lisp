;;; subr.lice --- basic lisp subroutines for Emacs

(in-package :lice)

(defun split-string (string &optional (separators " 
"))
  "Splits STRING into substrings where there are matches for SEPARATORS.
Each match for SEPARATORS is a splitting point.
The substrings between the splitting points are made into a list
which is returned.
***If SEPARATORS is absent, it defaults to \"[ \f\t\n\r\v]+\".

If there is match for SEPARATORS at the beginning of STRING, we do not
include a null substring for that.  Likewise, if there is a match
at the end of STRING, we don't include a null substring for that.

Modifies the match data; use `save-match-data' if necessary."
  ;; FIXME: This let is here because movitz doesn't 'lend optional'
  (let ((seps separators))
    (labels ((sep (c)
		  (find c seps :test #'char=)))
      (loop for i = (position-if (complement #'sep) string) 
	    then (position-if (complement #'sep) string :start j)
	    while i
	    as j = (position-if #'sep string :start i)
	    collect (subseq string i j)
	    while j))))
				     
(define-condition kbd-parse (lice-condition)
  () (:documentation "Raised when a kbd string failed to parse."))

(defun parse-mods (mods end)
  "MODS is a sequence of <MOD CHAR> #\- pairs. Return a list suitable
for passing as the last argument to (apply #'make-key ...)"
  (unless (evenp end)
    (signal 'kbd-parse))
  (apply #'nconc (loop for i from 0 below end by 2
		       if (char/= (char mods (1+ i)) #\-)
		       do (signal 'kbd-parse)
		       collect (case (char mods i)
				 (#\M (list :meta t))
				 (#\A (list :alt t))
				 (#\C (list :control t))
				 (#\H (list :hyper t))
				 (#\s (list :super t))
				 (#\S (list :shift t))
				 (t (signal 'kbd-parse))))))

(defun parse-char-name (string)
  "Return the character whose name is STRING."
  (or (name-char string)
      (and (= (length string) 1)
	   (char string 0))))

(defun parse-key (string)
  "Parse STRING and return a key structure."
  ;; FIXME: we want to return NIL when we get a kbd-parse error
  ;;(ignore-errors
    (let* ((p (when (> (length string) 2)
		(position #\- string :from-end t :end (- (length string) 1))))
	   (mods (parse-mods string (if p (1+ p) 0)))
	   (ch (parse-char-name (subseq string (if p (1+ p) 0)))))
      (and ch
	   (apply #'make-instance 'key :char ch mods))))
  
(defun parse-key-seq (keys)
  "KEYS is a key sequence. Parse it and return the list of keys."
  (mapcar 'parse-key (split-string keys)))

(defun kbd (keys)
  "Convert KEYS to the internal Emacs key representation.
KEYS should be a string constant in the format used for
saving keyboard macros ***(see `insert-kbd-macro')."
  ;; XXX: define-key needs to be fixed to handle a list of keys
  (first (parse-key-seq keys)))


;;; Argument types

(defun interactive (&rest prompts)
  "Read input from the minibuffer and return it in a list."
  (loop for p in prompts
	collect (read-from-minibuffer p)))

(defvar *extended-command-history* nil)

(defun read-command (prompt)
  "Read the name of a command and return as a symbol.
Prompt with prompt.  By default, return default-value."
  (let (cmds)
    (maphash (lambda (k v) 
               (declare (ignore v))
	       (push k cmds))
	     *commands*)
    (dformat +debug-v+ "commands: ~s~%" cmds)
    ;; Sadly, a cheap hack
    (find (completing-read prompt cmds :history '*extended-command-history*)
          cmds :test #'string-equal :key #'symbol-name)))

(defun read-buffer (prompt &optional def require-match)
  "Read the name of a buffer and return as a string.
Prompt with prompt.
Optional second arg def is value to return if user enters an empty line.
*If optional third arg require-match is non-nil,
* only existing buffer names are allowed."
  (declare (ignore require-match))
  (let* ((bufs (mapcar (lambda (b)
                         (cons (buffer-name b) b))
                       *buffer-list*))
         (b (completing-read (if def
                                 (format nil "~a(default ~a) " prompt def)
                               prompt)
                             bufs)))
    (if (zerop (length b))
        def
      b)))

(defun read-file-name (prompt &key dir default-filename mustmatch initial predicate)
  "Read file name, prompting with prompt and completing in directory dir.
Value is not expanded---you must call `expand-file-name' yourself.
Default name to default-filename if user exits the minibuffer with
the same non-empty string that was inserted by this function.
 (If default-filename is omitted, the visited file name is used,
  except that if initial is specified, that combined with dir is used.)
If the user exits with an empty minibuffer, this function returns
an empty string.  (This can only happen if the user erased the
pre-inserted contents or if `insert-default-directory' is nil.)
Fourth arg mustmatch non-nil means require existing file's name.
 Non-nil and non-t means also require confirmation after completion.
Fifth arg initial specifies text to start with.
If optional sixth arg predicate is non-nil, possible completions and
the resulting file name must satisfy (funcall predicate NAME).
dir should be an absolute directory name.  It defaults to the value of
`:default-directory'.

If this command was invoked with the mouse, use a file dialog box if
`use-dialog-box' is non-nil, and the window system or X toolkit in use
provides a file dialog box.

See also `read-file-name-completion-ignore-case'
and `read-file-name-function'."
  (declare (ignore predicate initial mustmatch default-filename dir))
  (completing-read prompt #'file-completions :initial-input (princ-to-string (buffer-local :default-directory))))

(defun read-string (prompt &optional initial-input history default-value)
  "Read a string from the minibuffer, prompting with string prompt.
If non-nil, second arg initial-input is a string to insert before reading.
  This argument has been superseded by default-value and should normally
  be nil in new code.  It behaves as in `read-from-minibuffer'.  See the
  documentation string of that function for details.
The third arg history, if non-nil, specifies a history list
  and optionally the initial position in the list.
See `read-from-minibuffer' for details of history argument.
Fourth arg default-value is the default value.  If non-nil, it is used
 for history commands, and as the value to return if the user enters
 the empty string.
**Fifth arg inherit-input-method, if non-nil, means the minibuffer inherits
 the current input method and the setting of `enable-multibyte-characters'."
  (read-from-minibuffer prompt :initial-contents initial-input :history history :default-value default-value))

(defun region-limit (beginningp)
  "Return the start or end position of the region.
BEGINNINGP non-zero means return the start.
If there is no region active, signal an error."
  (if (and (< (point) (mark))
	   beginningp)
      (point)
    (mark)))

(defun region-beginning ()
  "Return position of beginning of region, as an integer."
  (region-limit t))
  
(defun region-end ()
  "Return position of end of region, as an integer."
  (region-limit nil))

(defun add-command-arg-type (type fn)
  "TYPE is a symbol. Add it to the hash table of command types and link it to FN, a function or function symbol."
  (setf (gethash type *command-arg-type-hash*) fn))

(defun init-command-arg-types ()
  "populate the hash table with some defaults"
  ;; Reset the hash table. FIXME: should we do this? 
  (setf *command-arg-type-hash* (make-hash-table))
  (add-command-arg-type :buffer 'read-buffer)
  (add-command-arg-type :file 'read-file-name)
  (add-command-arg-type :string 'read-from-minibuffer)
  (add-command-arg-type :command 'read-command)
  (add-command-arg-type :prefix 'prefix-arg)
  (add-command-arg-type :raw-prefix 'raw-prefix-arg)
  (add-command-arg-type :region-beginning 'region-beginning)
  (add-command-arg-type :region-end 'region-end))

(defun get-buffer-window-list (buffer &optional minibuf frame)
  "Return list of all windows displaying BUFFER, or nil if none.
BUFFER can be a buffer or a buffer name.
See `walk-windows' for the meaning of MINIBUF and FRAME."
  (let ((buffer (if (bufferp buffer) buffer (get-buffer buffer))) windows)
    (mapc (lambda (window)
	    (if (eq (window-buffer window) buffer)
		(push window windows)))
	  (frame-window-list frame minibuf))
    windows))

;; FIXME: this isn't complete.
(defmacro defalias (from-symbol to-symbol)
"Set symbol's function definition to definition, and return definition."
  `(define-symbol-macro ,from-symbol ,to-symbol))

(defun intern-soft (name &optional (package *package*))
  (find-symbol name package))

;;;; Syntax tables

(defmacro with-syntax-table (table &body body)
  "Evaluate BODY with syntax table of current buffer set to TABLE.
The syntax table of the current buffer is saved, BODY is evaluated, and the
saved table is restored, even in case of an abnormal exit.
Value is what BODY returns."
  (declare (debug t))
  (let ((old-table (make-symbol "table"))
	(old-buffer (make-symbol "buffer")))
    `(let ((,old-table (syntax-table))
	   (,old-buffer (current-buffer)))
       (unwind-protect
	   (progn
	     (set-syntax-table ,table)
	     ,@body)
	 (save-current-buffer
	   (set-buffer ,old-buffer)
	   (set-syntax-table ,old-table))))))

(defun make-syntax-table (&optional oldtable)
  "Return a new syntax table.
Create a syntax table which inherits from OLDTABLE (if non-nil) or
from `standard-syntax-table' otherwise."
  (let ((table (make-char-table 'syntax-table nil)))
    (set-char-table-parent table (or oldtable (standard-syntax-table)))
    table))

(defun syntax-after (pos)
  "Return the raw syntax of the char after POS.
If POS is outside the buffer's accessible portion, return nil."
  (unless (or (< pos (point-min)) (>= pos (point-max)))
    (let ((st (if parse-sexp-lookup-properties
		  (get-char-property pos 'syntax-table))))
      (if (consp st) st
	(aref (or st (syntax-table)) (char-after pos))))))

(defun syntax-class (syntax)
  "Return the syntax class part of the syntax descriptor SYNTAX.
If SYNTAX is nil, return nil."
  (and syntax (logand (car syntax) 65535)))


(provide :lice-0.1/subr)
