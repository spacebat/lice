;;; subr.lice --- basic lisp subroutines for Emacs

(in-package "LICE")

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
  (completing-read prompt #'file-completions :initial-input (princ-to-string *default-directory*)))

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
  (if beginningp
      (min (point) (mark))
      (max (point) (mark))))

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

;;; reading from the buffer

(defun read-from-buffer (&aux (buffer (current-buffer)))
  "Read 1 sexp from the buffer at the current point, moving the point to the end of what was read"
  (when (< (buffer-char-to-aref buffer (point buffer))
	   (buffer-gap-start buffer))
    (gap-move-to-point buffer))
  (multiple-value-bind (obj pos)
      (read-from-string (buffer-data buffer) t nil
                        :start (buffer-char-to-aref buffer (point buffer)))
    (set-point (buffer-aref-to-char buffer pos))
    obj))

(defcommand eval-region ((start end &optional print-flag (read-function 'read-from-string))
                         :region-beginning :region-end)
  "Execute the region as Lisp code.
When called from programs, expects two arguments,
giving starting and ending indices in the current buffer
of the text to be executed.
Programs can pass third argument PRINTFLAG which controls output:
A value of nil means discard it; anything else is stream for printing it.
Also the fourth argument READ-FUNCTION, if non-nil, is used
instead of `read' to read each expression.  It gets one argument
which is the input stream for reading characters.

This function does not move point."
  (let* ((stdout (make-string-output-stream))
         (*standard-output* stdout)
         (*error-output* stdout)
         (*debug-io* stdout)
         (string (buffer-substring-no-properties start end))
         (pos 0)
         last obj)
    (loop
       (setf last obj)
       (multiple-value-setq (obj pos) (funcall read-function string nil string :start pos))
       (when (eq obj string)
         (cond ((eq print-flag t)
                (message "~s" last)))
         (return-from eval-region last)))))

(defun sit-for (seconds &optional nodisp)
  "Perform redisplay, then wait for seconds seconds or until input is available.
seconds may be a floating-point value, meaning that you can wait for a
fraction of a second.
 (Not all operating systems support waiting for a fraction of a second.)
Optional arg nodisp non-nil means don't redisplay, just wait for input.
Redisplay is preempted as always if input arrives, and does not happen
if input is available before it starts.
Value is t if waited the full time with no input arriving."
  (unless nodisp
    (frame-render (selected-frame)))
  ;; FIXME: poll for input
  (sleep seconds)
  t
;;   (let ((event (wait-for-event seconds)))
;;     (if event
;;         (progn
;;           (push event *unread-command-events*)
;;           nil)
;;         t))
  )


;;; Matching and match data
(defun match-string (num &optional string)
  "Return string of text matched by last search.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING."
  (if (match-beginning num)
      (if string
	  (substring string (match-beginning num) (match-end num))
	(buffer-substring (match-beginning num) (match-end num)))))

(defun match-string-no-properties (num &optional string)
  "Return string of text matched by last search, without text properties.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING."
  (if (match-beginning num)
      (if string
	  (substring-no-properties string (match-beginning num)
				   (match-end num))
	(buffer-substring-no-properties (match-beginning num)
					(match-end num)))))


(defun force-mode-line-update (&optional all)
  "Force redisplay of the current buffer's mode line and header line.
With optional non-nil ALL, force redisplay of all mode lines and
header lines.  This function also forces recomputation of the
menu bar menus and the frame title."
;;   (if all (save-excursion (set-buffer (other-buffer))))
;;   (set-buffer-modified-p (buffer-modified-p))
  )

(provide :lice-0.1/subr)
