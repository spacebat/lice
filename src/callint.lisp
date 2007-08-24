;;; Call a Lisp function interactively.

(defvar *prefix-arg* nil
  "The value of the prefix argument for the next editing command.
It may be a number, or the symbol `-' for just a minus sign as arg,
or a list whose car is a number for just one or more C-u's
or nil if no argument has been specified.

You cannot examine this variable to find the argument for this command
since it has been set to nil by the time you can look.
Instead, you should use the variable `current-prefix-arg', although
normally commands can get this prefix argument with (interactive \"P\").")

(defvar *last-prefix-arg* nil
"The value of the prefix argument for the previous editing command.
See `prefix-arg' for the meaning of the value.")

(defvar *current-prefix-arg* nil
  "The value of the prefix argument for this editing command.
It may be a number, or the symbol `-' for just a minus sign as arg,
or a list whose car is a number for just one or more C-u's
or nil if no argument has been specified.
This is what `(interactive \"P\")' returns.")

(defvar *command-history* nil
  "List of recent commands that read arguments from terminal.
Each command is represented as a form to evaluate.")

(defun call-interactively (function &optional record-flag (keys *this-command-keys*))
  "Call FUNCTION, reading args according to its interactive calling specs.
Return the value FUNCTION returns.
The function contains a specification of how to do the argument reading.
In the case of user-defined functions, this is specified by placing a call
to the function `interactive' at the top level of the function body.
See `interactive'.

Optional second arg RECORD-FLAG non-nil
means unconditionally put this command in the command-history.
Otherwise, this is done only if an arg is read using the minibuffer.

Optional third arg KEYS, if given, specifies the sequence of events to
supply, as a vector, if the command inquires which events were used to
invoke it.  If KEYS is omitted or nil, the return value of
`*this-command-keys-vector*' is used."
  (setf function (lookup-command function))
  (check-type function command)

  (let ((args (mapcar (lambda (a)
                        (if (listp a)
                            (apply (gethash (first a) *command-arg-type-hash*) (cdr a))
                            (funcall (gethash a *command-arg-type-hash*))))
                      (command-args function))))
    ;; XXX: Is this a sick hack?  We need to reset the
    ;; prefix-arg at the right time. After the command
    ;; is executed we can't because the universal
    ;; argument code sets the prefix-arg for the next
    ;; command. The Right spot seems to be to reset it
    ;; once a command is about to be executed, and
    ;; after the prefix arg has been gathered to be
    ;; used in the command. Which is right here.
    (setf *prefix-arg* nil)
    ;; Note that we use the actual function. If the
    ;; function is redefined, the command will
    ;; continue to be defined and will call the
    ;; function declared above, not the redefined one.
    (apply (command-fn function) args)))

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
