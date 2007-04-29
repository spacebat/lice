;; Command related functions

(in-package "LICE")

(defclass command ()
  ((name :type symbol :initarg :name :accessor command-name)
   (args :type list :initarg :args :accessor command-args)
   (fn :type function :initarg :fn :accessor command-fn)
   (doc :type (or null string) :initarg :doc :accessor command-doc))
  (:documentation "An interactive command."))

(defvar *commands* (make-hash-table)
  "A hash table of interactive commands")

(defmacro defcommand (name (&optional args &rest interactive-args) &body body)
  "Create an interactive command named NAME."
  (let ((tmp (gensym)))
    `(progn
       (defun ,name ,args
	 ,@body)
       (setf (gethash ',name *commands*)
	     (make-instance
	      'command
	      :name ',name
	      :args ',interactive-args
	      :doc ,(when (typep (first body) 'string) (first body))
	      :fn (lambda ()
		    (let ((,tmp (list ,@(mapcar (lambda (a)
						  (if (listp a)
						      `(funcall (gethash ,(first a) *command-arg-type-hash*) ,@(cdr a))
						    `(funcall (gethash ,a *command-arg-type-hash*))))
						interactive-args))))
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
		      (apply #',name ,tmp))))))))

(defgeneric lookup-command (name)
  (:documentation "lookup the command named NAME."))

(defmethod lookup-command ((name symbol))
  (gethash name *commands*))

(defmethod lookup-command ((name string))
  ;; FIXME: this can fill the keyword package with lots of junk
  ;; symbols.
  (gethash (intern (string-upcase name) "KEYWORD") *commands*))

(defun call-command (name &rest args)
  "Use this command to call an interactive command from a lisp program."
  (let ((cmd (lookup-command name)))
    (apply (command-fn cmd) args)))

(defvar *command-arg-type-hash* (make-hash-table)
  "A hash table of symbols. each symbol is an interactive argument
type whose value is a function that is called to gather input from the
user (or somewhere else) and return the result. For instance,
:BUFFER's value is read-buffer which prompts the user for a buffer and
returns it.

This variable is here to allow modules to add new argument types easily.")

(defvar mark-even-if-inactive nil
  "*Non-nil means you can use the mark even when inactive.
This option makes a difference in Transient Mark mode.
When the option is non-nil, deactivation of the mark
turns off region highlighting, but commands that use the mark
behave as if the mark were still active.")
