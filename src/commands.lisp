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
              :fn #',name)))))

(defgeneric lookup-command (name)
  (:documentation "lookup the command named NAME."))

(defmethod lookup-command ((name symbol))
  (gethash name *commands*))

(defmethod lookup-command ((name string))
  ;; FIXME: this can fill the keyword package with lots of junk
  ;; symbols.
  (gethash (intern (string-upcase name) "KEYWORD") *commands*))

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
