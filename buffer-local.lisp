;;; buffer local variables

(in-package "LICE")

(defstruct buffer-local-binding
  symbol value local-p doc-string)

(defvar *global-buffer-locals* (make-hash-table)
  "The default values of buffer locals and a hash table containing all possible buffer locals")

(defun buffer-local-exists-p (symbol)
  (multiple-value-bind (v b) (gethash symbol *global-buffer-locals*)
    (declare (ignore v))
    b))  

(defun get-buffer-local-create (symbol default-value &optional doc-string)
  (if (buffer-local-exists-p symbol)
      (gethash symbol *global-buffer-locals*)
      (setf (gethash symbol *global-buffer-locals*)
            (make-buffer-local-binding :symbol symbol
                                       :value default-value
                                       :doc-string doc-string))))

(defmacro define-buffer-local (symbol default-value &optional doc-string)
  "buffer locals are data hooks you can use to store values per
buffer. Use them when building minor and major modes. You
generally want to define them with this so you can create a
docstring for them. there is also `get-buffer-local-create'."
  `(progn
     (when (boundp ',symbol)
       (warn "Symbol ~s is already bound. Existing uses of symbol will not be buffer local." ',symbol)
       (makunbound ',symbol))
     (define-symbol-macro ,symbol (buffer-local ',symbol))
     (get-buffer-local-create ',symbol ,default-value ,doc-string)))

(defun (setf buffer-local) (value symbol &optional (buffer (current-buffer)))
  "Set the value of the buffer local in the current buffer."
  ;; create a global buffer local entry if needed.
  (let ((global-binding (get-buffer-local-create symbol value)))
    ;; if the symbol becomes buffer local when set or it has a buffer
    ;; value
    (if (or (buffer-local-binding-local-p global-binding)
            (second (multiple-value-list
                     (gethash symbol (buffer-locals buffer)))))
        ;; set the buffer's value
        (setf (gethash symbol (buffer-locals buffer)) value)
        ;; set the global value
        (setf (buffer-local-binding-value global-binding) value))))

(defun buffer-local (symbol &optional (buffer (current-buffer)))
  "Return the value of the buffer local symbol. If none exists
for the current buffer then use the global one. If that doesn't
exist, throw an error."
  (multiple-value-bind (v b) (gethash symbol (buffer-locals buffer))
    (if b
	v
	(multiple-value-bind (v b) (gethash symbol *global-buffer-locals*)
	  (if b
              (buffer-local-binding-value v)
              (error "No binding for buffer-local ~s" symbol))))))

(defun make-local-variable (symbol)
  "Make VARIABLE have a separate value in the current buffer.
Other buffers will continue to share a common default value.
\(The buffer-local value of VARIABLE starts out as the same value
VARIABLE previously had.  If VARIABLE was void, it remains void.\)
Return VARIABLE.

If the variable is already arranged to become local when set,
this function causes a local value to exist for this buffer,
just as setting the variable would do.

Unlike GNU/Emacs This function does not return
VARIABLE. See alse `(SETF MAKE-LOCAL-VARIABLE)'.

See also `make-variable-buffer-local' and `define-buffer-local'.

Do not use `make-local-variable' to make a hook variable buffer-local.
Instead, use `add-hook' and specify t for the LOCAL argument."
  (setf (gethash symbol (buffer-locals (current-buffer))) (buffer-local symbol))
  ;; only setq and setf expand the symbol-macro properly, so we can't
  ;; return the symbol.
  nil)

(defun (setf make-local-variable) (value symbol)
  "Make the symbol local to the current buffer like
`make-local-variable' and also set its value in the buffer."
  (setf (gethash symbol (buffer-locals (current-buffer))) value))

(defun make-variable-buffer-local (variable)
  "Make VARIABLE become buffer-local whenever it is set.
At any time, the value for the current buffer is in effect,
unless the variable has never been set in this buffer,
in which case the default value is in effect.
Note that binding the variable with `let', or setting it while
a `let'-style binding made in this buffer is in effect,
does not make the variable buffer-local.  Return VARIABLE.

In most cases it is better to use `make-local-variable',
which makes a variable local in just one buffer.

The function `default-value' gets the default value and `set-default' sets it."
  (setf (buffer-local-binding-local-p (gethash variable *global-buffer-locals*)) t))

(defun default-value (symbol)
  "Return SYMBOL's default value.
This is the value that is seen in buffers that do not have their own values
for this variable.  The default value is meaningful for variables with
local bindings in certain buffers."
  (buffer-local-binding-value (gethash symbol *global-buffer-locals*)))

(defun (setf default-value) (value symbol)
  "Set symbol's default value to value.  symbol and value are evaluated.
The default value is seen in buffers that do not have their own values
for this variable."
  (setf (buffer-local-binding-value (gethash symbol *global-buffer-locals*)) value)  )

(depricate set-default (setf default-value))
(defun set-default (symbol value)
  "Set symbol's default value to value.  symbol and value are evaluated.
The default value is seen in buffers that do not have their own values
for this variable."
  (setf (default-value symbol) value))


;;; Some built-in buffer local variables

(define-buffer-local *buffer-invisibility-spec* nil
  "Invisibility spec of this buffer.
The default is t, which means that text is invisible
if it has a non-nil `invisible' property.
If the value is a list, a text character is invisible if its `invisible'
property is an element in that list.
If an element is a cons cell of the form (PROP . ELLIPSIS),
then characters with property value PROP are invisible,
and they have an ellipsis as well if ELLIPSIS is non-nil.")

(define-buffer-local *selective-display* nil
  "Non-nil enables selective display.
An Integer N as value means display only lines
that start with less than n columns of space.
A value of t means that the character ^M makes itself and
all the rest of the line invisible; also, when saving the buffer
in a file, save the ^M as a newline.")

(define-buffer-local *mark-active* nil
  "Non-nil means the mark and region are currently active in this buffer.")
