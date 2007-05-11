(in-package :lice)

(defconstant +debug-v+ 1)
(defconstant +debug-vv+ 2)
(defconstant +debug-vvv+ 3)

(defparameter *debug-level* +debug-v+)
;;(defparameter *debug-level* +debug-vvv+)

(defun dformat (lvl &rest fmt-args)
  (when (>= *debug-level* lvl)
    (with-open-file (f #p"/tmp/debug" :direction :output :if-exists :append
                        :if-does-not-exist :create)
      (apply 'format f fmt-args))))

(defmacro verbose-body (&body body)
  "Print each sexpr in BODY and its return value."
  (let ((ret (gensym "RET")))
    (loop for i in body
	  collect `(let ((,ret ,i))
		     (format t "~s => ~s~%" ,i ,ret)
		     ,ret))))
		   
(defun last1 (l)
  "Return the last elt of the list."
  (car (last l)))

(defun nconc1 (list item)
  "destructively append ITEM to the end of L"
  (nconc list (list item)))

;; (defun grow-vector (vector amt initial-element)
;;   "grow the vector's size by AMT elements"
;;   (adjust-array vector (+ (length vector) amt) 
;; 		:initial-element initial-element
;; 		:fill-pointer t))

;; (defun vector-append (vector1 vector2)
;;   "append vector2 to the end of vector1."
;;   (let ((len (length vector1)))
;;     (grow-vector vector1 (length vector2) (elt vector2 0))
;;     (replace vector1 vector2 :start1 (1+ len) :start2 1)))

(define-condition lice-condition ()
  () (:documentation "The base condition for all lice related errors."))

(define-condition wrong-type-argument (lice-condition)
    ((:type :initarg :type :accessor wrong-type-argument-type)))

;; (defun fmt (fmt &rest args)
;;   "A movitz hack function. FORMAT basically doesn't work, so i use this to get around it."
;;   (let ((s (make-array 100 :fill-pointer 0 :element-type 'character)))
;;     (apply #'format s fmt args)
;;     s))

#+movitz
(defun read-from-string (string)
  "Read the string and return an sexpr. This is a MOVITZ hack
  because it doesn't have read-from-string."
  (muerte::simple-read-from-string string))

;;; Lisp function we like to have

(defmacro while (test &body body)
  "If TEST yields non-nil, eval BODY... and repeat.
The order of execution is thus TEST, BODY, TEST, BODY and so on
until TEST returns nil."
  (if body
      `(loop while ,test do ,@body)
      `(loop while ,test)))

(defvar *quit-code* 7
  "The terminal char code for the interrupt key.")

(defvar *inhibit-quit* nil
  ;; XXX: this is not correct docs
  "Non-nil inhibits C-g quitting from happening immediately.
Note that `quit-flag' will still be set by typing C-g,
so a quit will be signaled as soon as `inhibit-quit' is nil.
To prevent this happening, set `quit-flag' to nil
before making `inhibit-quit' nil.")

(defvar *waiting-for-input* nil
  "T when we're waiting for .. input")

(defvar *quit-flag* nil
  "Set to T when the user hit the quit key")

(defmacro depricate (symbol refer-to)
  "A macro to mark a symbol as depricated. This is done with
function in emacs whose purpose is better done another
way. For example, a set- function replaced by a setf function. "
  `(setf (get (quote ,symbol) :depricated) (quote ,refer-to)))

(defun read-string-with-escapes (stream close)
  "read in a string and handle \\f \\n \\r \\t \\v escape characters."
  (with-output-to-string (out)
    (do ((char (read-char stream nil :eof) (read-char stream nil :eof)))
        ((or (eq char :eof) (char= char close))
         (if (eq char :eof)
             (error 'end-of-file :stream stream)))
      (when (char= char #\\)
        (setq char (read-char stream nil :eof))
        (case char
          (:eof (error 'end-of-file :stream stream))
          (#\f (setq char #\Page))
          (#\n (setq char #\Newline))
          (#\r (setq char #\Return))
          (#\t (setq char #\Tab))
          (#\v (setq char #\Vt))))
        (write-char char out))))

;; LiCE handles a few escape codes like GNU Emacs
(set-macro-character #\" #'read-string-with-escapes)

(defun run-hooks (&rest hooks)
  "Run each hook in HOOKS.
Each argument should be a symbol, a hook variable.
These symbols are processed in the order specified.
If a hook symbol has a non-nil value, that value may be a function
or a list of functions to be called to run the hook.
If the value is a function, it is called with no arguments.
If it is a list, the elements are called, in order, with no arguments.

Major modes should not use this function directly to run their mode
hook; they should use `run-mode-hooks' instead.

Do not use `make-local-variable' to make a hook variable buffer-local.
Instead, use `add-hook' and specify t for the LOCAL argument."
  (mapc (lambda (h)
          (when (symbolp h)
            (setf h (symbol-value h)))
          (mapc 'funcall h))
        hooks))

(defun add-hook (hook function &optional append local)
  "Add to the value of HOOK the function function.
FUNCTION is not added if already present.
FUNCTION is added (if necessary) at the beginning of the hook list
unless the optional argument append is non-nil, in which case
function is added at the end.

The optional fourth argument, LOCAL, if non-nil, says to modify
the hook's buffer-local value rather than its default value.
This makes the hook buffer-local if needed, and it makes t a member
of the buffer-local value.  That acts as a flag to run the hook
functions in the default value as well as in the local value.

HOOK should be a symbol, and FUNCTION may be any valid function.  If
HOOK is void, it is first set to nil.  If HOOK's value is a single
function, it is changed to a list of functions."
  (declare (ignore append local))
  (pushnew function (symbol-value hook)))

(defun remove-hook (hook function &optional local)
  "Remove from the value of HOOK the function FUNCTION.
HOOK should be a symbol, and FUNCTION may be any valid function.  If
FUNCTION isn't the value of HOOK, or, if FUNCTION doesn't appear in the
list of hooks to run in HOOK, then nothing is done.  See `add-hook'.

The optional third argument, LOCAL, if non-nil, says to modify
the hook's buffer-local value rather than its default value."
  (declare (ignore local))
  (setf (symbol-value hook) (remove function (symbol-value hook))))

(defun int-to-string (n)
  "Return the decimal representation of number as a string.
Uses a minus sign if negative.
number may be an integer or a floating point number."
  (check-type n number)
  (prin1-to-string n))

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

;; A cheap memoizer. Obviously, a hashtable would be better.

(defstruct memoize-state
  (data (vector nil nil nil nil nil nil nil nil nil nil nil nil))
  (test 'equal)
  (pt 0))

(defun memoize-store (state thing value)
  (incf (memoize-state-pt state))
  (when (>= (memoize-state-pt state) 
            (length (memoize-state-data state)))
    (setf (memoize-state-pt state) 0))
  (setf (svref (memoize-state-data state) (memoize-state-pt state)) (cons thing value))
  value)

(defmacro memoize (mem-var thing compute)
  "Check if we've computed a value for thing. if so, use it. if
not compute it, store the result, and return it."
  (let ((match (gensym "MATCH")))
    `(let ((,match (find ,thing (memoize-state-data ,mem-var) :key 'first :test (memoize-state-test ,mem-var))))
       (if ,match
           (cdr ,match)
           (memoize-store ,mem-var ,thing ,compute)))))

(defun add-to-list (list-var element &optional append)
  "Add ELEMENT to the value of LIST-VAR if it isn't there yet.
The test for presence of ELEMENT is done with `equal'.
If ELEMENT is added, it is added at the beginning of the list,
unless the optional argument APPEND is non-nil, in which case
ELEMENT is added at the end.

The return value is the new value of LIST-VAR.

If you want to use `add-to-list' on a variable that is not defined
until a certain package is loaded, you should put the call to `add-to-list'
into a hook function that will be run only after loading the package.
`eval-after-load' provides one way to do this.  In some cases
other hooks, such as major mode hooks, can do the job."
  (if (member element (symbol-value list-var))
      (symbol-value list-var)
    (set list-var
	 (if append
	     (append (symbol-value list-var) (list element))
	   (cons element (symbol-value list-var))))))

(defmacro defsubst (name lambda-list &body body)
  "Define an inline function.  The syntax is just like that of `defun'."
  `(progn
     (declaim (inline ,name))
     (defun ,name ,lambda-list
       ,@body)))


(defvar *debug-on-error* t
  "Non-nil means enter the debugger if an unhandled error is signaled.")

(defvar *debug-on-quit* nil
  "Non-nil means enter the debugger if quit is signaled (C-g, for example).")

(defvar debug-ignored-errors nil
  "*List of errors for which the debugger should not be called.
Each element may be a condition-name or a regexp that matches error messages.
If any element applies to a given error, that error skips the debugger
and just returns to top level.
This overrides the variable `debug-on-error'.
It does not apply to errors handled by `condition-case'.")

(defun purecopy (thing)
  "Make a copy of object OBJ in pure storage.
Recursively copies contents of vectors and cons cells.
Does not copy symbols.  Copies strings without text properties."
  thing)

(defun garbage-collect ()
  "Reclaim storage for Lisp objects no longer needed."
  (warn "unimplemented"))

(provide :lice-0.1/global)
