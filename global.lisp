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

(defun concat (&rest strings)
  "Concatenate all the arguments and make the result a string.
The result is a string whose elements are the elements of all the arguments.
Each argument must be a string."
  (apply 'concatenate 'string strings))

(defmacro while (test &body body)
  "If TEST yields non-nil, eval BODY... and repeat.
The order of execution is thus TEST, BODY, TEST, BODY and so on
until TEST returns nil."
  (if body
      `(loop while ,test do ,@body)
      `(loop while ,test)))

(defmacro check-number-coerce-marker (marker-var)
  "Verify that MARKER-VAR is a number or if it's a marker then
set the var to the marker's position."
  `(progn
     (check-type ,marker-var (or number marker))
     (when (typep ,marker-var 'marker)
       (setf ,marker-var (marker-position ,marker-var)))))

(defun cdr-safe (object)
  "Return the cdr of OBJECT if it is a cons cell, or else nil."
  (when (consp object)
    (cdr object)))

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
  
;; XXX: get rid of this function and all callers
(defun assq (prop list)
  "Return non-nil if key is `eq' to the car of an element of list.
The value is actually the first element of list whose car is key.
Elements of list that are not conses are ignored."
  (assoc prop (remove-if 'listp list)))

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

(depricate substring subseq)
(defun substring (string from &optional (to (length string)))
  "Return a substring of string, starting at index from and ending before to.
to may be nil or omitted; then the substring runs to the end of string.
from and to start at 0.  If either is negative, it counts from the end.

This function allows vectors as well as strings."
  (subseq string from to))

(depricate memq member)
(defun memq (elt list)
  "Return non-nil if ELT is an element of LIST.
Comparison done with `eq'.  The value is actually the tail of LIST
whose car is ELT."
  (member elt list :test 'eq))


(provide :lice-0.1/global)
