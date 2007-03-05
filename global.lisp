(in-package :lice)

(defconstant +debug-v+ 1)
(defconstant +debug-vv+ 2)
(defconstant +debug-vvv+ 3)

(defparameter *debug-level* +debug-v+)
;;(defparameter *debug-level* +debug-vvv+)

(defmacro dformat (lvl &rest fmt-args)
  (declare (ignore lvl fmt-args))
;;   (let ((f (gensym)))
;;     `(when (>= *debug-level* ,lvl)
;;        (with-open-file (,f #p"/tmp/debug" :direction :output :if-exists :append
;; 			   :if-does-not-exist :create)
;;          (format ,f ,@fmt-args))))
  )

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
(defun assq (key list)
  "Return non-nil if key is `eq' to the car of an element of list.
The value is actually the first element of list whose car is key.
Elements of list that are not conses are ignored."
  (assoc prop (remove-if 'listp list)))

(provide :lice-0.1/global)
