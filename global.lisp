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

(provide :lice-0.1/global)
