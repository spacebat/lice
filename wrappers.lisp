;;; A collection of wrappers around extended functionality that may be
;;; different across CL implementations.

;;; To add support for a new CL implementation, an entry in each of
;;; these functions must be made for it.

(in-package :lice)

;;; Weak Pointers

(defun weak-pointer-p (wp)
  #+clisp (ext:weak-pointer-p (wp))
  #-(or clisp) (declare (ignore wp)))

(defun make-weak-pointer (data)
  #+clisp (ext:make-weak-pointer data)
  #+cmu (extensions:make-weak-pointer data)
  #+sbcl (sb-ext:make-weak-pointer data)
  #+(or movitz mcl) data)

(defun weak-pointer-value (wp)
  #+clisp (ext:weak-pointer-value wp)
  #+cmu (extensions:weak-pointer-value wp)
  #+sbcl (sb-ext:weak-pointer-value wp)
  #+(or movitz mcl) (values wp t))

(provide :lice-0.1/wrappers)
