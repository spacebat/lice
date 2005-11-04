;;; Implement the major mode system

(in-package :lice)

(defclass major-mode ()
  ((name :type string :initarg :name :accessor major-mode-name)
   (map :type keymap :initarg :map :accessor major-mode-map)
   (init-fn :type function :initarg :init-fn :accessor major-mode-init-fn))
  (:documentation "A Major Mode class."))

(defmacro define-major-mode (sym (&key name map) doc &body body)
  "Create a major mode."
  `(defparameter ,sym
     (make-instance 'major-mode
		    :name ,name :map ,map
		    :init-fn (lambda () ,@body))
     ,doc))

(define-major-mode fundamental-mode 
  (:name "Fundamental"
   :map (make-hash-table))
  "Fundamental Mode."
  ;; empty body
  )

(defun set-major-mode (mm &optional (buffer (current-buffer)))
  (setf (buffer-major-mode buffer) mm)
  (funcall (major-mode-init-fn mm)))

(provide :lice-0.1/major-mode)
