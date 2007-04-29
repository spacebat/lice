;;; Implement the major mode system

(in-package :lice)

(defclass major-mode ()
  ((name :type string :initarg :name :accessor major-mode-name)
   (map :type keymap :initarg :map :accessor major-mode-map)
   (syntax :initarg :syntax-table :accessor major-mode-syntax-table)
   (hook :initarg :hook :accessor major-mode-hook)
   (init :initarg :init :accessor major-mode-init)
   (inherit-map :type list :initarg :inherit-map :accessor major-mode-inherit-map)
   (inherit-syntax :type list :initarg :inherit-syntax :accessor major-mode-inherit-syntax)
   (inherit-init :type list :initarg :inherit-init :accessor major-mode-inherit-init))
  (:default-initargs 
   :map (make-sparse-keymap)
   :syntax-table *standard-syntax-table*
   :inherit-map nil
   :inherit-syntax nil
   :inherit-init nil
   :hook nil
   :init nil)
  (:documentation "A Major Mode class."))

(provide :lice-0.1/major-mode)
