;;; Implement the major mode system

(in-package "LICE")

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

(defun set-major-mode (mm)
  "Set the current buffer's major mode."
  (check-type mm symbol)
  (let ((mode (symbol-value mm)))
    ;; Call All inherited init functions
    (mapc 'set-major-mode (major-mode-inherit-init mode))

    (when (major-mode-map mode)
      (use-local-map (major-mode-map mode)))
    (when (major-mode-syntax-table mode)
      (set-syntax-table (major-mode-syntax-table mode)))

    ;; Now call this mm's init function
    (when (major-mode-init mode)
      (funcall (major-mode-init mode)))

    ;; Finally, set the mode and call the hook
    (setf (buffer-major-mode (current-buffer)) mm)
    (run-hooks (major-mode-hook mode))))

(provide :lice-0.1/major-mode)
