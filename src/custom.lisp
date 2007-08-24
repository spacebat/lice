(in-package "LICE")

;; FIXME: obviously this is incomplete
(defmacro defcustom (symbol value doc &rest args)
  (declare (ignore args))
  `(defvar ,symbol ,value ,doc))

;; FIXME: empty
(defmacro defgroup (name something docstring &rest stuff)
  (declare (ignore name something docstring stuff))
  )

;; FIXME: empty
(defmacro defface (name colors docstring &key group)
  (declare (ignore name colors docstring group))
  )

;; FIXME: this is incomplete
(defmacro defcustom-buffer-local (symbol value doc &rest args)
  (declare (ignore args))
  `(define-buffer-local ,symbol ,value ,doc))
