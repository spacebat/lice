(in-package "LICE")

;; FIXME: obviously this is incomplete
(defmacro defcustom (symbol value doc &rest args)
  `(defvar ,symbol ,value ,doc))

;; FIXME: empty
(defmacro defgroup (name something docstring &rest stuff)
  )

;; FIXME: empty
(defmacro defface (name colors docstring group)
  )

;; FIXME: this is incomplete
(defmacro defcustom-buffer-local (symbol value doc &rest args)
  `(define-buffer-local ,symbol ,value ,doc))
