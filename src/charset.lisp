(in-package "LICE")

(defun define-charset ()
  (error "unimplemented define-charset"))

(defun generic-character-list ()
  (error "unimplemented generic-character-list"))

(defun get-unused-iso-final-char ()
  (error "unimplemented get-unused-iso-final-char"))

(defun declare-equiv-charset ()
  (error "unimplemented declare-equiv-charset"))

(defun find-charset-region ()
  (error "unimplemented find-charset-region"))

(defun find-charset-string ()
  (error "unimplemented find-charset-string"))

(defun make-char-internal ()
  (error "unimplemented make-char-internal"))

(defun split-char ()
  (error "unimplemented split-char"))

(defun char-charset ()
  (error "unimplemented char-charset"))

(defun charset-after (&optional (pos (pt)))
  "Return charset of a character in the current buffer at position POS.
If POS is nil, it defauls to the current point.
If POS is out of range, the value is nil."
  (error "unimplemented charset-after"))

(defun iso-charset ()
  (error "unimplemented iso-charset"))

(defun char-valid-p ()
  (error "unimplemented char-valid-p"))

(defun unibyte-char-to-multibyte ()
  (error "unimplemented unibyte-char-to-multibyte"))

(defun multibyte-char-to-unibyte ()
  (error "unimplemented multibyte-char-to-unibyte"))

(defun char-bytes ()
  (error "unimplemented char-bytes"))

(defun char-width ()
  (error "unimplemented char-width"))

(defun string-width ()
  (error "unimplemented" string-width))

(defun char-direction ()
  (error "unimplemented char-direction"))

;; (defun string ()
;;   (error (format nil "unimplemented ~a"))

(defun setup-special-charsets ()
  (error "unimplemented setup-special-charsets"))
