(in-package "LICE")

;; FIXME: these case functions don't handle characters or propertized strings
(defun upcase (obj)
  "Convert argument to upper case and return that.
The argument may be a character or string.  The result has the same type.
The argument object is not altered--the value is a copy.
See also `capitalize', `downcase' and `upcase-initials'."
  (string-upcase obj))

(defun downcase (obj)
  "Convert argument to lower case and return that.
The argument may be a character or string.  The result has the same type.
The argument object is not altered--the value is a copy."
  (string-downcase obj))

(defun capitalize (obj)
  "Convert argument to capitalized form and return that.
This means that each word's first character is upper case
and the rest is lower case.
The argument may be a character or string.  The result has the same type.
The argument object is not altered--the value is a copy."
  (string-capitalize obj))

(defun upcase-initials (obj)
  "Convert the initial of each word in the argument to upper case.
Do not change the other letters of each word.
The argument may be a character or string.  The result has the same type.
The argument object is not altered--the value is a copy."
  ;; FIXME: don't touch the other letters. only the first one.
  (string-capitalize obj))

(defun upcase-region (beg end)
  (declare (ignore beg end))
  (error "Unimplemented"))  
(setf (get 'upcase-region 'disabled) t)

(defun downcase-region ()
  (error "Unimplemented"))
(setf (get 'downcase-region 'disabled) t)

(defun capitalize-region ()
  (error "Unimplemented"))

(defun upcase-initials-region ()
  (error "Unimplemented"))

(defun upcase-word ()
  (error "Unimplemented"))

(defun downcase-word ()
  (error "Unimplemented"))

(defun capitalize-word ()
  (error "Unimplemented"))

;;; Key bindings

(define-key *ctl-x-map* "C-u" 'upcase-region)
(define-key *ctl-x-map* "C-l" 'downcase-region)
(define-key *global-map* "M-u" 'upcase-word)
(define-key *global-map* "M-l" 'downcase-word)
(define-key *global-map* "M-c" 'capitalize-word)
