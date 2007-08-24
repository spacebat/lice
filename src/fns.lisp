;;; fns.lisp --- compatibility function from emacs

(in-package "LICE")

(defun concat (&rest strings)
  "Concatenate all the arguments and make the result a string.
The result is a string whose elements are the elements of all the arguments.
Each argument must be a string."
  (apply 'concatenate 'string strings))

(defun cdr-safe (object)
  "Return the cdr of OBJECT if it is a cons cell, or else nil."
  (when (consp object)
    (cdr object)))

;; XXX: get rid of this function and all callers
(defun assq (prop list)
  "Return non-nil if key is `eq' to the car of an element of list.
The value is actually the first element of list whose car is key.
Elements of list that are not conses are ignored."
  (assoc prop (remove-if 'listp list)))

(depricate substring subseq)
(defun substring (string from &optional (to (length string)))
  "Return a substring of string, starting at index from and ending before to.
to may be nil or omitted; then the substring runs to the end of string.
from and to start at 0.  If either is negative, it counts from the end.

This function allows vectors as well as strings."
  (when (< from 0)
    (setf from (max 0 (+ (length string) from))))
  (when (< to 0)
    (setf to (max 0 (+ (length string) to))))
  (subseq string from to))

(depricate memq member)
(defun memq (elt list)
  "Return non-nil if ELT is an element of LIST.
Comparison done with `eq'.  The value is actually the tail of LIST
whose car is ELT."
  (member elt list :test 'eq))

(depricate put (setf get))
(defun put (symbol propname value)
  "Store SYMBOL's PROPNAME property with value VALUE.
It can be retrieved with `(get SYMBOL PROPNAME)'."
  (setf (get symbol propname) value))

(defun featurep (feature &optional subfeature)
  "Returns t if FEATURE is present in this Emacs.

Use this to conditionalize execution of lisp code based on the
presence or absence of Emacs or environment extensions.
Use `provide' to declare that a feature is available.  This function
looks at the value of the variable `features'.  The optional argument
SUBFEATURE can be used to check a specific subfeature of FEATURE."
  (and (find feature *features*) t))
