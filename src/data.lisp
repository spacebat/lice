;;; data.lisp --- compatibility functions from emacs

(in-package "LICE")

(defun % (number divisor)
  "same as mod."
  (mod number divisor))

(defun setcar (cell newcar)
  "Set the car of cell to be newcar.  Returns newcar."
  (setf (car cell) newcar))

(depricate aset (setf aref))
(defun aset (array idx newelt)
  "Store into the element of ARRAY at index IDX the value NEWELT.
Return NEWELT.  ARRAY may be a vector, a string, a char-table or a
bool-vector.  IDX starts at 0."
  (setf (aref array idx) newelt))

(defmacro defalias (to from &optional docstring)
  "Set SYMBOL's function definition to DEFINITION, and return DEFINITION.
Associates the function with the current load file, if any.
The optional third argument DOCSTRING specifies the documentation string
for SYMBOL; if it is omitted or nil, SYMBOL uses the documentation string
determined by DEFINITION."
  ;; FIXME: implement
  (declare (ignore to from docstring)))

(defun plist-get (plist prop)
  "Extract a value from a property list.
PLIST is a property list, which is a list of the form
\(PROP1 VALUE1 PROP2 VALUE2...).  This function returns the value
corresponding to the given PROP, or nil if PROP is not
one of the properties on the list."
  (getf plist prop))

(defun sequencep (object)
  "Return t if OBJECT is a sequence (list or array)."
  (typep object 'sequence))

(defun copy-sequence (seq)
  "Return a copy of a list, vector, string or char-table.
The elements of a list or vector are not copied; they are shared
with the original."
  (copy-seq seq))
