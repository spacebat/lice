(in-package :lice)

(defstruct match-data
  buffer beg end)

(define-condition search-failed (lice-condition)
  () (:documentation "raised when a search failed to match"))

(defun search-forward (string &optional bound noerror (count 1))
  "Search forward from point for string.
Set point to the end of the occurrence found, and return point.
An optional second argument bounds the search; it is a buffer position.
The match found must not extend after that position.  nil is equivalent
  to (point-max).
Optional third argument, if t, means if fail just return nil (no error).
  If not nil and not t, move to limit of search and return nil.
Optional fourth argument is repeat count--search for successive occurrences.

Search case-sensitivity is determined by the value of the variable
`case-fold-search', which see.

See also the functions `match-beginning', `match-end' and `replace-match'."
  (gap-move-to (current-buffer) (buffer-point-aref (current-buffer)))
  (let* ((buffer (current-buffer))
	 pos
	 (n (loop for i from 0 below count
			count i
			do (setf pos (search string (buffer-data buffer) :start2 (buffer-point-aref buffer)))
			while pos)))
    (if (/= n count)
	(when (not noerror)
	  (signal 'search-failed))
      (goto-char (+ (buffer-aref-to-char buffer pos) (length string))))))
  