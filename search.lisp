(in-package :lice)

;; because gnu emacs' match-data is not reentrant we create this
;; structure that is returned for all searching functions. It is
;; passed into the match-data related functions.
(defstruct match-data
  obj start end reg-starts reg-ends)

(defun match-end (data idx)
  "Return position of start of text matched by last search.
SUBEXP, a number, specifies which parenthesized expression in the last
  regexp.
Value is nil if SUBEXPth pair didn't match, or there were less than
  SUBEXP pairs.
Zero means the entire text matched by the whole regexp or whole string."
  (if (zerop idx)
      (match-data-end data)
      (aref (match-data-reg-ends data) (1- idx))))

;; FIXME: needs a formatter and the search string
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
  (declare (ignore bound))
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

(defun looking-at (regexp &optional (buffer (current-buffer)))
  "Return the match-data if text after point matches regular expression regexp."
  ;; get the gap outta the way. It sucks we have to do this. Really we
  ;; should modify ppcre to generate scanner functions that hop the
  ;; gap. Meantime...
  (when (< (buffer-char-to-aref buffer (point buffer))
	   (buffer-gap-start buffer))
    (gap-move-to-point buffer))
  (multiple-value-bind (start end reg-starts reg-ends)
      (ppcre:scan regexp (buffer-data buffer) :start (buffer-char-to-aref buffer (point buffer)))
    (when (and start
	       (= start (buffer-char-to-aref buffer (point buffer))))
      (make-match-data :obj buffer
		       :start (buffer-aref-to-char buffer start)
		       :end (buffer-aref-to-char buffer end)
		       :reg-starts (map 'vector (lambda (n)
                                                  (buffer-aref-to-char buffer n))
                                        reg-starts)
		       :reg-ends (map 'vector (lambda (n)
                                                (buffer-aref-to-char buffer n))
                                      reg-ends)))))

(defun re-search-forward (regexp &key (bound (zv)) (error t) count &aux (buffer (current-buffer)))
  "Search forward from point for regular expression regexp.
Set point to the end of the occurrence found, and return match-data structure.
BOUND bounds the search; it is a buffer position.
The match found must not extend after that position.
ERROR, if nil, means if fail just return nil (no error).
  If not nil and not t, move to limit of search and return nil.
COUNT is repeat count--search for successive occurrences.
See also the functions `match-beginning', `match-end', `match-string',
and `replace-match'."
  (declare (ignore count))
  (when (< (buffer-char-to-aref buffer (point buffer))
	   (buffer-gap-start buffer))
    (gap-move-to-point buffer))
  (multiple-value-bind (start end reg-starts reg-ends)
      (ppcre:scan regexp (buffer-data buffer)
                  :start (buffer-char-to-aref buffer (point buffer)) 
                  :end (buffer-char-to-aref buffer bound))
    (cond (start
	   (goto-char (buffer-aref-to-char buffer start) buffer)
	   (make-match-data :obj buffer
                            :start (buffer-aref-to-char buffer start)
                            :end (buffer-aref-to-char buffer end)
                            :reg-starts (map 'vector (lambda (n)
                                                       (buffer-aref-to-char buffer n))
                                             reg-starts)
                            :reg-ends (map 'vector (lambda (n)
                                                     (buffer-aref-to-char buffer n))
                                           reg-ends)))
	  ((eq error t)
	   ;; FIXME: we need a search condition
	   (signal 'search-failed))
	  ((null error)
	   nil)
	  (bound
	   (goto-char bound buffer)
	   nil)
	  (t nil))))

(defun re-search-backward (regexp &key (bound (begv)) (error t) count &aux (buffer (current-buffer)))
  "Search backward from point for match for regular expression regexp.
Set point to the beginning of the match, and return match-data.
The match found is the one starting last in the buffer
and yet ending before the origin of the search.
BOUND bounds the search; it is a buffer position.
The match found must start at or after that position.
ERROR, if nil, means if fail just return nil (no error).
  If not nil and not t, move to limit of search and return nil.
COUNT is repeat count--search for successive occurrences.
See also the functions `match-beginning', `match-end', `match-string',
and `replace-match'."
  (declare (ignore count))
  (message "re-search-backward ~s ~d" regexp (point))
  (when (> (buffer-gap-start buffer)
           (buffer-char-to-aref buffer (point buffer)))
    (gap-move-to buffer (buffer-char-to-aref buffer (1+ (point buffer)))))
  ;; start search from point and keep walking back til we match something
  (let* ((start-aref (buffer-char-to-aref buffer (point buffer)))
         (pt-aref start-aref)
         (stop (buffer-char-to-aref buffer bound))
         (scanner (ppcre:create-scanner regexp)))
    (loop
       (multiple-value-bind (start end reg-starts reg-ends)
           (ppcre:scan scanner (buffer-data buffer) :start start-aref :end pt-aref)
         (when start
           (goto-char (buffer-aref-to-char buffer start) buffer)
           (return (make-match-data :obj buffer
                                    :start (buffer-aref-to-char buffer start)
                                    :end (buffer-aref-to-char buffer end)
                                    :reg-starts (map 'vector (lambda (n)
                                                               (buffer-aref-to-char buffer n))
                                                     reg-starts)
                                    :reg-ends (map 'vector (lambda (n)
                                                             (buffer-aref-to-char buffer n))
                                                   reg-ends))))
         (dec-aref start-aref buffer)
         (when (< start-aref stop)
           (cond ((eq error t)
                  ;; FIXME: we need a search condition
                  (signal 'search-failed))
                 ((null error)
                  (return-from re-search-backward nil))
                 (t
                  (when bound
                    (goto-char bound buffer))
                  (return-from re-search-backward nil))))))))

(defun string-match (regexp string &key (start 0) (end (length string)))
  "Return index of start of first match for regexp in string and match-data, or nil.
Matching ignores case if `case-fold-search' is non-nil.
START, start search at that index in string.
END, end search at that index in string.
**For index of first char beyond the match, do (match-end 0).
**`match-end' and `match-beginning' also give indices of substrings
**matched by parenthesis constructs in the pattern.

You can use the function `match-string' to extract the substrings
matched by the parenthesis constructions in regexp."
  (multiple-value-bind (start end reg-starts reg-ends)
      (ppcre:scan regexp string :start start :end end)
    (when start
      (values start
	      (make-match-data :obj string
			       :start start
			       :end end
			       :reg-starts reg-starts
			       :reg-ends reg-ends)))))

(defun regexp-quote (string)
  "Return a regexp string which matches exactly STRING and nothing else."
  (check-type string string)
  (coerce
   (loop for c across string
      when (find c "[*.\\?+^$" :test 'char=)
      collect #\\
      collect c)
   'string))
  