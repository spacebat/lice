(in-package "LICE")

;; because gnu emacs' match-data is not reentrant we create this
;; structure that is returned for all searching functions. It is
;; passed into the match-data related functions.
(defstruct match-data
  obj start end reg-starts reg-ends)

(defvar *match-data* nil
  "store the match data for searches.")

(defvar *with-match-data* nil
  "Set to true when inside a match-data block. If this is NIL
during one of the searches, a warning is signaled because it's
not thread safe. But, lots of code uses the search functions so
it's useful, at least now to be compatible with gnu emacs, even
if it's not thread safe. Never set this variable directly.")

(defmacro with-match-data (&body body)
  `(let ((*with-match-data* t)
         (*match-data* nil))
     ,@body))

(defun match-end (idx &optional (data *match-data*))
  "Return position of start of text matched by last search.
SUBEXP, a number, specifies which parenthesized expression in the last
  regexp.
Value is nil if SUBEXPth pair didn't match, or there were less than
  SUBEXP pairs.
Zero means the entire text matched by the whole regexp or whole string."
  (if (zerop idx)
      (match-data-end data)
      (aref (match-data-reg-ends data) (1- idx))))

(defun match-beginning (idx &optional (data *match-data*))
  "Return position of start of text matched by last search.
SUBEXP, a number, specifies which parenthesized expression in the last
  regexp.
Value is nil if SUBEXPth pair didn't match, or there were less than
  SUBEXP pairs.
Zero means the entire text matched by the whole regexp or whole string."
  (if (zerop idx)
      (match-data-start data)
      (aref (match-data-reg-starts data) (1- idx))))

(defun match-string (num &optional string)
  "Return string of text matched by last search.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING."
  (if (match-beginning num)
      (if string
	  (substring string (match-beginning num) (match-end num))
          (buffer-substring (match-beginning num) (match-end num)))))


(defun match-string-no-properties (num &optional string)
  "Return string of text matched by last search, without text properties.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING."
  (if (match-beginning num)
      (if string
	  (substring-no-properties string (match-beginning num)
				   (match-end num))
          (buffer-substring-no-properties (match-beginning num)
                                          (match-end num)))))

;; FIXME: needs a formatter and the search string
(define-condition search-failed (lice-condition)
  () (:documentation "raised when a search failed to match"))

(define-condition thread-unsafe (style-warning)
  () (:documentation "Raised when a search is not threadsafe. See also `*with-match-data*'"))

(defun check-search-thread-safe ()
  "Report a warning if the search is unsafe for threads."
  (unless *with-match-data*
    (signal 'thread-unsafe)))

(defun string-search-command (string bound error count direction)
  (check-search-thread-safe)
  (gap-move-to (current-buffer) (buffer-point-aref (current-buffer)))
  ;; normalize vars
  (setf count (* count direction)
        bound (if (minusp count)
                  (if bound (max bound (begv)) (begv))
                  (if bound (min bound (zv)) (zv))))
  (let* ((buffer (current-buffer))
	 pos
         (start-aref (buffer-point-aref buffer))
         (bound-aref (buffer-char-to-aref buffer bound))
	 (n (if (minusp count)
                (loop for i from 0 below (- count)
                   do (setf pos (search string (buffer-data buffer) :from-end t :end2 start-aref :start2 bound-aref))
                   while pos
                   count i)
                (loop for i from 0 below count
                   do (setf pos (search string (buffer-data buffer) :start2 start-aref :end2 bound-aref))
                   while pos
                   count i))))
    (if (/= n (abs count))
        (cond
	  ((eq error t)
	   (signal 'search-failed))
	  ((null error)
	   nil)
	  (bound
	   (set-point bound buffer)
	   nil)
          (t nil))
        (progn
          (if (minusp count)
              (set-point (+ (buffer-aref-to-char buffer pos) (length string)))
              (set-point (buffer-aref-to-char buffer pos)))
          (values (pt)
                  (setf *match-data*
                        (make-match-data :obj buffer
                                         :start (buffer-aref-to-char buffer pos)
                                         :end (+ (buffer-aref-to-char buffer pos) (length string))
                                         :reg-starts #()
                                         :reg-ends #())))))))

(defun search-forward (string &key bound (error t) (count 1))
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
  (string-search-command string bound error count 1))

(defun search-backward (string &key bound (error t) (count 1))
  "Search backward from point for STRING.
Set point to the beginning of the occurrence found, and return point.
An optional second argument bounds the search; it is a buffer position.
The match found must not extend before that position.
Optional third argument, if t, means if fail just return nil (no error).
 If not nil and not t, position at limit of search and return nil.
Optional fourth argument is repeat count--search for successive occurrences.

Search case-sensitivity is determined by the value of the variable
`case-fold-search', which see.

See also the functions `match-beginning', `match-end' and `replace-match'."
  (string-search-command string bound error count -1))

(defvar *regexp-cache* (make-memoize-state :test 'string=))

;; TODO: create compiler-macros for regex functions so the regexps can
;; be compiled at compile time.

(defun looking-at (regexp &optional (buffer (current-buffer)))
  "Return the match-data if text after point matches regular expression regexp."
  (check-type regexp string)
  (check-search-thread-safe)
  ;; get the gap outta the way. It sucks we have to do this. Really we
  ;; should modify ppcre to generate scanner functions that hop the
  ;; gap. Meantime...
  (when (< (buffer-char-to-aref buffer (pt buffer))
	   (buffer-gap-start buffer))
    (gap-move-to-point buffer))
  (multiple-value-bind (start end reg-starts reg-ends)
      (ppcre:scan (memoize *regexp-cache* regexp (ppcre:create-scanner regexp :multi-line-mode t)) (buffer-data buffer) 
                  :start (buffer-char-to-aref buffer (pt buffer))
                  :real-start-pos 0)
    (when (and start
	       (= start (buffer-char-to-aref buffer (pt buffer))))
      (values t
              (setf *match-data*
                    (make-match-data :obj buffer
                                     :start (buffer-aref-to-char buffer start)
                                     :end (buffer-aref-to-char buffer end)
                                     :reg-starts (map 'vector (lambda (n)
                                                                (buffer-aref-to-char buffer n))
                                                      reg-starts)
                                     :reg-ends (map 'vector (lambda (n)
                                                              (buffer-aref-to-char buffer n))
                                                    reg-ends)))))))

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
  (check-search-thread-safe)
  (when (< (buffer-char-to-aref buffer (pt buffer))
	   (buffer-gap-start buffer))
    (gap-move-to-point buffer))
  (multiple-value-bind (start end reg-starts reg-ends)
      (ppcre:scan (memoize *regexp-cache* regexp (ppcre:create-scanner regexp :multi-line-mode t)) (buffer-data buffer)
                  :start (buffer-char-to-aref buffer (pt buffer)) 
                  :end (buffer-char-to-aref buffer bound)
                  :real-start-pos 0)
    (cond (start
	   (set-point (buffer-aref-to-char buffer end) buffer)
           (values (pt)
                   (setf *match-data*
                         (make-match-data :obj buffer
                                          :start (buffer-aref-to-char buffer start)
                                          :end (buffer-aref-to-char buffer end)
                                          :reg-starts (map 'vector (lambda (n)
                                                                     (buffer-aref-to-char buffer n))
                                                           reg-starts)
                                          :reg-ends (map 'vector (lambda (n)
                                                                   (buffer-aref-to-char buffer n))
                                                         reg-ends)))))
	  ((eq error t)
	   (signal 'search-failed))
	  ((null error)
	   nil)
	  (bound
	   (set-point bound buffer)
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
  (check-search-thread-safe)
  ;;(message "re-search-backward ~s ~d" regexp (point))
  (when (> (buffer-gap-start buffer)
           (buffer-char-to-aref buffer (pt buffer)))
    (gap-move-to buffer (buffer-char-to-aref buffer (1+ (pt buffer)))))
  ;; start search from point and keep walking back til we match something
  (let* ((start-aref (buffer-char-to-aref buffer (pt buffer)))
         (pt-aref start-aref)
         (stop (buffer-char-to-aref buffer bound))
         (scanner (memoize *regexp-cache* regexp (ppcre:create-scanner regexp :multi-line-mode t))))
    (loop
       (multiple-value-bind (start end reg-starts reg-ends)
           (ppcre:scan scanner (buffer-data buffer) :start start-aref :end pt-aref :real-start-pos 0)
         (when start
           (set-point (buffer-aref-to-char buffer start) buffer)
           (return (values (pt)
                           (setf *match-data*
                                 (make-match-data :obj buffer
                                                  :start (buffer-aref-to-char buffer start)
                                                  :end (buffer-aref-to-char buffer end)
                                                  :reg-starts (map 'vector (lambda (n)
                                                                             (buffer-aref-to-char buffer n))
                                                                   reg-starts)
                                                  :reg-ends (map 'vector (lambda (n)
                                                                           (buffer-aref-to-char buffer n))
                                                                 reg-ends))))))
         (dec-aref start-aref buffer)
         (when (< start-aref stop)
           (cond ((eq error t)
                  ;; FIXME: we need a search condition
                  (signal 'search-failed))
                 ((null error)
                  (return nil))
                 (t
                  (when bound
                    (set-point bound buffer))
                  (return nil))))))))

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
  (check-search-thread-safe)
  (multiple-value-bind (start end reg-starts reg-ends)
      (ppcre:scan (memoize *regexp-cache* regexp (ppcre:create-scanner regexp :multi-line-mode t))
                  string :start start :end end)
    (when start
      (values start
              (setf *match-data*
                    (make-match-data :obj string
                                     :start start
                                     :end end
                                     :reg-starts reg-starts
                                     :reg-ends reg-ends))))))

(defun regexp-quote (string)
  "Return a regexp string which matches exactly STRING and nothing else."
  (check-type string string)
  (coerce
   (loop for c across string
      when (find c "[*.\\?+^$" :test 'char=)
      collect #\\
      collect c)
   'string))

(defun wordify (string)
  "Given a string of words separated by word delimiters,
compute a regexp that matches those exact words
separated by arbitrary punctuation."
  (error "unimplemented"))

(defun word-search-forward (string &key (bound (begv)) (error t) count &aux (buffer (current-buffer)))
  (error "unimplemented"))

(defun scan-buffer (buffer target start end count)
"Search for COUNT instances of the character TARGET between START and END.

If COUNT is positive, search forwards; END must be >= START.
If COUNT is negative, search backwards for the -COUNTth instance;
   END must be <= START.
If COUNT is zero, do anything you please; run rogue, for all I care.

If END is NIL, use BEGV or ZV instead, as appropriate for the
direction indicated by COUNT.

If we find COUNT instances, return the
position past the COUNTth match and 0.  Note that for reverse motion
this is not the same as the usual convention for Emacs motion commands.

If we don't find COUNT instances before reaching END, return END
and the number of TARGETs left unfound."
  (let ((shortage (abs count))
        last)
    (if (> count 0)
        (setf end (or end (zv buffer)))
        (setf end (or end (begv buffer))))
    (setf start (buffer-char-to-aref buffer start)
          end (buffer-char-to-aref buffer end))
    (loop while (and (> count 0)
                     (/= start end)) do
         (setf start
               (if (< start (buffer-gap-start buffer))
                   (or (position target (buffer-data buffer) :start start :end (min end (buffer-gap-start buffer)))
                       (and (> end (gap-end buffer))
                            (position target (buffer-data buffer) :start (gap-end buffer) :end end)))
                   (position target (buffer-data buffer) :start start :end end)))
         (if start
             (setf start (1+ start)
                   last start
                   count (1- count)
                   shortage (1- shortage))
             (setf start end)))
    (loop while (and (< count 0)
                     (/= start end)) do
         (setf start
               (if (> start (buffer-gap-start buffer))
                   (or (position target (buffer-data buffer) :start (max end (gap-end buffer)) :end start :from-end t)
                       (and (< end (buffer-gap-start buffer))
                            (position target (buffer-data buffer) :start end :end (buffer-gap-start buffer) :from-end t)))
                   (position target (buffer-data buffer) :start end :end start :from-end t)))
         (if start
             (setf last (+ start 1) ; match emacs functionality
                   count (1+ count)
                   shortage (1- shortage))
             (setf start end)))
    (if (zerop count)
        (values (and last (buffer-aref-to-char buffer last)) 0)
        (values (buffer-aref-to-char buffer end) shortage))))

(defun find-before-next-newline (from to cnt)
  "Like find_next_newline, but returns position before the newline,
not after, and only search up to TO.  This isn't just
find_next_newline (...)-1, because you might hit TO."
  (multiple-value-bind (pos shortage) (scan-buffer (current-buffer) #\Newline from to cnt)
    (when (zerop shortage)
      (decf pos))
    pos))

(defun buffer-scan-newline (buf start limit count)
  "Search BUF for COUNT newlines with a limiting point at LIMIT,
starting at START. Returns the point of the last newline or limit and
number of newlines found. START and LIMIT are inclusive."
  (declare (type buffer buf)
	   (type integer start limit count))
  (labels ((buffer-scan-bk (buf start limit count)
	     "count is always >=0. start >= limit."
	     (let* ((start-aref (buffer-char-to-aref buf start))
		    (limit-aref (buffer-char-to-aref buf limit))
		    (ceiling (if (>= start-aref (gap-end buf))
				 (max limit-aref (gap-end buf))
                                 limit-aref))
		    (i 0)
		    ;; :END is not inclusive but START is.
		    (start (1+ start-aref))
		    p)
	       (loop
		;; Always search at least once
		(setf p (position #\Newline (buffer-data buf) 
				  :start ceiling :end start :from-end t))
		(if p
		    (progn
		      ;; Move start. Note that start isn't set to (1+ p)
		      ;; because we don't want to search p again.
		      (setf start p)
		      ;; Count the newline
		      (incf i)
		      ;; Have we found enough newlines?
		      (when (>= i count)
			(return-from buffer-scan-bk (values (buffer-aref-to-char buf p)
							    i))))
		  ;; Check if we've searched up to the limit
		  (if (= ceiling limit-aref)
		      (return-from buffer-scan-bk (values limit i))
		    ;; if not, skip past the gap
		    (progn
		      (setf ceiling limit-aref)
		      (setf start (buffer-gap-start buf))))))))
	   (buffer-scan-fw (buf start limit count)
	     "count is always >=0. start >= limit."
	     (let* ((start-aref (buffer-char-to-aref buf start))
		    (limit-aref (1+ (buffer-char-to-aref buf limit)))
		    (ceiling (if (< start (buffer-gap-start buf))
				 (min limit-aref (buffer-gap-start buf))
                                 limit-aref))
		    (i 0)
		    (start start-aref)
		    p)
	       (loop
		;; Always search at least once
		(setf p (position #\Newline (buffer-data buf) :start start :end ceiling))
		(if p
		    (progn
		      ;; Move start. We don't want to search p again, thus the 1+.
		      (setf start (1+ p))
		      ;; Count the newline
		      (incf i)
		      ;; Have we found enough newlines?
		      (when (>= i count)
			(return-from buffer-scan-fw (values (buffer-aref-to-char buf p)
							    i))))
		  ;; Check if we've searched up to the limit
		  (if (= ceiling limit-aref)
		      (return-from buffer-scan-fw (values limit i))
		    ;; if not, skip past the gap
		    (progn
		      (setf ceiling limit-aref)
		      (setf start (gap-end buf)))))))))
    ;; make sure start and limit are within the bounds
    (setf start (max 0 (min start (1- (buffer-size buf))))
	  limit (max 0 (min limit (1- (buffer-size buf)))))
    ;; the search always fails on an empty buffer
    (when (= (buffer-size buf) 0)
      (return-from buffer-scan-newline (values limit 0)))
    (cond ((> count 0)
	   (dformat +debug-vv+ "scan-fw ~a ~a ~a~%" start limit count)
	   (buffer-scan-fw buf start limit count))
	  ((< count 0)
	   (dformat +debug-vv+ "scan-bk ~a ~a ~a~%" start limit count)
	   (buffer-scan-bk buf start limit (abs count)))
	  ;; 0 means the newline before the beginning of the current
	  ;; line. We need to handle the case where we are on a newline.
	  (t 
	   (dformat +debug-vv+ "scan-0 ~a ~a ~a~%" start limit count)
	   (if (char= (buffer-char-after buf start) #\Newline)
	       (buffer-scan-bk buf start limit 2)
	     (buffer-scan-bk buf start limit 1))))))
