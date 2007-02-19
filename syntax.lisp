;;; Cheap syntax functions

(in-package :lice)

(defparameter +syntax-classes+
  '(:whitespace :punctuation :word-constituent :symbol-constituent :open :close :quote :string :math :escape
    :character-quote :comment :end-comment :inherit :comment-fence :string-fence))

(deftype syntax-class ()
  '(member :whitespace          ; for a whitespace character 
 	   :punctuation 	       ; for random punctuation characters 
 	   :word-constituent    ; for a word constituent 
 	   :symbol-constituent  ; symbol constituent but not word constituent 
 	   :open 	       ; for a beginning delimiter 
 	   :close 	       ; for an ending delimiter 
 	   :quote 	       ; for a prefix character like Lisp ' 
 	   :string 	       ; for a string-grouping character like Lisp " 
 	   :math 	       ; for delimiters like $ in Tex.  
 	   :escape              ; for a character that begins a C-style escape 
	   :character-quote     ; for a character that quotes the
			       ;  following character
 	   :comment 	       ; for a comment-starting character 
 	   :end-comment 	       ; for a comment-ending character 
	   :inherit             ; use the standard syntax table for
			       ;  this character
	   :comment-fence       ; Starts/ends comment which is
			       ; delimited on the other side by any
			       ; char with the same syntaxcode.
	   :string-fence        ; Starts/ends string which is delimited
			       ; on the other side by any char with
			       ; the same syntaxcode.
	  ))

(defclass syntax-table ()
  ((hash :initform (make-hash-table :test 'equal) :initarg :hash :accessor syntax-table-hash)
   (parent :initform nil :initarg :parent :accessor syntax-table-parent))
  (:documentation "A syntax table class."))

(defstruct syntax-descriptor
  (class nil :type syntax-class)
  ;; FIXME: this will be a bitvector at some point but for now just
  ;; make it a list.
  (flags nil :type list)
  ;; this is where the terminator paren char etc is stored
  extra)

(defun set-raw-syntax-entry (table char descriptor)
  (setf (gethash char (syntax-table-hash table)) descriptor))

(defvar *syntax-code-object* (loop for i in +syntax-classes+
				  collect (make-syntax-descriptor :class i :flags nil))

  "A pool of syntax descriptors to be shared in the standard
syntax table in an attempt to save memory. FIXME: premature
optimization?")

(defvar *standard-syntax-table* 
  (let ((table (make-instance 'syntax-table :parent nil))
	tmp)
    ;; Control characters should not be whitespace.
    (setf tmp (getf *syntax-code-object* :punctuation))
    (loop for i below (char-code #\Space) do
	 (set-raw-syntax-entry table (code-char i) tmp))
    ;; Except that a few really are whitespace.
    (setf tmp (getf *syntax-code-object* :whitespace))
    (set-raw-syntax-entry table #\Space tmp)
    (set-raw-syntax-entry table #\Tab tmp)
    (set-raw-syntax-entry table #\Newline tmp)
    (set-raw-syntax-entry table #\Return tmp)
    (set-raw-syntax-entry table #\Page tmp)

    (setf tmp (getf *syntax-code-object* :word-constituent))
    (loop 
       for i from (char-code #\a) to (char-code #\z) 
       for j from (char-code #\A) to (char-code #\Z) do
       (set-raw-syntax-entry table (code-char i) tmp)
       (set-raw-syntax-entry table (code-char j) tmp))
    (loop for i from (char-code #\0) to (char-code #\9) do
	 (set-raw-syntax-entry table (code-char i) tmp))

    (set-raw-syntax-entry table #\$ tmp)
    (set-raw-syntax-entry table #\% tmp)

    (set-raw-syntax-entry table #\( (make-syntax-descriptor :class :open :extra ")"))
    (set-raw-syntax-entry table #\) (make-syntax-descriptor :class :close :extra "("))
    (set-raw-syntax-entry table #\[ (make-syntax-descriptor :class :open :extra "]"))
    (set-raw-syntax-entry table #\] (make-syntax-descriptor :class :close :extra "["))
    (set-raw-syntax-entry table #\{ (make-syntax-descriptor :class :open :extra "}"))
    (set-raw-syntax-entry table #\} (make-syntax-descriptor :class :close :extra "{"))

    (set-raw-syntax-entry table #\" (make-syntax-descriptor :class :string))
    (set-raw-syntax-entry table #\\ (make-syntax-descriptor :class :escape))
    
    (setf tmp (getf *syntax-code-object* :symbol-constituent))
    (loop for i across "_-+*/&|<>=" do
	 (set-raw-syntax-entry table i tmp))

    (setf tmp (getf *syntax-code-object* :punctuation))
    (loop for i across ".,;:?!#@~^'`" do
	 (set-raw-syntax-entry table i tmp))
    ;; TODO: i18n characters
    table)
  "The standard syntax table")

(defun make-syntax-table (&optional (parent *standard-syntax-table*))
  (make-instance 'syntax-table
		 :parent parent))

(defun copy-syntax-table (&optional (table *standard-syntax-table*))
  "Construct a new syntax table and return it.
It is a copy of the TABLE, which defaults to the standard syntax table."
  (let* ((hash (make-hash-table)))
    (maphash (lambda (k v)
	       (setf (gethash k hash) v))
	     (syntax-table-hash table))
    (make-instance 'syntax-table 
		   :hash hash :parent (syntax-table-parent table))))

(defun modify-syntax-entry (char class &key flags extra (table (syntax-table)))
  "Set syntax for character CHAR according to CLASS, FLAGS, and EXTRA."
  (check-type char character)
  (check-type class syntax-class)
  (check-type flags list)
  (check-type table syntax-table)
  (set-raw-syntax-entry table char
			(make-syntax-descriptor :class class :flags flags :extra extra)))

(defun char-syntax (character &optional (table (syntax-table)))
  "Return the syntax code of CHARACTER, described by a character.
For example, if CHARACTER is a word constituent,
the symbol `:WORD-CONSTITUENT' is returned."
  (let ((descr (gethash character (syntax-table-hash table))))
    (when descr
      (syntax-descriptor-class descr))))

(defparameter +word-constituents+ "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")

(defvar *words-include-escapes* nil
  "Non-nil means `forward-word', etc., should treat escape chars part of words.")

(defun syntax-after (pos &aux (buffer (current-buffer)))
  "Return the raw syntax of the char after pos.
If pos is outside the buffer's accessible portion, return nil."
  (let* ((ch (buffer-char-after buffer pos))
	 (descr (and ch (gethash ch (syntax-table buffer)))))
    (when descr
      (syntax-descriptor-class descr))))

;; FIXME: having the flags as a list is memory intensive. How about a
;; bit vector or number and a function that converts between the two?

(defun syntax-table (&aux (buffer (current-buffer)))
  (buffer-syntax-table buffer))

(defun (setf syntax-table) (value &aux (buffer (current-buffer)))
  "Select a new syntax table for the current buffer. One argument, a syntax table."
  (check-type value syntax-table)
  (setf (buffer-syntax-table buffer) value))

;; The above looks a bit weird so lets also have a set function.
(defun set-syntax-table (value)
  "Select a new syntax table for the current buffer. One argument, a syntax table."
  (setf (syntax-table) value))

(defun &syntax-with-flags (ch table &optional (default :whitespace))
  (or (gethash ch (syntax-table-hash table))
      ;; try the parent
      (and (syntax-table-parent table)
           (&syntax-with-flags ch (syntax-table-parent table) default))
      ;; return the default
      (make-syntax-descriptor :class default)))

(defun &syntax (ch table &optional (default :whitespace))
  (let ((descr (&syntax-with-flags ch table default)))
    (if descr
	(syntax-descriptor-class descr)
	default)))

(defun &syntax-flags-syntax (syntax)
  (syntax-descriptor-class syntax))

(defun &syntax-comment-start-first (ch table)
  (let ((descr (&syntax-with-flags ch table)))
    (when descr
      (and (find :comment-start-first (syntax-descriptor-flags descr)) t))))

(defun &syntax-flags-comment-start-first (syntax)
  (and (find :comment-start-first (syntax-descriptor-flags syntax)) t))

(defun &syntax-comment-start-second (ch table)
  (let ((descr (&syntax-with-flags ch table)))
    (when descr
      (and (find :comment-start-second (syntax-descriptor-flags descr)) t))))

(defun &syntax-flags-comment-start-second (syntax)
  (and (find :comment-start-second (syntax-descriptor-flags syntax)) t))

(defun &syntax-comment-end-first (ch table)
  (let ((descr (&syntax-with-flags ch table)))
    (when descr
      (and (find :comment-end-first (syntax-descriptor-flags descr)) t))))

(defun &syntax-flags-comment-end-first (syntax)
  (and (find :comment-end-first (syntax-descriptor-flags syntax)) t))

(defun &syntax-comment-end-second (ch table)
  (let ((descr (&syntax-with-flags ch table)))
    (when descr
      (and (find :comment-end-second (syntax-descriptor-flags descr)) t))))

(defun &syntax-flags-comment-end-second (syntax)
  (and (find :comment-end-second (syntax-descriptor-flags syntax)) t))

(defun &syntax-prefix (ch table)
  (let ((descr (&syntax-with-flags ch table)))
    (when descr
      (and (find :prefix (syntax-descriptor-flags descr)) t))))

(defun &syntax-flags-prefix (syntax)
  (and (find :prefix (syntax-descriptor-flags syntax)) t))

(defun &syntax-comment-style (ch table)
  (let ((descr (&syntax-with-flags ch table)))
    (when descr
      (and (find :comment-style (syntax-descriptor-flags descr)) t))))

(defun &syntax-flags-comment-style (syntax)
  (and (find :comment-style (syntax-descriptor-flags syntax)) t))

(defun &syntax-comment-nested (ch table)
  (let ((descr (&syntax-with-flags ch table)))
    (when descr
      (and (find :comment-nested (syntax-descriptor-flags descr)) t))))

(defun &syntax-flags-comment-nested (syntax)
  (and (find :comment-nested (syntax-descriptor-flags syntax)) t))

(defun scan-words (from count)
  "Return the position across COUNT words from FROM.
If that many words cannot be found before the end of the buffer,
return NIL.  COUNT negative means scan backward and stop at word
beginning."
  (let* ((buffer (current-buffer))
	 (beg (begv buffer))
	 (end (zv buffer))
	 (from-aref (buffer-char-to-aref buffer from))
	 (table (syntax-table))
	 ch code)
    (while (> count 0)
      (loop
         (when (= from end)
           (return-from scan-words nil))
         (setf ch (buffer-fetch-char from-aref buffer)
               code (&syntax ch table))
         (inc-both from from-aref buffer)
         (when (or (and *words-include-escapes*
                        (or (eq code :escape)
                            (eq code :character-quote)))
                   (eq code :word-constituent))
           (return nil)))
      (loop
         (when (= from end)
           (return nil))
         (setf ch (buffer-fetch-char from-aref buffer)
               code (&syntax from-aref table))
         (when (and (and (not *words-include-escapes*)
                         (or (eq code :escape)
                             (eq code :character-quote)))
                    (or (not (eq code :word-constituent))))
           ;; (word-boundary-p ..)
           (return nil))
         (inc-both from from-aref buffer))
      (decf count))
    (while (< count 0)
      (loop
         (when (= from beg)
           (return-from scan-words nil))
         (dec-both from from-aref buffer)
         (setf ch (buffer-fetch-char from-aref buffer)
               code (&syntax ch table))
         (when (or (and *words-include-escapes*
                        (or (eq code :escape) 
                            (eq code :character-quote)))
                   (eq code :word-constituent))
           (return nil)))
      (loop
         (when (= from beg)
           (return nil))
         (setf ch (buffer-fetch-char from-aref buffer)
               code (&syntax ch table))
         (when (and (and (not *words-include-escapes*)
                         (or (eq code :escape) 
                             (eq code :character-quote)))
                    (not (eq code :word-constituent)))
           (return nil))
         (dec-both from from-aref buffer))
      (incf count))
    from))
	 
(defcommand forward-word ((n) :prefix)
  "Move point forward ARG words (backward if ARG is negative).
Normally returns t.
If an edge of the buffer or a field boundary is reached, point is left there
and the function returns nil.  Field boundaries are not noticed if
`inhibit-field-text-motion' is non-nil."
  (labels ((isaword (c)
	     (find c +word-constituents+ :test #'char=)))
    (let ((buffer (current-buffer)))
      (cond ((> n 0)
	     (gap-move-to buffer (buffer-point-aref buffer))
	     ;; do it n times
	     (loop for i from 0 below n
		   while (let (p1 p2)
			   ;; search forward for a word constituent
			   (setf p1 (position-if #'isaword (buffer-data buffer) 
						 :start (buffer-point-aref buffer)))
			   ;; search forward for a non word constituent
			   (when p1
			     (setf p2 (position-if (complement #'isaword) (buffer-data buffer) :start p1)))
			   (if p2
			       (goto-char (buffer-aref-to-char buffer p2))
			     (goto-char (point-max)))
			   p2)))
	    ((< n 0)
	     (setf n (- n))
	     (gap-move-to buffer (buffer-point-aref buffer))
	     ;; do it n times
	     (loop for i from 0 below n
		   for start = (buffer-gap-start buffer) then (buffer-point-aref buffer)
		   while (let (p1 p2)
			   ;; search backward for a word constituent
			   (setf p1 (position-if #'isaword (buffer-data buffer) 
						 :from-end t
						 :end start))
			   ;; search backward for a non word constituent
			   (when p1
			     (setf p2 (position-if (complement #'isaword) (buffer-data buffer) :from-end t :end p1)))
			   (if p2
			       (goto-char (1+ (buffer-aref-to-char buffer p2)))
			     (goto-char (point-min)))
			   p2)))))))

(defcommand backward-word ((n) :prefix)
  "Move point forward ARG words (backward if ARG is negative).
Normally returns t.
If an edge of the buffer or a field boundary is reached, point is left there
and the function returns nil.  Field boundaries are not noticed if
`inhibit-field-text-motion' is non-nil."
  (forward-word (- n)))

(defcommand kill-word ((arg)
		       :prefix)
  "Kill characters forward until encountering the end of a word.
With argument, do this that many times."
  (kill-region (point) (progn (forward-word arg) (point))))

(defcommand backward-kill-word ((arg)
				:prefix)
  "Kill characters backward until encountering the end of a word.
With argument, do this that many times."
  (kill-word (- arg)))

(defvar *parse-sexp-ignore-comments* t
  "Non-nil means `forward-sexp', etc., should treat comments as whitespace.")

(defvar *open-paren-in-column-0-is-defun-start* t
  "*Non-nil means an open paren in column 0 denotes the start of a defun.")

;; Conditions used by the syntax code because the parsing is so bloody
;; hairy.
(define-condition syntax-done () ())
(define-condition syntax-lossage () ())

(define-condition expression-ends-prematurely (lice-condition)
  ())
(define-condition unbalanced-parenthesis (lice-condition)
  ((last-good :initarg :last-good :accessor unbalanced-parenthesis-last-good)
   (from :initarg :from :accessor unbalanced-parenthesis-from)))

(defun &char-quoted (char-pos aref-pos buffer table)
  "Returns TRUE if char at CHAR-POS is quoted.
Global syntax-table data should be set up already to be good at CHAR-POS
or after.  On return global syntax data is good for lookup at CHAR-POS."
  (let ((beg (begv buffer))
        quoted)
    (dec-both char-pos aref-pos buffer)
    (while (>= char-pos beg)
      (let* ((ch (buffer-fetch-char aref-pos buffer))
             (code (&syntax ch table)))
        (when (not (find code '(:character-quote :escape)))
          (return nil))
        (dec-both char-pos aref-pos buffer)
        (setf quoted (not quoted))))
    quoted))

(defun find-defun-start (pos pos-aref buffer table)
  "Return a defun-start position before POS and not too far before.
It should be the last one before POS, or nearly the last.

When open_paren_in_column_0_is_defun_start is nonzero,
only the beginning of the buffer is treated as a defun-start.

We record the information about where the scan started
and what its result was, so that another call in the same area
can return the same value very quickly.

There is no promise at which position the global syntax data is
valid on return from the subroutine, so the caller should explicitly
update the global data."
  (declare (ignore pos-aref))
  (unless *open-paren-in-column-0-is-defun-start*
    (return-from find-defun-start (make-parse-state :start-value (begv buffer)
                                                    :start-value-aref (begv-aref buffer))))
  ;; Back up to start of line.
  (let* ((begv (begv buffer))
         (pt (buffer-scan-newline buffer pos (begv buffer) -1))
         (pt-aref (buffer-char-to-aref buffer pt)))
    (while (> pt begv)
      (let ((ch (buffer-fetch-char pt-aref buffer)))
        (when (eq (&syntax ch table) :open)
          (return nil))
        ;; Move to beg of previous line.
        (setf pt (buffer-scan-newline buffer pt (begv buffer) -2)
              pt-aref (buffer-char-to-aref buffer pt))))
    ;; Return what we found
    (make-parse-state :start-value pt
                      :start-value-aref pt-aref
                      :start-buffer buffer
                      ;; :modiff MODIFF
                      :start-begv (begv buffer)
                      :start-pos pos)))


(defun skip-chars (forwardp syntaxp string lim)
  (declare (ignore syntaxp))
  (labels ((match-char (c negate ranges chars)
             ;; check ranges
             (catch :continue
               (loop for r in ranges do
                    (if negate
                        (when (<= (car r) (char-code c) (cdr r))
                          (throw :done nil))
                        (when (<= (car r) (char-code c) (cdr r))
                          (throw :continue nil))))
               ;; check chars
               (if negate
                   (when (find c chars :test 'char=)
                     (throw :done nil))
                   (when (find c chars :test 'char=)
                     (throw :continue nil)))
               (unless negate
                 ;; if the char fell through all that then we're done
                 (throw :done nil)))))
    (check-type string string)
    (check-number-coerce-marker lim)
    (let ((idx 0)
          negate
          ranges
          chars
          (start-point (point)))
      ;; don't allow scan outside bounds of buffer.
      (setf lim (min (max lim (begv)) (zv)))
    
      (when (char= (char string 0) #\^)
        (setf negate t)
        (incf idx))

      ;; compile the ranges and chars 
      (while (< idx (length string))
        (let ((c (char string idx)))
            
          ;;         (if syntaxp
          ;;             ;; TODO: handle syntaxp
          ;;             nil
          ;;             (progn
          ;;               ;; TODO: handle iso classes
          ;;               ))
          (when (char= c #\\)
            (incf idx)
            (when (= idx (length string))
              (return nil))
            (setf c (char string idx)))
          (incf idx)

          ;; Treat `-' as range character only if another character
          ;; follows.
          (if (and (< (1+ idx) (length string))
                   (char= (char string idx) #\-))
              (progn
                (incf idx)
                (let* ((c2 (char string idx))
                       (code1 (char-code c))
                       (code2 (char-code c2)))
                  (when (<= code1 code2)
                    (push (cons code1 code2) ranges))
                  (incf idx)))
              (progn
                (push c chars)))))
      ;; scan
      (let* ((buffer (current-buffer))
             (pos (point buffer))
             (pos-aref (buffer-char-to-aref buffer pos)))
        (catch :done
          (if forwardp
              (while (< pos lim)
                (match-char (buffer-fetch-char pos-aref buffer)
                            negate ranges chars)
                (inc-both pos pos-aref buffer))
              (progn
                ;; do a little dance to end up in the right spot
                (dec-both pos pos-aref buffer)
                (unwind-protect
                     (while (> pos lim)
                       (match-char (buffer-fetch-char pos-aref buffer)
                                   negate ranges chars)
                       (dec-both pos pos-aref buffer))
                  (inc-both pos pos-aref buffer)))))
        (set-point pos buffer)
        ;; return the number of chars we scanned
        (- pos start-point)))))
                       
(defun skip-chars-forward (string &optional (lim (zv)))
  "Move point forward, stopping before a char not in string."
  (skip-chars t nil string lim))

(defun skip-chars-backward (string &optional (lim (begv)))
  "Move point backward, stopping after a char not in string."
  (skip-chars nil nil string lim))

(defun &back-comment (from from-aref stop comment-nested comment-style buffer table)
  "Checks whether charpos FROM is at the end of a comment.
FROM_BYTE is the bytepos corresponding to FROM.
Do not move back before STOP.

Return a positive value if we find a comment ending at FROM/FROM_BYTE;
return -1 otherwise.

If successful, return the charpos of the comment's beginning, and the aref pos.

**Global syntax data remains valid for backward search starting at
**the returned value (or at FROM, if the search was not successful)."
  ;; Look back, counting the parity of string-quotes,
  ;; and recording the comment-starters seen.
  ;; When we reach a safe place, assume that's not in a string;
  ;; then step the main scan to the earliest comment-starter seen
  ;; an even number of string quotes away from the safe place.
  ;;
  ;; OFROM[I] is position of the earliest comment-starter seen
  ;; which is I+2X quotes from the comment-end.
  ;; PARITY is current parity of quotes from the comment end.
  (let ((string-style :none)
        (string-lossage nil)
        ;; Not a real lossage: indicates that we have passed a matching comment
        ;; starter plus a non-matching comment-ender, meaning that any matching
        ;; comment-starter we might see later could be a false positive (hidden
        ;; inside another comment).
        ;; Test case:  { a (* b } c (* d *)
        (comment-lossage nil)
        (comment-end from)
        (comment-end-aref from-aref)
        (comment-start-pos 0)
        comment-start-aref
        ;; Place where the containing defun starts,
        ;; or nil if we didn't come across it yet. 
        defun-start
        defun-start-aref
        code
        (nesting 1) ; current comment nesting
        ch
        (syntax nil))
    (handler-case 
        (progn
          ;; FIXME: A }} comment-ender style leads to incorrect behavior
          ;; in the case of {{ c }}} because we ignore the last two chars which are
          ;; assumed to be comment-enders although they aren't.
          ;;
          ;; At beginning of range to scan, we're outside of strings;
          ;; that determines quote parity to the comment-end.
          (while (/= from stop)
            (catch :continue
              (let (temp-aref prev-syntax com2start com2end)
                (dec-both from from-aref buffer)
                (setf prev-syntax syntax
                      ch (buffer-fetch-char from-aref buffer)
                      syntax (&syntax-with-flags ch table)
                      code (&syntax ch table)
                      ;; Check for 2-char comment markers.
                      com2start (and (&syntax-flags-comment-start-first syntax)
                                     (&syntax-flags-comment-start-second prev-syntax)
                                     (eq comment-style (&syntax-flags-comment-style prev-syntax))
                                     (eq (or (&syntax-flags-comment-nested prev-syntax)
                                             (&syntax-flags-comment-nested syntax))
                                         comment-nested))
                      com2end (and (&syntax-flags-comment-end-first syntax)
                                   (&syntax-flags-comment-end-second prev-syntax)))
                ;; Nasty cases with overlapping 2-char comment markers:
                ;; - snmp-mode: -- c -- foo -- c --
                ;;              --- c --
                ;;	      ------ c --
                ;; - c-mode:    *||*
                ;;	      |* *|* *|
                ;;	      |*| |* |*|
                ;;	      ///   */

                ;; If a 2-char comment sequence partly overlaps with
                ;; another, we don't try to be clever.
                (when (and (> from stop)
                           (or com2end com2start))
                  (let ((next from)
                        (next-aref from-aref)
                        next-c 
                        next-syntax)
                    (dec-both next next-aref buffer)
                    (setf next-c (buffer-fetch-char next-aref buffer)
                          next-syntax (&syntax-with-flags next-c table))
                    (when (or (and (or com2start comment-nested)
                                   (&syntax-flags-comment-end-second syntax)
                                   (&syntax-flags-comment-end-first next-syntax))
                              (and (or com2end comment-nested)
                                   (&syntax-flags-comment-start-second syntax)
                                   (eq comment-style (&syntax-flags-comment-style syntax))
                                   (&syntax-flags-comment-start-first next-syntax)))
                      (signal 'syntax-lossage))))

                (when (and com2start 
                           (= comment-start-pos 0))
                  ;; We're looking at a comment starter.  But it might be a comment
                  ;; ender as well (see snmp-mode).  The first time we see one, we
                  ;; need to consider it as a comment starter,
                  ;; and the subsequent times as a comment ender.
                  (setf com2end 0))
               
                ;; Turn a 2-char comment sequences into the appropriate syntax.
                (cond (com2end
                       (setf code :end-comment))
                      (com2start
                       (setf code :comment))
                      ;; Ignore comment starters of a different style.
                      ((and (eq code :comment)
                            (or (not (eq comment-style (&syntax-flags-comment-style syntax)))
                                (not (eq comment-nested (&syntax-flags-comment-nested syntax)))))
                       (throw :continue nil)))

                ;; Ignore escaped characters, except comment-enders.
                (when (and (not (eq code :end-comment))
                           (&char-quoted from from-aref buffer table))
                  (throw :continue nil))

                (case code
                  ((:string-fence :comment-fence :string)
                   (when (find code '(:string-fence :comment-fence))
                     (setf ch (if (eq code :string-fence) 
                                  :string-style
                                  :comment-style)))
                   ;; Track parity of quotes.
                   (cond ((eq string-style :none)
                          ;; Entering a string.
                          (setf string-style ch))
                         ((eq string-style ch)
                          ;; leaving the string
                          (setf string-style :none))
                         (t
                          ;; If we have two kinds of string delimiters.
                          ;; There's no way to grok this scanning backwards.
                          (setf string-lossage t))))
                  (:comment
                   ;; We've already checked that it is the relevant comstyle.
                   (when (or (eq string-style :none)
                             comment-lossage
                             string-lossage)
                     ;; There are odd string quotes involved, so let's be careful.
                     ;; Test case in Pascal: " { " a { " } */
                     (signal 'syntax-lossage))
                   (if (not comment-nested)
                       ;; Record best comment-starter so far.
                       (setf comment-start-pos from
                             comment-start-aref from-aref)
                       (progn
                         (decf nesting)
                         (when (<= nesting)
                           ;; nested comments have to be balanced, so we don't need to
                           ;; keep looking for earlier ones.  We use here the same (slightly
                           ;; incorrect) reasoning as below:  since it is followed by uniform
                           ;; paired string quotes, this comment-start has to be outside of
                           ;; strings, else the comment-end itself would be inside a string.
                           (signal 'syntax-done)))))
                  (:end-comment
                   (cond ((and (eq comment-style (&syntax-flags-comment-style syntax))
                               (or (and com2end
                                        (&syntax-flags-comment-nested prev-syntax))
                                   (eq comment-nested (&syntax-flags-comment-nested syntax))))
                          ;; This is the same style of comment ender as ours.
                          (if comment-nested
                              (incf nesting)
                              ;; Anything before that can't count because it would match
                              ;; this comment-ender rather than ours.
                              (setf from stop)))
                         ((or (/= comment-start-pos 0)
                              (char/= ch #\Newline))
                          ;; We're mixing comment styles here, so we'd better be careful.
                          ;; The (comstart_pos != 0 || c != '\n') check is not quite correct
                          ;; (we should just always set comment_lossage), but removing it
                          ;; would imply that any multiline comment in C would go through
                          ;; lossage, which seems overkill.
                          ;; The failure should only happen in the rare cases such as
                          ;; { (* } *)
                          (setf comment-lossage t))))
                  (:open
                   ;; Assume a defun-start point is outside of strings.
                   (when (and *open-paren-in-column-0-is-defun-start*
                              (or (= from stop)
                                  (progn
                                    (setf temp-aref (aref-minus-1 from-aref buffer))
                                    (char= (buffer-fetch-char temp-aref buffer) #\Newline))))
                     (setf defun-start from
                           defun-start-aref from-aref
                           ;; Break out of the loop.
                           from stop)))))))

          (if (= comment-start-pos 0)
              (setf from comment-end
                    from-aref comment-end-aref)
              ;; If comstart_pos is set and we get here (ie. didn't jump to `lossage'
              ;; or `done'), then we've found the beginning of the non-nested comment.
              (setf from comment-start-pos
                    from-aref comment-start-aref)))
      (syntax-lossage ()
        ;; We had two kinds of string delimiters mixed up
        ;; together.  Decode this going forwards.
        ;; Scan fwd from a known safe place (beginning-of-defun)
        ;; to the one in question; this records where we
        ;; last passed a comment starter.
        ;; If we did not already find the defun start, find it now.
        (when (= defun-start 0)
            (let ((ret (find-defun-start comment-end comment-end-aref buffer table)))
              (setf defun-start (parse-state-start-value ret)
                    defun-start-aref (parse-state-start-value-aref ret))))
        (loop do
             (let ((state (scan-sexps-forward defun-start defun-start-aref
                                              comment-end -10000 0 nil 0 buffer table)))
               (setf defun-start comment-end)
               (if (and (eq (parse-state-in-comment state) comment-nested)
                        (eq (parse-state-comment-style state) comment-style))
                   (setf from (parse-state-comment-string-start state))
                   (progn
                     (setf from comment-end)
                     (when (parse-state-in-comment state) ; XXX
                       ;; If comment_end is inside some other comment, maybe ours
                       ;; is nested, so we need to try again from within the
                       ;; surrounding comment.  Example: { a (* " *)
                       (setf defun-start (+ (parse-state-comment-string-start state) 2)
                             defun-start-aref (buffer-char-to-aref buffer defun-start))))))
             while (< defun-start comment-end))
        (setf from-aref (buffer-char-to-aref buffer from))))
    (values (if (= from comment-end) -1 from)
            from-aref)))

(defun &forward-comment (from from-aref stop nesting style prev-syntax buffer table)
  "Jump over a comment, assuming we are at the beginning of one.
FROM is the current position.
FROM_BYTE is the bytepos corresponding to FROM.
Do not move past STOP (a charpos).
The comment over which we have to jump is of style STYLE
  (either SYNTAX_COMMENT_STYLE(foo) or ST_COMMENT_STYLE).
NESTING should be positive to indicate the nesting at the beginning
  for nested comments and should be zero or negative else.
  ST_COMMENT_STYLE cannot be nested.
PREV_SYNTAX is the SYNTAX_WITH_FLAGS of the previous character
  (or nil If the search cannot start in the middle of a two-character).

If successful, return 1 and store the charpos of the comment's end
into *CHARPOS_PTR and the corresponding bytepos into *BYTEPOS_PTR.
Else, return 0 and store the charpos STOP into *CHARPOS_PTR, the
corresponding bytepos into *BYTEPOS_PTR and the current nesting
 (as defined for state.incomment) in *INCOMMENT_PTR.

The comment end is the last character of the comment rather than the
  character just after the comment.

Global syntax data is assumed to initially be valid for FROM and
remains valid for forward search starting at the returned position."
  (let (c
        c1
        code
        (syntax prev-syntax))
    (labels ((forward ()
               (when (= from stop)
                 (return-from &forward-comment
                   (values nil from from-aref nesting)))

               (setf c (buffer-fetch-char from-aref buffer)
                     code (&syntax c table)
                     syntax (&syntax-with-flags c table))

               (when (and (eq code :end-comment)
                          (eq (&syntax-flags-comment-style syntax) style)
                          (if (&syntax-flags-comment-nested syntax)
                              (and (> nesting 0)
                                   (progn (decf nesting)
                                          (zerop nesting)))
                              (< nesting 0)))
                 ;; we have encountered a comment end of the same
                 ;; style as the comment sequence which began this
                 ;; comment section.
                 (throw :done nil))
               (when (and (eq code :comment-fence)
                          (eq style :st-comment-style))
                 ;; we have encountered a comment end of the same style
                 ;; as the comment sequence which began this comment
                 ;; section.
                 (throw :done nil))
               (when (and (> nesting 0)
                          (eq code :comment)
                          (&syntax-flags-comment-nested syntax)
                          (eq (&syntax-flags-comment-style syntax) style))
                 ;; we have encountered a nested comment of the same style
                 ;; as the comment sequence which began this comment section
                 (incf nesting))
               (inc-both from from-aref buffer))
             (do-comment ()
               (when (and (< from stop)
                          (&syntax-flags-comment-end-first syntax)
                          (eq (&syntax-flags-comment-style syntax) style)
                          (progn 
                            (setf c1 (buffer-fetch-char from-aref buffer))
                            (&syntax-comment-end-second c1 table))
                          (if (or (&syntax-flags-comment-nested syntax)
                                  (&syntax-comment-nested c1 table))
                              (> nesting 0)
                              (< nesting 0)))
                 (decf nesting)
                 (if (<= nesting 0)
                     ;; we have encountered a comment end of the same style
                     ;; as the comment sequence which began this comment
                     ;; section
                     (throw :done nil)
                     (inc-both from from-aref buffer)))
               (when (and (> nesting 0)
                          (< from stop)
                          (&syntax-flags-comment-start-first syntax)
                          (progn
                            (setf c1 (buffer-fetch-char from-aref buffer))
                            (eq (&syntax-comment-style c1 table) style))
                          (or (&syntax-flags-comment-nested syntax)
                              (&syntax-comment-nested c1 table)))
                 ;; we have encountered a nested comment of the same style
                 ;; as the comment sequence which began this comment
                 ;; section
                 (inc-both from from-aref buffer)
                 (incf nesting))))
      ;; normalize nesting
      (cond ((or (null nesting)
                 (<= nesting 0))
             (setf nesting -1))
            ((not (numberp nesting))
             (setf nesting 1)))
      ;; Enter the loop in the middle so that we find
      ;; a 2-char comment ender if we start in the middle of it.
      (catch :done
        (if syntax
            (progn
              (do-comment)
              (loop
                 (forward)
                 (do-comment)))
            (loop
               (forward)
               (do-comment))))
      (values t from from-aref))))
    
(defun prev-char-comment-end-first (pos pos-aref buffer table)
  "Return the SYNTAX_COMEND_FIRST of the character before POS, POS_BYTE."
  (dec-both pos pos-aref buffer)
  (&syntax-comment-end-first (buffer-fetch-char pos-aref buffer)
                             table))

(defun &scan-lists (from count depth sexpflag &aux (buffer (current-buffer)))
  "This is from the emacs function"
  ;; the big TODO here is to use the CL readtable
  (labels ((lose (last-good from)
             (signal 'unbalanced-parenthesis :last-good last-good :from from)))
    (let ((stop (if (> count 0) (zv buffer) (begv buffer)))
          (from-aref (buffer-char-to-aref buffer from))
          (min-depth (min 0 depth))
          (table (syntax-table))
          (last-good from)
          quoted
          math-exit
          comment-start-first
          code
          ch ch1
          temp-code
          temp-pos
          comment-nested
          comment-style
          found
          prefix)
      ;; normalize FROM
      (setf from (max (min (zv buffer) from)
                      (begv buffer)))
      (while (> count 0)
        ;; the code needs to be able to jump out of the mess it got
        ;; in.
        (handler-case
            (progn
              (while (< from stop)
                (catch :continue
                  (setf ch (buffer-fetch-char from-aref buffer)
                        code (&syntax ch table)
                        comment-start-first (&syntax-comment-start-first ch table)
                        comment-nested (&syntax-comment-nested ch table)
                        comment-style (&syntax-comment-style ch table)
                        prefix (&syntax-prefix ch table))
                  (when (= depth min-depth)
                    (setf last-good from))
                  (inc-both from from-aref buffer)
                  (when (and (< from stop) comment-start-first
                             (progn (setf ch (buffer-fetch-char from-aref buffer))
                                    (&syntax-comment-start-second ch table))
                             *parse-sexp-ignore-comments*)
                    ;; we have encountered a comment start sequence and
                    ;; we are ignoring all text inside comments.  We
                    ;; must record the comment style this sequence
                    ;; begins so that later, only a comment end of the
                    ;; same style actually ends the comment section
                    (setf code :comment
                          ch1 (buffer-fetch-char from-aref buffer)
                          comment-style (&syntax-comment-style ch1 table)
                          comment-nested (or comment-nested
                                             (&syntax-comment-nested ch1 table)))
                    (inc-both from from-aref buffer))

                  (when prefix
                    (throw :continue nil))

                  (when (or (eq code :escape)
                            (eq code :character-quote))
                    (when (= from stop) (lose last-good from))
                    (inc-both from from-aref buffer)
                    ;; treat following character as a word constituent
                    (setf code :word-constituent))

                  (case code
                    ((:word-constituent :symbol-constituent)
                     (unless (or (not (zerop depth)) 
                                 (not sexpflag))
                       (let (temp)
                         (while (< from stop)
                           (setf ch (buffer-fetch-char from-aref buffer)
                                 temp (&syntax ch table))
                           (case temp
                             ((:escape :character-quote)
                              (inc-both from from-aref buffer)
                              (when (= from stop) (lose last-good from)))
                             ((:word-constituent :symbol-constituent :quote))
                             (t
                              (signal 'syntax-done)))
                           (inc-both from from-aref buffer)))
                       (signal 'syntax-done)))
                    ((:comment-fence :comment)
                     (when (eq code :comment-fence)
                       (setf comment-style :st-comment-style))
                     (multiple-value-setq (found from from-aref) (&forward-comment from from-aref stop comment-nested comment-style nil buffer table))
                     (unless found
                       (when (zerop depth) (signal 'syntax-done))
                       (lose last-good from))
                     (inc-both from from-aref buffer))
                    (:math
                     (when sexpflag
                       (when (and (/= from stop)
                                  (char= ch (buffer-fetch-char from-aref buffer)))
                         (inc-both from from-aref buffer))
                       (if math-exit
                           (progn
                             (setf math-exit nil)
                             (decf depth)
                             (when (zerop depth) (signal 'syntax-done))
                             (when (< depth min-depth)
                               (signal 'expression-ends-prematurely))) ; XXX
                           (progn
                             (setf math-exit t)
                             (incf depth)
                             (when (zerop depth) (signal 'syntax-done))))))
                    (:open
                     (incf depth)
                     (when (zerop depth) (signal 'syntax-done)))
                    (:close
                     (decf depth)
                     (when (zerop depth) (signal 'syntax-done))
                     (when (< depth min-depth)
                       (signal 'expression-ends-prematurely)))
                    ((:string :string-fence)
                     (let* ((tmp-pos (aref-minus-1 from-aref buffer))
                            (string-term (buffer-fetch-char tmp-pos buffer))
                            temp)
                       (loop
                          (when (>= from stop) (lose last-good from))
                          (setf ch (buffer-fetch-char from-aref buffer))
                          (when (if (eq code :string)
                                    (and (char= ch string-term)
                                         (eq (&syntax ch table) :string))
                                    (eq (&syntax ch table) :string-fence))
                            (return nil))
                          (setf temp (&syntax ch table))
                          (case temp
                            ((:character-quote :escape)
                             (inc-both from from-aref buffer)))
                          (inc-both from from-aref buffer))
                       (inc-both from from-aref buffer)
                       (when (and (zerop depth)
                                  sexpflag)
                         (signal 'syntax-done))))
                    (t
                     ;; Ignore whitespace, punctuation, quote, endcomment.
                     ))))
              (unless (zerop depth) (lose last-good from))
              (return-from &scan-lists nil))
          (syntax-done ()
            (decf count))))

      (while (< count 0)
        (handler-case
            (progn
              (while (> from stop)
                (catch :continue
                  (dec-both from from-aref buffer)
                  (setf ch (buffer-fetch-char from-aref buffer)
                        code (&syntax ch table))
                  (when (= depth min-depth)
                    (setf last-good from))
                  (setf comment-style nil
                        comment-nested (&syntax-comment-nested ch table))
                  (when (eq code :end-comment)
                    (setf comment-style (&syntax-comment-style ch table)))
                  (when (and (> from stop)
                             (&syntax-comment-end-second ch table)
                             (prev-char-comment-end-first from from-aref buffer table)
                             *parse-sexp-ignore-comments*)
                    ;; We must record the comment style
                    ;; encountered so that later, we can match
                    ;; only the proper comment begin sequence of
                    ;; the same style.
                    (dec-both from from-aref buffer)
                    (setf code :end-comment
                          ch1 (buffer-fetch-char from-aref buffer)
                          comment-nested (or comment-nested 
                                             (&syntax-comment-nested ch1 table))))
                  ;; Quoting turns anything except a comment-ender
                  ;; into a word character.  Note that this cannot
                  ;; be true if we decremented FROM in the
                  ;; if-statement above.
                  (cond 
                    ((and (not (eq code :end-comment))
                          (&char-quoted from from-aref buffer table))
                     (dec-both from from-aref buffer)
                     (setf code :word))
                    ((&syntax-prefix ch table)
                     ;; loop around again. I think this is nasty but fuckit.
                     (throw :continue nil)))
                  (case code
                    ((:word-constituent :symbol-constituent :escape :character-quote)
                     (unless (or (not (zerop depth))
                                 (not sexpflag))
                       ;;  This word counts as a sexp; count
                       ;;  object finished after passing it.
                       (while (> from stop)
                         (setf temp-pos from-aref)
                         (decf temp-pos)
                         (setf ch1 (buffer-fetch-char temp-pos buffer)
                               temp-code (&syntax ch1 table))
                         ;; Don't allow comment-end to be quoted.
                         (when (eq temp-code :end-comment)
                           (signal 'syntax-done))
                         (setf quoted (&char-quoted (1- from) temp-pos buffer table))
                         (when quoted
                           (dec-both from from-aref buffer)
                           (setf temp-pos (aref-minus-1 temp-pos buffer)))
                         (setf ch1 (buffer-fetch-char temp-pos buffer)
                               temp-code (&syntax ch1 table))
                         (when (not (or quoted 
                                        (eq temp-code :word-constituent)
                                        (eq temp-code :symbol-constituent)
                                        (eq temp-code :quote)))
                           (signal 'syntax-done))
                         (dec-both from from-aref buffer))
                       (signal 'syntax-done)))
                    (:math
                     (when sexpflag
                       (setf temp-pos (aref-minus-1 from-aref buffer))
                       (when (and (/= from stop)
                                  (char= ch (buffer-fetch-char temp-pos buffer)))
                         (dec-both from from-aref buffer))
                       (if math-exit
                           (progn
                             (setf math-exit nil)
                             (decf depth)
                             (when (zerop depth) (signal 'syntax-done))
                             (when (< depth min-depth)
                               (signal 'expression-ends-prematurely)))
                           (progn
                             (setf math-exit t)
                             (incf depth)
                             (when (zerop depth) (signal 'syntax-done))))))
                    (:close
                     (incf depth)
                     (when (zerop depth) (signal 'syntax-done)))
                    (:open
                     (decf depth)
                     (when (zerop depth) (signal 'syntax-done))
                     (when (< depth min-depth)
                       (signal 'expression-ends-prematurely)))
                    (:end-comment
                     (when *parse-sexp-ignore-comments*
                       (multiple-value-bind (found char-pos aref-pos) 
                           (&back-comment from from-aref stop comment-nested comment-style buffer table)
                         (when (eq found :not-comment-end)
                           (setf from char-pos
                                 from-aref aref-pos)))))
                    ((:comment-fence :string-fence)
                     (loop
                        (when (= from stop) (lose last-good from))
                        (dec-both from from-aref buffer)
                        (when (and (not (&char-quoted from from-aref buffer table))
                                   (progn
                                     (setf ch (buffer-fetch-char from-aref buffer))
                                     (eq (&syntax ch table) code)))
                          (return nil)))
                     (when (and (eq code :string-fence)
                                (zerop depth)
                                sexpflag)
                       (signal 'syntax-done)))
                    (:string
                     (let ((string-term (buffer-fetch-char from-aref buffer)))
                       (loop
                          (when (= from stop) (lose last-good from))
                          (dec-both from from-aref buffer)
                          (when (and (not (&char-quoted from from-aref buffer table))
                                     (progn
                                       (setf ch (buffer-fetch-char from-aref buffer))
                                       (char= string-term ch))
                                     (eq (&syntax ch table) :string))
                            (return nil)))
                       (when (and (zerop depth)
                                  sexpflag)
                         (signal 'syntax-done))))
                    (t
                     ;; Ignore whitespace, punctuation, quote, endcomment.
                     ))))
              (when (not (zerop depth)) (lose last-good from))
              (return-from &scan-lists nil))
          (syntax-done ()
            (incf count))))
      from)))

(defun scan-lists (from count depth)
  "Scan from character number FROM by COUNT lists.
Returns the character number of the position thus found.

If DEPTH is nonzero, paren depth begins counting from that value,
only places where the depth in parentheses becomes zero
are candidates for stopping; COUNT such places are counted.
Thus, a positive value for DEPTH means go out levels.

Comments are ignored if `*parse-sexp-ignore-comments*' is non-nil.

If the beginning or end of (the accessible part of) the buffer is reached
and the depth is wrong, an error is signaled.
If the depth is right but the count is not used up, nil is returned."
  (check-type from number)
  (check-type count number)
  (check-type depth number)
  (&scan-lists from count depth nil))

(defun scan-sexps (from count)
  "Scan from character number FROM by COUNT balanced expressions.
If COUNT is negative, scan backwards.
Returns the character number of the position thus found.

Comments are ignored if `parse-sexp-ignore-comments' is non-nil.

If the beginning or end of (the accessible part of) the buffer is reached
in the middle of a parenthetical grouping, an error is signaled.
If the beginning or end is reached between groupings
but before count is used up, nil is returned."
  (check-type from number)
  (check-type count number)
  (&scan-lists from count 0 t))

(defun backward-prefix-chars (&aux (buffer (current-buffer)) (table (syntax-table)))
  "Move point backward over any number of chars with prefix syntax.
This includes chars with \"quote\" or \"prefix\" syntax (' or p)."
  (let* ((beg (begv buffer))
         (pos (point buffer))
         (pos-aref (buffer-char-to-aref buffer pos))
         (opoint (point buffer))
         (opoint-aref (buffer-char-to-aref buffer pos))
         c)
    (when (<= pos beg)
      ;; SET_PT_BOTH (opoint, opoint_byte);
      (return-from backward-prefix-chars nil))

    (dec-both pos pos-aref buffer)
    (while (and (not (&char-quoted pos pos-aref buffer table))
                (progn
                  (setf c (buffer-fetch-char pos-aref buffer))
                  (or (eq (&syntax c table) :quote)
                      (&syntax-prefix c table))))
      (setf opoint pos
            opoint-aref pos-aref)
      (when (> (1+ pos) beg)
        (dec-both pos pos-aref buffer)))
    (set-point-both buffer opoint opoint-aref)
    nil))
         
(defstruct parse-state
  depth min-depth
  this-level-start
  prev-level-start
  location
  level-starts
  quoted
  in-comment
  comment-style
  comment-string-start
  in-string
  start-value
  start-value-aref)

(defstruct syntax-level
  last prev)

;; this function cries out for continuations.  you almost have to look
;; at the C code to understand what's going on here, i bet. Hell, I
;; don't even understand it.
(defun scan-sexps-forward (from from-aref end target-depth stop-before old-state comment-stop buffer table)
  "Parse forward from FROM / FROM_BYTE to END,
assuming that FROM has state OLDSTATE (nil means FROM is start of function),
and return a description of the state of the parse at END.
If STOPBEFORE is nonzero, stop at the start of an atom.
If COMMENTSTOP is 1, stop at the start of a comment.
If COMMENTSTOP is -1, stop at the start or end of a comment,
after the beginning of a string, or after the end of a string."
  ;;(message "scan-sexps-forward ~@{~a ~}" from from-aref end target-depth stop-before old-state comment-stop buffer table)
  (let ((state (make-parse-state))
        (prev-from from)
        (prev-from-aref from-aref)
        prev-from-syntax
        (boundary-stop (null comment-stop))
        no-fence
        c1
        code
        comment-nested
        depth
        min-depth
        temp
        start-quoted
        levels)
    (labels ((inc-from ()
               (setf prev-from from
                     prev-from-aref from-aref
                     temp (buffer-fetch-char prev-from-aref buffer)
                     prev-from-syntax (&syntax-with-flags temp table))
               (inc-both from from-aref buffer))
             (cur-level ()
               "Return the current level struct"
               (car levels))
             (do-start-in-comment ()
               ;; The (from == BEGV) test was to enter the loop in the middle so
               ;; that we find a 2-char comment ender even if we start in the
               ;; middle of it.  We don't want to do that if we're just at the
               ;; beginning of the comment (think of (*) ... (*)).
               (multiple-value-bind (found out-char out-aref in-comment)
                   (&forward-comment from from-aref end
                                     (parse-state-in-comment state)
                                     (parse-state-comment-style state)
                                     (if (or (eq from (begv buffer))
                                             (< from (+ (parse-state-comment-string-start state) 3)))
                                         nil prev-from-syntax)
                                     buffer table)
                 (setf from out-char
                       from-aref out-aref
                       (parse-state-in-comment state) in-comment)
                 ;; Beware!  prev_from and friends are invalid now.
                 ;; Luckily, the `done' doesn't use them and the INC_FROM
                 ;; sets them to a sane value without looking at them.
                 (unless found (throw :end :done))
                 (inc-from)
                 (setf (parse-state-in-comment state) nil
                       (parse-state-comment-style state) nil) ; reset the comment style
                 (when boundary-stop (throw :end :done))))
             (do-sym-done ()
               ;;(message "do-sym-done ~s" (parse-state-level-starts state))
               (setf (syntax-level-prev (cur-level)) (syntax-level-last (cur-level))))
             (do-sym-started ()
               ;; (message "do-sym-started")
               (while (< from end)
                 (case (&syntax (buffer-fetch-char from-aref buffer) table)
                   ((:escape :character-quote)
                    (inc-from)
                    (when (= from end)
                      (throw :end :end-quoted)))
                   ((:word-constituent :symbol-constituent :quote))
                   (t
                    (do-sym-done)
                    (return nil)))
                 (inc-from)))
             (do-start-quoted ()
               (when (= from end) (throw :end :end-quoted))
               (inc-from)
               (do-sym-started))
             (do-in-string-loop ()
               (loop
                  (let (c)
                    (when (>= from end) (throw :end :done))
                    (setf c (buffer-fetch-char from-aref buffer)
                          temp (&syntax c table))
                    ;; Check TEMP here so that if the char has
                    ;; a syntax-table property which says it is NOT
                    ;; a string character, it does not end the string.
                    (when (and no-fence
                               (equal c (parse-state-in-string state))
                               (eq temp :string))
                      (return nil))
                    (case temp
                      (:string-fence
                       (unless no-fence (return nil)))
                      ((:character-quote :escape)
                       (inc-from)
                       (when (>= from end) (throw :end :end-quoted))))
                    (inc-from))))
             (do-string-end ()
               ;;(message "do-string-end ~s" (parse-state-level-starts state))
               (setf (parse-state-in-string state) nil
                     (syntax-level-prev (cur-level)) (syntax-level-last (cur-level)))
               (inc-from)
               (when boundary-stop (throw :end :done)))
             (do-start-in-string ()
               (setf no-fence (not (eq (parse-state-in-string state) :st-string-style)))
               (do-in-string-loop)
               (do-string-end))
             (do-start-quoted-in-string ()
               (when (>= from end) (throw :end :end-quoted))
               (inc-from)
               (do-in-string-loop)))

      (when (/= from (begv buffer))
        (dec-both prev-from prev-from-aref buffer))

      (if old-state
          (progn
            (setf state old-state
                  start-quoted (parse-state-quoted state)
                  depth (or (parse-state-depth state) 0)
                  start-quoted (parse-state-quoted state))
            (dolist (i (parse-state-level-starts state))
              (push (make-syntax-level :last i) levels))
            ;; make sure we have at least one in the list
            (unless levels
              (push (make-syntax-level) levels)))
          (setf depth 0
                state (make-parse-state)
                levels (list (make-syntax-level))))

      ;;(message "top ~s" (parse-state-level-starts state))

      (setf (parse-state-quoted state) nil
            min-depth depth)

      (setf temp (buffer-fetch-char prev-from-aref buffer)
            prev-from-syntax (&syntax-with-flags temp table))

      ;; "Enter" the loop at a place appropriate for initial state. In
      ;; the C code this is a bunch of goto's. Here we call the
      ;; appropriate function that sync's us so we're ready to enter
      ;; the loop.
      (cond ((parse-state-in-comment state)
             (do-start-quoted))
            ((parse-state-in-string state)
             (setf no-fence (not (eq (parse-state-in-string state) :st-string-style)))
             (if start-quoted
                 (do-start-quoted-in-string)
                 (do-start-in-string)))
            (start-quoted
             (do-start-quoted)))
      ;; (message "sane here")
      (case
          (catch :end
            (while (< from end)
              (catch :continue
                (inc-from)
                (setf code (&syntax-flags-syntax prev-from-syntax))
                ;; (message "here the code is ~s" code)
                (cond ((and (< from end)
                            (&syntax-flags-comment-start-first prev-from-syntax)
                            (progn
                              (setf c1 (buffer-fetch-char from-aref buffer))
                              (&syntax-comment-start-second c1 table)))
                       ;; (message "here 1")
                       ;; Record the comment style we have entered so that only
                       ;; the comment-end sequence of the same style actually
                       ;; terminates the comment section.
                       (setf (parse-state-comment-style state) (&syntax-comment-style c1 table)
                             comment-nested (&syntax-flags-comment-nested prev-from-syntax)
                             comment-nested (or comment-nested 
                                                (&syntax-comment-nested c1 table))
                             (parse-state-in-comment state) comment-nested
                             (parse-state-comment-string-start state) prev-from)
                       (inc-from)
                       (setf code :comment))
                      ((eq code :comment-fence)
                       ;; (message "here 2")
                       ;; Record the comment style we have entered so that only
                       ;; the comment-end sequence of the same style actually
                       ;; terminates the comment section.
                       (setf (parse-state-comment-style state) :st-comment-style
                             (parse-state-in-comment state) -1 ; XXX
                             (parse-state-comment-string-start state) prev-from
                             code :comment))
                      ((eq code :comment)
                       ;; (message "here 3")
                       (setf (parse-state-comment-style state) (&syntax-flags-comment-style prev-from-syntax)
                             (parse-state-in-comment state) (&syntax-flags-comment-nested prev-from-syntax)
                             (parse-state-comment-string-start state) prev-from)))

                (when (&syntax-flags-prefix prev-from-syntax)
                  (throw :continue nil))

                ;;(message "code: ~s" code)
                (case code
                  ((:escape :character-quote)
                   ;; this arg means stop at sexp start
                   (when stop-before (throw :end :stop))
                   ;;(message ":escae ~s" (parse-state-level-starts state))
                   (setf (syntax-level-last (cur-level)) prev-from)
                   (do-start-quoted))

                  ((:word-constituent :symbol-constituent)
                   (when stop-before (throw :end :stop))
                   ;;(message ":word-con ~s" (parse-state-level-starts state))
                   (setf (syntax-level-last (cur-level)) prev-from)
                   (do-sym-started))

                  ((:comment-fence :comment)
                   (when (or comment-stop
                             boundary-stop)
                     (throw :end :done))
                   (do-start-in-comment))

                  (:open
                   (when stop-before (throw :end :stop))
                   (incf depth)
                   ;;(message ":open ~s" (parse-state-level-starts state))
                   (setf (syntax-level-last (cur-level)) prev-from)
                   ;; (message ":open ~a" (parse-state-level-starts state))
                   (push (make-syntax-level) levels)
                   ;;                   (when (> (length level-list) 100) ; XXX hardcoded
                   ;;                     (error "nesting too deep for parser"))
                   (when (= target-depth depth) (throw :end :done)))

                  (:close
                   (decf depth)
                   (when (< depth min-depth)
                     (setf min-depth depth))
                   (unless (= (length levels) 1)
                     (message "OKAY HERE!")
                     (pop levels))
                   (setf (syntax-level-prev (cur-level)) (syntax-level-last (cur-level)))
                   (when (= target-depth depth)
                     (throw :end :done)))

                  ((:string :string-fence)
                   (setf (parse-state-comment-string-start state) (1- from))
                   (when stop-before
                     (throw :end :stop))
                   (setf (syntax-level-last (cur-level)) prev-from)
                   (setf (parse-state-in-string state) (if (eq code :string)
                                                           (buffer-fetch-char prev-from-aref buffer)
                                                           :st-string-style))
                   (when boundary-stop
                     (throw :end :done))
                   (do-start-in-string))

                  (:math
                   ;; FIXME: We should do something with it.
                   )
                  (t 
                   ;; Ignore whitespace, punctuation, quote, endcomment.
                   ))))
            :done)
        (:stop
         ;; Here if stopping before start of sexp.
         ;; We have just fetched the char that starts it
         ;; but return the position before it.
         (setf from prev-from))
        (:end-quoted
         (setf (parse-state-quoted state) t)))

      ;;(message ":end ~s" (parse-state-level-starts state))
      ;; done
      (setf (parse-state-depth state) depth
            (parse-state-min-depth state) min-depth
            (parse-state-this-level-start state) (syntax-level-prev (cur-level))
            (parse-state-prev-level-start state) (if (<= (length levels) 1)
                                                     nil (syntax-level-last (second levels)))
            (parse-state-location state) from
            (parse-state-level-starts state) (mapcar 'syntax-level-last (cdr levels)))
      state)))

(defun parse-partial-sexp (from to &optional (target-depth -100000) stop-before old-state comment-stop &aux (buffer (current-buffer)) (table (syntax-table)))
  "Parse Lisp syntax starting at FROM until TO; return status of parse at TO.
Parsing stops at TO or when certain criteria are met;
 point is set to where parsing stops.
If fifth arg OLDSTATE is omitted or nil,
 parsing assumes that FROM is the beginning of a function.
Value is a list of elements describing final state of parsing:
 0. depth in parens.
 1. character address of start of innermost containing list; nil if none.
 2. character address of start of last complete sexp terminated.
 3. non-nil if inside a string.
    (it is the character that will terminate the string,
     or t if the string should be terminated by a generic string delimiter.)
 4. nil if outside a comment, t if inside a non-nestable comment,
    else an integer (the current comment nesting).
 5. t if following a quote character.
 6. the minimum paren-depth encountered during this scan.
 7. t if in a comment of style b; symbol `syntax-table' if the comment
    should be terminated by a generic comment delimiter.
 8. character address of start of comment or string; nil if not in one.
 9. Intermediate data for continuation of parsing (subject to change).
If third arg TARGETDEPTH is non-nil, parsing stops if the depth
in parentheses becomes equal to TARGETDEPTH.
Fourth arg STOPBEFORE non-nil means stop when come to
 any character that starts a sexp.
Fifth arg OLDSTATE is a list like what this function returns.
 It is used to initialize the state of the parse.  Elements number 1, 2, 6
 and 8 are ignored.
Sixth arg COMMENTSTOP non-nil means stop at the start of a comment.
 If it is symbol `syntax-table', stop after the start of a comment or a
 string, or after end of a comment or a string."
  (check-type target-depth number)
  (multiple-value-setq (from to) (validate-region from to buffer))
  (let ((state (scan-sexps-forward from (buffer-char-to-aref buffer from) to
                                   target-depth (not (null stop-before)) old-state
                                   ;; XXX
                                   (if comment-stop
                                       (if (eq comment-stop 'syntax-table) -1 1)
                                       0)
                                   buffer table)))
    (goto-char (parse-state-location state) buffer)
    state))
