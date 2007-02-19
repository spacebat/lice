;;; This is a cheap, pigeon lisp mode. One Day, it'll be replaced with
;;; something amazing.

(in-package "LICE")

(defcustom *defun-prompt-regexp* nil
  "*If non-nil, a regexp to ignore before a defun.
This is only necessary if the opening paren or brace is not in column 0.
See function `beginning-of-defun'."
  :type '(choice (const nil)
		 regexp)
  :group 'lisp)

(defvar *emacs-lisp-mode-syntax-table*
  (let ((table (make-syntax-table)))
    (loop for i from (char-code #\0) to (char-code #\9) do
         (modify-syntax-entry (code-char i) :symbol-constituent :table table))
    (loop for i from (char-code #\A) to (char-code #\Z) do
         (modify-syntax-entry (code-char i) :symbol-constituent :table table))

    (loop for i from (char-code #\a) to (char-code #\z) do
         (modify-syntax-entry (code-char i) :symbol-constituent :table table))

    (loop for c in '(#\{ #\| #\} #\~ #| 127 |#) do
         (modify-syntax-entry c :symbol-constituent :table table))

    (modify-syntax-entry #\Space :whitespace :table table)
    (modify-syntax-entry #\Tab :whitespace :table table)
    ;; XXX what is \f ? (modify-syntax-entry ?\f :whitespace :table table)
    (modify-syntax-entry #\Newline :end-comment :table table)
    ;; This is probably obsolete since nowadays such features use overlays.
    ;; ;; Give CR the same syntax as newline, for selective-display.
    ;; (modify-syntax-entry ?\^m ">   " :table table)
    (modify-syntax-entry #\; :comment :table table)
    (modify-syntax-entry #\` :quote :table table)
    (modify-syntax-entry #\' :quote :table table)
    (modify-syntax-entry #\, :quote :table table)
    (modify-syntax-entry #\@ :quote :table table)
    ;; Used to be singlequote; changed for flonums.
    (modify-syntax-entry #\. :symbol-constituent :table table)
    (modify-syntax-entry #\# :quote :table table)
    (modify-syntax-entry #\" :string :table table)
    (modify-syntax-entry #\\ :escape :table table)
    (modify-syntax-entry #\( :open  :extra #\) :table table)
    (modify-syntax-entry #\) :close :extra #\( :table table)
    (modify-syntax-entry #\[ :open  :extra #\] :table table)
    (modify-syntax-entry #\] :close :extra #\[ :table table)
    table))

(defvar *lisp-mode-syntax-table*
  (let ((table (copy-syntax-table *emacs-lisp-mode-syntax-table*)))
    (modify-syntax-entry #\[ :symbol-constituent :table table)
    (modify-syntax-entry #\] :symbol-constituent :table table)
    (modify-syntax-entry #\# :quote :flags '(:comment-start-first :comment-end-second :comment-style) :table table)
    (modify-syntax-entry #\| :string :flags '(:comment-start-second :comment-end-first :comment-style :comment-nested) :table table)
    table))

(define-major-mode lisp-interaction-mode
  (:name "Lisp Interaction"
   :map (let ((m (make-sparse-keymap)))
	  (define-key m (make-instance 'key :char #\j :control t) 'eval-print-last-sexp)
	  (define-key m (make-instance 'key :char #\Tab) 'lisp-indent-line)
	  (define-key m (make-instance 'key :char #\i :control t) 'lisp-indent-line)
	  m))
  "Lisp mode"
  (set-syntax-table *lisp-mode-syntax-table*))

(defun buffer-end (arg)
  "Return the \"far end\" position of the buffer, in direction ARG.
If ARG is positive, that's the end of the buffer.
Otherwise, that's the beginning of the buffer."
  (if (> arg 0) (point-max) (point-min)))

(defvar *end-of-defun-function* nil
  "If non-nil, function for function `end-of-defun' to call.
This is used to find the end of the defun instead of using the normal
recipe (see `end-of-defun').  Major modes can define this if the
normal method is not appropriate.")

(defun end-of-defun (&optional arg)
  "Move forward to next end of defun.
With argument, do it that many times.
Negative argument -N means move back to Nth preceding end of defun.

An end of a defun occurs right after the close-parenthesis that
matches the open-parenthesis that starts a defun; see function
`beginning-of-defun'.

If variable `*end-of-defun-function*' is non-nil, its value
is called as a function to find the defun's end."
  (interactive "p")
  (or (not (eq *this-command* 'end-of-defun))
      (eq *last-command* 'end-of-defun)
      ;;XXX (and transient-mark-mode mark-active)
      (push-mark))
  (if (or (null arg) (= arg 0)) (setq arg 1))
  (if *end-of-defun-function*
      (if (> arg 0)
	  (dotimes (i arg)
	    (funcall *end-of-defun-function*))
	;; Better not call beginning-of-defun-function
	;; directly, in case it's not defined.
	(beginning-of-defun (- arg)))
    (let ((first t))
      (while (and (> arg 0) (< (point) (point-max)))
	(let ((pos (point)))
	  (while (progn
                   (if (and first
                            (progn
                              (end-of-line 1)
                              (beginning-of-defun-raw 1)))
                       nil
                       (progn
                         (or (bobp) (forward-char -1))
                         (beginning-of-defun-raw -1)))
                   (setq first nil)
                   (forward-list 1)
                   (skip-chars-forward (coerce '(#\Space #\Tab) 'string))
                   (if (looking-at ";|\\n") ; XXX: used to be comment starter \\s<
                       (forward-line 1))
                   (<= (point) pos))))
	(setq arg (1- arg)))
      (while (< arg 0)
	(let ((pos (point)))
	  (beginning-of-defun-raw 1)
	  (forward-sexp 1)
	  (forward-line 1)
	  (if (>= (point) pos)
	      (if (beginning-of-defun-raw 2)
		  (progn
		    (forward-list 1)
		    (skip-chars-forward (coerce '(#\Space #\Tab) 'string))
		    (if (looking-at ";|\\n") ; XXX: used to be comment starter \\s<
			(forward-line 1)))
		(goto-char (point-min)))))
	(setq arg (1+ arg))))))

(defvar *forward-sexp-function* nil
  "If non-nil, `forward-sexp' delegates to this function.
Should take the same arguments and behave similarly to `forward-sexp'.")

(defcommand forward-sexp ((&optional (arg 1))
                          :prefix)
  "Move forward across one balanced expression (sexp).
With ARG, do it that many times.  Negative arg -N means
move backward across N balanced expressions."
  (if *forward-sexp-function*
      (funcall *forward-sexp-function* arg)
      (progn
        (goto-char (or (scan-sexps (point) arg) (buffer-end arg)))
        (if (< arg 0) (backward-prefix-chars)))))

(defcommand backward-sexp ((&optional (arg 1))
                           :prefix)
  "Move backward across one balanced expression (sexp).
With ARG, do it that many times.  Negative arg -N means
move forward across N balanced expressions."
  (forward-sexp (- arg)))

(defcommand forward-list ((&optional arg)
                          :prefix)
  "Move forward across one balanced group of parentheses.
With ARG, do it that many times.
Negative arg -N means move backward across N groups of parentheses."
  (or arg (setq arg 1))
  (goto-char (or (scan-lists (point) arg 0) (buffer-end arg))))

(defcommand backward-list ((&optional arg)
                           :prefix)
  "Move backward across one balanced group of parentheses.
With ARG, do it that many times.
Negative arg -N means move forward across N groups of parentheses."
  (or arg (setq arg 1))
  (forward-list (- arg)))

(defcommand eval-last-sexp ()
  (let ((start (point))
	end)
    ;; some nice'n'gross point handling
    (backward-sexp)
    (setf end (point))
    (goto-char start)
    (handler-case (eval-echo (buffer-substring-no-properties start end))
      (error (c) (message "Eval error: ~s" c)))))

(defcommand eval-print-last-sexp ()
  (let ((start (point))
	end)
    ;; some nice'n'gross point handling
    (backward-sexp)
    (setf end (point))
    (goto-char start)
    (handler-case (eval-print (buffer-substring-no-properties start end))
      (error (c) (message "Eval error: ~s" c)))))

(defcommand lisp-interaction-mode ()
  (set-major-mode lisp-interaction-mode))

(defvar *lisp-indent-offset* nil
  "If non-nil, indent second line of expressions that many more columns.")

(defvar *lisp-indent-function* 'common-lisp-indent-function)

(defcommand lisp-indent-line ((&optional whole-exp)
                              :raw-prefix)
  "Indent current line as Lisp code.
With argument, indent any additional lines of the same expression
rigidly along with this one."
  (let ((indent (calculate-lisp-indent)) shift-amt end
	(pos (- (point-max) (point)))
	(beg (progn (beginning-of-line) (point))))
    (skip-chars-forward (coerce '(#\Space #\Tab) 'string))
    (if (or (null indent) (looking-at ";;;")) ; XXX: used to be comment starter \\s<
	;; Don't alter indentation of a ;;; comment line
	;; or a line that starts in a string.
	(goto-char (- (point-max) pos))
        (progn
          (if (and (looking-at ";") (not (looking-at ";;"))) ; XXX: used to be comment starter \\s<
              ;; Single-semicolon comment lines should be indented
              ;; as comment lines, not as code.
              (progn (indent-for-comment) (forward-char -1))
              (progn
                (if (listp indent) (setq indent (car indent)))
                (setq shift-amt (- indent (current-column)))
                (if (zerop shift-amt)
                    nil
                    (progn
                      (delete-region beg (point))
                      (indent-to indent)))))
          ;; If initial point was within line's indentation,
          ;; position after the indentation.  Else stay at same point in text.
          (if (> (- (point-max) pos) (point))
              (goto-char (- (point-max) pos)))
          ;; If desired, shift remaining lines of expression the same amount.
          (and whole-exp (not (zerop shift-amt))
               (save-excursion
                 (goto-char beg)
             (message "this111")
                 (forward-sexp 1)
                 (message "done 11")
                 (setq end (point))
                 (goto-char beg)
                 (forward-line 1)
                 (setq beg (point))
                 (> end beg))
               (indent-code-rigidly beg end shift-amt))))))

(defvar *calculate-lisp-indent-last-sexp*)

(defun calculate-lisp-indent (&optional parse-start)
  "Return appropriate indentation for current line as Lisp code.
In usual case returns an integer: the column to indent to.
If the value is nil, that means don't change the indentation
because the line starts inside a string.

The value can also be a list of the form (COLUMN CONTAINING-SEXP-START).
This means that following lines at the same level of indentation
should not necessarily be indented the same as this line.
Then COLUMN is the column to indent to, and CONTAINING-SEXP-START
is the buffer position of the start of the containing expression."
  (save-excursion
    (beginning-of-line)
    (let ((indent-point (point))
          state paren-depth
          ;; setting this to a number inhibits calling hook
          (desired-indent nil)
          (retry t)
          *calculate-lisp-indent-last-sexp* containing-sexp)
      (if parse-start
          (goto-char parse-start)
          (beginning-of-defun))
      ;; Find outermost containing sexp
      (while (< (point) indent-point)
        (message "flitz ~d" indent-point (point))
        (setq state (parse-partial-sexp (point) indent-point 0)))
      ;; Find innermost containing sexp
      (while (and retry
		  state
                  (> (setq paren-depth (parse-state-depth state)) 0))
        (setq retry nil)
        (setq *calculate-lisp-indent-last-sexp* (parse-state-this-level-start state))
        (message "gaah ~d" *calculate-lisp-indent-last-sexp*)
        (setq containing-sexp (parse-state-prev-level-start state))
        ;; Position following last unclosed open.
        (goto-char (1+ containing-sexp))
        ;; Is there a complete sexp since then?
        (if (and *calculate-lisp-indent-last-sexp*
		 (> *calculate-lisp-indent-last-sexp* (point)))
            ;; Yes, but is there a containing sexp after that?
            (let ((peek (parse-partial-sexp *calculate-lisp-indent-last-sexp*
					    indent-point 0)))
              (if (setq retry (parse-state-prev-level-start peek)) (setq state peek)))))
      (message "retry ~a" retry)
      (if retry
          nil
          ;; Innermost containing sexp found
          (progn
            (goto-char (1+ containing-sexp))
            (if (not *calculate-lisp-indent-last-sexp*)
                ;; indent-point immediately follows open paren.
                ;; Don't call hook.
                (setq desired-indent (current-column))
                (progn
                  ;; Find the start of first element of containing sexp.
                  (parse-partial-sexp (point) *calculate-lisp-indent-last-sexp* 0 t)
                  (cond ((looking-at "\\(") ; XXX used to be open \\s(
                         ;; First element of containing sexp is a list.
                         ;; Indent under that list.
                         )
                        ((> (save-excursion (forward-line 1) (point))
                            *calculate-lisp-indent-last-sexp*)

                         ;; This is the first line to start within the containing sexp.
                         ;; It's almost certainly a function call.
                         (if (= (point) *calculate-lisp-indent-last-sexp*)
                             ;; Containing sexp has nothing before this line
                             ;; except the first element.  Indent under that element.
                             nil
                             ;; Skip the first element, find start of second (the first
                             ;; argument of the function call) and indent under.
                             (progn (forward-sexp 1)
                                    (parse-partial-sexp (point)
                                                        *calculate-lisp-indent-last-sexp*
                                                        0 t)))
                         (backward-prefix-chars))
                        (t
                         ;; Indent beneath first sexp on same line as
                         ;; `*calculate-lisp-indent-last-sexp*'.  Again, it's
                         ;; almost certainly a function call.
                         (goto-char *calculate-lisp-indent-last-sexp*)
                         (beginning-of-line)
                         (parse-partial-sexp (point) *calculate-lisp-indent-last-sexp*
                                             0 t)
                         (backward-prefix-chars)))))))
      ;; Point is at the point to indent under unless we are inside a string.
      ;; Call indentation hook except when overridden by *lisp-indent-offset*
      ;; or if the desired indentation has already been computed.
      (let ((normal-indent (current-column)))
        (cond ((parse-state-in-string state)
               ;; Inside a string, don't change indentation.
	       nil)
              ((and (integerp *lisp-indent-offset*) containing-sexp)
               ;; Indent by constant offset
               (goto-char containing-sexp)
               (+ (current-column) *lisp-indent-offset*))
              (desired-indent)
              ((and (boundp '*lisp-indent-function*)
                    *lisp-indent-function*
                    (not retry))
               (or (funcall *lisp-indent-function* indent-point state)
                   normal-indent))
              (t
               normal-indent))))))

(defvar *beginning-of-defun-function* nil
  "If non-nil, function for `beginning-of-defun-raw' to call.
This is used to find the beginning of the defun instead of using the
normal recipe (see `beginning-of-defun').  Major modes can define this
if defining `*defun-prompt-regexp*' is not sufficient to handle the mode's
needs.

The function (of no args) should go to the line on which the current
defun starts, and return non-nil, or should return nil if it can't
find the beginning.")

(defcommand beginning-of-defun-raw ((&optional (arg 1))
                                    :prefix)
  "Move point to the character that starts a defun.
This is identical to function `beginning-of-defun', except that point
does not move to the beginning of the line when `*defun-prompt-regexp*'
is non-nil.

If variable `*beginning-of-defun-function*' is non-nil, its value
is called as a function to find the defun's beginning."
  (if *beginning-of-defun-function*
      (if (> arg 0)
	  (dotimes (i arg)
	    (funcall *beginning-of-defun-function*))
          ;; Better not call *end-of-defun-function* directly, in case
          ;; it's not defined.
          (end-of-defun (- arg)))
      (progn
        (when (and (< arg 0) 
                   (not (eobp)))
          (forward-char 1))
        (let ((mdata (re-search-backward (if *defun-prompt-regexp*
                                             (concat (if *open-paren-in-column-0-is-defun-start*
                                                         "^\\(|" "")
                                                     "(?:" *defun-prompt-regexp* ")\\(")
                                             "\\n\\(") ;; used to be ^\\(
                                         :error 'move :count (or arg 1))))
          (when mdata
            (goto-char (1- (match-end mdata 0)))) t))))

(defcommand beginning-of-defun ((&optional (arg 1))
                                :prefix)
  "Move backward to the beginning of a defun.
With ARG, do it that many times.  Negative arg -N
means move forward to Nth following beginning of defun.
Returns t unless search stops due to beginning or end of buffer.

Normally a defun starts when there is a char with open-parenthesis
syntax at the beginning of a line.  If `*defun-prompt-regexp*' is
non-nil, then a string which matches that regexp may precede the
open-parenthesis, and point ends up at the beginning of the line.

If variable `*beginning-of-defun-function*' is non-nil, its value
is called as a function to find the defun's beginning."
  (or (not (eq *this-command* 'beginning-of-defun))
      (eq *last-command* 'beginning-of-defun)
      ;;XXX (and transient-mark-mode mark-active)
      (push-mark))
  (and (beginning-of-defun-raw arg)
       (progn (beginning-of-line) t)))

(defcommand indent-code-rigidly ((start end arg &optional nochange-regexp)
                                 :region-beginning :region-end :prefix)
  "Indent all lines of code, starting in the region, sideways by ARG columns.
Does not affect lines starting inside comments or strings, assuming that
the start of the region is not inside them.

Called from a program, takes args START, END, COLUMNS and NOCHANGE-REGEXP.
The last is a regexp which, if matched at the beginning of a line,
means don't indent that line."
  (let (state)
    (save-excursion
      (goto-char end)
      (setq end (point-marker))
      (goto-char start)
      (or (bolp)
	  (setq state (parse-partial-sexp (point)
					  (progn
					    (forward-line 1) (point))
					  nil nil state)))
      (while (< (point) end)
	(or (car (nthcdr 3 state))
	    (and nochange-regexp
		 (looking-at nochange-regexp))
	    ;; If line does not start in string, indent it
	    (let ((indent (current-indentation)))
	      (delete-region (point) (progn (skip-chars-forward (coerce '(#\Space #\Tab) 'string)) (point)))
	      (or (eolp)
		  (indent-to (max 0 (+ indent arg)) 0))))
	(setq state (parse-partial-sexp (point)
					(progn
					  (forward-line 1) (point))
					nil nil state))))))


(provide :lice-0.1/lisp-mode)