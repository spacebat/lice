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
          (define-key m (make-instance 'key :char #\q :control t :meta t) 'indent-sexp)
          (define-key m (make-instance 'key :char #\x :control t :meta t) 'eval-defun)
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

(defcommand end-of-defun ((&optional arg)
                          :prefix)
  "Move forward to next end of defun.
With argument, do it that many times.
Negative argument -N means move back to Nth preceding end of defun.

An end of a defun occurs right after the close-parenthesis that
matches the open-parenthesis that starts a defun; see function
`beginning-of-defun'.

If variable `*end-of-defun-function*' is non-nil, its value
is called as a function to find the defun's end."
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
                   (skip-whitespace-forward)
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
		    (skip-whitespace-forward)
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
      (error (c) (message "Eval error: ~a" c)))))

(defcommand eval-print-last-sexp ()
  (let ((start (point))
	end)
    ;; some nice'n'gross point handling
    (backward-sexp)
    (setf end (point))
    (goto-char start)
    (handler-case (eval-print (buffer-substring-no-properties start end))
      (error (c) (message "Eval error: ~a" c)))))

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
    (skip-whitespace-forward)
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
        (setq state (parse-partial-sexp (point) indent-point :target-depth 0)))
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
					    indent-point :target-depth 0)))
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
                  (parse-partial-sexp (point) *calculate-lisp-indent-last-sexp* :target-depth 0 :stop-before t)
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
                                                        :target-depth 0 :stop-before t)))
                         (backward-prefix-chars))
                        (t
                         ;; Indent beneath first sexp on same line as
                         ;; `*calculate-lisp-indent-last-sexp*'.  Again, it's
                         ;; almost certainly a function call.
                         (goto-char *calculate-lisp-indent-last-sexp*)
                         (beginning-of-line)
                         (parse-partial-sexp (point) *calculate-lisp-indent-last-sexp*
                                             :target-depth 0 :stop-before t)
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
        (let ((mdata (if *defun-prompt-regexp*
                         (re-search-backward (concat (if *open-paren-in-column-0-is-defun-start*
                                                         "^\\(|" "")
                                                     "(?:" *defun-prompt-regexp* ")\\(")
                                             :error 'move :count (or arg 1))
                         (search-backward (format nil "~%(")
                                          :error 'move :count (or arg 1))))) ;; used to be ^\\(
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
					  :old-state state)))
      (while (< (point) end)
	(or (car (nthcdr 3 state))
	    (and nochange-regexp
		 (looking-at nochange-regexp))
	    ;; If line does not start in string, indent it
	    (let ((indent (current-indentation)))
	      (delete-region (point) (progn (skip-whitespace-forward) (point)))
	      (or (eolp)
		  (indent-to (max 0 (+ indent arg)) 0))))
	(setq state (parse-partial-sexp (point)
					(progn
					  (forward-line 1) (point))
					:old-state state))))))


(defcommand indent-sexp ((&optional endpos))
  "Indent each line of the list starting just after point.
If optional arg ENDPOS is given, indent each line, stopping when
ENDPOS is encountered."
  (let ((indent-stack (list nil))
	(next-depth 0)
	;; If ENDPOS is non-nil, use nil as STARTING-POINT
	;; so that calculate-lisp-indent will find the beginning of
	;; the defun we are in.
	;; If ENDPOS is nil, it is safe not to scan before point
	;; since every line we indent is more deeply nested than point is.
	(starting-point (if endpos nil (point)))
	(last-point (point))
	last-depth bol outer-loop-done inner-loop-done state this-indent)
    (or endpos
	;; Get error now if we don't have a complete sexp after point.
	(save-excursion (forward-sexp 1)))
    (save-excursion
      (setq outer-loop-done nil)
      (while (if endpos (< (point) (ensure-number endpos))
	       (not outer-loop-done))
	(setq last-depth next-depth
	      inner-loop-done nil)
	;; Parse this line so we can learn the state
	;; to indent the next line.
	;; This inner loop goes through only once
	;; unless a line ends inside a string.
	(while (and (not inner-loop-done)
		    (not (setq outer-loop-done (eobp))))
	  (setq state (parse-partial-sexp (point) (progn (end-of-line) (point))
					  :old-state state))
	  (setq next-depth (parse-state-depth state))
	  ;; If the line contains a comment other than the sort
	  ;; that is indented like code,
	  ;; indent it now with indent-for-comment.
	  ;; Comments indented like code are right already.
	  ;; In any case clear the in-comment flag in the state
	  ;; because parse-partial-sexp never sees the newlines.
	  (if (parse-state-in-comment state) ;;(car (nthcdr 4 state))
	      (progn (indent-for-comment)
		     (end-of-line)
		     (setf (parse-state-in-comment state) nil))) ;;(setcar (nthcdr 4 state) nil)))
	  ;; If this line ends inside a string,
	  ;; go straight to next line, remaining within the inner loop,
	  ;; and turn off the \-flag.
	  (if (parse-state-in-string state) ;;(car (nthcdr 3 state))
	      (progn
		(forward-line 1)
		(setf (parse-state-in-string state) nil));;(setf (car (nthcdr 5 state)) nil))
	    (setq inner-loop-done t)))
	(and endpos
	     (<= next-depth 0)
	     (progn
	       (setq indent-stack (nconc indent-stack
					 (make-list (- next-depth) :initial-element nil))
		     last-depth (- last-depth next-depth)
		     next-depth 0)))
	(or outer-loop-done endpos
	    (setq outer-loop-done (<= next-depth 0)))
	(if outer-loop-done
	    (forward-line 1)
            (progn
              (while (> last-depth next-depth)
                (setq indent-stack (cdr indent-stack)
                      last-depth (1- last-depth)))
              (while (< last-depth next-depth)
                (setq indent-stack (cons nil indent-stack)
                      last-depth (1+ last-depth)))
              ;; Now go to the next line and indent it according
              ;; to what we learned from parsing the previous one.
              (forward-line 1)
              (setq bol (point))
              (skip-whitespace-forward)
              ;; But not if the line is blank, or just a comment
              ;; (except for double-semi comments; indent them as usual).
              (if (or (eobp) (looking-at "\\w|\\n")) ;; FIXME: used to be "\\s<|\\n"
                  nil
                  (progn
                    (if (and (car indent-stack)
                             (>= (car indent-stack) 0))
                        (setq this-indent (car indent-stack))
                        (let ((val (calculate-lisp-indent
                                    (if (car indent-stack) (- (car indent-stack))
                                        starting-point))))
                          (if (null val)
                              (setq this-indent val)
                              (if (integerp val)
                                  (setf (car indent-stack)
                                        (setq this-indent val))
                                  (progn
                                    (setf (car indent-stack) (- (car (cdr val))))
                                    (setq this-indent (car val)))))))
                    (if (and this-indent (/= (current-column) this-indent))
                        (progn (delete-region bol (point))
                               (indent-to this-indent)))))))
	(or outer-loop-done
	    (setq outer-loop-done (= (point) last-point))
	    (setq last-point (point)))))))

(defun lisp-indent-region (start end)
  "Indent every line whose first char is between START and END inclusive."
  (save-excursion
    (let ((endmark (copy-marker end)))
      (goto-char start)
      (and (bolp) (not (eolp))
	   (lisp-indent-line))
      (indent-sexp endmark)
      (set-marker endmark nil))))

(defun eval-defun-1 (form)
  "Treat some expressions specially.
Reset the `defvar' and `defcustom' variables to the initial value.
Reinitialize the face according to the `defface' specification."
  ;; The code in edebug-defun should be consistent with this, but not
  ;; the same, since this gets a macroexpended form.
  (cond ((not (listp form))
	 form)
	((and (eq (car form) 'defvar)
	      (cdr-safe (cdr-safe form))
	      (boundp (cadr form)))
	 ;; Force variable to be re-set.
	 `(progn (defvar ,(nth 1 form) nil ,@(nthcdr 3 form))
		 (setf ,(nth 1 form) ,(nth 2 form)))) ;; used to be setq-default
	;; `defcustom' is now macroexpanded to
	;; `custom-declare-variable' with a quoted value arg.
	((and (eq (car form) 'custom-declare-variable)
	      (boundp (eval (nth 1 form)))) ;; used to be default-boundp
	 ;; Force variable to be bound.
         ;; XXX: we can't handle defcustom
	 ;;(set-default (eval (nth 1 form)) (eval (nth 1 (nth 2 form))))
	 form)
	;; `defface' is macroexpanded to `custom-declare-face'.
	((eq (car form) 'custom-declare-face)
	 ;; Reset the face.
         ;; XXX: what do we do with this?
;; 	  (setq face-new-frame-defaults
;;  	       (assq-delete-all (eval (nth 1 form)) face-new-frame-defaults))
;; 	 (put (eval (nth 1 form)) 'face-defface-spec nil)
;; 	 ;; Setting `customized-face' to the new spec after calling
;; 	 ;; the form, but preserving the old saved spec in `saved-face',
;; 	 ;; imitates the situation when the new face spec is set
;; 	 ;; temporarily for the current session in the customize
;; 	 ;; buffer, thus allowing `face-user-default-spec' to use the
;; 	 ;; new customized spec instead of the saved spec.
;; 	 ;; Resetting `saved-face' temporarily to nil is needed to let
;; 	 ;; `defface' change the spec, regardless of a saved spec.
;; 	 (prog1 `(prog1 ,form
;; 		   (put ,(nth 1 form) 'saved-face
;; 			',(get (eval (nth 1 form)) 'saved-face))
;; 		   (put ,(nth 1 form) 'customized-face
;; 			,(nth 2 form)))
;; 	   (put (eval (nth 1 form)) 'saved-face nil))
         )
	((eq (car form) 'progn)
	 (cons 'progn (mapcar 'eval-defun-1 (cdr form))))
	(t form)))

(defcommand eval-defun-2 ()
  "Evaluate defun that point is in or before.
The value is displayed in the minibuffer.
If the current defun is actually a call to `defvar',
then reset the variable using the initial value expression
even if the variable already has some other value.
\(Normally `defvar' does not change the variable's value
if it already has a value.\)

With argument, insert value in current buffer after the defun.
Return the result of evaluation."
  (let* ((*debug-on-error* *eval-expression-debug-on-error*)
         (*print-length* *eval-expression-print-length*)
         (*print-level* *eval-expression-print-level*)
         ;; FIXME: accum the eval/compiler output and i guess do
         ;; something with it, cept in this case we don't.
         (*debug-io* (make-string-output-stream))
         (*standard-output* *debug-io*)
         (*error-output* *debug-io*))
    (save-excursion
      ;; FIXME: In gnu emacs eval-region handles recording which file defines
      ;; a function or variable. How do we do that in CL?

      (let ( ;;XXX (standard-output t)
            beg end form)
        ;; Read the form from the buffer, and record where it ends.
        (save-excursion
          (end-of-defun)
          (beginning-of-defun)
          (setq beg (point))
          (setq form (read-from-buffer))
          (setq end (point)))
        ;; Alter the form if necessary.  FIXME: we don't macroexpand
        ;; but really we want to macroexpand down to defvar (and
        ;; friends) which could be several layers of expansion
        ;; down. We don't want to go all the way since defvar is
        ;; itself a macro.
        (setq form (eval-defun-1 form ;; (macroexpand form)
                                 ))
        (eval form)))))

(defcommand eval-defun ((edebug-it)
                        :prefix)
  "Evaluate the top-level form containing point, or after point.

If the current defun is actually a call to `defvar' or `defcustom',
evaluating it this way resets the variable using its initial value
expression even if the variable already has some other value.
\(Normally `defvar' and `defcustom' do not alter the value if there
already is one.)

If `eval-expression-debug-on-error' is non-nil, which is the default,
this command arranges for all errors to enter the debugger.

With a prefix argument, instrument the code for Edebug.

If acting on a `defun' for FUNCTION, and the function was
instrumented, `Edebug: FUNCTION' is printed in the minibuffer.  If not
instrumented, just FUNCTION is printed.

If not acting on a `defun', the result of evaluation is displayed in
the minibuffer.  This display is controlled by the variables
`eval-expression-print-length' and `eval-expression-print-level',
which see."
  ;; FIXME: edebug?
  (declare (ignore edebug-it))
  (cond ;; (edebug-it
;; 	 (require 'edebug)
;; 	 (eval-defun (not edebug-all-defs)))
	(t
	 (if (null *eval-expression-debug-on-error*)
	     (eval-defun-2)
	   (let ((old-value (gensym "t")) new-value value)
	     (let ((*debug-on-error* old-value))
	       (setq value (eval-defun-2))
	       (setq new-value *debug-on-error*))
	     (unless (eq old-value new-value)
	       (setq *debug-on-error* new-value))
	     value)))))


(provide :lice-0.1/lisp-mode)