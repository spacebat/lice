;;; Handle input and key command dispatching

(in-package :lice)

(defvar *debug-on-error* t
  "Non-nil means enter the debugger if an unhandled error is signaled.")

(defvar *debug-on-quit* nil
  "Non-nil means enter the debugger if quit is signaled (C-g, for example).")

(define-condition quit (lice-condition)
  () (:documentation "A condition raised when the user aborted the
operation (by pressing C-g, for instance)."))

(defclass command ()
  ((name :type symbol :initarg :name :accessor command-name)
   (args :type list :initarg :args :accessor command-args)
   (fn :type function :initarg :fn :accessor command-fn)
   (doc :type (or null string) :initarg :doc :accessor command-doc))
  (:documentation "An interactive command."))

(defvar *commands* (make-hash-table)
  "A hash table of interactive commands")

(defmacro defcommand (name (&optional args &rest interactive-args) &body body)
  "Create an interactive command named NAME."
  (let ((tmp (gensym)))
    `(progn
       (defun ,name ,args
	 ,@body)
       (setf (gethash ',name *commands*)
	     (make-instance
	      'command
	      :name ',name
	      :args ',interactive-args
	      :doc ,(when (typep (first body) 'string) (first body))
	      :fn (lambda ()
		    (let ((,tmp (list ,@(mapcar (lambda (a)
						  (if (listp a)
						      `(funcall (gethash ,(first a) *command-arg-type-hash*) ,@(cdr a))
						    `(funcall (gethash ,a *command-arg-type-hash*))))
						interactive-args))))
		      ;; XXX: Is this a sick hack?  We need to reset the
		      ;; prefix-arg at the right time. After the command
		      ;; is executed we can't because the universal
		      ;; argument code sets the prefix-arg for the next
		      ;; command. The Right spot seems to be to reset it
		      ;; once a command is about to be executed, and
		      ;; after the prefix arg has been gathered to be
		      ;; used in the command. Which is right here.
		      (setf *prefix-arg* nil)
		      ;; Note that we use the actual function. If the
		      ;; function is redefined, the command will
		      ;; continue to be defined and will call the
		      ;; function declared above, not the redefined one.
		      (apply #',name ,tmp))))))))
			     

(defvar *last-command* nil
  "The last command executed.")

(defvar *this-command* nil
  "The command that was executed. This is to the command being
executed before it is executed. *last-command* will be set to this
when the command finishes. The command can change this value if it
wants to change what *last-command* will be set to. Used in the `yank'
and `yank-pop' commands.")

(defvar *prefix-arg* nil
  "The value of the prefix argument for the next editing command.
It may be a number, or the symbol `-' for just a minus sign as arg,
or a list whose car is a number for just one or more C-u's
or nil if no argument has been specified.

You cannot examine this variable to find the argument for this command
since it has been set to nil by the time you can look.
Instead, you should use the variable `current-prefix-arg', although
normally commands can get this prefix argument with (interactive \"P\").")

(defvar *current-prefix-arg* nil
  "The value of the prefix argument for this editing command.
It may be a number, or the symbol `-' for just a minus sign as arg,
or a list whose car is a number for just one or more C-u's
or nil if no argument has been specified.
This is what `(interactive \"P\")' returns.")

(defgeneric lookup-command (name)
  (:documentation "lookup the command named NAME."))

(defmethod lookup-command ((name symbol))
  (gethash name *commands*))

(defmethod lookup-command ((name string))
  ;; FIXME: this can fill the keyword package with lots of junk
  ;; symbols.
  (gethash (intern (string-upcase name) "KEYWORD") *commands*))

(defun call-command (name &rest args)
  "Use this command to call an interactive command from a lisp program."
  (let ((cmd (lookup-command name)))
    (apply (command-fn cmd) args)))

(defvar *command-arg-type-hash* (make-hash-table)
  "A hash table of symbols. each symbol is an interactive argument
type whose value is a function that is called to gather input from the
user (or somewhere else) and return the result. For instance,
:BUFFER's value is read-buffer which prompts the user for a buffer and
returns it.

This variable is here to allow modules to add new argument types easily.")

;; (defun collect-command-args (cmd)
;;   "Return a list of values (some collected from the user) to pass to the CMD function."
;;   (mapcar (lambda (arg)
;; 	    (funcall (gethash (second arg) *command-arg-type-hash*)))
;; 	  (command-args cmd)))

(defvar *this-command-keys* nil
  "The key sequence that invoked the current command.")

(defun this-command-keys ()
  "Return the key sequence that invoked this command.
The value is a list of KEYs."
  *this-command-keys*)

(defun dispatch-command (name)
  (let* ((cmd (lookup-command name))
	 ;; (args (collect-command-args cmd))
	 (*this-command* (command-name cmd))
	 (*current-prefix-arg* *prefix-arg*)
	 ;; Bind this locally so its value is restored after the
	 ;; command is dispatched. Otherwise, calls to set-buffer
	 ;; would stick.
	 (*current-buffer* (window-buffer (frame-current-window (selected-frame)))))
    (clear-minibuffer)
    (handler-case (funcall (command-fn cmd))
      (quit (c)
        (declare (ignore c))
	;; FIXME: debug-on-quit
        (message "Quit"))
      (lice-condition (c)
        (message "~a" c))
      ;;       (error (c)
      ;; 	;; FIXME: lice has no debugger yet, so use the lisp's
      ;; 	;; debugger.
      ;; 	(if *debug-on-error*
      ;; 	    (error c)
      ;; 	  (message "~a" c)))
      )
    (setf *last-command* *this-command*
	  ;; reset command keys, since the command is over.
	  *this-command-keys* nil)))

;;; events

(defclass key ()
  ((char :type character :initarg :char :reader key-char)
   (control :type boolean :initform nil :initarg :control :reader key-control)
   (meta :type boolean :initform nil :initarg :meta :reader key-meta)
   (alt :type boolean :initform nil :initarg :alt :reader key-alt)
   (shift :type boolean :initform nil :initarg :shift :reader key-shift)
   (hyper :type boolean :initform nil :initarg :hyper :reader key-hyper)
   (super :type boolean :initform nil :initarg :super :reader key-super))
  (:documentation "A key event."))

(defun print-mods (key)
  (concatenate 'string
	       (when (key-control key) "C-")
	       (when (key-meta key) "M-")
	       (when (key-alt key) "A-")
	       (when (key-shift key) "S-")
	       (when (key-super key) "s-")
	       (when (key-hyper key) "H-")))

(defun print-key (key)
  (format nil "~a~a" (print-mods key) (or (char-name (key-char key)) (key-char key))))

(defmethod print-object ((obj key) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~s" (print-key obj))))

(defvar *current-event* nil
  "The current event being processed.")

(defvar *unread-command-events* nil
  "List of events to be read as the command input.
These events are processed first, before actual keyboard input.")

;; This is probably, maybe, temporary
(deftype keymap () 'hash-table)

(defun make-sparse-keymap ()
  (make-hash-table :size 200 :test 'equal))

(defvar *overriding-terminal-local-map* nil
  "Per-terminal keymap that overrides all other local keymaps.
If this variable is non-nil, it is used as a keymap instead of the
buffer's local map, and the minor mode keymaps and text property keymaps.
It also replaces `overriding-local-map'.

This variable is intended to let commands such as `universal-argument'
set up a different keymap for reading the next command.")

(defvar *overriding-local-map* nil
  "Keymap that overrides all other local keymaps.
If this variable is non-nil, it is used as a keymap--replacing the
buffer's local map, the minor mode keymaps, and char property keymaps.")

(defvar *global-map* (make-sparse-keymap)
  "The top level global keymap.")

(defvar *ctl-x-4-map* (make-sparse-keymap)
  "The C-x 4 keymap.")

(defvar *ctl-x-map* (make-sparse-keymap)
  "The C-x keymap.")

(defvar *ctl-c-map* (make-sparse-keymap)
  "The C-c keymap.")

(defvar *ctl-h-map* (make-sparse-keymap)
  "The C-h keymap.")

(defvar *current-kmap* nil
  "The key map that the next key event will use to find a
corresponding command.")

(defun last-command-char ()
  "Return the character of the last key event in the list of key
events that invoked the current command."
  (key-char (car *this-command-keys*)))

(defgeneric handle-key-binding (binding key-seq))

(defmethod handle-key-binding ((binding hash-table) key-seq)
  (let ((*current-kmap* binding))
    (push key-seq *this-command-keys*)
    ;;(message "~{~a ~}" (mapcar 'print-key (this-command-keys)))
    (next-event)))

(defmethod handle-key-binding ((binding symbol) key-seq)
  ;; reset the current-kmap in case the command reads input. XXX: Is
  ;; this hacky?
  (let ((*current-kmap* nil))
    ;; TODO: handle gathering args
    (push key-seq *this-command-keys*)
    (dispatch-command binding)))

;; XXX: This is hacky. Convert the class into a sequence. Maybe we should
;; use defstruct then?
(defun key-hashid (event)
  (typecase event
    (key (list (key-char event)
	       (key-control event)
	       (key-meta event)
	       (key-alt event)
	       (key-hyper event)
	       (key-super event)))
    (t t)))

(defun lookup-key (keymap key &optional accept-default)
  "In keymap KEYMAP, look up key sequence KEY.  Return the definition.
nil means undefined.  See doc of `define-key' for kinds of definitions.

Normally, `lookup-key' ignores bindings for t, which act as default
bindings, used when nothing else in the keymap applies; this makes it
usable as a general function for probing keymaps.  However, if the
third optional argument ACCEPT-DEFAULT is non-nil, `lookup-key' will
recognize the default bindings, just as `read-key-sequence' does."
  (or (gethash (key-hashid key) keymap)
      (when accept-default
	(gethash t keymap))))

;; XXX: this is temporary
(defconstant +key-backspace+ 0407)
(defconstant +key-enter+ 0527)
(defconstant +key-tab+ 0407)
(defconstant +key-escape+ 27)

(defun wait-for-event ()
  (loop
     for event = (frame-read-event (selected-frame))
     for procs = (poll-processes) do
     (cond (event
	    (return event))
	   ;; handle subprocesses
	   (procs
	    (dispatch-processes procs)
	    (frame-render (selected-frame))))
     ;; FIXME: Yes, I'd love to be able to sleep until there was
     ;; activity on one of the streams lice is waiting for input on
     ;; but i don't know how to do that. So just sleep for a tiny
     ;; bit to pass control over to the operating system and then
     ;; check again.
     (sleep 0.01)))

;; This is really TTY specific
(defun next-event ()
  (let* ((*current-event* (if *unread-command-events*
			      (pop *unread-command-events*)
			      (wait-for-event)))
	 (def (if *current-kmap*
		  (lookup-key *current-kmap* *current-event* t)
		;; no current kmap? 
		(or 
		 (when *overriding-terminal-local-map* 
		   (lookup-key *overriding-terminal-local-map* *current-event* t))
		 (when *overriding-local-map* 
		   (lookup-key *overriding-local-map* *current-event* t))
		 (lookup-key (major-mode-map (buffer-major-mode (current-buffer))) *current-event* t)
		 ;; check the global map
		 (lookup-key *global-map* *current-event* t)))))
    (dformat +debug-v+ "~a ~s ~a~%"
	     def (key-hashid *current-event*) (key-char *current-event*))
    (if def
	(handle-key-binding def *current-event*)
      (message "~{~a ~}is undefined" (mapcar 'print-key (cons *current-event* (this-command-keys)))))))
  
(defun define-key (keymap key def)
  (setf (gethash (key-hashid key) keymap) def))

(defun make-ctrl-h-map ()
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap (make-instance 'key :char #\f) 'describe-symbol)
    kmap))

(defun make-ctrl-x-4-map ()
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap (make-instance 'key :char #\b) 'switch-to-buffer-other-window)
    kmap))

(defun make-ctrl-x-map (ctl-x-4-map)
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap (make-instance 'key :char #\e :control t) 'eval-last-sexp)
    (define-key kmap (make-instance 'key :char #\b) 'switch-to-buffer)
    (define-key kmap (make-instance 'key :char #\c :control t) 'save-buffers-kill-emacs)
    (define-key kmap (make-instance 'key :char #\f :control t) 'find-file)
    (define-key kmap (make-instance 'key :char #\s :control t) 'save-buffer)
    (define-key kmap (make-instance 'key :char #\k) 'kill-buffer)
    (define-key kmap (make-instance 'key :char #\o) 'other-window)
    (define-key kmap (make-instance 'key :char #\1) 'delete-other-windows)
    (define-key kmap (make-instance 'key :char #\2) 'split-window-vertically)
    (define-key kmap (make-instance 'key :char #\3) 'split-window-horizontally)
    (define-key kmap (make-instance 'key :char #\x :control t) 'exchange-point-and-mark)
    (define-key kmap (make-instance 'key :char #\t :control t) 'transpose-lines)
    (define-key kmap (make-instance 'key :char #\4) ctl-x-4-map)
    kmap))

(defun make-ctrl-c-map ()
  (let ((kmap (make-sparse-keymap)))
    kmap))

(defun make-global-map (ctl-x-prefix ctl-c-prefix ctl-h-prefix)
  "Generate self-insert commands for all printable characters. And
more."
  (let ((kmap (make-sparse-keymap)))
    (loop for i in '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
		     #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j
		     #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t
		     #\u #\v #\w #\x #\y #\z 
		     #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J
		     #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T
		     #\U #\V #\W #\X #\Y #\Z
		     #\Space #\! #\" #\# #\$ #\% #\& #\' #\( 
		     #\) #\* #\+ #\, #\- #\. #\/ #\: #\; #\< 
		     #\= #\> #\? #\@ #\[ #\\ #\] #\^ #\_ #\` 
		     #\| #\} #\~ #\{)
	  do (define-key kmap (make-instance 'key :char i) 'self-insert-command))
    (define-key kmap (make-instance 'key :char #\Return) 'newline)
    (define-key kmap (make-instance 'key :char #\Newline) 'newline)
    (define-key kmap (make-instance 'key :char #\o :control t) 'open-line)
    (define-key kmap (make-instance 'key :char #\j :control t) 'newline)
    (define-key kmap (make-instance 'key :char #\m :control t) 'newline)
    (define-key kmap (make-instance 'key :char #\f :control t) 'forward-char)
    (define-key kmap (make-instance 'key :char #\f :meta t) 'forward-word)
    (define-key kmap (make-instance 'key :char #\f :control t :meta t) 'forward-sexp)
    (define-key kmap (make-instance 'key :char #\b :control t :meta t) 'backward-sexp)
    (define-key kmap (make-instance 'key :char #\n :control t) 'next-line)
    (define-key kmap (make-instance 'key :char #\p :control t) 'previous-line)
    (define-key kmap (make-instance 'key :char #\b :control t) 'backward-char)
    (define-key kmap (make-instance 'key :char #\b :meta t) 'backward-word)
    (define-key kmap (make-instance 'key :char #\d :control t) 'delete-char)
    (define-key kmap (make-instance 'key :char #\d :meta t) 'kill-word)
    (define-key kmap (make-instance 'key :char #\Rubout :meta t) 'backward-kill-word)
    (define-key kmap (make-instance 'key :char #\Rubout) 'delete-backward-char)
    (define-key kmap (make-instance 'key :char #\Delete) 'delete-backward-char)
    (define-key kmap (make-instance 'key :char #\t :meta t) 'transpose-words)
    (define-key kmap (make-instance 'key :char #\t :control t) 'transpose-chars)
    ;;(define-key kmap (make-instance 'key :char #\h :control t) 'delete-backward-char)
    (define-key kmap (make-instance 'key :char #\u :control t) 'universal-argument)
    (define-key kmap (make-instance 'key :char #\a :control t) 'beginning-of-line)
    (define-key kmap (make-instance 'key :char #\e :control t) 'end-of-line)
    (define-key kmap (make-instance 'key :char #\g :control t) 'keyboard-quit)
    (define-key kmap (make-instance 'key :char #\v :control t) 'scroll-up)
    (define-key kmap (make-instance 'key :char #\v :meta t) 'scroll-down)
    (define-key kmap (make-instance 'key :char #\k :control t) 'kill-line)
    (define-key kmap (make-instance 'key :char #\w :control t) 'kill-region)
    (define-key kmap (make-instance 'key :char #\y :control t) 'yank)
    (define-key kmap (make-instance 'key :char #\y :meta t) 'yank-pop)
    (define-key kmap (make-instance 'key :char #\w :meta t) 'kill-ring-save)
    (define-key kmap (make-instance 'key :char #\> :meta t) 'end-of-buffer)
    (define-key kmap (make-instance 'key :char #\< :meta t) 'beginning-of-buffer)
    (define-key kmap (make-instance 'key :char #\x :meta t) 'execute-extended-command)
    (define-key kmap (make-instance 'key :char #\: :meta t) 'eval-expression)
    (define-key kmap (make-instance 'key :char #\Space :control t) 'set-mark-command)
    (define-key kmap (make-instance 'key :char #\` :control t) 'set-mark-command)
    (define-key kmap (make-instance 'key :char #\! :meta t) 'shell-command)
    (define-key kmap (make-instance 'key :char #\x :control t) ctl-x-prefix)
    (define-key kmap (make-instance 'key :char #\c :control t) ctl-c-prefix)
    (define-key kmap (make-instance 'key :char #\h :control t) ctl-h-prefix)
    kmap))

(defun make-global-keymaps ()
  "Create the default global keymaps and store them in *global-kmap
*ctl-x-map*, ..."
  (setf *ctl-x-4-map* (make-ctrl-x-4-map)
	*ctl-x-map* (make-ctrl-x-map *ctl-x-4-map*)
	*ctl-c-map* (make-ctrl-c-map)
	*ctl-h-map* (make-ctrl-h-map)
	*global-map* (make-global-map *ctl-x-map* *ctl-c-map* *ctl-h-map*)))

(provide :lice-0.1/input)
