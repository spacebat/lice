;;; Handle input and key command dispatching

(in-package "LICE")

(define-condition quit (lice-condition)
  () (:documentation "A condition raised when the user aborted the
operation (by pressing C-g, for instance)."))

(defvar *last-command* nil
  "The last command executed.")

(defvar *this-command* nil
  "The command that was executed. This is to the command being
executed before it is executed. *last-command* will be set to this
when the command finishes. The command can change this value if it
wants to change what *last-command* will be set to. Used in the `yank'
and `yank-pop' commands.")

(defvar *current-prefix-arg* nil
  "The value of the prefix argument for this editing command.
It may be a number, or the symbol `-' for just a minus sign as arg,
or a list whose car is a number for just one or more C-u's
or nil if no argument has been specified.
This is what `(interactive \"P\")' returns.")

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
	 (*current-prefix-arg* *prefix-arg*))
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
	  *this-command-keys* nil)
    ;; handle undo
    (undo-boundary)

))

;;; events

(defvar *unread-command-events* nil
  "List of events to be read as the command input.
These events are processed first, before actual keyboard input.")

(defun last-command-char ()
  "Return the character of the last key event in the list of key
events that invoked the current command."
  (key-char (car *this-command-keys*)))

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
		   (lookup-key-internal *overriding-terminal-local-map* *current-event* t *current-keymap-theme* t))
		 (when *overriding-local-map* 
		   (lookup-key-internal *overriding-local-map* *current-event* t *current-keymap-theme* t))
                 (when (current-local-map)
                   (lookup-key-internal (current-local-map) *current-event* t *current-keymap-theme* t))
                 ;;(lookup-key-internal (major-mode-map (major-mode)) *current-event* t *current-keymap-theme* t)
                 ;; TODO: minor mode maps
		 ;; check the global map
		 (lookup-key-internal *global-map* *current-event* t *current-keymap-theme* t)))))
    (dformat +debug-v+ "~a ~s ~a~%"
	     def #|(key-hashid *current-event*)|# *current-event* (key-char *current-event*))
    (if def
	(handle-key-binding def *current-event*)
      (message "~{~a ~}is undefined" (mapcar 'print-key (cons *current-event* (this-command-keys)))))))

(defgeneric handle-key-binding (binding key-seq))

(defmethod handle-key-binding ((binding keymap) key-seq)
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

;; XXX: this is temporary
(defconstant +key-backspace+ 0407)
(defconstant +key-enter+ 0527)
(defconstant +key-tab+ 0407)
(defconstant +key-escape+ 27)

(defun wait-for-event ()
  ;; don't let the user C-g when reading for input
  (let ((*waiting-for-input* t))
    (loop
       for event = (frame-read-event (selected-frame))
       for procs = (poll-processes) do
       ;; they hit the interrupt key so simulate that key press
       (when *quit-flag*
         (setf *quit-flag* nil
               event (make-key
                      :char (code-char (+ *quit-code* 96))
                      :control t)))
       (cond (event
              (return event))
             ;; handle subprocesses
             (procs
              ;; let the user break out of this stuff
              (let ((*waiting-for-input* nil))
                (dispatch-processes procs)
                (frame-render (selected-frame))))
             (t
              ;; FIXME: Yes, I'd love to be able to sleep until there was
              ;; activity on one of the streams lice is waiting for input on
              ;; but i don't know how to do that. So just sleep for a tiny
              ;; bit to pass control over to the operating system and then
              ;; check again.
              (sleep 0.01))))))


(defun top-level-next-event ()
  ;; Bind this locally so its value is restored after the
  ;; command is dispatched. Otherwise, calls to set-buffer
  ;; would stick.
  (setf *current-buffer* (window-buffer (frame-selected-window (selected-frame))))
  (next-event))

(provide :lice-0.1/input)
