;;; handle subprocesses and threads

(in-package "LICE")

(defclass base-process ()
  ((input-stream :initarg :error-stream :accessor process-input-stream)
   (output-stream :initarg :output-stream :accessor process-output-stream)
   (error-stream :initarg :error-stream :accessor process-error-stream)
   (name :initarg :name :accessor process-name)
   (filter :initarg :filter :initform nil :accessor process-filter)
   (sentinel :initarg :sentinel :initform nil :accessor process-sentinel)))

(defclass subprocess (base-process)
  ((process :initarg :internal-process :accessor subprocess-internal-process)))

(defclass thread-process (base-process)
  ((thread :initarg :thread :accessor thread-process-thread)))

(defclass stream-process (base-process)
  ()
  (:documentation "No process or thread is attached to this
process. It's just a stream whose filter function gets called
when input exists. An eof would presumably call the sentinel and
clean this process up.

This is how network streams can be handled like gnu emacs does."))

(defclass user-process (base-process)
  ()
  (:documentation "When we call the filter and sentinel functions
  is decided by calling a function. I'm gonna try implementing an irc mode with this."))

(defclass base-buffer-process-mixin ()
  ((buffer :initarg :buffer :accessor process-buffer)
   (mark :initarg :mark :accessor process-mark)))

(defclass buffer-subprocess (base-buffer-process-mixin subprocess)
  ())

(defclass buffer-thread-process (base-buffer-process-mixin thread-process)
  ())

(defclass buffer-stream-process (base-buffer-process-mixin stream-process)
  ())

(defvar *process-list* nil
  "")

(defmethod initialize-instance :after ((instance base-process) &rest initargs &key &allow-other-keys)
  "Keep track of the instances in our list."
  (declare (ignore initargs))
  (push instance *process-list*))

(defun subprocess-alive-p (subproc)
  (internal-process-alive-p (subprocess-internal-process subproc)))

(defun process-activity-p (process)
  (or (and (process-output-stream process)
	   (listen (process-output-stream process)))
      (and (process-error-stream process)
	   (listen (process-error-stream process)))
      ;; if the process quit, we need to clean things up
      (and (typep process 'subprocess)
	   (not (subprocess-alive-p process)))))

(defun poll-processes (&aux (process-list *process-list*))
  "Return the list of processes that need processing."
  (loop for p in process-list
       when (process-activity-p p)
       collect p))

(defun process-ready-streams (process)
  (let (streams)
    (and (process-output-stream process)
	 (listen (process-output-stream process))
	 (push (process-output-stream process) streams))
    (and (process-error-stream process)
	 (listen (process-error-stream process))
	 (push (process-error-stream process) streams))
    streams))

(defgeneric dispatch-process (process)
  (:documentation "Call the filter and sentinel functions on process, if needed."))

(defmethod dispatch-process :around ((process base-process))
  "Handle calling the filter functions."
  (let ((streams (process-ready-streams process)))
    (when streams
      (if (process-filter process)
	  (if (process-buffer process)
	      (with-current-buffer (process-buffer process)
		;; TODO: set the point to the process mark
		(funcall (process-filter process) process))
	      ;; FIXME: which buffer should be current if the process has no buffer?
	      (funcall (process-filter process) process))
	  ;; or discard the data
	  (mapc 'clear-input streams))))
  (call-next-method))

(defmethod dispatch-process ((process subprocess))
  "call the sentinel. FIXME: this isn't quite right. we wanna call
the sentinel when its status has changed which includes other
states."
  (unless (subprocess-alive-p process)
    (unwind-protect
	 (and (process-sentinel process)
	      (funcall (process-sentinel process) process))
      ;; its dead. clean it up.
      (setf *process-list* (remove process *process-list*)))))

(defmethod dispatch-process ((process thread-process))
  (unless (thread-alive-p (thread-process-thread process))
    (unwind-protect
	 (and (process-sentinel process)
	      (funcall (process-sentinel process) process))
      ;; its dead. clean it up.
      (setf *process-list* (remove process *process-list*)))))

(defmethod dispatch-process ((process stream-process))
  ;; FIXME: We maybe want something more flexible than just checking
  ;; the state of the stream.
  (unless (open-stream-p (process-output-stream process))
    (unwind-protect
	 (and (process-sentinel process)
	      (funcall (process-sentinel process) process))
      ;; its dead. clean it up.
      (setf *process-list* (remove process *process-list*)))))    

(defun dispatch-processes (process-list)
  "Call whatever handler function is hooked up to the process"
  (mapc 'dispatch-process process-list))

(defun default-buffer-process-filter (process)
  "The default process filter function. Read input from streams
and insert it into the process' buffer. Or discard it if buffer
is nil."
  (when (process-buffer process)
    ;; TODO: If the other process is spitting stuff out as fast as
    ;; possible is this gonna spin til the program calms down? We
    ;; don't want that I don't think. -sabetts
    (while (listen (process-output-stream process))
      (insert (read-line (process-output-stream process)) #\Newline))
    ;; handle error output too
    (while (listen (process-error-stream process))
      (insert (read-line (process-error-stream process)) #\Newline))))

(defun handle-subprocess-state-change (internal-proc)
  (let ((process (find internal-proc *process-list* 
		       :key 'subprocess-internal-process)))
    (when process
      (when (process-sentinel process)
	(funcall (process-sentinel process)))
      ;; assume the process is killed for now
      (setf *process-list* (remove process *process-list*)))))

(defun default-buffer-process-sentinel (process)
  (declare (ignore process))
  ;; uh wuddo we do here?
  )

(defun start-process (name buffer program &rest program-args)
  "Start a program in a subprocess.  Return the process object for it.
name is name for process.  It is modified if necessary to make it unique.
buffer is the buffer (or buffer name) to associate with the process.
 Process output goes at end of that buffer, unless you specify
 an output stream or filter function to handle the output.
 buffer may be also nil, meaning that this process is not associated
 with any buffer.
program is the program file name.  It is searched for in PATH.
Remaining arguments are strings to give program as arguments."
  (let* ((buf (and buffer (get-buffer-create buffer)))
	 (mark (and buf (copy-marker (point-marker buf)))))
    (multiple-value-bind (proc input output error) (run-program program program-args)
      (make-instance 'buffer-subprocess
		     :internal-process proc
		     :input-stream input
		     :output-stream output
		     :error-stream error
		     :filter 'default-buffer-process-filter
		     :sentinel 'default-buffer-process-sentinel
		     :name name
		     :buffer buf
		     :mark mark))))

(defun open-network-stream (name buffer host service)
  "Open a TCP connection for a service to a host.
Returns a subprocess-object to represent the connection.
Input and output work as for subprocesses; `delete-process' closes it.

Args are name buffer host service.
name is name for process.  It is modified if necessary to make it unique.
buffer is the buffer (or buffer name) to associate with the process.
 Process output goes at end of that buffer, unless you specify
 an output stream or filter function to handle the output.
 buffer may be also nil, meaning that this process is not associated
 with any buffer.
host is name of the host to connect to, or its IP address.
service is name of the service desired, or an integer specifying
 a port number to connect to."
  (declare (ignore name buffer host service))
  ;; TODO: implement
  (error "unimplemented")
  )
  
(defvar *shell-file-name* (getenv "SHELL")
  "File name to load inferior shells from.
Initialized from the SHELL environment variable.")

(defvar *shell-command-switch* "-c"
  "Switch used to have the shell execute its command line argument.")

(defcommand shell-command ((command)
			   (:string "Shell Command: "))
  (let ((buf (get-buffer-create "*Async Shell Command*")))
    (erase-buffer buf)
    (display-buffer buf)
    (start-process "Shell Command" "*Async Shell Command*"
		   *shell-file-name*
		   *shell-command-switch*
		   command)))

(defgeneric kill-process (process)
  (:documentation "Kill process process.  May be process or name of one.
See function `interrupt-process' for more details on usage."))

(defmethod kill-process ((process subprocess))
  ;; send the QUIT signal
  (internal-process-kill (subprocess-internal-process process)))
  
(defmethod kill-process ((obj thread-process))
  (kill-thread (thread-process-thread obj)))

(defgeneric delete-process (process)
  (:documentation "Delete process: kill it and forget about it immediately.
process may be a process, a buffer, the name of a process or buffer, or
nil, indicating the current buffer's process."))

;; (defmethod delete-process ((proces subprocess))
;;   ;; TODO
;;   )

;; (defmethod delete-process ((obj buffer))
;;   ;; TODO
;;   )

;; (defmethod delete-process ((obj string))
;;   ;; TODO
;;   )

;; (defmethod delete-process ((process thread-process))
;;   ;; TODO
;;   )

;; (defmethod delete-process ((process null))
;;   ;; TODO
;;   )
