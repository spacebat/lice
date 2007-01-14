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
  ())

(defclass base-buffer-process ()
  ((buffer :initarg :buffer :accessor process-buffer)
   (mark :initarg :mark :accessor process-mark)))

(defclass buffer-subprocess (base-buffer-process subprocess)
  ())

(defclass buffer-thread-process (base-buffer-process base-thread-process)
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

(defun dispatch-process (process)
  (cond 
    ;; call the filter function
    ((or (and (process-output-stream process)
	      (listen (process-output-stream process)))
	 (and (process-error-stream process)
	      (listen (process-error-stream process))))
     (if (process-filter process)
	 (if (process-buffer process)
	     (with-current-buffer (process-buffer process)
	       ;; TODO: set the point to the process mark
	       (funcall (process-filter process) process))
	     (funcall (process-filter process) process))
	 ;; or discard the data
	 (progn
	   (and (process-output-stream process)
		(clear-input (process-output-stream process)))
	   (and (process-error-stream process)
		(clear-input (process-error-stream process))))))
    ;; call the sentinel. FIXME: this isn't quite right. we wanna call
    ;; the sentinel when its status has changed which includes other
    ;; states.
    ((and (typep process 'subprocess)
	  (not (subprocess-alive-p process)))
     (unwind-protect
	  (and (process-sentinel process)
	       (funcall (process-sentinel process) process))
       ;; its dead. clean it up.
       (setf *process-list* (remove process *process-list*))))))

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
    (loop while (listen (process-output-stream process)) do
	 (insert (read-line (process-output-stream process)) #\Newline))))

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
	 (mark (and buf (make-marker (point buf) buf))))
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
