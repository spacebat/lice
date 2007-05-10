;;; A collection of wrappers around extended functionality that may be
;;; different across CL implementations.

;;; To add support for a new CL implementation, an entry in each of
;;; these functions must be made for it.

;; don't print the unable to optimize notes
#+sbcl (declaim (sb-ext:muffle-conditions sb-ext:compiler-note))

(in-package "LICE")

;;; Weak Pointers

(defun weak-pointer-p (wp)
  #+clisp (ext:weak-pointer-p (wp))
  #-(or clisp) (declare (ignore wp)))

(defun make-weak-pointer (data)
  #+clisp (ext:make-weak-pointer data)
  #+cmu (extensions:make-weak-pointer data)
  #+sbcl (sb-ext:make-weak-pointer data)
  #+(or movitz mcl) data)

(defun weak-pointer-value (wp)
  #+clisp (ext:weak-pointer-value wp)
  #+cmu (extensions:weak-pointer-value wp)
  #+sbcl (sb-ext:weak-pointer-value wp)
  #+(or movitz mcl) (values wp t))

;;; Some wrappers for access to filesystem things. file type, file
;;; size, ownership, modification date, mode, etc

(defun file-stats (pathname)
  "Return some file stats"
  #+sbcl
  (let ((stat (sb-posix:lstat pathname)))
    (values (sb-posix:stat-mode stat)
	    (sb-posix:stat-size stat)
	    (sb-posix:stat-uid stat)
	    (sb-posix:stat-gid stat)
	    (sb-posix:stat-mtime stat)))
  #+clisp
  (let ((stat (sys::file-stat pathname)))
    (values (butlast (sys::file-stat-mode stat))
	    (sys::file-stat-size stat)
	    (sys::file-stat-uid stat)
	    (sys::file-stat-gid stat)
	    (sys::file-stat-mtime stat)))
  #-(or sbcl clisp)
  (error "Not implemented"))

;;; subprocesses

(defun run-program (program args &key (output :stream) (error :stream) (input :stream) (sentinel nil))
  #+sbcl (let ((p (sb-ext:run-program program args :output output :error error :input input :status-hook sentinel)))
	   (values p
		   (sb-ext:process-input p)
		   (sb-ext:process-output p)
		   (sb-ext:process-error p)))
  #-sbcl (error "Not implemented"))

(defun internal-process-alive-p (process)
  #+sbcl (sb-ext:process-alive-p process)
  #-sbcl (error "Not implemented"))

(defun internal-process-kill (process &optional (signal 3))
  #+sbcl (sb-ext:process-kill process signal)
  #-(or sbcl) (error "Not implemented"))

;;; threads

(defun make-thread (function)
  #+sbcl (sb-thread:make-thread function)
  #-(or sbcl) (error "Not implemented"))

(defun thread-alive-p (thread)
  #+sbcl (sb-thread:thread-alive-p thread)
  #-(or sbcl) (error "Not implemented"))

(defun kill-thread (thread)
  #+sbcl (sb-thread:terminate-thread thread)
  #-(or sbcl) (error "Not implemented"))

(defun make-mutex ()
  #+sbcl (sb-thread:make-mutex)
  #-(or sbcl) (error "Not implemented"))

(defmacro with-mutex ((mutex) &body body)
  #+sbcl `(sb-thread:with-mutex (,mutex) ,@body)
  #-(or sbcl) (error "Not implemented"))

;;; environment

(defun getenv (var)
  "Return the value of the environment variable."
  #+clisp (ext:getenv (string var))
  #+sbcl (sb-posix:getenv (string var))
  #-(or clisp sbcl)
  (error "Not implemented"))

;;; debugger

(defun backtrace-as-string (&optional (depth most-positive-fixnum))
  (with-output-to-string (s)
    #+sbcl (sb-debug:backtrace depth s)
    #-(or sbcl)
    (error "Not implemented")))

;;; terminal manipulation

;;; SIGINT error. we setup our own error handler.

(define-condition user-break (simple-condition)
  ())

#+sbcl
(defun sbcl-sigint-handler (&rest junk)
  (declare (ignore junk))
  (flet ((break-it ()
           (with-simple-restart (continue "continue from break")
             (invoke-debugger (make-condition 'user-break
                                              :format-control "User break")))))
    (sb-thread:interrupt-thread (sb-thread::foreground-thread) #'break-it)))

(defun enable-sigint-handler ()
  #+sbcl (sb-unix::enable-interrupt sb-unix::sigint #'sbcl-sigint-handler)

  #-(or sbcl) (error "not implemented"))

(defvar *old-term-settings* nil)

(defun term-backup-settings ()
  #+sbcl
  (setf *old-term-settings* (sb-posix:tcgetattr 0))

  #-(or sbcl) (error "not implemented"))

(defun term-restore-settings ()
  #+sbcl
  (sb-posix:tcsetattr 0 0 *old-term-settings*)

  #-(or sbcl) (error "not implemented"))

(defun term-set-quit-char (code)
  #+sbcl
  (let ((attr (sb-posix:tcgetattr 0))
        ;; according to termios.h VINTR is 8
        (vintr 8))
    (setf (aref (sb-posix:termios-cc attr) vintr) code)
    (sb-posix:tcsetattr 0 0 attr))

  #-(or sbcl) (error "not implemented"))

;;; two way streams 


(provide :lice-0.1/wrappers)

