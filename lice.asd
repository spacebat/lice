;; -*- lisp -*-

(defpackage :lice
  (:use :cl))

#+sbcl (require 'sb-posix)

(defsystem :lice 
  :depends-on (cl-ncurses)
  :components ((:file "wrappers")
	       (:file "global" :depends-on ("wrappers"))
	       (:file "input" :depends-on ("global"))
	       (:file "subr" :depends-on ("input"))
	       (:file "major-mode" :depends-on ("subr"))
	       (:file "buffer" :depends-on ("major-mode"))
	       (:file "files" :depends-on ("buffer"))
	       (:file "intervals" :depends-on ("files"))
	       (:file "textprop" :depends-on ("intervals"))
	       (:file "editfns" :depends-on ("textprop"))
	       (:file "window" :depends-on ("editfns"))
	       (:file "frame" :depends-on ("window"))
	       (:file "tty-render" :depends-on ("frame"))
	       (:file "debugger" :depends-on ("tty-render"))
	       (:file "recursive-edit" :depends-on ("debugger"))
	       (:file "wm" :depends-on ("recursive-edit"))
	       (:file "minibuffer" :depends-on ("wm"))
	       (:file "simple" :depends-on ("minibuffer"))
	       (:file "lisp-mode" :depends-on ("wm"))
	       (:file "search" :depends-on ("lisp-mode"))
	       (:file "syntax" :depends-on ("search"))
	       (:file "help" :depends-on ("syntax"))
	       (:file "debug" :depends-on ("help"))
	       (:file "subprocesses" :depends-on ("debug"))
	       (:file "main" :depends-on ("subprocesses"))))
