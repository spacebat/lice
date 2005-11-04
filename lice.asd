;; -*- lisp -*-

(defpackage :lice
  (:use :cl))

(defsystem :lice 
  :depends-on (cl-ncurses)
  :components ((:file "wrappers")
	       (:file "global" :depends-on ("wrappers"))
	       (:file "recursive-edit" :depends-on ("global" "input" "frame"))
	       (:file "major-mode" :depends-on ("global" "input"))
	       (:file "buffer" :depends-on ("global" "major-mode"))
	       (:file "files" :depends-on ("buffer"))
	       (:file "intervals" :depends-on ("buffer"))
	       (:file "textprop" :depends-on ("buffer"))
	       (:file "editfns" :depends-on ("textprop" "intervals"))
	       (:file "input" :depends-on ("global"))
	       (:file "debug" :depends-on ("buffer" "frame"))
	       (:file "help" :depends-on ("buffer" "frame"))
	       (:file "main" :depends-on ("global" "tty-render" "simple" "buffer" "recursive-edit" "subr" "lisp-mode" "search" "syntax"))
	       (:file "minibuffer" :depends-on ("recursive-edit" "frame" "editfns" "wm"))
	       (:file "window" :depends-on ("buffer"))
	       (:file "frame" :depends-on ("window"))
	       (:file "wm" :depends-on ("frame" "window"))
	       (:file "tty-render" :depends-on ("window" "frame"))
	       (:file "simple" :depends-on ("global" "frame" "window"
					    "input" "buffer" "subr" "minibuffer"))
	       (:file "subr" :depends-on ("global"))
	       (:file "lisp-mode" :depends-on ("simple"))
	       (:file "search" :depends-on ("simple"))
	       (:file "syntax" :depends-on ("simple"))))
