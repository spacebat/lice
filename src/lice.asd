;; -*- lisp -*-

(defpackage :lice-system (:use :cl :asdf))
(in-package :lice-system)

(load "package.lisp")

(defsystem :lice
    :depends-on (#-clisp cl-ncurses cl-ppcre #+sbcl sb-posix)
    :components ((:file "wrappers")
                 (:file "elisp")
                 (:file "global")
                 (:file "custom")
                 (:file "commands")
                 (:file "data-types")
                 (:file "charset")
                 (:file "keymap" :depends-on ("global"))
                 (:file "casefiddle")
                 (:file "subprocesses" :depends-on ("wrappers" "commands"))
                 (:file "buffer-local" :depends-on ("data-types"))
                 (:file "buffer" :depends-on ("data-types" "buffer-local" "commands" "wrappers" "global"))
                 (:file "intervals" :depends-on ("data-types"))
                 (:file "textprop" :depends-on ("intervals" "global"))
                 (:file "search" :depends-on ("buffer"))
                 (:file "frame" :depends-on ("data-types"))
                 (:file "window" :depends-on ("buffer" "search" "commands" "frame" "data-types"))
                 (:file "render" :depends-on ("frame" "window"))
                 (:file "wm" :depends-on ("data-types" "window" "frame"))

                 ;; from this point on there are warnings because of two-way dependencies
                 (:file "insdel" :depends-on ("intervals" #|"undo"|# "buffer"))
                 (:file "cmds" :depends-on ("keymap" "insdel"))
                 (:file "editfns" :depends-on ("buffer" "insdel" "textprop" "cmds"))
                 (:file "undo" :depends-on ("commands" "window"))
                 (:file "syntax" :depends-on ("buffer"))
                 (:file "major-mode" :depends-on ("keymap" "syntax"))
                 (:file "keyboard" :depends-on ("commands" "keymap" "subprocesses" "render"))
                 (:file "debugger" :depends-on ("commands" "major-mode"))
                 (:file "recursive-edit" :depends-on ("keyboard" "render" "debugger"))
                 (:file "minibuffer" :depends-on ("buffer" "window" "recursive-edit" "wm"))
                 (:file "files" :depends-on ("buffer" "buffer-local" "commands" "custom"))
                 (:file "help" :depends-on ("buffer" "commands"))
                 (:file "debug" :depends-on ("buffer" "commands"))
                 #+sbcl (:file "tty-render" :depends-on ("buffer" "window" "frame" "render"))
                 #+clisp (:file "clisp-render" :depends-on ("buffer" "window" "frame" "render"))
                 (:file "main" :depends-on ("buffer" "major-mode" "elisp" #+sbcl "tty-render" #+clisp "clisp-render"))
                 (:file "indent" :depends-on (#|"subr"|#))

                 (:module lisp
                          :components ((:file "subr")
                                       (:file "simple" :depends-on ("subr" #|"textmodes/fill"|#))
                                       (:file "lisp-mode" :depends-on (#|"indent"|# "simple"))
                                       (:file "lisp-indent" :depends-on ("lisp-mode" #|"indent"|# "simple"))
                                       (:file "paragraphs" :depends-on ("simple"))))

                 (:module textmodes
                          :components (;; (:file "fill" :depends-on ()) ; this one is too advanced for now
                                       (:file "text-mode" :depends-on ())))

                 (:module play
                          :components ((:file "dissociate" :depends-on nil)
                                       (:file "hanoi")
                                       (:file "doctor" :depends-on (#|"simple" "paragraphs" text-mode"|#))))))
