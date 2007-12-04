;; -*- lisp -*-

(defpackage :lice-system (:use :cl :asdf))
(in-package :lice-system)

(defsystem :lice
    :depends-on (#-clisp cl-ncurses cl-ppcre #+sbcl sb-posix)
    :serial t
    :components ((:file "package")
                 (:file "wrappers")
                 (:file "emacs")
                 (:file "callproc")
                 (:file "elisp")
                 (:file "global")
                 (:file "fns")
                 (:file "data")
                 (:file "custom")
                 (:file "commands")
                 (:file "callint")
                 (:file "dired")
                 (:file "data-types")
                 (:file "charset")
                 (:file "subprocesses")
                 (:file "buffer-local")
                 (:file "keymap")
                 (:file "casefiddle")
                 (:file "buffer")
                 (:file "intervals")
                 (:file "textprop")
                 (:file "search")
                 (:file "frame")
                 (:file "window")
                 (:file "render")
                 (:file "wm")

                 ;; from this point on there are warnings because of two-way dependencies
                 (:file "insdel")
                 (:file "cmds")
                 (:file "editfns")
                 (:file "undo")
                 (:file "syntax")
                 (:file "major-mode")
                 (:file "keyboard")
                 (:file "debugger")
                 (:file "recursive-edit")
                 (:file "minibuffer")
                 (:file "files")
                 (:file "help")
                 (:file "debug")
                 #+sbcl (:file "tty-render")
                 #+clisp (:file "clisp-render")
                 (:file "indent")

                 (:module emacs-lisp
                          :serial t
                          :components ((:file "easy-mmode")
                                       (:file "lisp-mode")))

                 (:module lisp
                          :serial t
                          :components ((:file "subr")
                                       (:file "simple")
                                       (:file "lisp-indent")
                                       (:file "paragraphs")
                                       (:file "bindings")
                                       ;; (:file "paren")
				       ))

                 (:module textmodes
                          :serial t
                          :components (;; (:file "fill" :depends-on ()) ; this one is too advanced for now
                                       (:file "text-mode")))

                 (:module play
                          :serial t
                          :components ((:file "dissociate")
                                       (:file "hanoi")
                                       (:file "doctor")))

                 (:file "main")))
