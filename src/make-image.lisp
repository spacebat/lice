;;; SBCL
#+sbcl
(progn
  (require 'asdf)
  (require 'lice))
#+sbcl 
(sb-ext:save-lisp-and-die "lice" :toplevel (lambda ()
                                             ;; asdf requires sbcl_home to be set, so set it to the value when the image was built
                                             (sb-posix:putenv (format nil "SBCL_HOME=~A" #.(sb-ext:posix-getenv "SBCL_HOME")))
                                             (lice::lice)
                                             0)
                          :executable t)

;;; CLISP

;; asdf needs to be loaded. try putting (load "/path/to/asdf.lisp") in your .clisprc file
#+clisp
(asdf:oos 'asdf:load-op :lice)

#+clisp
(progn
  (ext:saveinitmem "lice" :init-function (lambda ()
                                              (lice::lice)
                                              (ext:quit))
                   :executable t :keep-global-handlers t :norc t :documentation "Lisp Computing Environment"))


#-(or sbcl clisp) (error "This lisp implementation is not supported.")
