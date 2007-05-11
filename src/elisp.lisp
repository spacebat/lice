(cl:defpackage "ELISP"
  (:nicknames "EL")
  (:use "CL")
  (:shadow cl:if cl:defun)
  (:export #:if #:defun))

(in-package "ELISP")

(defmacro if (test pass &rest else)
  "Elisp version of IF."
  `(cl:if ,test
          ,pass
          (progn 
            ,@else)))


(cl:defun parse-interactive (thing)
  (error "unimplemented"))

(defmacro defun (name lambda-list &body body)
  "Parse an elisp style defun and convert it to a cl defun or lice defcommand."
  (let ((doc (when (stringp (car body))
               (pop body)))
        (decls (loop while (eq (caar body) 'declare)
                  collect (pop body)))
        (interactive (when (and (listp (car body))
                                (eq (caar body) 'interactive))
                       (pop body))))
    (if interactive
        `(defcommand ,name (,lambda-list
                            ,@(parse-interactive (cdr interactive)))
           ,@(append (list doc) decls body))
        `(cl:defun ,name ,@(append (list doc) decls body)))))
