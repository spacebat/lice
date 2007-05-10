(cl:defpackage "ELISP"
  (:nicknames "EL")
  (:use "CL")
  (:shadow cl:if)
  (:export #:if))

(in-package "ELISP")

(defmacro if (test pass &rest else)
  "Elisp version of IF."
  `(cl:if ,test
          ,pass
          (progn 
            ,@else)))

