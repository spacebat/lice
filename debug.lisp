;;; lice debugging facilities

(in-package :lice)

(defun re-op-lice (op)
  "Perform an asdf operation on :lice and capture the output in a
buffer."
  (with-current-buffer (get-buffer-create "*lice-reload*")
    (erase-buffer)
    (insert 
     (with-output-to-string (s)
       (let ((*debug-io* s)
	     (*error-output* s)
	     (*standard-output* s))
	 (asdf:oos op :lice)))))
  (display-buffer "*lice-reload*"))

(defcommand recompile-lice ()
  (re-op-lice 'asdf:compile-op))

(defcommand reload-lice ()
  (re-op-lice 'asdf:load-op))

(provide :lice-0.1/debug)
