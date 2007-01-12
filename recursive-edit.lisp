;;; Implement the recursive edit.

(in-package :lice)

(defvar *recursive-edit-depth* 0
  "The current recursive-edit depth.")

;; TODO: Restore the window/buffer layout
(defun recursive-edit ()
  (let* ((*recursive-edit-depth* (1+ *recursive-edit-depth*))
	 ;; reset the command keys for the recursive edit
	 (*this-command-keys* nil)
	 (ret (catch 'exit
		(with-lice-debugger
		    (loop 
		       (frame-render (selected-frame))
		       (next-event))))))
    ;; return the ret val.
    (dformat +debug-v+ "ret ~a~%" ret)
    (when ret
      (signal 'quit))))

(provide :lice-0.1/recursive-edit)
