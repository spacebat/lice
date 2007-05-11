;;; Implement the recursive edit.

(in-package "LICE")

(defvar *recursive-edit-depth* 0
  "The current recursive-edit depth.")

;; TODO: Restore the window/buffer layout
(defun recursive-edit ()
  (let* ((*recursive-edit-depth* (1+ *recursive-edit-depth*))
	 ;; reset the command keys for the recursive edit
	 (*this-command-keys* nil)
         ;; restore the last command
         (*last-command* *last-command*)
	 (ret (catch 'exit
		;;(with-lice-debugger
		    (loop 
		       (frame-render (selected-frame))
		       (top-level-next-event)))))
    ;; return the ret val.
    (dformat +debug-v+ "ret ~a~%" ret)
    (when ret
      (signal 'quit))))

(provide :lice-0.1/recursive-edit)
