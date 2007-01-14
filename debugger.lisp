;;; An interactive debugger for lice

(in-package #:lice)

(define-major-mode debugger-mode
    (:name "Debugger"
     :map (let ((m (make-sparse-keymap)))
	    (define-key m (kbd "q") 'debugger-invoke-top-level-restart)
	    m))
  "debugger mode"
  ;; empty init
  )

(defun enter-debugger (condition old-debugger-value)
  "Create a debugger buffer, print the error and any active restarts."
  (declare (ignore old-debugger-value))
  ;; make sure we're not in the minibuffer
  (select-window (first (frame-window-list *current-frame*)))
  (pop-to-buffer (get-buffer-create "*debugger*"))
  (erase-buffer)
  (set-major-mode debugger-mode)
  (insert (format nil "Debugger~%~a~%~%~a~%~{~a~%~}" (backtrace-as-string) condition (compute-restarts)))
  (recursive-edit)
  ;; if we exit the recursive edit we'll fall into the regular debugger.
  )

(defmacro with-lice-debugger (&body body)
  `(let ((*debugger-hook* #'enter-debugger))
     ,@body))

(defcommand debugger-invoke-top-level-restart ()
  (when (get-buffer "*debugger*")
    (kill-buffer (get-buffer "*debugger*")))
  (invoke-restart (find-restart 'recursive-edit-top-level)))
