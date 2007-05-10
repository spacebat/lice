;;; An interactive debugger for lice

(in-package "LICE")

(defvar *debug-on-error* t
  "Non-nil means enter the debugger if an unhandled error is signaled.")

(defvar *debug-on-quit* nil
  "Non-nil means enter the debugger if quit is signaled (C-g, for example).")

(defvar *debugger-mode*
  (make-instance 'major-mode
                 :name "Debugger"
                 :map  (let ((m (make-sparse-keymap)))
                         (define-key m (kbd "q") 'debugger-invoke-top-level-restart)
                         m)))
(defun debugger-mode ()
  "See `*debugger-mode*'"
  (set-major-mode '*debugger-mode*))

(defun enter-debugger (condition old-debugger-value)
  "Create a debugger buffer, print the error and any active restarts."
  (declare (ignore old-debugger-value))
  ;; maybe continue a sigint
  (when (and (typep condition 'user-break)
             (or *inhibit-quit*
                 *waiting-for-input*))
    (setf *quit-flag* t)
    (continue))
  ;; make sure we're not in the minibuffer
  (select-window (first (frame-window-list (selected-frame))))
  (pop-to-buffer (get-buffer-create "*debugger*"))
  (erase-buffer)
  (set-major-mode '*debugger-mode*)
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

(defcommand toggle-debug-on-error ()
  "Toggle whether to enter Lisp debugger when an error is signaled.
In an interactive call, record this option as a candidate for saving
by \"Save Options\" in Custom buffers."
  (setf *debug-on-error* (not *debug-on-error*)))

(defcommand toggle-debug-on-quit ()
  "Toggle whether to enter Lisp debugger when C-g is pressed.
In an interactive call, record this option as a candidate for saving
by \"Save Options\" in Custom buffers."
  (setf *debug-on-quit* (not *debug-on-quit*)))
