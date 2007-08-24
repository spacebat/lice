;;; echo area related function. this stuff is in xdisp.c in emacs

(in-package "LICE")

(defun ensure-echo-area-buffers ()
  "Make sure echo area buffers in `echo_buffers' are live.
   If they aren't, make new ones."
  (unless (and (bufferp (frame-echo-area-current (selected-frame)))
               (buffer-live-p (frame-echo-area-current (selected-frame))))
    (let ((buf (get-buffer-create " *Echo Area 0*")))
      (setf (frame-echo-area-current (selected-frame)) buf
            (buffer-local 'truncate-lines buf) nil)))
  ;; bleh, duplicate code
  (unless (and (bufferp (frame-echo-area-pren (selected-frame)))
               (buffer-live-p (frame-echo-area-prev (selected-frame))))
    (let ((buf (get-buffer-create " *Echo Area 1*")))
      (setf (frame-echo-area-prev (selected-frame)) buf
            (buffer-local 'truncate-lines buf) nil))))
