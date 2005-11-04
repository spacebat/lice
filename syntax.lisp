;;; Cheap syntax functions

(in-package :lice)

(defparameter +word-constituents+ "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789")

(defcommand forward-word ((n) :prefix)
  "Move point forward ARG words (backward if ARG is negative).
Normally returns t.
If an edge of the buffer or a field boundary is reached, point is left there
and the function returns nil.  Field boundaries are not noticed if
`inhibit-field-text-motion' is non-nil."
  (labels ((isaword (c)
	     (find c +word-constituents+ :test #'char=)))
    (let ((buffer (current-buffer)))
      (cond ((> n 0)
	     (gap-move-to buffer (buffer-point-aref buffer))
	     ;; do it n times
	     (loop for i from 0 below n
		   while (let (p1 p2)
			   ;; search forward for a word constituent
			   (setf p1 (position-if #'isaword (buffer-data buffer) 
						 :start (buffer-point-aref buffer)))
			   ;; search forward for a non word constituent
			   (when p1
			     (setf p2 (position-if (complement #'isaword) (buffer-data buffer) :start p1)))
			   (if p2
			       (goto-char (buffer-aref-to-char buffer p2))
			     (goto-char (point-max)))
			   p2)))
	    ((< n 0)
	     (setf n (- n))
	     (gap-move-to buffer (buffer-point-aref buffer))
	     ;; do it n times
	     (loop for i from 0 below n
		   for start = (buffer-gap-start buffer) then (buffer-point-aref buffer)
		   while (let (p1 p2)
			   ;; search backward for a word constituent
			   (setf p1 (position-if #'isaword (buffer-data buffer) 
						 :from-end t
						 :end start))
			   ;; search backward for a non word constituent
			   (when p1
			     (setf p2 (position-if (complement #'isaword) (buffer-data buffer) :from-end t :end p1)))
			   (if p2
			       (goto-char (1+ (buffer-aref-to-char buffer p2)))
			     (goto-char (point-min)))
			   p2)))))))

(defcommand backward-word ((n) :prefix)
  "Move point forward ARG words (backward if ARG is negative).
Normally returns t.
If an edge of the buffer or a field boundary is reached, point is left there
and the function returns nil.  Field boundaries are not noticed if
`inhibit-field-text-motion' is non-nil."
  (forward-word (- n)))

(defcommand kill-word ((arg)
		       :prefix)
  "Kill characters forward until encountering the end of a word.
With argument, do this that many times."
  (kill-region (point) (progn (forward-word arg) (point))))

(defcommand backward-kill-word ((arg)
				:prefix)
  "Kill characters backward until encountering the end of a word.
With argument, do this that many times."
  (kill-word (- arg)))
