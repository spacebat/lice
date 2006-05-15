;;; This is a cheap, pigeon lisp mode. One Day, it'll be replaced with
;;; something amazing.

(in-package :lice)

(define-major-mode lisp-interaction-mode
  (:name "Lisp Interaction"
   :map (let ((m (make-sparse-keymap)))
	  (define-key m (make-instance 'key :char #\j :control t) 'eval-print-last-sexp)
	  m))
  "Lisp mode"
  ;; empty init
  )

(defun find-matching-paren-forward ()
  "Move the point to the matching paren."
  (let ((depth 0))
    (loop
     (case (char-after)
       (#\(
	(incf depth))
       (#\)
	(decf depth)))
     (forward-char)
     (when (zerop depth)
       (return)))))

(defun find-matching-paren-backward ()
  "Move the point to the matching paren."
  (let ((depth 0))
    (loop
     (case (char-before)
       (#\)
	(incf depth))
       (#\(
	(decf depth)))
     (backward-char 1)
     (when (zerop depth)
       (return)))))

(defun find-matching-quote-forward ()
  (let ((escaped nil))
    (loop
     (case (char-after)
       (#\\
	(setf escaped t))
       (#\"
	(unless escaped
	  (forward-char)
	  (return)))
       (t
	(setf escaped nil)))
     (forward-char))))

(defun find-matching-quote-backward ()
  (let ((maybe nil))
    (loop
     (case (char-before)
       (#\\
	(when maybe
	  (setf maybe nil)))
       (#\"
	(setf maybe t))
       (t
	(when maybe
	  (return))))
     (backward-char))))

;;(defun find-matching

(define-condition sexp-scan (lice-condition)
  ())

(defcommand forward-sexp ()
  (labels ((forward ()
	     (case (char-after)
	       (#\(
		(find-matching-paren-forward))
	       ;;(#\"
	       ((#\' #\` #\Space #\Newline)
		(forward-char)
		(forward))
	       (#\)
		(signal 'sexp-scan))
	       (#\"
		(forward-char)
		(find-matching-quote-forward))
	       (t
		(loop until (or (find (char-after) (format nil "~% )('`"))
				(>= (point) (zv)))
                   do (forward-char))))))
    (let ((p (point)))
      (handler-case (forward)
        (end-of-buffer (c)
          (declare (ignore c))
          (goto-char p)
          (signal 'sexp-scan))))))

(defcommand backward-sexp ()
  (labels ((back ()
	     (case (char-before)
	       (#\)
		(find-matching-paren-backward)
		(when (find (char-before) "'`")
		  (backward-char 1)))
	       ((#\Newline #\Space)
		(backward-char 1)
		(back))
	       (#\(
		(signal 'sexp-scan))
	       (#\"
		(backward-char)
		(find-matching-quote-backward))
	       (t
		(loop until (or (find (char-before) (format nil "~% ()"))
				(<= (point) (begv)))
                   do (backward-char 1))))))
    (let ((p (point)))
      (handler-case (back)
        (beginning-of-buffer (c)
          (declare (ignore c))
          (goto-char p)
          (signal 'sexp-scan))))))

(defcommand eval-last-sexp ()
  (let ((start (point))
	end)
    ;; some nice'n'gross point handling
    (backward-sexp)
    (setf end (point))
    (goto-char start)
    (handler-case (eval-echo (buffer-substring-no-properties start end))
      (error (c) (message "Eval error: ~s" c)))))

(defcommand eval-print-last-sexp ()
  (let ((start (point))
	end)
    ;; some nice'n'gross point handling
    (backward-sexp)
    (setf end (point))
    (goto-char start)
    (handler-case (eval-print (buffer-substring-no-properties start end))
      (error (c) (message "Eval error: ~s" c)))))

(defcommand lisp-interaction-mode ()
  (set-major-mode lisp-interaction-mode))

(provide :lice-0.1/lisp-mode)