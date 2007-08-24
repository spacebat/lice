(in-package :lice)

(defcustom *mode-require-final-newline* t
  "Whether to add a newline at end of file, in certain major modes.
Those modes set `require-final-newline' to this value when you enable them.
They do so because they are often used for files that are supposed
to end in newlines, and the question is how to arrange that.

A value of t means do this only when the file is about to be saved.
A value of `visit' means do this right after the file is visited.
A value of `visit-save' means do it at both of those times.
Any other non-nil value means ask user whether to add a newline, when saving.

nil means do not add newlines.  That is a risky choice in this variable
since this value is used for modes for files that ought to have final newlines.
So if you set this to nil, you must explicitly check and add
a final newline, whenever you save a file that really needs one."
  :type '(choice (const :tag "When visiting" visit)
		 (const :tag "When saving" t)
		 (const :tag "When visiting or saving" visit-save)
		 (const :tag "Don't add newlines" nil)
		 (other :tag "Ask each time" ask))
  :group 'editing-basics
  :version "22.1")

(defcustom-buffer-local *require-final-newline* nil
  "Whether to add a newline automatically at the end of the file.

A value of t means do this only when the file is about to be saved.
A value of `visit' means do this right after the file is visited.
A value of `visit-save' means do it at both of those times.
Any other non-nil value means ask user whether to add a newline, when saving.
nil means don't add newlines.

Certain major modes set this locally to the value obtained
from `mode-require-final-newline'."
  :type '(choice (const :tag "When visiting" visit)
		 (const :tag "When saving" t)
		 (const :tag "When visiting or saving" visit-save)
		 (const :tag "Don't add newlines" nil)
		 (other :tag "Ask each time" ask))
  :group 'editing-basics)

(defun format-filename (filename)
  (declare (type pathname filename))
  (format nil "~a~@[.~a~]" 
	  (pathname-name filename)
	  (pathname-type filename)))

(defun slurp-file (filename)
  "Return the contents of FILENAME as a string."
  (declare (type pathname filename))
  (with-open-file (in filename)
    ;; Note the 1+ is to leave a 1 character gap, because a buffer
    ;; can't have a 0 length gap.
    (let* ((str (make-array (1+ (file-length in)) :element-type 'character)))
      (read-sequence str in)
      str)))

(defun make-file-buffer (filename)
  "Assumes filename has been verified to exist and is a file."
  ;; load the file, put it in a buffer
  (declare (type pathname filename))
  (let* ((data (slurp-file filename))
	 (b (make-instance 'buffer
			   :file filename 
                           :point (make-marker)
                           :mark (make-marker)
			   :data data
			   :name (format-filename filename)
			   ;; 1- because the data has been allocated with 1 extra character
			   :gap-start (1- (length data))
			   :gap-size 1 ;;(length +other-buf+)
			   :major-mode '*fundamental-mode*)))
    (set-marker (buffer-point b) 0 b)
    (set-marker (mark-marker b) 0 b)
    b))

(defun find-file-no-select (filename)
  ;; TODO: verify the file is a file (not a dir) and it exists, etc.
  (let ((pn (parse-namestring filename)))
    ;; check that the directory exists
    (unless (ensure-directories-exist pn)
      (error "dir doesn't exist"))
    (let ((b (get-buffer-create (format-filename pn))))
      (setf (buffer-file b) pn)
      (when (probe-file pn)
        (setf (buffer-data b) (slurp-file pn)
            (buffer-gap-start b) (1- (length (buffer-data b)))
            (buffer-gap-size b) 1))
      b)))

(defcommand find-file ((filename)
		       (:file "Find File: "))
  ""
  (let ((b (find-file-no-select filename)))
    (switch-to-buffer b)))

(defcommand save-buffer ()
  (let ((buffer (current-buffer)))
    (when (buffer-file buffer)
      (if (buffer-modified-p buffer)
	  (with-open-file (out (buffer-file buffer)
			       :direction :output
			       :if-exists :overwrite
			       :if-does-not-exist :create)
	    ;; write the data before the gap
	    (write-sequence (buffer-data buffer) out 
			    :start (buffer-min buffer)
			    :end (buffer-gap-start buffer))
	    ;; write the data after the gap
	    (write-sequence (buffer-data buffer) out 
			    :start (gap-end buffer)
			    :end (length (buffer-data buffer)))
	    (setf (buffer-modified-p buffer) nil)
	    (message "Wrote ~a~%" (buffer-file (current-buffer))))
	(message "(No changes need to be saved)")))))

(defun file-completions (base predicate other)
  "Return a list of possible file completions given the base file, BASE. OTHER is not used."
  (declare (ignore other))
  ;; FIXME: they need to be strings
  (let ((tester (or predicate
		    (lambda (s)
		      (string= base s :end2 (min (length base)
                                                 (length s)))))))
    (loop for elt in (mapcar 'princ-to-string (directory (merge-pathnames (make-pathname :name :wild) base)))
       when (funcall tester elt)
       collect elt)))

(defcommand load-file ((file)
                       (:file "Load file: "))
  "Load the Lisp file named FILE."
  (load file))

;;; auto save

(defun recent-auto-save-p ()
  "Return t if current buffer has been auto-saved recently.
More precisely, if it has been auto-saved since last read from or saved
in the visited file.  If the buffer has no visited file,
then any auto-save counts as \"recent\"."
  ;; FIXME: implement
  nil)

(defun set-buffer-auto-saved ()
"Mark current buffer as auto-saved with its current text.
No auto-save file will be written until the buffer changes again."
  (setf (buffer-auto-save-modified (current-buffer)) (buffer-modiff (current-buffer))))

;; FIXME: maybe this should be a slot in the buffer with the rest of the autosave slots
(define-buffer-local buffer-auto-save-file-name nil
  "Name of file for auto-saving current buffer.
If it is nil, that means don't auto-save this buffer.")

(defcustom *delete-auto-save-files* t
  "Non-nil means delete auto-save file when a buffer is saved or killed.

Note that the auto-save file will not be deleted if the buffer is killed
when it has unsaved changes."
  :type 'boolean
  :group 'auto-save)

(defun delete-auto-save-file-if-necessary (&optional force)
  "Delete auto-save file for current buffer if `delete-auto-save-files' is t.
Normally delete only if the file was written by this Emacs since
the last real save, but optional arg FORCE non-nil means delete anyway."
  (and buffer-auto-save-file-name *delete-auto-save-files*
       (not (string= (buffer-file (current-buffer)) buffer-auto-save-file-name))
       (or force (recent-auto-save-p))
       (progn
	 (handler-case
	     (delete-file buffer-auto-save-file-name)
	   (file-error () nil))
	 (set-buffer-auto-saved))))

(defcommand save-buffers-kill-emacs ()
  ;; TODO: save-some-buffers
  (kill-emacs))

;;; Key bindings

(define-key *ctl-x-map* "C-f" 'find-file)
(define-key *ctl-x-map* "C-r" 'find-file-read-only)
(define-key *ctl-x-map* "C-v" 'find-alternate-file)
(define-key *ctl-x-map* "C-s" 'save-buffer)
(define-key *ctl-x-map* "s" 'save-some-buffers)
(define-key *ctl-x-map* "C-w" 'write-file)
(define-key *ctl-x-map* "i" 'insert-file)
(define-key *esc-map* "~" 'not-modified)
(define-key *ctl-x-map* "C-d" 'list-directory)
(define-key *ctl-x-map* "C-c" 'save-buffers-kill-emacs)
(define-key *ctl-x-map* "C-q" 'toggle-read-only)

(define-key *ctl-x-4-map* "f" 'find-file-other-window)
(define-key *ctl-x-4-map* "r" 'find-file-read-only-other-window)
(define-key *ctl-x-4-map* "C-f" 'find-file-other-window)
(define-key *ctl-x-4-map* "b" 'switch-to-buffer-other-window)
(define-key *ctl-x-4-map* "C-o" 'display-buffer)

(define-key *ctl-x-5-map* "b" 'switch-to-buffer-other-frame)
(define-key *ctl-x-5-map* "f" 'find-file-other-frame)
(define-key *ctl-x-5-map* "C-f" 'find-file-other-frame)
(define-key *ctl-x-5-map* "r" 'find-file-read-only-other-frame)
(define-key *ctl-x-5-map* "C-o" 'display-buffer-other-frame)


(provide :lice-0.1/files)
