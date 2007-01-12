(in-package :lice)

(defvar *frame-list* nil
  "List of frames lice frames.")

;; XXX: This is only temporary
(defvar *current-frame* nil
  "The frame that accepts input.")

(defun selected-frame ()
  "Return the frame that is now selected."
  *current-frame*)

(defclass frame ()
  ((window-tree :type (or list window) :initarg :window-tree :accessor frame-window-tree)
   (width :type fixnum :initarg :width :accessor frame-width)
   (height :type fixnum :initarg :height :accessor frame-height)
   (minibuffer-window :type window :initarg :minibuffer-window :accessor frame-minibuffer-window)
   (minibuffers-active :type fixnum :initform 0 :initarg minibuffers-active :accessor frame-minibuffers-active)
   (current-window :type window :initarg :current-window :accessor frame-current-window))
  (:documentation "A Lice frame is super cool."))

(defun set-frame-minibuffer (frame minibuffer)
  "Make MINIBUFFER the minibuffer for FRAME."
  (setf	(window-buffer (frame-minibuffer-window frame)) minibuffer))

;; The defmethods are found in the *-render.lisp files
(defgeneric frame-start-render (frame)
  (:documentation "Do any setup we need before we beginning rendering the frame."))

(defgeneric frame-end-render (frame)
  (:documentation "Do any cleanup or refreshing after the frame is rendered."))

;; the defmethods are found in the *-render.lisp files
(defgeneric window-render (window frame)
  (:documentation "Render the window in the given frame."))

(defgeneric frame-read-event (frame)
  (:documentation "Read a keyboard event for the specified frame."))

(defgeneric frame-move-cursor (frame window x y)
  (:documentation "Move the cursor to the X,Y location in WINDOW on the frame, FRAME."))

(defun frame-render (frame)
  "Render a frame."
  (let (cursor-x cursor-y win)
    (labels ((render (tree)
		     (cond ((null tree) nil)
			   ((atom tree) 
			    ;; reset the cache
			    (window-reset-cache tree)
			    ;; Figure out what part to display
			    (window-framer tree 
					   (window-point tree)
					   (truncate (window-height tree) 2))
			    (dformat +debug-vvv+ "after framer: ~a~%"
				     (lc-cache (window-cache tree)))
			    ;; display it
			    (multiple-value-bind (x y) (window-render tree frame)
			      (when (eq tree (frame-current-window frame))
				(setf win tree cursor-x x cursor-y y))))
			   (t (cons (render (car tree))
				    (render (cdr tree)))))))
      (frame-start-render frame)
      (render (frame-window-tree frame))
      (when (and win cursor-x cursor-y)
	(frame-move-cursor frame win cursor-x cursor-y))
      (frame-end-render frame))))


(defun resize-window (window amount &optional (dir :height))
  "grow or shrink window, resizing dependant windows as well."
  (declare (ignore window amount dir))
;;   (let* ((frame (frame-window-tree (frame-for-window window)))
;; 	 (sibling (tree-sibling frame window)))
;;     )
  )

(defun current-buffer ()
  "Return the current buffer."
  ;; FIXME: maybe this should just return *current-buffer*
  (or *current-buffer*
      ;;(window-buffer (frame-current-window (selected-frame)))
      ))

(defun active-minibuffer-window ()
  "Return the currently active minibuffer window or nil if there isn't
one."
  (let ((frame (selected-frame)))
    (unless (zerop (frame-minibuffers-active frame))
      (frame-minibuffer-window frame))))

(defun frame-window-list (frame &optional minibuf)
  "Return the list of windows in FRAME. If MINIBUF is true then include the minibuffer window."
;;   (declare (type frame frame))
  ;; FIXME: The reason we need to pass MB into flatten is because movitz can't "lend optional right now"
  (labels ((flatten (tree mb)
	     (if (atom tree)
		 (unless (and (typep tree 'minibuffer-window)
			      (not mb))
		   (list tree))
	       (nconc (flatten (first tree) mb)
                      (flatten (second tree) mb)))))
    (flatten (frame-window-tree frame) minibuf)))

(defun window-tree-find-if (fn tree &optional minibuf)
  "depth first search the tree. Return the element that satisfies FN."
  (cond ((listp tree)
	 (loop for i in tree
	       thereis (window-tree-find-if fn i minibuf)))
	((typep tree 'minibuffer-window)
	 (when (and minibuf
		    (funcall fn tree))
	   tree))
	(t 
	 (when (funcall fn tree)
	   tree))))
	     
(defun replace-window-in-frame-tree (window new)
  (labels ((doit (tree window new)
	     (let ((p (position window tree)))
	       (if p
		   (setf (nth p tree) new)
		 (loop for w in tree
		       until (and (listp w)
				  (doit w window new)))))))
    (doit (frame-window-tree (window-frame window))
	  window
	  new)))

;; (defun replace-window-parent-in-frame-tree (window new)
;;   (labels ((doit (tree parent window new)
;; 	     (when (listp tree)
;; 	       (let (loop for i in (remove-if-not 'listp tree)
;; 		      thereis (find window i))
;; 		   (parent
;; 		   (
;; 		   )))
;;     (doit (frame-window-tree (window-frame window))
;; 	  window
;; 	  new)))

(defun split-window (&optional (window (get-current-window)) size horflag)
  (when (typep window 'minibuffer-window)
    (error "Attempt to split minibuffer window"))
  (when (null size)
    (setf size (if horflag
		   (ceiling (window-width window t) 2)
		 (ceiling (window-height window t) 2))))
  (let (new)
    (if horflag
	(progn
	  (when (< size *window-min-width*)
	    (error "Window width ~a too small (after splitting)" size))
	  ;; will the other window be too big?
	  (when (> (+ size *window-min-width*)
		   (window-width window t))
	    (error "Window width ~a too small (after splitting)" (- (window-width window t) size)))
	  (setf new (make-window :x (+ (window-x window) size)
				 :y (window-y window)
				 :cols (- (window-width window t) size)
				 :rows (window-height window t)
				 :buffer (window-buffer window)
				 :frame (window-frame window))
		(window-seperator new) (window-seperator window)
		(window-seperator window) t
		(slot-value window 'w) size)
	  ;;(update-window-display-arrays window)
	  )
      (progn
	(when (< size *window-min-height*)
	  (error "Window height ~a too small (after splitting)" size))
	;; will the other window be too big?
	(when (> (+ size *window-min-height*)
		 (window-height window t))
	  (error "Window width ~a too small (after splitting)" (- (window-height window t) size)))
	(setf new (make-window :x (window-x window)
			       :y (+ (window-y window) size)
			       :cols (window-width window t)
			       :rows (- (window-height window t) size)
			       :buffer (window-buffer window)
			       :frame (window-frame window))
	      (window-seperator new) (window-seperator window)
	      (slot-value window 'h) size)
	;;(update-window-display-arrays window)
	))
    (replace-window-in-frame-tree window (list window new))
    new))

(defun next-window (window &optional minibuf)
  "Return next window after WINDOW in canonical ordering of windows.
FIXME: make this the same as Emacs' next-window."
  (let* ((frame (window-frame window))
	 (tree (frame-window-tree frame))
	 bit
	 ;; when we find WINDOW, set BIT to T and return the next window.
	 (w (window-tree-find-if (lambda (w)
				   (cond (bit w)
					 ((eq w window)
					  (setf bit t)
					  nil)))
				 tree
				 (and minibuf (> (frame-minibuffers-active frame) 0)))))
    ;; if we didn't find the next one, maybe it's the first one
    (if (not w)
	(let ((other (window-tree-find-if #'identity tree)))
	  (unless (eq window other)
	    other))
      w)))


(defun select-window (window &optional norecord)
  "Select WINDOW.  Most editing will apply to WINDOW's buffer.
If WINDOW is not already selected, also make WINDOW's buffer current.
Also make WINDOW the frame's selected window.
Optional second arg NORECORD non-nil means
do not put this buffer at the front of the list of recently selected ones.

**Note that the main editor command loop
**selects the buffer of the selected window before each command."
  (declare (ignore norecord))
  ;; FIXME: get NORECORD working
  (window-save-point (get-current-window))
  ;; FIXME: this doesn't make sure window-frame is current.
  (setf (frame-current-window (window-frame window)) window)
  (set-buffer (window-buffer window))
  (window-restore-point window))

(defun display-buffer (buffer &optional not-this-window frame)
  "Make BUFFER appear in some window but don't select it.
BUFFER can be a buffer or a buffer name.
If BUFFER is shown already in some window, just use that one,
unless the window is the selected window and the optional second
argument NOT-THIS-WINDOW is non-nil (interactively, with prefix arg).
**If `pop-up-frames' is non-nil, make a new frame if no window shows BUFFER.
**Returns the window displaying BUFFER.
**If `display-buffer-reuse-frames' is non-nil, and another frame is currently
**displaying BUFFER, then simply raise that frame."
  (declare (ignore frame))
  (setf buffer (get-buffer buffer))
  (let* ((cw (get-current-window))
	 (w (or (window-tree-find-if (lambda (w)
				       (and (not (and not-this-window
						      (eq w cw)))
					    (eq (window-buffer w) buffer)))
				     (frame-window-tree (selected-frame)))
		(next-window cw)
		(split-window cw))))
    (set-window-buffer w buffer)
    (window-restore-point w)
    w))

(defun pop-to-buffer (buffer &optional other-window norecord)
  "Select buffer BUFFER in some window, preferably a different one.
If `pop-up-windows' is non-nil, windows can be split to do this.
If optional second arg OTHER-WINDOW is non-nil, insist on finding another
window even if BUFFER is already visible in the selected window.
This uses the function `display-buffer' as a subroutine; see the documentation
of `display-buffer' for additional customization information.

**Optional third arg NORECORD non-nil means
**do not put this buffer at the front of the list of recently selected ones."
  (declare (ignore norecord))
  ;; FIXME: honour NORECORD
  (setf buffer (if buffer
		   (or (get-buffer buffer)
		       (progn
			 (get-buffer-create buffer)))
			 ;; FIXME: (set-buffer-major-mode buffer)
		 (other-buffer (current-buffer))))
  (select-window (display-buffer buffer other-window)))

(defun sit-for (seconds &optional nodisp)
  "Perform redisplay, then wait for seconds seconds or until input is available.
seconds may be a floating-point value, meaning that you can wait for a
fraction of a second.
 (Not all operating systems support waiting for a fraction of a second.)
Optional arg nodisp non-nil means don't redisplay, just wait for input.
Redisplay is preempted as always if input arrives, and does not happen
if input is available before it starts.
Value is t if waited the full time with no input arriving."
  (declare (ignore seconds nodisp))
  ;; FIXME: actually sleep
  (frame-render (selected-frame)))

(provide :lice-0.1/frame)
