;;; frame rendering routines

(in-package "LICE")

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
			      (when (eq tree (frame-selected-window frame))
				(setf win tree cursor-x x cursor-y y))))
			   (t (cons (render (car tree))
				    (render (cdr tree)))))))
      (frame-start-render frame)
      (render (frame-window-tree frame))
      (when (and win cursor-x cursor-y)
	(frame-move-cursor frame win cursor-x cursor-y))
      (frame-end-render frame))))
