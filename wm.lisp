;;; window configuration code

(in-package :lice)

(defstruct window-bk
  "A structure that stores the vital data needed to restore a window."
  x y w h seperator top bpoint buffer)

(defstruct frame-bk
  current-window
  window-tree)

(defun current-window-configuration (&optional (frame (selected-frame)))
  "Return the vital components of the window configuration of FRAME."
  (let ((frame frame)
	cw)
    (labels ((bk-window (window)
			(with-slots (x y w h seperator top bpoint buffer) window
			  (let ((bk (make-window-bk :x x
						    :y y
						    :w w
						    :h h
						    :seperator seperator
						    :top (copy-marker top)
						    :bpoint (copy-marker bpoint)
						    :buffer buffer)))
			    ;; record the current window
			    (when (eq window (frame-current-window frame))
			      (setf cw bk))
			    bk)))
	     (dup-tree (tree)
		       (cond ((typep tree 'window)
			      (bk-window tree))
			     (t (list (dup-tree (first tree))
				      (dup-tree (second tree)))))))
      (make-frame-bk :window-tree (dup-tree (first (frame-window-tree frame)))
		     :current-window cw))))
      
(defun set-window-configuration (configuration &optional (frame (selected-frame)))
  "CONFIGURATION must have been generated from FRAME. Otherwise, Bad Things could happen."
  (let ((frame frame)
	cw)
    (labels ((restore-window (bk)
			     (let ((w (make-window :frame frame
						   :x (window-bk-x bk)
						   :y (window-bk-y bk)
						   :cols (window-bk-w bk)
						   :rows (window-bk-h bk)
						   :buffer (window-bk-buffer bk)
						   :top (window-bk-top bk)
						   :bpoint (window-bk-bpoint bk))))
			       (unless (get-buffer (window-bk-buffer bk))
				 ;; default to scratch for deleted buffers
				 (let ((scratch (get-buffer-create "*scratch*")))
				   (setf (window-buffer w) scratch
					 (window-top w) (make-marker 0 scratch)
					 (window-bpoint w) (make-marker 0 scratch))))
			       (setf (window-seperator w) (window-bk-seperator bk))
			       (when (eq bk (frame-bk-current-window configuration))
				 (setf cw w))
			       w))
	     (restore-tree (tree)
			   (cond ((typep tree 'window-bk)
				  (restore-window tree))
				 (t (list (restore-tree (first tree))
					  (restore-tree (second tree)))))))
      (setf (frame-window-tree frame) (cons (restore-tree (frame-bk-window-tree configuration))
					    (cdr (frame-window-tree frame)))
	    (frame-current-window frame) cw)
      (set-buffer (window-buffer cw)))))

(defmacro save-window-excursion (&body body)
  "Execute body, preserving window sizes and contents.
Restore which buffer appears in which window, where display starts,
and the value of point and mark for each window.
Also restore the choice of selected window.
Also restore which buffer is current.
**Does not restore the value of point in current buffer."
  (let ((wc (gensym "WINDOW-CONFIGURATION")))
    `(let ((,wc (current-window-configuration)))
       (unwind-protect 
	   (progn 
	     ,@body)
	 (set-window-configuration ,wc)))))

(provide :lice-0.1/wm)
