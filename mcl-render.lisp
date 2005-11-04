(in-package :lice)

(defclass mcl-window (ccl:window)
  ())

(defclass mcl-frame (frame)
  ((mcl-window :type window :initarg :mcl-window :accessor mcl-frame-window)
   (double-buffer :type (array ccl:static-text-dialog-item 1) :initarg :double-buffer :accessor frame-double-buffer :documentation
		  "The display double buffer. This structure is compared to
the characters we want to blit. Only differences are sent to the video
hardware.")
   (2d-double-buffer :type (array ccl:static-text-dialog-item 2) :initarg :2d-double-buffer :accessor frame-2d-double-buffer :documentation
		     "Displaced from DISPLAY. This array is divided into rows and columns.")
   ))

(defmethod frame-start-render ((frame mcl-frame))
  )

(defmethod frame-end-render ((frame mcl-frame))
  (ccl:event-dispatch))


(defun window-move-cursor (window x y)
  )

(defmethod frame-move-cursor ((frame mcl-frame) win x y)
  (window-move-cursor win x y))

(defun putch (ch x y window frame)
  (ccl:set-dialog-item-text (aref (frame-2d-double-buffer frame) (+ y (window-y window)) (+ x (window-x window)))
                            (string ch)))

(defun putstr (s x y w frame)
  (loop for i from 0 below (length s)
	for j from x by 1
	;;if (char/= (aref (window-2d-display w) y j) (aref s i))
	do (putch (char s i) j y w frame)))

(defun clear-line-between (w y start end frame)
  "insert LEN spaces from START on line Y."
  (window-move-cursor w start y)
  ;; FIXME: this could be done faster
  (loop for i from start to end
	do (putch #\Space i y w frame)))

(defun clear-to-eol (y start window frame)
  (declare (type window window)
	   (type fixnum y start))
  (let ((display (frame-2d-double-buffer frame))
	(linear (frame-double-buffer frame)))
    (clear-line-between window y start (1- (window-width window)) frame)
    ;; draw the seperator
    (when (window-seperator window)
      (putch #\| (+ (window-x window) (1- (window-width window t))) y window frame))))

(defun turn-on-attributes (buffer point)
  )

(defmethod window-render (w (frame mcl-frame))
  "Render a window."
  (let ((p (buffer-char-to-aref (window-buffer w) (marker-position (window-top w))))
	;; current point in buffer buffer
	(bp (marker-position (window-top w)))		
	(buf (window-buffer w))
	;; The cursor point in the buffer. When the buffer isn't
	;; current, then use the window's backup of the point.
	(point (window-point w))
	cursor-x
	cursor-y
	(cache-size (length (lc-cache (window-cache w))))
	(linear (frame-double-buffer frame))
	(display (frame-2d-double-buffer frame)))
    ;; Special case: when the buffer is empty
    (if (= (buffer-size (window-buffer w)) 0)
	(progn 
	  (dotimes (y (window-height w))
	    (clear-to-eol y 0 w frame))
	  (setf cursor-x 0
		cursor-y 0))
      (let ((end (loop named row
		       for y below (window-height w)
		       for line from (window-top-line w) below cache-size
		       ;; return the last line, so we can erase the rest
		       finally (return-from row y)
		       ;; go to the next line
		       do (let* ((line-end (cache-item-end (item-in-cache w line)))
				 (line-start (cache-item-start (item-in-cache w line)))
 				 (next-prop (next-single-property-change line-start :face (window-buffer w) line-end)))
			    (setf bp (cache-item-start (item-in-cache w line))
				  p (buffer-char-to-aref (window-buffer w) bp))
			    ;; setup the display properties.
			    (turn-on-attributes (window-buffer w) bp)
			    (loop named col
				for x below (window-width w) do
				(progn
				  ;; Skip the gap
				  (when (= p (buffer-gap-start buf))
				    (incf p (buffer-gap-size buf)))
				  ;; Record point position on screen
				  (when (eq bp point)
				    (setf cursor-x x)
				    (setf cursor-y y))
				  (when (or (> bp line-end)
					    (>= p (length (buffer-data buf))))
				    ;; Check if the rest of the line is blank
				    (clear-to-eol y x w frame)
				    (return-from col))
				  ;; update attributes
				  (when (>= bp next-prop)
				    (turn-on-attributes (window-buffer w) bp))
				  (let ((ch (elt (buffer-data buf) p)))
				    ;; Update display
				    (cond ((char= ch #\Newline)
					   (putch #\Space x y w frame))
					  (t
					   (putch ch x y w frame)))
				    ;; go to the next character in the buffer
				    (incf p)
				    (incf bp))))))))
	;; Check if the bottom of the window needs to be erased.
	(when (< end (1- (window-height w)))
	  (loop for i from end below (window-height w) do
		(clear-to-eol i 0 w frame)))))
    ;; Update the mode-line if it exists. FIXME: Not the right place
    ;; to update the mode-line.
    (when (buffer-mode-line (window-buffer w))
      (update-mode-line (window-buffer w))
      (putstr (truncate-mode-line (window-buffer w) (window-width w))
	      0 (window-height w nil) w frame)
      ;; don't forget the seperator on the modeline line
      (when (window-seperator w)
	(putch #\| (+ (window-x w) (window-width w)) (window-height w) w frame)))
    (reset-line-state w)
    ;; Set the cursor at the right spot
    (values cursor-x cursor-y)))

(defmethod frame-read-event ((frame mcl-frame))
  (sleep 10)
  (make-instance 'key
                 :char #\a))

(defvar *mcl-key-list* nil
  "List of keys pressed.")

(defmethod view-key-event-handler :after ((w mcl-window) ch)
  (format t "LICE keypress: ~a~%" ch)
  (push ch *mcl-key-list*))

(defun make-default-mcl-frame (buffer)
  (let* ((lines 25)
	 (height (1- lines))
	 (cols 80)
	 (l (make-array (* lines cols)
			:element-type 'ccl:static-text-dialog-item))
	 (d (make-array (list lines cols)
			:element-type 'ccl:static-text-dialog-item
			:displaced-to l :displaced-index-offset 0))
	 (w (make-window :x 0 :y 0 :cols cols :rows height :buffer buffer))
	 (mb (make-minibuffer-window lines cols))
	 (frame (make-instance 'mcl-frame
			       :width cols
			       :height lines
			       :window-tree (list w mb)
			       :current-window w
			       :minibuffer-window mb
			       :double-buffer l
			       :2d-double-buffer d
                               :mcl-window (make-instance 'mcl-window
                                                          :view-size (ccl:make-point (* cols 11) (* lines 16))))))
    (dotimes (i (* cols lines))
      (setf (aref (frame-double-buffer frame) i) (make-instance 'ccl:static-text-dialog-item
                                                            :view-font '("Monaco" 12)
                                                            :view-container (mcl-frame-window frame)
                                                            :view-position (ccl:make-point (* (multiple-value-bind (q r) (truncate i cols) r) 11) (* (truncate i cols) 16))
                                                            :dialog-item-text (make-string 1 :initial-element #\-))))
    (setf (window-frame w) frame
	  (window-frame mb) frame)
    frame))

