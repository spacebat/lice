;; TTY rendering routines

(in-package :lice)

(defclass clisp-frame (frame)
  ((window-stream :type window-stream :initarg :window-stream :accessor frame-window-stream)
   (double-buffer :type (array character 1) :initarg :double-buffer :accessor frame-double-buffer :documentation
		  "The display double buffer. This structure is compared to
the characters we want to blit. Only differences are sent to the video
hardware.")
   (2d-double-buffer :type (array character 2) :initarg :2d-double-buffer :accessor frame-2d-double-buffer :documentation
		     "Displaced from DISPLAY. This array is divided into rows and columns.")))

(defmethod frame-start-render ((frame clisp-frame))
  )

(defmethod frame-end-render ((frame clisp-frame))
  ;; (screen:window-refresh (frame-window-stream frame))
  )

;; This has to be defined (it should be a generic function)
(defun window-move-cursor (window x y window-stream)
  (screen:set-window-cursor-position window-stream (+ y (window-y window)) (+ x (window-x window))))

(defmethod frame-move-cursor ((frame clisp-frame) win x y)
  (window-move-cursor win x y (frame-window-stream frame)))

(defun putch (ch x y window frame)
  (when (char/= (aref (frame-2d-double-buffer frame) (+ y (window-y window)) (+ x (window-x window))) ch)
    (window-move-cursor window x y (frame-window-stream frame))
    (write-char ch (frame-window-stream frame))
    (setf (aref (frame-2d-double-buffer frame) (+ y (window-y window)) (+ x (window-x window))) ch)))

(defun putstr (s x y w frame)
  (loop for i from 0 below (length s)
	for j from x by 1
	do (putch (aref s i) j y w frame)))

(defun line-height (buffer p)
  "Return the height of the line starting at p."
  (declare (ignore buffer p)))

(defun clear-line-between (w y start end frame)
  "insert LEN spaces from START on line Y."
  (loop for i from start to end
	do (putch #\Space i y w frame)))

;; Helper function for window-render
(defun clear-to-eol (y start window frame)
  (declare (type window window)
	   (type fixnum y start))
  (let ((display (frame-2d-double-buffer frame))
	(linear (frame-double-buffer frame)))
    (clear-line-between window y start (1- (window-width window)) frame)
    ;; draw the seperator
    (when (window-seperator window)
      (putch #\| (+ (window-x window) (1- (window-width window t))) y window frame))))
	      
(defun turn-on-attributes (buffer point frame)
  "Given the buffer and point, turn on the appropriate colors based on
the text properties present."
  ;; These are hardcoded for now
  (if (get-text-property point :face buffer)
      (screen:highlight-on (frame-window-stream frame))
    (screen:highlight-off (frame-window-stream frame))))

(defmethod window-render (w (frame clisp-frame))
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
			    (turn-on-attributes (window-buffer w) bp frame)
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
				    (turn-on-attributes (window-buffer w) bp frame))
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

;;; keyboard stuff

(defmethod frame-read-event ((frame clisp-frame))
  (let* ((input (read-char EXT:*KEYBOARD-INPUT*));; (input (screen::read-keyboard-char (frame-window-stream frame)))
	 (ch (if (sys::input-character-char input) 
				 (char-downcase (sys::input-character-char input))
			       (char-downcase (sys::input-character-key input))))
	 meta)
    (when (and (characterp ch)
	       (char= ch #\Escape))
      (setf input (read-char EXT:*KEYBOARD-INPUT*)
	    meta t))
    (make-instance 'key
		   :char (if (sys::input-character-char input) 
			     (char-downcase (sys::input-character-char input))
			   (char-downcase (sys::input-character-key input)))
		   :control (sys::char-bit input :control)
		   :meta (or meta
			     (sys::char-bit input :meta)))))

;;; some frame stuff

(defun init-clisp ()
  )

(defun shutdown ()
  (close (frame-window-stream (selected-frame))))

(defun make-default-clisp-frame (buffer)
  (let ((ws (screen:make-window)))
    (multiple-value-bind (lines cols) (screen:window-size ws)
      (let* ((height (1- lines))
	     (l (make-array (* lines cols)
			    :element-type 'character))
	     (d (make-array (list lines cols)
			    :element-type 'character
			    :displaced-to l :displaced-index-offset 0))
	     (w (make-window :x 0 :y 0 :cols cols :rows height :buffer buffer))
	     (mb (make-minibuffer-window lines cols))
	     (frame (make-instance 'clisp-frame
				   :width cols
				   :height lines
				   :window-tree (list w mb)
				   :current-window w
				   :minibuffer-window mb
				   :window-stream ws
				   :double-buffer l
				   :2d-double-buffer d)))
	(setf (window-frame w) frame
	      (window-frame mb) frame)
	frame))))
