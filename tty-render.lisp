;; TTY rendering routines

(in-package :lice)

(defclass tty-frame (frame)
  ((double-buffer :type (array character 1) :initarg :double-buffer :accessor frame-double-buffer :documentation
		  "The display double buffer. This structure is compared to
the characters we want to blit. Only differences are sent to the video
hardware.")
   (2d-double-buffer :type (array character 2) :initarg :2d-double-buffer :accessor frame-2d-double-buffer :documentation
		     "Displaced from DISPLAY. This array is divided into rows and columns.")))

(defmethod frame-start-render ((frame tty-frame))
  )

(defmethod frame-end-render ((frame tty-frame))
  (cl-ncurses::refresh))

(defun window-move-cursor (window x y)
  (cl-ncurses::move (+ y (window-y window)) (+ x (window-x window))))

(defmethod frame-move-cursor ((frame tty-frame) win x y)
  (window-move-cursor win x y))

(defun putch (ch x y window frame)
  (window-move-cursor window x y)
  (cl-ncurses::addch (char-int ch))
  (setf (aref (frame-2d-double-buffer frame) (+ y (window-y window)) (+ x (window-x window))) ch))

(defun putstr (s x y w frame)
  (loop for i from 0 below (length s)
	for j from x by 1
	;;if (char/= (aref (window-2d-display w) y j) (aref s i))
	do (putch (aref s i) j y w frame)))

(defun line-height (buffer p)
  "Return the height of the line starting at p."
  (declare (ignore buffer p)))

(defun clear-line-between (w y start end frame)
  "insert LEN spaces from START on line Y."
  (window-move-cursor w start y)
  ;; FIXME: this could be done faster
  (loop for i from start to end
	do (putch #\Space i y w frame)))

;; Helper function for window-render
(defun clear-to-eol (y start window frame)
  (declare (type window window)
	   (type fixnum y start))
  (clear-line-between window y start (1- (window-width window)) frame)
  ;; draw the seperator
  (when (window-seperator window)
    (putch #\| (+ (window-x window) (1- (window-width window t))) y window frame)))
	      
(defun turn-on-attributes (buffer point)
  "Given the buffer and point, turn on the appropriate colors based on
the text properties present."
  ;; These are hardcoded for now
  (case (get-text-property point :face buffer)
    (:face-1
     (cl-ncurses::attrset (cl-ncurses::COLOR-PAIR 1)))
    (:face-2
     (cl-ncurses::attrset (cl-ncurses::COLOR-PAIR 2)))
    (:face-3
     (cl-ncurses::attrset (cl-ncurses::COLOR-PAIR 3)))
    (:face-4
     (cl-ncurses::attrset (cl-ncurses::COLOR-PAIR 4)))
    (:face-5
     (cl-ncurses::attrset (cl-ncurses::COLOR-PAIR 5)))
    (:face-6
     (cl-ncurses::attrset (cl-ncurses::COLOR-PAIR 6)))
    (:face-7
     (cl-ncurses::attrset (cl-ncurses::COLOR-PAIR 7)))
    (:face-8
     (cl-ncurses::attrset (cl-ncurses::COLOR-PAIR 8)))
    (t
     (cl-ncurses::attrset (cl-ncurses::COLOR-PAIR 1)))))

(defmethod window-render (w (frame tty-frame))
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
    ;; rxvt draws black on black if i don't turn on a color
    (cl-ncurses::attron (cl-ncurses::COLOR-PAIR 1))
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
				    ;; gotta turn off attributes to do this
				    (cl-ncurses::attrset (cl-ncurses::COLOR-PAIR 1))
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
    ;; rxvt draws black on black if i don't turn on a color
    (cl-ncurses::attroff (cl-ncurses::COLOR-PAIR 1))
    ;; Update the mode-line if it exists. FIXME: Not the right place
    ;; to update the mode-line.
    (when (buffer-mode-line (window-buffer w))
      (update-mode-line (window-buffer w))
      ;;(cl-ncurses::attron cl-ncurses::A_REVERSE)
      (cl-ncurses::attron (cl-ncurses::COLOR-PAIR 2))
      (putstr (truncate-mode-line (window-buffer w) (window-width w))
	      0 (window-height w nil) w frame)
      (cl-ncurses::attroff (cl-ncurses::COLOR-PAIR 2))
      ;;(cl-ncurses::attroff cl-ncurses::A_REVERSE)
      ;; don't forget the seperator on the modeline line
      (when (window-seperator w)
	(putch #\| (+ (window-x w) (window-width w)) (window-height w) w frame)))
    (reset-line-state w)
    ;; Set the cursor at the right spot
    (values cursor-x cursor-y)))

;;; keyboard stuff

(defmethod frame-read-event ((frame tty-frame))
  (when (listen *standard-input*)
    (let ((ch (char-code (read-char)))
	  key meta control)
      (dformat +debug-v+ "read: ~a~%" ch)
      ;; ESC mean Meta
      (when (= ch +key-escape+)
	(dformat +debug-v+ "meta~%")
	(setf ch (char-code (read-char))
	      meta t))
      ;; the 8th bit could also mean meta
      (when (= (logand ch 128) 128)
	(decf ch 128)
	(setf meta t))
      ;; <27 means Control
      (when (< ch 27)
	(incf ch 96)
	(setf control t))
      ;; set key to the character
      (setf key (case ch
		  (+key-backspace+
		   #\Backspace)
		  (+key-enter+
		   #\Return)
		  (+key-tab+
		   #\Tab)
		  (t
		   (code-char ch))))
      (make-instance 'key
		     :char key
		     :control control
		     :meta meta))))

;;; some frame stuff

(defun init-tty-colors ()
  (cl-ncurses::start-color)
  (cl-ncurses::init-pair 1 cl-ncurses::COLOR_WHITE cl-ncurses::COLOR_BLACK)
  (cl-ncurses::init-pair 2 cl-ncurses::COLOR_GREEN cl-ncurses::COLOR_BLUE)
  (cl-ncurses::init-pair 3 cl-ncurses::COLOR_WHITE cl-ncurses::COLOR_BLACK)
  (cl-ncurses::init-pair 4 cl-ncurses::COLOR_RED cl-ncurses::COLOR_BLACK)
  (cl-ncurses::init-pair 5 cl-ncurses::COLOR_MAGENTA cl-ncurses::COLOR_GREEN)
  (cl-ncurses::init-pair 6 cl-ncurses::COLOR_BLACK cl-ncurses::COLOR_BLUE)
  (cl-ncurses::init-pair 7 cl-ncurses::COLOR_WHITE cl-ncurses::COLOR_CYAN)
  (cl-ncurses::init-pair 8 cl-ncurses::COLOR_RED cl-ncurses::COLOR_MAGENTA))
;;     (cl-ncurses::attron (cl-ncurses::COLOR-PAIR 1)))

(defun init-tty ()
  (let ((scr (cl-ncurses::initscr)))
    (init-tty-colors)
    (cl-ncurses::raw)
    (cl-ncurses::meta scr 1)
    (cl-ncurses::noecho)
    (cl-ncurses::erase)
    (cl-ncurses::scrollok scr 0)))

(defun shutdown-tty ()
  (cl-ncurses::endwin))

(defun make-default-tty-frame (buffer)
  (let* ((lines cl-ncurses::*lines*)
	 (height (1- lines))
	 (cols (prog1 cl-ncurses::*cols*))
	 (l (make-array (* lines cols)
			:element-type 'character))
	 (d (make-array (list lines cols)
			:element-type 'character
			:displaced-to l :displaced-index-offset 0))
	 (w (make-window :x 0 :y 0 :cols cols :rows height :buffer buffer))
	 (mb (make-minibuffer-window lines cols))
	 (frame (make-instance 'tty-frame
			       :width cols
			       :height lines
			       :window-tree (list w mb)
			       :current-window w
			       :minibuffer-window mb
			       :double-buffer l
			       :2d-double-buffer d)))
    (setf (window-frame w) frame
	  (window-frame mb) frame)
    frame))

(defun make-test-frame (buffer)
  "This can be used to create a frame configuration for testing."
  (let* ((lines 20)
	 (height (1- lines))
	 (cols 78)
	 (l (make-array (* lines cols)
			:element-type 'character
			:initial-element #\Space))
	 (d (make-array (list lines cols)
			:element-type 'character
			:displaced-to l :displaced-index-offset 0))
	 (w (make-window :x 0 :y 0 :cols cols :rows height :buffer buffer))
	 (mb (make-minibuffer-window lines cols))
	 (frame (make-instance 'tty-frame
			       :width cols
			       :height lines
			       :window-tree (list w mb)
			       :current-window w
			       :minibuffer-window mb
			       :double-buffer l
			       :2d-double-buffer d)))
    (setf (window-frame w) frame
	  (window-frame mb) frame)
    frame))
