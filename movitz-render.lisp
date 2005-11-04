(in-package :lice)

;; TTY rendering routines

(in-package :lice)

(defclass movitz-frame (frame)
  ((double-buffer :type (array character 1) :initarg :double-buffer :accessor frame-double-buffer :documentation
		  "The display double buffer. This structure is compared to
the characters we want to blit. Only differences are sent to the video
hardware.")
   ;; (2d-double-buffer :type (array character 2) :initarg :2d-double-buffer :accessor frame-2d-double-buffer :documentation
;; 		     "Displaced from DISPLAY. This array is divided into rows and columns.")
   ))

(defmethod frame-start-render ((frame movitz-frame))
  )

(defmethod frame-end-render ((frame movitz-frame))
  )

(defvar *current-attribute* 0
  "The currently set character attribute. Used while rendering a window.")

(defun window-move-cursor (window x y)
  (muerte.x86-pc::move-vga-cursor (+ x (window-x window)) (+ y (window-y window))))

(defmethod frame-move-cursor ((frame movitz-frame) win x y)
  (window-move-cursor win x y))

(defun setxy (x y n)
  "A very raw character & attribute blitter"
  (setf (muerte::memref-int muerte.x86-pc::*screen* :index (+ x (* y muerte.x86-pc::*screen-stride*)) :type :unsigned-byte16) n))

(defun set-char-attr (x y ch attr)
  ""
  (setxy x y (logior (ash attr 8) (char-code ch))))

(defun putch (ch x y window frame)
  (set-char-attr (+ x (window-x window)) (+ y (window-y window)) ch *current-attribute*)
  (setf (aref (frame-double-buffer frame) (+ (* (+ y (window-y window)) (frame-width frame))
					     (+ x (window-x window)))) ch))

(defun putstr (s x y w frame)
  (loop for i from 0 below (length s)
	for j from x by 1
	;;if (char/= (aref (window-2d-display w) y j) (aref s i))
	do (putch (aref s i) j y w frame)))

(defun clear-line-between (w y start end frame)
  "insert LEN spaces from START on line Y."
  ;;(window-move-cursor w start y)
  ;; FIXME: this could be done faster
  (loop for i from start to end
	do (putch #\Space i y w frame)))

;; Helper function for window-render
(defun clear-to-eol (y start window frame)
  (declare (type window window)
	   (type fixnum y start))
  (let (;; (display (frame-2d-double-buffer frame))
	;; (linear (frame-double-buffer frame))
	)
    (clear-line-between window y start (1- (window-width window)) frame)
    ;; draw the seperator
    (when (window-seperator window)
      (putch #\| (+ (window-x window) (1- (window-width window t))) y window frame))))
	      
(defun turn-on-attributes (buffer point)
  "Given the buffer and point, turn on the appropriate colors based on
the text properties present."
  ;; These are hardcoded for now
  (setf *current-attribute*
	(case (get-text-property point :face buffer)
	  (:face-1 7)
	  (:face-2 112)
	  (:face-3 14)
	  (:face-4 34)
	  (:face-5 8)
	  (:face-6 9)
	  (:face-7 2)
	  (:face-8 1)
	  (t 7))))

(defmethod window-render (w (frame movitz-frame))
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
	;; (display (frame-2d-double-buffer frame))
	)
    ;; rxvt draws black on black if i don't turn on a color
    (setf *current-attribute* 7)
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
				    (setf *current-attribute* 7)
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
    (setf *current-attribute* 7)
    ;; Update the mode-line if it exists. FIXME: Not the right place
    ;; to update the mode-line.
    (when (buffer-mode-line (window-buffer w))
      (update-mode-line (window-buffer w))
      ;;(cl-ncurses::attron cl-ncurses::A_REVERSE)
      (setf *current-attribute* 18)
      (putstr (truncate-mode-line (window-buffer w) (window-width w))
	      0 (window-height w nil) w frame)
      (setf *current-attribute* 7)
      ;;(cl-ncurses::attroff cl-ncurses::A_REVERSE)
      ;; don't forget the seperator on the modeline line
      (when (window-seperator w)
	(putch #\| (+ (window-x w) (window-width w)) (window-height w) w frame)))
    (reset-line-state w)
    ;; Set the cursor at the right spot
    (values cursor-x cursor-y)))

;;; keyboard stuff

(defmethod frame-read-event ((frame movitz-frame))
  "Return a key structure."
  (let (control meta shift)
    (loop
     (multiple-value-bind (key release) (muerte.x86-pc.keyboard::get-key)
       (when (and key
		  (characterp key) 
		  (not release))
	 (return-from frame-read-event (make-instance 'key
						      :char key
						      :control (logbitp muerte.x86-pc.keyboard::+qualifier-ctrl+
									muerte.x86-pc.keyboard::*qualifier-state*)
						      :meta (logbitp muerte.x86-pc.keyboard::+qualifier-alt+
								     muerte.x86-pc.keyboard::*qualifier-state*))))))))

;;; some frame stuff

(defun init-movitz ()
  )

(defun shutdown-movitz ()
  )

(defun make-default-movitz-frame (buffer)
  (let* ((lines muerte.x86-pc::*screen-height*)
	 (height (1- lines))
	 (cols muerte.x86-pc::*screen-width*)
	 (l (make-array (* lines cols)
			:element-type 'character))
;; 	 (d (make-array (list lines cols)
;; 			:element-type 'character
;; 			:displaced-to l :displaced-index-offset 0))
	 (w (make-window :x 0 :y 0 :cols cols :rows height :buffer buffer))
	 (mb (make-minibuffer-window lines cols))
	 (frame (make-instance 'movitz-frame
			       :width cols
			       :height lines
			       :window-tree (list w mb)
			       :current-window w
			       :minibuffer-window mb
			       :double-buffer l
			       ;; :2d-double-buffer d
			       )))
    (setf (window-frame w) frame
	  (window-frame mb) frame)
    frame))

;; (defun make-test-frame (buffer)
;;   "This can be used to create a frame configuration for testing."
;;   (let* ((lines 20)
;; 	 (height (1- lines))
;; 	 (cols 78)
;; 	 (l (make-array (* lines cols)
;; 			:element-type 'character
;; 			:initial-element #\Space))
;; 	 (d (make-array (list lines cols)
;; 			:element-type 'character
;; 			:displaced-to l :displaced-index-offset 0))
;; 	 (w (make-window :x 0 :y 0 :cols cols :rows height :buffer buffer))
;; 	 (mb (make-minibuffer-window lines cols))
;; 	 (frame (make-instance 'tty-frame
;; 			       :width cols
;; 			       :height lines
;; 			       :window-tree (list w mb)
;; 			       :current-window w
;; 			       :minibuffer-window mb
;; 			       :double-buffer l
;; 			       :2d-double-buffer d)))
;;     (setf (window-frame w) frame
;; 	  (window-frame mb) frame)
;;     frame))

(provide :lice-0.1/movitz-render)
