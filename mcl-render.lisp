(in-package :lice)

(defclass mcl-window (ccl:window)
  ())

(defclass mcl-frame (frame mcl-window)
  ((double-buffer :type (array character 1) :initarg :double-buffer :accessor frame-double-buffer :documentation
		  "The display double buffer. This structure is compared to
the characters we want to blit. Only differences are sent to the video
hardware.")
   (2d-double-buffer :type (array character 2) :initarg :2d-double-buffer :accessor frame-2d-double-buffer :documentation
		     "Displaced from DISPLAY. This array is divided into rows and columns.")
   (cursor :initform #(0 0) :accessor mcl-frame-cursor)
   (font-width :initarg :font-width :accessor mcl-frame-font-width)
   (font-height :initarg :font-height :accessor mcl-frame-font-height)
   (font-ascent :initarg :font-ascent :accessor mcl-frame-font-ascent)))

(defmethod frame-start-render ((frame mcl-frame))
  (declare (ignore frame))
  )

(defmethod frame-end-render ((frame mcl-frame))
  (declare (ignore frame))
  (ccl:event-dispatch))


(defun window-move-cursor (window x y)
  (declare (ignore window x y))
  )

(defmethod frame-move-cursor ((frame mcl-frame) win x y)
;;  (setf x (* x (mcl-frame-font-width frame))
;;        y (+ (* y (mcl-frame-font-height frame)) 10))
;;   (ccl:invert-rect frame x y (+ x (mcl-frame-font-width frame)) (+ y (mcl-frame-font-height frame)))
  (setf (mcl-frame-cursor frame) (vector (+ (window-x win) x) (+ (window-y win) y)))
  (window-move-cursor win x y))

(defun putch (ch x y window frame)
  ;; (0,0) is above the visible area of the window. so add 25 (experimentally determined).
;;  (setf x (* x (mcl-frame-font-width frame))
;;        y (+ (* y (mcl-frame-font-height frame)) 10))
;;  (ccl:move-to (mcl-frame-window frame) x y)
;;  (princ ch (mcl-frame-window frame))
  (setf (aref (frame-2d-double-buffer frame) (+ y (window-y window)) (+ (window-x window) x)) ch)
 )

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
;;   (let ((display (frame-2d-double-buffer frame))
;; 	(linear (frame-double-buffer frame)))
    (clear-line-between window y start (1- (window-width window)) frame)
    ;; draw the seperator
    (when (window-seperator window)
      (putch #\| (+ (window-x window) (1- (window-width window t))) y window frame)))

(defun turn-on-attributes (buffer point)
  (declare (ignore buffer point))
  )

(defmethod frame-end-render ((frame mcl-frame))
  (ccl:invalidate-view frame))

(defmethod window-render (w (frame mcl-frame))
  "Render a window."
  ;; clear the window
  ;; (ccl:with-fore-color ccl:*white-color*
;;     (ccl:paint-rect (mcl-frame-window frame)  0 0
;;                     (ccl:point-h (ccl:view-size (mcl-frame-window frame)))
;;                     (ccl:point-v (ccl:view-size (mcl-frame-window frame))))
;;     (ccl:event-dispatch))
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
    (declare (ignore display linear))
    ;; clear the window
;;     (ccl:erase-rect (mcl-frame-window frame) 0 0
;;                     (ccl:point-h (ccl:view-size (mcl-frame-window frame)))
;;                     (ccl:point-v (ccl:view-size (mcl-frame-window frame))))
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

(defvar *mcl-stackgroup* (ccl:make-stack-group "lice")
  "")

(defun mcl-lice ()
  "Boot lice on mcl"
  (ccl:stack-group-preset *mcl-stackgroup* #'lice)
  (funcall *mcl-stackgroup* nil))

(ccl:defrecord Event
    (what integer)
  (message longint)
  (when longint)
  (where point)
  (modifiers integer))

(defvar *mcl-key-list* nil
  "List of keys pressed.")

(defmethod ccl:view-key-event-handler :after ((w mcl-window) ch)
  (declare (ignore ch))
  (let* ((keystroke (ccl:keystroke-name (ccl:event-keystroke (ccl:rref ccl:*current-event* :event.message) (ccl:rref ccl:*current-event* :event.modifiers))))
         (ch (if (listp keystroke)
                 (car (last keystroke))
                 keystroke))
         (mods (and (listp keystroke)
                    (butlast keystroke))))
    ;;(format t "k: ~s ~s ~s~%" keystroke ch mods)
    (setf *mcl-key-list* (nconc *mcl-key-list* 
                                (list (make-instance 'key
                                                     :meta (and (find :meta mods) t)
                                                     :control (and (find :control mods) t)
                                                     :char ch))))
    (funcall *mcl-stackgroup* nil)))

(defmethod ccl:view-draw-contents ((win frame))
  (ccl:erase-rect win 0 0
                  (ccl:point-h (ccl:view-size win))
                  (ccl:point-v (ccl:view-size win)))
  ;; characters
  (loop
     for y from 0 below (array-dimension (frame-2d-double-buffer win) 0)
     for i from (mcl-frame-font-ascent win) by (mcl-frame-font-height win) do
       (loop 
          for x from 0 below (array-dimension (frame-2d-double-buffer win) 1)
          for j from 0 by (mcl-frame-font-width win) do 
            (ccl:move-to win j i)
            (princ (aref (frame-2d-double-buffer win) y x) win)))
  ;; point
  (let ((x (* (elt (mcl-frame-cursor win) 0) (mcl-frame-font-width win)))
        (y (* (elt (mcl-frame-cursor win) 1) (mcl-frame-font-height win))))
    (ccl:invert-rect win x y (+ x (mcl-frame-font-width win)) (+ y (mcl-frame-font-height win)))))

(defmethod frame-read-event ((frame mcl-frame))
  ;; wait for more input
  (unless *mcl-key-list*
    (ccl:stack-group-return nil))
  ;; process the new input
  (pop *mcl-key-list*))

(defun mcl-font-width (font-name font-size)
  (multiple-value-bind (ascent descent maxwidth leading) (ccl::font-info (list font-name font-size))
    (declare (ignore ascent descent leading))
    maxwidth))

(defun mcl-font-height (font-name font-size)
  (multiple-value-bind (ascent descent maxwidth leading) (ccl::font-info (list font-name font-size))
    (declare (ignore maxwidth))
    (+ ascent descent leading)))

(defun mcl-font-ascent (font-name font-size)
  (multiple-value-bind (ascent descent maxwidth leading) (ccl::font-info (list font-name font-size))
    (declare (ignore descent maxwidth leading))
    ascent))

(defun make-default-mcl-frame (buffer)
  (let* ((lines 25)
	 (height (1- lines))
         (fw (mcl-font-width "Monaco" 12))
         (fh (mcl-font-height "Monaco" 12))
         (ascent (mcl-font-ascent "Monaco" 12))
	 (cols 80)
	 (l (make-array (* lines cols)
			:element-type 'character))
	 (d (make-array (list lines cols)
			:element-type 'character
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
                               :font-height fh
                               :font-width fw
                               :font-ascent ascent
                               :view-size (ccl:make-point (* cols fw) (* lines fh))
                               :view-font '("Monaco" 12))))
    (setf (window-frame w) frame
	  (window-frame mb) frame)
    frame))

