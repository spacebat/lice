(in-package :lice)

(defvar *next-screen-context-lines* 2
  "Number of lines of continuity when scrolling by screenfuls.")

(defvar *window-min-height* 4
  "Delete any window less than this tall (including its mode line).")

(defvar *window-min-width* 10
  "Delete any window less than this wide.")

;; we just want a fast and easy dumping area for data. start and end
;; are inclusive.
(defstruct cache-item
   (start 0 :type integer)
   (end 0 :type integer))

(defun make-empty-cache-item-vector ()
;;   (make-array 0 :element-type 'cache-item
;; 	      :adjustable t
;; 	      :fill-pointer 0)
  ())

;; start and end are inclusive and are buffer points
(defclass line-cache ()
  ((start :type integer :initform 0 :initarg :start :accessor lc-start)
   (end :type integer :initform 0 :initarg :end :accessor lc-end)
   (valid :type boolean :initform nil :initarg :valid :accessor lc-valid)
   (cache :type list ;;(array cache-item 1) 
	  :initform nil ;; (make-array 0 :element-type 'cache-item
;; 			   :adjustable t
;; 			   :fill-pointer 0)
	  :initarg :cache :accessor lc-cache)))

(defun item-in-cache (window n)
  "Return the Nth item in the cache or NIL if it doesn't exist."
  (elt (lc-cache (window-cache window)) n))
;;   (when (< n (length (lc-cache (window-cache window))))
;;     (aref (lc-cache (window-cache window)) n)))

(defclass window ()
  ((frame :initarg :frame :accessor window-frame)
   (x :type integer :initarg :x :accessor window-x)
   (y :type integer :initarg :y :accessor window-y)
   (w :type integer :initarg :w :documentation
      "The width of the window's contents.")
   (h :type integer :initarg :h :documentation
      "The total height of the window, including the mode-line.")
   (seperator :type boolean :initform nil :accessor window-seperator :documentation
	      "T when the window is to draw a vertical seperator. used in horizontal splits.")
   (line-state :type (array integer 1) :initarg :line-state :accessor window-line-state)
   (cache :type line-cache :initarg :cache :accessor window-cache)
   ;; Indices into cache (inclusive) that describe the range of the
   ;; cache that will be displayed.
   (top-line :type integer :initarg :top-line :accessor window-top-line)
   (bottom-line :type integer :initarg :bottom-line :accessor window-bottom-line)
   (point-col :type integer :initarg :point-col :accessor window-point-col)
   (point-line :type integer :initarg :point-line :accessor window-point-line)
   ;; The rest refer to points in the buffer
   (buffer :type buffer :initarg :buffer :accessor window-buffer)
   (bpoint :type marker :initarg :bpoint :accessor window-bpoint :documentation
	   "A marker marking where in the text the window point is.")
   (top :type marker :initarg :top :accessor window-top :documentation
	"The point in buffer that is the first character displayed in the window")
   (bottom :type marker :initarg :bottom :accessor window-bottom :documentation
	   "The point in buffer that is the last character displayed
in the window. This should only be used if bottom-valid is T.")
   (bottom-valid :type boolean :initform nil :accessor window-bottom-valid :documentation
		 "When this is T then bottom should be used to
calculate the visible contents of the window. This is used when
scrolling up (towards the beginning of the buffer)."))
  (:documentation "A Lice Window."))

;; (defun update-window-display-arrays (window)
;;   "Used to update the window display structures for window splits."
;;   (let* ((rows (window-height window t))
;; 	 (cols (window-width window t))
;; 	 (l (make-array (* rows cols)
;; 			:element-type 'character))
;; 	 (d (make-array (list rows cols)
;; 			:element-type 'character
;; 			:displaced-to l :displaced-index-offset 0)))
;;     ;; FIXME: This forces needless redraw because the arrays are
;;     ;; reset.
;;     (setf (window-display window) l
;; 	  (window-2d-display window) d)))

(defun make-window (&key x y cols rows buffer frame 
			 (top (make-marker 0 buffer))
			 (bpoint (make-marker))
			 (type 'window))
  "Return a new window. This is handy for setting up all the pesky
display structures.

TYPE isn't used yet. it's just there for hype."
  (let* ((w (make-instance type
			   :frame frame
			   :x x :y y :w cols :h rows
			   :line-state (make-array rows :element-type 'integer :initial-element -1)
			   :cache (make-instance 'line-cache :valid t)
			   :top-line 0
			   :bottom-line 0
			   :point-col 0
			   :point-line 0
			   :buffer buffer
			   :top top
			   :bottom (make-marker 0 buffer)
			   :bpoint bpoint
			   :point-col 0
			   :point-line 0)))
    w))

(defun make-test-window (buffer)
  (make-window :x 0 :y 0 :cols 60 :rows 20 :buffer buffer))


;;; Other non-display related functions

(defun window-height (w &optional include-mode-line)
  "Return the height of the window. By default, the mode-line is not
included in the height."
  ;; if the mode-line is nil, then there is no modeline.
  (if (or include-mode-line
	  (null (buffer-mode-line (window-buffer w))))
      (slot-value w 'h)
    (1- (slot-value w 'h))))

(defun window-width (w &optional include-seperator)
  "Return the width of the window. By default, the vertical seperator,
for horizontal splits, is not included in the width."
  ;; if the mode-line is nil, then there is no modeline.
  (if (or include-seperator
	  (not (window-seperator w)))
      (slot-value w 'w)
    (1- (slot-value w 'w))))

(defun get-current-window (&optional (frame (selected-frame)))
  "Return the current window in the current frame. If FRAME is
specified, use that frame instead."
  (frame-current-window frame))

(defun set-window-buffer (window buffer &optional keep-margins)
  "Make WINDOW display BUFFER as its contents.
BUFFER can be a buffer or buffer name.
Optional third arg KEEP-MARGINS non-nil means that WINDOW's current
display margins, fringe widths, and scroll bar settings are maintained;
the default is to reset these from BUFFER's local settings or the frame
defaults."
  ;; this is redundant if buffer is a string, since its
  ;; looked up already.
  (declare (type window window)
           (type buffer buffer)
           (type boolean keep-margins)
           (ignore keep-margins))
  (let ((buf (get-buffer buffer)))
    (unless buf
      (error "No buffer named ~a" buffer))
    (unless (eq (window-buffer window) buf)
      ;; update buffer time stamps
      (incf (buffer-display-count buf))
      ;; MOVITZ doesn't have get-universal-time
      ;; (setf (buffer-display-time buf) (get-universal-time))
      ;; update buffer list
      (bring-buffer-to-front buf)
      ;; display stuff
      (set-marker (window-top window) 0 buf)
      (set-marker (window-bottom window) 100 buf)
      (set-marker (window-bpoint window) (marker-position (buffer-point buf)) buf)
      ;; finally set the buffer
      (setf (window-buffer window) buf)
      ;; TODO: run hooks
      )))

(defgeneric cache-size (object))

(defmethod cache-size ((object line-cache))
  (length (lc-cache object)))

(defmethod cache-size ((object window))
  (cache-size (window-cache object)))

(defun reset-line-state (window)
  (fill (window-line-state window) -1))

(defun window-reset-cache (window)
  (with-slots (cache) window
    (setf (lc-cache cache) nil
	  (lc-start cache) 0
	  (lc-end cache) 0
	  (lc-valid cache) t)))

(defun point-in-line-cache (line-cache p)
  "Return the line in the cache that P is on. NIL if p is not in range"
  (declare (type integer p))
  (position-if (lambda (l)
		 (and (>= p (cache-item-start l))
		      (<= p (cache-item-end l))))
	       line-cache))


;;; Display related functions. Generate the line cache based on
;;; character cells, not pixels.

(defun add-line-to-cache (cache from to &optional at-beginning)
  "Add a single line to the cache list. Return the new cache list."
  (let ((line (make-cache-item :start from :end to)))
    (if at-beginning
	(cons line cache)
      (nconc1 cache line))))
;; 	(progn
;; 	  (grow-vector lines 1 line)
;; 	  (replace lines lines :start1 1)
;; 	  (setf (elt lines 0) line))
;;       (vector-push-extend line lines))))
       

;; (defun generate-lines-region (cache buffer width from to)
;;   "FROM must not be a newline (It should be the character after a new
;; line or the beginning of the buffer) and TO must be newline or the
;; end of the buffer."
;;   (declare (type line-cache cache)
;; 	   (type buffer buffer)
;; 	   (type integer width from to))
;;   (let ((lines (make-array 0 :element-type 'cache-item
;; 			   :adjustable t
;; 			   :fill-pointer 0))
;; 	(rplc-start (= (1+ to) (lc-start cache)))
;; 	(rplc-end (= (1- from) (lc-end cache)))
;; 	(empty-cache (= (length (lc-cache cache)) 0)))
;;     (dformat +debug-vvv+ "generate-n-lines: ~a ~a~%" from to)
;;     ;; Make sure either from-1 or to+1 is already in the cache, its
;;     ;; the first one. A point cannot exist in 2 cache lines because
;;     ;; points are inclusive.
;;     (when (or rplc-start rplc-end empty-cache)
;;       ;; search for newlines until we hit TO
;;       (do ((last-p from (1+ p))
;; 	   (p (buffer-scan-newline buffer from to 1)
;; 	      (buffer-scan-newline buffer (1+ p) to 1))
;; 	   (l 0 (1+ l)))
;; 	  (nil)

;; 	;; Break the buffer line into chunks that fit on one window line
;; 	(dformat +debug-vvv+ "last-p: ~a p:~a~%" last-p p)
;; 	(loop for i from last-p by width
;; 	      do (vector-push-extend (make-cache-item :start i 
;; 						      :end (if (<= (+ i (1- width)) p)
;; 							       (+ i (1- width))
;; 							     p))
;; 				     lines)
;; 	      always (< (+ i (1- width)) p))
;; 	;; Once we've processed the new line, check if we've run out of
;; 	;; buffer to process.
;; 	(when (= p to)
;; 	  (return)))
;;       ;; Add these new items to the cache
;;       (let ((carray (lc-cache cache)))
;; 	(adjust-array cache
;; 		      (+ (length carray)
;; 			 (length lines))
;; 		      :initial-element (aref lines 0)
;; 		      :fill-pointer (+ (length carray)
;; 				       (length lines)))
;; 	(cond (rplc-start
;; 	       ;; Put it at the beginning
;; 	       (dformat +debug-vvv+ "rplc-start~%")
;; 	       (setf (lc-start cache) from)
;; 	       (replace carray carray :start1 (length lines))
;; 	       (replace carray lines))
;; 	      (rplc-end
;; 	       (dformat +debug-vvv+ "rplc-end~%")
;; 	       (setf (lc-end cache) to)
;; 	       (replace carray lines :start1 (- (length carray)
;; 					       (length lines))))
;; 	      (empty-cache
;; 	       (dformat +debug-vvv+ "empty-cache~%")
;; 	       (setf (lc-start cache) from)
;; 	       (setf (lc-end cache) to)
;; 	       ;; FIXME: we could just use lines instead of copy them over, right?
;; 	       (replace carray lines))))))
;;   (dformat +debug-vvv+ "after gen-n-lines: ~a~%" (lc-cache cache)))

(defun generate-n-lines-forward (buffer width from n-lines)
  "Return an array of cache-items for N-LINES lines in BUFFER rendered
with WIDTH columns starting at FROM. The array will have length at
least N-LINES."
  (declare (type buffer buffer)
	   (type integer width from))
  (let ((lines (make-empty-cache-item-vector))
	(to (1- (buffer-size buffer))))
    (dformat +debug-vvv+ "generate-n-lines: ~a ~a~%" from to)
    ;; search for newlines until we hit TO
    (do ((last-p from (1+ p))
	 (p (buffer-scan-newline buffer from to 1)
	    (buffer-scan-newline buffer (1+ p) to 1))
	 (l 0 (1+ l)))
	(nil)

      ;; Break the buffer line into chunks that fit on one window line
      (dformat +debug-vvv+ "last-p: ~a p:~a~%" last-p p)
      (loop for i from last-p by width
	    do (setf lines (add-line-to-cache lines i (if (<= (+ i (1- width)) p)
							  (+ i (1- width))
							p)))
;; (vector-push-extend (make-cache-item :start i 
;; 						    :end (if (<= (+ i (1- width)) p)
;; 							     (+ i (1- width))
;; 							   p))
;; 				   lines)
	    always (< (+ i (1- width)) p))
      ;; Once we've processed the new line, check if we've generated
      ;; enough lines. Return LINES we're done.
      (when (or (>= (length lines) n-lines)
		(>= p to))
	(return lines)))))

(defun generate-n-lines-backward (buffer width from n-lines)
  "Return an array of cache-items for N-LINES lines in BUFFER rendered
with WIDTH columns starting at FROM and going backward. The array will
have length at least N-LINES.

FROM is assumed to the char pos of the newline at the end of the
starting line."
  (declare (type buffer buffer)
	   (type integer width from))
  (let ((lines (make-empty-cache-item-vector))
	(to 0))
    (dformat +debug-vvv+ "generate-n-lines: ~a ~a~%" from to)
    ;; search for newlines until we hit TO.
    (do ((last-p from p)
	 (p (buffer-scan-newline buffer (1- from) to -1)
	    (buffer-scan-newline buffer (1- p) to -1))
	 (l 0 (1+ l)))
	(nil)

      ;; Break the buffer line into chunks that fit on one window line
      (dformat +debug-vvv+ "last-p: ~a p:~a~%" last-p p)
      ;; unless we're at the beginning of the buffer, we want the char
      ;; after p because p will be a newline. last-p will be the
      ;; newline at the end of the line, 1+ p will be the beginning.
      ;;
      ;; this is a bit hairy because we're going backwards, but we go
      ;; through the line forward.
      ;;(let ((items (make-empty-cache-item-vector)))
      (loop for i from (if (zerop p) 0 (1+ p)) by width
	    do (setf lines (add-line-to-cache lines
					      i
					      (if (<= (+ i (1- width)) last-p)
						  (+ i (1- width))
						last-p)
					      t))
	    ;; (vector-push-extend (make-cache-item :start i 
	    ;; 						      :end (if (<= (+ i (1- width)) last-p)
	    ;; 							       (+ i (1- width))
	    ;; 							     last-p))
	    ;; 				     items)
	    always (< (+ i (1- width)) last-p))
	;;(vector-append lines (nreverse items)))
      ;; Once we've processed the new line, check if we've generated
      ;; enough lines. Return LINES we're done.
      (when (or (>= (length lines) n-lines)
		(<= p to))
	(return  lines ;; (nreverse lines)
		 )))))

;; (defun update-cache (cache buffer width point n-many)
;;   "Add N-MANY lines to the end of the line cache CACHE unless N-MANY
;; is negative. In that case add (abs n-many) to the beginning. This
;; function requires at least 1 line in the cache already.

;; Lines are WIDTH in length. BUFFER is the data for caching."
;;   ;; Fill in above the cache
;;   (dformat +debug-vv+ "update-cache: ~a~%" n-many)
;;   (if (> n-many 0)
;;       (let* ((end (1+ (lc-end cache)))
;; 	     pt)
;; 	;; Go forward
;; 	(when (< end (1- (buffer-size buffer)))
;; 	    ;; Add cache entries
;; 	  (setf pt (buffer-scan-newline buffer
;; 					end (1- (buffer-size buffer))
;; 					n-many))
;; 	  (generate-lines-region cache buffer width end pt)))
;;     ;; Go backward
;;     (let* ((start (1- (lc-start cache)))
;; 	   pt)
;;       ;; We need this because start is a newline, which we want to skip over
;;       (setf n-many (1- n-many))
;;       (dformat +debug-vvv+ "backward: ~a ~a ~a~%"
;; 	       start n-many (lc-cache cache))
;;       (when (and (> start 0)
;; 		 (/= n-many 0))
;; 	;; Add cache entries
;; 	(setf pt (buffer-scan-newline buffer start 0 n-many))
;; 	(generate-lines-region cache buffer width (if (> pt 0) (1+ pt) pt) start)))))

(defun add-end-of-buffer (buffer lines)
  "The point can be at (buffer-size buffer) but we only scan to
  1- that. So if we're scanned to the end of the buffer properly
  alter LINES to contain that point."
  (let ((end (1- (buffer-size buffer)))
        (last-elt (elt lines (1- (length lines)))))
    (when (= (cache-item-end last-elt) end)
      (if (char= (buffer-char-after buffer end) #\Newline)
          (add-line-to-cache lines (buffer-size buffer) (buffer-size buffer))
        (incf (cache-item-end last-elt))))))

(defun window-framer-from-top (window point &optional always-return-lines)
  "Fill in window's line cache from WINDOW-TOP with a full window's
worth of lines and return T if POINT was in the line cache. otherwise
don't change anything and return nil."
  (let* ((lines (generate-n-lines-forward (window-buffer window) (window-width window)
					  (marker-position (window-top window)) 
					  (window-height window))))
    (add-end-of-buffer (window-buffer window) lines)
    (when (or always-return-lines
	      (point-in-line-cache lines point))
      lines)))

(defun window-framer-from-bottom (window point &optional always-return-lines)
  "Fill in window's line cache from WINDOW-BOTTOM with a full window's
worth of lines and return T if POINT was in the line cache. otherwise
don't change anything and return nil."
  (let* ((lines (generate-n-lines-backward (window-buffer window) (window-width window)
					   (marker-position (window-bottom window))
					   (window-height window))))
    (add-end-of-buffer (window-buffer window) lines)
    (when (or always-return-lines
	      (point-in-line-cache lines point))
      lines)))

(defun window-framer-around-point (window point n-many)
  "Fill in window's line cache going out from point with n-many lines
above WINDOW-POINT, or as many as possible if we hit the top of the window."
  ;; Add the line with the pointer on it
  (let* ((max (1- (buffer-size (window-buffer window))))
	 (b (buffer-scan-newline (window-buffer window) point 0 0))
	 (e (buffer-scan-newline (window-buffer window) point max 1))
	 (lines-above (generate-n-lines-backward (window-buffer window) (window-width window)
						 e n-many))
	 (lines-below (when (< e max)
			(generate-n-lines-forward (window-buffer window) (window-width window)
						  (1+ e)
						  (- (window-height window)
						     (min n-many 
							  (length lines-above)))))))
    (declare (ignore b))
    (if lines-below
        (add-end-of-buffer (window-buffer window) lines-below)
      (add-end-of-buffer (window-buffer window) lines-above))
    (when (or (point-in-line-cache lines-above point)
              (point-in-line-cache lines-below point))
      (if lines-below
	  (nconc lines-above lines-below)
;; 	  (grow-vector lines-above (length lines-below) (elt lines-below 0))
;; 	  (replace lines-above lines-below :start1 end))
      lines-above))))
  
(defun window-framer (window point n-many)
  "fill in window's line-cache."
  ;; first try the top/bottom markers. if point isn't in there then
  ;; center the window around point.
  (let* ((bot (and (window-bottom-valid window)
		   (window-framer-from-bottom window point)))
	 (top (unless bot
		(window-framer-from-top window point)))
	 (around (unless top
		   (window-framer-around-point window point n-many)))
	 (lines (or bot top around)))
    (assert lines)
    ;; set the top marker
    (setf (window-bottom-valid window) nil)
    (cond (bot
	   (let* ((tl (max 0 (- (length lines) (window-height window))))
		  (bl (min (1- (length lines)) (+ tl (1- (window-height window))))))
	     (setf (marker-position (window-top window)) 
		   (cache-item-start (elt lines tl))
		   (window-top-line window) tl
		   (marker-position (window-bottom window)) (cache-item-end (elt lines bl)))))
	  (top
	   (let* ((tl (point-in-line-cache lines (marker-position (window-top window))))
		  (bl (min (1- (length lines)) (+ tl (1- (window-height window))))))
	     (setf (window-top-line window) tl
		   (marker-position (window-bottom window)) (cache-item-end (elt lines bl)))))
	  (around
	   (let* ((pl (point-in-line-cache lines point))
		  (tl (max 0 (- pl n-many)))
		  (bl (min (1- (length lines)) (+ tl (1- (window-height window))))))
	     (setf (marker-position (window-top window))
		   (cache-item-start (elt lines tl))
		   (window-top-line window) tl
		   (marker-position (window-bottom window)) (cache-item-end (elt lines bl))))))
    ;; fill in window's cache
    (with-slots (cache) window
      (setf (lc-cache cache) lines
	    (lc-start cache) (cache-item-start (elt lines 0))
	    (lc-end cache) (cache-item-end (elt lines (1- (length lines))))
	    (lc-valid cache) t))))
  

;; (defun window-framer (window point n-many)
;;   "Decide what part of the buffer to display in window. Sets top,
;; bottom, point-col, and point-line in window. N-MANY is the number of
;; lines from point to the top of the window."
;;   ;; Add the line with the pointer on it
;;   (let ((b (buffer-scan-newline (window-buffer window) point 0 0))
;; 	(e (buffer-scan-newline (window-buffer window) 
;; 				point (1- (buffer-size (window-buffer window))) 1)))
;;     (dformat +debug-vv+ "point line: ~a ~a~%" b e)
;;     (generate-lines-region window (if (= b 0) b (1+ b)) e))
;;   ;; search up n-many the window height
;;   (update-cache window (- n-many))
;;   (dformat +debug-vvv+ "cache s/e: ~a ~a~%"
;; 	   (lc-start (window-cache window)) 
;; 	   (lc-end (window-cache window)))
;;   ;; search down height - n-many + 1 (we've already generated the point's line)
;;   (update-cache window (- (window-height window) n-many -1))
;;   ;; Special case. if we got to the end of the buffer and it ends with
;;   ;; a newline. Add an extra cache line for line after that which
;;   ;; could contain the cursor.
;;   (when (= (lc-end (window-cache window)) 
;; 	   (1- (buffer-size (window-buffer window))))
;;     (add-line-to-cache window
;; 		       (buffer-size (window-buffer window))
;; 		       (buffer-size (window-buffer window))
;; 		       nil t nil))
;;   ;; if we find window-top or window bottom in the cache then we
;;   ;; should use it as the top/bottom and generate the remaining lines
;;   (let ((wtop (point-window-line window (marker-position (window-top window))))
;; 	(pline (point-window-line window point))
;; 	(wbot (point-window-line window (marker-position (window-bottom window)))))
;;     (dformat +debug-vvv+ "cache: ~a~%" (lc-cache (window-cache window)))
;;     (dformat +debug-vv+ ">>>wtop: ~a ~a pline: ~a ~a wbot: ~a ~a~%" 
;; 	     wtop (marker-position (window-top window))
;; 	     pline point
;; 	     wbot (marker-position (window-bottom window)))
;;     (cond ((and wtop
;; 		(<= wtop pline))
;; 	   (dformat +debug-vv+ "wtop. ~a ~%" (cache-size window))
;; 	   (let ((lines-left (- (window-height window)
;; 				(- (cache-size window) wtop))))
;; 	     (when (> lines-left 0)
;; 	       (update-cache window lines-left))
;; 	     (dformat +debug-vvv+ "wtop cache: ~a~%" (lc-cache (window-cache window)))
;; 	     (setf (window-top-line window) wtop
;; 		   (marker-position (window-top window)) (cache-item-start 
;; 							  (aref (lc-cache (window-cache window)) wtop))
;; 		   (window-bottom-line window) (min (1- (cache-size window))
;; 						    (+ wtop (window-height window) -1))
;; 		   (marker-position (window-bottom window)) (cache-item-end
;; 							     (aref (lc-cache (window-cache window)) 
;; 								   (window-bottom-line window))))))
;; 	  ((and wbot
;; 		(>= wbot pline))
;; 	   (dformat +debug-vv+ "wbot. ~a ~%" (cache-size window))
;; 	   (let ((lines-left (- (window-height window) wbot 1)))
;; 	     (when (> lines-left 0)
;; 	       (update-cache window (- lines-left)))
;; 	     (dformat +debug-vvv+ "wbot cache: ~a~%" (lc-cache (window-cache window)))
;; 	     ;; we need to rescan bottom since lines may have been
;; 	     ;; added above it, invalidating wbot
;; 	     (setf wbot (point-window-line window (marker-position (window-bottom window)))
;; 		   (window-bottom-line window) wbot
;; 		   (marker-position (window-bottom window)) (cache-item-end (aref 
;; 									     (lc-cache (window-cache window))
;; 									     wbot))
;; 		   (window-top-line window) (max 0 (- wbot (window-height window) 1))
;; 		   (marker-position (window-top window)) (cache-item-start (aref 
;; 									    (lc-cache (window-cache window))
;; 									    (window-top-line window))))))
;; 	   (t
;; 	    (dformat +debug-vv+ "we need to scroll. ~a ~%" (cache-size window))
;; 	    (setf (window-top-line window) (max 0 (- pline n-many))
;; 		  (marker-position (window-top window)) (cache-item-start (aref (lc-cache (window-cache window))
;; 										(window-top-line window)))
;; 		  (window-bottom-line window) (min
;; 					       (1- (cache-size window))
;; 					       (+ (window-top-line window) (window-height window) -1))
;; 		  (marker-position (window-bottom window)) (cache-item-end
;; 							    (aref (lc-cache (window-cache window))
;; 								  (window-bottom-line window)))))))
;;   (setf (window-point-line window) (point-window-line window point))
;;   (dformat +debug-vv+ "<<<top: ~a ~a pt: ~a ~a bot: ~a ~a~%"
;; 	   (window-top-line window) (marker-position (window-top window))
;; 	   (window-point-line window) point
;; 	   (window-bottom-line window) (marker-position (window-bottom window))))

(defun window-point (&optional window)
  "Return current value of point in WINDOW. For a nonselected window,
this is the value point would have if that window were selected."
  (if (eq window (get-current-window))
      (point (window-buffer window))
    (marker-position (window-bpoint window))))

(defun set-window-point (window pos)
  (let ((mark (if (eq window (get-current-window))
		  (buffer-point (window-buffer window))
		(window-bpoint window))))
    (if (and (<= pos (buffer-max (window-buffer window)))
	     (>= pos (buffer-min (window-buffer window))))
	(setf (marker-position mark) pos)
      (error "out of range"))))

(defun get-buffer-window (buffer &optional frame)
  "Return a window currently displaying BUFFER, or nil if none.
If optional argument FRAME is `visible', search all visible frames.
If optional argument FRAME is 0, search all visible and iconified frames.
If FRAME is t, search all frames.
If FRAME is nil, search only the selected frame.
If FRAME is a frame, search only that frame."
  ;; TODO: honour FRAME
  (setf frame (selected-frame)
	buffer (get-buffer buffer))
  (find buffer (frame-window-list frame) :key 'window-buffer))

(defun window-scroll-up (window n-lines)
  "scroll the window up (go torwards the end of the buffer) LINES many
lines, moving the window point to be visible."
  (let* ((len (+ (window-height window) n-lines))
	 (lines (generate-n-lines-forward (window-buffer window) (window-width window)
					 (marker-position (window-top window)) 
					 len)))
    ;; if there aren't n-lines left in the buffer then signal
    ;; an end-of-buffer error.
;;     (unless (>= (length lines) n-lines)
;;       (error "end of buffer"))
    (setf (marker-position (window-top window)) (cache-item-start (elt lines
								       (1- (min (length lines)
										n-lines)))))
    ;; FIXME: for now, set the point at the top of the window if it
    ;; isn't visible.
    (when (or (< (window-point window) (marker-position (window-top window)))
	      (not (point-in-line-cache lines (window-point window))))
      (set-window-point window (marker-position (window-top window))))))

(defun window-scroll-down (window n-lines)
  "scroll the window down (go torwards the beginning of the buffer)
LINES many lines, moving the window point to be visible."
  (let* ((len (+ (window-height window) n-lines))
	 ;; FIXME: this is basically, gross.
	 (above (generate-n-lines-backward (window-buffer window) (window-width window)
					   (max (buffer-min (window-buffer window))
						(1- (marker-position (window-top window))))
					   n-lines))
	 (lines (generate-n-lines-forward (window-buffer window) (window-width window)
					  (cache-item-start 
					   (elt above (max 0 (- (length above) n-lines))))
					  len)))
    ;; if there aren't n-lines left in the buffer then signal
    ;; an end-of-buffer error.
;;     (unless (>= (length above) n-lines)
;;       (error "beginning of buffer"))
    (setf (marker-position (window-top window)) (cache-item-start (elt lines 0)))
    ;; FIXME: for now, set the point at the bottom of the window if it
    ;; isn't visible.
    (let ((eow (elt lines (1- (min (length lines)
				   (window-height window))))))
      (when (or (> (window-point window) (cache-item-end eow))
		(not (point-in-line-cache lines (window-point window))))
	(set-window-point window (cache-item-start eow))))))

(defun window-save-point (window)
  "Save WINDOW's buffer's point to WINDOW-BPOINT."
  (setf (marker-position (window-bpoint window)) (point (window-buffer window))))

(defun window-restore-point (window)
  "Restore the WINDOW's buffer's point from WINDOW-BPOINT."
  ;; restore the point
  (setf (marker-position (buffer-point (window-buffer window)))
	(marker-position (window-bpoint window))))

(defcommand delete-other-windows ()
  (let* ((frame (selected-frame))
	 (cw (get-current-window))
	 (mb (window-tree-find-if (lambda (w)
				    (typep w 'minibuffer-window))
				  (frame-window-tree frame)
				  t)))
    ;; FIXME: This doesn't properly refresh and the window's display
    ;; arrays aren't resized.
    (setf (window-x cw) 0
	  (window-y cw) 0
	  (window-seperator cw) nil
	  (slot-value cw 'w) (frame-width frame)
	  (slot-value cw 'h) (- (frame-height frame) (window-height mb t))
	  (frame-window-tree frame) (list cw mb))
    ;;(update-window-display-arrays cw)
    ))

(defun window-parent (window)
  "Return the parent list in frame-window-tree for WINDOW."
  (labels ((parent-of (tree parent window)
	     (cond ((listp tree)
		    (loop for i in tree
		       thereis (parent-of i tree window)))
		   (t 
		    (when (eq tree window)
		      parent)))))
    (parent-of (frame-window-tree (window-frame window)) nil window)))

(defun delete-window (&optional (window (selected-window)))
  (check-type window window)
  (when (or (typep window minibuffer-window)
	    (typep (frame-window-tree frame) 'window))
    (error "Attempt to delete minibuffer or sole ordinary window")))


(provide :lice-0.1/window)
