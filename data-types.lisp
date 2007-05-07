;;; declare all our data types

(in-package "LICE")


;;; Markers

(deftype marker-insertion-type () '(member :before :after))

(defclass marker ()
  ((position :type integer :initform 0 :accessor marker-position)
   (buffer #|:type (or buffer null)|# :initform nil :accessor marker-buffer)
   (insertion-type :type marker-insertion-type :initform :after :accessor marker-insertion-type))
  (:documentation "A Marker"))

(defmethod print-object ((obj marker) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a" (marker-position obj))))


;;; Intervals

;; interval node is a list: (key left right &rest plist)
(defstruct (interval
             (:print-function (lambda (i s d)
                                (declare (ignore d))
                                (format s "#S(interval ~s ~s ~s | ~s ~s)" 
                                        (interval-pt i)
                                        (interval-length i)
                                        (interval-plist i)
                                        (interval-left i)
                                        (interval-right i)))))
  (pt nil)
  (length nil)
  (left nil)
  (right nil)
  (parent nil #|:type (or null pstring buffer interval)|#)
  (plist nil :type list))

;; MOVITZ's defstruct doesn't create copy-interval 
#+movitz
(defun copy-interval (interval)
  (make-interval :pt (interval-pt interval)
		 :length (interval-length interval)
		 :left (interval-left interval)
		 :right (interval-right interval)
		 :parent (interval-parent interval)
		 :plist (interval-plist interval)))


;;; Buffers

(defclass pstring ()
  ((data :type string :initarg :data :accessor pstring-data)
   (intervals #|:type (or null interval)|# :initform nil :initarg :intervals :accessor intervals))
  (:documentation "The lice string implementation."))

(defmethod print-object ((obj pstring) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~s" (pstring-data obj))))

(defun pstring-length (ps)
  "Return the length of the string in PS"
  (declare (type pstring ps))
  (length (pstring-data ps)))

(defclass base-buffer ()
  ((file :type (or null pathname) :initarg :file :accessor buffer-file)
   (name :type string :initarg :name :accessor buffer-name)
   (mode-line-string :type string :initform "" :accessor buffer-mode-line-string)
   (modified :type boolean :initform nil :accessor buffer-modified-p)
   (read-only :type boolean :initform nil :accessor buffer-read-only)
   (tick :type integer :initform 0 :accessor buffer-modified-tick :documentation
	 "The buffer's tick counter. It is incremented for each change
in text.")
   (display-count :type integer :initform 0 :accessor buffer-display-count :documentation
		  "The buffer's display counter. It is incremented each time it
is displayed in a window.")
   (display-time :type integer :initform 0 :accessor buffer-display-time :documentation
		 "The last time the buffer was switched to in a window.")
   (major-mode #|:type major-mode|# :initarg :major-mode :accessor buffer-major-mode)
   (local-map :initform nil :initarg :local-map :accessor buffer-local-map :documentation
           "The keymap local to the buffer. This overrides major mode bindings.")
   (syntax-table :initform nil :initarg :syntax-table :accessor buffer-syntax-table)
   (locals-variables :type hash-table :initform (make-hash-table) :accessor buffer-local-variables)
   (locals :type hash-table :initform (make-hash-table) :accessor buffer-locals))
  (:documentation "A Buffer."))

;; undo structures used to record types of undo information. This is
;; an alternative to the cons cells gnu emacs uses which I find
;; obscure.
(defstruct undo-entry-insertion
  beg end)
(defstruct undo-entry-delete
  text position)
(defstruct undo-entry-modified
  time)
(defstruct undo-entry-property
  prop value beg end)
(defstruct undo-entry-apply
  function args)
(defstruct undo-entry-selective
  delta beg end function args)
(defstruct undo-entry-marker
  marker distance)

(defclass buffer (base-buffer)
  ((point #|:type marker|# :initarg :point :accessor buffer-point)
   (mark #|:type marker|# :initarg :mark :accessor buffer-mark-marker)
   ;; A string containing the raw buffer
   (data :type (array character 1) :initarg :data :accessor buffer-data)
   (intervals #|:type (or null interval)|# :initform nil :initarg :intervals :accessor intervals)
   (gap-start :type integer :initarg :gap-start :accessor buffer-gap-start)
   (gap-size :type integer :initarg :gap-size :accessor buffer-gap-size)
   (markers :type list :initform '() :accessor buffer-markers)
   (auto-save-modified :type integer :initform 0 :accessor buffer-auto-save-modified)
   (modiff :type integer :initform 0 :accessor buffer-modiff)
   ;;(syntax-table :initform *standard-syntax-table* :accessor buffer-syntax-table)
   (undo-list :initform '() :accessor buffer-undo-list
              :documentation "List of undo entries in current buffer.
Recent changes come first; older changes follow newer.

An entry (BEG . END) represents an insertion which begins at
position BEG and ends at position END.

An entry (TEXT . POSITION) represents the deletion of the string TEXT
from (abs POSITION).  If POSITION is positive, point was at the front
of the text being deleted; if negative, point was at the end.

An entry (t HIGH . LOW) indicates that the buffer previously had
\"unmodified\" status.  HIGH and LOW are the high and low 16-bit portions
of the visited file's modification time, as of that time.  If the
modification time of the most recent save is different, this entry is
obsolete.

An entry (nil PROPERTY VALUE BEG . END) indicates that a text property
was modified between BEG and END.  PROPERTY is the property name,
and VALUE is the old value.

An entry (apply FUN-NAME . ARGS) means undo the change with
\(apply FUN-NAME ARGS).

An entry (apply DELTA BEG END FUN-NAME . ARGS) supports selective undo
in the active region.  BEG and END is the range affected by this entry
and DELTA is the number of bytes added or deleted in that range by
this change.

An entry (MARKER . DISTANCE) indicates that the marker MARKER
was adjusted in position by the offset DISTANCE (an integer).

An entry of the form POSITION indicates that point was at the buffer
location given by the integer.  Undoing an entry of this form places
point at POSITION.

nil marks undo boundaries.  The undo command treats the changes
between two undo boundaries as a single step to be undone.

If the value of the variable is t, undo information is not recorded.
"))
  (:documentation "A text Buffer."))

(defmethod print-object ((obj buffer) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~a" (buffer-name obj))))

(defvar *current-buffer* nil
  "When this buffer is non-nil, it contains the current buffer. Calls
to `current-buffer' return this buffer. Otherwise, `current-buffer'
returns the current frames's current window's buffer.

This variable should never be set using `setq' or `setf'. Bind it with
`let' for as long as it needs to be set.")

(defun current-buffer ()
  "Return the current buffer."
  *current-buffer*)

;;; Windows

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

(defclass minibuffer-window (window)
  ())

(defvar *selected-window* nil
  "The window that the cursor now appears in and commands apply to.")

;;; frames

(defclass frame ()
  ((window-tree :type (or list window) :initarg :window-tree :accessor frame-window-tree)
   (width :type fixnum :initarg :width :accessor frame-width)
   (height :type fixnum :initarg :height :accessor frame-height)
   (minibuffer-window :type window :initarg :minibuffer-window :accessor frame-minibuffer-window)
   (minibuffers-active :type fixnum :initform 0 :initarg minibuffers-active :accessor frame-minibuffers-active)
   (selected-window :type window :initarg :selected-window :accessor frame-selected-window))
  (:documentation "A Lice frame is super cool."))

;; XXX: This is only temporary
(defvar *selected-frame* nil
  "The frame that accepts input.")

;;; Events

(defvar *last-point-position* nil
  "The value of point when the last command was started.")

(defvar *last-point-position-buffer* nil
  "The buffer that was current when the last command was started.")

(defvar *last-point-position-window* nil
  "The window that was selected when the last command was started.")

(defvar *current-event* nil
  "The current event being processed.")
