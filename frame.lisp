(in-package :lice)

(defvar *frame-list* nil
  "List of frames lice frames.")

(defun set-frame-minibuffer (frame minibuffer)
  "Make MINIBUFFER the minibuffer for FRAME."
  (setf	(window-buffer (frame-minibuffer-window frame)) minibuffer))

(defun resize-window (window amount &optional (dir :height))
  "grow or shrink window, resizing dependant windows as well."
  (declare (ignore window amount dir))
;;   (let* ((frame (frame-window-tree (frame-for-window window)))
;; 	 (sibling (tree-sibling frame window)))
;;     )
  )

(defun selected-frame ()
  "Return the frame that is now selected."
  *selected-frame*)

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

(defun framep (object)
  "Return non-nil if OBJECT is a frame.
Value is t for a termcap frame (a character-only terminal),
`x' for an Emacs frame that is really an X window,
`w32' for an Emacs frame that is a window on MS-Windows display,
`mac' for an Emacs frame on a Macintosh display,
`pc' for a direct-write MS-DOS frame.
See also `frame-live-p'."
  (typep object 'frame))

(defun frame-live-p ()
  (error "unimplemented"))

(defun make-terminal-frame ()
  (error "unimplemented"))

(defun handle-switch-frame ()
  (error "unimplemented"))

(defun select-frame (frame)
  "Select the frame FRAME.
Subsequent editing commands apply to its selected window.
The selection of FRAME lasts until the next time the user does
something to select a different frame, or until the next time this
function is called.  If you are using a window system, the previously
selected frame may be restored as the selected frame after return to
the command loop, because it still may have the window system's input
focus.  On a text-only terminal, the next redisplay will display FRAME.

This function returns FRAME, or nil if FRAME has been deleted."
  (declare (ignore frame))
  (error "unimplemented"))

(defun frame-root-window ()
  (error "unimplemented"))

(defun frame-first-window ()
  (error "unimplemented"))

(depricate set-frame-selected-window (setf frame-selected-window))
(defun set-frame-selected-window (frame window)
  "Set the selected window of frame object frame to window.
Return window.
If frame is nil, the selected frame is used.
If frame is the selected frame, this makes window the selected window."
  (setf (frame-selected-window (or frame (selected-frame))) window))

(defun frame-list ()
  "Return a list of all frames."
  (copy-list *frame-list*))

(defun next-frame ()
  (error "unimplemented"))

(defun previous-frame ()
  (error "unimplemented"))

(defun delete-frame ()
  (error "unimplemented"))

(defun mouse-position ()
  (error "unimplemented"))

(defun mouse-pixel-position ()
  (error "unimplemented"))

(defun set-mouse-position ()
  (error "unimplemented"))

(defun set-mouse-pixel-position ()
  (error "unimplemented"))

(defun make-frame-visible ()
  (error "unimplemented"))

(defun make-frame-invisible ()
  (error "unimplemented"))

(defun iconify-frame ()
  (error "unimplemented"))

(defun frame-visible-p ()
  (error "unimplemented"))

(defun visible-frame-list ()
  (error "unimplemented"))

(defun raise-frame ()
  (error "unimplemented"))

(defun lower-frame ()
  (error "unimplemented"))

(defun redirect-frame-focus ()
  (error "unimplemented"))

(defun frame-focus ()
  (error "unimplemented"))

(defun frame-parameters ()
  (error "unimplemented"))

(defun frame-parameter ()
  (error "unimplemented"))

(defun modify-frame-parameters ()
  (error "unimplemented"))

(defun frame-char-height ()
  (error "unimplemented"))

(defun frame-char-width ()
  (error "unimplemented"))

(defun frame-pixel-height ()
  (error "unimplemented"))

(defun frame-pixel-width ()
  (error "unimplemented"))

(defun set-frame-height ()
  (error "unimplemented"))

(defun set-frame-width ()
  (error "unimplemented"))

(defun set-frame-size ()
  (error "unimplemented"))

(defun set-frame-position ()
  (error "unimplemented"))


;; (defun x-get-resource ()
;;   (error "unimplemented"))

;; (defun x-parse-geometry ()
;;   (error "unimplemented"))

(provide :lice-0.1/frame)
