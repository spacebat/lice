;;; keymaps for lice

(in-package "LICE")

(defstruct key char control meta alt shift hyper super)
;; (defclass key ()
;;   ((char :type character :initarg :char :reader key-char)
;;    (control :type boolean :initform nil :initarg :control :reader key-control)
;;    (meta :type boolean :initform nil :initarg :meta :reader key-meta)
;;    (alt :type boolean :initform nil :initarg :alt :reader key-alt)
;;    (shift :type boolean :initform nil :initarg :shift :reader key-shift)
;;    (hyper :type boolean :initform nil :initarg :hyper :reader key-hyper)
;;    (super :type boolean :initform nil :initarg :super :reader key-super))
;;   (:documentation "A key event."))

(defun print-mods (key)
  (concatenate 'string
	       (when (key-control key) "C-")
	       (when (key-meta key) "M-")
	       (when (key-alt key) "A-")
	       (when (key-shift key) "S-")
	       (when (key-super key) "s-")
	       (when (key-hyper key) "H-")))

(defun print-key (key)
  (format nil "~a~a" (print-mods key) (or (char-name (key-char key)) (key-char key))))

(defmethod print-object ((obj key) stream)
  (print-unreadable-object (obj stream :type t :identity t)
    (format stream "~s" (print-key obj))))

;; ;; XXX: This is hacky. Convert the class into a sequence. Maybe we should
;; ;; use defstruct then?
;; (defun key-hashid (event)
;;   (typecase event
;;     (key (list (key-char event)
;; 	       (key-control event)
;; 	       (key-meta event)
;; 	       (key-alt event)
;; 	       (key-hyper event)
;; 	       (key-super event)))
;;     (t t)))

(defvar *current-keymap-theme* :lice)

(defvar *overriding-terminal-local-map* nil
  "Per-terminal keymap that overrides all other local keymaps.
If this variable is non-nil, it is used as a keymap instead of the
buffer's local map, and the minor mode keymaps and text property keymaps.
It also replaces `overriding-local-map'.

This variable is intended to let commands such as `universal-argument'
set up a different keymap for reading the next command.")

(defvar *overriding-local-map* nil
  "Keymap that overrides all other local keymaps.
If this variable is non-nil, it is used as a keymap--replacing the
buffer's local map, the minor mode keymaps, and char property keymaps.")

(defclass keymap ()
  ((parent :initform nil :initarg :parent :accessor keymap-parent)
   (prompt :initform nil :initarg :prompt :accessor keymap-prompt)
   (themes :initform (make-hash-table) :accessor keymap-themes)))

(defun make-sparse-keymap (&optional prompt)
  "Construct and return a new sparse keymap.
The optional arg STRING supplies a menu name for the keymap
in case you use it as a menu with `x-popup-menu'."
  (make-instance 'keymap :prompt prompt))

(defun get-keymap-theme (keymap theme)
  (gethash theme (keymap-themes keymap)))

(defun get-keymap-theme-create (keymap theme)
  (or (get-keymap-theme keymap theme)
      (setf (gethash theme (keymap-themes keymap)) (make-hash-table :size 200 :test 'equalp))))

(defun define-key (keymap key def &optional (theme :lice))
  (let ((map (get-keymap-theme-create keymap theme)))
    (setf (gethash #|(key-hashid key)|# key map) def)))

(defun lookup-key-internal (keymap key accept-default theme norecurse)
  (let* ((map (get-keymap-theme keymap theme))
         ;; some maps may not have a hash table for the theme.
         (cmd (and map (gethash #|(key-hashid key)|# key map))))
    (or
     ;; if the binding is another keymap, then lookup the rest of the key sequence
     (cond
       ((and (keymapp cmd) (not norecurse))
        (lookup-key cmd (cdr key) accept-default theme))
       (t cmd))
     ;; check parent for binding
     (when (keymap-parent keymap)
       (lookup-key (keymap-parent keymap) key nil theme))
     (when accept-default
       (and map (gethash t map))))))

(defun lookup-key (keymap key &optional accept-default (theme :lice))
  "In keymap KEYMAP, look up key sequence KEY.  Return the definition.
nil means undefined.  See doc of `define-key' for kinds of definitions.

Normally, `lookup-key' ignores bindings for t, which act as default
bindings, used when nothing else in the keymap applies; this makes it
usable as a general function for probing keymaps.  However, if the
third optional argument ACCEPT-DEFAULT is non-nil, `lookup-key' will
recognize the default bindings, just as `read-key-sequence' does."
  (check-type keymap keymap)
  (lookup-key-internal keymap key accept-default theme nil))

(defun keymapp (object)
  (typep object 'keymap))

(depricate set-keymap-parent (setf keymap-parent))
(defun set-keymap-parent (keymap parent)
  "Modify keymap to set its parent map to parent.
Return parent.  parent should be nil or another keymap."
  (setf (keymap-parent keymap) parent))

(defun make-keymap (&optional string)
  (declare (ignore string))
  (error 'unimplemented))

(defun map-keymap (function keymap &optional (theme :lice))
  "Call FUNCTION once for each event binding in KEYMAP.
FUNCTION is called with two arguments: the event that is bound, and
the definition it is bound to.  If the event is an integer, it may be
a generic character (see Info node `(elisp)Splitting Characters'), and
that means that all actual character events belonging to that generic
character are bound to the definition.

If KEYMAP has a parent, the parent's bindings are included as well.
This works recursively: if the parent has itself a parent, then the
grandparent's bindings are also included and so on."
  (let ((map (get-keymap-theme keymap theme)))
    (maphash function map)
    (when (keymap-parent keymap)
      (map-keymap function (keymap-parent keymap) theme))))

(defvar *global-map* (make-sparse-keymap)
  "The top level global keymap.")

(defvar *ctl-x-4-map* (make-sparse-keymap)
  "The C-x 4 keymap.")

(defvar *ctl-x-map* (make-sparse-keymap)
  "The C-x keymap.")

(defvar *ctl-c-map* (make-sparse-keymap)
  "The C-c keymap.")

(defvar *ctl-h-map* (make-sparse-keymap)
  "The C-h keymap.")

(defvar *current-global-map* *global-map*)

(defvar *current-kmap* nil
  "The key map that the next key event will use to find a
corresponding command.")

(defun make-ctrl-h-map ()
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap (make-key :char #\f) 'describe-symbol)
    kmap))

(defun make-ctrl-x-4-map ()
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap (make-key :char #\b) 'switch-to-buffer-other-window)
    kmap))

(defun make-ctrl-x-map (ctl-x-4-map)
  (let ((kmap (make-sparse-keymap)))
    (define-key kmap (make-key :char #\e :control t) 'eval-last-sexp)
    (define-key kmap (make-key :char #\b) 'switch-to-buffer)
    (define-key kmap (make-key :char #\c :control t) 'save-buffers-kill-emacs)
    (define-key kmap (make-key :char #\f :control t) 'find-file)
    (define-key kmap (make-key :char #\s :control t) 'save-buffer)
    (define-key kmap (make-key :char #\k) 'kill-buffer)
    (define-key kmap (make-key :char #\o) 'other-window)
    (define-key kmap (make-key :char #\1) 'delete-other-windows)
    (define-key kmap (make-key :char #\2) 'split-window-vertically)
    (define-key kmap (make-key :char #\3) 'split-window-horizontally)
    (define-key kmap (make-key :char #\x :control t) 'exchange-point-and-mark)
    (define-key kmap (make-key :char #\t :control t) 'transpose-lines)
    (define-key kmap (make-key :char #\4) ctl-x-4-map)
    kmap))

(defun make-ctrl-c-map ()
  (let ((kmap (make-sparse-keymap)))
    kmap))

(defun make-global-map (ctl-x-prefix ctl-c-prefix ctl-h-prefix)
  "Generate self-insert commands for all printable characters. And
more."
  (let ((kmap (make-sparse-keymap)))
    (loop for i in '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9
		     #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j
		     #\k #\l #\m #\n #\o #\p #\q #\r #\s #\t
		     #\u #\v #\w #\x #\y #\z 
		     #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J
		     #\K #\L #\M #\N #\O #\P #\Q #\R #\S #\T
		     #\U #\V #\W #\X #\Y #\Z
		     #\Space #\! #\" #\# #\$ #\% #\& #\' #\( 
		     #\) #\* #\+ #\, #\- #\. #\/ #\: #\; #\< 
		     #\= #\> #\? #\@ #\[ #\\ #\] #\^ #\_ #\` 
		     #\| #\} #\~ #\{)
	  do (define-key kmap (make-key :char i) 'self-insert-command))
    (define-key kmap (make-key :char #\Return) 'newline)
    (define-key kmap (make-key :char #\Newline) 'newline)
    (define-key kmap (make-key :char #\o :control t) 'open-line)
    (define-key kmap (make-key :char #\j :control t) 'newline)
    (define-key kmap (make-key :char #\m :control t) 'newline)
    (define-key kmap (make-key :char #\f :control t) 'forward-char)
    (define-key kmap (make-key :char #\f :meta t) 'forward-word)
    (define-key kmap (make-key :char #\f :control t :meta t) 'forward-sexp)
    (define-key kmap (make-key :char #\b :control t :meta t) 'backward-sexp)
    (define-key kmap (make-key :char #\n :control t) 'next-line)
    (define-key kmap (make-key :char #\p :control t) 'previous-line)
    (define-key kmap (make-key :char #\b :control t) 'backward-char)
    (define-key kmap (make-key :char #\b :meta t) 'backward-word)
    (define-key kmap (make-key :char #\d :control t) 'delete-char)
    (define-key kmap (make-key :char #\d :meta t) 'kill-word)
    (define-key kmap (make-key :char #\Rubout :meta t) 'backward-kill-word)
    (define-key kmap (make-key :char #\Rubout) 'delete-backward-char)
    (define-key kmap (make-key :char #\Delete) 'delete-backward-char)
    (define-key kmap (make-key :char #\t :meta t) 'transpose-words)
    (define-key kmap (make-key :char #\t :control t) 'transpose-chars)
    ;;(define-key kmap (make-key :char #\h :control t) 'delete-backward-char)
    (define-key kmap (make-key :char #\u :control t) 'universal-argument)
    (define-key kmap (make-key :char #\a :control t) 'beginning-of-line)
    (define-key kmap (make-key :char #\e :control t) 'end-of-line)
    (define-key kmap (make-key :char #\g :control t) 'keyboard-quit)
    (define-key kmap (make-key :char #\v :control t) 'scroll-up)
    (define-key kmap (make-key :char #\v :meta t) 'scroll-down)
    (define-key kmap (make-key :char #\k :control t) 'kill-line)
    (define-key kmap (make-key :char #\w :control t) 'kill-region)
    (define-key kmap (make-key :char #\y :control t) 'yank)
    (define-key kmap (make-key :char #\y :meta t) 'yank-pop)
    (define-key kmap (make-key :char #\w :meta t) 'kill-ring-save)
    (define-key kmap (make-key :char #\> :meta t) 'end-of-buffer)
    (define-key kmap (make-key :char #\< :meta t) 'beginning-of-buffer)
    (define-key kmap (make-key :char #\x :meta t) 'execute-extended-command)
    (define-key kmap (make-key :char #\: :meta t) 'eval-expression)
    (define-key kmap (make-key :char #\Space :control t) 'set-mark-command)
    (define-key kmap (make-key :char #\` :control t) 'set-mark-command)
    (define-key kmap (make-key :char #\! :meta t) 'shell-command)
    (define-key kmap (make-key :char #\Space :meta t) 'just-one-space)
    (define-key kmap (make-key :char #\\ :control t :meta t) 'indent-region)
    (define-key kmap (make-key :char #\a :control t :meta t) 'beginning-of-defun)
    (define-key kmap (make-key :char #\e :control t :meta t) 'end-of-defun)
    (define-key kmap (make-key :char #\_ :control t) 'undo)
    (define-key kmap (make-key :char #\/ :control t) 'undo)
    (define-key kmap (make-key :char #\x :control t) ctl-x-prefix)
    (define-key kmap (make-key :char #\c :control t) ctl-c-prefix)
    (define-key kmap (make-key :char #\h :control t) ctl-h-prefix)
    kmap))

(defun make-global-keymaps ()
  "Create the default global keymaps and store them in *global-kmap
*ctl-x-map*, ..."
  (setf *ctl-x-4-map* (make-ctrl-x-4-map)
	*ctl-x-map* (make-ctrl-x-map *ctl-x-4-map*)
	*ctl-c-map* (make-ctrl-c-map)
	*ctl-h-map* (make-ctrl-h-map)
	*global-map* (make-global-map *ctl-x-map* *ctl-c-map* *ctl-h-map*)))

(defun copy-keymap (keymap)
  (declare (ignore keymap))
  (error 'unimplemented))

(defun command-remapping ()
  (error 'unimplemented))

(defun key-binding (key &optional accept-default no-remap)
  (declare (ignore key accept-default no-remap))
  (error 'unimplemented))

(defun local-key-binding ()
  (error 'unimplemented))

(defun global-key-binding ()
  (error 'unimplemented))

(defun minor-mode-key-binding ()
  (error 'unimplemented))

(defun define-prefix-command ()
  (error 'unimplemented))

(defun use-global-map (keymap)
  (check-type keymap keymap)
  (setf *current-global-map* keymap))

(defun use-local-map (keymap)
  "Select KEYMAP as the local keymap.
If KEYMAP is nil, that means no local keymap.

LICE: a buffer's local map is really the major mode map. Except
it might not be in the future."
  (check-type keymap keymap)
  (error 'unimplemented))

(defun current-local-map ()
  "Return current buffer's local keymap, or nil if it has none.

LICE: the local map in really the major mode map. Except it might
not be in the future."
  (buffer-local-map (current-buffer)))

(defun current-global-map ()
  "Return the current global keymap."
  *current-global-map*)

(defun current-minor-mode-maps ()
  (error 'unimplemented))

(defun current-active-maps ()
  (error 'unimplemented))

(defun accessible-keymaps ()
  (error 'unimplemented))

(defun key-description ()
  (error 'unimplemented))

(defun describe-vector ()
  (error 'unimplemented))

(defun single-key-description ()
  (error 'unimplemented))

(defun text-char-description ()
  (error 'unimplemented))

(defun where-is-internal ()
  (error 'unimplemented))

(defun describe-buffer-bindings ()
  (error 'unimplemented))

(defun apropos-internal ()
  (error 'unimplemented))

