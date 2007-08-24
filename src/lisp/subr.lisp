;;; subr.lice --- basic lisp subroutines for Emacs

(in-package "LICE")

;;; Argument types

(defun interactive (&rest prompts)
  "Read input from the minibuffer and return it in a list."
  (loop for p in prompts
	collect (read-from-minibuffer p)))

(defvar *extended-command-history* nil)

(defun read-command (prompt)
  "Read the name of a command and return as a symbol.
Prompt with prompt.  By default, return default-value."
  (let (cmds)
    (maphash (lambda (k v) 
               (declare (ignore v))
	       (push k cmds))
	     *commands*)
    (dformat +debug-v+ "commands: ~s~%" cmds)
    ;; Sadly, a cheap hack
    (find (completing-read prompt cmds :history '*extended-command-history*)
          cmds :test #'string-equal :key #'symbol-name)))

(defun read-buffer (prompt &optional def require-match)
  "Read the name of a buffer and return as a string.
Prompt with prompt.
Optional second arg def is value to return if user enters an empty line.
*If optional third arg require-match is non-nil,
* only existing buffer names are allowed."
  (declare (ignore require-match))
  (let* ((bufs (mapcar (lambda (b)
                         (cons (buffer-name b) b))
                       *buffer-list*))
         (b (completing-read (if def
                                 (format nil "~a(default ~a) " prompt def)
                               prompt)
                             bufs)))
    (if (zerop (length b))
        def
      b)))

(defun read-file-name (prompt &key dir default-filename mustmatch initial predicate)
  "Read file name, prompting with prompt and completing in directory dir.
Value is not expanded---you must call `expand-file-name' yourself.
Default name to default-filename if user exits the minibuffer with
the same non-empty string that was inserted by this function.
 (If default-filename is omitted, the visited file name is used,
  except that if initial is specified, that combined with dir is used.)
If the user exits with an empty minibuffer, this function returns
an empty string.  (This can only happen if the user erased the
pre-inserted contents or if `insert-default-directory' is nil.)
Fourth arg mustmatch non-nil means require existing file's name.
 Non-nil and non-t means also require confirmation after completion.
Fifth arg initial specifies text to start with.
If optional sixth arg predicate is non-nil, possible completions and
the resulting file name must satisfy (funcall predicate NAME).
dir should be an absolute directory name.  It defaults to the value of
`:default-directory'.

If this command was invoked with the mouse, use a file dialog box if
`use-dialog-box' is non-nil, and the window system or X toolkit in use
provides a file dialog box.

See also `read-file-name-completion-ignore-case'
and `read-file-name-function'."
  (declare (ignore predicate initial mustmatch default-filename dir))
  (completing-read prompt #'file-completions :initial-input (princ-to-string *default-directory*)))

(defun read-string (prompt &optional initial-input history default-value)
  "Read a string from the minibuffer, prompting with string prompt.
If non-nil, second arg initial-input is a string to insert before reading.
  This argument has been superseded by default-value and should normally
  be nil in new code.  It behaves as in `read-from-minibuffer'.  See the
  documentation string of that function for details.
The third arg history, if non-nil, specifies a history list
  and optionally the initial position in the list.
See `read-from-minibuffer' for details of history argument.
Fourth arg default-value is the default value.  If non-nil, it is used
 for history commands, and as the value to return if the user enters
 the empty string.
**Fifth arg inherit-input-method, if non-nil, means the minibuffer inherits
 the current input method and the setting of `enable-multibyte-characters'."
  (read-from-minibuffer prompt :initial-contents initial-input :history history :default-value default-value))

(defun region-limit (beginningp)
  "Return the start or end position of the region.
BEGINNINGP non-zero means return the start.
If there is no region active, signal an error."
  (if beginningp
      (min (point) (mark))
      (max (point) (mark))))

(defun region-beginning ()
  "Return position of beginning of region, as an integer."
  (region-limit t))
  
(defun region-end ()
  "Return position of end of region, as an integer."
  (region-limit nil))

(defun add-command-arg-type (type fn)
  "TYPE is a symbol. Add it to the hash table of command types and link it to FN, a function or function symbol."
  (setf (gethash type *command-arg-type-hash*) fn))

(defun init-command-arg-types ()
  "populate the hash table with some defaults"
  ;; Reset the hash table. FIXME: should we do this? 
  (setf *command-arg-type-hash* (make-hash-table))
  (add-command-arg-type :buffer 'read-buffer)
  (add-command-arg-type :file 'read-file-name)
  (add-command-arg-type :string 'read-from-minibuffer)
  (add-command-arg-type :command 'read-command)
  (add-command-arg-type :prefix 'prefix-arg)
  (add-command-arg-type :raw-prefix 'raw-prefix-arg)
  (add-command-arg-type :region-beginning 'region-beginning)
  (add-command-arg-type :region-end 'region-end))

(defun get-buffer-window-list (buffer &optional minibuf frame)
  "Return list of all windows displaying BUFFER, or nil if none.
BUFFER can be a buffer or a buffer name.
See `walk-windows' for the meaning of MINIBUF and FRAME."
  (let ((buffer (if (bufferp buffer) buffer (get-buffer buffer))) windows)
    (mapc (lambda (window)
	    (if (eq (window-buffer window) buffer)
		(push window windows)))
	  (frame-window-list frame minibuf))
    windows))

;; FIXME: this isn't complete.
(defmacro defalias (from-symbol to-symbol)
"Set symbol's function definition to definition, and return definition."
  `(define-symbol-macro ,from-symbol ,to-symbol))

(defun intern-soft (name &optional (package *package*))
  (find-symbol name package))

;;; reading from the buffer

(defun read-from-buffer (&aux (buffer (current-buffer)))
  "Read 1 sexp from the buffer at the current point, moving the point to the end of what was read"
  (when (< (buffer-char-to-aref buffer (point buffer))
	   (buffer-gap-start buffer))
    (gap-move-to-point buffer))
  (multiple-value-bind (obj pos)
      (read-from-string (buffer-data buffer) t nil
                        :start (buffer-char-to-aref buffer (point buffer)))
    (set-point (buffer-aref-to-char buffer pos))
    obj))

(defcommand eval-region ((start end &optional print-flag (read-function 'read-from-string))
                         :region-beginning :region-end)
  "Execute the region as Lisp code.
When called from programs, expects two arguments,
giving starting and ending indices in the current buffer
of the text to be executed.
Programs can pass third argument PRINTFLAG which controls output:
A value of nil means discard it; anything else is stream for printing it.
Also the fourth argument READ-FUNCTION, if non-nil, is used
instead of `read' to read each expression.  It gets one argument
which is the input stream for reading characters.

This function does not move point."
  (let* ((stdout (make-string-output-stream))
         (*standard-output* stdout)
         (*error-output* stdout)
         (*debug-io* stdout)
         (string (buffer-substring-no-properties start end))
         (pos 0)
         last obj)
    (loop
       (setf last obj)
       (multiple-value-setq (obj pos) (funcall read-function string nil string :start pos))
       (when (eq obj string)
         (cond ((eq print-flag t)
                (message "~s" last)))
         (return-from eval-region last)))))

(defun sit-for (seconds &optional nodisp)
  "Perform redisplay, then wait for seconds seconds or until input is available.
seconds may be a floating-point value, meaning that you can wait for a
fraction of a second.
 (Not all operating systems support waiting for a fraction of a second.)
Optional arg nodisp non-nil means don't redisplay, just wait for input.
Redisplay is preempted as always if input arrives, and does not happen
if input is available before it starts.
Value is t if waited the full time with no input arriving."
  (unless nodisp
    (frame-render (selected-frame)))
  ;; FIXME: poll for input
  (sleep seconds)
  t
;;   (let ((event (wait-for-event seconds)))
;;     (if event
;;         (progn
;;           (push event *unread-command-events*)
;;           nil)
;;         t))
  )


;;; Matching and match data
(defun match-string (num &optional string)
  "Return string of text matched by last search.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING."
  (if (match-beginning num)
      (if string
	  (substring string (match-beginning num) (match-end num))
	(buffer-substring (match-beginning num) (match-end num)))))

(defun match-string-no-properties (num &optional string)
  "Return string of text matched by last search, without text properties.
NUM specifies which parenthesized expression in the last regexp.
 Value is nil if NUMth pair didn't match, or there were less than NUM pairs.
Zero means the entire text matched by the whole regexp or whole string.
STRING should be given if the last search was by `string-match' on STRING."
  (if (match-beginning num)
      (if string
	  (substring-no-properties string (match-beginning num)
				   (match-end num))
	(buffer-substring-no-properties (match-beginning num)
					(match-end num)))))


(defun force-mode-line-update (&optional all)
  "Force redisplay of the current buffer's mode line and header line.
With optional non-nil ALL, force redisplay of all mode lines and
header lines.  This function also forces recomputation of the
menu bar menus and the frame title."
  (declare (ignore all))
  (error "unimplemented force-mode-line-update")
;;   (if all (save-excursion (set-buffer (other-buffer))))
;;   (set-buffer-modified-p (buffer-modified-p))
  )

(defun add-minor-mode (toggle name &optional keymap after toggle-fun)
  "Register a new minor mode.

This is an XEmacs-compatibility function.  Use `define-minor-mode' instead.

TOGGLE is a symbol which is the name of a buffer-local variable that
is toggled on or off to say whether the minor mode is active or not.

NAME specifies what will appear in the mode line when the minor mode
is active.  NAME should be either a string starting with a space, or a
symbol whose value is such a string.

Optional KEYMAP is the keymap for the minor mode that will be added
to `*minor-mode-map-list*'.

Optional AFTER specifies that TOGGLE should be added after AFTER
in `*minor-mode-list*'.

Optional TOGGLE-FUN is an interactive function to toggle the mode.
It defaults to (and should by convention be) TOGGLE.

If TOGGLE has a non-nil `:included' property, an entry for the mode is
included in the mode-line minor mode menu.
If TOGGLE has a `:menu-tag', that is used for the menu item's label."
  (unless (memq toggle minor-mode-list)
    (push toggle minor-mode-list))

  (unless toggle-fun (setq toggle-fun toggle))
  (unless (eq toggle-fun toggle)
    (setf (get toggle :minor-mode-function) toggle-fun))
  ;; Add the name to the *minor-mode-list*.
  (when name
    (let ((existing (find toggle *minor-mode-list* :key 'first)))
      (if existing
	  (setf (cdr existing) (list name))
	(let ((found (member after *minor-mode-list* :key 'first)))
	  (if found
	      (let ((rest (cdr found)))
		(setf (cdr found) nil)
		(nconc found (list (list toggle name)) rest))
              (push (cons (list toggle name)
                          *minor-mode-list*) *minor-mode-list*))))))
;; FIXME: when menu support is added, use this code
;;   ;; Add the toggle to the minor-modes menu if requested.
;;   (when (get toggle :included)
;;     (define-key mode-line-mode-menu
;;       (vector toggle)
;;       (list 'menu-item
;; 	    (concat
;; 	     (or (get toggle :menu-tag)
;; 		 (if (stringp name) name (symbol-name toggle)))
;; 	     (let ((mode-name (if (symbolp name) (symbol-value name))))
;; 	       (if (and (stringp mode-name) (string-match "[^ ]+" mode-name))
;; 		   (concat " (" (match-string 0 mode-name) ")"))))
;; 	    toggle-fun
;; 	    :button (cons :toggle toggle))))

  ;; Add the map to the *minor-mode-map-list*.
  (when keymap
    (let ((existing (find toggle *minor-mode-map-list* :key 'minor-mode-map-variable)))
      (if existing
	  (setf (minor-mode-map-keymap existing) keymap)
	(let ((found (member after *minor-mode-map-list* :key 'minor-mode-map-variable)))
	  (if found
	      (let ((rest (cdr found)))
		(setf (cdr found) nil)
		(nconc found (list (make-minor-mode-map :variable toggle :keymap keymap)) rest))
	    (push (make-minor-mode-map :variable toggle :keymap keymap)
                  *minor-mode-map-list*)))))))


(defun replace-regexp-in-string (regexp rep string &optional
                                 fixedcase literal subexp start)
  "Replace all matches for REGEXP with REP in STRING.

Return a new string containing the replacements.

Optional arguments FIXEDCASE, LITERAL and SUBEXP are like the
arguments with the same names of function `replace-match'.  If START
is non-nil, start replacements at that index in STRING.

REP is either a string used as the NEWTEXT arg of `replace-match' or a
function.  If it is a function, it is called with the actual text of each
match, and its value is used as the replacement text.  When REP is called,
the match-data are the result of matching REGEXP against a substring
of STRING.

To replace only the first match (if any), make REGEXP match up to \\'
and replace a sub-expression, e.g.
  (replace-regexp-in-string \"\\\\(foo\\\\).*\\\\'\" \"bar\" \" foo foo\" nil nil 1)
    => \" bar foo\"
"

  ;; To avoid excessive consing from multiple matches in long strings,
  ;; don't just call `replace-match' continually.  Walk down the
  ;; string looking for matches of REGEXP and building up a (reversed)
  ;; list MATCHES.  This comprises segments of STRING which weren't
  ;; matched interspersed with replacements for segments that were.
  ;; [For a `large' number of replacements it's more efficient to
  ;; operate in a temporary buffer; we can't tell from the function's
  ;; args whether to choose the buffer-based implementation, though it
  ;; might be reasonable to do so for long enough STRING.]
  (let ((l (length string))
	(start (or start 0))
	matches str mb me)
    (with-match-data
      (while (and (< start l) (string-match regexp string start))
	(setq mb (match-beginning 0)
	      me (match-end 0))
	;; If we matched the empty string, make sure we advance by one char
	(when (= me mb) (setq me (min l (1+ mb))))
	;; Generate a replacement for the matched substring.
	;; Operate only on the substring to minimize string consing.
	;; Set up match data for the substring for replacement;
	;; presumably this is likely to be faster than munging the
	;; match data directly in Lisp.
	(string-match regexp (setq str (substring string mb me)))
	(setq matches
	      (cons (replace-match (if (stringp rep)
				       rep
				     (funcall rep (match-string 0 str)))
				   fixedcase literal str subexp)
		    (cons (substring string start mb)       ; unmatched prefix
			  matches)))
	(setq start me))
      ;; Reconstruct a string from the pieces.
      (setq matches (cons (substring string start l) matches)) ; leftover
      (apply #'concat (nreverse matches)))))


;;;; Key binding commands.

(defcommand global-set-key ((key command)
                            (:key "Set key globally: ")
                            (:command "Set key ~a to command: "))
  "Give KEY a global binding as COMMAND.
COMMAND is the command definition to use; usually it is
a symbol naming an interactively-callable function.
KEY is a key sequence; noninteractively, it is a string or vector
of characters or event types, and non-ASCII characters with codes
above 127 (such as ISO Latin-1) can be included if you use a vector.

Note that if KEY has a local binding in the current buffer,
that local binding will continue to shadow any global binding
that you make with this function."
  ;;(interactive "KSet key globally: \nCSet key %s to command: ")
  (or (vectorp key) (stringp key) (symbolp key) (clickp key)
      (signal 'wrong-type-argument :type (list 'arrayp key)))
  (define-key (current-global-map) key command))

(defcommand local-set-key ((key command)
                           (:key "Set key locally: ")
                           (:command "Set key ~a locally to command: "))
  "Give KEY a local binding as COMMAND.
COMMAND is the command definition to use; usually it is
a symbol naming an interactively-callable function.
KEY is a key sequence; noninteractively, it is a string or vector
of characters or event types, and non-ASCII characters with codes
above 127 (such as ISO Latin-1) can be included if you use a vector.

The binding goes in the current buffer's local map,
which in most cases is shared with all other buffers in the same major mode."
  ;;(interactive "KSet key locally: \nCSet key %s locally to command: ")
  (let ((map (current-local-map)))
    (or map
	(use-local-map (setq map (make-sparse-keymap))))
    (or (vectorp key) (stringp key)
	(signal 'wrong-type-argument (list 'arrayp key)))
    (define-key map key command)))

(defun global-unset-key (key)
  "Remove global binding of KEY.
KEY is a string or vector representing a sequence of keystrokes."
  (interactive "kUnset key globally: ")
  (global-set-key key nil))

(defun local-unset-key (key)
  "Remove local binding of KEY.
KEY is a string or vector representing a sequence of keystrokes."
  (interactive "kUnset key locally: ")
  (if (current-local-map)
      (local-set-key key nil))
  nil)


;;;; substitute-key-definition and its subroutines.

(defvar key-substitution-in-progress nil
 "Used internally by `substitute-key-definition'.")

(defun substitute-key-definition (olddef newdef keymap &optional oldmap prefix)
  "Replace OLDDEF with NEWDEF for any keys in KEYMAP now defined as OLDDEF.
In other words, OLDDEF is replaced with NEWDEF where ever it appears.
Alternatively, if optional fourth argument OLDMAP is specified, we redefine
in KEYMAP as NEWDEF those keys which are defined as OLDDEF in OLDMAP.

If you don't specify OLDMAP, you can usually get the same results
in a cleaner way with command remapping, like this:
  \(define-key KEYMAP [remap OLDDEF] NEWDEF)
\n(fn OLDDEF NEWDEF KEYMAP &optional OLDMAP)"
  ;; Don't document PREFIX in the doc string because we don't want to
  ;; advertise it.  It's meant for recursive calls only.  Here's its
  ;; meaning

  ;; If optional argument PREFIX is specified, it should be a key
  ;; prefix, a string.  Redefined bindings will then be bound to the
  ;; original key, with PREFIX added at the front.
  (or prefix (setq prefix ""))
  (let* ((scan (or oldmap keymap))
	 (prefix1 (vconcat prefix [nil]))
	 (key-substitution-in-progress
	  (cons scan key-substitution-in-progress)))
    ;; Scan OLDMAP, finding each char or event-symbol that
    ;; has any definition, and act on it with hack-key.
    (map-keymap
     (lambda (char defn)
       (aset prefix1 (length prefix) char)
       (substitute-key-definition-key defn olddef newdef prefix1 keymap))
     scan)))

(defun substitute-key-definition-key (defn olddef newdef prefix keymap)
  (let (inner-def skipped menu-item)
    ;; Find the actual command name within the binding.
    (el:if (eq (car-safe defn) 'menu-item)
	(setq menu-item defn defn (nth 2 defn))
      ;; Skip past menu-prompt.
      (while (stringp (car-safe defn))
	(push (pop defn) skipped))
      ;; Skip past cached key-equivalence data for menu items.
      (if (consp (car-safe defn))
	  (setq defn (cdr defn))))
    (el:if (or (eq defn olddef)
	    ;; Compare with equal if definition is a key sequence.
	    ;; That is useful for operating on function-key-map.
	    (and (or (stringp defn) (vectorp defn))
		 (equal defn olddef)))
	(define-key keymap prefix
	  (if menu-item
	      (let ((copy (copy-sequence menu-item)))
		(setcar (nthcdr 2 copy) newdef)
		copy)
	    (nconc (nreverse skipped) newdef)))
      ;; Look past a symbol that names a keymap.
      (setq inner-def
	    (or (indirect-function defn t) defn))
      ;; For nested keymaps, we use `inner-def' rather than `defn' so as to
      ;; avoid autoloading a keymap.  This is mostly done to preserve the
      ;; original non-autoloading behavior of pre-map-keymap times.
      (if (and (keymapp inner-def)
	       ;; Avoid recursively scanning
	       ;; where KEYMAP does not have a submap.
	       (let ((elt (lookup-key keymap prefix)))
		 (or (null elt) (natnump elt) (keymapp elt)))
	       ;; Avoid recursively rescanning keymap being scanned.
	       (not (memq inner-def key-substitution-in-progress)))
	  ;; If this one isn't being scanned already, scan it now.
	  (substitute-key-definition olddef newdef keymap inner-def prefix)))))


(provide :lice-0.1/subr)
