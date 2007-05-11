;;; Simple built-in editing commands.

(in-package "LICE")

(defun forward-point (n)
  "Return buffer position N characters after (before if N negative) point."
  (check-type n integer)
  (+ (pt) n))

(defcommand forward-char ((&optional (n 1))
			  :prefix)
  "Move the point forward N characters in the current buffer."
  (incf (marker-position (buffer-point (current-buffer))) n)
  (cond ((< (pt) (begv))
	 (set-point (begv))
	 (signal 'beginning-of-buffer))
	((> (pt) (zv))
	 (set-point (zv))
	 (signal 'end-of-buffer))))

(defcommand backward-char ((&optional (n 1))
			   :prefix)
  (forward-char (- n)))

(defun forward-line (n)
  "Move n lines forward (backward if n is negative).
Precisely, if point is on line I, move to the start of line I + n.
If there isn't room, go as far as possible (no error).
Returns the count of lines left to move.  If moving forward,
that is n - number of lines moved; if backward, n + number moved.
With positive n, a non-empty line at the end counts as one line
  successfully moved (for the return value)."
  (cond ((and (> n 0)
	      (= (pt) (zv)))
	 (signal 'end-of-buffer))
	((and (< n 0)
	      (= (pt) (begv)))
	 (signal 'beginning-of-buffer)))
  (if (> n 0)
      (multiple-value-bind (p lines) (buffer-scan-newline (current-buffer) 
                                                          (pt)
                                                          (1- (buffer-size (current-buffer)))
                                                          n)
        ;; Increment p by one so the point is at the beginning of the
        ;; line.
        (when (or (char= (buffer-char-after (current-buffer) p) #\Newline)
                  (= p (1- (buffer-size (current-buffer)))))
          (incf p))
        (set-point p)
        (when (zerop lines)
          (signal 'end-of-buffer))
        (- n lines))
      (if (and (= n 0)
               (not (buffer-char-before (current-buffer) (pt))))
          0
          ;; A little mess to figure out how many newlines to search
          ;; for to give the proper output.
          (let ((lines (if (and (buffer-char-after (current-buffer) (pt))
                                (char= (buffer-char-after (current-buffer) (pt)) #\Newline))
                           (- n 2)
                           (1- n))))
            (multiple-value-bind (p flines) 
                (buffer-scan-newline (current-buffer) 
                                     (pt) (begv)
                                     lines)
              (when (and (char= (buffer-char-after (current-buffer) p) #\Newline)
                         (= flines (- lines)))
                (incf p))
              (set-point p)
              (when (and (< n 0)
                         (zerop flines))
                (signal 'beginning-of-buffer))	
              (+ n flines))))))

(defun beginning_of_line ()
  (error "unimplemented"))

(defun end_of_line ()
  (error "unimplemented"))

(defcommand delete-char ()
  "Delete the following N characters."
  (buffer-delete (current-buffer) (pt) 1))

(defcommand delete-backward-char ()
  "Delete the previous N characters."
  (buffer-delete (current-buffer) (pt) -1))

(defcommand self-insert-command ((arg)
				 :prefix)
  "Insert the character you type.
Whichever character you type to run this command is inserted."
  (dformat +debug-v+ "currentb: ~a ~a~%" (current-buffer) *current-buffer*)
  (if (>= arg 2)
      (insert-move-point (current-buffer) (make-string arg :initial-element (key-char *current-event*)))
    (when (> arg 0)
      (insert-move-point (current-buffer) (key-char *current-event*)))))

;;; Key bindings

(define-key *global-map* "C-i" 'self-insert-command)

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
   do (define-key *global-map* (make-key :char i) 'self-insert-command))

(define-key *global-map* "C-a" 'beginning-of-line)
(define-key *global-map* "C-b" 'backward-char)
(define-key *global-map* "C-d" 'delete-char)
(define-key *global-map* "C-e" 'end-of-line)
(define-key *global-map* "C-f" 'forward-char)
(define-key *global-map* "DEL" 'delete-backward-char)