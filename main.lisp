(in-package :lice)

;; 
#+cmu (setf extensions:*gc-notify-after* (lambda (&rest r))
	    extensions:*gc-notify-before* (lambda (&rest r)))

(defun lice ()
  "Run the lice environment."
  (unwind-protect
      (progn
	(dformat +debug-v+ "-- Start lice~%")
	#+(or cmu sbcl) (init-tty)
	#+clisp (init-clisp)
	(setf *buffer-list* nil)
	#+movitz (init-commands)
	(make-default-buffers)
	(init-command-arg-types)
	(setf *frame-list* (list #+(or cmu sbcl) (make-default-tty-frame (get-buffer "*scratch*"))
				 #+clisp (make-default-clisp-frame (get-buffer "*scratch*"))
                                 #+mcl (make-default-mcl-frame (get-buffer "*scratch*"))
				 #+movitz (make-default-movitz-frame (get-buffer "*scratch*")))
	      *current-frame* (car *frame-list*)
	      *process-list* nil)
	;; for the scratch buffer
	(set-buffer (get-buffer "*scratch*"))
	(set-major-mode lisp-interaction-mode)
	(make-global-keymaps)
	(catch 'lice-quit 
	  #+clisp
	  (ext:with-keyboard
	      (loop
		 (with-simple-restart (recursive-edit-top-level "Return to LiCE top level")
		   (recursive-edit))))
	  #-clisp
	  (loop
	     (with-simple-restart (recursive-edit-top-level "Return to LiCE top level")
	       (recursive-edit)))))
    (progn 
      #+(or cmu sbcl) (shutdown-tty)
      #+clisp (shutdown)
      (dformat +debug-v+ "-- End lice~%"))))

#+movitz
(defun init-commands ()
  "Our special wikked hack."
  (macrolet ((create-cmd (name &rest args)
			 (let ((tmp (gensym)))
			   `(setf (gethash ',name *commands*)
				  (make-instance
				   'command
				   :name ',name
				   :args ',(delete nil (mapcar (lambda (a)
								 (when (listp a) (second a)))
							       args))
				   :doc nil
				   :fn (lambda ()
					 (let ((,tmp (list ,@(delete nil (mapcar (lambda (a)
										   (when (listp a)
										     `(funcall (gethash ,(second a) *command-arg-type-hash*) ,@(cddr a))))
										 args)))))
					   ;; XXX: Is this a sick hack?  We need to reset the
					   ;; prefix-arg at the right time. After the command
					   ;; is executed we can't because the universal
					   ;; argument code sets the prefix-arg for the next
					   ;; command. The Right spot seems to be to reset it
					   ;; once a command is about to be executed, and
					   ;; after the prefix arg has been gathered to be
					   ;; used in the command. Which is right here.
					   (setf *prefix-arg* nil)
					   ;; Note that we use the actual function. If the
					   ;; function is redefined, the command will
					   ;; continue to be defined and will call the
					   ;; function declared above, not the redefined one.
					   (apply #',name ,tmp))))))))
    (setf fundamental-mode
	  (make-instance 'major-mode
			 :name "Fundamental"
			 :map (make-hash-table)
			 :init-fn (lambda ()))
	  minibuffer-read-mode
	  (make-instance 'major-mode
			 :name "minibuffer mode"
			 :map (let ((m (make-sparse-keymap)))
				(define-key m (make-instance 'key :char #\m :control t) 'exit-minibuffer)
				(define-key m (make-instance 'key :char #\Newline) 'exit-minibuffer)
				(define-key m (make-instance 'key :char #\j :control t) 'exit-minibuffer)
				(define-key m (make-instance 'key :char #\p :meta t) 'previous-history-element)
				(define-key m (make-instance 'key :char #\n :meta t) 'next-history-element)
				(define-key m (make-instance 'key :char #\g :control t) 'abort-recursive-edit)
				m)
			 :init-fn (lambda ()))
	  minibuffer-complete-mode
	  (make-instance 'major-mode 
			 :name "minibuffer complete mode"
			 :map (let ((m (make-sparse-keymap)))
				(define-key m (make-instance 'key :char #\m :control t) 'minibuffer-complete-and-exit)
				(define-key m (make-instance 'key :char #\Newline) 'minibuffer-complete-and-exit)
				(define-key m (make-instance 'key :char #\j :control t) 'minibuffer-complete-and-exit)
				(define-key m (make-instance 'key :char #\p :meta t) 'previous-history-element)
				(define-key m (make-instance 'key :char #\n :meta t) 'next-history-element)
				(define-key m (make-instance 'key :char #\i :control t) 'minibuffer-complete)
				(define-key m (make-instance 'key :char #\Tab) 'minibuffer-complete)
				(define-key m (make-instance 'key :char #\g :control t) 'abort-recursive-edit)
				m)
			 :init-fn (lambda ()))
	  lisp-interaction-mode
	  (make-instance 'major-mode :name "Lisp Interaction"
			 :map (let ((m (make-sparse-keymap)))
				(define-key m (make-instance 'key :char #\j :control t) 'eval-print-last-sexp)
				m)
			 :init-fn (lambda ()))
	  *universal-argument-map*
	  (let ((map (make-sparse-keymap)))
	    (define-key map t 'universal-argument-other-key)
	    (define-key map (kbd "C-u") 'universal-argument-more)
	    (define-key map (kbd "-") 'universal-argument-minus)
	    (define-key map (kbd "0") 'digit-argument)
	    (define-key map (kbd "1") 'digit-argument)
	    (define-key map (kbd "2") 'digit-argument)
	    (define-key map (kbd "3") 'digit-argument)
	    (define-key map (kbd "4") 'digit-argument)
	    (define-key map (kbd "5") 'digit-argument)
	    (define-key map (kbd "6") 'digit-argument)
	    (define-key map (kbd "7") 'digit-argument)
	    (define-key map (kbd "8") 'digit-argument)
	    (define-key map (kbd "9") 'digit-argument)
	    map))
    (setf *mode-line-format* (list "--:" ;; fake it for hype
				       (lambda (buffer)
					 (format nil "~C~C"
					      ;; FIXME: add read-only stuff
					      (if (buffer-modified buffer)
						  #\* #\-)
					      (if (buffer-modified buffer)
						  #\* #\-)))
				       "  "
				       (lambda (buffer)
					 (format nil "~12,,,a" (buffer-name buffer)))
				       "   "
				       (lambda (buffer)
					 (format nil "(~a)" 
					      (major-mode-name (buffer-major-mode buffer))))))
    (setf *commands* (make-hash-table :size 100))
    (create-cmd forward-sexp)
    (create-cmd backward-sexp)
    (create-cmd eval-last-sexp)
    (create-cmd eval-print-last-sexp)
    (create-cmd lisp-interaction-mode)
    (create-cmd ask-user)
    (create-cmd exit-minibuffer)
    (create-cmd abort-recursive-edit)
    (create-cmd minibuffer-complete-and-exit)
    (create-cmd next-history-element (n :prefix))
    (create-cmd previous-history-element)
    (create-cmd minibuffer-complete)
    (create-cmd forward-char (n :prefix))
    (create-cmd backward-char (n :prefix))
    (create-cmd self-insert-command (arg :prefix))
    (create-cmd newline)
    (create-cmd next-line (n :prefix))
    (create-cmd previous-line (n :prefix))
    (create-cmd delete-backward-char)
    (create-cmd delete-char)
    (create-cmd beginning-of-line)
    (create-cmd end-of-line)
    (create-cmd erase-buffer)
    (create-cmd execute-extended-command (n :prefix))
    (create-cmd switch-to-buffer (buffer :buffer "Switch To Buffer: " (buffer-name (other-buffer (current-buffer)))))
    (create-cmd save-buffers-kill-emacs ())
    (create-cmd kill-buffer (buffer :buffer "Kill buffer: " (buffer-name (current-buffer)) t))
    (create-cmd eval-expression (s :string "Eval: "))
    (create-cmd exchange-point-and-mark)
    (create-cmd set-mark-command)
    (create-cmd scroll-up)
    (create-cmd scroll-down)
    (create-cmd end-of-buffer)
    (create-cmd beginning-of-buffer)
    (create-cmd split-window-vertically)
    (create-cmd split-window-horizontally)
    (create-cmd other-window)
    (create-cmd switch-to-buffer-other-window (buffer :buffer "Switch to buffer in other window: " (buffer-name (other-buffer (current-buffer)))))
    (create-cmd delete-other-windows)
    (create-cmd keyboard-quit)
    (create-cmd kill-ring-save)
    (create-cmd kill-region (beg :region-beginning) (end :region-end))
    (create-cmd kill-line)
    (create-cmd yank)
    (create-cmd yank-pop)
    (create-cmd universal-argument)
    (create-cmd universal-argument-more (arg :raw-prefix))
    (create-cmd negative-argument (arg :raw-prefix))
    (create-cmd digit-argument (arg :raw-prefix))
    (create-cmd universal-argument-minus (arg :raw-prefix))
    (create-cmd universal-argument-other-key (arg :raw-prefix))))

#+(or cmu sbcl)
(defun rl ()
  (asdf:oos 'asdf:load-op :lice))

;;; The Deep Hack

;;(muerte.init::lice-genesis)

;; (defun foo ()
;;   (let (a)
;;     (labels ((do1 ()
;; 		  ;; do stuff and then
;; 		  (when <stuff>
;; 		    (setf a <a struct>)))
;; 	     (do2 ()
;; 		  ;; do stuff and then call..
;; 		  (do1)))
;;       (do2)
;;       a)))

(provide :lice-0.1/main)
