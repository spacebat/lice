;;; implentation of an interval tree

(in-package :lice)

(defvar *text-property-default-nonsticky* nil
  "Alist of properties vs the corresponding non-stickinesses.
Each element has the form (PROPERTY . NONSTICKINESS).

If a character in a buffer has PROPERTY, new text inserted adjacent to
the character doesn't inherit PROPERTY if NONSTICKINESS is non-nil,
inherits it if NONSTICKINESS is nil.  The front-sticky and
rear-nonsticky properties of the character overrides NONSTICKINESS.")

(defvar *char-property-alias-alist* nil
  "Alist of alternative properties for properties without a value.
Each element should look like (PROPERTY ALTERNATIVE1 ALTERNATIVE2...).
If a piece of text has no direct value for a particular property, then
this alist is consulted.  If that property appears in the alist, then
the first non-nil value from the associated alternative properties is
returned.")

(defvar *default-text-properties* nil
  "Property-list used as default values.
The value of a property in this list is seen as the value for every
character that does not have its own value for that property.")

(defmacro doplist ((sym val plist &optional ret) &body body)
  "Loop through each symbol value pair in PLIST executing BODY."
  (let ((csym (gensym)))
    `(do* ((,csym ,plist (cddr ,csym)))
	 ((endp ,csym) ,ret)
       (let ((,sym (car ,csym))
	     (,val (cadr ,csym)))
	 ,@body))))

;; interval node is a list: (key left right &rest plist)

(defun print-interval (i s d)
  (declare (ignore d))
  (format s "#S(interval ~s ~s ~s | ~s ~s)" 
	  (interval-pt i)
	  (interval-length i)
	  (interval-plist i)
	  (interval-left i)
	  (interval-right i)))

(defstruct (interval
	    (:print-function print-interval))
  (pt nil)
  (length nil)
  (left nil)
  (right nil)
  (parent nil :type (or null pstring buffer interval))
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

(defun interval-has-object (interval)
  (and (interval-parent interval)
       (not (typep (interval-parent interval) 'interval))))

(defun interval-has-parent (interval)
  (and (interval-parent interval)
       (typep (interval-parent interval) 'interval)))

(defun check-total-length (interval)
  (when (< (interval-length interval) 0)
    (error "Interval length < 0 ~a" interval)))

(defun create-root-interval (object)
  "Return a fresh interval for OBJECT."
  (let ((i (make-interval :pt 0 :length 0 :parent object
			  :left nil :right nil :plist nil)))
    (cond 
     ((typep object 'buffer)
       (setf (intervals object) i
	     ;; XXX: are buffer-max buffer-min the right functions to use?
	     (interval-length i) (- (buffer-max object) (buffer-min object))
	     (interval-pt i) (buffer-min object)))
      ((typep object 'pstring)
       (setf (intervals object) i
	     (interval-length i) (pstring-length object))))
    i))

(defun plists-equal (p1 p2)
  "Return 1 if the two properties lists are equal, 0 otherwise."
  (when (= (length p1)
	   (length p2))
    (doplist (sym val p1 t)
      (unless (eql val (getf p2 sym))
	(return-from plists-equal nil)))))

(defun intervals-equal (i1 i2)
  "Return T if the two intervals have the same properties, NIL otherwise."
  (plists-equal (interval-plist i1) (interval-plist i2)))

(defun lookup-char-property (plist prop textprop)
  (let* ((val (getf plist prop))
	 (cat (getf plist 'category)))
    (when val
      (return-from lookup-char-property val))
    ;; This is what GNU emacs does...
    (when (and (symbolp cat)
	       (get cat prop))
      (return-from lookup-char-property (get cat prop)))
    ;; Check for alternative properties
    (let ((tail (assoc prop *char-property-alias-alist*)))
      (when tail
	(or (find-if (lambda (p)
		       (getf plist p))
		     (cdr tail))
	    (and textprop
		 (consp *default-text-properties*)
		 (getf *default-text-properties* prop)))))))

(defun textget (plist sym)
  "Get the value of property PROP from PLIST,
which is the plist of an interval.
We check for direct properties, for categories with property PROP,
and for PROP appearing on the default-text-properties list."
  (lookup-char-property plist sym t))
	     
(defun split-interval-left (interval offset)
  (let* ((new-length offset)
	 (new (make-interval :pt (interval-pt interval)
			     :length offset
			     :parent interval)))
    (incf (interval-pt interval) offset)
    (if (interval-left interval)
	(progn
	  (setf (interval-left new) (interval-left interval)
		(interval-parent (interval-left new)) new
		(interval-left interval) new
		(interval-length new) (+ new-length (interval-length (interval-left new))))
	  (check-total-length new)
	  (balance-an-interval new))
      (progn
	(setf (interval-left interval) new
	      (interval-length new) new-length)
	(check-total-length new)))
    (balance-possible-root-interval interval)
    new))

(defun split-interval-right (interval offset)
  (let* ((position (interval-pt interval))
	 (new-length (- (interval-text-length interval) offset))
	 (new (make-interval :pt (+ position offset)
			     :length 0
			     :parent interval)))
    (setf (interval-parent new) interval)
    (if (interval-right interval)
	(progn
	  (setf (interval-right new) (interval-right interval)
		(interval-parent (interval-right interval)) new
		(interval-right interval) new
		(interval-length new) (+ new-length (interval-length (interval-right new))))
	  (check-total-length new)
	  (balance-an-interval new))
      (progn
	(setf (interval-right interval) new
	      (interval-length new) new-length)
	(check-total-length new)))
    (balance-possible-root-interval interval)
    new))

(defun right-child-p (root)
  (eq root (interval-right (interval-parent root))))

(defun left-child-p (root)
  (eq root (interval-left (interval-parent root))))

(defun interval-past-top-p (interval)
  "Return t if INTERVAL is not an interval. Used to check when we've
climbed past the root interval."
  (not (typep (interval-parent interval) 'interval)))

(defun rotate-right (interval)
"Assuming that a left child exists, perform the following operation:

     A		  B
    / \		 / \
   B       =>       A
  / \		   / \
     c		  c
"
  (let ((old-total (interval-length interval))
	(b (interval-left interval))
	i)
    ;; Change interval's parent to point b.
    (unless (root-interval-p interval)
      (if (left-child-p interval)
	  (setf (interval-left (interval-parent interval)) b)
	(setf (interval-right (interval-parent interval)) b)))
    (setf (interval-parent b) (interval-parent interval))
    ;; Make b the parent of a
    (setf i (interval-right b)
	  (interval-right b) interval
	  (interval-parent interval) b)
    ;; make a point to c
    (setf (interval-left interval) i)
    (when i
      (setf (interval-parent i) interval))
    ;; A's total length is decreased by the length of B and its left child.
    (decf (interval-length interval) (- (interval-length b)
				    (left-total-length interval)))
    (check-total-length interval)
    ;; B must have the same total length of A.
    (setf (interval-length b) old-total)
    (check-total-length b)
    b))

(defun rotate-left (interval)
"Assuming that a right child exists, perform the following operation:

    A               B
   / \	           / \
      B	   =>     A
     / \         / \
    c               c
"
  (let ((old-total (interval-length interval))
	(b (interval-right interval))
	i)
    ;; Change interval's parent to point b.
    (unless (root-interval-p interval)
      (if (left-child-p interval)
	  (setf (interval-left (interval-parent interval)) b)
	(setf (interval-right (interval-parent interval)) b)))
    (setf (interval-parent b) (interval-parent interval))
    ;; Make b the parent of a
    (setf i (interval-left b)
	  (interval-left b) interval
	  (interval-parent interval) b)
    ;; make a point to c
    (setf (interval-right interval) i)
    (when i
      (setf (interval-parent i) interval))
    ;; A's total length is decreased by the length of B and its left child.
    (decf (interval-length interval) (- (interval-length b)
				    (right-total-length interval)))
    (check-total-length interval)
    ;; B must have the same total length of A.
    (setf (interval-length b) old-total)
    (check-total-length b)
    b))
	  
(defun total-length (root)
  "TOTAL_LENGTH"
  (if root
      (interval-length root)
    0))

(defun left-total-length (root)
  (if (interval-left root)
      (interval-length (interval-left root))
    0))

(defun right-total-length (root)
  (if (interval-right root)
      (interval-length (interval-right root))
    0))

(defun interval-text-length (root)
  "The size of text represented by this interval alone. LENGTH."
  (if root
      (- (total-length root)
	 (total-length (interval-right root))
	 (total-length (interval-left root)))
    0))

(defun balance-an-interval (i)						 
  (let (old-diff
	new-diff)
    (loop
     (setf old-diff (- (left-total-length i) (right-total-length i)))
     (cond ((> old-diff 0)
	    ;; Since the left child is longer, there must be one.
	    (setf new-diff (+ (- (interval-length i) 
				 (interval-length (interval-left i)))
			      (- (right-total-length (interval-left i))
				 (left-total-length (interval-left i)))))
	    (when (>= (abs new-diff) old-diff)
	      (return-from balance-an-interval i))
	    (setf i (rotate-right i))
	    (balance-an-interval (interval-right i)))
	   ((< old-diff 0)
	    (setf new-diff (+ (- (interval-length i) 
				 (interval-length (interval-right i)))
			      (- (left-total-length (interval-right i))
				 (right-total-length (interval-right i)))))
	    (when (>= (abs new-diff) (- old-diff))
	      (return-from balance-an-interval i))
	    (setf i (rotate-left i))
	    (balance-an-interval (interval-left i)))
	   (t (return-from balance-an-interval i))))))

(defun balance-possible-root-interval (interval)
  (let ((has-parent nil)
	parent)
  (when (null (interval-parent interval))
    (return-from balance-possible-root-interval interval))
  (when (interval-has-object interval)
    (setf parent (interval-parent interval)
	  has-parent t))
  (setf interval (balance-intervals interval))
  (when has-parent
    (setf (intervals parent) interval))
  interval))

(defun balance-intervals (tree)
  "Balance the interval tree TREE. Balancing is by weight: the amount
of text."
  (labels ((balance (tree)
             (when (interval-left tree)
	       (balance (interval-left tree)))
	     (when (interval-right tree)
	       (balance (interval-right tree)))
	     (balance-an-interval tree)))
    (when tree
      (balance tree))))

(defun find-interval (tree position)
  (let ((relative-position position))
    (when (null tree)
      (return-from find-interval nil))
    (assert (<= relative-position (total-length tree)))
    (balance-possible-root-interval tree)
    (loop
     (cond ((< relative-position (left-total-length tree))
	    (setf tree (interval-left tree)))
	   ((and (interval-right tree)
		 (>= relative-position (- (total-length tree)
					  (right-total-length tree))))
	    (decf relative-position (- (total-length tree)
				       (right-total-length tree)))
	    (setf tree (interval-right tree)))
	   (t
	    (setf (interval-pt tree) (+ (- position relative-position)
					(left-total-length tree)))
	    (return-from find-interval tree))))))

(defun next-interval (interval)
  (unless (null interval)
    (let ((i interval)
	  (next-position (+ (interval-pt interval)
			    (interval-text-length interval))))
      (when (interval-right interval)
	(setf i (interval-right i))
	(loop while (interval-left i)
	      do (setf i (interval-left i)))
	(setf (interval-pt i) next-position)
	(return-from next-interval i))
      (loop until (interval-past-top-p i)
	    if (left-child-p i)
	    do (progn 
		 (setf i (interval-parent i)
		       (interval-pt i) next-position)
		 (return-from next-interval i))
	    do (setf i (interval-parent i))))))

(defun previous-interval (interval)
  (unless (null interval)
    (let ((i interval))
      (when (interval-left interval)
	(setf i (interval-left i))
	(loop while (interval-right i)
	      do (setf i (interval-right i)))
	(setf (interval-pt i) (- (interval-pt interval)
				 (interval-text-length i)))
	(return-from previous-interval i))
      (loop until (interval-past-top-p i)
	    if (right-child-p i)
	    do (progn 
		 (setf i (interval-parent i)
		       (interval-pt i) (- (interval-pt interval)
					  (interval-text-length i)))
		 (return-from previous-interval i))
	    do (setf i (interval-parent i))))))

(defun merge-interval-right (i)
  (let ((absorb (interval-text-length i))
	successor)
    (decf (interval-length i) absorb)
    (check-total-length i)
    (when (interval-right i)
      (setf successor (interval-right i))
      (loop while (interval-left successor)
	    do (progn
		 (incf (interval-length successor) absorb)
		 (check-total-length successor)
		 (setf successor (interval-left successor))))
      (incf (interval-length successor) absorb)
      (check-total-length successor)
      (delete-interval i)
      (return-from merge-interval-right successor))
    (setf successor i)
    (loop while (interval-parent successor)
	  do (if (left-child-p successor)
                 (progn
                   (setf successor (interval-parent successor))
                   (delete-interval i)
                   (return-from merge-interval-right successor))
               (progn
                 (setf successor (interval-parent successor))
                 (decf (interval-length successor) absorb)
                 (check-total-length successor))))
    (error "merge-interval-right: gak")))

(defun merge-interval-left (i)
  (let ((absorb (interval-text-length i))
	predecessor)
    (decf (interval-length i) absorb)
    (check-total-length i)
    (when (interval-left i)
      (setf predecessor (interval-left i))
      (loop while (interval-right predecessor)
	    do (incf (interval-length predecessor) absorb)
	    (check-total-length predecessor)
	    do (setf predecessor (interval-right predecessor)))
      (incf (interval-length predecessor) absorb)
      (check-total-length predecessor)
      (delete-interval i)
      (return-from merge-interval-left predecessor))
    (setf predecessor i)
    (loop while (interval-parent predecessor)
	  do (when (interval-right predecessor)
	       (setf predecessor (interval-parent predecessor))
	       (delete-interval i)
	       (return-from merge-interval-left predecessor))
	  do (setf predecessor (interval-parent predecessor))
	  do (decf (interval-length predecessor) absorb)
	  (check-total-length predecessor)
	  )
    (error "merge-interval-left: gak")))


;; adjust_intervals_for_insertion (tree, position, length)

(defun adjust-intervals-for-insertion (tree position length)
  "Effect an adjustment corresponding to the addition of LENGTH characters
of text.  Do this by finding the interval containing POSITION in the
interval tree TREE, and then adjusting all of its ancestors by adding
LENGTH to them.

If POSITION is the first character of an interval, meaning that point
is actually between the two intervals, make the new text belong to
the interval which is \"sticky\".

If both intervals are \"sticky\", then make them belong to the left-most
interval.  Another possibility would be to create a new interval for
this text, and make it have the merged properties of both ends."
  (let* ((parent (interval-parent tree))
	 (offset (if (typep parent 'buffer)
		     (buffer-min parent)
		   0))
	 i temp eobp)
    ;; If inserting at point-max of a buffer, that position will be out
    ;; of range.  Remember that buffer positions are 1-based.
    (when (>= position (+ (total-length tree) offset))
      (setf position (+ (total-length tree) offset)
	    eobp t))
    (setf i (find-interval tree position))
    ;; If in middle of an interval which is not sticky either way,
    ;; we must not just give its properties to the insertion.
    ;; So split this interval at the insertion point.

    ;; Originally, the if condition here was this:
    ;;	(! (position == i->position || eobp)
    ;;   && END_NONSTICKY_P (i)
    ;;   && FRONT_NONSTICKY_P (i))
    ;; But, these macros are now unreliable because of introduction of
    ;; Vtext_property_default_nonsticky.  So, we always check properties
    ;; one by one if POSITION is in middle of an interval.
    (unless (or (= position (interval-pt i))
		eobp)
      (let* ((rear (getf (interval-plist i) 'rear-nonsticky))
	     (front (getf (interval-plist i) 'front-sticky))
	     (problem t))
	(when (or (and (not (consp rear)) rear)
		  (and (not (consp front)) front))
	  ;; All properties are nonsticky.  We split the interval.
	  (setf problem nil))

	;; Does any actual property pose an actual problem?  We break
	;; the loop if we find a nonsticky property.
	(when problem
	  (setf problem (do* ((tail (interval-plist i) (cddr tail))
			      (prop (cdr tail) (cdr tail)))
			    ((or (endp tail)
				 (and (not (and (consp front)
						(not (find prop front))))
				      (or (and (consp rear)
					       (not (find prop rear)))
					  (let ((tmp (assoc prop *text-property-default-nonsticky*)))
					    (and (consp tmp) tmp)))))
			     tail))))
	;; If any property is a real problem, split the interval.
	(when problem
	  (setf temp (split-interval-right i (- position (interval-pt i))))
	  (copy-properties i temp)
	  (setf i temp))))
    ;; If we are positioned between intervals, check the stickiness of
    ;; both of them.  We have to do this too, if we are at BEG or Z.
    (if (or (= position (interval-pt i))
	      eobp)
	(let ((prev (cond ((= position +beg+) nil)
			  (eobp i)
			  (t (previous-interval i)))))
	  (when eobp
	    (setf i nil))
	  ;; Even if we are positioned between intervals, we default
	  ;; to the left one if it exists.  We extend it now and split
	  ;; off a part later, if stickiness demands it.
	  (do ((tmp (or prev i) (when (interval-has-parent tmp)
				  (interval-parent tmp))))
	      ((null tmp))
	    (incf (interval-length tmp) length)
	    ;; 	  CHECK_TOTAL_LENGTH (temp);
	    (setf tmp (balance-possible-root-interval tmp)))
	  ;; If at least one interval has sticky properties, we check
	  ;; the stickiness property by property.

	  ;; Originally, the if condition here was this:
	  ;;	(END_NONSTICKY_P (prev) || FRONT_STICKY_P (i))
	  ;; But, these macros are now unreliable because of introduction
	  ;; of Vtext_property_default_nonsticky.  So, we always have to
	  ;; check stickiness of properties one by one.  If cache of
	  ;; stickiness is implemented in the future, we may be able to
	  ;; use those macros again.
	  (let* ((pleft (when prev
			  (interval-plist prev)))
		 (pright (when i
			   (interval-plist i)))
		 (new-props (merge-properties-sticky pleft pright)))
	    (cond ((not prev)
		   ;; /* i.e. position == BEG */
		   (unless (plists-equal (interval-plist i) new-props)
		     (setf i (split-interval-left i length)
			   (interval-plist i) new-props)))
		  ((not (plists-equal (interval-plist prev) new-props))
		   (setf prev (split-interval-right prev (- position (interval-pt prev)))
			 (interval-plist prev) new-props)
		   (when (and i
			      (plists-equal (interval-plist prev) (interval-plist i)))
		     (merge-interval-right prev)))
		  ((and (not prev)
			(not (null (interval-plist i))))
		   ;; Just split off a new interval at the left.
		   ;; Since I wasn't front-sticky, the empty plist is ok.
		   (setf i (split-interval-left i length))))))
      ;; Otherwise just extend the interval.
      (do ((tmp i (when (interval-has-parent tmp)
		    (interval-parent tmp))))
	  ((null tmp))
	(incf (interval-length tmp) length)
	;; CHECK_TOTAL_LENGTH (temp);
	(setf tmp (balance-possible-root-interval tmp))))
      tree))

(defun interval-deletion-adjustment (tree from amount)
  "Find the interval in TREE corresponding to the relative position
FROM and delete as much as possible of AMOUNT from that interval.
Return the amount actually deleted, and if the interval was
zeroed-out, delete that interval node from the tree.

Note that FROM is actually origin zero, aka relative to the
leftmost edge of tree.  This is appropriate since we call ourselves
recursively on subtrees.

Do this by recursing down TREE to the interval in question, and
deleting the appropriate amount of text."
  (let ((relative-position from))
    (cond
     ((null tree)
      0)
     ;; Left branch
     ((< relative-position (left-total-length tree))
      (let ((subtract (interval-deletion-adjustment (interval-left tree)
						    relative-position
						    amount)))
	(decf (interval-length tree) subtract)
	;; CHECK_TOTAL_LENGTH
	subtract))
     ;; Right branch
     ((>= relative-position (- (total-length tree)
			       (right-total-length tree)))
      (decf relative-position (- (interval-length tree)
				 (right-total-length tree)))
      (let ((subtract (interval-deletion-adjustment (interval-right tree)
						    relative-position
						    amount)))
	(decf (interval-length tree) subtract)
	;; CHECK_TOTAL_LENGTH
	subtract))
     ;; This node
     (t
      ;; How much can we delete from this interval?
      (let ((my-amount (- (interval-length tree)
			  (right-total-length tree)
			  relative-position)))
	(when (> amount my-amount)
	  (setf amount my-amount))
	(decf (interval-length tree) amount)
	;; CHECK_TOTAL_LENGTH
	(when (zerop (total-length tree))
	  (delete-interval tree))
	amount)))))
    
(defun adjust-intervals-for-deletion (buffer start length)
  "Effect the adjustments necessary to the interval tree of BUFFER to
correspond to the deletion of LENGTH characters from that buffer
text.  The deletion is effected at position START (which is a
buffer position, i.e. origin 1)."
  (let ((left-to-delete length)
	(tree (intervals buffer))
	(offset (buffer-min buffer)))
    (cond
     ((null tree))
     ((or (> start (+ offset (total-length tree)))
	  (> (+ start length) (+ offset (total-length tree))))
      (error "gak ~a ~a ~a ~a" tree offset (total-length tree) length))
     ((= length (total-length tree))
      (setf (intervals buffer) nil))
     ((only-interval-p tree)
      (decf (interval-length tree) length)) ;; CHECK_TOTAL_LENGTH
     (t
      (when (> start (+ offset (total-length tree)))
	(setf start (+ offset (total-length tree))))
      (loop while (> left-to-delete 0) do 
	    (progn
	      (decf left-to-delete 
		    (interval-deletion-adjustment tree (- start offset) left-to-delete))
	      (setf tree (intervals buffer))
	      (when (= left-to-delete (interval-length tree))
		(setf (intervals buffer) nil)
		(return))))))))
	  
(defun interval-start-pos (source)
  (if (or (null source)
	  (not (typep (interval-parent source) 'buffer)))
      0
    (buffer-min (interval-parent source))))

(defun graft-intervals-into-buffer (source position length buffer inherit)
  "Insert the intervals of SOURCE into BUFFER at POSITION.
LENGTH is the length of the text in SOURCE.

The `position' field of the SOURCE intervals is assumed to be
consistent with its parent; therefore, SOURCE must be an
interval tree made with copy_interval or must be the whole
tree of a buffer or a string.

This is used in insdel.c when inserting Lisp_Strings into the
buffer.  The text corresponding to SOURCE is already in the buffer
when this is called.  The intervals of new tree are a copy of those
belonging to the string being inserted; intervals are never
shared.

If the inserted text had no intervals associated, and we don't
want to inherit the surrounding text's properties, this function
simply returns -- offset_intervals should handle placing the
text in the correct interval, depending on the sticky bits.

If the inserted text had properties (intervals), then there are two
cases -- either insertion happened in the middle of some interval,
or between two intervals.

If the text goes into the middle of an interval, then new
intervals are created in the middle with only the properties of
the new text, *unless* the macro MERGE_INSERTIONS is true, in
which case the new text has the union of its properties and those
of the text into which it was inserted.

If the text goes between two intervals, then if neither interval
had its appropriate sticky property set (front_sticky, rear_sticky),
the new text has only its properties.  If one of the sticky properties
is set, then the new text \"sticks\" to that region and its properties
depend on merging as above.  If both the preceding and succeeding
intervals to the new text are \"sticky\", then the new text retains
only its properties, as if neither sticky property were set.  Perhaps
we should consider merging all three sets of properties onto the new
text..."
  (let ((tree (intervals buffer))
	over-used
	under over this prev)
;;   /* If the new text has no properties, then with inheritance it
;;      becomes part of whatever interval it was inserted into.
;;      To prevent inheritance, we must clear out the properties
;;      of the newly inserted text.  */
    (when (null source)
      (when (and (not inherit)
		 tree
		 (> length 0))
;; 	  XSETBUFFER (buf, buffer);
	(set-text-properties-1 position (+ position length) nil buffer 0))
      (when (intervals buffer)
	(setf (intervals buffer) (balance-an-interval (intervals buffer))))
      (return-from graft-intervals-into-buffer))
    (cond ((null tree)
;;       /* The inserted text constitutes the whole buffer, so
;; 	 simply copy over the interval structure.  */
	   (when (= (- (buffer-size buffer) (buffer-min buffer))
		    (total-length source))
	     (setf (intervals buffer) (reproduce-tree source buffer)
		   (interval-pt (intervals buffer)) (buffer-min buffer))
	     (return-from graft-intervals-into-buffer))
;;       /* Create an interval tree in which to place a copy
;; 	 of the intervals of the inserted string.  */
	   (setf tree (create-root-interval buffer)))
	  ((= (total-length tree)
	      (total-length source))
;;     /* If the buffer contains only the new string, but
;;        there was already some interval tree there, then it may be
;;        some zero length intervals.  Eventually, do something clever
;;        about inserting properly.  For now, just waste the old intervals.  */
	   (setf (intervals buffer) (reproduce-tree source (interval-parent tree))
		 (interval-pt (intervals buffer)) (buffer-min buffer))
	   (return-from graft-intervals-into-buffer))
	  ((zerop (total-length tree))
	   (error "bork")))
    (setf under (find-interval tree position)
	  this under
	  over (find-interval source (interval-start-pos source)))
    (if (> position (interval-pt under))
	(let ((end-unchanged (split-interval-left this (- position (interval-pt under)))))
	  (copy-properties under end-unchanged)
	  (setf (interval-pt under) position))
      (setf prev (previous-interval under)))
    (setf over-used 0)
    (loop while over do 
	  (if (< (- (interval-text-length over) over-used)
		 (interval-text-length under))
	      (progn
		(setf this (split-interval-left under (- (interval-text-length over)
							 over-used)))
		(copy-properties under this))
	    (setf this under))
;;       /* THIS is now the interval to copy or merge into.
;; 	 OVER covers all of it.  */
	  (if inherit
	      (merge-properties over this)
	    (copy-properties over this))
;;       /* If THIS and OVER end at the same place,
;; 	 advance OVER to a new source interval.  */
	  (if (= (interval-text-length this) 
		(- (interval-text-length over) over-used))
	      (progn
		(setf over (next-interval over)
		      over-used 0))
;; 	/* Otherwise just record that more of OVER has been used.  */
	    (incf over-used (interval-text-length this)))
;;       /* Always advance to a new target interval.  */
	  (setf under (next-interval this)))
    (when (intervals buffer)
      (setf (intervals buffer) (balance-an-interval (intervals buffer))))))

(defun root-interval-p (i)
  "Return true if i is the root interval node."
  (or (null (interval-parent i))
      (not (typep (interval-parent i) 'interval))))

(defun root-interval (interval)
  "Return the root of interval."
  (do ((i interval (interval-parent i)))
      ((root-interval-p i) i)))

(defun leaf-interval-p (i)
  "Return T if this interval has no children."
  (and (null (interval-left i))
       (null (interval-right i))))

(defun only-interval-p (i)
  "Return T if this interval is the only interval in the interval tree."
  (and (root-interval-p i)
       (leaf-interval-p i)))
  

(defun delete-node (i)
  ;; Trivial cases
  (when (null (interval-left i))
    (return-from delete-node (interval-right i)))
  (when (null (interval-right i))
    (return-from delete-node (interval-left i)))
  ;; Meat
  (let ((migrate (interval-left i))
	(this (interval-right i))
	(migrate-amt (interval-length (interval-left i))))
    (loop while (interval-left this)
	  do (setf this (interval-left this))
	  do (incf (interval-length this) migrate-amt))
    (check-total-length this)
    (setf (interval-left this) migrate)
    (setf (interval-parent migrate) this)
    (interval-right i)))

(defun delete-interval (i)
  (let ((amt (interval-text-length i))
	parent)
    (and (> amt 0)
	 (error "only used on zero length intervals."))
    (when (root-interval-p i)
      (let ((owner (interval-parent i)))
	(setf parent (delete-node i))
	(when (interval-parent parent)
	  (setf (interval-parent parent) owner))
	(setf (intervals owner) parent)
	(return-from delete-interval)))
    (setf parent (interval-parent i))
    (if (left-child-p i)
	(progn
	  (setf (interval-left parent) (delete-node i))
	  (when (interval-left parent)
	    (setf (interval-parent (interval-left parent)) parent)))
      (progn
	(setf (interval-right parent) (delete-node i))
	(when (interval-right parent)
	  (setf (interval-parent (interval-right parent)) parent))))))

(defun default-interval-p (i)
  (or (null i)
      (null (interval-plist i))))

(defun reproduce-tree (source parent)
  (let ((tree (copy-interval source)))
    (setf (interval-plist tree) (copy-list (interval-plist source))
	  (interval-parent tree) parent)
    (when (interval-left source)
      (setf (interval-left tree) (reproduce-tree (interval-left source) tree)))
    (when (interval-right source)
      (setf (interval-right tree) (reproduce-tree (interval-right source) tree)))
    tree))

(defun merge-properties (source target)
  "/* Merge the properties of interval SOURCE into the properties of
interval TARGET.  That is to say, each property in SOURCE is added to
TARGET if TARGET has no such property as yet.  */"
  (unless (and (default-interval-p source)
	       (default-interval-p target))
    (doplist (sym val (interval-plist source))
      (let ((found (getf (interval-plist target) sym)))
	(unless found
	  (setf (getf (interval-plist target) sym) val))))))

(defun merge-properties-sticky (pleft pright)
  "Any property might be front-sticky on the left, rear-sticky on the left,
front-sticky on the right, or rear-sticky on the right; the 16 combinations
can be arranged in a matrix with rows denoting the left conditions and
columns denoting the right conditions:
      _  __  _
_     FR FR FR FR
FR__   0  1  2  3
 _FR   4  5  6  7
FR     8  9  A  B
  FR   C  D  E  F

left-props  = '(front-sticky (p8 p9 pa pb pc pd pe pf)
		   rear-nonsticky (p4 p5 p6 p7 p8 p9 pa pb)
		   p0 L p1 L p2 L p3 L p4 L p5 L p6 L p7 L
		   p8 L p9 L pa L pb L pc L pd L pe L pf L)
right-props = '(front-sticky (p2 p3 p6 p7 pa pb pe pf)
		   rear-nonsticky (p1 p2 p5 p6 p9 pa pd pe)
		   p0 R p1 R p2 R p3 R p4 R p5 R p6 R p7 R
                   p8 R p9 R pa R pb R pc R pd R pe R pf R)

We inherit from whoever has a sticky side facing us.  If both sides
do (cases 2, 3, E, and F), then we inherit from whichever side has a
non-nil value for the current property.  If both sides do, then we take
from the left.

When we inherit a property, we get its stickiness as well as its value.
So, when we merge the above two lists, we expect to get this:

result      = '(front-sticky (p6 p7 pa pb pc pd pe pf)
		   rear-nonsticky (p6 pa)
		   p0 L p1 L p2 L p3 L p6 R p7 R
		   pa R pb R pc L pd L pe L pf L)

The optimizable special cases are:
    left rear-nonsticky = nil, right front-sticky = nil (inherit left)
    left rear-nonsticky = t,   right front-sticky = t   (inherit right)
    left rear-nonsticky = t,   right front-sticky = nil (inherit none)"
  (labels ((tmem (sym set)
             ;; Test for membership, allowing for t (actually any
	     ;; non-cons) to mean the universal set."
	     (if (consp set) 
		 (find sym set)
	       set)))
    (let (props 
	  front
	  rear 
	  (lfront (getf pleft  'front-sticky))
	  (lrear  (getf pleft  'rear-nonsticky))
	  (rfront (getf pright 'front-sticky))
	  (rrear  (getf pright 'rear-nonsticky))
	  cat use-left use-right)
      (doplist (sym rval pright)
	       (unless (or (eq sym 'rear-nonsticky)
			   (eq sym 'front-sticky))
		 ;; Indicate whether the property is explicitly
		 ;; defined on the left.  (We know it is defined
		 ;; explicitly on the right because otherwise we don't
		 ;; get here.)
		 (let* ((lval (getf pleft sym))
			;; Even if lrear or rfront say nothing about the
			;; stickiness of SYM,
			;; Vtext_property_default_nonsticky may give
			;; default stickiness to SYM.
			(tmp (assoc sym *text-property-default-nonsticky*)))
		   (setf use-left (and lval
				       (not (or (tmem sym lrear)
						(and (consp tmp)
						     (cdr tmp)))))
			 use-right (or (tmem sym lrear)
				       (and (consp tmp)
					    (null (cdr tmp)))))
		   (when (and use-left
			      use-right)
		     (cond ((null lval)
			    (setf use-left nil))
			   ((null rval)
			    (setf use-right nil))))
		   (cond (use-left
			  ;; We build props as (value sym ...) rather than (sym value ...)
			  ;; because we plan to nreverse it when we're done.
			  (setf (getf props sym) lval)
			  (when (tmem sym lfront)
			    (push sym front))
			  (when (tmem sym lrear)
			    (push sym rear)))
			 (use-right
			  (setf (getf props sym) rval)
			  (when (tmem sym rfront)
			    (push sym front))
			  (when (tmem sym rrear)
			    (push sym rear)))))))
      ;; Now go through each element of PLEFT.
      (doplist (sym lval pleft)
	       (unless (or (eq sym 'rear-nonsticky)
			   (eq sym 'front-sticky))
		 ;; If sym is in PRIGHT, we've already considered it.
		 (let* ((present (getf pright sym))
			;; Even if lrear or rfront say nothing about the
			;; stickiness of SYM,
			;; Vtext_property_default_nonsticky may give
			;; default stickiness to SYM.
			(tmp (assoc sym *text-property-default-nonsticky*)))
		   ;; XXX: if sym is set in pright to nil, its the same
		   ;; as sym not being in the list.
		   (unless present
		     ;; Since rval is known to be nil in this loop, the test simplifies.
		     (cond ((not (or (tmem sym lrear)
				     (and (consp tmp)
					  (cdr tmp))))
			    (setf (getf props sym) lval)
			    (when (tmem sym lfront)
			      (push sym front)))
			   ((or (tmem sym rfront)
				(and (consp tmp)
				     (null (cdr tmp))))
			    ;; The value is nil, but we still inherit the stickiness
			    ;; from the right.
			    (setf (getf props sym) lval)
			    (when (tmem sym rrear)
			      (push sym rear))))))))
      (when rear
	(setf (getf props 'rear-nonsticky) (nreverse rear)))
      (setf cat (textget props 'category))
      ;; If we have inherited a front-stick category property that is t,
      ;; we don't need to set up a detailed one.
      (when (and front
		 (not (and cat
			   (symbolp cat)
			   (eq (get cat 'front-sticky) t))))
	(setf (getf props 'front-sticky) (nreverse front)))
      props)))

(defun offset-intervals (buffer start length)
  "Make the adjustments necessary to the interval tree of BUFFER to
represent an addition or deletion of LENGTH characters starting
at position START.  Addition or deletion is indicated by the sign
of LENGTH."
  (unless (or (null (intervals buffer))
	      (zerop length))
    (if (> length 0)
	(adjust-intervals-for-insertion (intervals buffer) start length)
      (adjust-intervals-for-deletion buffer (+ start length) (- length)))))

(provide :lice-0.1/intervals)
