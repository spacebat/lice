;;; Lisp functions for making directory listings.

(in-package "LICE")

(defvar completion-ignored-extensions nil
  "Completion ignores file names ending in any string in this list.
It does not ignore them if all possible completions end in one of
these strings or when displaying a list of completions.
It ignores directory names if they match any string in this list which
ends in a slash.")

(defun directory-files ()
  (error "unimplemented"))

(defun directory-files-and-attributes ()
  (error "unimplemented"))

(defun file-name-completion ()
  (error "unimplemented"))

(defun file-name-all-completions ()
  (error "unimplemented"))

(defun file-attributes ()
  (error "unimplemented"))

(defun file-attributes-lessp ()
  (error "unimplemented"))
