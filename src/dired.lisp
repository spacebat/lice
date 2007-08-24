;;; Lisp functions for making directory listings.

(in-package "LICE")

(defvar completion-ignored-extensions nil
  "Completion ignores file names ending in any string in this list.
It does not ignore them if all possible completions end in one of
these strings or when displaying a list of completions.
It ignores directory names if they match any string in this list which
ends in a slash.")

(defun directory-files ()
  (error "unimplemented directory-files"))

(defun directory-files-and-attributes ()
  (error "unimplemented directory-files-and-attributes"))

(defun file-name-completion ()
  (error "unimplemented file-name-completion"))

(defun file-name-all-completions ()
  (error "unimplemented file-name-all-completions"))

(defun file-attributes ()
  (error "unimplemented file-attributes"))

(defun file-attributes-lessp ()
  (error "unimplemented file-attributes-lessp"))
