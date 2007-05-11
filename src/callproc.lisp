;; Synchronous subprocess invocation for GNU Emacs.

(in-package "LICE")

;; FIXME: Fill these with real values

(defvar shell-file-name nil
"*File name to load inferior shells from.
Initialized from the SHELL environment variable, or to a system-dependent
default if SHELL is not set.")

(defvar exec-path nil
"*List of directories to search programs to run in subprocesses.
Each element is a string (directory name) or nil (try default directory).")

(defvar exec-suffixes nil
"*List of suffixes to try to find executable file names.
Each element is a string.")
  Vexec_suffixes = Qnil;

(defvar exec-directory nil
"Directory for executables for Emacs to invoke.
More generally, this includes any architecture-dependent files
that are built and installed from the Emacs distribution.")

(defvar data-directory nil
"Directory of machine-independent files that come with GNU Emacs.
These are files intended for Emacs to use while it runs.")

(defvar doc-directory nil
"Directory containing the DOC file that comes with GNU Emacs.
This is usually the same as `data-directory'.")

(defvar configure-info-directory nil
"For internal use by the build procedure only.
This is the name of the directory in which the build procedure installed
Emacs's info files; the default value for `Info-default-directory-list'
includes this.")

(defvar shared-game-score-directory nil
"Directory of score files for games which come with GNU Emacs.
If this variable is nil, then Emacs is unable to use a shared directory.")

(defvar temp-file-name-pattern nil
"Pattern for making names for temporary files.
This is used by `call-process-region'.")

(defvar process-environment nil
"List of environment variables for subprocesses to inherit.
Each element should be a string of the form ENVVARNAME=VALUE.
If multiple entries define the same variable, the first one always
takes precedence.
The environment which Emacs inherits is placed in this variable
when Emacs starts.
Non-ASCII characters are encoded according to the initial value of
`locale-coding-system', i.e. the elements must normally be decoded for use.
See `setenv' and `getenv'.")
