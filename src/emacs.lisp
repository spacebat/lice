(in-package "LICE")

;; FIXME: figure out the system type
(defvar system-type :undefined
  "Value is symbol indicating type of operating system you are using.
Special values:
  `:gnu/linux'   compiled for a GNU/Linux system.
  `:darwin'      compiled for Darwin (GNU-Darwin, Mac OS X, ...).
  `:macos'       compiled for Mac OS 9.
  `:ms-dos'      compiled as an MS-DOS application.
  `:windows-nt'  compiled as a native W32 application.
  `:cygwin'      compiled using the Cygwin library.
  `:vax-vms' or
  `:axp-vms'     compiled for a (Open)VMS system.
Anything else indicates some sort of Unix system.  */);
  Vsystem_type = intern (SYSTEM_TYPE")

(defvar kill-emacs-hook nil
  "Hook to be run when kill-emacs is called.
Since `kill-emacs' may be invoked when the terminal is disconnected (or
in other similar situations), functions placed on this hook should not
expect to be able to interact with the user.  To ask for confirmation,
see `kill-emacs-query-functions' instead.

The hook is not run in batch mode, i.e., if `noninteractive' is non-nil.")

(defun kill-emacs (&optional arg)
  "Exit the Emacs job and kill it.
If ARG is an integer, return ARG as the exit program code.
If ARG is a string, stuff it as keyboard input.

The value of `kill-emacs-hook', if not void,
is a list of functions (of no args),
all of which are called before Emacs is actually killed."
  (run-hooks 'kill-emacs-hook)
  (throw 'lice-quit arg))
