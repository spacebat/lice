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
