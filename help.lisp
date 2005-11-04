;;; Documentation and help related commands

(in-package :lice)

(defcommand describe-symbol ()
  "Display the full documentation of a symbol."
  (let* ((pkgs (mapcar (lambda (p)
                         (cons (package-name p) p))
                       (list-all-packages)))
         (pkg (cdr (find (completing-read "Package: " pkgs)  pkgs :key 'car :test 'string-equal)))
         (symbols (loop for s being each present-symbol of pkg
                        collect s))
         (symbol (find-symbol (string-upcase (completing-read "Symbol: " symbols)) pkg)))
    (with-current-buffer (get-buffer-create "*Help*")
      (erase-buffer)
      (insert (with-output-to-string (out)
		(describe symbol out)))
      (pop-to-buffer (current-buffer)))))


(provide :lice-0.1/help)
