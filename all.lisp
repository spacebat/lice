(provide :lice-0.1/all :load-priority -1)

(defpackage :lice
  (:use :cl))

(require :lice-0.1/wrappers)
(require :lice-0.1/global)
(require :lice-0.1/major-mode)
(require :lice-0.1/buffer)

(require :lice-0.1/window)
(require :lice-0.1/frame)
(require :lice-0.1/movitz-render)

(require :lice-0.1/intervals)
(require :lice-0.1/textprop)
(require :lice-0.1/editfns)

(require :lice-0.1/input)
(require :lice-0.1/recursive-edit)

(require :lice-0.1/minibuffer)

(require :lice-0.1/subr)
(require :lice-0.1/simple)

;; ;; (require :lice-0.1/debug)
;; ;; (require :lice-0.1/files)
;; ;; (require :lice-0.1/help)

(require :lice-0.1/wm)
(require :lice-0.1/lisp-mode)

(require :lice-0.1/main)

;; (progn
;;   (movitz-compile-file #p"losp/lice-0.1/wrappers.lisp" :load-priority -1)
;;   (movitz-compile-file #p"losp/lice-0.1/global.lisp" :load-priority -1)
;;   (movitz-compile-file #p"losp/lice-0.1/major-mode.lisp" :load-priority -1)
;;   (movitz-compile-file #p"losp/lice-0.1/buffer.lisp" :load-priority -1)
;;   (movitz-compile-file #p"losp/lice-0.1/window.lisp" :load-priority -1)
;;   (movitz-compile-file #p"losp/lice-0.1/frame.lisp" :load-priority -1)
;;   (movitz-compile-file #p"losp/lice-0.1/movitz-render.lisp" :load-priority -1)
;;   (movitz-compile-file #p"losp/lice-0.1/intervals.lisp" :load-priority -1)
;;   (movitz-compile-file #p"losp/lice-0.1/textprop.lisp" :load-priority -1)
;;   (movitz-compile-file #p"losp/lice-0.1/editfns.lisp" :load-priority -1)
;;   (movitz-compile-file #p"losp/lice-0.1/input.lisp" :load-priority -1)
;;   (movitz-compile-file #p"losp/lice-0.1/recursive-edit.lisp" :load-priority -1)
;;   (movitz-compile-file #p"losp/lice-0.1/minibuffer.lisp" :load-priority -1)
;;   (movitz-compile-file #p"losp/lice-0.1/subr.lisp" :load-priority -1)
;;   (movitz-compile-file #p"losp/lice-0.1/simple.lisp" :load-priority -1)
;;   (movitz-compile-file #p"losp/lice-0.1/wm.lisp" :load-priority -1)
;;   (movitz-compile-file #p"losp/lice-0.1/lisp-mode.lisp" :load-priority -1)
;;   (movitz-compile-file #p"losp/lice-0.1/main.lisp" :load-priority -1))
