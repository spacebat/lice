;;; text-mode.el --- text mode, and its idiosyncratic commands

;; Copyright (C) 1985, 1992, 1994, 2002, 2003, 2004,
;;   2005, 2006 Free Software Foundation, Inc.

;; Maintainer: FSF
;; Keywords: wp

;; This file is part of GNU Emacs.

;; GNU Emacs is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; This package provides the fundamental text mode documented in the
;; Emacs user's manual.

;;; Code:

(in-package "LICE")

(defcustom *text-mode-hook* nil
  "Normal hook run when entering Text mode and many related modes."
  :type 'hook
  :options '(turn-on-auto-fill turn-on-flyspell)
  :group 'data)

(define-buffer-local *text-mode-variant* nil
  "Non-nil if this buffer's major mode is a variant of Text mode.
Use (derived-mode-p 'text-mode) instead.")

(defvar *text-mode-syntax-table*
  (let ((st (make-syntax-table)))
    (modify-syntax-entry #\" :punctuation :table st)
    (modify-syntax-entry #\\ :punctuation :table st)
    ;; We add `p' so that M-c on 'hello' leads to 'Hello' rather than 'hello'.
    (modify-syntax-entry #\' :word-constituent :flags '(:prefix) :table st)
    st)
  "Syntax table used while in `text-mode'.")

(defvar *text-mode-map*
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "M-TAB") 'ispell-complete-word)
    (define-key map (kbd "M-s") 'center-line)
    (define-key map (kbd "M-S") 'center-paragraph)
    map)
  "Keymap for `text-mode'.
Many other modes, such as `mail-mode', `outline-mode' and `indented-text-mode',
inherit all the commands defined in this map.")


(defvar *text-mode*
  (make-instance 'major-mode
                 :name "Text"
                 :map *text-mode-map*
                 :syntax-table *text-mode-syntax-table*
                 :hook '*text-mode-hook*
                 :init 
                 (lambda ()
                   (setf (make-local-variable '*text-mode-variant*) t
                         (make-local-variable '*require-final-newline*)
                         *mode-require-final-newline*
                         (make-local-variable '*indent-line-function*)
                         'indent-relative)))  
  "Major mode for editing text written for humans to read.
In this mode, paragraphs are delimited only by blank or white lines.
You can thus get the full benefit of adaptive filling
 (see the variable `adaptive-fill-mode').
\\{text-mode-map}.")

(defcommand text-mode ()
  "See `*text-mode*'."
  (set-major-mode '*text-mode*))

(defvar *paragraph-indent-text-mode*
  (make-instance 'major-mode
                 :name "parindent"
                 :inherit-map '(*text-mode*)
                 :inherit-syntax '(*text-mode*)
                 :inherit-init '(*text-mode*)
                 :init (lambda ()
                         (paragraph-indent-minor-mode)))
  "Major mode for editing text, with leading spaces starting a paragraph.
In this mode, you do not need blank lines between paragraphs
when the first line of the following paragraph starts with whitespace.
`paragraph-indent-minor-mode' provides a similar facility as a minor mode.
Special commands:
\\{text-mode-map}
Turning on Paragraph-Indent Text mode runs the normal hooks
`text-mode-hook' and `paragraph-indent-text-mode-hook'.")

(defcommand paragraph-indent-text-mode ()
  "see `*paragraph-indent-text-mode*'."
  (set-major-mode '*paragraph-indent-text-mode*))

(defcommand paragraph-indent-minor-mode ()
  "Minor mode for editing text, with leading spaces starting a paragraph.
In this mode, you do not need blank lines between paragraphs when the
first line of the following paragraph starts with whitespace, as with
`paragraph-indent-text-mode'.
Turning on Paragraph-Indent minor mode runs the normal hook
`paragraph-indent-text-mode-hook'."
  (setf (make-local-variable 'paragraph-start)
        (concat "[ \t\n\f]\\|" paragraph-start))
  (set (make-local-variable 'indent-line-function) 'indent-to-left-margin)
  (run-hooks '*paragraph-indent-text-mode-hook*))

;;(defalias 'indented-text-mode 'text-mode)

;; This can be made a no-op once all modes that use text-mode-hook
;; are "derived" from text-mode.
(defun text-mode-hook-identify ()
  "Mark that this mode has run `text-mode-hook'.
This is how `toggle-text-mode-auto-fill' knows which buffers to operate on."
  (setf (make-local-variable *text-mode-variant*) t))

(add-hook '*text-mode-hook* 'text-mode-hook-identify)

(defcommand toggle-text-mode-auto-fill ()
  "Toggle whether to use Auto Fill in Text mode and related modes.
This command affects all buffers that use modes related to Text mode,
both existing buffers and buffers that you subsequently create."
  (let ((enable-mode (not (find 'turn-on-auto-fill *text-mode-hook*))))
    (if enable-mode
	(add-hook '*text-mode-hook* 'turn-on-auto-fill)
        (remove-hook '*text-mode-hook* 'turn-on-auto-fill))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
	(if (or (eq (buffer-major-mode (current-buffer)) *text-mode*)
                *text-mode-variant*)
	    (auto-fill-mode (if enable-mode 1 0)))))
    (message "Auto Fill %s in Text modes"
	     (if enable-mode "enabled" "disabled"))))

(defcommand center-paragraph ()
  "Center each nonblank line in the paragraph at or after point.
See `center-line' for more info."
  (save-excursion
    (forward-paragraph)
    (or (bolp) (newline 1))
    (let ((end (point)))
      (backward-paragraph)
      (center-region (point) end))))

(defcommand center-region ((from to)
                           :region-beginning :region-end)
  "Center each nonblank line starting in the region.
See `center-line' for more info."
  (if (> from to)
      (let ((tem to))
	(setq to from from tem)))
  (save-excursion
    (save-restriction
      (narrow-to-region from to)
      (goto-char from)
      (while (not (eobp))
	(or (save-excursion (skip-chars-forward " \t") (eolp))
	    (center-line))
	(forward-line 1)))))

(defcommand center-line ((&optional nlines)
                         :raw-prefix)
  "Center the line point is on, within the width specified by `fill-column'.
This means adjusting the indentation so that it equals
the distance between the end of the text and `fill-column'.
The argument NLINES says how many lines to center."
  (if nlines (setq nlines (prefix-numeric-value nlines)))
  (while (not (eq nlines 0))
    (save-excursion
      (let ((lm (current-left-margin))
	    line-length)
	(beginning-of-line)
	(delete-horizontal-space)
	(end-of-line)
	(delete-horizontal-space)
	(setq line-length (current-column))
	(if (> (- fill-column lm line-length) 0)
	    (indent-line-to
	     (+ lm (/ (- fill-column lm line-length) 2))))))
    (cond ((null nlines)
	   (setq nlines 0))
	  ((> nlines 0)
	   (setq nlines (1- nlines))
	   (forward-line 1))
	  ((< nlines 0)
	   (setq nlines (1+ nlines))
	   (forward-line -1)))))

;;; arch-tag: a07ccaad-da13-4d7b-9c61-cd04f5926aab
;;; text-mode.el ends here
