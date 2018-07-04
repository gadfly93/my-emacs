;;; -*- lexical-binding: t; -*-

;;; midas-mode.el --- major mode for editing midas source in Emacs

;; Copyright (C) 2018 Andrea Corallo.

;; Author: Andrea Corallo <andrea.corallo@arm.com>

;; This mode is based on verilog-mode. I usually use and raccomend the verilog mode form:
;; <http://www.veripool.com/verilog-mode>

;; INSTALLING THE MODE
;; ===================

;; The short list of installation instructions are:
;; Install verilog-mode from <http://www.veripool.com/verilog-mode>.
;; Put this file in your load path.
;; Add the following in code (please un comment it first!) in your
;; .emacs.

;; (require 'midas-mode)

;; USAGE
;; =====

;; Once in an midas file: M-x describe-mode

(require 'verilog-mode)

(defconst midas-mode-magic-str "$3^2&d#~"
  "Hope we don't find this string in your code...")

(defun midas-init ()
  "Function called during Midas mode initialization."
  (auto-complete-mode))

;; FIXME use font-lock instead
;; (defconst midas-comment-start-regexp "%!"
;;   "Dual comment value for `comment-start-regexp'.")

(defun midas-highlight-comments ()
  "Highlight correctly midas comments."
  (highlight-regexp "%!.*" font-lock-comment-face))

(defun midas-comment-line ()
  "Comment a single line."
  (interactive)
  (save-excursion
    (search-backward "\n" nil t)
    (replace-match "\n%!"))
  (midas-highlight-comments))

(defun midas-comment-region (start end)
  "Comment a region."
  (interactive "r")
  (save-excursion
    (save-restriction
      (goto-char start)
      (insert "%!")
      (narrow-to-region (point) (1+ end))
      (forward-char)
      (while (re-search-forward "^" nil t)
	(replace-match "%!"))))
    (midas-highlight-comments))

(defun midas-comment (start end)
  "Generic comment function.
This is called on C-c C-c"
  (interactive "r")
  (if (use-region-p)
      (midas-comment-region start end)
    (midas-comment-line)))

(defun midas-demidisify ()
  "Mask midas directive as comments."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward-regexp "^[ ]*%%" nil t)
      (replace-match (concat "//"
			     midas-mode-magic-str
			     "%%")))
    (goto-char (point-min))
    (while (search-forward-regexp "^[ ]*%!" nil t)
      (replace-match (concat "//"
			     midas-mode-magic-str
			     "%!")))
    (goto-char (point-min))
    (while (search-forward "{%" nil t)
      (replace-match (concat "/*"
			     midas-mode-magic-str
			     "{%")))
    (goto-char (point-min))
    (while (search-forward "%}" nil t)
      (replace-match (concat "%}"
			     midas-mode-magic-str
			     "*/")))))

(defun midas-remidisify ()
  "Reinsert midas directive back."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (search-forward (concat "//"
				   midas-mode-magic-str
				   "%%")
			   nil t)
      (replace-match "%%"))
    (goto-char (point-min))
    (while (search-forward (concat "//"
				   midas-mode-magic-str
				   "!%")
			   nil t)
      (replace-match "!%"))
    (goto-char (point-min))
    (while (search-forward (concat "/*"
				   midas-mode-magic-str
				   "{%")
			   nil t)
      (replace-match "{%"))
    (goto-char (point-min))
    (while (search-forward (concat "%}"
				   midas-mode-magic-str
				   "*/")
			   nil t)
      (replace-match "%}"))))

(defun midas-electric-indent ()
  "Function called when TAB is pressed in Midas mode."
  (interactive)
  (midas-demidisify)
  (electric-verilog-tab)
  (midas-remidisify))

(defun midas-indent-buffer ()
  "Function called when TAB is pressed in Midas mode."
  (interactive)
  (midas-demisify)
  (verilog-indent-buffer)
  (midas-remidisify))

(defvar midas-mode-map nil "Keymap for `midas-mode'")
;; make sure that the var name is your mode name followed by -map.
;; That way, define-derived-mode will automatically set it as local map
;; also, by comidasntion, variable names for keymap should end in -map

(progn
  (setq midas-mode-map (make-sparse-keymap))
  (define-key midas-mode-map (kbd "TAB") 'midas-electric-indent)
  (define-key midas-mode-map (kbd "C-c C-c") 'midas-comment)
  ;; by convention, major mode's keys should begin with the form C-c C-‹key›
  ;; by convention, keys of the form C-c ‹letter› are reserved for user. don't define such keys in your major mode
  )

(define-derived-mode
  midas-mode
  verilog-mode
  "MIDAS"
  "Major mode for working in MIDAS hardware description language."
  (midas-init))

(provide 'midas-mode)

(add-to-list 'auto-mode-alist '("\\.mds" . midas-mode))
