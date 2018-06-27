(require 'verilog-mode)

(defun midas-init ()
  "Function called during Midas mode initialization."
  (auto-complete-mode))

(defun midas-demidisify ()
  "Transform midas directive into comments."
  (interactive)
  (save-excursion
    (goto-char 0)
    (while (search-forward-regexp "\n[ ]*%%" nil t)
      (replace-match "\n//%%"))))

(defun midas-remidisify ()
  "Reinsert midas directive back and align them."
  (save-excursion
    (goto-char 0)
    (while (search-forward-regexp "\n[ ]*//%%" nil t)
      (replace-match "\n%%"))))

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
