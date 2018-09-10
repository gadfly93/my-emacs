;;; verilog-mode-expansions.el --- Expansions for verilog-mode

;; Copyright (C) 2018 Andrea Corallo

;; Author: Andrea Corallo <andrea_corallo@yahoo.it>
;; Keywords: marking region

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Feel free to contribute any other expansions for enh-ruby-mode at
;;
;;     https://github.com/magnars/expand-region.el

;;; Code:

(defun er/veri-balanced-beg-end ()
  (save-excursion
    (let ((res 0))
      (narrow-to-region (point) (mark))
      (goto-char (point-min))
      (while (re-search-forward "begin\\|end" nil t)
	(if (equal (match-string 0) "begin")
	    (incf res)
	  (decf res)))
      (widen)
      res)))

(defun er/mark-line ()
  (set-mark (line-beginning-position))
  (end-of-line)
  (exchange-point-and-mark))

(defun er/mark-begin-end-in ()
  (re-search-backward "begin[[:space:]\n]+")
  (right-word)
  (set-mark (point))
  (re-search-forward "end[[:space:]\n]+")
  (left-word)
  (exchange-point-and-mark))

(defun er/mark-begin-end ()
  (right-char)
  (re-search-backward "begin[[:space:]\n]+")
  (set-mark (point))
  (re-search-forward "\\(end\\)[[:space:]\n]+")
  (goto-char (match-end 1))
  (exchange-point-and-mark))

(defun er/mark-veri-code-blk ()
  (re-search-forward "end\\(\\w+\\)")
  (set-mark (point))
  (backward-word)
  (search-backward (match-string 1)))

(defun er/add-verilog-mode-expansions ()
  "Adds JS-specific expansions for buffers in verilog-mode"
  (set (make-local-variable 'er/try-expand-list) (append
                                                  er/try-expand-list
                                                  '(er/mark-line
						    er/mark-begin-end-in
						    er/mark-begin-end
						    er/mark-veri-code-blk
                                                    ))))

(er/enable-mode-expansions 'verilog-mode 'er/add-verilog-mode-expansions)

(provide 'verilog-mode-expansions)

;; verilog-mode-expansions.el ends here

;; should not be here
(eval-after-load 'verilog-mode '(require 'verilog-mode-expansions))
