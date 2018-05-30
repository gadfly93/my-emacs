;; list of packages to install from the distro repo
;; sudo apt-get install emacs doxymacs ecb python-mode xkbset
;; sudo apt-get install gcc gdb g++ bison flex git valgrind
;; sudo apt-get install make automake texinfo git libgnutls-dev libncurses-dev
;; sudo apt-get install libcunit1 libcunit1-dev
;; sudo apt-get install global  (gtags)
;; sudo apt-get install offlineimap mu4e libwebkit-dev

;; Define to t to enable mu4e
(setq mail-setup t)

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))

(setq package-list '(bash-completion
                     helm-gtags
                     magit
                     undo-tree
                     async
                     auto-complete
                     bison-mode
                     dash
                     f
                     ghub
                     git
                     git-blamed
                     git-commit
                     git-gutter
                     helm
                     helm-core
                     let-alist
                     magit-popup
                     popup
                     s
                     vlf
                     with-editor
		     minimap
		     google-translate
		     browse-kill-ring
		     beacon
		     color-theme-sanityinc-tomorrow))

(if mail-setup
    (add-to-list 'package-list 'mu4e-alert))

(package-initialize)

(or (file-exists-p package-user-dir) (package-refresh-contents))

(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;; Disable startup message
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
;; Disable menu bar
(menu-bar-mode -1)
;; Do not display GUI Toolbar
(tool-bar-mode -1)
;; Disable scroll bars
(scroll-bar-mode -1)

;; Store backups and auto-saved files in TEMPORARY-FILE-DIRECTORY (which defaults
;; to /tmp on Unix), instead of in the same directory as the file.
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Confirm before closing Emacs
(setq confirm-kill-emacs 'y-or-n-p)

;; Human readable units in dired-mode
(setq-default dired-listing-switches "-alh")

;; automatically update dired buffers
(setq dired-auto-revert-buffer t)

;; Ask y/n instead of yes/no
(fset 'yes-or-no-p 'y-or-n-p)

;; Auto revert files on change
(global-auto-revert-mode t)

;; Enable Narrow To Region
;; Enable narrow-to-region (C-x n n / C-x n w). This is disabled by default to not
;; confuse beginners.
(put 'narrow-to-region 'disabled nil)

;; Windmove is built into Emacs. It lets you move point from window to window using
;; Shift and the arrow keys. This is easier to type than ‘C-x o’ when there are multiple
;; windows open.
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))

;; Allows to ‘undo’ (and ‘redo’) changes in the window configuration with the key
;; commands ‘C-c left’ and ‘C-c right’.
(when (fboundp 'winner-mode)
  (winner-mode 1))

;; When entering eww, use cursors to scroll without changing point.
;; (add-hook 'eww-mode-hook 'scroll-lock-mode)

;; Use undo tree mode everywhere by default.
(global-undo-tree-mode)

(column-number-mode)
(show-paren-mode 1)
(setq line-move-visual nil)

(global-set-key (kbd "<f2>") 'debug-rtx)
(global-set-key (kbd "<f3>") 'compilation-shell-minor-mode)
(global-set-key (kbd "<f4>") 'compile)
(global-set-key (kbd "<f5>") 'gdb)
(global-set-key (kbd "<f6>") 'gdb-restore-windows)
(global-set-key (kbd "<f7>") 'create-tags)
(global-set-key (kbd "<f8>") 'rename-buffer-shell)
(global-set-key (kbd "<f9>") 'mem-expl)

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x C-m") 'mu4e)
(global-set-key (kbd "C-c t") 'google-translate-at-point)
(global-set-key (kbd "C-c T") 'google-translate-query-translate)
(global-set-key (kbd "C-c l") 'org-link-generate)
(global-set-key (kbd "C-c f") ' find-name-dired)

;; Shortcut for undo redo changes in the window configuration
(global-set-key (kbd "C-c C-<right>") 'winner-redo)
(global-set-key (kbd "C-c C-<left>") 'winner-undo)


;; Shortcut for changing font-size
(define-key global-map (kbd "C-1") 'text-scale-decrease)
(define-key global-map (kbd "C-2") 'text-scale-increase)

;; (global-set-key (kbd "M-.") 'xref-find-definitions-other-frame)

(setq tramp-default-method "scp")

;(add-to-list 'auto-mode-alist '("valsumm.out$" . compilation-minor-mode))
(add-to-list 'auto-mode-alist '("\\.cl$" . c-mode))
(add-to-list 'auto-mode-alist '("\\.def$" . c-mode))
(add-to-list 'auto-mode-alist '("\\.l$" . flex-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.pyw\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.yy$" . bison-mode))
(add-to-list 'auto-mode-alist '("NOTES.txt" . org-mode))
(add-to-list 'auto-mode-alist '("\\.vc" . verilog-mode))

(delete-selection-mode)
(setq ediff-diff-options "-w")

(add-to-list 'load-path "~/.emacs.d/lisp/")

(add-to-list 'load-path "~/nve/")
(require 'nve-mode)

(set-default 'compile-command "make -j4")

(setq-default indent-tabs-mode t)

(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(require 'git)
(require 'cc-fonts)
(require 'bison-mode)
(require 'flex-mode)
(require 'auto-complete-config)
(require 'ido)
(require 'vlf)
(require 'bash-completion)
(require 'whitespace)
(require 'helm-gtags)
(require 'git-gutter)
(require 'google-translate)
(require 'google-translate-default-ui)
(require 'browse-kill-ring)

;; explore kill ring history
(setq browse-kill-ring-highlight-inserted-item t
      browse-kill-ring-highlight-current-entry nil
      browse-kill-ring-show-preview t)
(define-key browse-kill-ring-mode-map (kbd "<up>") 'browse-kill-ring-previous)
(define-key browse-kill-ring-mode-map (kbd "<down>") 'browse-kill-ring-forward)

(global-git-gutter-mode t) ;; Show uncommitted git diffs

;; Whenever the window scrolls a light will shine on top of your cursor.
(require 'beacon)
(beacon-mode 1)

;; mu4e setup

(if mail-setup
    (progn
      (require 'mu4e)
      (require 'mu4e-contrib)

      (add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e/")

      (setq
       mu4e-maildir       "~/.mail"           ;; top-level Maildir
       mu4e-sent-folder   "/Sent Items"       ;; folder for sent messages
       mu4e-drafts-folder "/Drafts"           ;; unfinished messages
       mu4e-trash-folder  "/Deleted Items"    ;; trashed messages
       mu4e-refile-folder "/Saved")           ;; saved messages

      ;; set offlineimap fro fetching mails
      (setq mu4e-get-mail-command "offlineimap"
	    mu4e-update-interval 300)             ;; update every 5 minutes

      ;; use mu4e for e-mail in emacs
      (setq mail-user-agent 'mu4e-user-agent)

      (require 'starttls)
      (setq starttls-use-gnutls t)

      (setq send-mail-function  'smtpmail-send-it
	    message-send-mail-function 'smtpmail-send-it
	    smtpmail-stream-type 'starttls
	    smtpmail-default-smtp-server "smtp.office365.com"
	    smtpmail-smtp-server "smtp.office365.com"
	    smtpmail-smtp-service 587
	    smtpmail-smtp-user "andrea.corallo@arm.com"
	    user-mail-address "andrea.corallo@arm.com"
	    user-full-name  "Andrea Corallo")

      ;; mu4e-maildir-shortcuts  '(("/iCloud/INBOX"    . ?i)
      ;;                             ("/Sent Items"   . ?s)
      ;;                             ("/Trash"        . ?t)
      ;;                             ("/All Mail"     . ?a))

      (setq mu4e-compose-signature "Sent with GNU Emacs\n")

      ;; use 'fancy' non-ascii characters in various places in mu4e
      (setq mu4e-use-fancy-chars t)

      ;; automatically update header-mail buffers
      (setq mu4e-headers-auto-update t)

      (setq mu4e-html2text-command 'mu4e-shr2text)
      (add-to-list 'mu4e-view-actions '("ViewInBrowser" . mu4e-action-view-in-browser) t)
      (mu4e-alert-set-default-style 'libnotify)
      (add-hook 'after-init-hook #'mu4e-alert-enable-notifications)
      (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)

      ;; attach files using dired

      (require 'gnus-dired)
      ;; make the `gnus-dired-mail-buffers' function also work on
      ;; message-mode derived modes, such as mu4e-compose-mode
      (defun gnus-dired-mail-buffers ()
	"Return a list of active message buffers."
	(let (buffers)
	  (save-current-buffer
	    (dolist (buffer (buffer-list t))
	      (set-buffer buffer)
	      (when (and (derived-mode-p 'message-mode)
			 (null message-sent-message-via))
		(push (buffer-name buffer) buffers))))
	  (nreverse buffers)))

      (setq gnus-dired-mail-mode 'mu4e-user-agent)
      (add-hook 'dired-mode-hook 'turn-on-gnus-dired-mode)))

(bash-completion-setup)

(ido-mode)

(add-to-list 'ac-dictionary-directories "/home/andrea/.emacs.d/lisp//ac-dict")
(ac-config-default)
(setq gud-gdb-command-name "~/gdb-8.1/gdb/gdb -i=mi")

(setq ;; use gdb-many-windows by default
 gdb-many-windows t
 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t)


(global-ede-mode 1)                       ; Enable the Project management system
(semantic-mode)                           ; Enable prototype help and smart completion
(global-semantic-idle-summary-mode 1)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (sanityinc-tomorrow-eighties)))
 '(custom-safe-themes
   (quote
    ("628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" default)))
 '(package-selected-packages
   (quote
    (beacon browse-kill-ring google-translate color-theme-sanityinc-tomorrow mu4e-alert minimap twittering-mode undo-tree epresent stickyfunc-enhance sr-speedbar sos realgud bash-completion gh-md markdown-mode flymd sos dictcc stickyfunc-enhance sr-speedbar realgud magit helm-gtags helm-git ggtags dismal csv-mode company)))
 '(verilog-align-ifelse t)
 '(verilog-auto-delete-trailing-whitespace t)
 '(verilog-auto-inst-param-value t)
 '(verilog-auto-inst-vector nil)
 '(verilog-auto-lineup (quote all))
 '(verilog-auto-newline nil)
 '(verilog-auto-save-policy nil)
 '(verilog-auto-template-warn-unused t)
 '(verilog-case-indent 2)
 '(verilog-cexp-indent 2)
 '(verilog-highlight-grouping-keywords t)
 '(verilog-highlight-modules t)
 '(verilog-indent-level 2)
 '(verilog-indent-level-behavioral 2)
 '(verilog-indent-level-declaration 2)
 '(verilog-indent-level-module 2)
 '(verilog-tab-to-comment nil))




;; (autoload 'autopair-global-mode "autopair" nil t)
;; (autopair-global-mode)
;; (add-hook 'lisp-mode-hook
;;           #'(lambda () (setq autopair-dont-activate t)))



(add-hook 'before-save-hook 'delete-trailing-whitespace)


(defun annotate-pdb ()
  (interactive)
  (highlight-lines-matching-regexp "import pdb")
  (highlight-lines-matching-regexp "pdb.set_trace()"))
(add-hook 'python-mode-hook 'annotate-pdb)

(defun python-add-breakpoint ()
  (interactive)
  (insert "import ipdb; ipdb.set_trace()")
  (highlight-lines-matching-regexp "^[ ]*import ipdb; ipdb.set_trace()"))
                                        ;(define-key (kbd "C-c C-t") 'python-add-breakpoint)

(defun rulex-to-c ()
  (interactive)
  (beginning-of-buffer)
  (replace-regexp "AND" "&&")
  (beginning-of-buffer)
  (replace-regexp "IF" "if")
  (beginning-of-buffer)
  (replace-regexp "THEN" "")
  (beginning-of-buffer)
  (replace-regexp "AND" "&&")
  (beginning-of-buffer)
  (replace-regexp "([0-9.,) ]+$" ";")
  (beginning-of-buffer)
                                        ;  (search-forward-regexp "[0-9.]+ [<=>]+ [a-zA-z0-9]+ [<=>]+ [0-9.]+")
                                        ;  (search-backward-regexp " [a-zA-Z0-9]+ ")
                                        ;  (mark-word)
                                        ;  (query-replace-regexp)
                                        ;  (kill-region)
                                        ;  (yank)
                                        ;  (insert "&&")
  (highlight-regexp "[0-9.]+ [<=>]+ [a-zA-z0-9]+ [<=>]+ [0-9.]+")
  )


(defun copy-android-scripts ()
  (interactive)
  (save-buffer)
  (shell-command (concat "adb push " buffer-file-name " /mnt/sdcard/sl4a/scripts/project/"))
  )


(server-start)

(fset 'mem-expl
      "x /10bfx ")
(fset 'debug-rtx
      "call debug_rtx(insn)")

;; Ibuffer conf
(defalias 'list-buffers 'ibuffer)
(add-hook 'ibuffer-mode-hook (lambda () (ibuffer-auto-mode 1)))

(setq ibuffer-saved-filter-groups
      (quote (("default"
	       ("lisp" (or
			(mode . lisp-mode)
			(mode . slime-repl-mode)
			(mode . slime-inspector-mode)
			(name . "^\\*slime-\\(description\\|compilation\\|xref\\)\\*$")
			(name . "^\\*sldb .*\\*$")
			(filename . "^/usr/local/doc/HyperSpec/")))
	       ("python" (or
			  (mode . python-mode)
			  (mode . inferior-python-mode)
			  (name . "^\\*Python \\(Check\\|Doc\\)\\*$")))
	       ("shell" (or
			 (mode . shell-mode)
			 (mode . term-mode)
			 (mode . sh-mode)
			 (mode . conf-unix-mode)
			 (mode . eshell-mode)
			 (name . "^\\*Shell Command Output\\*$")))
	       ("C" (or
		     (derived-mode . c-mode)
		     (mode . c++-mode)))
	       ("asm" (mode . asm-mode))
	       ("verilog" (mode . verilog-mode))
	       ("dired" (or
			 (mode . dired-mode)
			 (mode . wdired-mode)
			 (mode . archive-mode)
			 (mode . proced-mode)))
	       ("man" (or
		       (mode . Man-mode)
		       (mode . woman-mode)))
	       ("data" (or
			(filename . ".*\\.\\([ct]sv\\|dat\\)$")))
	       ("LaTeX" (or
			 (mode . latex-mode)
			 (mode . tex-shell)
			 (mode . TeX-output-mode)
			 (name . "^\\*\\(Latex Preview Pane \\(Welcome\\|Errors\\)\\|pdflatex-buffer\\)\\*$")))
	       ("text" (mode . text-mode))
	       ("pdf" (or
		       (mode . doc-view-mode)
		       (mode . pdf-view-mode)))
	       ("web" (or
		       (mode . w3m-mode)
		       (mode . eww-mode)))
	       ("org" (or (derived-mode . org-mode)
			  (mode . org-agenda-mode)))
	       ("planner" (or
			   (name . "^\\*Calendar\\*$")
			   (name . "^diary$")
			   (mode . muse-mode)))
	       ("org" (or (mode . org-mode)
			  (filename . "OrgMode")))
	       ("git" (or (derived-mode . magit-mode)
			  (filename . "\\.git\\(ignore\\|attributes\\)$")))
	       ("diff" (or
			(mode . diff-mode)
			(mode . ediff-mode)
			(name . "^\\*[Ee]?[Dd]iff.*\\*$")))
	       ("mail" (or
			(mode . message-mode)
			(mode . bbdb-mode)
			(mode . mail-mode)
			(mode . gnus-group-mode)
			(mode . gnus-summary-mode)
			(mode . gnus-article-mode)
			(mode . mu4e-compose-mode)
			(name . "*mu4e*")
			(name . "^\\.bbdb$")
			(name . "^\\.newsrc-dribble")))
	       ("emacs" (or
			 (mode . emacs-lisp-mode)
			 (mode . lisp-interaction-mode)
			 (mode . help-mode)
			 (mode . Info-mode)
			 (mode . package-menu-mode)
			 (mode . finder-mode)
			 (mode . Custom-mode)
			 (mode . apropos-mode)
			 (mode . ioccur-mode)
			 (mode . occur-mode)
			 (mode . reb-mode)
			 (mode . calc-mode)
			 (mode . calc-trail-mode)
			 (mode . messages-buffer-mode)))
	       ("misc" (name . "^\\*[0-9A-Za-z_]+\\*$"))))))

(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-switch-to-saved-filter-groups "default")))

(setq ibuffer-show-empty-filter-groups nil)
(setq ibuffer-jump-offer-only-visible-buffers t)

(bash-completion-setup)

(defun cleanup-document ()
  "Examines every character in the document, removing any 'special'
characters."
  (interactive)
  (goto-char (point-min))
  (while (not (eobp))
    ;; This regexp considers anything besides tab, newline, and characters
    ;; [space] (ASCII 32) thru tilde (ASCII 126) as 'special'
    (if (looking-at "[\t\n -~]")
        (forward-char 1)
      (delete-char 1 ()))))

(setq whitespace-style '(face lines-tail trailing))
(global-whitespace-mode t)
(global-semantic-stickyfunc-mode t)
;;find . -type f -iname "*.[chS]" | xargs etags -a

(defun create-tags ()
  "Create tags file."
  (interactive)
  (let ((default-directory (read-directory-name "create tags for dir: ")))
    ;; (delete-file "TAGS")
    (shell-command
     "~/ctags/ctags -e -R . \"*.cpp\" \"*.hpp\" \"*.c\" \"*.h\" \"*.s\"")
    (visit-tags-table "./")))

(setq c-backslash-max-column 79)

;; C indent
(setq c++-mode-hook
      (function (lambda ()
                  (setq indent-tabs-mode nil)
                  (setq c-indent-level 2))))

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Add kernel style
            (c-add-style
             "linux-tabs-only"
             '("linux" (c-offsets-alist
                        (arglist-cont-nonempty
                         c-lineup-gcc-asm-reg
                         c-lineup-arglist-tabs-only))))))

;; (add-hook 'c-mode-hook
;;           (lambda ()
;;             (let ((filename (buffer-file-name)))
;;               ;; Enable kernel mode for the appropriate files
;;               (when (and filename
;;                          (string-match (expand-file-name "pbm")
;;                                        filename))
;;                 (setq indent-tabs-mode nil)
;;                 (c-set-style "K&r")))))

(put 'upcase-region 'disabled nil)

(setq
 helm-gtags-ignore-case t
 helm-gtags-auto-update t
 helm-gtags-use-input-at-cursor t
 helm-gtags-pulse-at-cursor t
 helm-gtags-prefix-key "\C-cg"
 helm-gtags-suggested-key-mapping t)

;; Enable helm-gtags-mode
(add-hook 'dired-mode-hook 'helm-gtags-mode)
(add-hook 'eshell-mode-hook 'helm-gtags-mode)
(add-hook 'c-mode-hook 'helm-gtags-mode)
(add-hook 'c++-mode-hook 'helm-gtags-mode)
(add-hook 'asm-mode-hook 'helm-gtags-mode)

(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode 'asm-mode 'dired-mode)
              (helm-gtags-mode))))
(add-hook 'c-mode-common-hook
          (lambda ()
            (when (derived-mode-p 'c-mode 'c++-mode 'java-mode)
              (hs-minor-mode))))

(define-key helm-gtags-mode-map (kbd "C-c g a") 'helm-gtags-tags-in-this-function)
(define-key helm-gtags-mode-map (kbd "C-j") 'helm-gtags-select)
(define-key helm-gtags-mode-map (kbd "M-.") 'helm-gtags-dwim)
(define-key helm-gtags-mode-map (kbd "M-,") 'helm-gtags-pop-stack)
(define-key helm-gtags-mode-map (kbd "C-c <") 'helm-gtags-previous-history)
(define-key helm-gtags-mode-map (kbd "C-c >") 'helm-gtags-next-history)

(setq-local imenu-create-index-function #'ggtags-build-imenu-index)

(defun rename-buffer-shell ()
  "Renames current shell buffer with the directory in it."
  (interactive)
  (rename-buffer
   (concat "*"
           (car (last (butlast (split-string default-directory "/") 1)))
           "-shell*")))

(defun org-link-generate ()
  "Put into the kill ring a valid org link for the current cursor position."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new (concat "[[" filename "::" (int-to-string (line-number-at-pos))
			"]]"))
      (message "Org link for file name '%s' to the kill ring." filename))))

  (add-hook 'c-mode-hook
            (lambda ()
              (add-to-list 'ac-sources 'ac-source-c-headers)
              (add-to-list 'ac-sources 'ac-source-c-header-symbols t)))

; extend org-mode keywords
(setq org-todo-keywords
      '((sequence "TODO" "WAITING" "ONGOING" "|" "DONE" "DELEGATED" "WONTDO")))

; eww search usign google
(setq eww-search-prefix "https://www.google.com/search?q=")
; eww better render for google results
(setq shr-color-visible-luminance-min 60)

; (standard-display-ascii ?\t "\t")
; pkill -SIGUSR2 emacs

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
