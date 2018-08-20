;; list of packages to install from the distro repo
;; sudo apt-get install emacs doxymacs ecb python-mode xkbset
;; sudo apt-get install gcc gdb g++ bison flex git valgrind
;; sudo apt-get install make automake texinfo git libgnutls-dev libncurses-dev
;; sudo apt-get install libcunit1 libcunit1-dev
;; sudo apt-get install global  (gtags)
;; sudo apt-get install offlineimap mu4e libwebkit-dev

(add-to-list 'load-path "~/.guix-profile/share/emacs/site-lisp/")

;; Define to t to enable mu4e
(setq mail-setup nil)

;; Define to t to enable exwm setup
(setq exwm-setup nil)

(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")
			 ("org" . "https://orgmode.org/elpa/")))

(setq package-list '(
		     ace-jump-mode
		     async
		     auto-complete
		     bash-completion
		     beacon
		     bison-mode
		     browse-kill-ring
		     color-theme-sanityinc-tomorrow
		     dash
		     elfeed
		     elisp-slime-nav
		     epresent
		     expand-region
		     f
		     ghub
		     git
		     git-blamed
		     git-commit
		     git-gutter
		     git-timemachine
		     google-translate
		     helm
		     helm-core
		     helm-git-grep
		     helm-gtags
		     htmlize
		     let-alist
		     magit
		     magit-popup
		     minimap
		     nov
		     org
		     org-plus-contrib
		     ox-reveal
		     popup
		     projectile
		     s
		     simple-call-tree
		     slime
		     undo-tree
		     vlf
		     which-key
		     with-editor
		     yaml-mode
		     ))

(when exwm-setup
  (add-to-list 'package-list 'exwm)
  (add-to-list 'package-list 'desktop-environment)
  (add-to-list 'package-list 'pulseaudio-control)
  (add-to-list 'package-list 'exwm-edit))

(when mail-setup
  (add-to-list 'package-list 'mu4e-alert))

(package-initialize)

(with-demoted-errors
    (when (or (file-exists-p package-user-dir) (package-refresh-contents))
      (dolist (package package-list)
	(unless (package-installed-p package)
	  (package-install package)))))

;; Increase garbage collection threshold
(setq gc-cons-threshold 20000000)

;; Disable startup message
(setq inhibit-splash-screen t)
(setq inhibit-startup-message t)
;; Disable menu bar
(menu-bar-mode -1)
;; Do not display GUI Toolbar
(tool-bar-mode -1)
;; Disable scroll bars
(scroll-bar-mode -1)

;; Display available keybindings in popup
(which-key-mode 1)

;; Display date + time into status bar
(setq display-time-day-and-date t)
(setq  display-time-24hr-format t)
(display-time)

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

;; Set ibuffer name column width
(require 'ibuffer)

(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
   ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
   (t (format "%8d" (buffer-size)))))

(setq ibuffer-formats
      '((mark modified read-only " "
              (name 35 35 :left :nil) " "
              (size-h 9 -1 :right) " "
              (mode 16 16 :left :elide) " "
              filename-and-process)))

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

;; Make windmove work in org-mode when possible.
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)

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
(global-set-key (kbd "<f12>") 'shell-clean-exec-last)

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x C-m") 'mu4e)
(global-set-key (kbd "C-c t") 'google-translate-at-point)
(global-set-key (kbd "C-c T") 'google-translate-query-translate)
(global-set-key (kbd "C-c f") 'find-name-dired)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-x G") 'helm-grep-do-git-grep)
(global-set-key (kbd "C-c SPC") 'ace-jump-mode)
(global-set-key (kbd "M-=") 'er/expand-region)

;; Shortcut for undo redo changes in the window configuration
(global-set-key (kbd "C-c C-<right>") 'winner-redo)
(global-set-key (kbd "C-c C-<left>") 'winner-undo)


;; Shortcut for changing font-size
(define-key global-map (kbd "C-1") 'text-scale-decrease)
(define-key global-map (kbd "C-2") 'text-scale-increase)

;; In magit-mode bind original vc keybindings to magit ones.
(require 'magit-mode)
(define-key magit-mode-map (kbd "C-x v l") 'magit-log-buffer-file)
(define-key magit-mode-map (kbd "C-x v L") 'magit-log-head)

;; (global-set-key (kbd "M-.") 'xref-find-definitions-other-frame)

(setq tramp-default-method "scp")

;(add-to-list 'auto-mode-alist '("valsumm.out$" . compilation-minor-mode))
(add-to-list 'auto-mode-alist '("\\.cl$" . c-mode))
(add-to-list 'auto-mode-alist '("\\.def$" . c-mode))
(add-to-list 'auto-mode-alist '("\\.l$" . flex-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.pyw\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.yy$" . bison-mode))
(add-to-list 'auto-mode-alist '("\\.vc" . verilog-mode))
(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
(add-to-list 'auto-mode-alist '("\\.app\\'" . verilog-mode))

;; When Delete Selection mode is enabled, typed text replaces the selection
;; if the selection is active.  Otherwise, typed text is just inserted at
;; point regardless of any selection.
(delete-selection-mode)

;; Whitespace diff insensitivity
(setq ediff-diff-options "-w")

(add-to-list 'load-path "~/.emacs.d/lisp/")

;; If present load nve veri-kompass and smime
(when (file-exists-p "~/nve-mode/")
    (add-to-list 'load-path "~/nve-mode/")
  (require 'nve-mode))
(when (file-exists-p "~/veri-kompass/")
    (add-to-list 'load-path "~/veri-kompass/")
  (require 'veri-kompass-mode))
(when (file-exists-p "~/smime/")
    (add-to-list 'load-path "~/smime/")
  (require 'smime))

;; helm-git-grep conf
(require 'helm-git-grep)
(global-set-key (kbd "C-c g") 'helm-git-grep)
;; Invoke `helm-git-grep' from isearch.
(define-key isearch-mode-map (kbd "C-c g") 'helm-git-grep-from-isearch)
;; Invoke `helm-git-grep' from other helm.
(eval-after-load 'helm
  '(define-key helm-map (kbd "C-c g") 'helm-git-grep-from-helm))

;; By default we run compilation on 4 cores
(set-default 'compile-command "make -j4")

;; Indentation can insert tabs
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
(require 'helm-config)
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
      (setq mu4e-mu-binary "~/.guix-profile/bin/mu")
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

;; Register bash completion for the shell buffer and shell command line.
(bash-completion-setup)

;; Toggle Ido mode on. Helm suggests not to do so. REVISIT
(ido-mode)

;; auto-complete default configuation
(ac-config-default)

;; Default gdb command I use
(setq gud-gdb-command-name "~/gdb-8.1/gdb/gdb -i=mi")

(setq ;; use gdb-many-windows by default
 gdb-many-windows t
 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t)


(global-ede-mode 1)                       ; Enable the Project management system
(semantic-mode)                           ; Enable prototype help and smart completion
(global-semantic-idle-summary-mode 1)

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
	       ("exwm" (mode . exwm-mode))
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
	       ("midas" (mode . smime))
	       ("yaml" (mode . yaml-mode))
	       ("verilog " (mode . verilog-mode))
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

;; Elfeed
(global-set-key (kbd "C-x w") 'elfeed)

;; List of RSS feeds its good to follow
(setq elfeed-feeds
      '("https://static.fsf.org/fsforg/rss/news.xml"
	"http://planet.gnu.org/rss20.xml"
	"http://nullprogram.com/feed/"
        "http://planet.emacsen.org/atom.xml"
	"http://feed.dilbert.com/dilbert/daily_strip?format=xml"
	"http://tromey.com/blog/?feed=rss2"
	"http://tromey.com/blog/?feed=comments-rss2"))

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

(add-hook 'verilog-mode-hook
	  (lambda ()
	    (setq indent-tabs-mode nil)))

(add-hook 'verilog-mode-hook
	  '(lambda ()
	     (add-hook 'before-save-hook
		       (lambda ()
			 (untabify (point-min) (point-max)))
		       nil t)))

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

; extend org-mode keywords
(setq org-todo-keywords
      '((sequence "TODO" "INVESTIGATING" "WAITING" "ONGOING" "|" "DONE"
		  "DELEGATED" "WONTDO")))

; eww search usign google
(setq eww-search-prefix "https://www.google.com/search?q=")
; eww better render for google results
(setq shr-color-visible-luminance-min 60)

;; EXWM setup
(if exwm-setup
    (progn
      ;; Also shrink fringes to 1 pixel.
      (fringe-mode 1)

      ;; Turn on `display-time-mode' if you don't use an external bar.
      (setq display-time-default-load-average nil)
      (display-time-mode t)

      ;; Load EXWM.
      (require 'exwm)

      ;; Fix problems with Ido (if you use it).
      (require 'exwm-config)
      (exwm-config-ido)

      ;; Set the initial number of workspaces (they can also be created later).
      (setq exwm-workspace-number 4)

      ;; All buffers created in EXWM mode are named "*EXWM*". You may want to
      ;; change it in `exwm-update-class-hook' and `exwm-update-title-hook', which
      ;; are run when a new X window class name or title is available.  Here's
      ;; some advice on this topic:
      ;; + Always use `exwm-workspace-rename-buffer` to avoid naming conflict.
      ;; + For applications with multiple windows (e.g. GIMP), the class names of
					;    all windows are probably the same.  Using window titles for them makes
      ;;   more sense.
      ;; In the following example, we use class names for all windows expect for
      ;; Java applications and GIMP.
      (add-hook 'exwm-update-class-hook
		(lambda ()
		  (unless (or (string-prefix-p "sun-awt-X11-" exwm-instance-name)
                              (string= "gimp" exwm-instance-name))
		    (exwm-workspace-rename-buffer exwm-class-name))))
      (add-hook 'exwm-update-title-hook
		(lambda ()
		  (when (or (not exwm-instance-name)
			    (string-prefix-p "sun-awt-X11-" exwm-instance-name)
			    (string= "gimp" exwm-instance-name))
		    (exwm-workspace-rename-buffer exwm-title))))

      ;; Global keybindings can be defined with `exwm-input-global-keys'.
      ;; Here are a few examples:
      (setq exwm-input-global-keys
            `(
              ;; Bind "s-r" to exit char-mode and fullscreen mode.
              ([?\s-r] . exwm-reset)
              ;; Bind "s-w" to switch workspace interactively.
              ([?\s-w] . exwm-workspace-switch)
              ;; Bind "s-0" to "s-9" to switch to a workspace by its index.
              ,@(mapcar (lambda (i)
                          `(,(kbd (format "s-%d" i)) .
                            (lambda ()
                              (interactive)
                              (exwm-workspace-switch-create ,i))))
                        (number-sequence 0 9))
              ;; Bind "s-&" to launch applications ('M-&' also works if the output
              ;; buffer does not bother you).
              ([?\s-&] . (lambda (command)
      			   (interactive (list (read-shell-command "$ ")))
      			   (start-process-shell-command command nil command)))
              ;; Bind "s-<f2>" to "slock", a simple X display locker.
              ([s-f2] . (lambda ()
      			  (interactive)
      			  (start-process "" nil "/usr/bin/slock")))))

      ;; To add a key binding only available in line-mode, simply define it in
      ;; `exwm-mode-map'.  The following example shortens 'C-c q' to 'C-q'.
      (define-key exwm-mode-map [?\C-q] #'exwm-input-send-next-key)

      ;; The following example demonstrates how to use simulation keys to mimic
      ;; the behavior of Emacs.  The value of `exwm-input-simulation-keys` is a
      ;; list of cons cells (SRC . DEST), where SRC is the key sequence you press
      ;; and DEST is what EXWM actually sends to application.  Note that both SRC
      ;; and DEST should be key sequences (vector or string).
      (setq exwm-input-simulation-keys
            '(
              ;; movement
              ([?\C-b] . [left])
              ([?\M-b] . [C-left])
              ([?\C-f] . [right])
              ([?\M-f] . [C-right])
              ([?\C-p] . [up])
              ([?\C-n] . [down])
              ([?\C-a] . [home])
              ([?\C-e] . [end])
              ([?\M-v] . [prior])
              ([?\C-v] . [next])
              ([?\C-d] . [delete])
              ([?\C-k] . [S-end delete])
              ;; cut/paste.
              ([?\C-w] . [?\C-x])
              ([?\M-w] . [?\C-c])
              ([?\C-y] . [?\C-v])
              ;; search
              ([?\C-s] . [?\C-f])))

      ;; Hide the minibuffer and echo area when they're not used?
      ;; (setq exwm-workspace-minibuffer-position 'bottom)

      ;; Do not forget to enable EXWM. It will start by itself when things are
      ;; ready.  You can put it _anywhere_ in your configuration.
      (exwm-enable)

      (require 'exwm-randr)

      (add-hook 'exwm-randr-screen-change-hook
		'exwm-auto-toggle-screen)

      (exwm-randr-enable)

      (defun exwm-enable-laptop-screen ()
	(interactive)
	(setq exwm-randr-workspace-output-plist nil)
	(start-process-shell-command
	 "xrandr" nil "xrandr --output eDP-1 --auto"))

      (defun exwm-auto-toggle-screen ()
	(interactive)
	(with-temp-buffer
	  (call-process "xrandr" nil t nil)
	  (goto-char (point-min))
	  (if (and (search-forward "DP-2-1 connected" nil 'noerror)
		   (search-forward "DP-2-2 connected" nil 'noerror))
	      (progn
		(start-process-shell-command
		 "xrandr" nil "xrandr --output eDP-1 --off")
		(start-process-shell-command
		 "xrandr" nil "xrandr --output DP-2-1 --auto")
		(start-process-shell-command
		 "xrandr" nil "xrandr --output DP-2-2 --primary --auto --right-of DP-2-1")
		(setq exwm-randr-workspace-output-plist '(0 "DP-2-1"
							    1 "DP-2-1"
							    2 "DP-2-2"
							    3 "DP-2-1"
							    4 "DP-2-2"
							    5 "DP-2-1")))
	    (if (progn
		  (goto-char (point-min))
		  (search-forward "HDMI-2 connected" nil 'noerror))
		(progn
		  (start-process-shell-command
		   "xrandr" nil "xrandr --output eDP-1 --off")
		  (start-process-shell-command
		   "xrandr" nil "xrandr --output HDMI-2 --auto")
		  (start-process-shell-command
		   "xrandr" nil "setxkbmap -layout us -option ctrl:nocaps"))
	      (setq exwm-randr-workspace-output-plist nil)
	      (start-process-shell-command
 	       "xrandr" nil "xrandr --output eDP-1 --auto")))
	  (start-process-shell-command
	   "setxkbmap" nil "setxkbmap -layout gb -option ctrl:nocaps")))

      ;; Avoid floating windows?
      (setq exwm-manage-force-tiling t)

      (require 'desktop-environment)
      (desktop-environment-mode)
      ;; Key binding.
      ;; (global-set-key (kbd "<XF86AudioMute>")
      ;; 		      'pulseaudio-control-toggle-current-sink-mute)
      ;; (global-set-key (kbd "<XF86AudioRaiseVolume>")
      ;; 		      'pulseaudio-control-increase-volume)
      ;; (global-set-key (kbd "<XF86AudioLowerVolume>")
      ;; 		      'pulseaudio-control-decrease-volume))

      (global-set-key (kbd "<XF86Display>")
		      'exwm-auto-toggle-screen)

      ;; Enable exwm-edit:
      ;; C-c '​ or C-c C-'​ - edit
      ;; C-c '​ or C-c C-c - finish editing
      ;; C-c C-k - cancel editing
      (require 'exwm-edit)

      ;; Open new url in new windows
      (setq browse-url-new-window-flag t)))

;; Lisp configuration stuffs

;; M-. runs the command elisp-slime-nav-find-elisp-thing-at-point
;; M-, to navigate back
(require 'elisp-slime-nav)
(dolist (hook '(emacs-lisp-mode-hook ielm-mode-hook))
  (add-hook hook 'turn-on-elisp-slime-nav-mode))

;; SLIME and sbcl
(let ((sbcl-path "/usr/local/bin/sbcl")
      (slime-helper-path "~/quicklisp/slime-helper.el"))
  (when (and (file-exists-p sbcl-path)
	     (file-exists-p slime-helper-path))
    (load (expand-file-name slime-helper-path))
    (setq inferior-lisp-program sbcl-path)
    (setq slime-contribs '(slime-fancy))))

;; move custom pkg dependency generated list out of here
(setq custom-file "~/.emacs.d/custom.el")
(write-region "" nil custom-file 'append)
(load custom-file)

;; Wdired (C-x C-q)
;; Make permissions bits of the files are editable.
(setq wdired-allow-to-change-permissions t)


;; ;; These regexp are used in compilation-mode and compilation-shell-minor-mode
;; ;; UVM
;; (add-to-list 'compilation-error-regexp-alist-alist
;; 	     '(uvm
;; 	       "^# \\(UVM_INFO\\|UVM_WARNING\\|UVM_ERROR\\|UVM_FATAL\\) \\(.+\\)(\\([0-9]+\\)).*$"
;; 	       2 3))
;; (push 'uvm compilation-error-regexp-alist)
;; ;; Mentor QuestaSim
;; (add-to-list 'compilation-error-regexp-alist-alist
;; 	     '(questa
;; 	       "^ | .* \\(/[a-z0-9/_.]+\\)(\\([0-9]+\\)).*$"
;; 	       1 2))
;; (push 'questa compilation-error-regexp-alist)
;; ;; Sva assertions

(add-to-list 'compilation-error-regexp-alist-alist
 	     '(core-tb1
 	       "^.*File: \\(.+\\) Line: \\([0-9]+\\).*$"
 	       1 2))
(push 'core-tb1 compilation-error-regexp-alist)

(setq compilation-directory-matcher
      '("\\(?:Entering\\|Leavin\\(g\\)\\) directory [`']\\(.+\\)univent_tarmac_build'$"
	(2 . 1)))

(add-to-list 'compilation-error-regexp-alist-alist
	     '(core-tb2
	       "^ | .* \\(.*\\)(\\([0-9]+\\)).*$"
	       1 2))
(push 'core-tb2 compilation-error-regexp-alist)

(defun shell-clean-exec-last ()
  (interactive)
  (delete-region (point-min) (point-max))
  (comint-previous-input 1)
  (comint-send-input)
  (fundamental-mode)
  (shell-mode)
  (compilation-shell-minor-mode))

; (standard-display-ascii ?\t "\t")
; pkill -SIGUSR2 emacs
