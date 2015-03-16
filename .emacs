;; list of packages to install from the distro repo
;; sudo apt-get install emacs doxymacs anything-el ecb python-mode xkbset
;; sudo apt-get install gcc gdb g++ bison flex git valgrind
;; sudo apt-get install libcunit1 libcunit1-dev
;; sudo apt-get install gnome-tweak-tool

(column-number-mode)
(show-paren-mode 1)
(setq line-move-visual nil)

(global-set-key (kbd "C-x <up>") 'windmove-up)
(global-set-key (kbd "C-x <down>") 'windmove-down)
(global-set-key (kbd "C-x <right>") 'windmove-right)
(global-set-key (kbd "C-x <left>") 'windmove-left)

(global-set-key (kbd "<f2>") 'ecb-activate)
(global-set-key (kbd "<f3>") 'compilation-shell-minor-mode)
(global-set-key (kbd "<f4>") 'compile)
(global-set-key (kbd "<f5>") 'gud-gdb)
(global-set-key (kbd "<f6>") 'pdb)
(global-set-key (kbd "<f7>") 'create-tags)

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

(global-set-key [f8]  'graphnode_addr)
(global-set-key [f9]  'graphnode)

(setq tramp-default-method "scp")

;(add-to-list 'auto-mode-alist '("valsumm.out$" . compilation-minor-mode))
(add-to-list 'auto-mode-alist '("\\.cl$" . c-mode))
(add-to-list 'auto-mode-alist '("\\.l$" . flex-mode))
(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.pyw\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.yy$" . bison-mode))
(add-to-list 'auto-mode-alist '("NOTES.txt" . org-mode))
(delete-selection-mode)

(add-hook 'c-mode-common-hook
  (lambda ()
    (require 'doxymacs)
    (doxymacs-mode t)
    ;; (doxymacs-font-lock)
    ))

(setq doxymacs-doxygen-style "JavaDoc")

(defadvice pdb (before gud-query-cmdline activate)
  "Provide a better default command line when called interactively."
  (interactive
   (list (gud-query-cmdline pdb-path
                            (file-name-nondirectory buffer-file-name)))))


(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'load-path "~/.emacs.d/lisp/slime-master/")

;(set-default 'compile-command "make")
(set-default 'compile-command "make __TARGET__=t53.1 DEBUG=1 NOCOV=1")
;(set-default 'compile-command "make __TARGET__=m27.1 DEBUG=1 NOCOV=1 -B")

(setq-default indent-tabs-mode nil)

(setq c-default-style "K&R"
      c-basic-offset 4)


(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
         (column (c-langelem-2nd-pos c-syntactic-element))
         (offset (- (1+ column) anchor))
         (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(require 'cc-fonts)
;;(require 'ecb)
(require 'bison-mode)
(require 'flex-mode)
;;(require 'anything)
(require 'auto-complete-config)
;;(require 'python-pep8)
;;(require 'python-pylint)
;;(require 'python-mode)
(require 'ido)
(require 'slime-autoloads)
(type-break-mode)

(setq inferior-lisp-program "sbcl")
                                        ;(setq inferior-lisp-program "clisp")
(setq slime-contribs '(slime-fancy))
(ido-mode)

(add-to-list 'ac-dictionary-directories "/home/andrea/.emacs.d/lisp//ac-dict")
(ac-config-default)
(setq gud-gdb-command-name "gdb --annotate=3 python")
(global-ede-mode 1)                       ; Enable the Project management system
(semantic-mode)                           ; Enable prototype help and smart completion
                                        ;(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion
                                        ;(global-srecode-minor-mode 1)            ; Enable template insertion menu

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ecb-layout-window-sizes nil)
 '(ecb-options-version "2.40")
 '(ecb-source-path (quote ("" "/home/andrea/Workspace/")))
                                        ; '(initial-buffer-choice "~/Workspace/Rulex/")
 )
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
                                        ;(ecb-activate)

;; (autoload 'autopair-global-mode "autopair" nil t)
;; (autopair-global-mode)
;; (add-hook 'lisp-mode-hook
;;           #'(lambda () (setq autopair-dont-activate t)))

(add-hook 'python-mode-hook
          #'(lambda ()
              (push '(?' . ?')
                    (getf autopair-extra-pairs :code))
              (setq autopair-handle-action-fns
                    (list #'autopair-default-handle-action
                          #'autopair-python-triple-quote-action))))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(autoload 'pylookup-lookup "pylookup")
(autoload 'pylookup-update "pylookup")
(setq pylookup-program "~/.emacs.d/lisp/pylookup/pylookup.py")
(setq pylookup-db-file "~/.emacs.d/lisp/pylookup/pylookup.db")
(global-set-key "\C-ch" 'pylookup-lookup)

(defun compile-debug ()
  (interactive)
  (compile "make")
                                        ;  (gdb "gdb --annotate=3 python")
  )

(defun annotate-pdb ()
  (interactive)
  (highlight-lines-matching-regexp "import pdb")
  (highlight-lines-matching-regexp "pdb.set_trace()"))
(add-hook 'python-mode-hook 'annotate-pdb)

(defun python-add-breakpoint ()
  (interactive)
  (py-newline-and-indent)
  (insert "import ipdb; ipdb.set_trace()")
  (highlight-lines-matching-regexp "^[ ]*import ipdb; ipdb.set_trace()"))
                                        ;(define-key (kbd "C-c C-t") 'python-add-breakpoint)

(defun doxygen-comment()
  (interactive)
  (search-backward "/*")
  (forward-char 2)
  (insert "!")
  )

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

(setq py-shell-name "ipython")

(server-start)

(fset 'graphnode
      "call graph_node(dag,value_n,0)")
(fset 'graphnode_addr
      "call graph_node(dag,value_n,1)")

(defun forward-open-bracket ()
  "Move cursor to the next occurrence of left bracket or quotation mark."
  (interactive)
  (forward-char 1)
  (search-forward-regexp "(\\|{\\|\\[\\|<\\|〔\\|【\\|〖\\|〈\\|「\\|『\\|“\\|‘\\|‹\\|«")
  (backward-char 1)
  )

(defun backward-open-bracket ()
  "Move cursor to the previous occurrence of left bracket or quotation mark.."
  (interactive)
  (search-backward-regexp "(\\|{\\|\\[\\|<\\|〔\\|【\\|〖\\|〈\\|「\\|『\\|“\\|‘\\|‹\\|«")
  )

(defun forward-close-bracket ()
  "Move cursor to the next occurrence of right bracket or quotation mark."
  (interactive)
  (search-forward-regexp ")\\|\\]\\|}\\|>\\|〕\\|】\\|〗\\|〉\\|」\\|』\\|”\\|’\\|›\\|»")
  )

(defun backward-close-bracket ()
  "Move cursor to the next occurrence of right bracket or quotation mark."
  (interactive)
  (backward-char 1)
  (search-backward-regexp ")\\|\\]\\|}\\|>\\|〕\\|】\\|〗\\|〉\\|」\\|』\\|”\\|’\\|›\\|»")
  (forward-char 1)
  )

(global-set-key (kbd "<M-left>") 'backward-open-bracket) ; Alt+←
(global-set-key (kbd "<M-right>") 'forward-close-bracket) ; Alt+→

(defalias 'list-buffers 'ibuffer)

(require 'bash-completion)
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

(defun sbcl-debug-keys ()
  "Binds locally some keys to send clisp debugger commands to the inferior-lisp
<f5> step into
<f6> next
<f7> step over
<f8> continue
"
  (interactive)
  (macrolet ((cmd (string)
                  `(lambda ()
                     (interactive)
                     (comint-send-string (inferior-lisp-proc)
                                         ,(format "%s\n" string)))))
    (local-set-key (kbd "<f5>") (cmd "step"))
    (local-set-key (kbd "<f6>") (cmd "next"))
    (local-set-key (kbd "<f7>") (cmd "over"))
    (local-set-key (kbd "<f8>") (cmd "out"))))

(require 'whitespace)
(setq whitespace-style '(face lines-tail trailing))
(global-whitespace-mode t)
;;find . -type f -iname "*.[chS]" | xargs etags -a

(defun create-tags ()
  "Create tags file."
  (interactive)
  (shell-command "find . -type f -iname \"*.[chS\"def\"]\" | xargs etags -a")
  (visit-tags-table "./"))


(defcustom type-break-interval (* 30 30)
  "Number of seconds between scheduled typing breaks."
  :type 'integer
  :group 'type-break)


;; Hyperstone stuffs

;; open Hyperstone Makefile (*.mak) in makefile-mode
(setq auto-mode-alist (cons '("\\.mak" . makefile-mode) auto-mode-alist))

;; C indent

(c-add-style "HyStyle"
             '("linux"
               (c-basic-offset . 2); Guessed value
               (c-offsets-alist
                (arglist-cont . 0); Guessed value
                (arglist-intro . 0); Guessed value
                (block-close . 0); Guessed value
                (cpp-macro-cont . ++); Guessed value
                (defun-block-intro . +); Guessed value
                (defun-close . 0); Guessed value
                (defun-open . 0); Guessed value
                (else-clause . 0); Guessed value
                (func-decl-cont . *); Guessed value
                (statement . 0)    ; Guessed value
                (statement-block-intro . +) ; Guessed value
                (statement-cont . +)
                (substatement . +); Guessed value
                (substatement-open . 0); Guessed value
                (topmost-intro . 0); Guessed value
                (access-label . -)
                (annotation-top-cont . 0)
                (annotation-var-cont . +)
                (arglist-close . c-lineup-close-paren)
                (arglist-cont-nonempty . c-lineup-arglist)
                (block-open . 0)
                (brace-entry-open . 0)
                (brace-list-close . 0)
                (brace-list-entry . 0)
                (brace-list-intro . +)
                (brace-list-open . 0)
                (c . c-lineup-C-comments)
                (case-label . 2)
                (catch-clause . 0)
                (class-close . 0)
                (class-open . 0)
                (comment-intro . c-lineup-comment)
                (composition-close . 0)
                (composition-open . 0)
                (cpp-define-intro c-lineup-cpp-define +)
                (cpp-macro . -1000)
                (do-while-closure . 0)
                (extern-lang-close . 0)
                (extern-lang-open . 0)
                (friend . 0)
                (inclass . +)
                (incomposition . +)
                (inexpr-class . +)
                (inexpr-statement . +)
                (inextern-lang . +)
                (inher-cont . c-lineup-multi-inher)
                (inher-intro . +)
                (inlambda . c-lineup-inexpr-block)
                (inline-close . 0)
                (inline-open . +)
                (inmodule . +)
                (innamespace . +)
                (knr-argdecl . 0)
                (knr-argdecl-intro . 0)
                (label . 0)
                (lambda-intro-cont . +)
                (member-init-cont . c-lineup-multi-inher)
                (member-init-intro . +)
                (module-close . 0)
                (module-open . 0)
                (namespace-close . 0)
                (namespace-open . 0)
                (objc-method-args-cont . c-lineup-ObjC-method-args)
                (objc-method-call-cont c-lineup-ObjC-method-call-colons c-lineup-ObjC-method-call +)
                (objc-method-intro .
                                   [0])
                (statement-case-intro . +)
                (statement-case-open . 0)
                (stream-op . c-lineup-streamop)
                (string . -1000)
                (substatement-label . 0)
                (template-args-cont c-lineup-template-args +)
                (topmost-intro-cont . c-lineup-topmost-intro-cont))))



;; (setq c-default-style "HyStyle")
;; (setq c-mode-hook
;;     (function (lambda ()
;;                 (setq indent-tabs-mode nil)
;;                 (setq c-indent-level 2))))

(setq objc-mode-hook
      (function (lambda ()
                  (setq indent-tabs-mode nil)
                  (setq c-indent-level 2))))
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

(add-hook 'c-mode-hook
          (lambda ()
            (let ((filename (buffer-file-name)))
              ;; Enable kernel mode for the appropriate files
              (when (and filename
                         (string-match (expand-file-name "~/pmc-0.2/")
                                       filename))
                (setq indent-tabs-mode t)
                (c-set-style "linux-tabs-only")))))

(add-hook 'c-mode-hook
          (lambda ()
            (let ((filename (buffer-file-name)))
              ;; Enable kernel mode for the appropriate files
              (when (and filename
                         (string-match (expand-file-name "~/pbm/")
                                       filename))
                (setq indent-tabs-mode nil)
                (c-set-style "HyStyle")))))
