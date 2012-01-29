;; DESCRIPTION: dans settings

;; Manually set PATH for use by eshell, rspec-mode, etc.
(let ((path))
  (setq path (concat "/opt/ruby-enterprise/bin:"
                     "/opt/local/bin:"
                     "~/bin:"
                     "/usr/local/bin:"
                     "/usr/bin:"
                     "/bin:"
                     "/usr/local/git/bin"))
  (setenv "PATH" path))

;; Use Alt for the Meta key
(setq mac-option-modifier 'meta)

;; Fix the meta key.  It should be option
(setq mac-option-key-is-meta nil)
(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)
(setq mac-option-modifier nil)

;; fix path options in OSX
(when (equal system-type 'darwin)
  (setenv "PATH" (concat "/opt/local/bin:/usr/local/bin:" (getenv "PATH")))
  (push "/opt/local/bin" exec-path))

(add-to-list 'load-path (concat dotfiles-dir "/vendor"))

;; Save backups in one place
;; Put autosave files (ie #foo#) in one place, *not*
;; scattered all over the file system!
(defvar autosave-dir
  (concat "/tmp/emacs_autosaves/" (user-login-name) "/"))
(make-directory autosave-dir t)

(defun auto-save-file-name-p (filename)
  (string-match "^#.*#$" (file-name-nondirectory filename)))

(defun make-auto-save-file-name ()
  (concat autosave-dir
          (if buffer-file-name
              (concat "#" (file-name-nondirectory buffer-file-name) "#")
            (expand-file-name
             (concat "#%" (buffer-name) "#")))))

;; Put backup files (ie foo~) in one place too. (The backup-directory-alist
;; list contains regexp=>directory mappings; filenames matching a regexp are
;; backed up in the corresponding directory. Emacs will mkdir it if necessary.)
(defvar backup-dir (concat "/tmp/emacs_backups/" (user-login-name) "/"))
(setq backup-directory-alist (list (cons "." backup-dir)))

(add-hook 'before-save-hook 'delete-trailing-whitespace)

(setq default-tab-width 2)
(setq tab-width 2)

;; Clojure
;;(eval-after-load 'clojure-mode '(clojure-slime-config))

;; Plain Text
;;; Stefan Monnier <foo at acm.org>. It is the opposite of
;;; fill-paragraph. Takes a multi-line paragraph and makes
;;; it into a single line of text.
(defun unfill-paragraph ()
  (interactive)
  (let ((fill-column (point-max)))
    (fill-paragraph nil)))

(defun refresh-file ()
  (interactive)
  (revert-buffer t t t))
(global-set-key [f5] 'refresh-file)

;; Snippets
(add-to-list 'load-path (concat dotfiles-dir "/vendor/yasnippet"))
(require 'yasnippet)
(yas/initialize)
(yas/load-directory (concat dotfiles-dir "/vendor/yasnippet/snippets"))

;; Commands
(require 'unbound)

;; autotest
(add-to-list 'load-path (concat dotfiles-dir "/vendor/autotest"))
(require 'autotest)

;; Minor Modes
(add-to-list 'load-path (concat dotfiles-dir "/vendor/textmate.el"))
(require 'textmate)
(textmate-mode)
(require 'whitespace)

;; Major Modes

;; Javascript
;; TODO javascript-indent-level 2


;; Remove scrollbars and make hippie expand
;; work nicely with yasnippet
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(require 'hippie-exp)
(setq hippie-expand-try-functions-list
      '(yas/hippie-try-expand
        try-expand-dabbrev
        try-expand-dabbrev-visible
        try-expand-dabbrev-all-buffers
        ;; try-expand-dabbrev-from-kill
        ;; try-complete-file-name
        ;; try-complete-file-name-partially
        ;; try-complete-lisp-symbol
        ;; try-complete-lisp-symbol-partially
        ;; try-expand-line
        ;; try-expand-line-all-buffers
        ;; try-expand-list
        ;; try-expand-list-all-buffers
        ;; try-expand-whole-kill
        ))

(defun indent-or-complete ()
  (interactive)
  (if (and (looking-at "$") (not (looking-back "^\\s-*")))
      (hippie-expand nil)
    (indent-for-tab-command)))
(add-hook 'find-file-hooks (function (lambda ()
                                       (local-set-key (kbd "TAB") 'indent-or-complete))))

;; dabbrev-case-fold-search for case-sensitive search

;; Rinari
;;(add-to-list 'load-path (concat dotfiles-dir "/vendor/jump.el"))
;; (add-to-list 'load-path (concat dotfiles-dir "/vendor/rinari"))
(require 'rinari)
(define-key rinari-minor-mode-map [(control meta shift down)] 'rinari-find-rspec)
(define-key rinari-minor-mode-map [(control meta shift left)] 'rinari-find-controller)
(define-key rinari-minor-mode-map [(control meta shift up)] 'rinari-find-model)
(define-key rinari-minor-mode-map [(control meta shift right)] 'rinari-find-view)

(add-to-list 'load-path (concat dotfiles-dir "/vendor/rspec-mode"))
(require 'rspec-mode)


(autoload 'applescript-mode "applescript-mode" "major mode for editing AppleScript source." t)
(setq auto-mode-alist
      (cons '("\\.applescript$" . applescript-mode) auto-mode-alist))

;; org-mode
(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(global-set-key [(control meta return)] 'org-insert-heading)

(require 'textile-mode)
(add-to-list 'auto-mode-alist '("\\.textile\\'" . textile-mode))

(autoload 'markdown-mode "markdown-mode.el"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.mdown\\'" . markdown-mode))

(require 'haml-mode)
(add-to-list 'auto-mode-alist '("\\.haml$" . haml-mode))
(define-key haml-mode-map [(control meta down)] 'haml-forward-sexp)
(define-key haml-mode-map [(control meta up)] 'haml-backward-sexp)
(define-key haml-mode-map [(control meta left)] 'haml-up-list)
(define-key haml-mode-map [(control meta right)] 'haml-down-list)

(require 'sass-mode)
(add-to-list 'auto-mode-alist '("\\.sass$" . sass-mode))

(add-to-list 'auto-mode-alist '("\\.sake\\'" . ruby-mode))

;;ruby block mode
(require 'ruby-block)
(ruby-block-mode t)
(setq ruby-block-highlight-toggle 'overlay)

;; ruby electric
;(add-hook 'ruby-mode-hook 'ruby-electric-mode)

(require 'ruby-braces)

;; setup tags support.  Generate them each time a ruby file is opened.
(require 'gtags)
(defun dpj-ruby-gtags-create-or-update ()
  "create or update the gnu global tag file"
  (interactive)
  (if (not (= 0 (call-process "global" nil nil nil " -p"))) ; tagfile doesn't exist?
    (let ((olddir default-directory)
          (topdir (read-directory-name
                    "gtags: top of source tree:" default-directory)))
      (cd topdir)
      (shell-command "export GTAGSLABEL=rtags && gtags && echo 'created tagfile'")
      (cd olddir)) ; restore
    ;;  tagfile already exists; update it
    (shell-command "global -u && echo 'updated tagfile'")))

;; (add-hook 'gtags-mode-hook
;;   (lambda()
;;     (local-set-key (kbd "M-.") 'gtags-find-tag)   ; find a tag, also M-.
;;    (local-set-key (kbd "M-,") 'gtags-find-rtag))) ; reverse tag

(add-hook 'ruby-mode-hook
  (lambda ()
    (require 'gtags)
    (gtags-mode t)
    (setq gtags-symbol-regexp "[A-Za-z_:][A-Za-z0-9_#.:?]*")
    (dpj-ruby-gtags-create-or-update)))

(define-key gtags-mode-map "\M-," 'gtags-find-rtag)

;; setup ruby debugging
(add-to-list 'load-path (concat dotfiles-dir "/vendor/rdebug"))
(require 'rdebug)
(setq rdebug-short-key-mode t)

;; Setup anything
(add-to-list 'load-path (concat dotfiles-dir "/vendor/anything-config"))
(require 'anything-startup)

;; Setup RCodeTools
;(add-to-list 'load-path (concat dotfiles-dir "/vendor/rcodetools"))
;(require 'rcodetools)
;(require 'anything-rcodetools)
;(setq rct-get-all-methods-command "PAGER=cat fri -l")
;(define-key anything-map "\C-z" 'anything-execute-persistent-action)
;(setq rct-find-tag-if-available nil)

;; Run a command in inf-ruby
(defun dpj-ruby-console (&optional edit-cmd-args)
  "Run a ruby script in a compilation buffer, with command
history and links between errors and source code.  With optional
prefix argument allows editing of the console command arguments."
  (interactive)
  (run-ruby (dpj-find-console))
  (save-excursion
    (set-buffer "*ruby*")
    (set (make-local-variable 'inf-ruby-first-prompt-pattern) "^>> ")
    (set (make-local-variable 'inf-ruby-prompt-pattern) "^>> ")))

(defun dpj-find-console (&optional dir home)
  (or dir (setq dir default-directory))
  (if (file-exists-p (expand-file-name
		      "console" (expand-file-name "script" dir)))
      (expand-file-name "console" (file-name-as-directory
					       (expand-file-name "script" dir)))
    (let ((new-dir (expand-file-name (file-name-as-directory "..") dir)))
      ;; regexp to match windows roots, tramp roots, or regular posix roots
      (unless (string-match "\\(^[[:alpha:]]:/$\\|^/[^\/]+:\\|^/$\\)" dir)
        (dpj-find-console new-dir)))))

(define-key ruby-mode-map "\C-c s" 'dpj-ruby-console)


(require 'browse-kill-ring+)

;; XCODE
(require 'objc-c-mode)

;; (setq c-default-style "bsd"
;; c-basic-offset 2)

(require 'cc-menus)

(require 'xcode)
(define-key objc-mode-map [(meta r)] 'xcode-compile)
(define-key objc-mode-map [(meta K)] 'xcode-clean)
(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key [(meta O)] 'ff-find-other-file)))
(add-hook 'c-mode-common-hook
          (lambda()
            (local-set-key (kbd "C-c <right>") 'hs-show-block)
            (local-set-key (kbd "C-c <left>") 'hs-hide-block)
            (local-set-key (kbd "C-c <up>") 'hs-hide-all)
            (local-set-key (kbd "C-c <down>") 'hs-show-all)
            (hs-minor-mode t))) ; Hide and show blocks
(add-to-list 'auto-mode-alist '("\\.h\\'" . objc-mode))
(require 'objj-mode)

;; gist
(require 'gist)

;; Mercurial
;;(require 'mercurial)

;; Color Themes
(add-to-list 'load-path (concat dotfiles-dir "/vendor/color-theme"))
(require 'color-theme)
(color-theme-initialize)

;; Cedet
(load-file (concat dotfiles-dir "vendor/cedet/common/cedet.el"))

;; ECB
(add-to-list 'load-path (concat dotfiles-dir "/vendor/ecb-2.40"))
(require 'ecb)

;; Window Numbering
(require 'window-numbering)
(window-numbering-mode 1)

;; Tempo Snippets
(require 'tempo-snippets)

;; Tramp commands (added to fix error when entering ruby mode)
(require `tramp-cmds)

;;setup ri-emacs
(setq ri-ruby-script (concat dotfiles-dir "/vendor/ri-emacs/ri-emacs.rb"))
(autoload 'ri "ri-emacs/ri-ruby.el" nil t)

(add-hook 'ruby-mode-hook (lambda ()
                            (local-set-key [f1] 'ri)
                            (local-set-key "\M-\C-i" 'ri-ruby-complete-symbol)
                            (local-set-key [f4] 'ri-ruby-show-args)
                            ))

;; Test Case
(autoload 'test-case-mode "test-case-mode" nil t)
(autoload 'enable-test-case-mode-if-test "test-case-mode")
(autoload 'test-case-find-all-tests "test-case-mode" nil t)
(autoload 'test-case-compilation-finish-run-all "test-case-mode")
;;(add-hook 'find-file-hook 'enable-test-case-mode-if-test)

;; Functions

(require 'line-num)

;; Full screen toggle
(defun toggle-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen (if (frame-parameter nil 'fullscreen)
                                           nil
                                         'fullboth)))

;; Keyboard

;; (add-hook `ruby-mode-hook `(lambda () (inf-ruby-keys)))

;; Other

(prefer-coding-system 'utf-8)

(server-start)

;; Supress echo in inf-ruby
(defun echo-false-comint ()
(setq comint-process-echoes t))

(add-hook `inf-ruby-mode-hook `echo-false-comint)

;; Activate theme
;; (color-theme-emacs-21)
;;(color-theme-charcoal-black)
(color-theme-zenburn)
;;(color-theme-subtle-hacker)
;;(color-theme-arjen)
;;(color-theme-gray30)
;;(set-default-font "-unknown-Inconsolata-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1")
(set-default-font
     "-apple-consolas-medium-r-normal--12-0-72-72-m-0-iso10646-1")

;; Get stuff
(require 'registers)
(require 'bindings)

