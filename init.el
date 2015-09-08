;;;;
;; Good and userfriendly settings
;;;;

(require 'cl)
;; Get the current users name.
(defvar current-user
      (getenv
       (if (equal system-type 'windows-nt) "USERNAME" "USER")))

;; This is me
(setq user-full-name "Filip Lindvall")
(setq user-email-address "drpytho@gmail.com")

;; Send a friendly message letting the user
;; know work is being done.
(message "Emacs is powering up... Be patient, Master %s!" current-user)

;; Check for a sufficient version
(when (version< emacs-version "24.1")
  (error "Prelude requires at least GNU Emacs 24.1, but you're running %s" emacs-version))


;;;;
;; Packages
;;;;

;; Define package repositories
(require 'package)
(package-initialize)

;; Define package repos
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)


;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(defvar drPytho/packages
  '(magit                      ;Git for emacs
    yasnippet                  ;Snippets for langs
    rainbow-delimiters         ;Nice brackets
    ac-slime                   ;Source for AC vv
    auto-complete              ;Auto complete from dicktionary
    autopair                   ;()[]{}
    clojure-mode               ;
    clojure-test-mode          ;
    deft                       ;Folder searching
    feature-mode               ;Some weird testing... For future filip
    flycheck                   ;Syntax checking
    gist                       ;Handeling your gists on github
    go-mode                    ;For golang
    graphviz-dot-mode          ;Check on this in the future
    haml-mode                  ;HTML major mode
    htmlize                    ;Weird.. Exports X to html in a buffer
    markdown-mode              ;MD
    marmalade                  ;Extra gue for marmalade
    nodejs-repl                ;The node.js repl
    ;nrepl                      ;A repl
    ;o-blog                    ;For blogposts
    org                        ;Org-mode
    php-mode                   ;PHP goodness
    restclient                 ;Used to test rest clients
    smex                       ;Good...
    web-mode                   ;HTML, scripts and templates
    writegood-mode             ;Hepls to write academic texts
    exec-path-from-shell)      ;Load enviorment vars for Mac OSX fails. 
"Packages to install")

;; This is a function to check if all
;; packages are installed.
(defun drPytho/packages-installed-p ()
  "Check if all packages are installed."
  (loop for pkg in drPytho/packages
        when (not (package-installed-p pkg)) do (return nil)
        finally (return t)))

;; Unless all packages are installed.
;; Install them.
(unless (drPytho/packages-installed-p)
  (message "%s" "Refreshing package database...")
  (package-refresh-contents)
  (dolist (pkg drPytho/packages)
    (when (not (package-installed-p pkg))
      (package-install pkg))))



;;;;
;; Settings
;;;;

(setq snippets-folder (expand-file-name "snippets" user-emacs-directory))

;; Autosave files - I use git so this is good. Read a bit more about it though
(setq auto-save-default t)

;; Highlight current line
(global-hl-line-mode 1)

;; Set meta to be cmd instead of alt
(setq mac-option-modifier nil
      mac-command-modifier 'meta)

;; Get the latest binarys
(setq  load-prefer-newer t)

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; Skip the spash screen
;; No initial scratch message
;; Set us in org mode.
(setq inhibit-splash-screen t
      initial-scratch-message "Welcome to Emacs"
      initial-major-mode 'org-mode)


;; More sensable region options
(delete-selection-mode t)
(transient-mark-mode t)

;; Share clipboard with the rest of the system.
(setq x-select-enable-clipboard t)

;; Tab indentation
;; 4 spaces
;; No tabs
(setq tab-width 4
      indent-tabs-mode nil)

;; No backup files. Use git instead
(setq make-backup-files nil)

;; Add a vendor directory
(defvar drPytho/vendor-dir (expand-file-name "vendor" user-emacs-directory))
(add-to-list 'load-path drPytho/vendor-dir)

(dolist (project (directory-files drPytho/vendor-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))


;; NO TEMP FILES
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;;;;
;; Misc Cust Keymaps
;;;;


;; Interactive search key bindings. By default, C-s runs
;; isearch-forward, so this swaps the bindings.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "C-;") 'comment-or-uncomment-region)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C--") 'text-scale-decrease)
(global-set-key (kbd "C-c C-k") 'compile)
(global-set-key (kbd "C-x g") 'magit-status)

;;;;
;; ORG mode
;;;;

;; http://aaronbedra.com/emacs.d/#org-mode
;; ^^ for information about org settings

(setq org-log-done t
      org-todo-keywords '((sequence "TODO" "INPROGRESS" "DONE"))
      org-todo-keyword-faces '(("INPROGRESS" . (:foreground "blue" :weight bold))))
(add-hook 'org-mode-hook
          (lambda ()
            (flyspell-mode)))
(add-hook 'org-mode-hook
          (lambda ()
            (writegood-mode)))

(require 'org)


;;;;
;; Utils
;;;;

;; Start an emacs server to use for editing
(server-start)

;; Sets up exec-path-from shell
;; https://github.com/purcell/exec-path-from-shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH" "GOPATH")))


;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(require 'saveplace)
(setq-default save-place t)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (expand-file-name "places" user-emacs-directory))


;; Autopair brackets
(require 'autopair)

;; Power lisp
(setq lisp-modes '(lisp-mode
                   emacs-lisp-mode
                   common-lisp-mode
                   scheme-mode
                   clojure-mode))

(defvar lisp-power-map (make-keymap))
(define-minor-mode lisp-power-mode "Fix keybindings; add power."
  :lighter " (power)"
  :keymap lisp-power-map)
;(define-key lisp-power-map [delete] 'paredit-forward-delete)
;(define-key lisp-power-map [backspace] 'paredit-backward-delete)

(defun drPytho/engage-lisp-power ()
  (lisp-power-mode t))

(dolist (mode lisp-modes)
  (add-hook (intern (format "%s-hook" mode))
            #'drPytho/engage-lisp-power))

(setq inferior-lisp-program "clisp")
(setq scheme-program-name "racket")

;; Auto complete
(require 'auto-complete-config)
(ac-config-default)

;; Indentation & buffer cleanup
(defun untabify-buffer ()
  (interactive)
  (untabify (point-min) (point-max)))

(defun indent-buffer ()
  (interactive)
  (indent-region (point-min) (point-max)))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer."
  (interactive)
  (indent-buffer)
  (untabify-buffer)
  (delete-trailing-whitespace))

(defun cleanup-region (beg end)
  "Remove tmux artifacts from region."
  (interactive "r")
  (dolist (re '("\\\\│\·*\n" "\W*│\·*"))
    (replace-regexp re "" nil beg end)))

(global-set-key (kbd "C-x M-t") 'cleanup-region)
(global-set-key (kbd "C-c n") 'cleanup-buffer)

(setq-default show-trailing-whitespace t)

;; Flyspell
(setq flyspell-issue-welcome-flag nil)
(if (eq system-type 'darwin)
    (setq-default ispell-program-name "/usr/local/bin/aspell")
  (setq-default ispell-program-name "/usr/bin/aspell"))
(setq-default ispell-list-command "list")

;; Smex
(setq smex-save-file (expand-file-name ".smex-items" user-emacs-directory))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;; Ido
(ido-mode t)
(setq ido-enable-flex-matching t
      ido-use-virtual-buffers t)


;;;;
;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/custom")
(load "ui.el")
(load "editor.el")

(add-to-list 'load-path "~/.emacs.d/lang")
(load "org.el")
(load "golang.el")

;;;;
;; Theme
;;;;
;; Color Themes
;; Read http://batsov.com/articles/2012/02/19/color-theming-in-emacs-reloaded/
;; for a great explanation of emacs color themes.
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Custom-Themes.html
;; for a more technical explanation.
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(add-to-list 'load-path (expand-file-name "themes" user-emacs-directory))
(load-theme 'tomorrow-night t)


