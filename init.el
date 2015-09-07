;;;;
;; Good and userfriendly settings
;;;;


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
    ac-slime
    auto-complete
    autopair
    clojure-mode
    clojure-test-mode
    coffee-mode
    csharp-mode
    deft
    erlang
    feature-mode
    flycheck
    gist
    go-mode
    graphviz-dot-mode
    haml-mode
    haskell-mode
    htmlize
    magit
    markdown-mode
    marmalade
    nodejs-repl
    nrepl
    o-blog
    org
    paredit
    php-mode
    puppet-mode
    restclient
    rvm
    scala-mode
    smex
    sml-mode
    solarized-theme
    web-mode
    writegood-mode
    yaml-mode
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
      initial-scratch-message nil
      initial-major-mode 'org-mode)

;; Don't want no X
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; More sensable region options
(delete-selection-mode t)
(transient-mark-mode t)

;; Share clipboard with the rest of the system.
(setq x-select-enable-clipboard t)

;; Display settings, not sure about these though...
(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (set-face-attribute 'default nil
                      :family "Inconsolata"
                      :height 140
                      :weight 'normal
                      :width 'normal)

  (when (functionp 'set-fontset-font)
    (set-fontset-font "fontset-default"
                      'unicode
                      (font-spec :family "DejaVu Sans Mono"
                                 :width 'normal
                                 :size 12.4
                                 :weight 'normal))))

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; Tab indentation
;; 4 spaces
;; No tabs
(setq tab-width 4
      indent-tabs-mode nil)

;; No backup files. Use git instead
(setq make-backup-files nil)

;; Answer with 'y' instead of 'yes'
(defalias 'yes-or-no-p 'y-or-n-p)

;; Show commands fast, vissable bell, Use echo area for y or n
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell t)
(show-paren-mode t)

;; Add a vendor directory
(defvar drPytho/vendor-dir (expand-file-name "vendor" user-emacs-directory))
(add-to-list 'load-path drPytho/vendor-dir)

(dolist (project (directory-files drPytho/vendor-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;;;;
;; Misc Cust Keymaps
;;;;

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

(setq org-log-done t
      org-todo-keywords '((sequence "TODO" "INPROGRESS" "DONE"))
      org-todo-keyword-faces '(("INPROGRESS" . (:foreground "blue" :weight bold))))
(add-hook 'org-mode-hook
          (lambda ()
            (flyspell-mode)))
(add-hook 'org-mode-hook
          (lambda ()
            (writegood-mode)))



;;;;
;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
(load "elisp-editing.el")

;; Langauage-specific
(load "setup-clojure.el")
(load "setup-js.el")

(load "golang.el")
