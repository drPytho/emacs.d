;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This is the init.el file for Filip Lindvall
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;
;; Start the setup process
;;;;

;; Check for a sufficient version
(when (version< emacs-version "24.1")
  (error "Prelude requires at least GNU Emacs 24.1, but you're running %s" emacs-version))

;; Start with package handeling. Firstly init he load-path variable
;; This sets up the load path so that we can override it
(package-initialize nil)


;; Add a vendor directory
(defvar drPytho/vendor-dir (expand-file-name "vendor" user-emacs-directory))
(add-to-list 'load-path drPytho/vendor-dir)

;; Load everything in vendor dir.
(dolist (project (directory-files drPytho/vendor-dir t "\\w+"))
  (when (file-directory-p project)
    (add-to-list 'load-path project)))

;; Load the rest of the packages
(package-initialize nil)


;;;;
;; Simple settings
;;;;

;; I want to load all the packages. Lazy loading.
(setq package-enable-at-startup nil)

;; Define package repos
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(unless (assoc-default "melpa" package-archives)
  (add-to-list 'package-archives '("melpa-orig" . "http://melpa.org/packages/") t)
  (package-refresh-contents))

;; Install use-package if not installed. We will later use `use-package`
;; to install the rest of the packages.
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
;; Import use-package so that we can use it.
(setq use-package-verbose t)
(require 'use-package)


;; This is me
(setq user-full-name "Filip Lindvall"
      user-email-address "drpytho@gmail.com")
;; And these are my secrets
;; easy to just push .emacs.d without worrying
(load "~/.emacs.secrets" t)

;; Send a friendly message letting the user
;; know work is being done.
(message "Emacs is powering up... Be patient, Master %s!" user-full-name)

;; Tell emacs to get the lates bins possible.
(setq load-prefer-newer t)

;; Backups may save my ass. If they ever come in to use.
;; Buy me an icecream for these
(setq backup-directory-alist '(("." . "~/.emacs.d/backups")))
(setq delete-old-versions -1)
(setq version-control t)
(setq vc-make-backup-files t)
(setq auto-save-file-name-transforms '((".*" "~/.emacs.d/auto-save-list/" t)))

(setq savehist-file "~/.emacs.d/savehist")
(savehist-mode 1)
(setq history-length t)
(setq history-delete-duplicates t)
(setq savehist-save-minibuffer-history 1)
(setq savehist-additional-variables
      '(kill-ring
        search-ring
        regexp-search-ring))

;; I hate the unessesary padding of the menues.
;; Get ridd of them.
;; Keep this high up in init.d I never want to see those menues
(when window-system
  (tooltip-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1))

(setq sentence-end-double-space nil)

;; Instead of haveing to answer yes or no. You can answer y or n.
(fset 'yes-or-no-p 'y-or-n-p)

;; Autosave files - I use git so this is good. Read a bit more about it though
(setq auto-save-default t)

;; Highlight current line
(global-hl-line-mode 1)


;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

;; Skip the spash screen
;; No initial scratch message
;; Set us in org mode.
(setq inhibit-splash-screen t
      initial-scratch-message "Hello master Filip, Welcome to Emacs"
      initial-major-mode 'org-mode)

;; More sensable region options
(delete-selection-mode t)
(transient-mark-mode t)

;; Share clipboard with the rest of the system.
(setq x-select-enable-clipboard t)

;; Tab indentation
;; 4 spaces
;; No tabs
(setq-default tab-width 4
              indent-tabs-mode nil)

;; Set meta to be cmd instead of alt
(setq mac-option-modifier nil
      mac-command-modifier 'meta)

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(require 'saveplace)
(setq-default save-place t)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (expand-file-name "places" user-emacs-directory))

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; Show line numbers
(global-linum-mode)

;; No cursor blinking, it's distracting
(blink-cursor-mode 0)

;; yay rainbows! Color brackets based on depth
;; (rainbow-delimiters-mode t)

;; Show the white space ending lines
(setq-default show-trailing-whitespace t)

;; Use utf-8 as encoding
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))


(setq set-mark-command-repeat-pop t)

(require 'find-dired)
(setq find-ls-option '("-print0 | xargs -0 ls -ld" . "-ld"))

(require 'recentf)
(setq recentf-max-saved-items 200
      recentf-max-menu-items 15)
(recentf-mode)

(setq hippie-expand-try-functions-list
      '(yas-hippie-try-expand
        try-expand-all-abbrevs
        try-complete-file-name-partially
        try-complete-file-name
        try-expand-dabbrev
        try-expand-dabbrev-from-kill
        try-expand-dabbrev-all-buffers
        try-expand-list
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

(setq default-cursor-color "gray")
(setq yasnippet-can-fire-cursor-color "purple")

(add-hook 'post-command-hook 'my/change-cursor-color-when-can-expand)

(column-number-mode 1)

(setq vc-diff-switches '("-b" "-B" "-u"))
(setq vc-git-diff-switches nil)

(use-package fill-column-indicator
  :config (fci-mode 1))

;;;;
;; My custom functions and marcos
;;;;

(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))

(defun endless/load-gh-pulls-mode ()
  "Start `magit-gh-pulls-mode' only after a manual request."
  (interactive)
  (require 'magit-gh-pulls)
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
  (magit-gh-pulls-mode 1)
  (magit-gh-pulls-reload))

(defun my/magit-commit-all ()
  "Publish the current file and commit all the current changes."
  (interactive)
  (magit-status default-directory)
  (magit-stage-all)
  (call-interactively 'magit-log-edit))

(defvar my/javascript-test-regexp (concat (regexp-quote "/** Testing **/") "\\(.*\n\\)*")
  "Regular expression matching testing-related code to remove.
See `my/copy-javascript-region-or-buffer'.")

(defun my/copy-javascript-region-or-buffer (beg end)
  "Copy the active region or the buffer, wrapping it in script tags.
Add a comment with the current filename and skip test-related
code. See `my/javascript-test-regexp' to change the way
test-related code is detected."
  (interactive "r")
  (unless (region-active-p)
    (setq beg (point-min) end (point-max)))
  (kill-new
   (concat
    "<script type=\"text/javascript\">\n"
    (if (buffer-file-name) (concat "// " (file-name-nondirectory (buffer-file-name)) "\n") "")
    (replace-regexp-in-string
     my/javascript-test-regexp
     ""
     (buffer-substring (point-min) (point-max))
     nil)
    "\n</script>")))

(defun my/hippie-expand-maybe (arg)
  "Try to expand text before point, using multiple methods.
The expansion functions in `hippie-expand-try-functions-list' are
tried in order, until a possible expansion is found.  Repeated
application of `hippie-expand' inserts successively possible
expansions.
With a positive numeric argument, jumps directly to the ARG next
function in this list.  With a negative argument or just \\[universal-argument],
undoes the expansion."
  (interactive "P")
  (require 'hippie-exp)
  (if (or (not arg)
          (and (integerp arg) (> arg 0)))
      (let ((first (or (= he-num -1)
                       (not (equal this-command last-command)))))
        (if first
            (progn
              (setq he-num -1)
              (setq he-tried-table nil)))
        (if arg
            (if (not first) (he-reset-string))
          (setq arg 0))
        (let ((i (max (+ he-num arg) 0)))
          (while (not (or (>= i (length hippie-expand-try-functions-list))
                          (apply (nth i hippie-expand-try-functions-list)
                                 (list (= he-num i)))))
            (setq i (1+ i)))
          (setq he-num i))
        (if (>= he-num (length hippie-expand-try-functions-list))
            (progn (setq he-num -1) nil)
          (if (and hippie-expand-verbose
                   (not (window-minibuffer-p)))
              (message "Using %s"
                       (nth he-num hippie-expand-try-functions-list)))))
    (if (and (>= he-num 0)
             (eq (marker-buffer he-string-beg) (current-buffer)))
        (progn
          (setq he-num -1)
          (he-reset-string)
          (if (and hippie-expand-verbose
                   (not (window-minibuffer-p)))
              (message "Undoing expansions"))))))

;; It will test whether it can expand, if yes, cursor color -> green.
(defun yasnippet-can-fire-p (&optional field)
  (interactive)
  (setq yas--condition-cache-timestamp (current-time))
  (let (templates-and-pos)
    (unless (and yas-expand-only-for-last-commands
                 (not (member last-command yas-expand-only-for-last-commands)))
      (setq templates-and-pos (if field
                                  (save-restriction
                                    (narrow-to-region (yas--field-start field)
                                                      (yas--field-end field))
                                    (yas--templates-for-key-at-point))
                                (yas--templates-for-key-at-point))))
    (and templates-and-pos (first templates-and-pos))))

(defun my/change-cursor-color-when-can-expand (&optional field)
  (interactive)
  (when (eq last-command 'self-insert-command)
    (set-cursor-color (if (my/can-expand)
                          yasnippet-can-fire-cursor-color
                        default-cursor-color))))

(defun my/can-expand ()
  "Return true if right after an expandable thing."
  (or (abbrev--before-point) (yasnippet-can-fire-p)))

(defun my/insert-space-or-expand ()
  "For binding to the SPC SPC keychord."
  (interactive)
  (condition-case nil (or (my/hippie-expand-maybe nil) (insert "  "))))

(defun sanityinc/kill-back-to-indentation ()
  "Kill from point back to the first non-whitespace character on the line."
  (interactive)
  (let ((prev-pos (point)))
    (back-to-indentation)
    (kill-region (point) prev-pos)))

(defun my/sort-sexps-in-region (beg end)
    "Can be handy for sorting out duplicates.
Sorts the sexps from BEG to END. Leaves the point at where it
couldn't figure things out (ex: syntax errors)."
    (interactive "r")
    (let ((input (buffer-substring beg end))
          list last-point form result)
      (save-restriction
        (save-excursion
          (narrow-to-region beg end)
          (goto-char (point-min))
          (setq last-point (point-min))
          (setq form t)
          (while (and form (not (eobp)))
            (setq form (ignore-errors (read (current-buffer))))
            (when form
              (add-to-list
               'list
               (cons
                (prin1-to-string form)
                (buffer-substring last-point (point))))
              (setq last-point (point))))
          (setq list (sort list (lambda (a b) (string< (car a) (car b)))))
          (delete-region (point-min) (point))
          (insert (mapconcat 'cdr list "\n"))))))

(defun sanityinc/eval-last-sexp-or-region (prefix)
  "Eval region from BEG to END if active, otherwise the last sexp."
  (interactive "P")
  (if (and (mark) (use-region-p))
      (eval-region (min (point) (mark)) (max (point) (mark)))
    (eval-last-sexp prefix)))

(defmacro my/insert-unicode (unicode-name)
  `(lambda () (interactive)
     (insert-char (cdr (assoc-string ,unicode-name (ucs-names))))))

;; from FAQ at http://web-mode.org/ for smartparens
(defun my/web-mode-hook ()
  (setq web-mode-enable-auto-pairing nil))

(defun my/sp-web-mode-is-code-context (id action context)
  (when (and (eq action 'insert)
             (not (or (get-text-property (point) 'part-side)
                      (get-text-property (point) 'block-side))))
    t))

(defun prelude-copy-file-name-to-clipboard ()
  "Copy the current buffer file name to the clipboard."
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (kill-new filename)
      (message "Copied buffer file name '%s' to the clipboard." filename))))

(defun my/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

(defun my/key-chord-define (keymap keys command)
  "Define in KEYMAP, a key-chord of two keys in KEYS starting a COMMAND.
\nKEYS can be a string or a vector of two elements. Currently only elements
that corresponds to ascii codes in the range 32 to 126 can be used.
\nCOMMAND can be an interactive function, a string, or nil.
If COMMAND is nil, the key-chord is removed.

MODIFICATION: Do not define the transposed key chord.
"
  (if (/= 2 (length keys))
      (error "Key-chord keys must have two elements"))
  ;; Exotic chars in a string are >255 but define-key wants 128..255 for those
  (let ((key1 (logand 255 (aref keys 0)))
        (key2 (logand 255 (aref keys 1))))
    (define-key keymap (vector 'key-chord key1 key2) command)))

(fset 'key-chord-define 'my/key-chord-define)

(defun my/switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun my/vsplit-last-buffer (prefix)
  "Split the window vertically and display the previous buffer."
  (interactive "p")
  (split-window-vertically)
  (other-window 1 nil)
  (if (= prefix 1)
    (switch-to-next-buffer)))
(defun my/hsplit-last-buffer (prefix)
  "Split the window horizontally and display the previous buffer."
  (interactive "p")
  (split-window-horizontally)
  (other-window 1 nil)
  (if (= prefix 1) (switch-to-next-buffer)))


(defadvice kill-region (before slick-cut activate compile)
  "When called interactively with no active region, kill a single line instead."
  (interactive
    (if mark-active (list (region-beginning) (region-end))
      (list (line-beginning-position)
	    (line-beginning-position 2)))))

(defun my/def-rep-command (alist)
    "Return a lambda that calls the first function of ALIST.
It sets the transient map to all functions of ALIST,
allowing you to repeat those functions as needed."
    (let ((keymap (make-sparse-keymap))
                  (func (cdar alist)))
      (mapc (lambda (x)
              (when x
                (define-key keymap (kbd (car x)) (cdr x))))
            alist)
      (lambda (arg)
        (interactive "p")
        (when func
          (funcall func arg))
        (set-transient-map keymap t))))

;;;;
;; Custom keybinds
;;;;

(bind-key "C-c r n" 'rename-file-and-buffer)

(bind-key "C-s" 'isearch-forward-regexp)
(bind-key "C-r" 'isearch-backward-regexp)
(bind-key "C-M-s" 'isearch-forward)
(bind-key "C-M-r" 'isearch-backward)

(bind-key "RET" 'newline-and-indent)
(bind-key "C-;" 'comment-or-uncomment-region)
(bind-key "C-+" 'text-scale-increase)
(bind-key "C--" 'text-scale-decrease)
(bind-key "C-c C-k" 'compile)
(bind-key "C-x p" 'pop-to-mark-command)
(bind-key "C-x 2" 'my/vsplit-last-buffer)
(bind-key "C-x 3" 'my/hsplit-last-buffer)

(bind-key "C-a" 'my/smarter-move-beginning-of-line)

(bind-key "C-x 8 s" (my/insert-unicode "ZERO WIDTH SPACE"))
(bind-key "C-x 8 S" (my/insert-unicode "SNOWMAN"))

(bind-key "M-SPC" 'cycle-spacing)
(bind-key "M-/" 'hippie-expand)

(bind-key "C-M-<backspace>" 'sanityinc/kill-back-to-indentation)

(define-key emacs-lisp-mode-map (kbd "C-c .") 'find-function-at-point)
(bind-key "C-c f" 'find-function)
(bind-key "M-:" 'pp-eval-expression)

(bind-key "C-x C-e" 'sanityinc/eval-last-sexp-or-region emacs-lisp-mode-map)

;;;;
;; Theme
;;;;
(setq theme-path
      (expand-file-name "themes" user-emacs-directory))

(add-to-list 'custom-theme-load-path theme-path)
(add-to-list 'load-path theme-path)
(load-theme 'tomorrow-night t)


;;;;
;; Packages
;;;;

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :config (progn
	    (exec-path-from-shell-initialize)
	    (exec-path-from-shell-copy-envs
	     '("PATH" "GOPATH"))))

(use-package restclient)

(use-package writegood-mode
  :bind ("C-c g g" . writegood-mode))


(use-package flycheck
  :ensure t
  :defer t)

(use-package rainbow-delimiters)

(use-package multiple-cursors
  :ensure t :defer t
  :bind
   (("C-c m t" . mc/mark-all-like-this)
    ("C-c m m" . mc/mark-all-like-this-dwim)
    ("C-c m l" . mc/edit-lines)
    ("C-c m e" . mc/edit-ends-of-lines)
    ("C-c m a" . mc/edit-beginnings-of-lines)
    ("C-c m n" . mc/mark-next-like-this)
    ("C-c m p" . mc/mark-previous-like-this)
    ("C-c m s" . mc/mark-sgml-tag-pair)
    ("C-c m d" . mc/mark-all-like-this-in-defun)))

(use-package ace-jump-mode
  :ensure t
  :bind ("C-c SPC" . ace-jump-mode))

(use-package ace-window
  :ensure t :defer t
  :config (setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?p))
  :bind ("C-x o" . ace-window))

(use-package smartparens
  :ensure t
  :defer 1
  :init
  (progn
    (require 'smartparens-config)
    (add-hook 'emacs-lisp-mode-hook 'smartparens-mode)
    (add-hook 'emacs-lisp-mode-hook 'show-smartparens-mode)))


;; Get auto-compile to compile the elisp to get the newest
;; version of it all the time.
(use-package auto-compile
	     :ensure t
	     :config (auto-compile-on-save-mode))

;; Basicly to be able to use CMD-z for windows, buffers and layouts.
(use-package winner
  :ensure t
  :defer t
  :config (winner-mode 1))

;; Helm, is help. I need to learn more about heml
(use-package helm
  :ensure t
  :diminish helm-mode
  :init
  (progn
    (require 'helm-config)
    (setq helm-candidate-number-limit 100)
    ;; From https://gist.github.com/antifuchs/9238468
    (setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
          helm-input-idle-delay 0.01  ; this actually updates things
                                        ; reeeelatively quickly.
          helm-yas-display-key-on-candidate t
          helm-quick-update t
          helm-M-x-requires-pattern nil
          helm-ff-skip-boring-files t)
    (helm-mode))
  :bind (("C-c h" . helm-mini)
         ("C-h a" . helm-apropos)
         ("C-x C-b" . helm-buffers-list)
         ("C-x b" . helm-buffers-list)
         ("M-y" . helm-show-kill-ring)
         ("M-x" . helm-M-x)
         ("C-x c o" . helm-occur)
         ("C-x c s" . helm-swoop)
         ("C-x c y" . helm-yas-complete)
         ("C-x c Y" . helm-yas-create-snippet-on-region)
         ("C-x c b" . my/helm-do-grep-book-notes)
         ("C-x c SPC" . helm-all-mark-rings)))
(ido-mode -1) ;; Turn off ido mode in case I enabled it accidentally

(use-package helm-descbinds
  :defer t
  :bind (("C-h b" . helm-descbinds)
         ("C-h w" . helm-descbinds)))

;; Make the mode-line sexy again.
(use-package smart-mode-line
  :defer 1)

;; Bring up the minibuffer to a larger one to make it easier to
;; do commands.
(use-package miniedit
  :defer t
  :ensure t
  :commands minibuffer-edit
  :init (miniedit-install))

;; Visualise your undo history as a tree.
(use-package undo-tree
  :defer 1
  :ensure t
  :diminish undo-tree-mode
  :init
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

;; Shows you your keybindings
(use-package guide-key
  :defer 2 ;; Idle seconds
  :diminish guide-key-mode
  :init (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-c"))
  :config (guide-key-mode 1))  ; Enable guide-key-mode

;; Helm search. List in other buffer...
(use-package helm-swoop
 :defer t
 :bind
 (("C-S-s" . helm-swoop)
  ("M-i" . helm-swoop)
  ("M-s s" . helm-swoop)
  ("M-s M-s" . helm-swoop)
  ("M-I" . helm-swoop-back-to-last-point)
  ("C-c M-i" . helm-multi-swoop)
  ("C-x M-i" . helm-multi-swoop-all)
  )
 :config
 (progn
   (define-key isearch-mode-map (kbd "M-i") 'helm-swoop-from-isearch)
   (define-key helm-swoop-map (kbd "M-i") 'helm-multi-swoop-all-from-helm-swoop)))

;; Easielly switch between windows.
(use-package windmove
  :defer t
  :bind
  (("C-c f" . windmove-right)
   ("C-c b" . windmove-left)
   ("C-c p" . windmove-up)
   ("C-c n" . windmove-down)))

;; Learn to use me
;;(use-package key-chord)

;; Jump to similar symbols
;; HEADS UP: I changed idle to init.
(use-package smartscan
  :defer t
  :config (global-smartscan-mode t))

;; We do not like certan words
;; HEADS UP: I changed idle to init
(use-package artbollocks-mode
  :defer t
  :init
  (progn
    (setq artbollocks-weasel-words-regex
          (concat "\\b" (regexp-opt
                         '("one of the"
                           "should"
                           "just"
                           "sort of"
                           "a lot"
                           "probably"
                           "maybe"
                           "perhaps"
                           "I think"
                           "really"
                           "pretty"
                           "nice"
                           "action"
                           "utilize"
                           "leverage") t) "\\b"))
    ;; Don't show the art critic words, or at least until I figure
    ;; out my own jargon
    (setq artbollocks-jargon nil)))

;; Well ORG mode. 
(use-package org)

;;;;
;; Coding packages
;;;;

;; Genpop
;;Snippets
(use-package yasnippet
  :ensure t
  :defer 2 ;; 2 idle seconds
  :diminish yas-minor-mode
  :commands yas-global-mode
  :init
  (progn
    (add-hook 'hippie-expand-try-functions-list 'yas-hippie-try-expand)
    (setq yas-key-syntaxes '("w_" "w_." "^ "))
    (setq yas-installed-snippets-dir "~/elisp/yasnippet-snippets")
    (setq yas-expand-only-for-last-commands '(self-insert-command))
    (yas-global-mode 1)
    (bind-key "\t" 'hippie-expand yas-minor-mode-map)
    (add-to-list 'yas-prompt-functions 'shk-yas/helm-prompt)))

;;git
(use-package magit
      :ensure t
      :defer t
      :load-path "~/elisp/magit"
      :config
      (progn
        (setq magit-diff-options '("-b")) ; ignore whitespace
        (define-key magit-mode-map "#gg" 'endless/load-gh-pulls-mode)
        (defvar my/magit-limit-to-directory nil "Limit magit status to a specific directory.")
        (defun my/magit-status-in-directory (directory)
          "Displays magit status limited to DIRECTORY.
  Uses the current `default-directory', or prompts for a directory
  if called with a prefix argument. Sets `my/magit-limit-to-directory'
  so that it's still active even after you stage a change. Very experimental."
          (interactive (list (expand-file-name
                              (if current-prefix-arg
                                  (read-directory-name "Directory: ")
                                default-directory))))
          (setq my/magit-limit-to-directory directory)
          (magit-status directory))

        (defadvice magit-insert-untracked-files (around sacha activate)
          (if my/magit-limit-to-directory
              (magit-with-section (section untracked 'untracked "Untracked files:" t)
                (let ((files (cl-mapcan
                              (lambda (f)
                                (when (eq (aref f 0) ??) (list f)))
                              (magit-git-lines
                               "status" "--porcelain" "--" my/magit-limit-to-directory))))
                  (if (not files)
                      (setq section nil)
                    (dolist (file files)
                      (setq file (magit-decode-git-path (substring file 3)))
                      (magit-with-section (section file file)
                        (insert "\t" file "\n")))
                    (insert "\n"))))
            ad-do-it))

        (defadvice magit-insert-unstaged-changes (around sacha activate)
          (if my/magit-limit-to-directory
              (let ((magit-current-diff-range (cons 'index 'working))
                    (magit-diff-options (copy-sequence magit-diff-options)))
                (magit-git-insert-section (unstaged "Unstaged changes:")
                    #'magit-wash-raw-diffs
                  "diff-files"
                  "--" my/magit-limit-to-directory
                  ))
            ad-do-it))

        (defadvice magit-insert-staged-changes (around sacha activate)
          "Limit to `my/magit-limit-to-directory' if specified."
          (if my/magit-limit-to-directory
              (let ((no-commit (not (magit-git-success "log" "-1" "HEAD"))))
                (when (or no-commit (magit-anything-staged-p))
                  (let ((magit-current-diff-range (cons "HEAD" 'index))
                        (base (if no-commit
                                  (magit-git-string "mktree")
                                "HEAD"))
                        (magit-diff-options (append '("--cached") magit-diff-options)))
                    (magit-git-insert-section (staged "Staged changes:")
                        (apply-partially #'magit-wash-raw-diffs t)
                      "diff-index" "--cached" base "--" my/magit-limit-to-directory))))
            ad-do-it)))
      :bind (("C-x v d" . magit-status)
             ("C-x v C-d" . my/magit-status-in-directory)
             ("C-x v p" . magit-push)
             ("C-x v c" . my/magit-commit-all)))


(use-package magit-gh-pulls
  :ensure t :defer t)

(use-package git-messenger
  :ensure t :defer t
  :bind (("C-x v m" . git-messenger:popup-message)))

;;Projects
(use-package projectile
  :ensure t
  :defer 1
  :diminish projectile-mode
  :init
  (progn
    (setq projectile-keymap-prefix (kbd "C-c p"))
    (setq projectile-completion-system 'default)
    (setq projectile-enable-caching t)
    (projectile-global-mode)))

(use-package helm-projectile
   :defer t :ensure t
   :ensure helm-projectile)

;;Autocomplete
(use-package company
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'company-mode))

;; Expands region with sensable stepps
(use-package expand-region
  :ensure expand-region
  :defer t
  :bind ("C-=" . er/expand-region))

;; HTML, CSS, JS

;; Web mode
(use-package web-mode
  :ensure t
  :defer t
  :mode "\\.html?\\'"
  :config
  (progn
    (setq tab-width 2
        ;; this will make sure spaces are used instead of tabs
          indent-tabs-mode nil)
    (setq tab-stop-list (number-sequence 2 120 2))
    (setq web-mode-markup-indent-offset 2)
    (setq css-mode-markup-indent-offset 2)
    (setq web-mode-enable-current-element-highlight t)
    (setq web-mode-ac-sources-alist
          '(("css" . (ac-source-css-property))
            ("html" . (ac-source-words-in-buffer ac-source-abbrev)))
          )))

(use-package js2-mode
  :ensure t
  :defer t
  :commands js2-mode
  :init
  (progn
    (add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
    (add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-mode))
    (setq-default js2-basic-offset 2)
    (add-to-list 'interpreter-mode-alist (cons "node" 'js2-mode)))
  :config
  (progn
    (js2-imenu-extras-setup)
    (bind-key "C-x C-e" 'js-send-last-sexp js2-mode-map)
    (bind-key "C-M-x" 'js-send-last-sexp-and-go js2-mode-map)
    (bind-key "C-c b" 'js-send-buffer js2-mode-map)
    (bind-key "C-c C-b" 'js-send-buffer-and-go js2-mode-map)
    (bind-key "C-c w" 'my/copy-javascript-region-or-buffer js2-mode-map)
    (bind-key "C-c l" 'js-load-file-and-go js2-mode-map)))

(use-package tern
  :ensure t
  :defer 1
  :init
  (progn
  (setq tern-command '("cmd" "/c" "tern"))
  (add-hook 'js2-mode-hook 'tern-mode)))

(use-package company-tern
  :ensure t
  :defer t
  :init (add-to-list 'company-backends 'company-tern))

(use-package coffee-mode
  :ensure t
  :defer 1
  :init (setq-default coffee-js-mode 'js2-mode coffee-tab-width 2))

(use-package scss-mode
  :ensure t
  :defer 1
  :mode "\\.scss\\'"
  :config (progn
            (setq c-basic-offset 2)
            (setq css-mode-markup-indent-offset 2)
            (setq scss-indent-offset 2)
            (setq css-indent-offset 2)
            (setq tab-width 2
                  indent-tabs-mode nil)
            (setq tab-stop-list (number-sequence 2 120 2))))

(use-package jade-mode
  :ensure t
  :defer 1
  :mode "\\.jade\\'")

;; Lets you push code to chrome
(use-package skewer-mode
  :ensure t :defer t
  :config (skewer-setup))

;; Working with tags
(use-package tagedit
   :ensure t
   :defer 1
   :init (add-hook 'web-mode-hook 'tagedit-mode))


;;;;
;; CC-mode
;; This is for c, c++, java, mm
;;;;
(setq c-basic-offset 4)

(defconst my-c-style
  '((c-tab-always-indent . t)
    (c-comment-only-line-offset . 4)
    (c-hanging-braces-alist . ((substatement-open after)
                               (brace-list-open)))
    (c-hanging-colons-alist . ((member-init-intro before)
                               (inher-intro)
                               (case-label after)
                               (label after)
                               (access-label after)))
    (c-cleanup-list . (scope-operator
                       empty-defun-braces
                       defun-close-semi))
    (c-offsets-alist . ((arglist-close . c-lineup-arglist)
                        (substatement-open . 0)
                        (case-label . 4)
                        (block-open . 0)
                        (innamespace . [8])
                        (knr-argdecl-intro . -)))
    (c-echo-syntactic-information-p . t))
  "My C Programming Style")

;; Customizations for all modes in CC Mode.
(defun my-c-mode-common-hook ()
  ;; other customizations
  (setq tab-width 4
        ;; this will make sure spaces are used instead of tabs
        indent-tabs-mode nil)
  (setq c-default-style '((java-mode . "java")
			  (awk-mode . "awk")
			  (other . "linux")))
  ;; set my personal style for the current buffer
  (c-set-style "PERSONAL")
  ;; we like auto-newline, but not hungry-delete
  (c-toggle-auto-newline 1))

(add-hook 'c-mode-common-hook 'my-c-mode-common-hook)




;; Lisp (Emacs lisp)
;; Docs for emacs lisp
(use-package "eldoc"
  :diminish eldoc-mode
  :commands turn-on-eldoc-mode
  :defer t
  :init
  (progn
  (add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
  (add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)))

;; C-c C-v l : elint current buffer in clean environment.
;; C-c C-v L : elint current buffer by multiple emacs binaries.
;;             See `erefactor-lint-emacsen'
;; C-c C-v r : Rename symbol in current buffer.
;;             Resolve `let' binding as long as i can.
;; C-c C-v R : Rename symbol in requiring modules and current buffer.
;; C-c C-v h : Highlight current symbol in this buffer
;;             and suppress `erefacthr-highlight-mode'.
;; C-c C-v d : Dehighlight all by above command.
;; C-c C-v c : Switch prefix bunch of symbols.
;;             ex: '(hoge-var hoge-func) -> '(foo-var foo-func)
;; C-c C-v ? : Display flymake elint warnings/errors

(use-package erefactor
    :ensure t
    :defer t
    :config
    (define-key emacs-lisp-mode-map "\C-c\C-v" erefactor-map))

;; Common lisp editing extentions
(use-package redshank
    :ensure t
    :defer t
    :init (add-hook 'emacs-lisp-mode-hook 'redshank-mode))

;; Markdown
(use-package markdown-mode
  :defer t
  :init (progn
	  (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
	  (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
	  (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))))

(use-package go-mode
  :defer t
  )


