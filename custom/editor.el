
;; Autosave files - I use git so this is good. Read a bit more about it though
(setq auto-save-default t)

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
      standard-indent 4
      indent-tabs-mode nil)

;; No backup files. Use git instead
(setq make-backup-files nil)

;; NO TEMP FILES
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))


;;;;
;; Misc Cust Keymaps
;;;;

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

