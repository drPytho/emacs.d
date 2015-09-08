
;; Column number mode
(setq column-number-mode t)

;; Show commands fast, vissable bell, Use echo area for y or n
(setq echo-keystrokes 0.1
      use-dialog-box nil
      visible-bell nil)
(show-paren-mode t)

;; Answer with 'y' instead of 'yes'
(defalias 'yes-or-no-p 'y-or-n-p)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (set-face-attribute 'default nil
                      :family "Consolas"
		      :height 140
                      :weight 'normal)

  (when (functionp 'set-fontset-font)
    (set-fontset-font "fontset-default"
                      'unicode
                      (font-spec :family "Consolas"
                                 :width 'normal
                                 :size 12.4
                                 :weight 'normal))))

(setq-default indicate-empty-lines t)
(when (not indicate-empty-lines)
  (toggle-indicate-empty-lines))

;; Don't want no X
(scroll-bar-mode -1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; Show line numbers
(global-linum-mode)

;; No cursor blinking, it's distracting
(blink-cursor-mode 0)

;; yay rainbows! Color brackets based on depth
(rainbow-delimiters-mode t)

;; Highlight current line
(global-hl-line-mode 1)

;; Show the white space ending lines
(setq-default show-trailing-whitespace t)



;;;;
;; Theme
;;;;
(setq theme-path
      (expand-file-name "themes" user-emacs-directory))

(add-to-list 'custom-theme-load-path theme-path)
(add-to-list 'load-path theme-path)
(load-theme 'tomorrow-night t)



