
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






