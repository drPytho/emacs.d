;;;;
;; Golang settings
;;;;

(setq exec-path (cons "/usr/local/bin" exec-path))
(add-to-list 'exec-path (expand-file-name "bin" (getenv GOPATH)))
(add-hook 'before-save-hook 'gofmt-before-save)




