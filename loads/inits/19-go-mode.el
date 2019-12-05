;(use-package go-mode-load
;  :ensure t)

;;; 環境変数の整備
;; go
;(use-package exec-path-from-shell
;  :ensure t)
;(let ((envs '("PATH" "GOPATH")))
; (exec-path-from-shell-copy-envs envs))

;;; Code:
;(add-hook 'go-mode-hook
;          '(lambda()
;            (setq c-basic-offset 4)
;            (setq indent-tabs-mode t)
;            (local-set-key (kbd "M-.") 'godef-jump)
;            (local-set-key (kbd "C-c r") 'go-remove-unused-imports)
;            (local-set-key (kbd "C-c i") 'go-goto-imports)
;            (local-set-key (kbd "C-c d") 'godoc)
;            ))

;(add-hook 'before-save-hook 'gofmt-before-save)

;(provide '19-go-mode)
;;; 19-go-mode.el ends here
