(require 'go-mode-load)

;;; Code:
(add-hook 'go-mode-hook
          '(lambda()
            (setq c-basic-offset 4)
            (setq indent-tabs-mode t)
            (local-set-key (kbd "M-.") 'godef-jump)
            (local-set-key (kbd "C-c C-r") 'go-remove-unused-imports)
            (local-set-key (kbd "C-c i") 'go-goto-imports)
            (local-set-key (kbd "C-c d") 'godoc)
            ))

(add-hook 'before-save-hook 'gofmt-before-save)

;;; godef
;; sudo apt-get instal mercurial (hg)

(provide '19-go-mode)
;;; 19-go-mode.el ends here
