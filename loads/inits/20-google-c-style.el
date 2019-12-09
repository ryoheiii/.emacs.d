;;; Code:
(use-package google-c-style
  :ensure t
  :init
  (add-hook 'c-mode-common-hook
            (lambda ()
              (google-set-c-style)
              (google-make-newline-indent)))
  :config
  ;; (c-set-offset 'statement-case-open 0)
  )

(provide '20-google-c-style)
;;; 20-google-c-style ends here
