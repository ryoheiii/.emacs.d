;;; Code:
(require 'google-c-style)

;; google-c-style.elの定義関数
(add-hook 'c-mode-common-hook 'google-set-c-style)
(add-hook 'c-mode-common-hook 'google-make-newline-indent)

(provide '20-google-c-style)
;;; 20-google-c-style ends here
