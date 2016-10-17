;;; expand-region
;;; インクリメンタルに選択範囲を広げる

(require 'expand-region)
(global-set-key (kbd "C-,") 'er/expand-region)
(global-set-key (kbd "C-M-,") 'er/contract-region)

;; transient-mark-modeが nilでは動作しない
(transient-mark-mode t)
