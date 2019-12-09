;;; expand-region
;;; インクリメンタルに選択範囲を広げる
(use-package expand-region
  :ensure t
  :bind (("C-," . er/expand-region)
         ("C-M-," . er/contract-region))
  :config
  ;; transient-mark-modeが nilでは動作しない
  (transient-mark-mode t)
  )
