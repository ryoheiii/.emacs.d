;;; Code:
(use-package highlight-symbol
  :ensure t
  :bind
  (
   ([f3] . highlight-symbol-at-point)
   ([f4] . highlight-symbol-remove-all)
   )
  :config
  ;; 使いたい色を設定、repeat
  (setq highlight-symbol-colors '("DarkOrange" "DodgerBlue1" "DeepPink1"))
  )

(use-package auto-highlight-symbol
  :ensure t
  :config
  (global-auto-highlight-symbol-mode t)
  ;; memo
  ;; C-x C-aで一括rename
  )

(provide '20-highlight-symbol)
;;; 20-highlight-symbol ends here
