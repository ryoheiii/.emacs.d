;;; Code:
(require 'highlight-symbol)

;; 使いたい色を設定、repeat
(setq highlight-symbol-colors '("DarkOrange" "DodgerBlue1" "DeepPink1"))

;; 適宜keybindの設定
(global-set-key (kbd "<f3>") 'highlight-symbol-at-point)
;; (global-set-key (kbd "C-<f3>") 'highlight-symbol-remove-all)
(global-set-key (kbd "<f4>") 'highlight-symbol-remove-all)

(provide '20-highlight-symbol)
;;; 20-highlight-symbol ends here
