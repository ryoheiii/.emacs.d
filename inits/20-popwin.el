;; http://qiita.com/catatsuy/items/ae9875706769d4f02317
;; popwin.el
(require 'popwin)
;; おまじない（よく分かってない、、）
(setq display-buffer-function 'popwin:display-buffer)
;; ポップアップを画面下に表示
(setq popwin:popup-window-position 'bottom)

;; google-translate.elの翻訳バッファをポップアップで表示させる
(push '("*Google Translate*") popwin:special-display-config)
