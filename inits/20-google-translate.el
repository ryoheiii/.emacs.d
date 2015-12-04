;; http://qiita.com/catatsuy/items/ae9875706769d4f02317
;; google-translate.el
(require 'google-translate)

;; キーバインドの設定（お好みで）
(global-set-key (kbd "C-c C-t") 'google-translate-at-point)

;; 翻訳のデフォルト値を設定（en -> ja）
(custom-set-variables
  '(google-translate-default-source-language "en")
  '(google-translate-default-target-language "ja"))
