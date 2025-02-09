;;; 01-ui.el --- UI 設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; Emacs の UI 設定

;;; Code:

(when (display-graphic-p)
  ;;;;;; [Group] Font Settings - フォント設定 ;;;;;;
  ;; Ricty Diminished Discord フォントの設定
  ;; インストール: sudo apt install fonts-ricty-diminished
  (let* ((font-name "Ricty Diminished Discord")
         (font-size 15)  ;; フォントサイズ（ポイント単位）
         (font-spec (format "%s-%d" font-name font-size))
         (japanese-charsets '(japanese-jisx0208
                              japanese-jisx0212
                              katakana-jisx0201
                              unicode)))
    ;; 基本フォントの設定
    (set-face-attribute 'default nil :font font-spec)

    ;; 日本語フォントの設定（幅を統一）
    (dolist (charset japanese-charsets)
      (set-fontset-font t charset (font-spec :family font-name))))
  )

(provide '01-ui)
;;; 01-ui.el ends here
