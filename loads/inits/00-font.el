(when (display-graphic-p)
  ;;; Ricty Diminished Discord フォントの設定
  ;; インストール: sudo apt install fonts-ricty-diminished
  (let ((font-name "Ricty Diminished Discord")
        (font-size 150)) ;; フォントサイズ
    ;; 基本フォント
    (set-face-attribute 'default nil :font (format "%s-%d" font-name (/ font-size 10)))

    ;; 日本語フォントの設定（幅を統一）
    (dolist (charset '(japanese-jisx0208
                       japanese-jisx0212
                       katakana-jisx0201
                       unicode))
      (set-fontset-font t charset (font-spec :family font-name))))
  )
