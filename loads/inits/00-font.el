(when window-system
  ;;; Ricty Diminished Discord フォントの設定
  ;; sudo apt install fonts-ricty-diminished
  (let ((font-name "Ricty Diminished Discord"))
    (set-face-attribute 'default nil :family font-name :height 150)
    (set-fontset-font t 'japanese-jisx0208 (font-spec :family font-name))
    (set-fontset-font t 'japanese-jisx0212 (font-spec :family font-name))
    (set-fontset-font t 'katakana-jisx0201 (font-spec :family font-name))
    )
  )
