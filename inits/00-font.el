;;;; $ sudo aptitude install ttf-inconsolata ttf-takao
;;; フォント設定
;; 121203
(set-default-font "Inconsolata-12")
(set-face-font 'variable-pitch "Inconsolata-12")
(set-fontset-font (frame-parameter nil 'font)
                  'japanese-jisx0208
                  '("Takaoゴシック" . "unicode-bmp")
)
