;; ;;;; $ sudo aptitude install fonts-inconsolata fonts-takao
;; ;;; フォント設定
;; ;; 121203
;; (set-default-font "Inconsolata-12")
;; (set-face-font 'variable-pitch "Inconsolata-12")
;; (set-fontset-font (frame-parameter nil 'font)
;;                   'japanese-jisx0208
;;                   '("Takaoゴシック" . "unicode-bmp")
;; )

;; ;; 半角と全角の比を1：2にする
;; (setq face-font-rescale-alist
;;       '((".*Hiragino_Mincho_pro.*" . 1.2)))
