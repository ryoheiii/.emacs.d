;;; 見た目
;; 121207
;; ソースコードに色を付ける
(global-font-lock-mode t)
(transient-mark-mode t)

;; 括弧の範囲色
;; (set-face-background 'show-paren-match-face "#500")
;; 選択領域の色
(set-face-background 'region "#555")
;; 行末の空白を強調表示
(setq-default show-trailing-whitespace t)
(set-face-background 'trailing-whitespace "#b14770")

;; 色設定
;; (defvar my/bg-color "#232323")
;; (set-face-attribute 'whitespace-trailing nil
;;                     :background my/bg-color
;;                     :foreground "DeepPink"
;;                     :underline t)
;; (set-face-attribute 'whitespace-tab nil
;;                     :background my/bg-color
;;                     :foreground "LightSkyBlue"
;;                     :underline t)
;; (set-face-attribute 'whitespace-space nil
;;                     :background my/bg-color
;;                     :foreground "GreenYellow"
;;                     :weight 'bold)
;; (set-face-attribute 'whitespace-empty nil
;;                     :background my/bg-color)

;;; 位置
;; 現在行を目立たせる(ハイライト)
(defface hlline-face
  '((((class color)
      (background dark))
     (:background "dark slate gray"))
    (((class color)
      (background light))
     (:background  "#98FB98"))
    (t
     ()))
  "*Face used by hl-line.")
(setq hl-line-face 'hlline-face)
(global-hl-line-mode)


;;; モードラインカラーの設定
;; 130516
(if window-system (progn
;; 文字の色を設定します。
;(add-to-list 'default-frame-alist '(foreground-color . "white"))
;; 背景色を設定します。
;(add-to-list 'default-frame-alist '(background-color . "navy"))
;; カーソルの色を設定します。
;(add-to-list 'default-frame-alist '(cursor-color . "yellow"))
;; マウスポインタの色を設定します。
(add-to-list 'default-frame-alist '(mouse-color . "SlateBlue2"))
;; モードラインの文字の色を設定します。
(set-face-foreground 'mode-line "black")
;; モードラインの背景色を設定します。
(set-face-background 'mode-line "#a6baff")
;; 選択中のリージョンの色を設定します。
;(set-face-background 'region "deeppink1")
;; モードライン（アクティブでないバッファ）の文字色を設定します。
;(set-face-foreground 'mode-line-inactive "black")
(set-face-foreground 'mode-line-inactive "gray30")
;; モードライン（アクティブでないバッファ）の背景色を設定します 
(set-face-background 'mode-line-inactive "white")
;(set-face-background 'mode-line-inactive "gray85")
))



;;; 背景を透過に
;; 121203
(add-to-list 'default-frame-alist '(alpha . (85 70)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; [基本] 背景色・透過
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 背景色の変更
;; 121203
(custom-set-faces
 '(default ((t (:background "black" :foreground "#55FF55"))))
 '(cursor (
           (((class color) (background dark )) (:background "#00AA00"))
           (((class color) (background light)) (:background "#999999"))
           (t ())
           )))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
