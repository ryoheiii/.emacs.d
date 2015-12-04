;;; 見た目
;; 121207
;; ソースコードに色を付ける
(global-font-lock-mode t)
(transient-mark-mode t)

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
