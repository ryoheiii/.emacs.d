;;; Code:

;;; 見た目
;; 121207
;; ソースコードに色を付ける
(global-font-lock-mode t)
(transient-mark-mode t)

;;; load your favorite theme
;; https://github.com/emacs-jp/replace-colorthemes/blob/master/screenshots.md
(load-theme 'hober t t)
(enable-theme 'hober)
;; (load-theme 'railscast t t)
;; (enable-theme 'railscast)


;;; 空白
;; 2011-10-27
(require 'whitespace)
;; 1行が80桁を超えたら長すぎると判断する。; 無効
;;; (setq whitespace-line-column 80)
(setq whitespace-style '(face       ; faceを使って視覚化する。
                         trailing   ; 行末の空白を対象とする。
                         tabs       ; タブ
                         spaces     ; スペース
                         space-mark ; 表示のマッピング
                         tab-mark
                         ))
(setq whitespace-display-mappings
      '((space-mark ?\u3000 [?\u25a1])
        ;; WARNING: the mapping below has a problem.
        ;; When a TAB occupies exactly one column, it will display the
        ;; character ?\xBB at that column followed by a TAB which goes to
        ;; the next TAB column.
        ;; If this is a problem for you, please, comment the line below.
        (tab-mark ?\t [?\u00BB ?\t] [?\\ ?\t])))
;; スペースは全角のみを可視化
(setq whitespace-space-regexp "\\(\u3000+\\)")
;; デフォルトで視覚化を有効にする。
(global-whitespace-mode 1)

;;; 位置
;;現在行を目立たせる(ハイライト)
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


;;; 背景を透過に
;; 121203
(if window-system
    (progn
      (add-to-list 'default-frame-alist '(alpha . (85 70)))
      )
)

(provide '00-color)
;;; 00-color.el ends here
