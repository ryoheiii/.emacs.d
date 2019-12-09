;;; Code:

;; ;;; Please set your themes directory to 'custom-theme-load-path (replace-colorthemes)
;; (add-to-list 'custom-theme-load-path
;;              (file-name-as-directory "~/.emacs.d/loads/site-elisp/replace-colorthemes/"))

;;; 見た目
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
;; 空白や長すぎる行を視覚化する。
(use-package whitespace
  :ensure t
  :config
  (progn
    (setq whitespace-style '(face              ; faceを使って視覚化する。
                             tabs              ; タブ
                             spaces            ; スペース
                             newline
                             ;; indentation::tab
                             ;; indentation::space
                             empty             ; 先頭/末尾の空行
                             ;; lines-tail        ; develock.elに移行 (rainbow-delimiterが無効になるので、やっぱこっち)

                             space-mark        ; 表示のマッピング
                             ;; tab-mark ; ※  tab-markすると表示がずれる
                             newline-mark

                             ;; trailing          ; 行末の空白を対象とする。
                             ;; lines-tail        ; 長すぎる行のうち
                             ;;                   ; whitespace-line-column以降のみを
                             ;;                   ; 対象とする。
                             ;; indentation       ; indent-tabs-modeと逆のインデントを
                             ;;                   ; 対象とする。
                             ;;                   ; 2013-05-03
                             ;;space-before-tab  ; タブの前にあるスペースを対象とする。
                             ;; space-after-tab   ; タブの後にあるスペースを対象とする。
                             ))
    ;; (set-face-foreground 'whitespace-newline "gray40")
    (set-face-foreground 'whitespace-tab "DarkMagenta")
    ;; (set-face-foreground 'whitespace-tab "gray40")
    (set-face-background 'whitespace-tab 'nil)
    (set-face-underline  'whitespace-tab t)

    (setq whitespace-line-column 100) ;; lines-tail
    (setq whitespace-display-mappings
          '(
            (space-mark   ?\x3000 [?\□]); 全角スペース
            ;; (space-mark ?\u0020 [?\xB7])  ; 半角スペース
            ;; (newline-mark ?\n   [?\u21B5 ?\n] [?$ ?\n]) ; 改行記号
            ;; (newline-mark ?\n   [?\u2193 ?\n] [?$ ?\n]) ; 改行記号
            (tab-mark ?\t [?\u2192 ?\t] [?\\ ?\t]) ; タブ
            ))

    ;; スペースは全角のみを可視化
    (setq whitespace-space-regexp "\\(\u3000+\\)")

    ;; デフォルトで視覚化を有効にする。
    (global-whitespace-mode 1)
    )
  )


;;; 位置
;;現在行を目立たせる(ハイライト)
(defface hlline-face
  '((((class color)
      (background dark))
     (:background "dark slate gray"))
    (((class color)
      (Background Light))
     (:Background  "#98FB98"))
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
