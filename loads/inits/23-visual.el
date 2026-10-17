;;; 23-visual.el --- 視覚効果・アイコン・ハイライトの設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; 視覚的フィードバック、アイコン、ハイライト関連パッケージの設定

;;; Code:

;; 遅延ロード先とプラットフォーム固有定義をコンパイラへ伝える。
(declare-function color-saturate-name "color")
(declare-function perfect-margin-mode "perfect-margin")

;;; カーソルと編集箇所の強調
;;;; pulsar - カーソルの位置を明確にするためにエフェクトを追加
(use-package pulsar
  :straight t
  ;; tty ではパルスアニメーションが再描画のカクつきを生むため GUI 限定
  :if (display-graphic-p)
  :defer 0.5
  :config
  (pulsar-global-mode 1))

;;;; goggles - 編集箇所をハイライト
(use-package goggles
  :straight t
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t))

;;; 余白と本文の配置
;;;; spacious-padding - 画面に余白を付ける
(use-package spacious-padding
  :straight t
  :if (display-graphic-p)
  :defer 0.5
  :config
  (setq spacious-padding-widths
        '( :internal-border-width 15
           :header-line-width 4
           :mode-line-width 6
           :tab-width 4
           :right-divider-width 30
           :scroll-bar-width 8))

  ;; アクティブなモードラインは背景になじませ、非アクティブ側は境界を残す。
  (setq spacious-padding-subtle-frame-lines
        `( :mode-line-active 'default
           :mode-line-inactive vertical-border))
  (spacious-padding-mode 1))

;;;; perfect-margin - ウィンドウの左右の余白を調整して本文を中央へ寄せる
(use-package perfect-margin
  :straight t
  :defer 0.5
  :config
  (setq perfect-margin-ignore-filters nil)
  (perfect-margin-mode 1))

;;; 括弧の色分け
;; rainbow-delimiters で括弧の深さと対応関係を表示する。
(use-package rainbow-delimiters
  :straight t
  :hook (prog-mode . rainbow-delimiters-mode)
  :custom-face
  (rainbow-delimiters-depth-1-face ((t (:foreground "#c0c5ce"))))  ;; 青みがかったグレー
  (rainbow-delimiters-depth-2-face ((t (:foreground "#a3be8c"))))  ;; 淡い緑
  (rainbow-delimiters-depth-3-face ((t (:foreground "#b48ead"))))  ;; 薄い紫
  (rainbow-delimiters-depth-4-face ((t (:foreground "#96b5b4"))))  ;; 水色
  (rainbow-delimiters-depth-5-face ((t (:foreground "#ab7967"))))  ;; 落ち着いたオレンジ
  (rainbow-delimiters-depth-6-face ((t (:foreground "#ebcb8b"))))  ;; 柔らかい黄色
  (rainbow-delimiters-depth-7-face ((t (:foreground "#d08770"))))  ;; くすんだ赤
  (rainbow-delimiters-depth-8-face ((t (:foreground "#bf616a"))))  ;; 深めの赤
  (rainbow-delimiters-depth-9-face ((t (:foreground "#a3be8c"))))  ;; 緑リピート

  ;; 対応しない括弧をエラー色で目立たせる。
  (rainbow-delimiters-mismatched-face ((t (:foreground "#ff6c6b" :background "#5f0000" :weight bold :underline t))))
  (rainbow-delimiters-unmatched-face ((t (:foreground "#ffffff" :background "#af0000" :weight bold :underline t))))
  :bind (("C-c l" . rainbow-delimiters-using-stronger-colors))  ;; より強調した色に変更
  :config
  ;; color-saturate-name は autoload されない。起動直後は color.el が未ロードで、
  ;; 他パッケージの推移的ロードに依存していたため明示する
  (require 'color)
  ;; 強調した括弧の色を適用する関数
  (defun rainbow-delimiters-using-stronger-colors ()
    (interactive)
    (cl-loop for index from 1 to rainbow-delimiters-max-face-count
             do (let ((face (intern (format "rainbow-delimiters-depth-%d-face" index))))
                  (cl-callf color-saturate-name (face-foreground face) 50))))
  ;; 一致しない括弧をより目立たせる
  (set-face-attribute 'rainbow-delimiters-unmatched-face nil
                      :foreground     'unspecified
                      :inherit        'error
                      :strike-through t))

;;; GUI 用アイコン
;;;; Nerd-icons - グラフィック環境向けのアイコン
(use-package nerd-icons
  :straight t
  :if (display-graphic-p))

;;;; Nerd-icons-completion - vertico 等の補完パッケージでアイコンを表示
(use-package nerd-icons-completion
  :straight t
  :if (display-graphic-p)
  :hook (after-init . nerd-icons-completion-mode))

;;;; Nerd-icons-dired - dired でアイコンを表示
(use-package nerd-icons-dired
  :straight t
  :if (display-graphic-p)
  :hook (dired-mode . nerd-icons-dired-mode))

;;; モードラインの補助表示
;;;; nyan-mode - バッファ内の位置をキャラクターで表示
(use-package nyan-mode
  :straight t
  :if (display-graphic-p)
  :defer 0.5
  :custom
  (nyan-bar-length 24)
  :config
  (nyan-mode 1))

;;;; Minions - マイナーモードをハンバーガーメニューで表示
(use-package minions
  :straight t
  :defer 0.5
  :config
  (minions-mode 1))

;;; page-break-lines - 改ページ文字 ^L を横線として表示
(use-package page-break-lines
  :straight t
  :defer 0.5
  :config
  (global-page-break-lines-mode 1))

;;; Emacs Lisp の色付け
;;;; highlight-defined - 既知のシンボルに色を付ける
(use-package highlight-defined
  :straight t
  :hook (emacs-lisp-mode . highlight-defined-mode))

;;;; highlight-quoted - 引用符と引用記号に色を付ける
(use-package highlight-quoted
  :straight t
  :hook (emacs-lisp-mode . highlight-quoted-mode))

(provide '23-visual)
;;; 23-visual.el ends here
