;;; 23-visual.el --- 視覚効果・アイコン・ハイライトの設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; 視覚的フィードバック、アイコン、ハイライト関連パッケージの設定

;;; Code:

;;;;; [Group] Visual - 視覚効果関連 ;;;;;
;;; pulsar - カーソルの位置を明確にするためにエフェクトを追加
(use-package pulsar
  :straight t
  :if (display-graphic-p)
  :defer t
  :init
  ;; pulsar-global-mode は autoload 済み → タイマーでパッケージロード + :config 実行
  (run-with-idle-timer 0.5 nil #'pulsar-global-mode 1)
  )

;;; goggles - 編集箇所をハイライト
(use-package goggles
  :straight t
  :hook ((prog-mode text-mode) . goggles-mode)
  :config
  (setq-default goggles-pulse t)
  )

;;; spacious-padding - 画面に余白を付ける
(use-package spacious-padding
  :straight t
  :if (display-graphic-p)
  :defer t
  :init
  ;; spacious-padding-mode は autoload 済み → タイマーでパッケージロード + :config 実行
  (run-with-idle-timer 0.5 nil #'spacious-padding-mode 1)
  :config
  (setq spacious-padding-widths
        '( :internal-border-width 15
           :header-line-width 4
           :mode-line-width 6
           :tab-width 4
           :right-divider-width 30
           :scroll-bar-width 8))

  ;; Read the doc string of `spacious-padding-subtle-mode-line' as it
  ;; is very flexible and provides several examples.
  (setq spacious-padding-subtle-mode-line
        `( :mode-line-active 'default
           :mode-line-inactive vertical-border)))

;;; perfect-margin - バッファが一つの時に中央寄せ
(use-package perfect-margin
  :straight t
  :defer t
  :init
  ;; perfect-margin-mode に autoload が未定義のため、明示的に require してから有効化
  (run-with-idle-timer 0.5 nil (lambda () (require 'perfect-margin) (perfect-margin-mode 1)))
  :config
  (setq perfect-margin-ignore-filters nil)
  )

;;; volatile-highlights - 一時的なハイライト（選択範囲など）を強調表示
(use-package volatile-highlights
  :straight t
  :hook (after-init . volatile-highlights-mode)
  :custom-face
  (vhl/default-face ((nil (:foreground "#FF3333" :background "#FFCDCD"))))
  )

;;; highlight-symbol - シンボルのハイライトとナビゲーション
(use-package highlight-symbol
  :straight t
  :bind (([f3] . highlight-symbol-at-point)
         ([f4] . highlight-symbol-remove-all))
  :config
  (setq highlight-symbol-colors '("DeepSkyBlue1" "LimeGreen" "HotPink1" "Yellow"
                                  "Cyan" "OrangeRed1" "MediumOrchid1" "SkyBlue"))
  )

;;; Rainbow-delimiters - 括弧の色分けで対応関係を視覚化
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

  ;; **括弧の過不足 (エラー) を目立たせる**
  (rainbow-delimiters-mismatched-face ((t (:foreground "#ff6c6b" :background "#5f0000" :weight bold :underline t))))
  (rainbow-delimiters-unmatched-face ((t (:foreground "#ffffff" :background "#af0000" :weight bold :underline t))))
  :bind (("C-c l" . rainbow-delimiters-using-stronger-colors))  ;; より強調した色に変更
  :config
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
                      :strike-through t)
  )

;;; Nerd-icons - グラフィック環境向けのアイコン
(use-package nerd-icons
  :straight t
  :if (display-graphic-p)
  )

;;; Nerd-icons-completion - vertico 等の補完パッケージでアイコンを表示
(use-package nerd-icons-completion
  :straight t
  :if (display-graphic-p)
  :hook (after-init . nerd-icons-completion-mode)
  )

;;; Nerd-icons-dired - dired でアイコンを表示
(use-package nerd-icons-dired
  :straight t
  :if (display-graphic-p)
  :hook (dired-mode . nerd-icons-dired-mode)
  )

;;; Nyan-mode バッファ上での位置をニャンキャットが教えてくれる
(use-package nyan-mode
  :straight t
  :if (display-graphic-p)
  :defer t
  :custom
  (nyan-bar-length 24)
  :init
  ;; nyan-mode は autoload 済み → タイマーでパッケージロード + :config 実行
  (run-with-idle-timer 0.5 nil #'nyan-mode 1)
  )

;;; Minions - マイナーモードをハンバーガーメニューで表示
(use-package minions
  :straight t
  :if (display-graphic-p)
  :defer t
  :init
  ;; minions-mode は autoload 済み → タイマーでパッケージロード + :config 実行
  (run-with-idle-timer 0.5 nil #'minions-mode 1)
  )

;;; page-break-lines ^Lの改ページ文字の表示を良くする
(use-package page-break-lines
  :straight t
  :defer t
  :init
  ;; page-break-lines-mode は autoload 済み（バッファローカル）
  (run-with-idle-timer 0.5 nil #'page-break-lines-mode 1)
  )

;;; highlight-defined - 既知のシンボルに色を付ける
(use-package highlight-defined
  :straight t
  :hook (emacs-lisp-mode . highlight-defined-mode)
  )

;;; highlight-quoted - 引用符と引用記号に色を付ける
(use-package highlight-quoted
  :straight t
  :hook (emacs-lisp-mode . highlight-quoted-mode)
  )

(provide '23-visual)
;;; 23-visual.el ends here
