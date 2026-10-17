;;; 30-buffer-file.el --- バッファとファイル管理の設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; バッファ管理、ファイル履歴、スクロール関連パッケージの設定

;;; Code:

;; 遅延ロード先とプラットフォーム固有定義をコンパイラへ伝える。
(declare-function recentf-save-list "recentf")
(declare-function dashboard-insert-startupify-lists "dashboard")
(declare-function dashboard-initialize "dashboard")
(declare-function dashboard-resize-on-hook "dashboard")

;;; xclip - クリップボードとの共有
(use-package xclip
  :if (and (not (display-graphic-p)) ; GUIではない
           (getenv "DISPLAY")        ; X11 の DISPLAY 変数がある
           (executable-find "xclip")) ; `xclip` がシステムにインストールされている
  :straight t
  :defer 0.5
  :config
  (xclip-mode 1))

;;; dashboard - Emacs のスタートアップ画面をカスタマイズ
;; dashboard-setup-startup-hook は after-init-hook へ描画関数を登録するが、after-init の
;; 実行中に同じフックへ足した関数は走らない（run-hooks は開始時点のリストを走査する）。
;; 従来は window-setup-hook のリサイズ経路が偶然描画していた。同関数と同じ条件・フック
;; 構成にし、描画だけはその場で行う。
(use-package dashboard
  :straight t
  ;; :hook から参照する関数は :preface で定義する (:init だと多重定義警告が出る)
  :preface
  (defun my/dashboard-setup ()
    "引数なし起動のときだけ dashboard を描画し、表示とリサイズのフックを登録する。
`dashboard-setup-startup-hook' と同じ条件で、ファイル引数付きの起動では何もしない。"
    (when (< (length command-line-args) 2)
      (require 'dashboard)
      (dashboard-insert-startupify-lists)
      (add-hook 'emacs-startup-hook #'dashboard-initialize)
      (add-hook 'window-setup-hook #'dashboard-resize-on-hook)
      (add-hook 'window-size-change-functions #'dashboard-resize-on-hook 100)))
  :hook (after-init . my/dashboard-setup))

;;; recentf - 最近使用したファイルの履歴管理
(use-package recentf
  :straight nil
  :hook (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 2000)                                 ; 保存するファイルの数
  (recentf-max-menu-items 15)                                    ; メニューに表示するアイテム数
  (recentf-exclude '("recentf-"))                 ; 除外するファイルパターン
  (recentf-auto-cleanup 'never)                                  ; 履歴の自動削除を無効化
  (recentf-save-file (my-set-history "recentf-" user-full-name)) ; recentf の保存パス
  (recentf-show-messages nil)                                    ; 自動保存の "Wrote ..." を出さない
  :config
  ;; 入力が止まって 30 秒たったときに recentf リストを保存する。
  ;; recentf-autosave-interval は定周期タイマーのため使わない。
  (run-with-idle-timer 30 t #'recentf-save-list))

;;; recentf-ext - recentf の拡張機能
(use-package recentf-ext
  :straight t
  :after recentf)

;;; pixel-scroll - ピクセル単位のスムーズなスクロール(組み込み)
;; ロード時の GUI 判定のため daemon 起動は対象外
;; (doom-modeline 等の既存 GUI 限定宣言と同一方針)
(use-package pixel-scroll
  :straight nil
  :if (display-graphic-p)
  :hook (after-init . pixel-scroll-precision-mode)
  :custom
  (pixel-scroll-precision-interpolate-page t)) ; ページ送りも補間する

;;; anzu - 検索・置換時にマッチ数や現在位置を表示
(use-package anzu
  :straight t
  :hook (after-init . global-anzu-mode)
  :custom
  (anzu-mode-lighter "")                    ; モードラインの表示を非表示
  (anzu-deactivate-region t)                ; 選択範囲の置換を始めるときに選択表示を解除する
  (anzu-search-threshold 1000)              ; 最大検索数
  (anzu-replace-to-string-separator " => ")) ; 置換時の表示フォーマット

(provide '30-buffer-file)
;;; 30-buffer-file.el ends here
