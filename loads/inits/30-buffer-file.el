;;; 30-buffer-file.el --- バッファとファイル管理の設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; バッファ管理、ファイル履歴、スクロール関連パッケージの設定

;;; Code:

;; 遅延ロード先とプラットフォーム固有定義をコンパイラへ伝える。
(declare-function recentf-save-list "recentf")
(declare-function dashboard-insert-startupify-lists "dashboard")
(declare-function dashboard-initialize "dashboard")
(declare-function dashboard-resize-on-hook "dashboard")

;;;;; [Group] Buffer-and-File-management - バッファとファイル管理関連 ;;;;;
;;; xclip - クリップボードとの共有
(use-package xclip
  :if (and (not (display-graphic-p)) ; GUIではない
           (getenv "DISPLAY")        ; X11 の DISPLAY 変数がある
           (executable-find "xclip")) ; `xclip` がシステムにインストールされている
  :straight t
  :defer 0.5
  :config
  (xclip-mode 1)
  )

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
  :hook (after-init . my/dashboard-setup)
  )

;;; recentf - 最近使用したファイルの履歴管理
(use-package recentf
  :straight nil
  ;; :config 内の defmacro は byte compile 時に未定義のまま展開されるため関数にし、
  ;; :config のタイマー登録から参照できるよう :preface で定義する
  :preface
  (defun my/recentf-save-list-quietly ()
    "エコー領域と *Messages* への出力を抑制して recentf リストを保存する。"
    (let ((inhibit-message t)
          (message-log-max nil))
      (recentf-save-list)))
  :hook (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 2000)                                 ; 保存するファイルの数
  (recentf-max-menu-items 15)                                    ; メニューに表示するアイテム数
  (recentf-exclude '("recentf-"))                 ; 除外するファイルパターン
  (recentf-auto-cleanup 'never)                                  ; 自動整理の設定
  (recentf-save-file (my-set-history "recentf-" user-full-name)) ; recentf の保存パス
  :config
  ;; 30秒ごとに recentf リストを自動保存
  (run-with-idle-timer 30 t #'my/recentf-save-list-quietly)
  )

;;; recentf-ext - recentf の拡張機能
(use-package recentf-ext
  :straight t
  :after recentf
  )

;;; pixel-scroll - ピクセル単位のスムーズなスクロール(組み込み)
;; ロード時の GUI 判定のため daemon 起動は対象外
;; (doom-modeline 等の既存 GUI 限定宣言と同一方針)
(use-package pixel-scroll
  :straight nil
  :if (display-graphic-p)
  :hook (after-init . pixel-scroll-precision-mode)
  :custom
  (pixel-scroll-precision-interpolate-page t) ; ページ送りも補間する
  )

;;; anzu - 検索・置換時にマッチ数や現在位置を表示
(use-package anzu
  :straight t
  :hook (after-init . global-anzu-mode)
  :custom
  (anzu-mode-lighter "")                    ; モードラインの表示を非表示
  (anzu-deactivate-region t)                ; 領域選択時も `anzu` を使う
  (anzu-search-threshold 1000)              ; 最大検索数
  (anzu-replace-to-string-separator " => ") ; 置換時の表示フォーマット
  )

(provide '30-buffer-file)
;;; 30-buffer-file.el ends here
