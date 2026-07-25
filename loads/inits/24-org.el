;;; 24-org.el --- Org モードの設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; Org モードとその関連パッケージの設定

;;; Code:

;;;;; [Group] Org - Org 関係 ;;;;;
;;; Org - org 設定
;; org-mode は tab-width 8 を前提にする（構造化テキストの桁がずれる）。
;; 一方 19-language-modes.el の `my/text-mode-setup' は text-mode 派生へ
;; tab-width 2 を設定し、org-mode もその対象になる。org 側で 8 へ戻す。
(defun my/org-mode-setup ()
  "Org モード用の設定。text-mode 側で変更された `tab-width' を org の前提へ戻す。"
  (setq tab-width 8))

(use-package org
  :straight nil
  :hook ((org-mode . visual-line-mode)   ; 自動改行の有効化
         (org-mode . my/org-mode-setup))
  :defer t
  :custom
  (org-return-follows-link t)            ; Returnキーでリンク先を開く
  (org-hide-leading-stars t)             ; 見出しの*を非表示
  (org-startup-indented t)               ; インデント表示をデフォルトで有効化
  (org-src-fontify-natively t)           ; ソースコードをシンタックスハイライト
  (org-src-tab-acts-natively t)          ; org-babelでタブキーを言語モードに連動
  (org-edit-src-content-indentation 2)   ; org-babelのソースコードインデント
  (org-startup-folded 'content)          ; 初期表示で折りたたむ
  (org-log-done 'time)                   ; タスク完了時に時間を記録
  (org-log-into-drawer t)                ; ログを :LOGBOOK: に格納
  (org-adapt-indentation nil)            ; インデントの自動調整をオフにする
  (org-cycle-separator-lines 2)          ; 見出しの間隔
  (org-ellipsis " ▼")                   ; 折りたたみ表示の記号変更
  (org-todo-keywords
   '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)")))
  )

(provide '24-org)
;;; 24-org.el ends here
