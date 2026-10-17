;;; 24-org.el --- Org モードの設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; Org モードとその関連パッケージの設定

;;; Code:

;;;;; [Group] Org 関係 ;;;;;
;;; Org の表示とタスク管理
;; org-mode は tab-width 8 を前提にする（構造化テキストの桁がずれる）。
;; 一方 19-language-modes.el の `my/text-mode-setup' は text-mode 派生へ
;; tab-width 2 を設定し、org-mode もその対象になる。org 側で 8 へ戻す。
(defun my/org-mode-setup ()
  "Org モード用の設定。text-mode 側で変更された `tab-width' を org の前提へ戻す。"
  (setq tab-width 8))

(use-package org
  :straight nil
  :hook ((org-mode . visual-line-mode)   ; 表示上の折り返し（改行文字は挿入しない）
         (org-mode . my/org-mode-setup))
  :defer t
  :custom
  (org-return-follows-link t)            ; Returnキーでリンク先を開く
  (org-hide-leading-stars t)             ; 見出しの先頭に並ぶ余分な * を隠す
  (org-startup-indented t)               ; インデント表示をデフォルトで有効化
  (org-src-fontify-natively t)           ; ソースコードをシンタックスハイライト
  (org-src-tab-acts-natively t)          ; org-babelでタブキーを言語モードに連動
  (org-src-content-indentation 2)        ; org-babelのソースコードインデント
  (org-startup-folded 'content)          ; 起動時は見出しを表示し、本文を折りたたむ
  (org-log-done 'time)                   ; タスク完了時に時間を記録
  (org-log-into-drawer t)                ; ログを :LOGBOOK: に格納
  (org-adapt-indentation nil)            ; インデントの自動調整をオフにする
  (org-cycle-separator-lines 2)          ; 見出し前に空行が 2 行以上あれば、折りたたみ時も 1 行残す
  (org-ellipsis " ▼")                   ; 折りたたみ表示の記号変更
  (org-todo-keywords
   '((sequence "TODO(t)" "IN-PROGRESS(i)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)"))))

(provide '24-org)
;;; 24-org.el ends here
