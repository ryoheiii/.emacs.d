;;; 33-vcs.el --- バージョン管理の設定 -*- lexical-binding: t; -*-
;;; Commentary:
;; Git, SVN 等のバージョン管理ツール関連パッケージの設定

;;; Code:

;; 遅延ロード先とプラットフォーム固有定義をコンパイラへ伝える。
(declare-function magit-restore-window-configuration "magit-mode")
(declare-function magit-mode-get-buffers "magit-mode")

;;;;; [Group] バージョン管理関連 ;;;;;
;;; Magit - Git の状態確認と操作
(use-package magit
  :straight t
  :bind (("C-x G" . magit-status)
         ("C-x M-g" . magit-dispatch)
         :map magit-status-mode-map
         ("q" . mu-magit-kill-buffers))
  ;; :bind から参照する関数は :preface で定義する (:init だと多重定義警告が出る)
  :preface
  (defun mu-magit-kill-buffers ()
    "ウィンドウ構成を復元し、現在のリポジトリの Magit バッファをすべて閉じる。"
    (interactive)
    (let ((buffers (magit-mode-get-buffers)))
      (magit-restore-window-configuration)
      (mapc #'kill-buffer buffers))))

;;; Diff-hl - 変更点を表示 (git-gutter の置き換え)
(use-package diff-hl
  :straight t
  :defer 1
  :hook ((magit-post-refresh . diff-hl-magit-post-refresh)
         (dired-mode . diff-hl-dired-mode))
  :config
  (global-diff-hl-mode +1)
  (global-diff-hl-show-hunk-mouse-mode +1)
  (diff-hl-margin-mode +1))

;;; Difftastic - 構文を考慮した差分表示を Magit から呼ぶ
(use-package difftastic
  :straight t
  :bind (:map magit-blame-read-only-mode-map
              :package magit-blame
              ("D" . difftastic-magit-show)
              ("S" . difftastic-magit-show))
  :init
  (with-eval-after-load 'magit-diff
    (transient-append-suffix 'magit-diff '(-1 -1)
      [("D" "Difftastic diff (dwim)" difftastic-magit-diff)
       ("S" "Difftastic show" difftastic-magit-show)])))

;;; Dsvn - SVN 管理ツール
(use-package dsvn
  :straight t
  :defer t
  :commands (svn-status svn-update))

(provide '33-vcs)
;;; 33-vcs.el ends here
